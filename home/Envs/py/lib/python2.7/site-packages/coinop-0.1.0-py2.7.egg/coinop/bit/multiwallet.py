from binascii import hexlify, unhexlify

from nacl.utils import random
from pycoin.key import bip32

from .script import Script
from .keys import PrivateKey, PublicKey

import bitcoin.base58 as base58

# A MultiWallet maintains any number of BIP 32 HDW trees, treating them
# as parallel structures.  Given a path, the MultiWallet can produce
# a MultiNode containing the appropriate nodes for each tree.  These
# nodes can be used in multi-sig applications.

# The HDW trees need not be private; in many applications, you specifically
# need to use some public trees and some private trees.

# MultiWallet trees are named, allowing them to be distinguished easily.

class MultiWallet(object):

    # Given a list of tree names, create a MultiWallet containing private
    # trees.
    @classmethod
    def generate(cls, names, network=u'testnet'):
        seeds = {}
        def create_node(name):
            secret = random(32)
            # FIXME: set blockchain/network correctly
            if network in [u'bitcoin', u'mainnet']:
                tree = bip32.Wallet.from_master_secret(secret, netcode=u'BTC')
            else:
                tree = bip32.Wallet.from_master_secret(secret, netcode=u'XTN')
            return tree

        for name in names:
            seeds[name] = create_node(name).wallet_key(as_private=True)

        return cls(private=seeds, network=network)


    # The private and public arguments are dicts that contain HDW seed
    # strings, keyed by name.
    def __init__(self, private={}, public={}, network=u'testnet'):
        # It is possible to distinguish between private and public seeds
        # based on the string content.  Consider modifying this function
        # to take merely one dict of seeds.  Trees should still be stored
        # separately.
        self.trees = {}
        self.private_trees = {}
        self.public_trees = {}
        self._network = network

        for name, seed in private.iteritems():
            tree = bip32.Wallet.from_wallet_key(seed)
            self.private_trees[name] = self.trees[name] = tree

        for name, seed in public.iteritems():
            tree = bip32.Wallet.from_wallet_key(seed)
            self.public_trees[name] = self.trees[name] = tree

    @property
    def network(self): return self._network

    @network.setter
    def network(self, value): self._network = value

    def to_dict(self):
        return dict(private=self.private_seeds(), public=self.public_seeds())

    def private_seed(self, name):
        try:
            return self.private_trees[name].wallet_key(as_private=True)
        except KeyError:
            raise Exception("No private tree for '{0}'".format(name))

    def private_seeds(self):
        out = {}
        for name, tree in self.private_trees.iteritems():
            out[name] = self.private_seed(name)
        return out

    def public_seed(self, name):
        tree = self.public_trees.get(name, None)
        if not tree:
            tree = self.private_trees.get(name, None)
        if not tree:
            raise Exception("No public tree for '{0}'".format(name))
        return tree.wallet_key()

    def public_seeds(self):
        out = {}
        for name, tree in self.public_trees.iteritems():
            out[name] = self.public_seed(name)
        return out

    # Given a wallet path, returns a MultiNode for that path.
    def path(self, path):
        _path = path[2:]
        options = { 'private': {}, 'public': {} }

        for name, tree in self.private_trees.iteritems():
            options['private'][name] = tree.subkey_for_path(_path)
        for name, tree in self.public_trees.iteritems():
            options['public'][name] = tree.subkey_for_path(_path)
        options['network'] = self.network

        return MultiNode(path, **options)

    # Determines whether the script included in an Output was generated
    # from this wallet.
    def is_valid_output(self, output):
        # TODO: better error handling in case no wallet_path is found.
        # May also be better to take the wallet_path as an argument to
        # the function.
        path = output.metadata['wallet_path']
        node = self.path(path)
        # TODO: use python equiv of ruby to_s
        # apparently the global str() ?
        # FIXME: node.p2sh_script should be taking an M argument indicating
        # how many signatures are required to authorize.
        ours = node.p2sh_script(network=self.network).to_string()
        theirs = output.script.to_string()
        return ours == theirs

    # Returns a list of signature dicts, corresponding to the inputs
    # for the supplied transaction.
    def signatures(self, transaction):
        return map(self.sign_input, transaction.inputs)

    # Given an Input (the output of which must contain a wallet_path in
    # its metadata) return a dictionary of signatures.  The dict keys
    # are the names of the private trees.
    def sign_input(self, input):
        path = input.output.metadata['wallet_path']
        node = self.path(path)
        sig_hash = input.sig_hash(node.script())
        return node.signatures(sig_hash)


# Manages any number of BIP 32 nodes (private and/or public) derived from
# a given path.
class MultiNode:

    def __init__(self, path, private={}, public={}, network=u'testnet'):
        self.path = path
        self.private = private
        self.public = public

        self.private_keys = {}
        self.public_keys = {}
        self.network = network

        for name, node in private.iteritems():
            priv = PrivateKey.from_secret(node.secret_exponent_bytes)
            self.private_keys[name] = priv

            pub = priv.public_key()
            self.public_keys[name] = pub

        for name, node in public.iteritems():
            pub = PublicKey.from_pair(node.public_pair)
            self.public_keys[name] = pub
            pass

    def script(self, m=2):
        names = sorted(self.public_keys.keys())
        keys = [self.public_keys[name].compressed() for name in names]

        return Script(public_keys=keys, needed=m)

    # Returns the P2SH address for a m-of-n multisig script using the
    # public keys derived for this node.
    def address(self, network=None):
        network = network if network else self.network
        # FIXME: Should take the M argument and pass into self.script()
        # Otherwise, this is always a 2-of-n script address.
        return self.script().p2sh_address(network=network)


    # Returns the P2SH script to be used as the scriptPubkey for an Output.
    def p2sh_script(self, network=None):
        network = network if network else self.network
        # FIXME: take an M argument to pass to self.address()
        return Script(p2sh_address=self.address(network=network))

    # Returns a dict of signatures, keyed by the tree names.
    def signatures(self, value):
        names = sorted(self.private_keys.keys())
        s = ((name, base58.encode(self.sign(name, value))) for name in names)
        return dict(s)

    def sign(self, name, value):
        try:
            key = self.private_keys[name]
            # Append a "hashtype" byte to the signature.
            # \x01 means the hash type is SIGHASH_ALL.  Other hashtypes are not
            # often used.
            # https://en.bitcoin.it/wiki/OP_CHECKSIG#Hashtype_SIGHASH_ALL_.28default.29
            return key.sign(value) + b'\x01'
        except KeyError:
            raise Exception("No such key: '{0}'".format(name))


    # Generate the script_sig for a set of signatures.
    def script_sig(self, signatures):
        self.script.p2sh_sig(signatures=signatures)
