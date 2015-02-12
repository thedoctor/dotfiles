from binascii import hexlify, unhexlify

#python-bitcoinlib

from bitcoin.core.script import CScript, SignatureHash, SIGHASH_ALL
from bitcoin.core import lx, b2lx, COutPoint, CTxIn, CTxOut, CTransaction

# sha256 the input value twice.  Duplicated in this project's util package.
from bitcoin.core.serialize import Hash

from coinop.bit.script import Script

# Wrapper for CTxIn
class Input:

    def __init__(self, data, transaction=None, index=None):
        # TODO:  seems overkill to take a data arg that only ever
        # contains the "output" field.  Consider making output be
        # a direct argument.
        self.transaction = transaction
        self.index = index

        # the output property may be either a dict or an Output instance
        output = data['output']
        if isinstance(output, Output):
            self.output = output
        else:
            self.output = Output(output)

        self.signatures = []

        self.sig_hash_hex = data.get('sig_hash', None)
        if self.sig_hash_hex:
            self.sig_hash_bytes = unhexlify(self.sig_hash_hex)
        else:
            self.sig_hash_bytes = None

    # Returns a CTxIn corresponding to this Input
    def native(self):
        tx_hex = self.output.transaction_hash
        # Convert a little-endian hex string to bytes
        txid = lx(tx_hex)
        vout = self.output.index
        outpoint = COutPoint(txid, vout)
        return CTxIn(outpoint)

    # Returns the digest for this input, the value to be signed when
    # authorizing the input (creating the scriptSig)
    def sig_hash(self, redeem_script=None):
        # FIXME: As noted in the Transaction method below, we probably
        # should always require a redeem_script.
        return self.transaction.sig_hash(self, redeem_script)

# Wrapper for CTxOut
class Output:

    def __init__(self, data, transaction=None):
        if transaction:
            self.transaction = transaction
        elif 'transaction_hash' in data:
            self._transaction_hash = data['transaction_hash']

        self.index = data.get('index', None)
        # TODO:  determine whether this is a magic number, or merely intended
        # to signify that the value is unknown.  If the latter, reconsider the
        # design.
        self.value = data.get('value', -1)
        self.address = data.get('address', None)
        self.metadata = data.get('metadata', {})

        if 'script' in data:
            # The dict here has two keys: 'type' and 'string'.
            # The Script class notices the 'string' key and parses
            # it to instantiate the native bitcoin.core.script.Cscript
            self.script = Script(**data['script'])
        else:
            self.script = None

    @property
    def transaction_hash(self):
        return self._transaction_hash or self.transaction.hash

    # Returns a CTxOut corresponding to this Output
    def native(self):
        return CTxOut(self.value, self.script.cscript)


# A wrapper to make it easier to work with CTransaction
class Transaction:


    def __init__(self, **options):
        self.inputs = []
        self.outputs = []
        # The original reason for checking properties in the options dict
        # was the expectation that we might eventually want to pass in a
        # CTransaction instance, or possibly a tx-id which we could fetch
        # from the blockchain.
        if 'data' in options:
            self.set_data(options['data'])
        else:
            raise Exception("Invalid options")

    def set_data(self, data):
        self.version = data.get('version', 1)
        self.lock_time = data.get('lock_time', 0)
        self.hash = data.get('hash', None)

        for input_data in data.get('inputs', []):
            index = len(self.inputs)
            _input = Input(transaction=self, data=input_data, index=index)
            self.inputs.append(_input)

        for output_data in data.get('outputs', []):
            output = Output(transaction=self, data=output_data)
            self.outputs.append(output)

    def native(self):
        ins = [input.native() for input in self.inputs]
        outs = [output.native() for output in self.outputs]
        return CTransaction(ins, outs)


    def sig_hash(self, input, redeem_script=None):
        # FIXME: This is going to break if no redeem_script is supplied.
        # The idea in allowing it to be None was that in some cases,
        # the redeem_script could be determined from the input object.
        # This is probably a bad idead, so the default value for redeem_script
        # should be eliminated.

        # We only allow SIGHASH_ALL at this time
        # https://en.bitcoin.it/wiki/OP_CHECKSIG#Hashtype_SIGHASH_ALL_.28default.29
        return SignatureHash(redeem_script.cscript, self.native(), input.index, SIGHASH_ALL)

    # Serialize to a byte string.
    def serialize(self):
        return self.native().serialize()

    # Encode the serialized byte string as hexadecimal.
    def to_hex(self):
        return hexlify(self.serialize())

    def txid(self):
        return Hash(self.serialize())

    def hex_hash(self):
        # bytes to little-endian hex
        return b2lx(Hash(self.native().serialize()))

