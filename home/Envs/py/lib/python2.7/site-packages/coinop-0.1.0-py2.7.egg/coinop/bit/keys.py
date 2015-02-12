from binascii import hexlify, unhexlify

from ecdsa import SECP256k1, SigningKey, VerifyingKey
from ecdsa.util import sigencode_der, sigdecode_der
from ecdsa.util import string_to_number, number_to_string, randrange

from ecdsa.ellipticcurve import Point


# Wrappers around the ecdsa classes.  Primary purpose is to provide a
# clean interface for using pycoin's bip32 HDWs.

class PrivateKey(object):

    @classmethod
    def from_secret(cls, secret):
        # TODO: consider renaming to from_string, for parallelism with
        # PublicKey's class method of that name.
        key = SigningKey.from_string(secret, curve=SECP256k1)
        return cls(key)

    # ECDSA private keys can be derived from a single integer, the exponent.
    @classmethod
    def from_exponent(cls, exponent):
        key = SigningKey.from_secret_exponent(exponent, curve=SECP256k1)
        return cls(key)

    def __init__(self, key):
        self.key = key

    def public_key(self):
        return PublicKey(self.key.get_verifying_key())

    def sign(self, digest):
        return self.key.sign_digest(digest, sigencode=sigencode_der)

    def to_string(self):
        return self.key.to_string()


class PublicKey(object):

    @classmethod
    def from_string(cls, string):
        return cls(VerifyingKey.from_string(string, curve=SECP256k1))

    # Create a PublicKey from an x,y pair.
    @classmethod
    def from_pair(cls, pair):
        x, y = pair
        point = Point(curve=SECP256k1.curve, x=x, y=y, order=SECP256k1.order)
        key = VerifyingKey.from_public_point(point, curve=SECP256k1)
        return cls(key)


    def __init__(self, key):
        self.key = key

    def verify(self, digest, sig):
        return self.key.verify_digest(sig, digest, sigdecode=sigdecode_der)

    def to_string(self):
        return self.key.to_string()

    def uncompressed(self):
        return b'\04' + self.key.to_string()

    def compressed(self):
        pubkey = self.key.pubkey
        order = pubkey.order
        x, y = [pubkey.point.x(), pubkey.point.y()]
        return  chr(2 + (y & 1)) + number_to_string(x, order)

