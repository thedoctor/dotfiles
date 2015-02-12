'''
[Simplified BSD, see http://www.opensource.org/licenses/bsd-license.html]

Copyright (c) 2011, Sam Rushing
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following disclaimer
      in the documentation and/or other materials provided with the
      distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'''


# -*- Mode: Python -*-

# python-ecdsa wrapper
# see https://github.com/warner/python-ecdsa
# see http://lapo.it/asn1js/

import ecdsa
import random
from ecdsa import der

_a  = 0x0000000000000000000000000000000000000000000000000000000000000000L
_b  = 0x0000000000000000000000000000000000000000000000000000000000000007L
_p  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2FL
_Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798L
_Gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8L
_r  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141L
_oid = (1, 3, 132, 0, 10)

curve = ecdsa.ellipticcurve.CurveFp(_p, _a, _b)
gen = ecdsa.ellipticcurve.Point(curve, _Gx, _Gy, _r)
randrange = random.SystemRandom().randrange
secp256k1 = ecdsa.curves.Curve("secp256k1", curve, gen, _oid)
ecdsa.curves.curves.append(secp256k1)

class KEY:

    def __init__(self):
        self.prikey = None
        self.pubkey = None
        self.compressed = False
        self.secret = None

    def generate(self, secret=None):
        if secret:
            self.prikey = ecdsa.SigningKey.from_string(secret, curve=secp256k1)
        else:
            self.prikey = ecdsa.SigningKey.generate(curve=secp256k1)
        self.pubkey = self.prikey.get_verifying_key()
        return self.prikey.to_der()

    def set_privkey(self, key):
        if len(key) >= 214:
            seq1, rest = der.remove_sequence(key)
            integer, rest = der.remove_integer(seq1)
            secret, rest = der.remove_octet_string(rest)
            tag1, cons1, rest, = der.remove_constructed(rest)
            tag2, cons2, rest, = der.remove_constructed(rest)
            point_str, rest = der.remove_bitstring(cons2)
            self.compressed = ord(point_str[1]) != 4
            self.prikey = ecdsa.SigningKey.from_string(secret, curve=secp256k1)
        else:
            self.prikey = ecdsa.SigningKey.from_der(key)
        self.pubkey = self.prikey.get_verifying_key()

    def set_pubkey(self, key):
        key = key[1:]
        self.pubkey = ecdsa.VerifyingKey.from_string(key, curve=secp256k1)

    def encode_point(self, p):
        order = self.prikey.curve.generator.order()
        x_str = ecdsa.util.number_to_string(p.x(), order)
        y_str = ecdsa.util.number_to_string(p.y(), order)
        if self.compressed:
            return chr(2 + (p.y() & 1)) + x_str
        else:
            return chr(4) + x_str + y_str

    def get_privkey(self):
        return der.encode_sequence(
            der.encode_integer(1),
            der.encode_octet_string(self.prikey.to_string()),
            der.encode_constructed(0,
                der.encode_sequence(
                    ecdsa.der.encode_integer(1),
                    der.encode_sequence(
                        der.encode_oid(*(1, 2, 840, 10045, 1, 1)),
                        der.encode_integer(self.prikey.curve.curve.p()),
                    ),
                    der.encode_sequence(
                        der.encode_octet_string(chr(0)),
                        der.encode_octet_string(chr(7)),
                    ),
                    der.encode_octet_string(self.encode_point(self.prikey.curve.generator)),
                    der.encode_integer(self.prikey.curve.generator.order()),
                    der.encode_integer(1)
                )
            ),
            der.encode_constructed(1, 
                der.encode_bitstring(chr(0) + self.encode_point(self.pubkey.pubkey.point))
            ),
        )

    def get_pubkey(self):
        return self.encode_point(self.pubkey.pubkey.point)

    def sign(self, hash):
        sig = self.prikey.sign_digest(hash, sigencode=ecdsa.util.sigencode_der)
        return sig

    def verify(self, hash, sig):
        return self.pubkey.verify_digest(sig[:-1], hash, sigdecode=ecdsa.util.sigdecode_der)

    def set_compressed(self, compressed=False):
        self.compressed = compressed

    def get_secret(self):
        return self.prikey.to_string()

if __name__ == '__main__':
    # ethalone keys
    ec_secret = '' + \
        'a0dc65ffca799873cbea0ac274015b9526505daaaed385155425f7337704883e'
    ec_private = '308201130201010420' + \
        'a0dc65ffca799873cbea0ac274015b9526505daaaed385155425f7337704883e' + \
        'a081a53081a2020101302c06072a8648ce3d0101022100' + \
        'fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f' + \
        '300604010004010704410479be667ef9dcbbac55a06295ce870b07029bfcdb2d' + \
        'ce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a6' + \
        '8554199c47d08ffb10d4b8022100' + \
        'fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141' + \
        '020101a14403420004' + \
        '0791dc70b75aa995213244ad3f4886d74d61ccd3ef658243fcad14c9ccee2b0a' + \
        'a762fbc6ac0921b8f17025bb8458b92794ae87a133894d70d7995fc0b6b5ab90'

    k = KEY()
    k.generate (ec_secret.decode('hex'))
    k.set_compressed(True)
    print k.get_privkey ().encode('hex')
    print k.get_pubkey().encode('hex')
    print k.get_secret().encode('hex')

