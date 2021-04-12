#!/usr/bin/env python3

# NOTE: this is just an example of how dict works
# internally; subclass dict only if you want to
# extend it.

class MyDict(dict):

	def __missing__(self, key):
		raise Exception("Key {} not found.".format(key))

	def __getitem__(self, key):
		val = dict.__getitem__(self, key)
		return val

	def __setitem__(self, key, value):
		dict.__setitem__(self, hex(key), hex(value))
		dict.__setitem__(self, key, value)

mydict = MyDict()

mydict[10] = 15

print(mydict)
# {'0xa': '0xf', 10: 15}

print(10 in mydict)
# True

print("0xa" in mydict)
# True

print(mydict[10])
# 15

print(mydict["0xa"])
# 0xf

try:
    print(mydict[4])
except:
    print("Exception: Key 4 not found")
# Exception: Key 4 not found.

mydict.update({10: 8})
print(mydict)
# {'0xa': '0xf', 10: 8}

# Note: Using dict.update() produces some unexpected
# results here. So usually, one has to hack around
# with providing custom update(), __init__() etc.
# This is why it is better to use UserDict for
# subclassing, if you have high expectations from
# your own dict type.

del mydict[10]
print(mydict)
# {'0xa': '0xf'}

from collections import UserDict

# UserDict is an implementation of MutableMapping
# which uses dict for internal storage; subclass
# this if you want dict-like behaviour but override
# some of the methods. You can access the dict
# in UserData via self.data.

from collections.abc import MutableMapping

# Subclass MutableMapping if you want dict-like
# behaviour and you want to control exactly how
# data is stored.

class MyUserDict(MutableMapping):
    pass    # equivalent of nop

try:
    myuserdict = MyUserDict()
except TypeError as terror:
    print(terror)

# Can't instantiate abstract class MyUserDict with abstract methods __delitem__, __getitem__, __iter__, __len__, __setitem__
