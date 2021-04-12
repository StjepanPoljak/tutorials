#!/usr/bin/env python3

empty_dictionary = {}

print(empty_dictionary)
# {}

if empty_dictionary:
    print("Not empty.")
else:
    print("Empty.")
# Empty.

color = {
    'Red': 127,
    'Green': 32,
    'Blue': 255
}

print(color)
# {'Red': 127, 'Green': 32, 'Blue': 255}

print(color['Green'])
# 32

color['Alpha'] = 0.5

print(color)
# {'Red': 127, 'Green': 32, 'Blue': 255, 'Alpha': 0.5}

color['Alpha'] = 1.0

print(color)
# {'Red': 127, 'Green': 32, 'Blue': 255, 'Alpha': 1.0}

del color['Alpha']

for key, value in color.items():
    print(key + "\t-> " + str(value))
# Red       -> 127
# Green     -> 32
# Blue      -> 255

val_string = "Values:"
for key in color.keys():
    val_string += " "
    val_string += str(color[key])
print(val_string)
# Values: 127 32 255

print(sorted(color.values()))
# [32, 127, 255]

if 'Alpha' not in color.keys():
    print("No alpha!")
else:
    print("There is alpha.")
# No alpha!

del color["Red"]

color.update({ "Alpha": 0.5, "Red": 5 })
print(color)
# {'Green': 32, 'Blue': 255, 'Alpha': 0.5, 'Red': 5}

from collections import OrderedDict

# OrderedDict keeps order of items as they were added.

numbers_dict = OrderedDict()

numbers_dict["One"] = 1
numbers_dict["Two"] = 2
numbers_dict["Three"] = 3
numbers_dict["Zero"] = 0

for number in numbers_dict.keys():
    print("\t* " + number + ": " + str(numbers_dict[number]))
#	* One: 1
#	* Two: 2
#	* Three: 3
#   * Zero: 0

from collections import Counter

# Counter is a dictionary that counts each time key was set.

print(Counter("otorhinolaryngology"))
# Counter({'o': 5, 'r': 2, 'n': 2, 'l': 2, 'y': 2, 'g': 2, 't': 1, 'h': 1, 'i': 1, 'a': 1})

from collections import ChainMap

# ChainMap is best if one has to use dictionaries within
# dictionaries (optimally).

mix = ChainMap(color, Counter("Honolulu"), numbers_dict)

print(mix["o"])
# 2

print(mix["Alpha"])
# 0.5

print(mix["Three"])
# 0

from types import MappingProxyType

# MappingProxyType is a dictionary proxy - it
# offers only a read-only view of a dictionary.

color_proxy = MappingProxyType(color)

print(color_proxy)
# {'Green': 32, 'Blue': 255, 'Alpha': 0.5, 'Red': 5}

try:
    color_proxy["Yellow"] = 5
except TypeError as terror:
    print(terror)
# 'mappingproxy' object does not support item assignment
