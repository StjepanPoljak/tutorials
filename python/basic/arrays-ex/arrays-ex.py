#!/usr/bin/env python3

from array import array

floats = array('d', (0.3, 0.2, 0.6, 0.4))
print(floats[1])
# 0.2

floats[1] = 0.5
print(floats)
# array('d', [0.3, 0.5, 0.6, 0.4])

print(floats.typecode)
# d

fp = open("floats.bin", "wb")
floats.tofile(fp)
fp.close()

floats2 = array('d')
fp = open("floats.bin", "rb")
floats2.fromfile(fp, 4)
fp.close()
print(floats2[2])
# 0.6

floats3 = array('d')
fp = open("floats.bin", "rb")
floats3.frombytes(fp.read())
fp.close()
print(floats3[-1])
# 0.4

print(floats3 == floats)
# True

# Note: pickle is even better for object serialization,
# especially if dealing with complex numbers, nested
# collections and user-defined classes

print(list(floats3))
# [0.3, 0.5, 0.6, 0.4]

# it's better to do insort() while adding files to array
# instead of copying it into a new array object as below

sorted_floats3 = array(floats3.typecode, sorted(floats3))
print(sorted_floats3)
# array('d', [0.3, 0.4, 0.5, 0.6])

# memoryview is best to manipulate arrays in-place, on
# raw byte-level

memv_ints = memoryview(sorted_floats3).cast('B')
print([hex(x) for x in memv_ints][:4])
# ['0x33', '0x33', '0x33', '0x33']

print(memv_ints.tolist()[:4])
# [51, 51, 51, 51]

memv_ints[2] = 0
print(memv_ints.tolist()[:4])
# [51, 51, 0, 51]

print(sorted_floats3)
# array('d', [0.29999999981446307, 0.4, 0.5, 0.6])

