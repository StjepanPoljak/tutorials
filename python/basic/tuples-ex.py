# Python3 Tuple Examples

pythagorean = (3, 4, 5)

# Note: You cannot change tuple
# element values after defining it

print(pythagorean)
# (3, 4, 5)

print(pythagorean[1])
# 4

print("Pythagorean tuple elements:")
for element in pythagorean:
    print("\t* " + str(element))
# Pythagorean tuple elements:
#       * 3
#       * 4
#       * 5

(a, b, c) = pythagorean
print(", ".join([str(a), str(b), str(c)]))
# 3, 4, 5

[x, y, z] = [value ** 2 for value in pythagorean]
print(", ".join([str(x), str(y), str(z)]))
# 9, 16, 25

if x + y == z or y + z == x or z + x == y:
    print("Pythagorean")
else:
    print("Not pythagorean")
# Pythagorean

if 3 in pythagorean and 4 in pythagorean and 5 in pythagorean:
    print("3, 4 and 5 present")
# 3, 4 and 5 present

if 42 not in [x, y, z]:
    print("no meaning of life")
# no meaning of life

tests = (True, False)

if not tests[0]:
    print("There is no truth")
elif tests[1]:
    print("There is only lie")
else:
    print("What is in between truth and lie?")
# What is in between truth and lie?

from collections import *

# named tuples take exactly the same amount
# of memory as tuples and less than a regular
# object

Horse = namedtuple("Horse", ["name", "speed", "age", "sex"])

horse = Horse("Secretariat", 200, 34, "m")

print(horse._fields)
# ('name', 'speed', 'age', 'sex')

print(horse._asdict())
# {'name': 'Secretariat', 'speed': 200, 'age': 34, 'sex': 'm'}

print("{} is a {} year old {} horse who "
      "achieves speed of {}mph.".format(horse.name, horse.age,
      "male" if horse.sex == "m" else "female", horse.speed))
# Secretariat is a 34 year old male horse who achieves speed of 200mph.

# Note: You can create a Horse instance from an iterable
# by using either Horse._make(iterable) or Horse(*iterable).

# Note: Putting mutable items in a tuple is not a good pattern.
