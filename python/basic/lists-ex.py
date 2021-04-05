# Python3 List Examples

empty_list = []

print(empty_list)
# []

print([True, 1, "Test", 1.5])
# [True, 1, 'Test', 1.5]

if empty_list:
    print("List is not empty")
else:
    print("List is empty")
# List is empty

fruit = ["banana", "pineapple", "apple", "cherry"]

print(fruit)
# ['banana', 'pineapple', 'apple', 'cherry']

print(fruit == ["banana", "pineapple", "apple", "cherry"])
# True

print(fruit == ["banana", "pineapple", "cherry", "apple"])
# False

print(fruit[0] + " and " + fruit[2])
# banana and apple

print(fruit[-1] + " and " + fruit[-3])
# cherry and pineapple

fruit.append("tomato")
print(fruit)
# ['banana', 'pineapple', 'apple', 'cherry', 'tomato']

del fruit[-1]               # remove by position
fruit.remove("pineapple")   # remove by value
print(fruit)
# ['banana', 'apple', 'cherry']

fruit.insert(1, "pineapple")
print(fruit)
# ['banana', 'pineapple', 'apple', 'cherry']

last_fruit = fruit.pop()
print(last_fruit)
# cherry

second_fruit = fruit.pop(1)
print(second_fruit)
# pineapple

vegetables = ['spinach', 'tomato', 'cucumber']

vegetables.reverse()
print(vegetables)
# ['cucumber', 'tomato', 'spinach']

# The following example shows the standard Python
# pattern where object.method() usually modifies
# the object in place while function(object) returns
# a new object.

print(sorted(vegetables))
# ['cucumber', 'spinach', 'tomato']

print(vegetables)
# ['cucumber', 'tomato', 'spinach']

vegetables.sort()
print(vegetables)
# ['cucumber', 'spinach', 'tomato']

vegetables.extend(['broccoli', 'cabbage'])
# ['cucumber, 'spinach', 'tomato', 'broccoli', 'cabbage']

animals = ['dog', 'cat', 'mouse']

animals.sort(reverse=True)
print(animals)
# ['mouse', 'dog', 'cat']

print(sorted(animals, key=len))
# ['dog', 'cat', 'mouse']

print(len(animals))
# 3

print("Animal list:")
for animal in animals:
    print("\t* " + animal)
# Animal list:
#       * mouse
#       * dog
#       * cat

print("Enumerated animal list:")
for index, animal in enumerate(animals, start=1):
    print("\t" + str(index) + ". " + animal)
# Enumerated animal list:
#       1. mouse
#       2. dog
#       3. cat

print(list(range(1, 5)))
# [1, 2, 3, 4]

print("First three numbers:")
for value in range(1, 4):
    print("\t* " + str(value))
# First three numbers:
#       * 1
#       * 2
#       * 3

odd_numbers = list(range(11, 21, 2))
print("Odd numbers in [10, 20]: " + str(odd_numbers))
# Odd numbers in [10, 20]: [11, 13, 15, 17, 19]

print(min(odd_numbers))
# 11

print(max(odd_numbers))
# 19

print(sum(odd_numbers))
# 75

# <-- list comprehension -->

triple_even_numbers = [3 * value for value in range(1,11,2)]
print(triple_even_numbers)
# [3, 9, 15, 21, 27]

# <-- slicing a list -->

print(triple_even_numbers[1:4])
# [9, 15, 21]

print(triple_even_numbers[:3])
# [3, 9, 15]

# Note: This is the same as
# triple_even_numbers[0:3]
# triple_even_numbers[:-2]

print(triple_even_numbers[3:])
# [21, 27]

# Note: This is the same as
# triple_even_numbers[3:len(triple_even_numbers)]
# triple_even_numbers[-2:]

print("Some numbers:")
for number in triple_even_numbers[2:4]:
    print("\t* " + str(number))
# Some numbers:
#       * 15
#       * 21

triple_even_numbers_copy = triple_even_numbers[:]

del triple_even_numbers_copy[0]
del triple_even_numbers[-1]

print(triple_even_numbers_copy)
# [9, 15, 21, 27]

print(triple_even_numbers)
# [3, 9, 15, 21]

print(", ".join(["a", "b", "c"]))
# a, b, c

some_names = [ "John", "Jim", "James", "Jim", "Jim", "Jerry" ]

while "Jim" in some_names:
    some_names.remove("Jim")

print(some_names)
# ['John', 'James', 'Jerry']

some_names.extend(['John', 'Jacob', 'Joseph'])
print(some_names)
# ['John', 'James', 'Jerry', 'John', 'Jacob', 'Joseph']

x = [1, 2, 3]
y = x       # pass by reference
x.append(4)
print(x)
print(y)
# [1, 2, 3, 4]
# [1, 2, 3, 4]

z = x[:]    # copy / pass by value
x.append(5)
print(x)
print(z)
# [1, 2, 3, 4, 5]
# [1, 2, 3, 4]

# <-- bisect and insort -->

import bisect

# bisect.bisect(haystack, needle) returns the place in a
# haystack (sorted array) where needle (a new element)
# should be placed so it remains sorted (basically, does
# a binary search)

some_names.sort()
print(some_names)
# ['Jacob', 'James', 'Jerry', 'John', 'John', 'Joseph']

print(bisect.bisect(some_names, "Jamie"))
# 2

# Note: bisect() is an alias for bisect_right()

print(bisect.bisect_right(some_names, "James"))
# 2

print(bisect.bisect_left(some_names, "James"))
# 1

# Note: You can narrow the search by providing lo and hi
# optional arguments (defaults: lo=0, hi=len(haystack))

# insort does insertion sort

bisect.insort(some_names, "Jamie")
print(some_names)
# ['Jacob', 'James', 'Jamie', 'Jerry', 'John', 'John', 'Joseph']
