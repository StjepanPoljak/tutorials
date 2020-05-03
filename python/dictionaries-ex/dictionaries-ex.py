# Python3 Dictionary Examples

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

# Note: It is possible to store lists and
# dictionaries in dictionaries (and vice versa).
