# Python3 Number Data Model Examples

class ModularInt():

    def __init__(self, val, mod):
        self.mod = mod
        self.val = val

        if val > 0 and val >= mod:
            self.val = val % mod
        elif val < 0:
            self.val += (-val // mod) * (mod * 2)

    def __str__(self):

        return "%d_[%d]" % (self.val, self.mod)

    def __repr__(self):

        return "ModularInt(%d, %d)" % (self.val, self.mod)

    def check_mod(self, other):

        if other.mod != self.mod:
            raise Exception(
                    "The modular integers " + str(self) +
                    " and " + str(other) + " belong to " +
                    "different groups."
                    )

    def __add__(self, other):
        self.check_mod(other)

        return ModularInt((self.val + other.val) % self.mod, self.mod)

    def __mul__(self, other):
        self.check_mod(other)

        return ModularInt((self.val * other.val) % self.mod, self.mod)

test_list = [
        ("+", ((3, 7), (5, 7))),
        ("*", ((3, 7), (7, 7))),
        ("+", ((3, 5), (3, 3))),
        ("*", ((-11, 7), (4, 7)))
        ]

for test in test_list:
    op, (x, y) = test

    try:
        mx = ModularInt(*x)
        my = ModularInt(*y)
        mz = mx + my if op == "+" else mx * my if op == "*" else None
        print("%s %s %s = %s" % (str(mx), op, str(my), str(mz)))
        #print("{} {} {} = {}".format(mx, op, my, mz))

    except:
        print(str(mx) + " and " + str(my) + " belong to different groups!")

        continue

# if __iadd__ (in-place addition) and __imul__ (in-place multiplacation)
# are not implemented, then a += b and a *= b will assign new objects
# to a, as demonstrated in the example below

ma = ModularInt(1, 5)
mb = ModularInt(4, 5)
print("id(ma) = {}".format(id(ma)))
ma += mb
print("id(ma) = {} (after ma += mb)".format(id(ma)))

print(
        "\n"
        "For more information on numeric data model:\n\n\t"
        "https://docs.python.org/3/reference/"
        "datamodel.html#emulating-numeric-types"
        "\n"
        )


