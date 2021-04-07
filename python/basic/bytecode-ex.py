#!/usr/bin/env python3

import dis

print("Disassembly of a + 3:")

dis.dis('a + 3')

# Disassembly of a + 3:
#   1           0 LOAD_NAME                0 (a)
#               2 LOAD_CONST               0 (3)
#               4 BINARY_ADD
#               6 RETURN_VALUE

