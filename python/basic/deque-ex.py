from collections import deque

# in contrast to list methods such as .append(),
# .pop() and .pop(0), deque is a thread-safe
# double-ended queue designed for fast inserting
# and removing from both ends; it can also serve
# as a circular buffer

dq = deque([4, 6, 2, 3, 7], maxlen=4)
print(list(dq))
# [6, 2, 3, 7]

dq.rotate(2)
print(list(dq))
# [3, 7, 6, 2]

dq.rotate(-2)
print(list(dq))
# [6, 2, 3, 7]

dq.append(1)
print(list(dq))
# [2, 3, 7, 1]

dq.appendleft(6)
print(list(dq))
# [6, 2, 3, 7]

dq.extend([1, 9])
print(list(dq))
# [3, 7, 1, 9]

dq.extendleft([2, 6])
print(list(dq))
# [6, 2, 3, 7]

dq.pop()
print(list(dq))
# [6, 2, 3]

dq.popleft()
print(list(dq))
# [2, 3]

dq.insert(1, 5)
print(list(dq))
# [2, 5, 3]

dq.remove(5)
print(list(dq))
# [2, 3]

del dq[1]
print(list(dq))
# [2]
