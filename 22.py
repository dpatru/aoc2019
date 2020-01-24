from __future__ import division, print_function
import fileinput

# This solution from reddit: https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbwpk5k?utm_source=share&utm_medium=web2x
n = 119315717514047
c = 2020

a, b = 1, 0
for l in fileinput.input():
    if l == 'deal into new stack\n':
        la, lb = -1, -1
    elif l.startswith('deal with increment '):
        la, lb = int(l[len('deal with increment '):]), 0
    elif l.startswith('cut '):
        la, lb = 1, -int(l[len('cut '):])
    # la * (a * x + b) + lb == la * a * x + la*b + lb
    # The `% n` doesn't change the result, but keeps the numbers small.
    a = (la * a) % n
    b = (la * b + lb) % n

print (a,b)

M = 101741582076661
# Now want to morally run:
# la, lb = a, b
# a = 1, b = 0
# for i in range(M):
#     a, b = (a * la) % n, (la * b + lb) % n

# For a, this is same as computing (a ** M) % n, which is in the computable
# realm with fast exponentiation.
# For b, this is same as computing ... + a**2 * b + a*b + b
# == b * (a**(M-1) + a**(M) + ... + a + 1) == b * (a**M - 1)/(a-1)
# That's again computable, but we need the inverse of a-1 mod n.

# Fermat's little theorem gives a simple inv:
def inv(a, n): return pow(a, n-2, n)

Ma = pow(a, M, n)
Mb = (b * (Ma - 1) * inv(a-1, n)) % n

# This computes "where does 2020 end up", but I want "what is at 2020".
#print((Ma * c + Mb) % n)

# So need to invert (2020 - MB) * inv(Ma)
print(((c - Mb) * inv(Ma, n)) % n)
