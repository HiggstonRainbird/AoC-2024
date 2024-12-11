Taken from [my comment here](https://old.reddit.com/r/adventofcode/comments/1hbm0al/2024_day_11_solutions/m1hno3o/):

# Part 3

I suspect - [but can't prove](https://en.wikipedia.org/wiki/Collatz_conjecture) - that all stones eventually converge on the same loop, and that it's possible to compute the answer for 10^100 with an appropriate modulus in O(log(n)) time and O(1) space.

A stone of 0 will finish with a loop of exactly 54 elements, and so will every stone from 1 to 99 (since the one-digit numbers are explicitly in the loop, and the two-digit numbers will split into one-digit numbers.  The first stone that won't is 100, and a stone of 100 creates a loop length of 3811 - which happens to be the same loop length as my own input, and also for every other input I've tested not present in the 54-element loop starting with zero.

If that holds true, then all you need to do is continue iterating mod N until you reach the steady state, and then make a `3811x3811`  transition matrix.  You can then use [modular exponentiation by squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring) to raise the matrix to the power of 10^100.

I don't know if this works for every input, but it works for my input, and also works for the test case of `125 17` - which happens to conveniently be in the 54-element loop and not the 3811-element loop.  And so, with the magic of the [undocumented function `Algebra MatrixPowerMod[]`](https://mathematica.stackexchange.com/a/123241), I believe that for the example (`125 17`):

Blinks | Stones
-------|----------
0      | 2
1      | 3
25     | 55312
75     | 38650482
10^2   | 486382721
10^16  | 519885608
10^100 | 180213553