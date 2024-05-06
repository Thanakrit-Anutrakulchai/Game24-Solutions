# Game24-Solutions
## All possible starting quads to Game 24/Maths 24/24-Cards

Lists every single quads of four natural numbers from 0 to 9 inclusive that, once used exactly once
each to add, subtract, multiply, and divide with each other, can result in 24 
(as well as ones that can not). 

Note that different permutations of the same four numbers are not repeated. 
The numbers are ordered in ascending order.
For example, (2, 9, 7, 4) and (4, 2, 7, 9) are not listed —only (2, 4, 7, 9) is.

The Haskell code used to generate these quads are also given. 
The file generates a list of all possible expression trees produced by each quad and tests whether 
there is a single expression tree that evaluates to 24. 
If there is, the quad is considered a 'possible' one.

The `generalPossibleStarts` function is provided as an extra —it can be used to compute the 
possible quads for any 'game' given the same rule, but with a different natural number as a goal 
e.g. 32 instead of 24.

Currently, there is a QuickCheck test which generates random expression trees with four numbers 
and tests that the expression is covered by the rest of the code i.e. that it evaluates to
the same value as a non-randomly generated expression tree. The quad files are only written
if this test passes.
