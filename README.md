# Game24-Solutions
(Proof of) all possible starting quads and solutions to Game 24/Maths 24/24-Cards


Lists every single quads of four natural numbers from 0 to 9 inclusive that, once used exactly once each to multiply and
add with each other, can result in 24 (as well as ones that can not). 


The Haskell code used to generate these quads are also given. The file generates a list of all possible expression trees produced by each
quad and tests whether there is a single expression tree that evaluates to 24. If there is, the quad is considered a 'possible' one.


The generalPossibleStarts function is provided as an extra --it can be used to compute the possible starts for any 'game' given the
same rule, but with a different natural number as a goal e.g. 32 instead of 24.

