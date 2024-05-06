-- Generates every possible solution for all possible quads for Game 24
{-# LANGUAGE InstanceSigs #-}
module Game24Solutions where 

import Data.List (uncons)

import Test.QuickCheck ( Arbitrary(arbitrary), Gen, frequency )

-- data structure representing all possible basic arithmetic expressions
data Expr = Const Double
          | Add Expr Expr 
          | Mul Expr Expr
          | Sub Expr Expr  
          | Div Expr Expr 
          deriving (Show)

instance Arbitrary Expr where
    arbitrary :: Gen Expr
    arbitrary = frequency
        [ 
            (4, Const <$> arbitrary)
        ,   (1, Add <$> arbitrary <*> arbitrary)
        ,   (1, Mul <$> arbitrary <*> arbitrary)
        ,   (1, Sub <$> arbitrary <*> arbitrary)
        ,   (1, Div <$> arbitrary <*> arbitrary)
        ]

-- lists all operations allowed between two expressions
-- Sub,Div are listed twice to account for non-commutativity
allOps :: Expr -> Expr -> [Expr]
allOps exprL exprR = [ Add exprL exprR
                     , Mul exprL exprR
                     , Sub exprL exprR
                     , Sub exprR exprL
                     , Div exprL exprR
                     , Div exprR exprL
                     ]

-- evaluates an expression tree in the obvious way. Division by 0 results in Nothing
eval :: Expr -> Maybe Double 
eval (Const n) = Just n
eval (Add m n) = (+) <$> eval m <*> eval n
eval (Mul m n) = (*) <$> eval m <*> eval n
eval (Sub m n) = (-) <$> eval m <*> eval n
eval (Div m n) = case eval n of 
                   Just 0 -> Nothing
                   ev_n   -> (/) <$> eval m <*> ev_n   
                   

-- all possible quads of naturals from 0 to 9 inclusive, 
--  numbers in quad ordered from lowest to highest
allStarts :: [(Double, Double, Double, Double)]
allStarts = [(i, j, k, l) | i <- [0..9], j <- [i..9], k <- [j..9], l <- [k..9]]

-- all quads in allStarts that can produce 24 via basic arithmetics
possibleStarts :: [(Double, Double, Double, Double)]
possibleStarts = filter get24 allStarts

-- all quads in allStarts that's not in possibleStarts
impossibleStarts :: [(Double, Double, Double, Double)]
impossibleStarts = filter (not . get24) allStarts

-- Generates every single possible expression trees using all possible quads
-- Some expressions are repeated
allAttempts :: [Expr]
allAttempts =
    do  quad <- allStarts
        genExprs quad

-- All expression trees in allAttempts that evaluates to 24
passedAttempts :: [Expr]
passedAttempts = filter eq24 allAttempts

-- Gets all expression trees made from a specific quad the that evaluates to 24
getSolutions :: (Double, Double, Double, Double) -> [Expr]
getSolutions = filter eq24 . genExprs 

-- Lists all possible quads that can generate an expression tree that evaluates to n
generalPossibleStarts :: Double -> [(Double, Double, Double, Double)]
generalPossibleStarts n = filter (any (== Just n) . map eval . genExprs) allStarts

-- Checks if the quad generates at least one expression tree that evaluates to 24
get24 :: (Double, Double, Double, Double) -> Bool
get24 = any eq24 . genExprs

-- Checks if a tree evaluates to (Just) 24
eq24 :: Expr -> Bool
eq24 = (== Just 24) . eval 

-- Lists all ways to choose k elements by position from a list and also return the rest
choose :: Int -> [a] -> [([a], [a])]
choose k _ | k < 0 = error $ "Expected a non-negative integer but given: " ++ show k
choose 0 ls = [([], ls)]
choose k ls =
    case uncons ls of
        Nothing -> []
        Just (i, rest) -> 
            do
                (chosens, leftovers) <- choose k rest 
                return (chosens, i:leftovers) 
            ++ do
                (chosens, leftovers) <- choose (k - 1) rest
                return (i : chosens, leftovers)

-- Lists all ways to choose 2 elements by position from a list and also return the rest
-- This is here for more type safety in genExprs.
chooseTwo :: [a] -> [(a, a, [a])]
chooseTwo ls = do
    ([x, y], rest) <- choose 2 ls
    return (x, y, rest)


-- Generates all possible expression trees from a specific quad
-- Some expressions are repeated
genExprs :: (Double, Double, Double, Double) -> [Expr]
genExprs (i, j, k, l) =
        -- Generates every possible way to choose a pair of numbers to perform an 
        --   operation on while separating the rest
    do  (n1, n2, rest) <- chooseTwo $ map Const [i, j, k, l]
        expr1 <- allOps n1 n2
        -- applies every operation on two of the three numbers from above
        (m1, m2, finalExpr) <- chooseTwo (expr1 : rest)
        expr2 <- allOps m1 m2
        case finalExpr of
            [e] -> allOps expr2 e
            _   -> error $
                    "An unexpected number of subexpressions were generated in genExprs.\n"
                    ++ "These subexpressions were generated:"
                    ++ (([show expr1, show expr2] ++ map show finalExpr) >>= ("\n\n\t"++))
