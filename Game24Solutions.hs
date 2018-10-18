-- Proves every possible solution for Game 24
module Game24Solutions where 


-- data structure representing all possible basic arithmetic expressions
data Expr = Const Double
          | Add Expr Expr 
          | Mul Expr Expr
          | Sub Expr Expr  
          | Div Expr Expr 
          deriving (Show)


-- evaluates an expression tree in the obvious way. divide by 0 results in Nothing (the value)
eval :: Expr -> Maybe Double 
eval (Const n) = Just n
eval (Add m n) = (+) <$> eval m <*> eval n
eval (Mul m n) = (*) <$> eval m <*> eval n
eval (Sub m n) = (-) <$> eval m <*> eval n
eval (Div m n) = case eval n of 
                   Just 0 -> Nothing
                   ev_n   -> (/) <$> eval m <*> ev_n   
                   

-- all possible unordered quads of naturals from 0 to 9 inclusive, 
--  numbers in quad ordered from lowest to highest
allStarts :: [(Double, Double, Double, Double)]
allStarts = [(i, j, k, l) | i <- [0..9], j <- [i..9], k <- [j..9], l <- [k..9]]


-- all quads in allStarts that can produce 24 via basic arithmetics
possibleStarts :: [(Double, Double, Double, Double)]
possibleStarts = filter get24 allStarts


-- all quads in allStarts that's not in possibleStarts
impossibleStarts :: [(Double, Double, Double, Double)]
impossibleStarts = filter (not . get24) allStarts


-- Some expressions are repeated
allAttempts :: [Expr]
allAttempts =
    do  quad <- allStarts
        genExprs quad


passedAttempts :: [Expr]
passedAttempts = filter eq24 allAttempts


getSolutions :: (Double, Double, Double, Double) -> [Expr]
getSolutions = filter eq24 . genExprs 


generalPossibleStarts :: Double -> [(Double, Double, Double, Double)]
generalPossibleStarts n = filter (any (== (Just n)) . map eval . genExprs) allStarts


get24 :: (Double, Double, Double, Double) -> Bool
get24 = any eq24 . genExprs


eq24 :: Expr -> Bool
eq24 = (== (Just 24)) . eval 


-- Some expressions are repeated
genExprs :: (Double, Double, Double, Double) -> [Expr]
genExprs (i, j, k, l) =
    do  ((n1, n2), (n3, n4)) <- [((i, j), (k, l)), ((i, k), (j, l)), ((i, l), (j, k)), 
                                ((j, k), (i, l)), ((j, l), (i, k)), ((k, l), (i, j))]
        (expr1, m1, m2) <- [(Add (Const n1) (Const n2), n3, n4), 
                            (Mul (Const n1) (Const n2), n3, n4), 
                            (Sub (Const n1) (Const n2), n3, n4),
                            (Sub (Const n2) (Const n1), n3, n4),
                            (Div (Const n1) (Const n2), n3, n4),
                            (Div (Const n2) (Const n1), n3, n4)]
        (expr2, m3) <- [(Add expr1 (Const m1), m2),
                        (Mul expr1 (Const m1), m2),
                        (Sub expr1 (Const m1), m2),
                        (Sub (Const m1) expr1, m2),
                        (Div expr1 (Const m1), m2),
                        (Div (Const m1) expr1, m2)]
        [Add expr2 (Const m3),
         Mul expr2 (Const m3),
         Sub expr2 (Const m3),
         Sub (Const m3) expr2,
         Div expr2 (Const m3),
         Div (Const m3) expr2]
       
