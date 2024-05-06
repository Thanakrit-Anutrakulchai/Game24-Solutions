{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck
import Game24Solutions

size :: Expr -> Int
size (Const _) = 1
size (Add e1 e2) = size e1 + size e2
size (Mul e1 e2) = size e1 + size e2
size (Sub e1 e2) = size e1 + size e2
size (Div e1 e2) = size e1 + size e2

listToQuad :: [Double] -> (Double, Double, Double, Double)
listToQuad [x, y, z, w] = (x, y, z, w)
listToQuad ls = error $ "Expected a list of four numbers, but got: " ++ show ls

numbers :: Expr -> [Double]
numbers (Const d) = [d]
numbers (Add e1 e2) = numbers e1 ++ numbers e2
numbers (Mul e1 e2) = numbers e1 ++ numbers e2
numbers (Sub e1 e2) = numbers e1 ++ numbers e2
numbers (Div e1 e2) = numbers e1 ++ numbers e2

-- ensures all valid expression trees are accounted for
--   the trees are evaluated before comparing equality since some expressions
--   are purposely omitted from genExprs (i.e. since addition is commutative)
prop_size4InGenExprs :: Expr -> Property
prop_size4InGenExprs expr = 
    size expr == 4 ==> 
        elem (eval expr) (eval <$> genExprs (listToQuad $ numbers expr))

-- the `return []` is absolutely baffling boilerplate, for info see:
--   https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html#v:quickCheckAll
return [] 
runTests :: IO Bool
runTests = $forAllProperties $
    quickCheckWithResult (stdArgs {maxSuccess = 50, maxDiscardRatio = 300} )
