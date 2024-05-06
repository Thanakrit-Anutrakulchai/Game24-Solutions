-- file for generating the (im)possible quads text files
module Main where

import Data.List ( intercalate )
import Data.List.Split (chunksOf)

import Game24Solutions ( possibleStarts, impossibleStarts )
import Tests ( runTests )

possibleQuadsFile :: String
possibleQuadsFile = "PossibleQuads.txt"

impossibleQuadsFile :: String
impossibleQuadsFile = "ImpossibleQuads.txt"

roundQuad :: (Double, Double, Double, Double) -> (Int, Int, Int, Int)
roundQuad (x, y, z, w) = (round x, round y, round z, round w)

possibleQuads :: [(Int, Int, Int, Int)]
possibleQuads = map roundQuad possibleStarts 

impossibleQuads :: [(Int, Int, Int, Int)]
impossibleQuads = map roundQuad impossibleStarts 

showQuads :: [(Int, Int, Int, Int)] -> String
showQuads = intercalate "\n" . map (intercalate "   ") . chunksOf 4 . map show

main :: IO ()
main = do
    testsPassed <- runTests
    if testsPassed
        then do 
            writeFile possibleQuadsFile (showQuads possibleQuads)
            writeFile impossibleQuadsFile (showQuads impossibleQuads)
        else
            fail "Failed a test case!"
