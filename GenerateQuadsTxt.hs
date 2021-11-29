-- file for generating the (im)possible quads text files
import Game24Solutions ( possibleStarts, impossibleStarts )
import Data.List ( intercalate )
import Data.List.Split (chunksOf)

possibleQuadsFile :: [Char]
possibleQuadsFile = "PossibleQuads.txt"

impossibleQuadsFile :: [Char]
impossibleQuadsFile = "ImpossibleQuads.txt"

roundQuad :: (Double, Double, Double, Double) -> (Int, Int, Int, Int)
roundQuad (x, y, z, w) = (round x, round y, round z, round w)

possibleQuads :: [(Int, Int, Int, Int)]
possibleQuads = map roundQuad possibleStarts 

impossibleQuads :: [(Int, Int, Int, Int)]
impossibleQuads = map roundQuad impossibleStarts 

quadsToTxt :: [(Int, Int, Int, Int)] -> String
quadsToTxt = intercalate "\n" . map (intercalate "   ") . chunksOf 4 . map show

main :: IO ()
main = do
    writeFile possibleQuadsFile (quadsToTxt possibleQuads)
    writeFile impossibleQuadsFile (quadsToTxt impossibleQuads)


