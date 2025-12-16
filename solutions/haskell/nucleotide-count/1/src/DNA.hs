module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map,empty,adjust,fromList)
import Control.Monad(foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM processChar initialMap xs
 where 
   initialMap :: Map Nucleotide Int
   initialMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
   
   processChar :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
   processChar acc c = case charToNucleotide c of
                         Left err -> Left err
                         Right nuc -> Right (adjust (+1) nuc acc)

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide c = Left ("Invalid nucleotide: " ++ [c])
