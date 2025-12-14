module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = case mapM toRNABase xs of
              Left err -> Left err
              Right rna -> Right rna

toRNABase :: Char -> Either Char Char
toRNABase 'G' = Right 'C'
toRNABase 'C' = Right 'G'
toRNABase 'T' = Right 'A'
toRNABase 'A' = Right 'U'
toRNABase c = Left c
