module ProteinTranslation (proteins) where

-- | 将 RNA 序列翻译成蛋白质序列
proteins :: String -> Maybe [String]
proteins rna
  | length rna `mod` 3 /= 0 = Nothing -- 1. 长度检查
  | otherwise = Just (translateCodons (chunksOf 3 rna)) -- 2. 分割并翻译
  where
    -- 辅助函数：将密码子列表翻译成氨基酸列表
    translateCodons :: [String] -> [String]
    translateCodons [] = []
    translateCodons (codon:rest)
      | isStopCodon codon = [] -- 3. 遇到终止密码子，立即返回空列表
      | otherwise = codonToProtein codon : translateCodons rest -- 4. 正常翻译

    -- 辅助函数：判断是否为终止密码子
    isStopCodon :: String -> Bool
    isStopCodon codon = codon `elem` ["UAA", "UAG", "UGA"]

    -- 辅助函数：将单个密码子翻译成氨基酸
    codonToProtein :: String -> String
    codonToProtein "AUG" = "Methionine"
    codonToProtein "UUU" = "Phenylalanine"
    codonToProtein "UUC" = "Phenylalanine"
    codonToProtein "UUA" = "Leucine"
    codonToProtein "UUG" = "Leucine"
    codonToProtein "UCU" = "Serine"
    codonToProtein "UCC" = "Serine"
    codonToProtein "UCA" = "Serine"
    codonToProtein "UCG" = "Serine"
    codonToProtein "UAU" = "Tyrosine"
    codonToProtein "UAC" = "Tyrosine"
    codonToProtein "UGU" = "Cysteine"
    codonToProtein "UGC" = "Cysteine"
    codonToProtein "UGG" = "Tryptophan"
    -- 注意：这里没有为终止密码子定义，因为它们会在 isStopCodon 中被处理掉。

-- 辅助函数：将列表按指定大小分组
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)