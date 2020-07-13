--Mohanad Omar Mostafa Abotaleb 20214054
--Abhinav George Basil 20205163

chomp :: String -> String
chomp str1 = takeWhile (== head str1) str1

munch :: String -> String
munch str1 = if length str1 > 9
             then take 9 (chomp str1)
             else chomp str1

runs :: String -> [String]
runs "" = []
runs xs = run1 : runs (drop (length run1) xs)
  where run1 = munch xs


encode :: String -> [(Char,Int)]
encode "" = []
encode xs = [(head x, length x ) | x <- runs xs]


flatten :: [(Char,Int)] -> String
flatten [] = []
flatten ((a,b):xs) = a : show b ++ flatten xs

compress :: String -> String
compress str1 = flatten(encode str1)

decode :: [(Char,Int)] -> String
decode [] = []
decode ((a,b):xs) = (take b(repeat a)) ++ decode xs

expand :: String -> [(Char,Int)]
expand str1 = [(a, (read [b] :: Int)) | (a,b) <- zip str1 (tail str1), elem b "123456789"]

decompress :: String -> String
decompress str1 = decode(expand str1)