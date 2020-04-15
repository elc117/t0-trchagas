isBin :: String -> Bool
isBin "" = True
isBin str = if head str == '0' || head str == '1'
    then isBin (tail str) 
    else False

isBin' :: String -> Bool
isBin' str = if length [c | c <- str, notElem c "10"] == 0 && str/="" 
    then True 
    else False

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [] exp = 0
auxBin2Dec lis exp = head lis*(2^exp) + auxBin2Dec (tail lis) (exp-1)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec' :: [Int] -> Int
bin2dec' lis = sum(zipWith(*) lis exp)
    where exp = reverse [2^x | x<- [0..(length lis) -1]]


dec2binAux :: Int -> [Int]
dec2binAux 0 = []
dec2binAux x
    | x `mod` 2 == 1 = 1 : dec2binAux(x `div` 2)
    | x `mod` 2 == 0 = 0 : dec2binAux(x `div` 2)

dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin x = reverse (dec2binAux x)

isHex :: String -> Bool
isHex str = if length[c | c <- str, notElem c "0123456789ABCDEF"] == 0 
    then True 
    else False





