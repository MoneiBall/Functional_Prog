module hw2
import StdEnv

//Task 1

pal :: Int -> [Int]
pal 0 = []
pal x = [x rem 10 : pal (x/10)] 

//Start = pal 45654

result :: Int -> Bool
result x = pal x == reverse (pal x)

//Start = result 5

//Task 2
// 1. Find [argument ++ density ++ percentage]
f2 :: [Int] Int -> [[Int]]
f2 [] total = []
f2 n total = [[hd n] ++ [length( filter( (==) (hd n)) n )] ++ [length( filter( (==) (hd n)) n ) * 100 / total] ] ++ f2 (filter ( (<>) (hd n)) (tl n)) total
// 2. Convert into lists of list [[ ]]
MakeFrequenceTable :: [Int] -> [[Int]]
MakeFrequenceTable [] = [[]] 
MakeFrequenceTable  n = f2 n (length n)

Start = MakeFrequenceTable [1..10]
//Start = MakeFrequenceTable []
//Start = MakeFrequenceTable [1,2]
