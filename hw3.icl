module hw3

import StdEnv

/**1. Using foldr or foldl, write a function that will return true if every element in a 
list is even.For empty list, the function should return False. */

AllEven :: [Int] -> Bool
AllEven [] = False
AllEven x = foldr (&&) True (map isEven x)

//Start = AllEven[2,3,5,3,5,2,1]
//Start = AllEven [1..4] //False
//Start = AllEven [2,4,6,8,10] //True
//Start = AllEven [] //False

/**2. Given a list of two numbers n and k, generate a list of k pieces of multiples of n.
For lists with only 1 member it should return that number back. For empty list it should return an empty list. */

//Start = func 2 3
 
func n 0 = []
func n k = map ((+) n) [0 : func (n) (k-1)]

Multiples :: [Int] -> [Int]
Multiples [n] = [n]
Multiples [] = []
Multiples [n,k] = func n k
   
//Start = Multiples [2,3] //[2,4,6] 
//Start = Multiples [4,8] //[4,8,12,16,20,24,28,32]
//Start = Multiples [1,1] //[1]
//Start = Multiples [4] //[4]
//Start = Multiples [] //[]

/**3. Write a function that will calculate the average from the maximum elements of sublists.
Empty sublists will be ignored. Empty case is required, when the output must be -1. */
ignor :: [[Int]] -> [[Int]]
ignor [] = []
ignor [x:xs]
|x == [] = ignor xs
= [x : ignor xs]

MaxAverages :: [[Int]] -> Int
MaxAverages [[]] = -1
MaxAverages x = foldr (+) 0 (map maxList (ignor x)) / length (ignor x)

//Start = MaxAverages [ [4,2,5] , [2,4,1] , [1,2,3], [1..8]] //5
//Start = MaxAverages [ [] ]// -1
//Start = MaxAverages [[1], [0], [1], []] // 0
