module hw1
import StdEnv

////////////////////////////////// Task 1
f1 :: Int -> Int
f1 n
|n == 0 = 0
= n*n + f1 (n-1)

//Start = sum1 3

////////////////////////////////// Task 2
// I have 2 more alternatives to make sure.

f2 :: Int Int -> Int
f2 n k
|k == 0 = 1
|n < k = 0
|otherwise = n * f2 (n-1)(k-1) / k

Start = f2 4 10

//Alternative 2

/* f2 :: Int ->Int 
f2 n
|n == 0 = 1
= n * f2 (n-1) 

result :: Int Int -> Int
result n k = f2 n / ( f2 k * f2 (n-k))

Start = result 5 2 */


//Alternative 3
/*
// 1. Find n! / (n-k)!
comb :: Int Int ->Int 
comb n k
|k == 1 = n
= (n - (k-1)) * comb n (k-1) 

// 2. Find k!
fact :: Int -> Int
fact x
|x == 1 = 1
= x * fact (x-1)

// 3. Finally n choose k  = [n! / (n-k)!] / k!
result :: Int Int -> Int
result n k = comb n k / fact k

Start = result 4 2 */
























