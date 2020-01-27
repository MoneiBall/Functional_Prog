module reddots
import StdEnv


//A : Sadi Mamedov


// 1. // Given a collection of n lists of elements, // take the first k (k belongs to [1,n]) elements from each list and sort them increasingly.

takeAndSort :: [[t]] -> [t] | Ord t
takeAndSort x = sort (flatten [(take k) a \\ a <- x & k <- [1..length(x)]])

//Start = takeAndSort [[1], [2,3], [4,5,6]]// [1,2,3,4,5,6]
//Start = takeAndSort [[1..], [2..], [4..]] // [1,2,3,4,5,6]
//Start = takeAndSort [[],[1], [10], [-1,-2]] // [-2,-1,1,10]
//Start = takeAndSort [[4,10..], [-10,-20..], [1..], [10,9..], [100,0..]] // [-300,-200,-100,-20,-10,0,1,2,3,4,7,8,9,10,100]
//Start = takeAndSort [['A'..], ['Z', 'C'..], ['P', 'E'..], ['E', 'L', 'T', 'E']] // [':','A','C','E','E','E','L','P','T','Z']

// 2.
// You are standing at a magical well. // It has two positive integers written on it: a and b. // Each time you cast a magic marble into the well, it gives you a * b dollars and then both a and b increase by 1. // You have n magic marbles. How much money will you make?

// Example

// For a = 1, b = 2, and n = 2, the output should be // magicalwell a b n = 8.

// You will cast your first marble and get $2, after which the numbers will become 2 and 3. When you cast your second marble, the well will give you $6. Overall, you'll make $8. So, the output is 8.

magicalwell :: Int Int Int -> Int
magicalwell a b 0 = 0
magicalwell a b n = a*b + magicalwell (a+1) (b+1) (n-1)

//Start = magicalwell 1 2 2 // 8 
//Start = magicalwell 1 1 1 // 1 
//Start = magicalwell 6 5 3 // 128

// 3. // Given a list of three points as tuples, determine if they span a plane. // Tip: A plane is spanned iff the points are not colinears

isPlane :: [(Int,Int)] -> Bool
isPlane [t1,t2,t3] // *points x,y,z are collinear iff (y2 - y1)(x3 - x2) = (y3 - y2)(x2 - x1) 
|(snd(t2)-snd(t1)) * (fst(t3) - fst(t2)) == (snd(t3)-snd(t2)) * (fst(t2) - fst(t1)) = False
= True 

//Start = isPlane [(0,0),(0,1),(0,2)] // False 
//Start = isPlane [(0,0),(0,1),(0,-1)] // False 
//Start = isPlane [(0,0),(0,1),(0,5)] // False 
//Start = isPlane [(50,1),(2,1),(1,0)] // True

// 4. // Given a list of sublists, remove all sublists whose integers add up to a fibonacci sequence number.
//A number is Fibonacci if and only if one of (5*n2 + 4) or (5*n2 – 4) is an integer 
isFibo :: Int -> Bool
isFibo n = size (toString (sqrt(5.0*(toReal n)*(toReal n) + 4.0))) < 5 || size (toString (sqrt(5.0*(toReal n)*(toReal n) - 4.0))) < 5

removeSumFib :: [[Int]] -> [[Int]]
removeSumFib [] = [[]]
removeSumFib [x:xs]
|isFibo (sum x) = removeSumFib xs
= [x : removeSumFib xs]

//Start = removeSumFib [[~10,4,40], [10,3,0], [1,100,43]]
//Start = removeSumFib [[~10,4,40], [10,3,0], [1,10,3], [10,2], []] //[[1,10,3], [10,2], []] 
//Start = removeSumFib [[~10,4,40], [10,3,0], [1,100,43]] //[[]] 
//Start = removeSumFib [[~10,4,40], [10,3,0], [1], [30,~20,79], [32]] //[[32]]

// 5. // Roots of unity // mathematical definition: // A root of unity is a complex number that, when raised to a positive integer power n, results in 1 . // for this exercise assume 1 <= n <= 100. // Given a complex number, return true if it's a root of unity
:: C = { re :: Real
        ,im :: Real }

mkC n d = { re = n
          , im = d }
          
isRoot :: C -> Bool
isRoot x //Absolute value of Z should be 1
| sqrt(x.re^2.0 + x.im^2.0) <> 1.0 = False
= True
//Start = isRoot (mkC 1.0 4.0) //False
//Start = isRoot (mkC 0.0 (~1.0)) //True



// 6. // Given two complex numbers, return their multiplication. // Multiplication of complex numbers is defined as followed: // (a + bi)(c + di) = ac + adi + bci + cdi^2 // but since i^2 := -1, it will be (ac - cd) + (ad+bc)i

mulC :: C C -> C
mulC x y = mkC (x.re * y.re - x.im * y.im) (x.re * y.im + x.im * y.re)

//Start = mulC (mkC 1.0 4.0) (mkC 3.0 2.0)// (C -5 14)
//Start = mulC (mkC 2.0 6.0) (mkC 2.0 4.0)// (C -20 20)
