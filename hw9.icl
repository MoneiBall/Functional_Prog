module hw9
import StdEnv

//A : Sadi Mamedov

:: Tree a = Node a (Tree a) (Tree a)
            | Leaf
  
aTree = Node 2 (Node 1 (Node 7 Leaf Leaf)(Node 4 Leaf Leaf))
               (Node 5 (Node 9 Leaf Leaf) Leaf)
largeTree = Node 4 (Node 2(Node 1 Leaf Leaf)(Node 3 Leaf Leaf))
                   (Node 6(Node 5 Leaf Leaf)(Node 17 Leaf Leaf))      

emptyTree = Leaf 
               
// Define the type Tree Int and the type Q.

// 1. Write a function that returns the maximum value of the values stored

// in a binary tree. Assume all values are positive; return -1 if the tree is empty.

TreeToList :: (Tree Int) -> [Int]
TreeToList Leaf = []
TreeToList (Node x l r) = [x] ++ TreeToList l ++ TreeToList r //collecting in a list


f1 :: (Tree Int) -> Int
f1 Leaf = -1
f1 (Node x l r) = last (sort (TreeToList (Node x l r))) //max of list -> max of tree

//Start = f1 aTree 
//Start = f1 emptyTree
//Start = f1 largeTree
// 2. Find the left tree of a node

f2 :: Int (Tree Int) -> [Tree Int]
f2 n Leaf = []
f2 n (Node x l r)
|n == x = [l] ++ f2 n l ++ f2 n r 
= f2 n l ++ f2 n r
//Start = f2 4 (largeTree)
//Start = f2 2 (aTree)

// 3. Find the right tree of a node.

f3 :: Int (Tree Int) -> [Tree Int]
f3 n Leaf = []
f3 n (Node x l r)
|n == x = [r] ++ f3 n l ++ f3 n r 
= f3 n l ++ f3 n r
//Start = f3 1 (aTree)
//Start = f3 3 (largeTree)

// 4. Given two numbers check if they are brothers, i.e. the children of a same parent.


f4 :: Int Int (Tree Int) -> Bool
f4 a b Leaf = False            //- no children at all
f4 a b (Node x l Leaf) = False //- only left child
f4 a b (Node x Leaf r) = False //- only right child
f4 a b (Node x l r)
| (a == NodeVal l) && (b == NodeVal r) = True
= (f4 a b l) || (f4 a b r) //- until true condition; otherwise false
  where NodeVal (Node x l r) = x

//Start = f4 7 5 aTree
//Start = f4 1 3 largeTree



:: Q = { nom :: Int
        ,den :: Int }
Q1::Q
Q1 = {nom = 1, den = 2} // 1/2
Q2::Q
Q2 = {nom = 2, den = 3} // 2/3
Q3::Q
Q3 = {nom = 3, den = 9} // 3/9

simplify :: Q -> Q
simplify {nom=n,den=d}
  | d == 0 = abort " division by 0 not defined"
  | d < 0  = { nom = ~n/g, den = ~d/g}
  | otherwise =  { nom = n/g, den = d/g}
  where g = gcdm n d
  
gcdm :: Int Int -> Int
gcdm x y = gcdnat (abs x) (abs y)
  where gcdnat x 0 = x
        gcdnat x y = gcdnat y (x rem y)       
// 5. Check about a rational number if it is irreducible.

f5 :: Q -> Bool
f5 {nom = n, den = d}
|gcdm n d == 1 = True // a fraction n/d is irreducible 
= False               //   if and only if n and d are coprime
//Start = f5 Q2
//Start = f5 Q3

// 6. Define maxQ for finding the maximum of two rational numbers.
f6 :: Q Q -> Q
f6 {nom = n1, den = d1} {nom = n2, den = d2}
|(n1 * d2) > (n2 * d1) = {nom = n1, den = d1}
= {nom = n2, den = d2}
//Start = f6 Q3 Q1
//Start = f6 Q2 (simplify Q3)

// 7. Find the inverse of a rational number.
f7 :: Q -> Q
f7 {nom = n, den = d} = {nom = d, den = n}
//Start = f7 Q1
//Start = f7 (simplify Q3)








