module EndtermSolutions
import StdEnv

:: Fighter = { name :: String, tagH :: String, health :: Int, tagA :: String, attack :: Int, tagD :: String, defense :: Int}
:: Tree a = Node a (Tree a) (Tree a)
			| Leaf
:: Month = January | February  | March   | April    | May | June | July
         | August  | September | October | November | December
         
Rick = { name = "Rick Astley", tagH = "Health:", health = 50, tagA = "Attack:", attack = 100, tagD = "Defense:", defense = 10}
Terry = { name = "Terry Crews", tagH = "Health:", health = 100, tagA = "Attack:", attack = 80, tagD = "Defense:", defense = 60}
Hulk = { name = "Bruce Banner", tagH = "Health:", health = 1000, tagA = "Attack:", attack = 1000, tagD = "Defense:", defense = 1000}
Evan = { name = "FP Tutor", tagH = "Health:", health = 50, tagA = "Attack:", attack = 60, tagD = "Defense:", defense = 50}
Viktoria = { name = "FP Professor", tagH = "Health:", health = 50, tagA = "Attack:", attack = 60, tagD = "Defense:", defense = 60}
Rimus = { name = "FP Creator", tagH = "Health:", health = 50, tagA = "Attack:", attack = 70, tagD = "Defense:", defense = 50}

bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))		
shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
noTree = Leaf
unitTree = Node 1337 Leaf Leaf

//Write a function that takes a month and a natural number and iterates the month by that many months.
//That is you will "add" the number to the month.
//For example, monthIterate January 10 = November because November is 10 months after January.
//Additionally monthIterate May 134 = July because 134 is equal to (12*11)+2. Meaning that 2 months
//after May is July.
monthIterate :: Month Int -> Month
monthIterate January 1 = February
monthIterate January n = monthIterate February ((n rem 12)-1)
monthIterate February 1 = March
monthIterate February n = monthIterate March ((n rem 12)-1)
monthIterate March 1 = April
monthIterate March n = monthIterate April ((n rem 12)-1)
monthIterate April 1 = May
monthIterate April n = monthIterate May ((n rem 12)-1)
monthIterate May 1 = June
monthIterate May n = monthIterate June ((n rem 12)-1)
monthIterate June 1 = July
monthIterate June n = monthIterate July ((n rem 12)-1)
monthIterate July 1 = August
monthIterate July n = monthIterate August ((n rem 12)-1)
monthIterate August 1 = September
monthIterate August n = monthIterate September ((n rem 12)-1)
monthIterate September 1 = October
monthIterate September n = monthIterate October ((n rem 12)-1)
monthIterate October 1 = November
monthIterate October n = monthIterate November ((n rem 12)-1)
monthIterate November 1 = December
monthIterate November n = monthIterate December ((n rem 12)-1)
monthIterate December 1 = January
monthIterate December n = monthIterate January ((n rem 12)-1)

//Start = monthIterate January 5 //June
//Start = monthIterate May 134 //July
//Start = monthIterate July 120000002 //September

//Write a function that takes a list of months and sorts the list by order of the months.
//Notably, January should be sorted before February, which should be sorted before March... 
//and so on and so forth.
//Duplicates can be kept.

toNum :: Month -> Int
toNum January = 0
toNum February = 1
toNum March = 2
toNum April = 3
toNum May = 4
toNum June = 5
toNum July = 6
toNum August = 7
toNum September = 8
toNum October = 9
toNum November = 10
toNum December = 11

toMonth :: Int -> Month
toMonth 0 = January
toMonth 1 = February
toMonth 2 = March
toMonth 3 = April
toMonth 4 = May
toMonth 5 = June
toMonth 6 = July
toMonth 7 = August
toMonth 8 = September
toMonth 9 = October
toMonth 10 = November
toMonth 11 = December

monthSort :: [Month] -> [Month]
monthSort list = map toMonth(sort(map toNum list))
//Start = monthSort [February, October, January, June, December, May, April, October] //[January,February,April,May,June,October,October,December]
//Start = monthSort [] //[]

//Given a list of arrays, sort them by their greatest element.
//The order of elements in the arrays must be preserved.
//In the case of arrays with equal greatest elements, their original order in the list must be preserved.
//For example: sortArrays [{2,3,4},{1,2,3},{3,4}] will return [{1,2,3},{2,3,4},{3,4}]
sortArrays :: [{Int}] -> [{Int}]
sortArrays list = sortBy sortAux list
//Start = sortArrays [{4,2,5,6},{1,4,2},{5,2,1,0,3,2}] //[{1,4,2},{5,2,1,0},{4,2,5,6}]
//Start = sortArrays [{1,2,3},{2},{2,5,2},{3,1},{1,2},{0}] //[{0},{2},{1,2},{1,2,3},{3,1},{2,5,2}]
//Start = sortArrays [] //[]

sortAux :: {Int} {Int} -> Bool
sortAux a1 a2 = (maxArray a1) < (maxArray a2)
arrayToList :: {Int} -> [Int]
arrayToList array = [x\\x<-:array]
listToArray :: [Int] -> {Int}
listToArray list = {x\\x<-list}
maxArray :: {Int} -> Int
maxArray array = maxList(arrayToList array)

//Given a month, generate an array of months from that month til the end of the year.
//The months must be in order.
yearEnd :: Month -> {Month}
yearEnd m = {toMonth x\\x<-[(toNum m)..11]}
//Start = yearEnd January //{January,February,March,April,May,June,July,August,September,October,November,December}
//Start = yearEnd July //{July,August,September,October,November,December}
//Start = yearEnd December //{December}

//Given two fighter records, which contains their name, health, attack, and defense,
//write a function that will simulate an attack from fighter A to fighter B.
//Fighter B's health should be depleted by fighter A's attack minus fighter B's defense.
//You MUST use the '&' operator to update fighter B's values.
//Note: if fighter B's defense > fighter A's attack, there should be no health change.
//Note: if fighter B's health would be depleted past 0, then set it to 0. No negative values.
//Extra note: the tag fields, are there for display convenience, they do not need to be touched.
(attacks) :: Fighter Fighter -> Fighter
(attacks) fA fB = attackAux fA fB

attackAux fA fB
| (fA.attack - fB.defense) > fB.health = {fB & health = 0}
| fA.attack > fB.defense = {fB & health = fB.health - (fA.attack - fB.defense)}
= fB

//Start = Rick attacks Terry //(Fighter "Terry Crews" "Health:" 60 "Attack:" 80 "Defense:" 60)
//Start = Terry attacks Rick //(Fighter "Rick Astley" "Health:" 0 "Attack:" 100 "Defense:" 10)
//Start = Evan attacks Rick //(Fighter "Rick Astley" "Health:" 0 "Attack:" 100 "Defense:" 10)
//Start = Hulk attacks Evan //(Fighter "FP Tutor" "Health:" 0 "Attack:" 60 "Defense:" 50)

//Write a function that takes the names of two fighters, and returns the name of the winner
//of a simulated match between them.
//Have them attack each other until someone dies. Yay!
//That is, first have fighter A attack fighter B, then fighter B attacks fighter A,
//then fighter A attacks fighter B, and so on and so forth until one of them dies.
//You MUST use the '&' operator to update fighter values.
//Note: we can assume that the first fighter will perform the first attack.
//Note: observe ALL notes from prior task regarding health value change.
(versus) :: String String -> String
(versus) nameA nameB = (fightAux (toFighter nameA) (toFighter nameB)).name
//Start = "Rick Astley" versus "Terry Crews" //"Terry Crews"
//Start = "Terry Crews" versus "Rick Astley" //"Terry Crews"
//Start = "FP Professor" versus "FP Creator" //"FP Professor"
//Start = "FP Creator" versus "FP Professor" //"FP Creator"
//Start = "FP Tutor" versus "Bruce Banner" //"Bruce Banner"

fightAux fA fB
| (fA attacks fB).health > 0 = fightAux (fA attacks fB) fA
= fA
//Start = fightAux Rick Terry
//Start = fightAux Viktoria Rimus

listFighters = [Rick, Terry, Hulk, Evan, Viktoria, Rimus]
toFighter :: String -> Fighter
toFighter name
| isEmpty filtered = abort "No such fighter"
= hd filtered
	where
		filtered = [x\\x<-listFighters|x.name == name]
//Start = toFighter "FP Tutor"

extractNode :: (Tree Int) -> Int
extractNode (Node x l r) = x

goL :: (Tree Int) -> (Tree Int)
goL (Node x l r) = l
goR :: (Tree Int) -> (Tree Int)
goR (Node x l r) = r
isLeaf :: (Tree Int) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Write a function that takes a tree as a parameter and returns a list of nodes whose children are both Leaf.
// An empty tree will return [] and a single element tree will return a list of one element.
leaves :: (Tree Int) -> [Int]
leaves Leaf = []
leaves (Node x l r)
| isLeaf l && isLeaf r = [x]
= (leaves l)++(leaves r)

//Start = leaves bestTree //[3,12,16,18,20]
//Start = leaves ourTree //[1,8,11,19,24,28]
//Start = leaves unitTree //[1337]
//Start =  leaves noTree //[]

//Write a function that takes a binary search tree and a list of integers and adds the integers
//into the tree, in the order given by the list. Duplicates are not allowed here.
//Note: order of elements in the list changes the resulting tree.
addList :: [Int] (Tree Int) -> (Tree Int)
addList list tree
| isEmpty list = tree
= addList (tl list) (addNode (hd list) tree)
//Start = addList [1,10,14,20] shortTree //(Node 14 (Node 11 (Node 1 Leaf (Node 10 Leaf Leaf)) (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) (Node 20 Leaf Leaf)))
//Start = addList [20,10,1,14] shortTree //(Node 14 (Node 11 (Node 10 (Node 1 Leaf Leaf) Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) (Node 20 Leaf Leaf)))
//Start = addList [2,3,2,1] (Node 3 Leaf Leaf) //(Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf)
//Start = addList [2,5,1,4,3] Leaf //(Node 2 (Node 1 Leaf Leaf) (Node 5 (Node 4 (Node 3 Leaf Leaf) Leaf) Leaf))
//Start = addList [1,2,3,4,5] Leaf //(Node 1 Leaf (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf (Node 5 Leaf Leaf)))))
//Start = addList [] shortTree //(Node 14 (Node 11 Leaf (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) Leaf))
//Start = addList [] Leaf //Leaf

//Write a function that, given a list of elements, creates a balanced binary search tree.
//The tree must be a binary search tree AND the left and right subtrees must have a depth difference of at most 1.
//(Odd number of elements will have equal depths on both sides.)
levelBalance :: [Int] -> (Tree Int)
levelBalance list
| isEmpty list = Leaf
= (Node (sorted!!mid) (levelBalance (take mid sorted)) (levelBalance (drop (mid+1) sorted)))
	where
		sorted = sort (removeDup list)
		mid = (length sorted)/2
//Start = levelBalance [1..10] //(Node 6 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 5 (Node 4 Leaf Leaf) Leaf)) (Node 9 (Node 8 (Node 7 Leaf Leaf) Leaf) (Node 10 Leaf Leaf)))
//Start = levelBalance [2,4,2,6] //(Node 4 (Node 2 Leaf Leaf) (Node 6 Leaf Leaf))
//Start = levelBalance [] //Leaf

addNode :: Int (Tree Int) -> (Tree Int)
addNode n Leaf = (Node n Leaf Leaf)
addNode n (Node x l r)
| n == x = (Node x l r)
| n < x = (Node x (addNode n l) r)
| n > x = (Node x l (addNode n r))

minTree :: (Tree Int) -> Int
minTree tree
| isLeaf(goL tree)= extractNode tree
= minTree (goL tree)

remMin :: (Tree Int) -> (Tree Int)
remMin (Node x Leaf r) = r
remMin (Node x l r)
| extractNode l == minTree (Node x l r) = (Node x (goR l) r)
= (Node x (remMin l) r)

//Write a function that takes a binary search tree, and deletes the root node
//It must preserve the binary search tree property (i.e left is smaller and right is bigger)
//If all nodes are removed you should have a single Leaf.
//Converting the tree to a list and then back to a tree is not permitted.
remRoot :: (Tree Int) -> (Tree Int)
remRoot Leaf = Leaf
remRoot (Node x l r)
| isLeaf l && isLeaf r = Leaf
| isLeaf l = r
| isLeaf r = l
= (Node (minTree r) l (remMin r))

//Start = remRoot ourTree //(Node 18 (Node 3 (Node 1 Leaf Leaf) (Node 10 (Node 7 Leaf (Node 8 Leaf Leaf)) (Node 13 (Node 11 Leaf Leaf) Leaf))) (Node 20 (Node 19 Leaf Leaf) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf)))))
//Start = remRoot bestTree //(Node 11 (Node 6 (Node 1 Leaf (Node 5 (Node 2 Leaf (Node 4 (Node 3 Leaf Leaf) Leaf)) Leaf)) Leaf) (Node 14 (Node 13 (Node 12 Leaf Leaf) Leaf) (Node 17 (Node 15 Leaf (Node 16 Leaf Leaf)) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf)))))
//Start = remRoot unitTree //Leaf
//Start = remRoot noTree //Leaf

isPrime :: Int -> Bool
isPrime n = isEmpty[x\\x<-[2..(n-1)]|n rem x == 0]

//Write a function that takes a condition testing function (such as isPrime, isEven, etc)
//and a binary search tree, and filters the tree according to the condition.
//That is, nodes that match the condition will be kept and nodes that do not match will be removed.
//Converting the tree to a list and then back to a tree is not permitted.
filterTree :: (Int -> Bool) (Tree Int) -> (Tree Int)
filterTree cond Leaf = Leaf
filterTree cond (Node x l r)
| cond x = (Node x (filterTree cond l) (filterTree cond r))
= remRoot (Node x (filterTree cond l) (filterTree cond r))

//Start =  filterTree isEven bestTree //(Node 10 (Node 6 (Node 2 Leaf (Node 4 Leaf Leaf)) Leaf) (Node 14 (Node 12 Leaf Leaf) (Node 18 (Node 16 Leaf Leaf) (Node 20 Leaf Leaf))))
//Start = filterTree isEven ourTree //(Node 18 (Node 10 (Node 8 Leaf Leaf) Leaf) (Node 20 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
//Start = filterTree ((>)10) unitTree //Leaf
//Start = filterTree isOdd noTree //Leaf