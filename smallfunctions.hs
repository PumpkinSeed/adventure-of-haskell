-- cylinder getting two value 
-- r is the radius
-- h is the height
-- return the cylinder area
-- using the 'let in' syntax where the let part is define the data 
-- the in part is using the data defined in let and calculate the area
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

-- maximum redefine the built-in maximum function
-- getting an Ord list 
-- return the maximum by a recursion
-- using pattern matching and recursion
-- using guards in the last pattern and the where call the maximum 
-- while the array have more than one element
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

-- replicate redefine the built-in replicate function
-- return an array by replicating the second argument,
-- first argument times
-- recursion using the otherwise while the n is not zero
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- take redefine the built-in take function
-- if the first argument 0 or less than 0 its return empty array
-- if the second argument empty array, return empty array
-- _ means it doesn't care about the parameter, want to check the other
-- otherwise the third pattern break into head and tail
-- concatenat the heads until n element
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- zip redefine the built-in zip function
-- first two argument handle the empty lists
-- third pattern take the heads of the lists and
-- create a tuple of them then call the zip to
-- do it with the tail of the list
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- elem redefine the built-n elem function
-- the first pattern handle empty list
-- the second have guards, return True if the elem
-- is the head of the list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- quicksort implement the quicksort algorithm
-- the first pattern check for empty array
-- the second pattern get the head of the list, take the smaller
-- and the bigger after and before then concat them,
-- the smaller and bigger also sorted recursivly
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- multThree get three parameter and multiple them
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- zipWith redefine the built-in zipWith function
-- the first argument is a function which takes 3 argument
-- the other arguments are lists and the response is also a list
-- the first two pattern checks the lists are not empty
-- the last pattern get the heads and the tails, and run the function
-- what it got from paramter
-- after pass it to itself recursivly
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

main = do
    print (cylinder 12.11 33.22)
    print (maximum' [10,11,12])
    print (replicate' 12 22)
    print (take' 3 [1,2,3,4,5])
    print (zip' [1,2,3] [5,4,3])
    print (elem' 3 [5,4,2,3])
    print (quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9])

    -- multTwoWithNine create a new function because give
    -- too few parameters and it is created a new function on the fly
    let multTwoWithNine = multThree 9
    print (multTwoWithNine 2 3)

    print (zipWith' (+) [4,4,3,6,7] [6,7,5,4,1])


