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

main = do
    print (cylinder 12.11 33.22)
    print (maximum' [10,11,12])
    print (replicate' 12 22)
    print (take' 3 [1,2,3,4,5])


