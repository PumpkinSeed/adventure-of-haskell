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

main = do
    print (cylinder 12.11 33.22)
    print (maximum' [10,11,12])


