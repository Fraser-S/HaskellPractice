lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number 7"
lucky x = "Sorry, you're out of luck, pal"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Not between 1 and 5"

factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1)(x2,y2) = (x1 + x2, y1 + y2)

--example of pattern matching
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

--example of pattern matching
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y 
tell (x:y:_) = "This is a long list. The first two elements are: " ++ show x ++ " and " ++ show y

--example of pattern matching
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

--Example of patterns
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--Example of pattern matching
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30.5 = "You're overweight"
    | otherwise   = "You're obese"
    where bmi = weight / height ^ 2

--Max value using pattern matching
max' :: (Ord a) => a -> a -> a  
max' a b 
    | a > b     = a  
    | otherwise = b  

--An example of pattern matching and 
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT

--Another example of pattern matching
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

--example of comprehension that uses pattern matching
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]-
    where bmi weight height = weight / height ^ 2

--Example of a function that uses let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

--Example of comprehension with a predefined function
calcBmis' :: (RealFloat a) =>[(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
{-
--Example of a case statement
describeList :: [a] -> String
describeList xs = "The List is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"-}