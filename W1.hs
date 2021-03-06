module W1 where

-- Week 1:
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

-- Ex 1: define variables one, two and three. They should all have
-- type Int and values 1, 2 and 3. This exercise has no tests.

one :: Integer
one = 1
two :: Integer
two = 2
three :: Integer
three = 3

-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = two * x 

-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = double $ double x

-- Ex 4: define the function poly2. It should take four arguments of
-- type Double, a, b, c, and x and return a*x^2+b*x+c. Give poly2 a
-- type signature, i.e. poly2 :: something.

poly2 :: Double -> Double  -> Double -> Double -> Double
poly2 a b c x = a * x^two + b * x + c

-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

-- Ex 6: fizzbuzz! Define the a function fizzbuzz that returns "Fizz"
-- for numbers divisible by 3, "Buzz" for numbers divisible by 5, and
-- "FizzBuzz" for numbers divisible by both. For other numbers it
-- returns the empty string.
--
-- You can use the function mod to compute modulo.
isDivisible :: Integer -> Integer -> Bool
isDivisible n k =  (mod n k) == 0

fizzbuzz :: Integer -> String
fizzbuzz x 
    | (isDivisible x 5)  && (isDivisible x three) = "FizzBuzz"  
    | (isDivisible x 5) = "Buzz" 
    | (isDivisible x three) = "Fizz" 
    | otherwise = ""
-- Ex 7: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. the type of booleans in haskell is Bool

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

-- Ex 8: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo(n-1)

-- Ex 9: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
power x 0 = one
power x y = x * power x (y-1)

-- Ex 10: ilog2 n should be the number of times you can halve the
-- integer n (rounding down) before you get 1.
--
-- Use recursion to define ilog2. Use the function "div" for integer
-- division.

ilog2 :: Integer -> Integer
ilog2 x = ilog2' 0 x

ilog2' :: Integer -> Integer -> Integer
ilog2' n 1 = n
ilog2' n x = ilog2'(n+1) (div x 2)

-- Ex 11: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial n 0 = one
binomial 0 k = if k > 0 then 0 else (binomial (-1) k) + (binomial (-1) (k-1))
binomial n k = (binomial (n-1) k) + (binomial (n-1) (k-1))

-- Ex 12: The tribonacci numbers are defined by the equations
--
--   T(1) = 1
--   T(2) = 1
--   T(3) = 2
--   T(n+1) = T(n)+T(n-1)+T(n-2)
--
-- Implement an efficient, linear time, recursive function that
-- computes T(n). You'll probably want to define a helper function.

tribonacci :: Integer -> Integer
tribonacci 1 = one
tribonacci 2 = one
tribonacci 3 = two
tribonacci n = tribonacci' two one one n
    where tribonacci' a b c n = if n < 4 then a else tribonacci' (a+b+c) a b  (n-1)

-- Ex 13: implement the euclidean algorithm for finding the greatest
-- common divisor: http://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd a 0 = a
myGcd a b = myGcd b (mod a b)   

-- Ex 14: The Haskell Prelude (standard library) defines the type
-- Ordering with values LT, GT and EQ. You try out Ordering by
-- evaluating the following expressions:
--
--   compare 3 4
--   compare 4 3
--   compare 0 0
--   compare "Hei" "Moi"
--   compare True False
--
-- Your task is to implement the function funnyCompare, that orders
-- integers in the following way:
--
-- 1. All even numbers come before odd numbers
-- 2. Within even and odd numbers the ordering is normal
--
-- In other words,
-- odd numbers compare normally:
--   funnyCompare 5 7 ==> LT
-- even numbers compare normally:
--   funnyCompare 8 6 ==> GT
-- even comes before odd:
--   funnyCompare 8 3 ==> LT
--   funnyCompare 2 3 ==> LT

funnyCompare :: Int -> Int -> Ordering
funnyCompare a b
    | (even a && even b) || (odd a && odd b) = compare a b
    | even a && odd b = LT
    | odd a && even b = GT
    | otherwise = LT    

-- Ex 15: Implement the function funnyMin that returns the minimum of
-- its two arguments, according to the ordering implemented by
-- funnyCompare.
--
-- Use pattern matching on the Ordering value returned by
-- funnyCompare. To do this, you need to either use the case-of
-- expression or define a helper function.

funnyMin :: Int -> Int -> Int
funnyMin a b = if funnyCompare a b == LT then a else b 
-- Ex 16: implement the recursive function pyramid that returns
-- strings like this:
--
-- pyramid 0 ==> "0"
-- pyramid 1 ==> "0,1,0"
-- pyramid 2 ==> "0,1,2,1,0"
-- pyramid 3 ==> "0,1,2,3,2,1,0"
--
-- Hints:
-- * you can glue strings together with the operator ++
-- * the function show transforms a number into a string
-- * you'll need a (recursive) helper function

pyramid :: Integer -> String
pyramid 0 = "0"
pyramid n = (pyramid' n one 0) ++ "," ++ (pyramid' 0 (-1) (n-1))
    where pyramid' limit factor n = if n == limit then (show n) else (show n) ++ "," ++ (pyramid' limit factor (n+factor))


-- Ex 17: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!

smallestDivisor :: Integer -> Integer
smallestDivisor n = smallestDivisor' n two
    where smallestDivisor' n k = if (isDivisible n k) then k else smallestDivisor' n (k + one)

-- Ex 18: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = (smallestDivisor n) == n

-- Ex 19: implement a function nextPrime that returns the first prime
-- number that comes after the given number. Use the function isPrime
-- you just defined.

nextPrime :: Integer -> Integer
nextPrime n = if isPrime nn then nn else nextPrime nn
    where nn = n + one
