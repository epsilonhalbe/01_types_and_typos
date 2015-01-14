module Main where


newtype Age = Age Int
type Age' = Int

newtype Speed = Speed {getSpeed :: Double}
newtype Forget a = Remember {value :: a}

data Tree a = Empty | Tree a [Tree a]

main :: IO ()
main = print "New-Type and haskell"

a :: Int
a = 5
b :: Age'
b = Age 5
c :: Age'
c = 5
d :: Speed
d = Speed 5
e :: Forget Char
e = Remember 'a'

