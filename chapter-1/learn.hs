module Learn where

x = 10 * 5 + y
myResult = x * 5
y = 10

z = let h = 20
        y = 10
    in 2 ^ (h - y)
