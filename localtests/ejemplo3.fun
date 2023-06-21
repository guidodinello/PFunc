foo :: (Int,Bool) -> Int
foo(x,b) = if b then x * x else x + 2

main = 23 + foo(2,True) + foo(3,False)