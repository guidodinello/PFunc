suma :: (Int,Int) -> Int
suma (x,y) = x+y  

foo :: (Int,Int) -> Int
foo(x,y) = let w :: Bool = suma(x,True) == False    
           in True == (False==suma(False))

main = foo(2,4)

