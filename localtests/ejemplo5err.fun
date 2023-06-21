foo :: (Int,Int,Bool) -> Int
foo(x,y,z) = if (3==True) 
                then z
                else if z then x+y else x==y 

main = foo(2,4)

