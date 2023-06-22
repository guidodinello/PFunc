foo :: (Int,Int) -> Int
foo(x,y) = let w :: Bool = if (x==0) then 0 else False   
           in let z :: Int = 4
              in if (x==1) 
                    then if w then x==y else x 
                    else x+y

main = foo(2,4)
