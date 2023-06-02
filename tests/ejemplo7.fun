foo :: (Int,Int) -> Int
foo(x,y) = (let x :: Int = 4 in x) 
              + (let y :: Int = 11 + 2 in y + 1)

bar :: (Int) -> Int
bar (x) = let x :: Int = 1
          in let x :: Int = 2
	     in x
	     
main = let x :: Int = 2
       in  foo(x,4) + bar(x)