foo :: (Int,Int) -> Int
foo(x,y) = (let x :: Int = y in x) 
              + (let y :: Int = x + y in y + 1)

bar :: (Int) -> Int
bar (x) = let x :: Int = x + 1
          in let x :: Int = x + 1
	        in x
	     
main = let x :: Int = 2
       in  foo(x,4) + bar(x)