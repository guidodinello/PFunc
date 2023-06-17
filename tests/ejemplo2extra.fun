foo :: (Int,Int) -> Int
foo(x,y) = (let x :: Int = 1 in (let x :: Int = 2 in ( let x:: Int = 3 in x )))
+ (let y :: Int = 1 in ( let y :: Int = 2 in y ))

main = 1