foo :: (Int,Int) -> Int
foo(x,y) = (let x :: Int = y in x)
              + (let y :: Int = x + y in y)

bar :: (Int) -> Bool
bar (x) = foo(let x :: Int = 5 in x,x) == foo (x,let x :: Int = 5 in x)

main = foo(2,4) + if (bar(5) == True) then 3 else 4