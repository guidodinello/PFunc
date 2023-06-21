main = let x :: Int = (let x :: Int = 3 in x)
       in let y :: Int = 4+x
          in x+y+5