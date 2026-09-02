module Route.Total where

total :: Int -> Int
total stops =
  let each = 12
   in
      -- the sum every caller ends up wanting
      each * stops
