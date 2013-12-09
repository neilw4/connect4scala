module C4Scala.Generic where

-- Gives a Just value if the boolean was true, else Nothing
maybeIf :: a -> Bool -> Maybe a
maybeIf x True  = Just x
maybeIf _ False = Nothing

-- Appends a value to the end of a list
append :: a -> [a] -> [a]
append x xs = xs ++ [x]

-- Applies a function to the element at a certain index
updateEntryAt :: (a -> a) -> Int -> [a] -> [a]
updateEntryAt f 0 (x:xs)   = f x:xs
updateEntryAt f col (x:xs) = x:updateEntryAt f (col - 1) xs

-- Always start from middle and work outwards
colSeq size = take size $ iterate nextCol (size `div` 2)
    where nextCol col | col * 2 < size = size - col
                      | otherwise      = size - col - 1
