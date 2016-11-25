lastButOne :: [a] -> a
-- lastButOne xs = head ( drop (length xs - 2) xs )
lastButOne xs = if null (tail (tail xs))
                then head xs
                else lastButOne (tail xs)
