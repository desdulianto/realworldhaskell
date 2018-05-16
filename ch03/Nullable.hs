data Maybe a = Just a | Nothing
               deriving (Show)

someBool = Main.Just True

someString = Main.Just "something"

wrapped = Main.Just (Main.Just "wrapped")
