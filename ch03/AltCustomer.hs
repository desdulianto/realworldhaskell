data Customer = Customer CustomerID String [String]
                deriving (Show)

type CustomerID = Int

customerID :: Customer -> CustomerID
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
