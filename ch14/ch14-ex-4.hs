-- make a gen random generator for the data type

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genMoreFulse :: Gen Fool
genMoreFulse = elements [Fulse, Fulse, Frue]

