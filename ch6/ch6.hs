-- Given the following data type definitions: 

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Which of the following will type check? 

-- phew = Papu "chases" True

-- Will not type check because missing Yeah and Rocks constructors
--     • Couldn't match expected type ‘Rocks’ with actual type ‘[Char]’
--     • Couldn't match expected type ‘Yeah’ with actual type ‘Bool’

truth = Papu (Rocks "foo") (Yeah True)

-- This will typecheck because it has the proper constructors 

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p' 

-- above will typecheck because Papu derives Eq 

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p' 

-- this will not typecheck because Papu does not derive Ord, thus
-- can't determine ordering
--     • No instance for (Ord Papu) arising from a use of ‘>’


