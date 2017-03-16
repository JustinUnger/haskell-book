data Quantum = 
    Yes
  | No
  | Both deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = False
convert No = False
convert Both = False

convert2 Yes = False
convert2 No  = False
convert2 Both = True

convert3 Yes = False
convert3 No = True
convert3 Both = True

convert4 Yes = True
convert4 No = True
convert4 Both = True

convert5 Yes = True
convert5 No = True
convert5 Both = False

convert6 Yes = True
convert6 No = False
convert6 Both = False

convert7 Yes = True
convert7 No = False
convert7 Both = True

convert8 Yes = False
convert8 No = True
convert8 Both = False


data Quad = 
    One
  | Two
  | Three
  | Four

-- eQuad :: Either Quad Quad
-- 4 + 4 = 8

-- prodQuad :: (Quad, Quad)
-- 4 * 4 = 16

-- funcQuad :: Quad -> Quad
-- 4 ^ 4 = 256

-- prodTBool :: (Bool, Bool, Bool)
-- 2 * 2 * 2

-- gTwo :: Bool -> Bool -> Bool
-- 2 ^ 2 ^ 2 = 16

-- fTwo :: Bool -> Quad -> Quad
-- 4 ^ 4 ^ 2 = 65535
-- (2 ^ 4) ^ 4 = 65535




