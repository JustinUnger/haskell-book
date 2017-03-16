import Test.QuickCheck

half x = x / 2

halfIdentity = (*2) . half

prop_thereAndBack :: Double -> Bool
prop_thereAndBack x = (halfIdentity x) == x

main :: IO ()
main = quickCheck prop_thereAndBack
