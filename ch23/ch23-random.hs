import System.Random

sg = mkStdGen 0

newSg = snd (next sg)

r = random newSg
