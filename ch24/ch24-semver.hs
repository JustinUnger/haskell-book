-- exercise 1, write a parser for semantic versions defined
-- by http://semver.org. after making a working parser, write an Ord instance
-- for the SemVer type that obeys the specification outlined on the SemVer 
-- website

import Control.Applicative ((<|>))
import Text.Trifecta

data NumberOrString =
      NOSS String
    | NOSI Integer
    deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = 
    SemVer Major Minor Patch Release Metadata
    deriving (Show, Eq)

instance Ord NumberOrString where
    compare (NOSS s) (NOSS s') = compare s s'
    compare (NOSI i) (NOSI i') = compare i i'
-- Numeric identifiers always have lower precedence than non-numeric identifiers
    compare (NOSS _) _ = GT
    compare _ _ = LT

instance Ord SemVer where
    compare (SemVer maj minor pat rel _) (SemVer maj' minor' pat' rel' _) =
        case compare maj maj' of
            EQ -> case compare minor minor' of
                    EQ -> case compare pat pat' of
                            EQ -> compare rel rel'
                            x  -> x
                    x -> x
            x -> x

parseNos :: Parser NumberOrString
-- use natural rather than integer unless you actually wany negative numbers
--parseNos = (NOSS <$> some letter) <|> (NOSI <$> integer)
parseNos = (NOSS <$> some letter) <|> (NOSI <$> natural)

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    _ <- char '.'
    minor <- decimal 
    _ <- char '.'
    patch <- decimal
    rel <- optional parseRelease
    case rel of
        Nothing -> return (SemVer major minor patch [] [])
        Just r -> do
            meta <- optional parseRelease
            return (SemVer major minor patch r (maybe [] id meta))

parseRelease' :: Parser [NumberOrString]
parseRelease' = many ((parseNos) <* (skipMany (char '.')))

parseRelease :: Parser [NumberOrString]
parseRelease = char '-' >> parseRelease'

parseTests = [ 
      ("2.1.1",SemVer 2 1 1 [] [])
    , ("1.0.0-x.7.z.92", SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
 ]

compareTests = [
     (SemVer 2 2 1 [] [] > SemVer 2 1 0 [] [], True)
 ]

foo :: (String, SemVer) -> Bool
foo (s, r) = 
    case parseString parseSemVer mempty s of
        Failure _  -> False
        Success r' -> r == r'

doesItWork = all foo parseTests && all (\(a,b) -> a == b) compareTests

main = do
    print $ "It works? " ++ show doesItWork

--stop = unexpected "stop"
