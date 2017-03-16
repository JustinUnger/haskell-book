import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

npa :: Parser String
npa = count 3 digit <* many (char '-')

npa' :: Parser String
npa' = char '(' *> count 3 digit <* char ')' <* char ' '

npa'' :: Parser String
npa'' = char '1' >> char '-' >> npa

parseNpa :: Parser NumberingPlanArea
parseNpa = do
    x <- try npa <|> try npa' <|> npa''  
    return $ read x

parseExchange :: Parser Exchange
parseExchange = do
    x <- try (count 3 digit <* char '-') <|> count 3 digit 
    return $ read x

parseLine :: Parser LineNumber
parseLine = do
    x <- count 4 digit
    return $ read x


parsePhone :: Parser PhoneNumber
parsePhone = do
    n <- parseNpa
    exchange <- parseExchange
    l <- parseLine
    return $ PhoneNumber n exchange l
    
