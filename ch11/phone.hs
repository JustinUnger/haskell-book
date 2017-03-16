import Data.Char

type Digit = Char
type Presses = Int

data Button = Button { digit :: Digit, letters :: String } deriving (Eq, Show)

bs =
  [ Button '1' "1" 
  , Button '2' "ABC2"
  , Button '3' "DEF3"
  , Button '4' "GHI4"
  , Button '5' "JKL5"
  , Button '6' "MNO6"
  , Button '7' "PQRS7"
  , Button '8' "TUV8"
  , Button '9' "WXYZ"
  , Button '*' ""
  , Button '0' " 0"
  , Button '#' ".,#"
  ]

buttonToLetters :: Button -> [(Char, (Digit, Presses))]
buttonToLetters b = map (\(l,p) -> (l, ((digit b), p))) (zip (letters b) [1..])

ls = concat $ foldr (\x acc -> buttonToLetters x : acc) [] bs

findButton x = lookup x ls

reverseTaps c = case findButton (toUpper c) of
                    (Just b) -> if isUpper c then [('*',1),b] else [b]
                    Nothing -> []

toPresses xs = concat $ map reverseTaps xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]


freq :: String -> [(Char,Int)]
freq xs = foldr (\c acc -> case lookup c acc of
                            Nothing -> (c,1) : acc
                            Just n -> (c,n+1) : filter (\x -> fst x /= c) acc) [] xs
