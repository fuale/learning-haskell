module Module44 where

import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show)

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson l =
  if not (hasEq $ lines l)
    then Left ParsingError
    else
      if not (hasFields l)
        then Left IncompleteDataError
        else
          if not (thisDigit $ strJust $ ageString l)
            then Left (IncorrectDataError (strJust (ageString l)))
            else Right (Person {firstName = strJust $ lookup "firstName" $ list2HashList l, lastName = strJust $ lookup "lastName" $ list2HashList l, age = read (strJust $ ageString l) :: Int})

sumStrings acc [] = acc
sumStrings acc (x : xs) = sumStrings (acc ++ x ++ " ") xs

thisDigit xs = all isDigit xs

strJust (Just x) = x
strJust (Nothing) = ""

ageString l = lookup "age" $ list2HashList l

hasFields :: String -> Bool
hasFields l = null (checkFields l)

checkFields l = filter (== Nothing) [lookup "firstName" $ list2HashList l, lookup "lastName" $ list2HashList l, lookup "age" $ list2HashList l]

list2HashList :: String -> [(String, String)]
list2HashList l = map l2h (lines l)
  where
    l2h str = l2hM (buildPair str)
    buildPair str = ((take (findEqN str) str), (drop ((findEqN str) + 1) str))
    l2hM p =
      if ((' ' == (last $ fst p)) && (' ' == (head $ snd p)))
        then (init $ fst p, tail $ snd p)
        else p

hasEq :: [String] -> Bool
hasEq l = foldl1 (&&) (map findEq l)

findEq :: String -> Bool
findEq [] = False
findEq (x : xs) =
  if (x == '=')
    then True
    else findEq xs

findEqN :: String -> Int
findEqN l = findEqN' 0 l

findEqN' :: Int -> String -> Int
findEqN' acc [] = 0
findEqN' acc (x : xs) =
  if (x == '=')
    then acc
    else findEqN' (acc + 1) xs