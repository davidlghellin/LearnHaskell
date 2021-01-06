module Otro.Youtube.MonoidalValidator where

import qualified Data.List as List


data ValidationResult = Success
                      | Error [String]


validateBadWord :: String -> String -> ValidationResult
validateBadWord word text =
  if word `List.isInfixOf` text 
     then Error ["String contains a bad word: " ++ word]
     else Success


validateLength maxLength text =
  if length text <= maxLength
     then Success
     else Error ["String is too long"]

instance Semigroup ValidationResult where
  (<>) a Success = a
  (<>) Success a = a
  (<>) (Error x) (Error y) = Error (x ++ y)

instance Monoid ValidationResult where
  mempty = Success


instance Show ValidationResult where
  show Success = "Ok"
  show (Error msgs) = List.intercalate ", " msgs


main :: IO ()
main = do
  let phrase = "cobol has native support for monads"
      cool = "Cool stuff"

      validations = [
        validateLength 10,
        validateBadWord "monad",
        validateBadWord "cobol"
        ]

  putStrLn phrase
  print $ foldMap (\x -> x phrase) validations
  putStrLn cool
  print $ foldMap (\x -> x cool) validations