-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

import qualified Data.Map
import Data.Maybe

-- | Just to give `tr` a more descriptive type
type CharSet = String



-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.

tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs = if _outset == Nothing
                        then delete _inset xs
                        else translate xs _inset _outset_str
                       where _outset_str = Data.Maybe.fromJust _outset

delete :: String -> String -> String
delete delStr str = [x | x <- str, not $ x `elem` delStr]

translate :: String -> String -> String -> String
translate str from to = [ if x `elem` from then replace x d else x | x <- str]
    where d = Data.Map.fromList $ zip from (to ++ repeat (last to))

replace :: Char -> Data.Map.Map Char Char -> Char
replace x d = Data.Maybe.fromJust $ Data.Map.lookup x d
