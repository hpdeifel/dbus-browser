-- | Helpers for printing expressions with minimal parentheses
module Unparse
       ( Associativity(..)
       , Side(..)
       , Unparse(..)
       , unparse
       -- | * Convenience combinators
       , (<+)
       , (+>)
       ) where

-- | Associativity of an operator
data Associativity = LeftAssoc
                   | RightAssoc
                   | Assoc
                   | NoneAssoc
                   deriving (Eq)

-- | Specifies on which side the subexpression appears beside an operator
data Side = LeftSide | RightSide deriving (Eq)

-- | Class of an unparsable datatype
class Unparse u where
  -- | Return the precedence of every constructor
  precedence :: u -> Int
  -- | Return the associativity of every constructor
  associativity :: u -> Associativity
  -- | Print u, using the first argument to print subexpressions
  printRec :: (u -> Side -> String) -> u -> String

up :: Unparse u => Int -> Associativity -> u -> Side -> String
up p a u s
  | precedence u > p = noparen
  | precedence u < p = paren
  | a == Assoc       = noparen
  | a == LeftAssoc && s == LeftSide = noparen
  | a == RightAssoc && s == RightSide = noparen
  | otherwise = paren


  where noparen = r
        paren   = "(" ++ r ++ ")"

        r = printRec (up (precedence u) (associativity u)) u

-- | Print u with minimal parentheses
unparse :: Unparse u => u -> String
unparse u = printRec r u
  where r = up (precedence u) (associativity u)

-- | Add subexpression to the left
(<+) :: (Side -> String) -> String -> String
(<+) f s = f LeftSide ++ s

infixr 1 <+

-- | Add subexpression to the right
(+>) :: String -> (Side -> String) -> String
(+>) s f = s ++ f RightSide
