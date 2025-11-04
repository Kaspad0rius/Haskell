{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
  , ToCliCommand(..)
  , process) where

import qualified Lib1
import Data.Char (isDigit, isSpace)

------------------------------------------------------------
-- Basic types
------------------------------------------------------------

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

------------------------------------------------------------
-- Helper functions
------------------------------------------------------------

-- Drop leading whitespace
trimLeft :: String -> String
trimLeft = dropWhile isSpace

-- Run parser and ensure all input is consumed (ignoring trailing whitespace)
runParser :: Parser a -> String -> Either ErrorMsg a
runParser p s = case p (trimLeft s) of
  Right (a, rest)
    | all isSpace rest -> Right a
    | otherwise        -> Left $ "Unconsumed input: " ++ show rest
  Left e -> Left e

------------------------------------------------------------
-- Primitive parsers
------------------------------------------------------------

charP :: Char -> Parser Char
charP _ [] = Left "Unexpected end of input"
charP c (x:xs)
  | c == x    = Right (c, xs)
  | otherwise = Left $ "Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "'"

satisfy :: (Char -> Bool) -> Parser Char
satisfy _ [] = Left "Unexpected end of input"
satisfy p (x:xs)
  | p x       = Right (x, xs)
  | otherwise = Left $ "Unexpected character: " ++ [x]

stringP :: String -> Parser String
stringP "" s = Right ("", s)
stringP (c:cs) s = do
  (_, s1) <- charP c s
  (rest, s2) <- stringP cs s1
  Right (c:rest, s2)

keyword :: String -> Parser String
keyword kw s = do
  (parsed, rest) <- stringP kw s
  if null rest || isSpace (head rest)
     then Right (parsed, rest)
     else Left $ "Keyword '" ++ kw ++ "' not properly delimited"

whitespace :: Parser String
whitespace s =
  let (spaces, rest) = span isSpace s
  in if null spaces then Left "Expected whitespace" else Right (spaces, rest)

optionalWhitespace :: Parser ()
optionalWhitespace s = Right ((), trimLeft s)

digitP :: Parser Char
digitP = satisfy isDigit

numberP :: Parser Int
numberP s =
  let (ds, rest) = span isDigit s
  in if null ds then Left "Expected number" else Right (read ds, rest)

quotedStringP :: Parser String
quotedStringP [] = Left "Unexpected end of input"
quotedStringP ('"':xs) =
  let (inside, rest) = break (== '"') xs
  in case rest of
       ('"':after) -> Right (inside, after)
       _            -> Left "Missing closing quote"
quotedStringP _ = Left "Expected quoted string"

-- Combine two parsers sequentially
and2 :: Parser a -> Parser b -> Parser (a, b)
and2 pa pb s = do
  (a, s1) <- pa s
  (b, s2) <- pb (trimLeft s1)
  Right ((a,b), s2)

-- Combine two parsers as alternatives
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 s = case p1 s of
  Left _ -> p2 s
  ok     -> ok

-- | mapP applies a function to the result of a parser.
mapP :: (a -> b) -> Parser a -> Parser b
mapP f p s = case p s of
  Left err       -> Left err
  Right (a, out) -> Right (f a, out)

------------------------------------------------------------
-- Grammar-specific parsers
------------------------------------------------------------

-- Book:  book "Title" "Author" Pages
bookP :: Parser Lib1.Book
bookP s = do
  (_, s1) <- keyword "book" s
  (title, s2)  <- quotedStringP (trimLeft s1)
  (author, s3) <- quotedStringP (trimLeft s2)
  (pages, s4)  <- numberP (trimLeft s3)
  Right (Lib1.Book title author pages, s4)

-- Shelf: shelf "Name" { elements }
shelfP :: Parser Lib1.Shelf
shelfP s = do
  (_, s1) <- keyword "shelf" s
  (name, s2) <- quotedStringP (trimLeft s1)
  (_, s3) <- charP '{' (trimLeft s2)
  (elems, s4) <- manyElements (trimLeft s3)
  (_, s5) <- charP '}' (trimLeft s4)
  Right (Lib1.Shelf name elems, s5)
  where
    manyElements :: Parser [Lib1.LibraryElement]
    manyElements s' =
      case libraryElemP (trimLeft s') of
        Left _ -> Right ([], s')
        Right (e, rest) -> do
          (es, rest2) <- manyElements rest
          Right (e:es, rest2)

-- LibraryElement: book ... | shelf ...
libraryElemP :: Parser Lib1.LibraryElement
libraryElemP =
  mapP Lib1.ElemBook bookP
  `orElse`
  mapP Lib1.ElemShelf shelfP

------------------------------------------------------------
-- Command parsers
------------------------------------------------------------

-- add-book <book>
addBookP :: Parser Lib1.Command
addBookP s = do
  (_, s1) <- keyword "add-book" s
  (b, s2) <- bookP (trimLeft s1)
  Right (Lib1.AddBook b, s2)

-- add-shelf <shelf>
addShelfP :: Parser Lib1.Command
addShelfP s = do
  (_, s1) <- keyword "add-shelf" s
  (sh, s2) <- shelfP (trimLeft s1)
  Right (Lib1.AddShelf sh, s2)

-- count-pages
countPagesP :: Parser Lib1.Command
countPagesP s = do
  (_, rest) <- keyword "count-pages" s
  Right (Lib1.CountPages, rest)

-- list-contents
listContentsP :: Parser Lib1.Command
listContentsP s = do
  (_, rest) <- keyword "list-contents" s
  Right (Lib1.ListContents, rest)

-- dump examples
dumpP :: Parser Lib1.Command
dumpP s = do
  (_, s1) <- keyword "dump" s
  (_, s2) <- keyword "examples" (trimLeft s1)
  Right (Lib1.Dump Lib1.Examples, s2)

------------------------------------------------------------
-- Main command parser (recursive)
------------------------------------------------------------

parseCommand :: Parser Lib1.Command
parseCommand =
      addBookP
  `orElse` addShelfP
  `orElse` countPagesP
  `orElse` listContentsP
  `orElse` dumpP

------------------------------------------------------------
-- Processing & CLI conversion
------------------------------------------------------------

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) =
  "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

------------------------------------------------------------
-- ToCliCommand class
------------------------------------------------------------

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.AddBook (Lib1.Book t a p)) =
    unwords ["add-book", "book", showQ t, showQ a, show p]
  toCliCommand (Lib1.AddShelf (Lib1.Shelf n elems)) =
    "add-shelf shelf " ++ showQ n ++ " { " ++ unwords (map elemToCli elems) ++ " }"
    where
      elemToCli (Lib1.ElemBook (Lib1.Book t a p)) =
        unwords ["book", showQ t, showQ a, show p]
      elemToCli (Lib1.ElemShelf (Lib1.Shelf n2 es)) =
        "shelf " ++ showQ n2 ++ " { " ++ unwords (map elemToCli es) ++ " }"
  toCliCommand Lib1.CountPages = "count-pages"
  toCliCommand Lib1.ListContents = "list-contents"
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

showQ :: String -> String
showQ s = "\"" ++ s ++ "\""

------------------------------------------------------------
-- Manual Eq instance for Command (no deriving)
------------------------------------------------------------

-- Manual Eq instance for Book
instance Eq Lib1.Book where
  (Lib1.Book t1 a1 p1) == (Lib1.Book t2 a2 p2) =
    t1 == t2 && a1 == a2 && p1 == p2

-- Manual Eq instance for Shelf
instance Eq Lib1.Shelf where
  (Lib1.Shelf n1 c1) == (Lib1.Shelf n2 c2) =
    n1 == n2 && c1 == c2

-- Manual Eq instance for LibraryElement
instance Eq Lib1.LibraryElement where
  (Lib1.ElemBook b1)  == (Lib1.ElemBook b2)  = b1 == b2
  (Lib1.ElemShelf s1) == (Lib1.ElemShelf s2) = s1 == s2
  _ == _ = False


instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.AddBook (Lib1.Book t1 a1 p1)) == (Lib1.AddBook (Lib1.Book t2 a2 p2)) =
    t1 == t2 && a1 == a2 && p1 == p2
  (Lib1.AddShelf (Lib1.Shelf n1 c1)) == (Lib1.AddShelf (Lib1.Shelf n2 c2)) =
    n1 == n2 && c1 == c2
  Lib1.CountPages == Lib1.CountPages = True
  Lib1.ListContents == Lib1.ListContents = True
  (Lib1.Dump Lib1.Examples) == (Lib1.Dump Lib1.Examples) = True
  _ == _ = False
