module Lib1
    ( examples
    , Command(..)
    , Dumpable(..)
    , Book(..)
    , Shelf(..)
    , LibraryElement(..)
    ) where

data Dumpable = Examples
  deriving Show

data Book = Book
  { title  :: String, 
    author :: String, 
    pages  :: Int
  } deriving Show

data Shelf = Shelf
  { shelfName :: String,
    contents  :: [LibraryElement]
  } deriving Show

-- LibraryElement can be a book or a nested shelf
data LibraryElement
  = ElemBook Book | ElemShelf Shelf
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Command
  = AddBook Book | AddShelf Shelf | CountPages | ListContents | Dump Dumpable
  deriving Show

examples :: [Command]
examples =
  [ AddBook (Book "1984" "Orwell" 328), 
    AddShelf (Shelf "Fiction" [ElemBook (Book "Brave New World" "Huxley" 268)]), 
    AddShelf (Shelf "Children" [ElemBook (Book "Harry Potter" "Rowling" 412)]), 
    CountPages, 
    Dump Examples
  ]


