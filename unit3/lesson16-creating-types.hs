data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    year :: Int,
    bookPrice :: Double
  }

-- Quick Check 16.1 --

data AuthorName = AuthorName
  { firstName :: String,
    lastName :: String
  }

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist

data Author = Author Name

data Artist
  = Person Name
  | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0

-- Quick Check 16.3 --
-- madeBy :: StoreItem -> String
-- madeBy (BookItem book) = show $ author book
-- madeBy (RecordItem record) = show $ artist record
-- madeBy _ = "unknown"

-- Q16.1
data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletDescription :: String,
    contact :: String
  }

-- Q16.2
type Width = Double

type Radius = Double

type Height = Double

data Shape
  = Circle Radius
  | Square Width
  | Rectangle Width Height

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square w) = w ^ 2
area (Rectangle w h) = w * h

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square w) = 4 * w
perimeter (Rectangle w h) = 2 * w + 2 * h
