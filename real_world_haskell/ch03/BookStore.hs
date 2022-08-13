data BookInfo = Book Int String [String]
  deriving (Show)
data MagazineInfo = Magazine Int String [String]
  deriving (Show)

myInfo = Book 9380135072455 "Algebra of Programming"
  ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

data Bool = False | True

type CardHolder = String
type CardNumber = String
type Address = [String]

--- sample different value constructors
data BillingInfo = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
    deriving (Show)
