data BookInfo = Book Int String [String]
                deriving (Show)
data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
