module Domain.Models where

import Data.Aeson
import RIO
import RIO.Time

data OrderStatus = 
    Pending |
	Paid |
	Reserved |
	Canceled |
	Completed |
	Rejected 
    deriving (Enum, Eq, Generic, Show, ToJSON, FromJSON)

data TransactionType = 
    Purchase |
	Cancelation 
    deriving (Enum, Eq, Generic, Show, ToJSON, FromJSON)

data CancelationReason = 
    Ok | 
    NotEnoughMoney |
	OutOfStock |
	InternalError
    deriving (Enum, Eq, Generic, Show, ToJSON, FromJSON)

newtype OrderId = OrderId Int
    deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype OrderItemId = OrderItemId Int
    deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype ProductId = ProductId Int
    deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype UserId = UserId Int
    deriving (Eq, Generic, Show, ToJSON, FromJSON)

data Order = Order {
    orderId :: OrderId,
    orderUserId :: UserId,
    orderCreatedAt :: UTCTime,
    orderStatus :: OrderStatus,
    orderItems :: [OrderItem]
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data OrderItem = OrderItem {
	orderItemId :: OrderItemId,
	orderItemOrderId :: OrderId,
	orderItemProductId :: ProductId,
	orderItemCount :: Int,
	orderItemProductPrice :: Float
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data Product = Product {
    productId :: ProductId,
    productTitle :: String,
    productPrice :: Float
} deriving (Eq, Generic, Show, ToJSON, FromJSON)