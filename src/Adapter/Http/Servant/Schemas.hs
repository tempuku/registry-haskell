{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.Servant.Schemas where

import RIO
import Servant
import qualified Domain.Models as D
import Data.Aeson.Types

data CreateOrderRequest = CreateOrderRequest {
    user_id :: !D.UserId
    ,order_items :: ![CreateOrderRequestItem]
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data CreateOrderRequestItem = CreateOrderRequestItem {
    product_id :: !D.ProductId
    ,count :: !Natural
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data CreateOrderResponse = CreateOrderResponse {
    status :: !String
} deriving (Eq, Generic, Show, ToJSON, FromJSON)
