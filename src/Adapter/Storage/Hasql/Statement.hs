module Adapter.Storage.Hasql.Statement where

import RIO
import Data.Functor.Contravariant (contramap)
import Hasql.Statement
import Hasql.TH
import qualified Hasql.Decoders as DEC
import qualified Hasql.Encoders as ENC
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D
import Control.Lens (dimap)


-- -- Function to insert a new order into the "orders" table
-- insertNewOrder :: Statement D.UserId (Maybe D.OrderId)
-- insertNewOrder =
--       dimap
--         (\ (D.UserId a) -> fromIntegral a)
--         (\ case
--         Just a -> Just . D.OrderId . fromIntegral $ a
--         Nothing -> Nothing)
--         [maybeStatement|
--         insert into orders 
--         values ($1 :: int8)
--         returning (id :: int8)|]

insertNewOrder :: Statement D.UserId D.OrderId
insertNewOrder = Statement sql encoder decoder True
    where
        sql = "INSERT INTO orders (user_id) VALUES ($1) RETURNING id"
        encoder = contramap (\ (D.UserId a) -> fromIntegral a) $ ENC.param $ ENC.nonNullable ENC.int8
        decoder = DEC.singleRow . DEC.column . DEC.nonNullable $ D.OrderId . fromIntegral <$> DEC.int8

insertOrderItems :: Statement (D.OrderId, IN.NewOrderItemDTO) D.OrderItemId
insertOrderItems = Statement sql encoder decoder True
    where
      sql = "INSERT INTO order_items (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4) RETURNING id"
      encoder =
        contramap ((\ (D.OrderId a) -> fromIntegral a) . fst) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap ((\ (D.ProductId a) -> fromIntegral a) . IN.nOrItProductId . snd) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap (fromIntegral . IN.nOrItCount . snd) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap (IN.nOrItProductPrice . snd) (ENC.param (ENC.nonNullable ENC.float4))
      decoder = DEC.singleRow . DEC.column . DEC.nonNullable $ D.OrderItemId . fromIntegral <$> DEC.int8

-- -- Function to insert the order items into the "order_items" table
-- insertOrderItems :: Connection -> D.OrderId -> [IN.NewOrderItemDTO] -> IO (Either Session.QueryError ())
-- insertOrderItems conn orderId orderItems = run session conn
--   where
--     session =
--       statement (orderId, orderItems) $ Statement sql encoder decoder True
--     sql = "INSERT INTO order_items (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)"
--     encoder =
--       contramap (unOrderId . fst) (param (ENC.nonNullable ENC.int8))
--         <> contramap (IN.nOrItProductId . snd) (param (ENC.nonNullable ENC.int8))
--         <> contramap (fromIntegral . IN.nOrItCount . snd) (param (ENC.nonNullable ENC.int8))
--         <> contramap (IN.nOrItProductPrice . snd) (param (ENC.nonNullable ENC.float4))
--     decoder = DEC.noResult
