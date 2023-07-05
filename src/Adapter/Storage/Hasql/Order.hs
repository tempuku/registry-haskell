{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Storage.Hasql.Order where

import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D
import qualified Hasql.Connection as HConn
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
-- import qualified PostgreSQL.ErrorCodes as PgErr
import RIO hiding (trace)
import qualified Interfaces.Usecases as UC
import Data.Functor.Contravariant (contramap)
import Hasql.Connection
import qualified Hasql.Decoders as DEC
import qualified Hasql.Encoders as ENC
import Hasql.Session(run, QueryError)
import Hasql.Statement
import Hasql.Transaction
import Hasql.Transaction.Sessions
import RIO.Partial (toEnum)
import RIO.Time (UTCTime)


-- execTransaction :: MonadIO m => HConn.Connection -> Transaction.Transaction -> m (Either Session.QueryError b)
-- execQuery conn transaction = liftIO $ Session.run (Transaction.run transaction) conn

data Order = Order {
    orderId :: D.OrderId,
    orderUserId :: D.UserId,
    orderCreatedAt :: UTCTime,
    orderStatus :: D.OrderStatus
}


-- Encode/Decode functions for PostgreSQL queries
orderItemDecoder :: DEC.Row D.OrderItem
orderItemDecoder =
  D.OrderItem
    <$> DEC.column (DEC.nonNullable (D.OrderItemId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (D.OrderId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (D.ProductId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable DEC.float4)


orderDecoder :: DEC.Row Order
orderDecoder =
  Order
    <$> DEC.column (DEC.nonNullable (fmap (D.OrderId . fromIntegral) DEC.int8))
    <*> DEC.column (DEC.nonNullable (fmap (D.UserId . fromIntegral) DEC.int8))
    <*> DEC.column (DEC.nonNullable DEC.timestamptz)
    <*> DEC.column (DEC.nonNullable $ toEnum . fromIntegral <$> DEC.int2)


orderEncoder :: Order -> ENC.Params Order
orderEncoder order =
  contramap orderId (ENC.param (ENC.nonNullable $ D.OrderId . fromIntegral <$> ENC.int8))
    <> contramap orderUserId (ENC.param (ENC.nonNullable $ D.UserId . fromIntegral <$> ENC.int8))
    <> contramap orderCreatedAt (ENC.param (ENC.nonNullable ENC.timestamptz))
    <> contramap orderStatus (ENC.param (ENC.nonNullable $ toEnum . fromIntegral <$> ENC.int2))

orderItemEncoder :: D.OrderItem -> ENC.Params D.OrderItem
orderItemEncoder orderItem =
  contramap D.orderItemId (ENC.param (ENC.nonNullable $ D.OrderItemId . fromIntegral <$> ENC.int8))
    <> contramap D.orderItemProductId (ENC.param (ENC.nonNullable $ D.orderItemProductId . fromIntegral <$> ENC.int8))
    <> contramap D.orderItemCount (ENC.param (ENC.nonNullable $ fromIntegral <$> ENC.int8))
    <> contramap D.orderItemProductPrice (ENC.param (ENC.nonNullable ENC.float4))

-- Function to insert a new order into the database
insertNewOrder :: Connection -> IN.NewOrderDTO -> IO (Either QueryError ())
insertNewOrder conn orderDto =
  transaction conn $ do
    -- Insert the order and retrieve its ID
    orderInsertResult <- run (statement (IN.nOrUserId orderDto) (Statement "INSERT INTO \"order\" (user_id) VALUES ($1) RETURNING id" orderEncoder (DEC.rowMaybe orderDecoder) True))
    case orderInsertResult of
      Left err -> condemn >= return (Left err)
      Right (Just order) -> do
        -- Insert the order items
        let orderItems = IN.nOrOrderItems orderDto
        itemsInsertResult <- run (statement orderItems (Statement "INSERT INTO order_item (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)" (DEC.rowList (DEC.nonNullable $ DEC.composite orderItemEncoder)) DEC.noResult True))
        case itemsInsertResult of
          Left err -> return (Left err)
          Right _ -> return (Right ())
