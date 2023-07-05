{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Storage.Hasql.Order where

import qualified Interfaces.DAO as IN
import qualified Data.UUID as UUID
import qualified Domain.Messages as D
import qualified Domain.User as D
import qualified Hasql.Connection as HConn
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified PostgreSQL.ErrorCodes as PgErr
import RIO hiding (trace)
import qualified Usecase.Interactor as UC
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import qualified Data.Text as T
import Hasql.Connection
import Hasql.Decoders
import Hasql.Encoders
import Hasql.Query
import Hasql.Session
import Hasql.Statement
import Hasql.Transaction

execQuery :: MonadIO m => HConn.Connection -> a -> Statement a b -> m (Either Session.QueryError b)
execQuery conn param stmt = liftIO $ Session.run (Session.statement param stmt) conn

execTransaction :: MonadIO m => HConn.Connection -> Transaction.Transaction -> m (Either Session.QueryError b)
execQuery conn transaction = liftIO $ Session.run (Transaction.run transaction) conn

createOrder :: (MonadIO m, UC.Logger m) => IN.NewOrderDTO -> m (Either ErrDAO D.Order)
createOrder conn newOrder = do
  result <- execQuery conn (getQueryFields newOrder) insertUserStmt
  case result of
    Right _ -> pure Nothing
    Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code e _ _ _ ))) ->
      if code == PgErr.unique_violation
        then do
          UC.logDebug e
          pure $ Just (IN.ErrValidation e)
        else handleErr e
    Left e -> handleErr e
  where
    getQueryFields :: D.User -> D.Password -> (UUID.UUID, Text, Text, Text)
    getQueryFields (D.User uid' (D.Name name') (D.Email email')) (D.Password password') = (uid', name', email', password')
    insertUserStmt =
      [TH.rowsAffectedStatement|
                      insert into "users" (uid, name, email, password)
                      values ($1::uuid, $2::text, $3::text, $4::text)
                      |]
    handleErr e = do
      UC.logDebug e
      pure $ Just IN.ErrTechnical e



-- Encode/Decode functions for PostgreSQL queries
orderItemDecoder :: Row OrderItem
orderItemDecoder =
  OrderItem
    <$> column (nonNullable int8)
    <*> column (nonNullable int8)
    <*> column (nonNullable int8)
    <*> column (nonNullable float4)

orderDecoder :: Row Order
orderDecoder =
  Order
    <$> column (nonNullable int8)
    <*> column (nonNullable int8)

orderEncoder :: Order -> Params
orderEncoder order =
  contramap orId (param (nonNullable int8))
    <> contramap orUserId (param (nonNullable int8))

orderItemEncoder :: OrderItem -> Params
orderItemEncoder orderItem =
  contramap oItOrderId (param (nonNullable int8))
    <> contramap oItProductId (param (nonNullable int8))
    <> contramap oItCount (param (nonNullable int8))
    <> contramap oItProductPrice (param (nonNullable float4))

-- Function to insert a new order into the database
insertNewOrder :: Connection -> NewOrderDTO -> IO (Either QueryError ())
insertNewOrder conn orderDto =
  session conn $ do
    -- Insert the order and retrieve its ID
    orderInsertResult <- run (statement (nOrUserId orderDto) (Statement "INSERT INTO \"order\" (user_id) VALUES ($1) RETURNING id" orderEncoder (rowMaybe orderDecoder) True))
    case orderInsertResult of
      Left err -> return (Left err)
      Right (Just order) -> do
        -- Insert the order items
        let orderItems = nOrOrderItems orderDto
        itemsInsertResult <- run (statement orderItems (Statement "INSERT INTO order_item (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)" (arrayParam (nonNullable $ composite orderItemEncoder)) (unitDecoder) True))
        case itemsInsertResult of
          Left err -> return (Left err)
          Right _ -> return (Right ())
