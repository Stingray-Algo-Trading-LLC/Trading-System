{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataTypes
  ( Trade (..),
    Bar (..),
    StreamData (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Text (Text)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------
data Trade = Trade
  { tradeSymbol :: Text, -- Stock symbol
    exchange :: Text, -- Exchange trade occured on
    price :: Double, -- Trade price
    size :: Int, -- Trade size
    condition :: [Text], -- Trade conditions
    tradeTimestamp :: UTCTime, -- Trade timestamp
    tape :: Text -- Tape of symbol (ie. SPY is NYSE Arca as B)
  }
  deriving (Show, Eq)

data Bar = Bar
  { barSymbol :: Text, -- Stock symbol
    open :: Double, -- Bar open price
    high :: Double, -- Bar high price
    low :: Double, -- Bar low price
    close :: Double, -- Bar close price
    volume :: Int, -- Bar volume (trade_i count x trade_i size for all i)
    barTimestamp :: UTCTime -- Bar timestamp
  }
  deriving (Show, Eq)

data StreamData
  = TradeData Trade
  | BarData Bar
  | BarUpdateData Bar
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parsing Logic
--------------------------------------------------------------------------------

instance FromJSON StreamData where
  parseJSON = withObject "StreamData" $ \v -> do
    msgType <- v .: "T" -- Type (t, b, u for trade, bar, updated bar, respectively)
    case (msgType :: Text) of
      "t" ->
        TradeData
          <$> parseJSON (toJSON v)
      "b" ->
        BarData
          <$> parseJSON (toJSON v)
      "u" ->
        BarUpdateData
          <$> parseJSON (toJSON v)
      _ -> fail $ "Unknown message type: " ++ show msgType

instance FromJSON Trade where
  parseJSON = withObject "Trade" $ \v ->
    Trade
      <$> v .: "S" -- Symbol
      <*> v .: "x" -- Exchange
      <*> v .: "p" -- Price
      <*> v .: "s" -- Size
      <*> v .: "c" -- Condition
      <*> v .: "t" -- Timestamp
      <*> v .: "z" -- Tape

instance FromJSON Bar where
  parseJSON = withObject "Bar" $ \v ->
    Bar
      <$> v .: "S" -- Symbol
      <*> v .: "o" -- Open
      <*> v .: "h" -- High
      <*> v .: "l" -- Low
      <*> v .: "c" -- Close
      <*> v .: "v" -- Volume
      <*> v .: "t" -- Timestamp