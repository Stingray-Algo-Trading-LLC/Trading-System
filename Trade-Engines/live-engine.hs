{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, decode, eitherDecode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Network.Socket (PortNumber)
import Network.WebSockets (Connection, receiveData, sendTextData)
import System.Environment (lookupEnv)
import Wuss (runSecureClient)

--------------------------------------------------------------------------------
-- Adjust these as needed for Alpaca's streaming endpoint:
--   - For SIP feed: wss://stream.data.alpaca.markets/v2/sip
--   - For IEX feed: wss://stream.data.alpaca.markets/v2/iex
--------------------------------------------------------------------------------

wsHost :: String
wsHost = "stream.data.alpaca.markets"

wsPort :: PortNumber
wsPort = 443

wsPath :: String
wsPath = "/v2/test"

--------------------------------------------------------------------------------
-- Build our JSON messages
--------------------------------------------------------------------------------

authMessage :: Text -> Text -> Value
authMessage key secret =
  object
    [ "action" .= ("auth" :: Text),
      "key" .= key,
      "secret" .= secret
    ]

subscribeMessage :: Value
subscribeMessage =
  object
    [ "action" .= ("subscribe" :: Text),
      "trades" .= (["FAKEPACA"] :: [Text]),
      "bars" .= (["FAKEPACA"] :: [Text]),
      "updatedBars" .= (["FAKEPACA"] :: [Text])
    ]

--------------------------------------------------------------------------------
-- Our main read loop (recursive)
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

handleMessage :: StreamData -> IO ()
handleMessage (TradeData trade) = putStrLn $ "Received Trade: " ++ show trade
handleMessage (BarData bar) = putStrLn $ "Received Bar: " ++ show bar
handleMessage (BarUpdateData bar) = putStrLn $ "Received Bar Update: " ++ show bar

readLoop :: Connection -> IO ()
readLoop conn = do
  rawMsg <- receiveData conn
  case eitherDecode rawMsg :: Either String [StreamData] of
    Right messages -> mapM_ handleMessage messages
    Left err -> putStrLn $ "Could not parse message: " ++ err

  -- Call ourselves again (recursive)
  readLoop conn

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Reading ALPACA_API_KEY and ALPACA_API_SECRET from environment..."

  maybeKey <- lookupEnv "ALPACA_API_KEY"
  maybeSecret <- lookupEnv "ALPACA_SECRET_KEY"

  case (maybeKey, maybeSecret) of
    (Just key, Just secret) -> do
      putStrLn "Connecting to Alpaca WebSocket..."
      runSecureClient wsHost wsPort wsPath $ \conn -> do
        putStrLn "Connected!"

        -- 1. Send auth
        let authMsg = authMessage (pack key) (pack secret)
        sendTextData conn (encode authMsg)

        -- 2. Wait for auth response
        msg1 <- receiveData conn
        putStrLn $ "Auth response: " ++ unpack msg1

        -- 3. Send subscribe
        sendTextData conn (encode subscribeMessage)

        -- 4. Wait for subscribe ack
        msg2 <- receiveData conn
        putStrLn $ "Subscribe response: " ++ unpack msg2

        -- 5. Start reading messages in a loop
        putStrLn "Entering readLoop..."
        readLoop conn
    _ -> do
      putStrLn "ERROR: Missing environment variables."
      putStrLn "Please set ALPACA_API_KEY and ALPACA_API_SECRET."

{-
{-# LANGUAGE RankNTypes #-}

data Data = Trade | Bar deriving (Show)

data Algo_State_F
  = LVRH_State (forall y. Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y)
  | BRH_State (forall y. Data -> (Data -> Int -> Int -> y) -> y)

state_transition ::
  Data -> Algo_State_F -> Algo_State_F
state_transition d (LVRH_State lvrh_f) = LVRH_State (lvrh_f d lvrh_state_transition) -- pattern match this branch for Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y --
state_transition Bar (BRH_State brh_f) = BRH_State (brh_f Bar brh_state_transition) -- pattern match this branch for Data -> (Data -> Int -> Int -> y) -> y --
-- Pass-by logic if algo state not affected by current incoming data (ie. BRH algo not affected by a Trade) --
-- We can do something like the following. This avoids calling brh_state_transition, which in turn would generate a  new brh_state --
state_transition Trade (BRH_State brh_f) = BRH_State brh_f

buy_logic ::
  Data -> Algo_State_F -> Bool
buy_logic d (LVRH_State lvrh_f) = lvrh_f d lvrh_buy_logic
buy_logic d (BRH_State brh_f) = brh_f d brh_buy_logic

main :: IO ()
main = do
  -- Create an initial state
  let init_state = lvrh_state 1 2 3 4
  let gen_updated_state = state_transition Trade (LVRH_State init_state)
  let gen_result = buy_logic Trade gen_updated_state
  print gen_result
  -- Use that state by applying Data=Trade plus the transition function
  let updated_state = init_state Trade lvrh_state_transition

  -- Finally, apply Data=Trade again plus a "result function"
  let result = updated_state Bar lvrh_buy_logic
  print result

  let init_bar_state = BRH_State (brh_state 1 2)
  let updated_bar_state = state_transition Trade init_bar_state -- should result to same state --
  let result_buy = buy_logic Trade updated_bar_state
  print result_buy

-}