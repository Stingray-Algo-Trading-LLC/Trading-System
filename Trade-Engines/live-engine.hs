{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import BRH (brhBuyLogic, brhState, brhStateTransition)
import Data.Aeson (Value, eitherDecode, encode, object, (.:), (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (Text, pack)
import DataTypes (StreamData (..))
import LVRH (lvrhBuyLogic, lvrhState, lvrhStateTransition)
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

handleMessage :: StreamData -> IO ()
handleMessage (TradeData trade) = putStrLn $ "Received Trade: " ++ show trade
handleMessage (BarData bar) = putStrLn $ "Received Bar: " ++ show bar
handleMessage (BarUpdateData bar) = putStrLn $ "Received Bar Update: " ++ show bar

getNewState :: [AlgoStateF] -> (StreamData -> [AlgoStateF])
getNewState algoStatesF streamData = map (stateTransition streamData) algoStatesF

getBuyOrder :: [AlgoStateF] -> (StreamData -> [Bool])
getBuyOrder algoStatesF streamData = map (buyLogic streamData) algoStatesF

readLoop :: Connection -> [AlgoStateF] -> IO ()
readLoop conn state = do
  rawMsg <- receiveData conn
  case eitherDecode rawMsg :: Either String [StreamData] of
    Right messages ->
      mapM_
        ( \message -> do
            let newState = getNewState state message
            let buyOrder = getBuyOrder newState message
            putStrLn $ "Buy orders: " ++ show buyOrder
            readLoop conn newState
        )
        messages
    Left err -> do
      putStrLn $ "Could not parse message: " ++ err

      readLoop conn state

data AlgoStateF
  = LVRHState (forall y. StreamData -> (StreamData -> Int -> Int -> Int -> Int -> y) -> y)
  | BRHState (forall y. StreamData -> (StreamData -> Int -> Int -> y) -> y)

stateTransition ::
  StreamData -> AlgoStateF -> AlgoStateF
stateTransition d (LVRHState lvrhF) = LVRHState (lvrhF d lvrhStateTransition) -- pattern match this branch for Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y --
stateTransition (BarData bar) (BRHState brhF) = BRHState (brhF (BarData bar) brhStateTransition) -- pattern match this branch for Data -> (Data -> Int -> Int -> y) -> y --
-- Pass-by logic if algo state not affected by current incoming data (ie. BRH algo not affected by a Trade) --
-- We can do something like the following. This avoids calling brh_stateTransition, which in turn would generate a  new brh_state --
stateTransition (TradeData trade) (BRHState brhF) = BRHState brhF

buyLogic ::
  StreamData -> AlgoStateF -> Bool
buyLogic d (LVRHState lvrhF) = lvrhF d lvrhBuyLogic
buyLogic d (BRHState brhF) = brhF d brhBuyLogic

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

        readLoop conn [LVRHState $ lvrhState 0 0 0 0, BRHState $ brhState 0 0]
    _ -> do
      putStrLn "ERROR: Missing environment variables."
      putStrLn "Please set ALPACA_API_KEY and ALPACA_API_SECRET."

{-

data Data = Trade | Bar deriving (Show)

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