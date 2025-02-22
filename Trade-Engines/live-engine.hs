{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Algos.BRH (BRHStateParam (..), brhBuyLogic, brhState, brhStateTransition)
import Algos.LVRH (LVRHStateParam (..), lvrhBuyLogic, lvrhState, lvrhStateTransition)
import Data.Aeson (Value, eitherDecode, encode, object, (.:), (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text, pack)
import Lib.DataTypes (StreamData (..))
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

data AlgoState
  = LVRHState (forall y. StreamData -> (LVRHStateParam -> StreamData -> y) -> y)
  | BRHState (forall y. StreamData -> (BRHStateParam -> StreamData -> y) -> y)

stateTransition :: StreamData -> AlgoState -> AlgoState
stateTransition streamData (LVRHState lvrhF) = LVRHState (lvrhF streamData lvrhStateTransition) -- pattern match this branch for Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y --
stateTransition (BarData bar) (BRHState brhF) = BRHState (brhF (BarData bar) brhStateTransition) -- pattern match this branch for Data -> (Data -> Int -> Int -> y) -> y --
-- Pass-by logic if algo state not affected by current incoming data (ie. BRH algo not affected by a Trade) --
-- We can do something like the following. This avoids calling brh_stateTransition, which in turn would generate a  new brh_state --
stateTransition (TradeData trade) (BRHState brhF) = BRHState brhF

buyLogic :: StreamData -> AlgoState -> Bool -> Bool
buyLogic streamData (LVRHState lvrhF) buyState = lvrhF streamData (lvrhBuyLogic buyState)
buyLogic streamData (BRHState brhF) buyState = brhF streamData (brhBuyLogic buyState)

getNewState :: [AlgoState] -> (StreamData -> [AlgoState])
getNewState algoStatesF streamData = map (stateTransition streamData) algoStatesF

getNewBuyState :: [AlgoState] -> [Bool] -> (StreamData -> [Bool])
getNewBuyState algoStatesF buyStates streamData = zipWith (buyLogic streamData) algoStatesF buyStates

readLoop :: Connection -> [AlgoState] -> [Bool] -> IO ()
readLoop conn state buyState = do
  -- rawMsg <- receiveData conn
  let rawMsg = BL.pack $ "[{\"T\":\"t\",\"i\":96921,\"S\":\"AAPL\",\"x\":\"D\",\"p\":126.55,\"s\":1,\"t\":\"2021-02-22T15:51:44.208Z\",\"c\":[\"@\",\"I\"],\"z\":\"C\"}]"
  case eitherDecode rawMsg :: Either String [StreamData] of
    Right messages ->
      mapM_
        ( \message -> do
            let newState = getNewState state message
            let newBuyState = getNewBuyState newState buyState message
            handleMessage message
            putStrLn $ "Buy orders: " ++ show newBuyState
            readLoop conn newState newBuyState
        )
        messages
    Left err -> do
      putStrLn $ "Could not parse message: " ++ err

      readLoop conn state buyState

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

        let lvrhInitParams =
              LVRHStateParam
                { openPrice = 0.0,
                  highPrice = 0.0,
                  lowPrice = 0.0,
                  lastPrice = 0.0,
                  openTime = 0.0,
                  firstTime = 0.0,
                  lastTime = 0.0
                }

        let brhInitParams =
              BRHStateParam
                { openPrice = 0.0,
                  highPrice = 0.0,
                  lowPrice = 0.0,
                  lastPrice = 0.0,
                  openTime = 0.0,
                  firstTime = 0.0,
                  lastTime = 0.0
                }

        readLoop conn [LVRHState $ lvrhState lvrhInitParams, BRHState $ brhState brhInitParams] [False, False]
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