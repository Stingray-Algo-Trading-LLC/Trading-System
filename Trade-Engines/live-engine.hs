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

lvrh_state ::
  Int -> Int -> Int -> Int -> (Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y)
lvrh_state a b c d = \data_0 f -> f data_0 a b c d

lvrh_state_transition ::
  Data -> Int -> Int -> Int -> Int -> (Data -> (Data -> Int -> Int -> Int -> Int -> y) -> y)
lvrh_state_transition Trade a b c d =
  let new_a = a + 10
      new_b = b + 100
   in lvrh_state new_a new_b c d
lvrh_state_transition Bar a b c d =
  let new_c = c + 200
      new_d = d + 300
   in lvrh_state a b new_c new_d

lvrh_buy_logic ::
  Data -> Int -> Int -> Int -> Int -> Bool
lvrh_buy_logic Trade a b c d = True
lvrh_buy_logic Bar a b c d = False

brh_state ::
  Int -> Int -> (Data -> (Data -> Int -> Int -> y) -> y)
brh_state a b = \data_0 f -> f data_0 a b

brh_state_transition ::
  Data -> Int -> Int -> (Data -> (Data -> Int -> Int -> y) -> y)
brh_state_transition Trade a b =
  let new_a = a + 1000
   in brh_state new_a b
brh_state_transition Bar a b =
  let new_b = b + 2000
   in brh_state a new_b

brh_buy_logic ::
  Data -> Int -> Int -> Bool
brh_buy_logic Trade a b = False
brh_buy_logic Bar a b = True

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

