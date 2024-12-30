module State where 
import Control.Monad
exercisesFoundAt :: String 
exercisesFoundAt = 
    "https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State"
data TurnstileState = Locked 
                    | Unlocked 
                    deriving (Eq,Show)
data TurnstileOutput = Thank | Open | Tut 
                     deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank , Unlocked)

push Unlocked = (Tut, Locked)
push Locked   = (Open,Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1,s1)  = coin s0 
      (a2,s2)  = push s1 
      (a3, s3) = push s2 
      (a4, s4) = coin s3 
      (a5, s5) = push s4 
  in   ([a1,a2,a3,a4,a5], s5)

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 = 
    let (a1, s1) = coin s0 
        (a2,s2)  = push s1
    in ([a1,a2], s2)

distractedPerson s0 = 
    let (a1,s1) = coin s0 
    in (,) [a1] s1

hastyPerson s0 = 
    let (a1,s1) = push s0 
    in 
    case a1 of 
        Tut    -> ([a1], s1) 
        Open   -> let (outputs, st) = regularPerson s1 
                 in (a1 : outputs, st)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 = do 
     (st)   <- regularPerson s0
     (st') <- hastyPerson st 
     (st2) <- distractedPerson st' 
     (st3) <- hastyPerson st2 
     return st3

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair b s0 = 
    case b of 
        True -> (False,snd $ do 
            state1 <- regularPerson s0
            state2 <- distractedPerson state1 
            return state2)
        False -> (True,snd $ do 
            state1 <- distractedPerson s0 
            state2 <- regularPerson state1 
            return state2)

newtype State s a = State {runState :: s -> (a, s)}

state :: (s -> (a,s)) -> State s a 
state = State 

instance Functor (State s) where 
    fmap = liftM 

instance Applicative (State s) where 
    pure = return 
    (<*>) = ap 

instance Monad (State s) where 
    return :: a -> State s a 
    return x = state (\s -> (x , s))
    (>>=) :: State s a -> (a -> State s b) -> State s b
    p >>= k = state $ \ s0 -> 
        let (x,s1) = (runState p) s0 
        in (runState (k x)) s1
  
tuesday2 :: State TurnstileState [TurnstileOutput]
tuesday2 = State {runState = tuesday}

compose :: (s -> (a,s)) 
          ->  (a -> (s -> (b,s)))
          -> (s -> (b,s))          {- composed function -}
compose f g = \s0 -> let (a1, s1) = f s0 in (g a1) s1 
{-This lambda expression threads both intermediate results produced by f into those required by g -}

coinS , pushS :: State TurnstileState TurnstileOutput 
coinS = State coin 
pushS = State push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do 
    a1 <- coinS 
    a2 <- pushS 
    a3 <- pushS 
    a4 <- coinS 
    a5 <- pushS 
    return [a1,a2,a3,a4,a5]