module Salo
  ( State(..)
  , Error(..)
  , mkE
  , throw
  , panic
  , run
  , printErr
  ) where

import           Control.Applicative
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- | Language state monad.
data State a = Exception Error | Running a

instance Functor State where
  fmap = liftM

instance Applicative State where
  (<*>) = ap
  pure  = Running

instance Monad State where
  (>>) = (*>)
  (>>=) (Exception e) _ = Exception e
  (>>=) (Running   x) f = f x
  return = pure

-- | Throw some provided error x.
throw :: Error -> State a
throw x = Exception x

-- | Just panic for unknown reasons.
--
-- Please don't use this unless it's absolutely necessary. Seriously, the Haskell ninjas will hunt
-- you down.
panic :: State a
panic = Exception Error
  { message  =
    "Panicked without reason. \nThis is probably an internal error. Please report this bug."
  , position = 0
  }

-- | Run a given state of type a. This can either error out to an `Error` or return a.
run :: State a -> Either Error a
run (Exception e) = Left e
run (Running   a) = Right a

-- | Print a given error to stderr.
printErr :: Error -> IO ()
printErr e = putStrLn "Encountered error, have the record."

-- | An error type.
data Error = Error
  { message  :: String
  , position :: Int
  }

mkE :: String -> Int -> Error
mkE m p = Error { message = m, position = p }
