module Reader where

-- As you can see here - reader monad is just a function / computation
-- that takes extra argument
-- computer scientists have a fancy word for that argument
-- they call it ENVIRONMENT
data Reader env a = Reader (env -> a)

-- we need a higher order function
-- that "spins" our computation
-- it basically applies our computation
-- to the enviromnent we are passing in
runReader :: Reader env a -> env -> a
runReader (Reader computation) env = computation env

-- this function captures the essence of MonadReader interface
-- it allows us to get our environment
getEnvironment :: Reader env env
getEnvironment = Reader id

instance Functor (Reader env) where
  fmap f (Reader comp) = Reader $ \env -> (f . comp) env

instance Applicative (Reader e) where
  pure x = Reader $ \env -> x
  (Reader f) <*> (Reader x) = Reader $ \env -> (f env) (x env)

instance Monad (Reader env) where
    return value = Reader $ \env -> value
    computation >>= f = Reader $ \env -> runReader (f (runReader computation env)) env

-- as you may have noticed - what we are basically doing here
-- is create these lambdas all the way
-- so we can thread our environment through !
-- thats it !! that is the essence of monad reader
-- we can compose our Reader computations because they are just functions that expect
-- one more argument - which is your environment !!!
-- it is worth mentioning that our implementation allows us to do READ ONLY operations
-- this is why it is called Reader - you can't modify the environment

example :: Reader Int Int
example = do
    env <- getEnvironment -- get value from environment
    return (env + 1)

main :: IO ()
main = print $ runReader example 1 -- prints 2
