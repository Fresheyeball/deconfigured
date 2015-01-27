module Application.Types where

-- | The environment accessible from our application
data Env = Env
  { envPrefix :: String
  , envHostname :: String }
    deriving (Show, Eq)
