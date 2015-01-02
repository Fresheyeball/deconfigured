module Application.Types where

-- | The environment accessible from our application
data Env = Env
  { envHostname :: String }
