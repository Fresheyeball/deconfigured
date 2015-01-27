{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Application
import Application.Types

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import           Web.Scotty.Trans hiding (header)
import qualified Data.Text as T

import System.Directory
import System.IO
import GHC.Generics

import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader

-- | Application-wide options
data AppOpts = AppOpts
  { port :: Maybe Int
  , host :: Maybe String
  , prefix :: Maybe String }
  deriving (Show, Eq, Generic)

-- | Like a monoid, but as a "setter"
class Override a where
  override :: a -> a -> a

instance Override (Maybe a) where
  Nothing `override` Nothing = Nothing
  Nothing `override` (Just a) = Just a
  (Just a) `override` Nothing = Just a
  (Just a) `override` (Just b) = Just b

instance Override AppOpts where
  (AppOpts p h pr) `override` (AppOpts p' h' pr') =
    AppOpts
      (p `override` p')
      (h `override` h')
      (pr `override` pr')

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts (Just 3000) (Just "http://localhost") (Just "./")

-- | Command-invocation parser for ubiquitous options
appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> optional ( option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on" ))
  <*> optional ( strOption
        ( long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "host to deploy URLs over" ))
  <*> optional ( strOption
        ( long "prefix"
       <> metavar "PREFIX"
       <> help "folder the app is being deployed from" ))

-- | Command-line options
data App = App
  { options :: AppOpts
  , configPath :: Maybe String }
    deriving (Show, Eq)

-- | Completed command invocation parser
app :: Parser App
app = App
  <$> appOpts
  <*> optional ( strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "path to config file" ))

main :: IO ()
main = do
  (commandOpts :: App) <- execParser opts

  let yamlConfigPath = fromMaybe
        "config/config.yaml" $
        configPath commandOpts

  -- Yaml bug
  yamlConfigExists <- doesFileExist yamlConfigPath
  yamlConfigSize <-
    if yamlConfigExists
      then do
        yamlConfigHandle <- openFile yamlConfigPath ReadMode
        hFileSize yamlConfigHandle
      else return 0

  (mYamlConfig :: Maybe AppOpts) <- if yamlConfigSize /= 0
        then Y.decodeFile yamlConfigPath
        else return Nothing

  let yamlConfig :: AppOpts
      yamlConfig = fromMaybe
        def
        mYamlConfig

      -- Merge command-line and config file options
      config :: AppOpts
      config =
        (def `override` yamlConfig)
          `override` options commandOpts

  entry (fromJust $ port config) $ appOptsToEnv config

  where
    opts :: ParserInfo App
    opts = info (helper <*> app)
      ( fullDesc
     <> progDesc "Serve application in PREFIX over HOST:PORT"
     <> header "deconfigured - a web server" )

-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> Env
appOptsToEnv (AppOpts (Just p) (Just h) (Just pr)) =
  Env pr $ h <> ":" <> show p
appOptsToEnv (AppOpts Nothing _ _) =
  error "default overrides failed somehow... VOODOO"

-- | Entry point, post options parsing
entry :: Int -> Env -> IO ()
entry port env = do
  let -- Access the url root from a @ScottyT@ or @ActionT@ expression with
      -- @(root :: T.Text) <- lift ask@.
      hostConf :: ReaderT Env m a -> m a
      hostConf =
        flip runReaderT env

  putStrLn $ "Using Environment: " <> show env

  scottyT port
    hostConf
    hostConf
    application
