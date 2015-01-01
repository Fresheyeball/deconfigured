{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Application where

import Server

import UrlPath
import Web.Page.Lucid

import Web.Scotty.Trans
import Network.Wai.Middleware.Static
import Lucid
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.String
import Data.Monoid
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Trans

-- The environment accessible from our application
data Env = Env
  { envHostname :: String }

application :: ( MonadReader Env m
               , MonadIO m
               , Functor m
               ) => ScottyT LT.Text m ()
application = do
  middleware $ staticPolicy (noDots >-> addBase "static")

  notFound $ do
    (root :: T.Text) <- (T.pack . envHostname) <$> lift ask

    let page :: Monad m => WebPage (HtmlT m ()) T.Text
        page = mempty { metaVars = meta_ [ makeAttribute "http-equiv" "refresh"
                                         , content_ $ "3;url=" <> root
                                         ]
                      }

        -- Coerce the monad inside to change the deployment scheme.
        content :: HtmlT (AbsoluteUrlT T.Text Identity) ()
        content = template page $
          div_ [] $ mconcat
            [ h1_ [] "Not Found!"
            , p_ [] $ mconcat
              [ "Your request was not found. Redirecting you to "
              , do
                home <- lift $ plainUrl ""
                a_ [href_ home] "the homepage"
              , "..."
              ]
            ]

    html $ runIdentity $ (runUrlReader $ renderTextT content) root
