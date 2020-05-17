{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp     as Warp
import           Network.Wai.Middleware.Gzip  (gzip)
import           Network.Wai.Middleware.Vhost (vhost)
import           Network.Wai.Static.TH
import           Reflex.Dom

mkStaticApp "static"

main :: IO ()
main = do
  let port = 3000
  -- print test_css
  Warp.run port $ vhost [ ((==) (Just . T.encodeUtf8 . T.pack . (<>) "static.localhost:" $ show port) . requestHeaderHost, gzip def $ staticApp True)
                        ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

  -- run port $ vhost [ ((==) (Just . encodeUtf8 . T.pack . (<>) "static.localhost:" $ show port) . requestHeaderHost, gzip def appReloadForStatic)
  --                  , ((==) (Just . encodeUtf8 . T.pack . (<>) "localhost:" $ show port) . requestHeaderHost, sessionMiddleware $ handleTurbolinks $ app $ env pool)
  --                  ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")
