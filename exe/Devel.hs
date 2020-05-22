module Main where

import qualified Data.ByteString.Lazy.Char8             as C8L
import           Language.Javascript.JSaddle            (syncPoint)
import qualified Language.Javascript.JSaddle.WebSockets as JW
import           Network.WebSockets                     (defaultConnectionOptions)

import           App.Frontend
import           App.Server

main :: IO ()
main = run $ \domain port -> do
  jsApp <- JW.jsaddleOr defaultConnectionOptions (mainJS >> syncPoint) $
    JW.jsaddleAppWithJs $ JW.jsaddleJs' (Just $ "http://jsaddle." <> C8L.pack domain <> ":" <> (C8L.pack . show $ port)) False
  pure [(Just "jsaddle", jsApp)]
