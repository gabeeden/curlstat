{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


import           Data.Maybe
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics
import           System.Environment             ( lookupEnv )
import           Network.CURL730
import           Control.Monad.IO.Class
import           Servant
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Throttle
                                                ( defaultThrottleSettings
                                                , throttle
                                                , initThrottler
                                                )
import           System.Clock
import qualified Data.ByteString.Char8         as B


data TestResult = TestResult {
  nameLookupTime :: Double,
  connectTime :: Double,
  appConnectTime :: Double,
  preTransferTime :: Double,
  startTransferTime :: Double,
  redirectTime :: Double,
  totalTime :: Double
} deriving (Eq, Show, Generic)
instance ToJSON TestResult
instance FromJSON TestResult

type ServantType = QueryParam "u" String :> Get '[JSON] TestResult

curlInfo :: String -> IO TestResult
curlInfo u = do
  curl <- curl_easy_init
  curl_easy_setopt
    curl
    [ CURLOPT_URL u
    , CURLOPT_VERBOSE False
    , CURLOPT_WRITEFUNCTION (Just writeNull)
    ]
  curl_easy_perform curl
  nlt <- curl_easy_getinfo curl CURLINFO_NAMELOOKUP_TIME
  ct  <- curl_easy_getinfo curl CURLINFO_CONNECT_TIME
  act <- curl_easy_getinfo curl CURLINFO_APPCONNECT_TIME
  ptt <- curl_easy_getinfo curl CURLINFO_PRETRANSFER_TIME
  st  <- curl_easy_getinfo curl CURLINFO_STARTTRANSFER_TIME
  rt  <- curl_easy_getinfo curl CURLINFO_REDIRECT_TIME
  tt  <- curl_easy_getinfo curl CURLINFO_TOTAL_TIME
  return $ TestResult nlt ct act ptt st rt tt

handlerApi :: Maybe String -> Handler TestResult
handlerApi u = liftIO . curlInfo $ fromJust u

server :: Server ServantType
server = handlerApi

writeNull :: CURL_write_callback
writeNull b = do
  writeFile "/dev/null" $ B.unpack b
  return CURL_WRITEFUNC_OK

app :: Application
app = serve (Proxy :: Proxy ServantType) server

main :: IO ()
main = do
  t         <- fromMaybe "World" <$> lookupEnv "TARGET"
  pStr      <- fromMaybe "8080" <$> lookupEnv "PORT"
  throttler <- initThrottler $ defaultThrottleSettings $ TimeSpec 3600 0
  let f = throttle throttler
  let p = read pStr :: Int
  run p (f app)