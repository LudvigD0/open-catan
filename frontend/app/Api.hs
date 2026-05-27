{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api
  ( requestStartGame
  , requestGameAction
  ) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Miso

import ClientTypes
import Types

apiRoot :: MisoString
apiRoot = "http://localhost:8080"

-- | Våra basic headers vi skickar med i request
jsonHeaders :: [(MisoString, MisoString)]
jsonHeaders =
  [ ("Accept", "application/json")
  , ("Content-Type", "application/json")
  ]

-- | Funktionen som används för att starta spelet
requestStartGame :: Effect parent model Action
requestStartGame =
  requestGameEndpoint "/game/start" Nothing

-- | Skicka en 
requestGameAction :: GameAction -> Effect parent model Action
requestGameAction action =
  requestGameEndpoint "/game/action" (Just (ms (encode action)))

-- | Funktionen som påbörjar request till backend och hanterar svaret
--
-- Eftersom att funktionen retunerar Effect ... så innebär detta sidoeffekter
requestGameEndpoint :: MisoString -> Maybe MisoString -> Effect parent model Action
requestGameEndpoint path maybeBody =
  withSink $ \sink -> do
    bodyValue <- traverse toJSVal maybeBody
    fetch (apiRoot <> path) "POST" bodyValue jsonHeaders
      (\(response :: Response MisoString) ->
          sink (decodeGameResponse (body response)))
      (\(response :: Response MisoString) ->
          sink (ApiRequestFailed (responseError response)))
      TEXT

decodeGameResponse :: MisoString -> Action
decodeGameResponse responseBody =
  case eitherDecode (fromMisoString responseBody :: BL.ByteString) of
    Left err -> ApiRequestFailed (ms err)
    Right response -> GotGameResponse response

responseError :: Response MisoString -> MisoString
responseError response =
  case errorMessage response of
    Just message -> message
    Nothing -> "Request failed"
