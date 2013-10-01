--------------------------------------------------------------------------------
--
--  Copyright (c) 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- * bytestring
import Data.ByteString (ByteString)

-- * conduit
import Data.Conduit (ResourceT)

-- * cookie
import Web.Cookie (def, parseCookiesText)

-- * http-conduit
import Network.HTTP.Conduit (Manager, newManager)

-- * http-reverse-proxy
import Network.HTTP.ReverseProxy  ( ProxyDest(..)
                                  , defaultOnExc, waiProxyTo
                                  ) 

-- * http-types
import Network.HTTP.Types.Header (RequestHeaders, hCookie)

-- * text
import Data.Text (Text)

-- * transformers
import Control.Monad.IO.Class (liftIO)

-- * wai
import Network.Wai ( Application, Middleware, Request, Response, pathInfo
                   , requestHeaders
                   )
  
-- * wai-extra
import Network.Wai.Middleware.Rewrite (rewritePure)

-- * warp
import Network.Wai.Handler.Warp (run)

-- *
import CurrentDoc

--------------------------------------------------------------------------------

-- The proxy takes requests and sends them to either the system or to
-- the doc aap

main :: IO ()
main = do
  keyBS <- readKeyFromDB
  run proxyPort $ modApp keyBS $ proxy keyBS


proxy :: ByteString ->  Application
proxy keyBS req = do
  manager :: Manager <- liftIO $ newManager def
                        
  waiProxyTo (mkWaiProxyResponse keyBS)
             defaultOnExc
             manager
             req


modApp :: ByteString -> Middleware
modApp keyBS = rewritePure $ convertPath keyBS
        

-- For paths leading to the system, leave the path unchanged.
-- All other paths lead to the doc app. Strip app/orgName/docName
-- from the path and proxy to the doc app.  The doc app does not
-- need the orgName and docName from the path.  Instead it will
-- read the current docID from the cookie. 

convertPath :: ByteString -> [Text] -> RequestHeaders -> [Text]
convertPath keyBS path0 headers =
  if isPathToSystem path0
  then path0
  else
    
    -- Ensure that the link and the current document cookie match.
    -- The link has the form app/orgName/docName...
    -- orgName and docName should match those in the cookie,
    -- which was set in getGoToDocAppR (system.hs)
    
    case eOrgAndDocNames of
      Left _ -> [""]
      Right (orgName, docName) ->
        if length path0 > 3 && path0 !! 0 == "app"
                            && path0 !! 1 == orgName
                            && path0 !! 2 == docName
    
        then drop 3 path0
        else [""]     

  where
    eOrgAndDocNames = getOrgAndDocNamesFromHeaders keyBS headers
     


isPathToSystem :: [Text] -> Bool
isPathToSystem p = null p || head p == "system" || head p == "auth"
    

mkWaiProxyResponse :: ByteString -> Request
                   -> ResourceT IO (Either Response ProxyDest)
                   
mkWaiProxyResponse keyBS req = do
  let
    path = pathInfo req
    
    headers      = requestHeaders req
    -- code copied from parseWaiRequest'
    reqCookie    = lookup hCookie headers
    cookies      = maybe [] parseCookiesText reqCookie
    eCurrDocInfo = getCurrDocInfoFromCookies keyBS cookies

    CurrentDocumentInfo _ _ _ docuHost docuPort = case eCurrDocInfo of
      Left errMsg -> error $ "mkWaiProxyResponse:" ++ errMsg
      Right cdi   -> cdi    
      
    (host, port) = if isPathToSystem path
                   then (systemHost, systemPort)
                   else (docuHost,    docuPort)
                        
  return $ Right $ ProxyDest host port
