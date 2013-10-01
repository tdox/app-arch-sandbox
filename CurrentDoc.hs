--------------------------------------------------------------------------------
--
--  Copyright (c) 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CurrentDoc where

-- * base
import Data.Monoid   ((<>))
import GHC.Generics
import System.IO (IOMode(WriteMode), hPutStr, withFile)

-- * bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- * cereal
import Data.Serialize (Serialize, decode, encode)

-- * clientsession
import qualified Web.ClientSession as CS

-- * cookie
import Web.Cookie (parseCookiesText)

-- * errors
import Control.Error.Util (note)

-- * http-types
import Network.HTTP.Types.Header (RequestHeaders, hCookie)

-- * text
import Data.Text          (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

--------------------------------------------------------------------------------

    
type DocumentId = Int
type Host       = ByteString
type Port       = Int
    
data CurrentDocumentInfo = CurrentDocumentInfo Text Text DocumentId Host Port
                         deriving Show

data CurrentDocumentInfoStr = CurrentDocumentInfoStr String
                                                     String
                                                     DocumentId
                                                     Host
                                                     Port
                            deriving Generic

instance Serialize CurrentDocumentInfoStr

--------------------------------------------------------------------------------

docHost, proxyHost, systemHost :: Host
docPort, proxyPort, systemPort :: Port


proxyHost = "localhost"
proxyPort = 3000

proxyRoot :: Text
proxyRoot = "http://"  <> decodeUtf8 proxyHost <> ":"
                       <> pack (show proxyPort)
                          
docHost = "localhost"
docPort = 3002

systemHost = "localhost"
systemPort = 3001

--------------------------------------------------------------------------------

-- Make a cookie to store the CurrentDocumentInfo
-- The code follows the examples in Yesod ClientSession

mkCurrDocCookie :: ByteString -> CurrentDocumentInfo -> IO ByteString
mkCurrDocCookie keyBS (CurrentDocumentInfo orgName docName docId host port) =
  do  
  let 
    cpiStr = CurrentDocumentInfoStr (unpack orgName)
                                    (unpack docName)
                                    docId
                                    host
                                    port
                                   
    cpiBS     = encode cpiStr
    Right key = CS.initKey keyBS
      
  iv <- CS.randomIV
  return $ CS.encrypt key iv cpiBS

    
readCurrDocCookie :: ByteString -> ByteString
                  -> Either String CurrentDocumentInfo  

readCurrDocCookie keyBS cpiEncrypted = do
  
    key    <- CS.initKey keyBS
    
    cpiBS0 <- note "readCurrDocCookie: could not decrypt"
                   $ CS.decrypt key cpiEncrypted
                  
    cpiBS1 <- decode cpiBS0    
    
    let (CurrentDocumentInfoStr orgNameStr docNameStr docId host port) = cpiBS1
        
    return $ CurrentDocumentInfo (pack orgNameStr)
                                 (pack docNameStr)
                                 docId
                                 host
                                 port


-- In this sandbox example, the key is simply written to a file.
-- In real app, the key could be stored in a database that is
-- shared between the system, docApp and proxy.

writeKeyToDB :: ByteString -> IO ()
writeKeyToDB keyBS =
  withFile "keyFile" WriteMode $ \h -> hPutStr h $ BS.unpack keyBS

readKeyFromDB :: IO ByteString
readKeyFromDB = do
  keyStr <- readFile "keyFile"
  return $ BS.pack keyStr
  


getCurrDocInfoFromCookies :: ByteString -> [(Text, Text)]
                          -> Either String CurrentDocumentInfo
                             
getCurrDocInfoFromCookies keyBS cookies = do
  docCookie  <- note "getCurrDocInfoFromCookies: could not find doc cookie"
                (lookup "doc" cookies)
                            
  readCurrDocCookie keyBS $ encodeUtf8 docCookie



getCurrDocInfoFromHeaders :: ByteString -> RequestHeaders
                          -> Either String CurrentDocumentInfo
                          
getCurrDocInfoFromHeaders keyBS headers =
  getCurrDocInfoFromCookies keyBS cookies
  where
    reqCookie    = lookup hCookie headers
    cookies      = maybe [] parseCookiesText reqCookie



getOrgAndDocNamesFromCookies :: ByteString -> [(Text, Text)]
                             -> Either String (Text,Text)
                           
getOrgAndDocNamesFromCookies keyBS cookies = do
  currDocInfo <- getCurrDocInfoFromCookies keyBS cookies
  let CurrentDocumentInfo orgName docName _ _ _ = currDocInfo
  return (orgName, docName)
  
  
getOrgAndDocNamesFromHeaders :: ByteString -> RequestHeaders
                             -> Either String (Text,Text)
                           
getOrgAndDocNamesFromHeaders keyBS headers = do
  currDocInfo <- getCurrDocInfoFromHeaders keyBS headers
  let CurrentDocumentInfo orgName docName _ _ _ = currDocInfo
  return (orgName, docName)
  


  