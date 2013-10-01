--------------------------------------------------------------------------------
--
--  Copyright (c) 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------

-- * base
import Data.Monoid   ((<>))

-- * blaze-builder
-- import Blaze.ByteString.Builder

-- * bytestring
import Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as BS

-- * clientsession
import qualified Web.ClientSession as CS

-- * cookie
import Web.Cookie (SetCookie(..))

-- * containers
import Data.Map (Map, (!), fromList)

-- * errors
-- import Control.Error.Util (note)

-- * http-conduit
import Network.HTTP.Conduit (Manager, newManager, def)

-- * text
import Data.Text          (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- * time
-- import Data.Time.Clock (getCurrentTime)

-- * yesod
import Yesod

-- * yesod-auth
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail

-- *
import CurrentDoc

--------------------------------------------------------------------------------

data System = System { httpManager :: Manager
                     , sKeyBS      :: ByteString
                     }
    
{-
docHost :: Host
docHost = "localhost"

docPort :: Port
docPort = 3002
-}

docs :: [(Text, Text, DocumentId, Host, Port)]    
docs = [ ("org1", "doc1", 1, docHost, docPort)
       , ("org2", "doc2", 2, docHost, docPort)
       , ("org3", "doc3", 3, docHost, docPort)
       , ("org4", "doc4", 4, docHost, docPort)
       , ("org5", "doc5", 5, docHost, docPort)
       ]
       

systemAppRoot :: Text
systemAppRoot = "http://" <> decodeUtf8 systemHost <> ":"
                          <> pack (show systemPort)


--------------------------------------------------------------------------------



mkYesod "System" [parseRoutes|
/                              RootR         GET
/system/documentList           DocumentListR GET
/system/goToDocApp/#Text/#Text GoToDocAppR   GET
/system/login                  SysLoginR     GET

/auth                          AuthR Auth getAuth
|]

instance Yesod System where
  
  -- In order for the auth subsite to correctly work with Persona (BrowserID)
  -- and Google, I must use a static approot.
  
  approot = ApprootStatic systemAppRoot


  -- Because the approot is static, in order to display the the proxy
  -- server as the host for the other links, I must change how the other URLs
  -- are rendered.

  urlRenderOverride y url0 =
    if (not (null ps) &&  head ps == "auth")
    then Nothing
    else Just $ joinPath y root ps params'
         
    where
      (ps, params') = renderRoute url0
      
      root = "http://" <> (decodeUtf8 proxyHost) <> ":"
                       <> (pack (show proxyPort)) :: Text
                                                     
                       

instance YesodAuth System where
    type AuthId System = Text
    getAuthId = return . Just . credsIdent

    loginDest  _ = RootR
    logoutDest _ = RootR

    authPlugins _ =
        [ authBrowserId
        , authGoogleEmail
        ]

    authHttpManager = httpManager


instance RenderMessage System FormMessage where
    renderMessage _ _ = defaultFormMessage

--------------------------------------------------------------------------------

docMap :: Map Text (DocumentId, (Host, Port))
docMap = fromList $ map (\(o, p, iD, h, port) -> (mkKey o p, (iD, (h, port))))
                        docs
        

mkKey :: Text -> Text -> Text
mkKey o p = o <> ":" <> p


getRootR :: Handler RepHtml
getRootR = do
    maid <- maybeAuthId
    liftIO $ putStrLn $ "maid: " ++ (show maid)
    System _ keyBS <- getYesod
    request <- getRequest
    
    let
      cookies      = reqCookies request
      eCurrDocInfo = getCurrDocInfoFromCookies keyBS cookies
      currDocTxt   = show eCurrDocInfo

        
        
    defaultLayout [whamlet|
<p>Your current auth ID: #{show maid}
$maybe _ <- maid
    <p>
      <li> <a href=@{AuthR LogoutR}>Logout
      <li> <a href=@{DocumentListR}>My Documents
    <p>  
       #{show cookies}
       #{show currDocTxt}
$nothing
    <p>
        <a href=@{AuthR LoginR}>Go to the login page
|]





getGoToDocAppR :: Text -> Text -> Handler ()
getGoToDocAppR orgName docName = do

  System _ keyBS <- getYesod
  
  -- set the current document cookie
  
  let (docId, (host, port)) = docMap ! mkKey orgName docName
      cpi = CurrentDocumentInfo orgName docName docId host port
      
  cpiCookie <- liftIO $ mkCurrDocCookie keyBS cpi

                                         
  setCookie def{ setCookieName  = encodeUtf8 "doc"
               , setCookieValue = cpiCookie
               , setCookiePath  = Just "/"
               }
    
  -- create a user-readable link to the document that the proxy
  -- will check against in convertPath (wai_proxy.hs)

  redirect $ proxyRoot <> "/app/" <> orgName <> "/" <> docName
                       <> "/" <> "pg1"



getDocumentListR :: Handler RepHtml
getDocumentListR = do
  
      
  defaultLayout [whamlet|
<h1> My Documents
<ul>                                
   $forall (org, doc, _, _, _) <- docs
     <li><a href=@{GoToDocAppR org doc}>#{org}:#{doc}</a>
|]


getSysLoginR :: Handler ()
getSysLoginR = redirect $ AuthR LoginR

--------------------------------------------------------------------------------
     
main :: IO ()
main = do
    man <- newManager def
    (keyBS, _) <- CS.randomKey
    writeKeyToDB keyBS
    warpDebug systemPort $ System man keyBS
    
--------------------------------------------------------------------------------


{-

NOTE: could not get this to compile.  Decided to switch to cookie method

getSessionMapR :: {- IO ClientSessionDateCache -- ^ current time
               -> -}
                  CS.Key 
               -> Handler RepHtml
               
getSessionMapR {- getCachedDate -} key = do
  date <- liftIO getCurrentTime
  request :: Request <- getRequest
  let cookies = reqCookies request :: [(Text, Text)]
      
  -- from Yesod/Internal/Core/loadClientSession2    
  let req :: W.Request = undefined
      mSessionMap :: [(Text, ByteString)] =  fromMaybe [] $ do
      raw :: ByteString <- lookup "Cookie" $ W.requestHeaders req 
      encrypted <- lookup "_SESSION" $ parseCookies raw -- Maybe ByteString

  -- from Yesod-Internal-Session/decodeClientSession
      decrypted <- CS.decrypt key encrypted

      SessionCookie (Left expire) rhost' session' <-
         either (const Nothing) Just $ decode decrypted

      guard $ expire > date
--      guard $ expire > csdcNow date
--      guard $ rhost' == rhost
      return session'
        
      
      
  error ""

-- copied from Yesod-Internal-Session.hs (1.1.8.3)



data SessionCookie =
  SessionCookie (Either UTCTime ByteString) ByteString [(Text, ByteString)]
  deriving (Show, Read)
             
instance Serialize SessionCookie where
    put (SessionCookie a b c) = do
        either putTime putByteString a
        put b
        put (map (first unpack) c)
    get = do
        a <- getTime
--        b <- getByteString
--        b <- get
        b <- Data.Serialize.get
--        b <- id
        c <- map (first pack) <$> Data.Serialize.get
        return $ SessionCookie (Left a) b c


putTime :: Putter UTCTime
putTime (UTCTime d t) =
  let d' = fromInteger  $ toModifiedJulianDay d
      t' = fromIntegral $ fromEnum (t / diffTimeScale)
  in put (d' * posixDayLength_int64 + min posixDayLength_int64 t')
     
getTime :: Get UTCTime
getTime = do
  val <- Data.Serialize.get
  let (d, t) = val `divMod` posixDayLength_int64
      d' = ModifiedJulianDay $! fromIntegral d
      t' = fromIntegral t
  d' `seq` t' `seq` return (UTCTime d' t')

posixDayLength_int64 :: Int64
posixDayLength_int64 = 86400     

diffTimeScale :: DiffTime
diffTimeScale = 1e12


-}
--------------------------------------------------------------------------------


-- /goToProject3                  GoToProject3R GET POST
-- /session                       SessionR      GET POST
-- /projectHome/#Text/#Text       ProjectHomeR  GET
-- /goToProject/#Text/#Text       GoToProjectR  GET
-- /goToProject2                  GoToProject2R GET
-- /cookies                       CookiesR      GET POST
-- /cookies2                      Cookies2R     GET POST


{-
getProjId :: Text -> Text -> Int
getProjId orgName projName = error ""
  where
    projMap =
-}


{-

getGoToProject2R :: Text -> Text -> Handler RepHtml
getGoToProject2R orgName projName = do
  let (projId, serverName) = projMap ! mkKey orgName projName
      
  setSession "orgName"  orgName
  setSession "projName" projName
  setSession "projId"   $ pack $ show projId
  setSession "serverName" serverName
  
  redirect $ proxyServerProjectPage <> "/" <> orgName
                                    <> "/" <> projName
-}

{-

data ProjId = ProjId Text Text deriving Show



goToProject3Form :: Html
                -> MForm System System (FormResult ProjId, Widget)
                
goToProject3Form = renderDivs $ ProjId <$> areq textField "Org Name"  Nothing
                                      <*> areq textField "Proj Name" Nothing


getGoToProject3R :: Handler RepHtml
getGoToProject3R = do
  (widget, enctype) <- generateFormPost goToProject3Form
  defaultLayout [whamlet|
<h1>Go To Project
<form method=post action=@{GoToProject3R} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]                  

postGoToProject3R :: Handler RepHtml
postGoToProject3R = do
  ((result, widget), enctype) <- runFormPost goToProject3Form
  
  case result of
    FormSuccess (ProjId orgName projName) ->
--      redirect "http:/www.google.com"
      redirect $ ProjectHomeR orgName projName
{-      
      defaultLayout [whamlet|
<h1>Will go to project #{orgName}/#{projName} instead
|]
-}
    _ -> defaultLayout [whamlet|
<p>Invalid input. Try again
<form method=post action=@{GoToProject3R} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]              


-}

{-
getProjectHomeR :: Text -> Text -> Handler RepHtml
getProjectHomeR orgName projName = do
--  let url = mkGoToProjUrl orgName projName
      
  defaultLayout [whamlet|
<h1>Links to projects
<p>
  <a href=@{GoToProjectR orgName projName}>Project: #{orgName} > #{projName}

|]                
-}


--     <li><a href=@{GoToProject2R}>#{org}:#{proj}</a>
--     <li><a href=@{GoToProjectR org proj}>#{org}:#{proj}</a>

{-
mkGoToProjUrl :: Text -> Text -> Route System
mkGoToProjUrl orgName projName = GoToProjectR orgName projName
-}

{-
getSessionR :: Handler RepHtml
getSessionR = do
    sess <- getSession
    hamletToRepHtml [hamlet|
<h1> Session                     
<form method=post>
    <input type=text name=key>
    <input type=text name=val>
    <input type=submit>
<p>
  #{show sess}
|]

postSessionR :: Handler ()
postSessionR = do
  
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key"
                                      <*> iopt textField "val"
                                      
    case mval of
        Nothing -> deleteSession  key
        Just val -> setSession key val
        
    liftIO $ print (key, mval)
    redirect SessionR
-}


{-
getCookiesR :: Handler RepHtml
getCookiesR = do
  
  date <- liftIO $ getCurrentTime
  
  setCookie def{ setCookieName  = encodeUtf8 "get-cookies"
               , setCookieValue = BS.pack $ show date
               }
  
  request <- getRequest
  let cookies = reqCookies request
  defaultLayout [whamlet|
<h1> Cookies                 
<form method=post>
    <input type=text name=key>
    <input type=text name=val>
    <input type=submit>
<p>
  #{show cookies}
|]
                 


postCookiesR :: Handler ()
postCookiesR = do
  
  (key, mval) <- runInputPost $ (,) <$> ireq textField "key"
                                    <*> ireq textField "val"
--                                    <*> iopt textField "val"
                                      
--  case mval of
--    Nothing ->  error "postCookiesR" -- deleteCookie key
--    Just val -> setCookie key val
  
  setCookie def{ setCookieName  = encodeUtf8 key
               , setCookieValue = encodeUtf8 mval
               }
        
  setCookie def{ setCookieName  = encodeUtf8 "post-cookies"
               , setCookieValue = encodeUtf8 "post-cookies"
               }
  
  liftIO $ print (key, mval)
  
  redirect CookiesR


getCookies2R :: Handler RepHtml
getCookies2R = do
  
  date <- liftIO $ getCurrentTime
  
  setCookie def{ setCookieName  = encodeUtf8 "get-cookies2"
               , setCookieValue = BS.pack $ show date
               }
  
  request <- getRequest
  let cookies = reqCookies request
  defaultLayout [whamlet|
<h1> Cookies2
<form method=post>
    <input type=text name=key>
    <input type=text name=val>
    <input type=submit>
<p>
  #{show cookies}
|]
                 


postCookies2R :: Handler ()
postCookies2R = do
  
{-  
  (key, mval) <- runInputPost $ (,) <$> ireq textField "key"
                                    <*> ireq textField "val"
--                                    <*> iopt textField "val"
                                      
--  case mval of
--    Nothing ->  error "postCookiesR" -- deleteCookie key
--    Just val -> setCookie key val
  
  setCookie def{ setCookieName  = encodeUtf8 key
               , setCookieValue = encodeUtf8 mval
               }
        
  setCookie def{ setCookieName  = encodeUtf8 "testkey2"
               , setCookieValue = encodeUtf8 "testval2"
               }
 
  liftIO $ print (key, mval)
-}  
  redirect Cookies2R

-}


{-
    makeSessionBackend _ = do
        key <- CS.getKey CS.defaultKeyFile
        let timeout = fromIntegral 60
        (getCachedDate, _closeDateCacher) <- clientSessionDateCacher timeout
        return $ Just $ clientSessionBackend2 key getCachedDate
--        return $ Just $ clientSessionBackend key 1
-}

--      <li> <a href=@{GoToProject3R}>Go To Project
--      <li> <a href=@{SessionR}>Session Info
--      <li> <a href=@{GoToProjectR "dumOrg" "dumProj"}>Go To Project
--      <li> <a href=@{CookiesR}>Cookies
--      <li> <a href=@{Cookies2R}>Cookies2


{-
getGoToProject2R :: Handler ()
getGoToProject2R = do
  
  date <- liftIO $ getCurrentTime
  
  let cookie = def{ setCookieName  = encodeUtf8 "get-go-to-project2"
                  , setCookieValue = BS.pack $ show date
                  }
               
  liftIO $ print $ "cookie: " ++ show cookie               
  setCookie cookie
  redirect CookiesR

-}