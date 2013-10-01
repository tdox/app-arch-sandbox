--------------------------------------------------------------------------------
--
--  Copyright (c) 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------

-- * base
import Control.Monad (when)
import Data.Maybe    (isNothing)
import Data.Monoid   ((<>))

-- * bytestring
import Data.ByteString (ByteString)

-- * text
import Data.Text          (Text)

-- * yesod
import Yesod

-- * yesod-auth
import Yesod.Auth

-- *
import CurrentDoc

--------------------------------------------------------------------------------

data App = App {aKeyBS :: ByteString}


mkYesod "App" [parseRoutes|
/    RootR GET               
/pg1 Pg1R  GET

|]


instance Yesod App where

                       

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest  _ = RootR
    logoutDest _ = RootR

{-
    authPlugins _ =
        [ authBrowserId
        , authGoogleEmail
        ]

    authHttpManager = httpManager
-}  

  -- design for real system:
  --   Get the key from the database.  The key was created by the system, which
  --   is then stored the key in the database.
    
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

--------------------------------------------------------------------------------



getRootR :: Handler ()
getRootR = redirect proxyRoot


getCurrDocInfo :: Handler (Either String CurrentDocumentInfo)
getCurrDocInfo = do
  App keyBS  <- getYesod
  request      <- getRequest
  let cookies = reqCookies request
    
  return $ getCurrDocInfoFromCookies keyBS cookies
  


getPg1R :: Handler RepHtml

getPg1R = do
  
  maid <- maybeAuthId
  
--  liftIO $ print maid
--  redirect (proxyAppRoot <> "/system/login")
  
  when (isNothing maid) (redirect (proxyRoot <> "/system/login"))
    
  eCurrDocInfo <- getCurrDocInfo
  
  case eCurrDocInfo of
    Right (CurrentDocumentInfo orgName projName docId _ _) ->
  
      defaultLayout [whamlet|
<h1>Pg1
<p>#{orgName}:#{projName}:#{(*) 2 docId}
<p><a href=#{proxyRoot}>System Home
|]

    Left _ -> redirect proxyRoot


  
main :: IO ()
main = do
--  man   <- newManager def
  keyBS <- readKeyFromDB
  warpDebug docPort $ App keyBS

--------------------------------------------------------------------------------
  
{-
getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|
<h1>Home
<p><a href=@{Pg1R}/>pg1
<p><a href=@{Pg2R}/>pg1
|]

getPg1R :: Handler RepHtml
getPg1R = do
  eCurrDocInfo <- getCurrDocInfo
      
  case eCurrDocInfo of
    Left errMsg -> return $ RepHtml $ toContent errMsg
    Right (CurrentDocumentInfo orgName projName docId _ _) ->
      
      return $ RepHtml $ toContent $ orgName <> ":" <> projName
                                   <> ":" <> (pack $ show $ docId*2)
-}
                                 
  
  
{-

data MyNumber = MyNumber Int deriving Show

numberForm :: Html -> MForm App App (FormResult MyNumber, Widget)
numberForm = renderDivs $ MyNumber <$> areq intField "Number" Nothing

-- numberForm :: Html -> MForm App App (FormResult MyNumber, Widget)
-- numberForm = renderDivs $ numberMForm

getPg3R :: Handler RepHtml
getPg3R = do
  App keyBS <- getYesod
  let (CurrentDocumentInfo orgName projName _ _) = readCurrDocCookie
  
  (widget, enctype) <- generateFormPost numberForm
  defaultLayout [whamlet|
<h1>Form
<form method=post action=@{Pg3R orgName projName} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]                  

postPg3R :: Handler RepHtml
postPg3R = do
  App keyBS <- getYesod
  let (CurrentDocumentInfo orgName projName _ _) = readCurrDocCookie
  
  ((result, widget), enctype) <- runFormPost numberForm
  
  case result of
    FormSuccess (MyNumber i) -> defaultLayout [whamlet|
<h1>Pg3 Post
<p>#{orgName}:#{projName}:#{(*) 2 i}
|]
    _ -> defaultLayout [whamlet|
<p>Invalid input. Try again
<form method=post action=@{Pg3R} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]                  

                                           

getSessionR :: Handler RepHtml
getSessionR = do
    sess <- getSession
    hamletToRepHtml [hamlet|
<form method=post>
    <input type=text name=key>
    <input type=text name=val>
    <input type=submit>
<h1>#{show sess}
|]

postSessionR :: Handler ()
postSessionR = do
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
    case mval of
        Nothing -> deleteSession key
        Just val -> setSession key val
    liftIO $ print (key, mval)
    redirect SessionR
-}


