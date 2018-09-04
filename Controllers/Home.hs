{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Controllers.Home
    ( home
    , login
    , post
    ) where


import           Text.Blaze.Html (Html)
import           Views.Home (homeView)
import           Web.Scotty (ScottyM, get, html, json)
import           Data.Aeson (ToJSON)
import           GHC.Generics

home :: Html -> ScottyM ()
home xml = get "/" (homeView xml)

login :: ScottyM ()
login = get "/login" $ html "login"

{-
  Example data structure to demonstrate JSON serialization
-}
data Post = Post
  { postId    :: Int
  , postTitle :: String } deriving Generic

instance ToJSON Post

post :: ScottyM()
post = get "/post" $ json $ Post 1 "Yello world"

