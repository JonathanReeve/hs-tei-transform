{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad                        (join)
import           Control.Applicative                  ((<$>))
import           Controllers.Home                     (home, login, post)
import qualified Data.Map as M
import           Data.Maybe                           (fromMaybe)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Prelude hiding                       (readFile, writeFile)
import           System.Environment                   (lookupEnv)
import           Text.Blaze.Html5                     (Html, pre, toHtml)
import           Text.Read                            (readMaybe)
import           Text.XML
import           Web.Scotty                           (middleware, scotty)

main :: IO ()
main = do
  port <- fromMaybe 3000
        . join
        . fmap readMaybe <$> lookupEnv "PORT"
  -- readFile will throw any parse errors as runtime exceptions
  -- def uses the default settings
  Document prologue root epilogue <- readFile def "3020.xml"
  let root' = transform root
      asText = renderText def (Document prologue root' epilogue)
      rendered = toHtml asText

  scotty port $ do
         middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
         middleware logStdoutDev
         home rendered >> login >> post

transform :: Element -> Element
transform (Element _name attrs children) =
  Element "html" M.empty $ concatMap goNode children

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- Convert tag strings to formal names
-- Assumes TEI namespace.

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element name attrs children) =
  case (nameLocalName name) of
    "lg" -> Element "div" lgAttrs transformedChildren
    "l" -> Element "span" M.empty transformedChildren
    "head" -> Element "h1" M.empty transformedChildren
    "teiHeader" -> Element "div" (hidden attrs) transformedChildren
    otherwise -> Element name attrs transformedChildren
  where
    transformedChildren = concatMap goNode children
    hidden mattrs = M.insert "class" "hidden" mattrs
    lgAttrs = M.fromList [("class" :: Name, "stanza")]
