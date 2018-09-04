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
  Element "html" M.empty childrenTransformed
  where
    childrenTransformed = concatMap goNode children

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- Convert tag strings to formal names
-- Assumes TEI namespace.
toName tag = Name tag (Just "http://www.tei-c.org/ns/1.0") Nothing
lg = toName "lg"
l = toName "l"
head = toName "head"
-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element "lg" attrs children) =
    Element "div" attrs $ concatMap goNode children
goElem (Element (toName "l") attrs children) =
    Element "span" attrs $ concatMap goNode children
goElem (Element (toName "head") attrs children) =
    Element "h1" attrs $ concatMap goNode children
goElem (Element (toName "teiHeader") attrs children) =
    Element "div" (hidden attrs) $ concatMap goNode children
  where
    hidden mattrs = M.insert "class" "hidden" mattrs
goElem (Element "image" attrs _children) =
    Element "img" (fixAttr attrs) [] -- images can't have children
  where
    fixAttr mattrs
        | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
        | otherwise                 = mattrs
goElem (Element name attrs children) =
    -- don't know what to do, just pass it through...
    Element name attrs $ concatMap goNode children
