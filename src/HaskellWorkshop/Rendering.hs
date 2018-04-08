{-# LANGUAGE OverloadedStrings #-}

module HaskellWorkshop.Rendering
  ( presentationDocument
  , renderPresentation
  ) where

import Data.ByteString.Lazy             (ByteString)
import Data.Char
import Data.List
import Data.Text.Lazy                   (Text, pack)
import System.FilePath                  ((</>))
import Text.Blaze.Html.Renderer.Utf8    (renderHtml)
import Text.Markdown

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HaskellWorkshop.Lecture
import HaskellWorkshop.Resources
import HaskellWorkshop.Types
import HaskellWorkshop.Util


quoted :: String -> String
quoted s = "`" ++ s ++ "`"


linkTo :: String -> String -> String
linkTo h s = "[" ++ s ++ "](" ++ h ++ ")"


bulletList :: [String] -> String
bulletList =
  intercalate "\n" . map bullet
  where
    bullet item = "  * " ++ item


prettyFeatures :: [HaskellFeature] -> String
prettyFeatures = bulletList . nub . sort . map (quoted . show)


slugified :: String -> String
slugified = intercalate "-" . words . map (clean . toLower)
  where
    clean c
      | isAlphaNum c = c
      | otherwise    = ' '


anchoredItemTitle :: Item -> String
anchoredItemTitle item =
  "<a name=\"" ++ slugified title ++ "\"></a>\n\n" ++ title
  where
    title = itemTitle item


linkedItemTitle :: Item -> String
linkedItemTitle item =
  linkTo ("#" ++ slugified title) title
  where
    title = itemTitle item


prettyItem :: FilePath -> Item -> IO String
prettyItem dirPath item = do
  fileContents <- readFile (dirPath </> itemFilename item)
  return $ intercalate "\n" $
    [ anchoredItemTitle item
    , "---"
    , ""
    , itemDescription item
    ]
    |> applyIf
        (not . null $ intros)
        (++ ["", "Introduces:", "", prettyFeatures intros])
    |> applyIf
        (not . null $ deps)
        (++ ["", "Requires:", "", prettyFeatures deps])
    |> (++ ["", codeBlock fileContents])
  where
    intros = itemIntroductions item
    deps   = itemDependencies item

    codeBlock text =
      "```haskell\n"
        ++ "-- " ++ itemFilename item
        ++ "\n\n"
        ++ text
        ++ "\n```"


presentationDocument :: FilePath -> IO String
presentationDocument dirPath = do
  items <- presentationOrder <$> allItems dirPath
  articles <- mapM (prettyItem dirPath) items
  return $
    "Haskell Workshop @<img height=\"30px\" src=\"https://www.underconsideration.com/brandnew/archives/eventbrite_logo.png\">"
    ++ "\n===\n\n\n"
    ++ index items
    ++ "\n\n\n"
    ++ intercalate "\n\n\n" articles
  where
    index = bulletList . map linkedItemTitle


renderPresentation :: String -> ByteString
renderPresentation doc =
  renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.title "Haskell Workshop"
      H.style "body { max-width: 800px; margin: 40px auto; } p { text-align: justify; }"
      H.link
        H.! A.rel "stylesheet"
        H.! A.href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css"
      H.script ""
        H.! A.src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
      H.script ""
        H.! A.src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js"
      H.link
        H.! A.rel "stylesheet"
        H.! A.href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/github.min.css"
      H.script "hljs.initHighlightingOnLoad();"
    H.body H.! A.class_ "markdown-body" $ do
      markdown defaultMarkdownSettings (pack doc)
