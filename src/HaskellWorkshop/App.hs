{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorkshop.App where

import Prelude hiding (unlines)
import Data.Char
import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.List ((\\), intercalate, nub, sort)
import Data.Maybe (fromJust)
import GHC.Generics
import System.FilePath ((</>))
import System.Directory

import Data.Text.Lazy (pack)
import Data.Yaml
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Markdown
import Text.Regex.Posix

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


unlines = intercalate "\n"

applyIf b f x = if b then f x else x

(|>) = flip ($)

quoted s = "`" ++ s ++ "`"

linkTo h s = "[" ++ s ++ "](" ++ h ++ ")"

bulletList =
  unlines . map bullet
  where
    bullet item = "  * " ++ item

data HaskellFeature
  = BasicTypes
  | BasicSyntax
  | BasicData
  | DoSyntax
  | Main
  | LanguageExtensions
  | Lazyness
  | Purity
  | TypeClasses
  | PatternMatching
  | AdvancedTypes
  | Libraries
  | PropertyTesting
  | PartialFunctions
  | Abstraction
  | Operators
  | Functions
  | Records
  | Recursion
  | GeneralProgramming
  | Sectioning
  | StringsAsLists
  | MutableState
  | Currying
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance FromJSON HaskellFeature
instance ToJSON HaskellFeature

allFeatures :: [HaskellFeature]
allFeatures = [minBound..maxBound]

prettyFeatures :: [HaskellFeature] -> String
prettyFeatures = bulletList . nub . sort . map (quoted . show)

data Item
  = Item
  { itemTitle :: String
  , itemFilename :: String
  , itemDescription :: String
  , itemDependencies :: [HaskellFeature]
  , itemIntroductions :: [HaskellFeature]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Item
instance ToJSON Item

readItemFromFile :: FilePath -> IO Item
readItemFromFile path =
  decodeFileEither path >>=
    \case
      Left err   -> fail (show err)
      Right item -> return item

slugified = intercalate "-" . words . map (clean . toLower)
  where
    clean c
      | isAlphaNum c = c
      | otherwise    = ' '

anchoredItemTitle item =
  "<a name=\"" ++ slugified title ++ "\"></a>\n\n" ++ title
  where
    title = itemTitle item

linkedItemTitle item =
  linkTo ("#" ++ slugified title) title
  where
    title = itemTitle item

prettyItem dirPath item = do
  fileContents <- readFile (dirPath </> itemFilename item)
  return $ unlines $
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

---

allItems :: FilePath -> IO [Item]
allItems dirPath = do
  paths <- listDirectory dirPath
  let yamlFiles = map (dirPath </>) . filter (=~ pattern) $ paths
  mapM readItemFromFile yamlFiles
  where
    pattern = "\\.yaml$" :: String

presentationOrder items =
  allNodes
  |> stronglyConnComp
  |> flattenSCCs
  |> nub
  |> tail  -- gets rid of the "root" node, which naturally rises to the top
  |> map fromJust
  where
    rootNode = (Nothing, Nothing, [])

    allNodes = rootNode : concatMap toNodes items

    toNodes item =
      -- `Maybe` is used to keep the graph connected
      -- with a global dependency to a "root" node,
      -- which does not change the overall topography.
      map (\ft -> (Just item, Just ft, Nothing : fmap Just outwards)) local
      where
        local = itemIntroductions item
        outwards = itemDependencies item

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

allIntroductions items =
  concatMap itemIntroductions items

featuresNotIntroduced items =
  allFeatures \\ allIntroductions items

---

renderPresentation doc =
  renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.style "body { max-width: 800px; margin: 40px auto; }"
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

app _ respond = do
  docHtml <- renderPresentation <$> presentationDocument "res"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/html")]
      docHtml

---