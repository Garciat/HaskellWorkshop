{-# LANGUAGE OverloadedStrings #-}

module HaskellWorkshop.App (app) where

import Network.Wai
import Network.HTTP.Types

import HaskellWorkshop.Rendering


app :: Application
app _ respond = do
  docHtml <- renderPresentation <$> presentationDocument "res"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/html")]
      docHtml
