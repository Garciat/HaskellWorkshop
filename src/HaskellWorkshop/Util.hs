module HaskellWorkshop.Util where

applyIf :: Bool -> (a -> a) -> a -> a
applyIf b f x = if b then f x else x

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
