-- https://www.w3.org/TR/2008/REC-xml-20081126/

{-# LANGUAGE RecordWildCards #-}

module Helios.Text.XML.Types
  ( ToXML(..)
  , DocTypeKind(..)
  , DocType(..)
  , XML(..)
  , Attr(..)
  , ToAttr(..)
  , xml10
  , tag
  , ptag
  , render
  ) where

import Data.Monoid

import Helios.Data.String

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

class ToXML a where
  toXML :: a -> XML

newtype XML
  = XML [Elem]
  deriving (Eq, Ord, Show)

instance Semigroup XML where
  XML x1 <> XML x2
    = XML (x1 <> x2)

instance Monoid XML where
  mempty
    = XML []

data Elem
  = PCDATA
    { pcdata :: String
    }
  | Tag
    { tagType :: TagType
    , tagName :: String
    , attrs :: [Attr]
    , child :: XML
    }
  deriving (Eq, Ord, Show)

data TagType
  = Normal
  | QuestionMark
  | ExclamationMark
  | Comment
  deriving (Eq, Ord, Show)

tag :: String -> [Attr] -> XML
tag tag attrs = XML [Tag Normal tag attrs mempty]

ptag :: String -> [Attr] -> XML -> XML
ptag tag attrs child = XML [Tag Normal tag attrs child]

qtag :: String -> [Attr] -> XML
qtag tag attrs = XML [Tag QuestionMark tag attrs mempty]

xtag :: String -> [Attr] -> XML
xtag tag attrs = XML [Tag ExclamationMark tag attrs mempty]

data Attr
  = Attr String
  | String := String
  deriving (Eq, Ord, Show)

class ToAttr a where
  toAttr :: a -> [Attr]

--------------------------------------------------------------------------------
-- Xml
--------------------------------------------------------------------------------

data Xml
  = Xml
    { version :: String
    , encoding :: String
    , standalone :: Bool
    }
  deriving (Eq, Ord, Show)

instance ToXML Xml where
  toXML Xml{..}
    = qtag "xml"
      [ "version" := version
      , "encoding" := encoding
      , "standalone" := yesNo standalone
      ]

xml10 :: Bool -> XML
xml10 standalone = toXML (Xml "1.0" "UTF-8" standalone)

--------------------------------------------------------------------------------
-- DOCTYPE
--------------------------------------------------------------------------------

data DocTypeKind
  = PUBLIC
    { publicIdentifier :: String
    , systemIdentifier' :: Maybe String
    }
  | SYSTEM
    { systemIdentifier :: String
    }
  deriving (Eq, Ord, Show)

attrsDocTypeKind :: DocTypeKind -> [Attr]
attrsDocTypeKind (PUBLIC publicIdentifier Nothing)
  = Attr <$> ["PUBLIC", quotes publicIdentifier]
attrsDocTypeKind (PUBLIC publicIdentifier (Just systemIdentifier'))
  = Attr <$> ["PUBLIC", quotes publicIdentifier, quotes systemIdentifier']
attrsDocTypeKind (SYSTEM systemIdentifier)
  = Attr <$> ["SYSTEM", quotes systemIdentifier]

data DocType
  = DocType
    { rootElement :: String
    , docTypeKind :: DocTypeKind
    }
  deriving (Eq, Ord, Show)

instance ToXML DocType where
  toXML (DocType rootElement docTypeKind)
    = xtag "DOCTYPE" (Attr rootElement : attrsDocTypeKind docTypeKind)

--------------------------------------------------------------------------------
-- Render
--------------------------------------------------------------------------------

render :: XML -> String
render = unlines . renderXML

renderXML :: XML -> [String]
renderXML (XML elems) = concatMap renderElem elems

renderElem :: Elem -> [String]
renderElem (PCDATA pcdata)
  = [ pcdata
    ]
renderElem (Tag tagType tagName attrs (XML []))
  = [ tagOpen tagType ++ tagName ++ renderAttrs attrs ++ tagClose tagType
    ]
renderElem (Tag tagType tagName attrs child)
  = concat
    [ [ tagOpen tagType ++ tagName ++ renderAttrs attrs ++ tagClose' tagType ]
    , renderXML child
    , [ tagOpen' tagType ++ tagName ++ tagClose' tagType ]
    ]

renderAttrs :: [Attr] -> String
renderAttrs [] = ""
renderAttrs as = concatMap renderAttr as

renderAttr :: Attr -> String
renderAttr (Attr a) = " " ++ a
renderAttr (k := v) = " " ++ k ++ "=" ++ quotes v

tagOpen Normal = "<"
tagOpen QuestionMark = "<?"
tagOpen ExclamationMark = "<!"
tagOpen Comment = "<!--"

tagClose Normal = " />"
tagClose QuestionMark = "?>"
tagClose ExclamationMark = ">"
tagClose Comment = "-->"

tagOpen' Normal = "</"
tagOpen' _ = error "tagOpen'"

tagClose' Normal = ">"
tagClose' _ = error "tagOpen'"
