{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- :(

-- https://www.w3.org/TR/xml/
-- https://www.w3.org/TR/xml-names/
-- https://www.w3.org/TR/SVG11/
-- https://www.w3.org/TR/CSS22/
-- https://www.ietf.org/rfc/rfc3987.txt

module Helios.Visual.SVG.Types
  ( module Helios.Text.XML.Types
  , mimeType
  , fileNameExtension
  , namespace
  , doctype
  -- 4
  , Angle(..)
  , Anything(..)
  , Color(..)
  , RGB(..)
  , Coordinate(..)
  , Frequency(..)
  , FrequencyUnit(..)
  , FuncIRI(..)
  , ICCColor(..)
  , IRI(..)
  , Length(..)
  , LengthUnit(..)
  , Name(..)
  , Paint'(..)
  , Paint(..)
  , Inherit(..)
  , ColorKeyword(..)
  , colorKeywordToRGB
  -- 5
  , SVG
  , cm
  , Attr_SVG(..)
  , HasVersion(..)
  , HasBaseProfile(..)
  , HasX(..)
  , HasY(..)
  , HasWidth(..)
  , HasHeight(..)
  , HasViewBox(..)
  , svg
  , g
  , HasRX(..)
  , HasRY(..)
  , rect
  , HasCX(..)
  , HasCY(..)
  , HasR(..)
  , circle
  , HasX1(..)
  , HasY1(..)
  , HasX2(..)
  , HasY2(..)
  , line
  , HasPoints(..)
  , polyline
  , HasFill(..)
  , HasOpacity(..)
  , HasStroke(..)
  , HasStrokeWidth(..)
  ) where

import Helios.Data.String
import Helios.Text.XML.Types

(<|) :: Maybe a -> Maybe a -> Maybe a
Nothing <| y = y
x <| _ = x

-- | 1.2 SVG MIME type, file name extension and Macintosh file type

mimeType :: String
mimeType = "image/svg+xml"

fileNameExtension :: String
fileNameExtension = "svg"

-- | 1.3 SVG Namespace, Public Identifier and System Identifier

namespace :: String
namespace = "http://www.w3.org/2000/svg"

doctype :: DocType
doctype
  = DocType
    { rootElement
        = "svg"
    , docTypeKind
        = PUBLIC
          { publicIdentifier
              = "-//W3C//DTD SVG 1.1//EN"
          , systemIdentifier'
              = Just "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
          }
    }

-- | 4 Basic Data Types and Interfaces

-- | 4.2 Basic data types

-- * <angle>

data AngleUnit
  = Deg | Grad | Rad
  deriving (Eq, Ord)

instance Show AngleUnit where
  show Deg = "deg"
  show Grad = "grad"
  show Rad = "rad"

data Angle = Angle Number AngleUnit
  deriving (Eq, Ord)

instance Show Angle where
  show (Angle x u) = show x ++ show u

-- * <anything>

newtype Anything
  = Anything String
  deriving (Eq, Ord)

instance Show Anything where
  show (Anything x) = x

-- * <color>

data RGB
  = RGB Int Int Int
  deriving (Eq, Ord)

instance Show RGB where
  show (RGB r g b)
    = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Color
  = ColorKeyword ColorKeyword
  | ColorRGB RGB
  deriving (Eq, Ord)

instance Show Color where
  show (ColorKeyword x) = show x
  show (ColorRGB x) = show x

-- * <coordinate>

newtype Coordinate
  = Coordinate Length
  deriving (Eq, Ord)

instance Show Coordinate where
  show (Coordinate x) = show x

-- * <frequency>

data FrequencyUnit
  = Hz
  | KHz
  deriving (Eq, Ord)

instance Show FrequencyUnit where
  show Hz = "Hz"
  show KHz = "kHz"

data Frequency
  = Frequency Number FrequencyUnit
  deriving (Eq, Ord)

instance Show Frequency where
  show (Frequency x u) = show x ++ show u

-- * <FuncIRI>

newtype FuncIRI
  = FuncIRI IRI
  deriving (Eq, Ord)

instance Show FuncIRI where
  show (FuncIRI x) = show x

-- * <icccolor>

data ICCColor
  = ICCColor Name [Number]
  deriving (Eq, Ord)

instance Show ICCColor where
  show (ICCColor x cs) = "icccolor(" ++ show x ++ commas cs ++ ")"

-- * <IRI>

newtype IRI
  = IRI String
  deriving (Eq, Ord)

instance Show IRI where
  show (IRI x) = x

-- * <length>

data LengthUnit
  = CurrentUserCoordinateSystem
  | Em
  | Ex
  | Px
  | In
  | Cm
  | Mm
  | Pt
  | Pc
  | Percent
  deriving (Eq, Ord)

instance Show LengthUnit where
  show CurrentUserCoordinateSystem = ""
  show Em = "em"
  show Ex = "ex"
  show Px = "px"
  show In = "in"
  show Cm = "cm"
  show Mm = "mm"
  show Pt = "pt"
  show Pc = "pc"
  show Percent = "%"

data Length
  = Length Number LengthUnit
  deriving (Eq, Ord)

instance Show Length where
  show (Length x u) = show x ++ show u

cm :: Number -> Length
cm x = Length x Cm

-- * <list-of-family-names>

newtype FamilyName
  = FamilyName String
  deriving (Eq, Ord)

instance Show FamilyName where
  show (FamilyName x) = bracket1 "\"" x

data GenericFamily
  = Serif
  | SansSerif
  | Cursive
  | Fantasy
  | Monospace
  deriving (Eq, Ord)

instance Show GenericFamily where
  show Serif = "serif"
  show SansSerif = "sans-serif"
  show Cursive = "cursive"
  show Fantasy = "fantasy"
  show Monospace = "monospace"

-- * <list-of-Ts>

commas :: (Show a) => [a] -> String
commas = concatMap ((',':) . show)

-- * <name>

newtype Name
  = Name String
  deriving (Eq, Ord)

instance Show Name where
  show (Name x) = x

-- * <paint>

data Paint'
  = Paint'_None
  | Paint'_CurrentColor
  | Paint'_Color Color (Maybe ICCColor)
  deriving (Eq, Ord)

instance Show Paint' where
  show Paint'_None = "none"
  show Paint'_CurrentColor = "currentColor"
  show (Paint'_Color c m) = show c ++ maybe "" ((' ':) . show) m

data Paint
  = Paint_Paint' Paint'
  | Paint_FuncIRI FuncIRI (Maybe Paint')
  | Paint_Inherit
  deriving (Eq, Ord)

instance Show Paint where
  show (Paint_Paint' p) = show p
  show (Paint_FuncIRI f m) = show f ++ maybe "" ((' ':) . show) m
  show Paint_Inherit = "inherit"

-- * <percentage>

newtype Percentage
  = Percentage String
  deriving (Eq, Ord, Show)

-- * <time>

data TimeUnit
  = Ms
  | S
  deriving (Eq, Ord)

instance Show TimeUnit where
  show Ms = "ms"
  show S = "s"

data Time
  = Time Number TimeUnit
  deriving (Eq, Ord)

instance Show Time where
  show (Time x u) = show x ++ show u

-- * <transform-list>

data Transform
  = Matrix Double Double Double Double Double Double
  | Translate Double Double
  | Scale Double Double
  | Rotate Angle Double Double
  | SkewX Angle
  | SkewY Angle
  deriving (Eq, Ord)

instance Show Transform where
  show (Matrix a b c d e f)
    = function "matrix" (map show [a, b, c, d, e, f])

-----------------

data Inherit
  = Inherit
  deriving (Eq, Ord)

instance Show Inherit where
  show Inherit = "inherit"

-- | 4.3 Real number precision

type Number = Double

-- | 4.4 Recognized color keyword names

data ColorKeyword
  = Aliceblue
  | Antiquewhite
  | Aqua
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Black
  | Blanchedalmond
  | Blue
  | Blueviolet
  | Brown
  | Burlywood
  | Cadetblue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflowerblue
  | Cornsilk
  | Crimson
  | Cyan
  | Darkblue
  | Darkcyan
  | Darkgoldenrod
  | Darkgray
  | Darkgreen
  | Darkgrey
  | Darkkhaki
  | Darkmagenta
  | Darkolivegreen
  | Darkorange
  | Darkorchid
  | Darkred
  | Darksalmon
  | Darkseagreen
  | Darkslateblue
  | Darkslategray
  | Darkslategrey
  | Darkturquoise
  | Darkviolet
  | Deeppink
  | Deepskyblue
  | Dimgray
  | Dimgrey
  | Dodgerblue
  | Firebrick
  | Floralwhite
  | Forestgreen
  | Fuchsia
  | Gainsboro
  | Ghostwhite
  | Gold
  | Goldenrod
  | Gray
  | Grey
  | Green
  | Greenyellow
  | Honeydew
  | Hotpink
  | Indianred
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavenderblush
  | Lawngreen
  | Lemonchiffon
  | Lightblue
  | Lightcoral
  | Lightcyan
  | Lightgoldenrodyellow
  | Lightgray
  | Lightgreen
  | Lightgrey
  | Lightpink
  | Lightsalmon
  | Lightseagreen
  | Lightskyblue
  | Lightslategray
  | Lightslategrey
  | Lightsteelblue
  | Lightyellow
  | Lime
  | Limegreen
  | Linen
  | Magenta
  | Maroon
  | Mediumaquamarine
  | Mediumblue
  | Mediumorchid
  | Mediumpurple
  | Mediumseagreen
  | Mediumslateblue
  | Mediumspringgreen
  | Mediumturquoise
  | Mediumvioletred
  | Midnightblue
  | Mintcream
  | Mistyrose
  | Moccasin
  | Navajowhite
  | Navy
  | Oldlace
  | Olive
  | Olivedrab
  | Orange
  | Orangered
  | Orchid
  | Palegoldenrod
  | Palegreen
  | Paleturquoise
  | Palevioletred
  | Papayawhip
  | Peachpuff
  | Peru
  | Pink
  | Plum
  | Powderblue
  | Purple
  | Red
  | Rosybrown
  | Royalblue
  | Saddlebrown
  | Salmon
  | Sandybrown
  | Seagreen
  | Seashell
  | Sienna
  | Silver
  | Skyblue
  | Slateblue
  | Slategray
  | Slategrey
  | Snow
  | Springgreen
  | Steelblue
  | Tan
  | Teal
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White
  | Whitesmoke
  | Yellow
  | Yellowgreen
  deriving (Eq, Ord)

instance Show ColorKeyword where
  show Aliceblue = "aliceblue"
  show Antiquewhite = "antiquewhite"
  show Aqua = "aqua"
  show Aquamarine = "aquamarine"
  show Azure = "azure"
  show Beige = "beige"
  show Bisque = "bisque"
  show Black = "black"
  show Blanchedalmond = "blanchedalmond"
  show Blue = "blue"
  show Blueviolet = "blueviolet"
  show Brown = "brown"
  show Burlywood = "burlywood"
  show Cadetblue = "cadetblue"
  show Chartreuse = "chartreuse"
  show Chocolate = "chocolate"
  show Coral = "coral"
  show Cornflowerblue = "cornflowerblue"
  show Cornsilk = "cornsilk"
  show Crimson = "crimson"
  show Cyan = "cyan"
  show Darkblue = "darkblue"
  show Darkcyan = "darkcyan"
  show Darkgoldenrod = "darkgoldenrod"
  show Darkgray = "darkgray"
  show Darkgreen = "darkgreen"
  show Darkgrey = "darkgrey"
  show Darkkhaki = "darkkhaki"
  show Darkmagenta = "darkmagenta"
  show Darkolivegreen = "darkolivegreen"
  show Darkorange = "darkorange"
  show Darkorchid = "darkorchid"
  show Darkred = "darkred"
  show Darksalmon = "darksalmon"
  show Darkseagreen = "darkseagreen"
  show Darkslateblue = "darkslateblue"
  show Darkslategray = "darkslategray"
  show Darkslategrey = "darkslategrey"
  show Darkturquoise = "darkturquoise"
  show Darkviolet = "darkviolet"
  show Deeppink = "deeppink"
  show Deepskyblue = "deepskyblue"
  show Dimgray = "dimgray"
  show Dimgrey = "dimgrey"
  show Dodgerblue = "dodgerblue"
  show Firebrick = "firebrick"
  show Floralwhite = "floralwhite"
  show Forestgreen = "forestgreen"
  show Fuchsia = "fuchsia"
  show Gainsboro = "gainsboro"
  show Ghostwhite = "ghostwhite"
  show Gold = "gold"
  show Goldenrod = "goldenrod"
  show Gray = "gray"
  show Grey = "grey"
  show Green = "green"
  show Greenyellow = "greenyellow"
  show Honeydew = "honeydew"
  show Hotpink = "hotpink"
  show Indianred = "indianred"
  show Indigo = "indigo"
  show Ivory = "ivory"
  show Khaki = "khaki"
  show Lavender = "lavender"
  show Lavenderblush = "lavenderblush"
  show Lawngreen = "lawngreen"
  show Lemonchiffon = "lemonchiffon"
  show Lightblue = "lightblue"
  show Lightcoral = "lightcoral"
  show Lightcyan = "lightcyan"
  show Lightgoldenrodyellow = "lightgoldenrodyellow"
  show Lightgray = "lightgray"
  show Lightgreen = "lightgreen"
  show Lightgrey = "lightgrey"
  show Lightpink = "lightpink"
  show Lightsalmon = "lightsalmon"
  show Lightseagreen = "lightseagreen"
  show Lightskyblue = "lightskyblue"
  show Lightslategray = "lightslategray"
  show Lightslategrey = "lightslategrey"
  show Lightsteelblue = "lightsteelblue"
  show Lightyellow = "lightyellow"
  show Lime = "lime"
  show Limegreen = "limegreen"
  show Linen = "linen"
  show Magenta = "magenta"
  show Maroon = "maroon"
  show Mediumaquamarine = "mediumaquamarine"
  show Mediumblue = "mediumblue"
  show Mediumorchid = "mediumorchid"
  show Mediumpurple = "mediumpurple"
  show Mediumseagreen = "mediumseagreen"
  show Mediumslateblue = "mediumslateblue"
  show Mediumspringgreen = "mediumspringgreen"
  show Mediumturquoise = "mediumturquoise"
  show Mediumvioletred = "mediumvioletred"
  show Midnightblue = "midnightblue"
  show Mintcream = "mintcream"
  show Mistyrose = "mistyrose"
  show Moccasin = "moccasin"
  show Navajowhite = "navajowhite"
  show Navy = "navy"
  show Oldlace = "oldlace"
  show Olive = "olive"
  show Olivedrab = "olivedrab"
  show Orange = "orange"
  show Orangered = "orangered"
  show Orchid = "orchid"
  show Palegoldenrod = "palegoldenrod"
  show Palegreen = "palegreen"
  show Paleturquoise = "paleturquoise"
  show Palevioletred = "palevioletred"
  show Papayawhip = "papayawhip"
  show Peachpuff = "peachpuff"
  show Peru = "peru"
  show Pink = "pink"
  show Plum = "plum"
  show Powderblue = "powderblue"
  show Purple = "purple"
  show Red = "red"
  show Rosybrown = "rosybrown"
  show Royalblue = "royalblue"
  show Saddlebrown = "saddlebrown"
  show Salmon = "salmon"
  show Sandybrown = "sandybrown"
  show Seagreen = "seagreen"
  show Seashell = "seashell"
  show Sienna = "sienna"
  show Silver = "silver"
  show Skyblue = "skyblue"
  show Slateblue = "slateblue"
  show Slategray = "slategray"
  show Slategrey = "slategrey"
  show Snow = "snow"
  show Springgreen = "springgreen"
  show Steelblue = "steelblue"
  show Tan = "tan"
  show Teal = "teal"
  show Thistle = "thistle"
  show Tomato = "tomato"
  show Turquoise = "turquoise"
  show Violet = "violet"
  show Wheat = "wheat"
  show White = "white"
  show Whitesmoke = "whitesmoke"
  show Yellow = "yellow"
  show Yellowgreen = "yellowgreen"

colorKeywordToRGB :: ColorKeyword -> RGB
colorKeywordToRGB Aliceblue = RGB 240 248 255
colorKeywordToRGB Antiquewhite = RGB 250 235 215
colorKeywordToRGB Aqua = RGB  0 255 255
colorKeywordToRGB Aquamarine = RGB 127 255 212
colorKeywordToRGB Azure = RGB 240 255 255
colorKeywordToRGB Beige = RGB 245 245 220
colorKeywordToRGB Bisque = RGB 255 228 196
colorKeywordToRGB Black = RGB  0 0 0
colorKeywordToRGB Blanchedalmond = RGB 255 235 205
colorKeywordToRGB Blue = RGB  0 0 255
colorKeywordToRGB Blueviolet = RGB 138 43 226
colorKeywordToRGB Brown = RGB 165 42 42
colorKeywordToRGB Burlywood = RGB 222 184 135
colorKeywordToRGB Cadetblue = RGB  95 158 160
colorKeywordToRGB Chartreuse = RGB 127 255 0
colorKeywordToRGB Chocolate = RGB 210 105 30
colorKeywordToRGB Coral = RGB 255 127 80
colorKeywordToRGB Cornflowerblue = RGB 100 149 237
colorKeywordToRGB Cornsilk = RGB 255 248 220
colorKeywordToRGB Crimson = RGB 220 20 60
colorKeywordToRGB Cyan = RGB  0 255 255
colorKeywordToRGB Darkblue = RGB  0 0 139
colorKeywordToRGB Darkcyan = RGB  0 139 139
colorKeywordToRGB Darkgoldenrod = RGB 184 134 11
colorKeywordToRGB Darkgray = RGB 169 169 169
colorKeywordToRGB Darkgreen = RGB  0 100 0
colorKeywordToRGB Darkgrey = RGB 169 169 169
colorKeywordToRGB Darkkhaki = RGB 189 183 107
colorKeywordToRGB Darkmagenta = RGB 139 0 139
colorKeywordToRGB Darkolivegreen = RGB  85 107 47
colorKeywordToRGB Darkorange = RGB 255 140 0
colorKeywordToRGB Darkorchid = RGB 153 50 204
colorKeywordToRGB Darkred = RGB 139 0 0
colorKeywordToRGB Darksalmon = RGB 233 150 122
colorKeywordToRGB Darkseagreen = RGB 143 188 143
colorKeywordToRGB Darkslateblue = RGB  72 61 139
colorKeywordToRGB Darkslategray = RGB  47 79 79
colorKeywordToRGB Darkslategrey = RGB  47 79 79
colorKeywordToRGB Darkturquoise = RGB  0 206 209
colorKeywordToRGB Darkviolet = RGB 148 0 211
colorKeywordToRGB Deeppink = RGB 255 20 147
colorKeywordToRGB Deepskyblue = RGB  0 191 255
colorKeywordToRGB Dimgray = RGB 105 105 105
colorKeywordToRGB Dimgrey = RGB 105 105 105
colorKeywordToRGB Dodgerblue = RGB  30 144 255
colorKeywordToRGB Firebrick = RGB 178 34 34
colorKeywordToRGB Floralwhite = RGB 255 250 240
colorKeywordToRGB Forestgreen = RGB  34 139 34
colorKeywordToRGB Fuchsia = RGB 255 0 255
colorKeywordToRGB Gainsboro = RGB 220 220 220
colorKeywordToRGB Ghostwhite = RGB 248 248 255
colorKeywordToRGB Gold = RGB 255 215 0
colorKeywordToRGB Goldenrod = RGB 218 165 32
colorKeywordToRGB Gray = RGB 128 128 128
colorKeywordToRGB Grey = RGB 128 128 128
colorKeywordToRGB Green = RGB  0 128 0
colorKeywordToRGB Greenyellow = RGB 173 255 47
colorKeywordToRGB Honeydew = RGB 240 255 240
colorKeywordToRGB Hotpink = RGB 255 105 180
colorKeywordToRGB Indianred = RGB 205 92 92
colorKeywordToRGB Indigo = RGB  75 0 130
colorKeywordToRGB Ivory = RGB 255 255 240
colorKeywordToRGB Khaki = RGB 240 230 140
colorKeywordToRGB Lavender = RGB 230 230 250
colorKeywordToRGB Lavenderblush = RGB 255 240 245
colorKeywordToRGB Lawngreen = RGB 124 252 0
colorKeywordToRGB Lemonchiffon = RGB 255 250 205
colorKeywordToRGB Lightblue = RGB 173 216 230
colorKeywordToRGB Lightcoral = RGB 240 128 128
colorKeywordToRGB Lightcyan = RGB 224 255 255
colorKeywordToRGB Lightgoldenrodyellow = RGB 250 250 210
colorKeywordToRGB Lightgray = RGB 211 211 211
colorKeywordToRGB Lightgreen = RGB 144 238 144
colorKeywordToRGB Lightgrey = RGB 211 211 211
colorKeywordToRGB Lightpink = RGB 255 182 193
colorKeywordToRGB Lightsalmon = RGB 255 160 122
colorKeywordToRGB Lightseagreen = RGB  32 178 170
colorKeywordToRGB Lightskyblue = RGB 135 206 250
colorKeywordToRGB Lightslategray = RGB 119 136 153
colorKeywordToRGB Lightslategrey = RGB 119 136 153
colorKeywordToRGB Lightsteelblue = RGB 176 196 222
colorKeywordToRGB Lightyellow = RGB 255 255 224
colorKeywordToRGB Lime = RGB  0 255 0
colorKeywordToRGB Limegreen = RGB  50 205 50
colorKeywordToRGB Linen = RGB 250 240 230
colorKeywordToRGB Magenta = RGB 255 0 255
colorKeywordToRGB Maroon = RGB 128 0 0
colorKeywordToRGB Mediumaquamarine = RGB 102 205 170
colorKeywordToRGB Mediumblue = RGB  0 0 205
colorKeywordToRGB Mediumorchid = RGB 186 85 211
colorKeywordToRGB Mediumpurple = RGB 147 112 219
colorKeywordToRGB Mediumseagreen = RGB  60 179 113
colorKeywordToRGB Mediumslateblue = RGB 123 104 238
colorKeywordToRGB Mediumspringgreen = RGB  0 250 154
colorKeywordToRGB Mediumturquoise = RGB  72 209 204
colorKeywordToRGB Mediumvioletred = RGB 199 21 133
colorKeywordToRGB Midnightblue = RGB  25 25 112
colorKeywordToRGB Mintcream = RGB 245 255 250
colorKeywordToRGB Mistyrose = RGB 255 228 225
colorKeywordToRGB Moccasin = RGB 255 228 181
colorKeywordToRGB Navajowhite = RGB 255 222 173
colorKeywordToRGB Navy = RGB  0 0 128
colorKeywordToRGB Oldlace = RGB 253 245 230
colorKeywordToRGB Olive = RGB 128 128 0
colorKeywordToRGB Olivedrab = RGB 107 142 35
colorKeywordToRGB Orange = RGB 255 165 0
colorKeywordToRGB Orangered = RGB 255 69 0
colorKeywordToRGB Orchid = RGB 218 112 214
colorKeywordToRGB Palegoldenrod = RGB 238 232 170
colorKeywordToRGB Palegreen = RGB 152 251 152
colorKeywordToRGB Paleturquoise = RGB 175 238 238
colorKeywordToRGB Palevioletred = RGB 219 112 147
colorKeywordToRGB Papayawhip = RGB 255 239 213
colorKeywordToRGB Peachpuff = RGB 255 218 185
colorKeywordToRGB Peru = RGB 205 133 63
colorKeywordToRGB Pink = RGB 255 192 203
colorKeywordToRGB Plum = RGB 221 160 221
colorKeywordToRGB Powderblue = RGB 176 224 230
colorKeywordToRGB Purple = RGB 128 0 128
colorKeywordToRGB Red = RGB 255 0 0
colorKeywordToRGB Rosybrown = RGB 188 143 143
colorKeywordToRGB Royalblue = RGB  65 105 225
colorKeywordToRGB Saddlebrown = RGB 139 69 19
colorKeywordToRGB Salmon = RGB 250 128 114
colorKeywordToRGB Sandybrown = RGB 244 164 96
colorKeywordToRGB Seagreen = RGB  46 139 87
colorKeywordToRGB Seashell = RGB 255 245 238
colorKeywordToRGB Sienna = RGB 160 82 45
colorKeywordToRGB Silver = RGB 192 192 192
colorKeywordToRGB Skyblue = RGB 135 206 235
colorKeywordToRGB Slateblue = RGB 106 90 205
colorKeywordToRGB Slategray = RGB 112 128 144
colorKeywordToRGB Slategrey = RGB 112 128 144
colorKeywordToRGB Snow = RGB 255 250 250
colorKeywordToRGB Springgreen = RGB  0 255 127
colorKeywordToRGB Steelblue = RGB  70 130 180
colorKeywordToRGB Tan = RGB 210 180 140
colorKeywordToRGB Teal = RGB  0 128 128
colorKeywordToRGB Thistle = RGB 216 191 216
colorKeywordToRGB Tomato = RGB 255 99 71
colorKeywordToRGB Turquoise = RGB  64 224 208
colorKeywordToRGB Violet = RGB 238 130 238
colorKeywordToRGB Wheat = RGB 245 222 179
colorKeywordToRGB White = RGB 255 255 255
colorKeywordToRGB Whitesmoke = RGB 245 245 245
colorKeywordToRGB Yellow = RGB 255 255 0
colorKeywordToRGB Yellowgreen = RGB 154 205 50

-- | 5 Document Structure

newtype SVG
  = SVG { xml :: XML }
  deriving (Eq, Ord, Show)

deriving instance Semigroup SVG
deriving instance Monoid SVG

instance ToXML SVG where
  toXML svg =
    xml10 False
      <>
    toXML doctype
      <>
    xml svg

-- | 5.1.2 The ‘svg’ element

data Attr_SVG
  = Attr_SVG
    { attr_SVG_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_SVG_Core :: Attr_Core
    , attr_SVG_DocumentEvent :: Attr_DocumentEvent
    , attr_SVG_GraphicalEvent :: Attr_GraphicalEvent
    , attr_SVG_Presentation :: Attr_Presentation
    , attr_SVG_version :: Maybe String
    , attr_SVG_baseProfile :: Maybe String
    , attr_SVG_x :: Maybe String
    , attr_SVG_y :: Maybe String
    , attr_SVG_width :: Maybe String
    , attr_SVG_height :: Maybe String
    , attr_SVG_preserveAspectRatio :: Maybe String
    , attr_SVG_contentScriptType :: Maybe String
    , attr_SVG_contentStyleType :: Maybe String
    , attr_SVG_zoomAndPan :: Maybe String
    , attr_SVG_externalResourcesRequired :: Maybe String
    , attr_SVG_class :: Maybe String
    , attr_SVG_style :: Maybe String
    , attr_SVG_viewBox :: Maybe ViewBox
    }
  deriving (Eq, Ord, Show)

instance Semigroup Attr_SVG where
  a1 <> a2
    = Attr_SVG
      { attr_SVG_ConditionalProcessing = attr_SVG_ConditionalProcessing a1 <> attr_SVG_ConditionalProcessing a2
      , attr_SVG_Core = attr_SVG_Core a1 <> attr_SVG_Core a2
      , attr_SVG_DocumentEvent = attr_SVG_DocumentEvent a1 <> attr_SVG_DocumentEvent a2
      , attr_SVG_GraphicalEvent = attr_SVG_GraphicalEvent a1 <> attr_SVG_GraphicalEvent a2
      , attr_SVG_Presentation = attr_SVG_Presentation a1 <> attr_SVG_Presentation a2
      , attr_SVG_version = attr_SVG_version a1 <> attr_SVG_version a2
      , attr_SVG_baseProfile = attr_SVG_baseProfile a1 <> attr_SVG_baseProfile a2
      , attr_SVG_x = attr_SVG_x a1 <> attr_SVG_x a2
      , attr_SVG_y = attr_SVG_y a1 <> attr_SVG_y a2
      , attr_SVG_width = attr_SVG_width a1 <> attr_SVG_width a2
      , attr_SVG_height = attr_SVG_height a1 <> attr_SVG_height a2
      , attr_SVG_preserveAspectRatio = attr_SVG_preserveAspectRatio a1 <> attr_SVG_preserveAspectRatio a2
      , attr_SVG_contentScriptType = attr_SVG_contentScriptType a1 <> attr_SVG_contentScriptType a2
      , attr_SVG_contentStyleType = attr_SVG_contentStyleType a1 <> attr_SVG_contentStyleType a2
      , attr_SVG_zoomAndPan = attr_SVG_zoomAndPan a1 <> attr_SVG_zoomAndPan a2
      , attr_SVG_externalResourcesRequired = attr_SVG_externalResourcesRequired a1 <> attr_SVG_externalResourcesRequired a2
      , attr_SVG_class = attr_SVG_class a1 <> attr_SVG_class a2
      , attr_SVG_style = attr_SVG_style a1 <> attr_SVG_style a2
      , attr_SVG_viewBox = attr_SVG_viewBox a1 <| attr_SVG_viewBox a2
      }

instance Monoid Attr_SVG where
  mempty
    = Attr_SVG
      { attr_SVG_ConditionalProcessing = mempty
      , attr_SVG_Core = mempty
      , attr_SVG_DocumentEvent = mempty
      , attr_SVG_GraphicalEvent = mempty
      , attr_SVG_Presentation = mempty
      , attr_SVG_version = mempty
      , attr_SVG_baseProfile = mempty
      , attr_SVG_x = mempty
      , attr_SVG_y = mempty
      , attr_SVG_width = mempty
      , attr_SVG_height = mempty
      , attr_SVG_preserveAspectRatio = mempty
      , attr_SVG_contentScriptType = mempty
      , attr_SVG_contentStyleType = mempty
      , attr_SVG_zoomAndPan = mempty
      , attr_SVG_externalResourcesRequired = mempty
      , attr_SVG_class = mempty
      , attr_SVG_style = mempty
      , attr_SVG_viewBox = Nothing
      }

instance ToAttr Attr_SVG where
  toAttr Attr_SVG{..}
    = concat
      [ toAttr attr_SVG_ConditionalProcessing
      , toAttr attr_SVG_Core
      , toAttr attr_SVG_DocumentEvent
      , toAttr attr_SVG_GraphicalEvent
      , toAttr attr_SVG_Presentation
      , maybeAttr "version" attr_SVG_version
      , maybeAttr "baseProfile" attr_SVG_baseProfile
      , maybeAttr "x" attr_SVG_x
      , maybeAttr "y" attr_SVG_y
      , maybeAttr "width" attr_SVG_width
      , maybeAttr "height" attr_SVG_height
      , maybeAttr "preserveAspectRatio" attr_SVG_preserveAspectRatio
      , maybeAttr "contentScriptType" attr_SVG_contentScriptType
      , maybeAttr "contentStyleType" attr_SVG_contentStyleType
      , maybeAttr "zoomAndPan" attr_SVG_zoomAndPan
      , maybeAttr "externalResourcesRequired" attr_SVG_externalResourcesRequired
      , maybeAttr "class" attr_SVG_class
      , maybeAttr "style" attr_SVG_style
      , maybeAttr "viewBox" (show <$> attr_SVG_viewBox)
      ]

maybeAttr :: String -> Maybe String -> [Attr]
maybeAttr _ Nothing = []
maybeAttr x (Just y) = [x := y]

class HasVersion a where
  version :: String -> a

instance HasVersion Attr_SVG where
  version x = mempty { attr_SVG_version = Just x }

class HasBaseProfile a where
  baseProfile :: String -> a

instance HasBaseProfile Attr_SVG where
  baseProfile x = mempty { attr_SVG_baseProfile = Just x }

class HasX a where
  x :: String -> a

instance HasX Attr_SVG where
  x x' = mempty { attr_SVG_x = Just x' }

class HasY a where
  y :: String -> a

instance HasY Attr_SVG where
  y x = mempty { attr_SVG_y = Just x }

class HasWidth a where
  width :: String -> a

instance HasWidth Attr_SVG where
  width x = mempty { attr_SVG_width = Just x }

class HasHeight a where
  height :: String -> a

instance HasHeight Attr_SVG where
  height x = mempty { attr_SVG_height = Just x }

class HasViewBox a where
  viewBox :: Double -> Double -> Double -> Double -> a

instance HasViewBox Attr_SVG where
  viewBox minX minY width height
    = mempty { attr_SVG_viewBox = Just (ViewBox minX minY width height) }

svg :: Attr_SVG -> SVG -> SVG
svg attr content
  = SVG (ptag "svg" ("xmlns" := namespace : toAttr attr) (xml content))

-- | 5.2.2 The ‘g’ element

data Attr_G
  = Attr_G
    { attr_G_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_G_Core :: Attr_Core
    , attr_G_GraphicalEvent :: Attr_GraphicalEvent
    , attr_G_Presentation :: Attr_Presentation
    , attr_G_externalResourcesRequired :: Maybe String
    , attr_G_class :: Maybe String
    , attr_G_style :: Maybe String
    , attr_G_transform :: Maybe String
    }
  deriving (Eq, Ord, Show)

instance InheritsPresentation Attr_G where
  injPresentation x = mempty { attr_G_Presentation = x }

instance Semigroup Attr_G where
  a1 <> a2
    = Attr_G
      { attr_G_ConditionalProcessing = attr_G_ConditionalProcessing a1 <> attr_G_ConditionalProcessing a2
      , attr_G_Core = attr_G_Core a1 <> attr_G_Core a2
      , attr_G_GraphicalEvent = attr_G_GraphicalEvent a1 <> attr_G_GraphicalEvent a2
      , attr_G_Presentation = attr_G_Presentation a1 <> attr_G_Presentation a2
      , attr_G_externalResourcesRequired = attr_G_externalResourcesRequired a1 <> attr_G_externalResourcesRequired a2
      , attr_G_class = attr_G_class a1 <> attr_G_class a2
      , attr_G_style = attr_G_style a1 <> attr_G_style a2
      , attr_G_transform = attr_G_transform a1 <> attr_G_transform a2
      }

instance Monoid Attr_G where
  mempty
    = Attr_G
      { attr_G_ConditionalProcessing = mempty
      , attr_G_Core = mempty
      , attr_G_GraphicalEvent = mempty
      , attr_G_Presentation = mempty
      , attr_G_externalResourcesRequired = mempty
      , attr_G_class = mempty
      , attr_G_style = mempty
      , attr_G_transform = mempty
      }

instance ToAttr Attr_G where
  toAttr Attr_G{..}
    = concat
      [ toAttr attr_G_ConditionalProcessing
      , toAttr attr_G_Core
      , toAttr attr_G_GraphicalEvent
      , toAttr attr_G_Presentation
      , maybeAttr "externalResourcesRequired" attr_G_externalResourcesRequired
      , maybeAttr "class" attr_G_class
      , maybeAttr "style" attr_G_style
      , maybeAttr "transform" attr_G_transform
      ]

g :: Attr_G -> SVG -> SVG
g attr content
  = SVG (ptag "g" (toAttr attr) (xml content))

-- | 5.8 Conditional processing

data Attr_ConditionalProcessing
  = Attr_ConditionalProcessing
  deriving (Eq, Ord, Show)

instance Semigroup Attr_ConditionalProcessing where
  Attr_ConditionalProcessing <> Attr_ConditionalProcessing
    = Attr_ConditionalProcessing

instance Monoid Attr_ConditionalProcessing where
  mempty = Attr_ConditionalProcessing

instance ToAttr Attr_ConditionalProcessing where
  toAttr _ = []

-- | 5.9 Specifying whether external resources are required for proper rendering

-- | 5.10 Common attributes

data Attr_Core
  = Attr_Core
  deriving (Eq, Ord, Show)

instance Semigroup Attr_Core where
  Attr_Core <> Attr_Core
    = Attr_Core

instance Monoid Attr_Core where
  mempty = Attr_Core

instance ToAttr Attr_Core where
  toAttr _ = []

-- | 6 Styling

data Attr_Presentation
  = Attr_Presentation
    { attr_Presentation_fill :: Maybe String
    , attr_Presentation_opacity :: Maybe String
    , attr_Presentation_stroke :: Maybe String
    , attr_Presentation_strokeWidth :: Maybe PercentageLengthInherit
    }
  deriving (Eq, Ord, Show)

instance Semigroup Attr_Presentation where
  a1 <> a2
    = Attr_Presentation
      { attr_Presentation_fill = attr_Presentation_fill a1 <> attr_Presentation_fill a2
      , attr_Presentation_opacity = attr_Presentation_opacity a1 <> attr_Presentation_opacity a2
      , attr_Presentation_stroke = attr_Presentation_stroke a1 <> attr_Presentation_stroke a2
      , attr_Presentation_strokeWidth = attr_Presentation_strokeWidth a1 <| attr_Presentation_strokeWidth a2
      }

instance Monoid Attr_Presentation where
  mempty
    = Attr_Presentation
      { attr_Presentation_fill = mempty
      , attr_Presentation_opacity = mempty
      , attr_Presentation_stroke = mempty
      , attr_Presentation_strokeWidth = Nothing
      }

instance ToAttr Attr_Presentation where
  toAttr Attr_Presentation{..}
    = concat
      [ maybeAttr "fill" attr_Presentation_fill
      , maybeAttr "opacity" attr_Presentation_opacity
      , maybeAttr "stroke" attr_Presentation_stroke
      , maybeAttr "stroke-width" (show <$> attr_Presentation_strokeWidth)
      ]

class InheritsPresentation a where
  injPresentation :: Attr_Presentation -> a

class HasFill a where
  fill :: String -> a

instance InheritsPresentation a => HasFill a where
  fill x = injPresentation (mempty { attr_Presentation_fill = Just x })

class HasOpacity a where
  opacity :: String -> a

instance InheritsPresentation a => HasOpacity a where
  opacity x = injPresentation (mempty { attr_Presentation_opacity = Just x })

class HasStroke a where
  stroke :: String -> a

instance InheritsPresentation a => HasStroke a where
  stroke x = injPresentation (mempty { attr_Presentation_stroke = Just x })

class HasStrokeWidth b a where
  strokeWidth :: b -> a

instance (ToPercentageLengthInherit v, InheritsPresentation a) => HasStrokeWidth v a where
  strokeWidth x
    = injPresentation
      ( mempty
        { attr_Presentation_strokeWidth = Just (toPercentageLengthInherit x)
        }
      )

-- | 6.11 The ‘class’ attribute

-- | 6.12 The ‘style’ attribute

-- | 7.6 The ‘transform’ attribute

-- | 7.7 The ‘viewBox’ attribute

data ViewBox
  = ViewBox Double Double Double Double
  deriving (Eq, Ord)

instance Show ViewBox where
  show (ViewBox minX minY width height)
    = concat (intersperse " " (map show [minX, minY, width, height]))

-- | 9 Basic Shapes

-- | 9.2 The ‘rect’ element

data Attr_Rect
  = Attr_Rect
    { attr_Rect_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_Rect_Core :: Attr_Core
    , attr_Rect_GraphicalEvent :: Attr_GraphicalEvent
    , attr_Rect_Presentation :: Attr_Presentation
    , attr_Rect_x :: Maybe String
    , attr_Rect_y :: Maybe String
    , attr_Rect_width :: Maybe String
    , attr_Rect_height :: Maybe String
    , attr_Rect_rx :: Maybe String
    , attr_Rect_ry :: Maybe String
    , attr_Rect_externalResourcesRequired :: Maybe String
    , attr_Rect_class :: Maybe String
    , attr_Rect_style :: Maybe String
    , attr_Rect_transform :: Maybe String
    }
  deriving (Eq, Ord, Show)

instance InheritsPresentation Attr_Rect where
  injPresentation x = mempty { attr_Rect_Presentation = x }

instance Semigroup Attr_Rect where
  a1 <> a2
    = Attr_Rect
      { attr_Rect_ConditionalProcessing = attr_Rect_ConditionalProcessing a1 <> attr_Rect_ConditionalProcessing a2
      , attr_Rect_Core = attr_Rect_Core a1 <> attr_Rect_Core a2
      , attr_Rect_GraphicalEvent = attr_Rect_GraphicalEvent a1 <> attr_Rect_GraphicalEvent a2
      , attr_Rect_Presentation = attr_Rect_Presentation a1 <> attr_Rect_Presentation a2
      , attr_Rect_x = attr_Rect_x a1 <> attr_Rect_x a2
      , attr_Rect_y = attr_Rect_y a1 <> attr_Rect_y a2
      , attr_Rect_width = attr_Rect_width a1 <> attr_Rect_width a2
      , attr_Rect_height = attr_Rect_height a1 <> attr_Rect_height a2
      , attr_Rect_rx = attr_Rect_rx a1 <> attr_Rect_rx a2
      , attr_Rect_ry = attr_Rect_ry a1 <> attr_Rect_ry a2
      , attr_Rect_externalResourcesRequired = attr_Rect_externalResourcesRequired a1 <> attr_Rect_externalResourcesRequired a2
      , attr_Rect_class = attr_Rect_class a1 <> attr_Rect_class a2
      , attr_Rect_style = attr_Rect_style a1 <> attr_Rect_style a2
      , attr_Rect_transform = attr_Rect_transform a1 <> attr_Rect_transform a2
      }

instance Monoid Attr_Rect where
  mempty
    = Attr_Rect
      { attr_Rect_ConditionalProcessing = mempty
      , attr_Rect_Core = mempty
      , attr_Rect_GraphicalEvent = mempty
      , attr_Rect_Presentation = mempty
      , attr_Rect_x = mempty
      , attr_Rect_y = mempty
      , attr_Rect_width = mempty
      , attr_Rect_height = mempty
      , attr_Rect_rx = mempty
      , attr_Rect_ry = mempty
      , attr_Rect_externalResourcesRequired = mempty
      , attr_Rect_class = mempty
      , attr_Rect_style = mempty
      , attr_Rect_transform = mempty
      }

instance ToAttr Attr_Rect where
  toAttr Attr_Rect{..}
    = concat
      [ toAttr attr_Rect_ConditionalProcessing
      , toAttr attr_Rect_Core
      , toAttr attr_Rect_GraphicalEvent
      , toAttr attr_Rect_Presentation
      , maybeAttr "x" attr_Rect_x
      , maybeAttr "y" attr_Rect_y
      , maybeAttr "width" attr_Rect_width
      , maybeAttr "height" attr_Rect_height
      , maybeAttr "rx" attr_Rect_rx
      , maybeAttr "ry" attr_Rect_ry
      , maybeAttr "externalResourcesRequired" attr_Rect_externalResourcesRequired
      , maybeAttr "class" attr_Rect_class
      , maybeAttr "style" attr_Rect_style
      , maybeAttr "transform" attr_Rect_transform
      ]

instance HasX Attr_Rect where
  x x' = mempty { attr_Rect_x = Just x' }

instance HasY Attr_Rect where
  y x = mempty { attr_Rect_y = Just x }

instance HasWidth Attr_Rect where
  width x = mempty { attr_Rect_width = Just x }

instance HasHeight Attr_Rect where
  height x = mempty { attr_Rect_height = Just x }

class HasRX a where
  rx :: String -> a

instance HasRX Attr_Rect where
  rx x = mempty { attr_Rect_rx = Just x }

class HasRY a where
  ry :: String -> a

instance HasRY Attr_Rect where
  ry x = mempty { attr_Rect_ry = Just x }

rect :: Attr_Rect -> SVG -> SVG
rect attr content
  = SVG (ptag "rect" (toAttr attr) (xml content))

-- | 9.3 The ‘circle’ element

data Attr_Circle
  = Attr_Circle
    { attr_Circle_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_Circle_Core :: Attr_Core
    , attr_Circle_GraphicalEvent :: Attr_GraphicalEvent
    , attr_Circle_Presentation :: Attr_Presentation
    , attr_Circle_cx :: Maybe String
    , attr_Circle_cy :: Maybe String
    , attr_Circle_r :: Maybe String
    , attr_Circle_externalResourcesRequired :: Maybe String
    , attr_Circle_class :: Maybe String
    , attr_Circle_style :: Maybe String
    , attr_Circle_transform :: Maybe String
    }
  deriving (Eq, Ord, Show)

instance InheritsPresentation Attr_Circle where
  injPresentation x = mempty { attr_Circle_Presentation = x }

instance Semigroup Attr_Circle where
  a1 <> a2
    = Attr_Circle
      { attr_Circle_ConditionalProcessing = attr_Circle_ConditionalProcessing a1 <> attr_Circle_ConditionalProcessing a2
      , attr_Circle_Core = attr_Circle_Core a1 <> attr_Circle_Core a2
      , attr_Circle_GraphicalEvent = attr_Circle_GraphicalEvent a1 <> attr_Circle_GraphicalEvent a2
      , attr_Circle_Presentation = attr_Circle_Presentation a1 <> attr_Circle_Presentation a2
      , attr_Circle_cx = attr_Circle_cx a1 <> attr_Circle_cx a2
      , attr_Circle_cy = attr_Circle_cy a1 <> attr_Circle_cy a2
      , attr_Circle_r = attr_Circle_r a1 <> attr_Circle_r a2
      , attr_Circle_externalResourcesRequired = attr_Circle_externalResourcesRequired a1 <> attr_Circle_externalResourcesRequired a2
      , attr_Circle_class = attr_Circle_class a1 <> attr_Circle_class a2
      , attr_Circle_style = attr_Circle_style a1 <> attr_Circle_style a2
      , attr_Circle_transform = attr_Circle_transform a1 <> attr_Circle_transform a2
      }

instance Monoid Attr_Circle where
  mempty
    = Attr_Circle
      { attr_Circle_ConditionalProcessing = mempty
      , attr_Circle_Core = mempty
      , attr_Circle_GraphicalEvent = mempty
      , attr_Circle_Presentation = mempty
      , attr_Circle_cx = mempty
      , attr_Circle_cy = mempty
      , attr_Circle_r = mempty
      , attr_Circle_externalResourcesRequired = mempty
      , attr_Circle_class = mempty
      , attr_Circle_style = mempty
      , attr_Circle_transform = mempty
      }

instance ToAttr Attr_Circle where
  toAttr Attr_Circle{..}
    = concat
      [ toAttr attr_Circle_ConditionalProcessing
      , toAttr attr_Circle_Core
      , toAttr attr_Circle_GraphicalEvent
      , toAttr attr_Circle_Presentation
      , maybeAttr "cx" attr_Circle_cx
      , maybeAttr "cy" attr_Circle_cy
      , maybeAttr "r" attr_Circle_r
      , maybeAttr "externalResourcesRequired" attr_Circle_externalResourcesRequired
      , maybeAttr "class" attr_Circle_class
      , maybeAttr "style" attr_Circle_style
      , maybeAttr "transform" attr_Circle_transform
      ]

class HasCX a where
  cx :: String -> a

instance HasCX Attr_Circle where
  cx x = mempty { attr_Circle_cx = Just x }

class HasCY a where
  cy :: String -> a

instance HasCY Attr_Circle where
  cy x = mempty { attr_Circle_cy = Just x }

class HasR a where
  r :: String -> a

instance HasR Attr_Circle where
  r x = mempty { attr_Circle_r = Just x }

circle :: Attr_Circle -> SVG -> SVG
circle attr content
  = SVG (ptag "circle" (toAttr attr) (xml content))

-- | 9.5 The ‘line’ element

data Attr_Line
  = Attr_Line
    { attr_Line_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_Line_Core :: Attr_Core
    , attr_Line_GraphicalEvent :: Attr_GraphicalEvent
    , attr_Line_Presentation :: Attr_Presentation
    , attr_Line_x1 :: Maybe String
    , attr_Line_y1 :: Maybe String
    , attr_Line_x2 :: Maybe String
    , attr_Line_y2 :: Maybe String
    , attr_Line_externalResourcesRequired :: Maybe String
    , attr_Line_class :: Maybe String
    , attr_Line_style :: Maybe String
    , attr_Line_transform :: Maybe String
    }
  deriving (Eq, Ord, Show)

instance InheritsPresentation Attr_Line where
  injPresentation x = mempty { attr_Line_Presentation = x }

instance Semigroup Attr_Line where
  a1 <> a2
    = Attr_Line
      { attr_Line_ConditionalProcessing = attr_Line_ConditionalProcessing a1 <> attr_Line_ConditionalProcessing a2
      , attr_Line_Core = attr_Line_Core a1 <> attr_Line_Core a2
      , attr_Line_GraphicalEvent = attr_Line_GraphicalEvent a1 <> attr_Line_GraphicalEvent a2
      , attr_Line_Presentation = attr_Line_Presentation a1 <> attr_Line_Presentation a2
      , attr_Line_x1 = attr_Line_x1 a1 <> attr_Line_x1 a2
      , attr_Line_y1 = attr_Line_y1 a1 <> attr_Line_y1 a2
      , attr_Line_x2 = attr_Line_x2 a1 <> attr_Line_x2 a2
      , attr_Line_y2 = attr_Line_y2 a1 <> attr_Line_y2 a2
      , attr_Line_externalResourcesRequired = attr_Line_externalResourcesRequired a1 <> attr_Line_externalResourcesRequired a2
      , attr_Line_class = attr_Line_class a1 <> attr_Line_class a2
      , attr_Line_style = attr_Line_style a1 <> attr_Line_style a2
      , attr_Line_transform = attr_Line_transform a1 <> attr_Line_transform a2
      }

instance Monoid Attr_Line where
  mempty
    = Attr_Line
      { attr_Line_ConditionalProcessing = mempty
      , attr_Line_Core = mempty
      , attr_Line_GraphicalEvent = mempty
      , attr_Line_Presentation = mempty
      , attr_Line_x1 = mempty
      , attr_Line_y1 = mempty
      , attr_Line_x2 = mempty
      , attr_Line_y2 = mempty
      , attr_Line_externalResourcesRequired = mempty
      , attr_Line_class = mempty
      , attr_Line_style = mempty
      , attr_Line_transform = mempty
      }

instance ToAttr Attr_Line where
  toAttr Attr_Line{..}
    = concat
      [ toAttr attr_Line_ConditionalProcessing
      , toAttr attr_Line_Core
      , toAttr attr_Line_GraphicalEvent
      , toAttr attr_Line_Presentation
      , maybeAttr "x1" attr_Line_x1
      , maybeAttr "y1" attr_Line_y1
      , maybeAttr "x2" attr_Line_x2
      , maybeAttr "y2" attr_Line_y2
      , maybeAttr "externalResourcesRequired" attr_Line_externalResourcesRequired
      , maybeAttr "class" attr_Line_class
      , maybeAttr "style" attr_Line_style
      , maybeAttr "transform" attr_Line_transform
      ]

class HasX1 a where
  x1 :: String -> a

instance HasX1 Attr_Line where
  x1 x = mempty { attr_Line_x1 = Just x }

class HasY1 a where
  y1 :: String -> a

instance HasY1 Attr_Line where
  y1 x = mempty { attr_Line_y1 = Just x }

class HasX2 a where
  x2 :: String -> a

instance HasX2 Attr_Line where
  x2 x = mempty { attr_Line_x2 = Just x }

class HasY2 a where
  y2 :: String -> a

instance HasY2 Attr_Line where
  y2 x = mempty { attr_Line_y2 = Just x }

line :: Attr_Line -> SVG -> SVG
line attr content
  = SVG (ptag "line" (toAttr attr) (xml content))

-- | 9.6 The ‘polyline’ element

data Attr_Polyline
  = Attr_Polyline
    { attr_Polyline_ConditionalProcessing :: Attr_ConditionalProcessing
    , attr_Polyline_Core :: Attr_Core
    , attr_Polyline_GraphicalEvent :: Attr_GraphicalEvent
    , attr_Polyline_Presentation :: Attr_Presentation
    , attr_Polyline_points :: Maybe Points
    , attr_Polyline_externalResourcesRequired :: Maybe String
    , attr_Polyline_class :: Maybe String
    , attr_Polyline_style :: Maybe String
    , attr_Polyline_transform :: Maybe String
    }
  deriving (Eq, Ord, Show)

instance InheritsPresentation Attr_Polyline where
  injPresentation x = mempty { attr_Polyline_Presentation = x }

instance Semigroup Attr_Polyline where
  a1 <> a2
    = Attr_Polyline
      { attr_Polyline_ConditionalProcessing = attr_Polyline_ConditionalProcessing a1 <> attr_Polyline_ConditionalProcessing a2
      , attr_Polyline_Core = attr_Polyline_Core a1 <> attr_Polyline_Core a2
      , attr_Polyline_GraphicalEvent = attr_Polyline_GraphicalEvent a1 <> attr_Polyline_GraphicalEvent a2
      , attr_Polyline_Presentation = attr_Polyline_Presentation a1 <> attr_Polyline_Presentation a2
      , attr_Polyline_points = attr_Polyline_points a1 <> attr_Polyline_points a2
      , attr_Polyline_externalResourcesRequired = attr_Polyline_externalResourcesRequired a1 <> attr_Polyline_externalResourcesRequired a2
      , attr_Polyline_class = attr_Polyline_class a1 <> attr_Polyline_class a2
      , attr_Polyline_style = attr_Polyline_style a1 <> attr_Polyline_style a2
      , attr_Polyline_transform = attr_Polyline_transform a1 <> attr_Polyline_transform a2
      }

instance Monoid Attr_Polyline where
  mempty
    = Attr_Polyline
      { attr_Polyline_ConditionalProcessing = mempty
      , attr_Polyline_Core = mempty
      , attr_Polyline_GraphicalEvent = mempty
      , attr_Polyline_Presentation = mempty
      , attr_Polyline_points = mempty
      , attr_Polyline_externalResourcesRequired = mempty
      , attr_Polyline_class = mempty
      , attr_Polyline_style = mempty
      , attr_Polyline_transform = mempty
      }

instance ToAttr Attr_Polyline where
  toAttr Attr_Polyline{..}
    = concat
      [ toAttr attr_Polyline_ConditionalProcessing
      , toAttr attr_Polyline_Core
      , toAttr attr_Polyline_GraphicalEvent
      , toAttr attr_Polyline_Presentation
      , maybeAttr "points" (show <$> attr_Polyline_points)
      , maybeAttr "externalResourcesRequired" attr_Polyline_externalResourcesRequired
      , maybeAttr "class" attr_Polyline_class
      , maybeAttr "style" attr_Polyline_style
      , maybeAttr "transform" attr_Polyline_transform
      ]

class HasPoints a where
  points :: [(Double, Double)] -> a

instance HasPoints Attr_Polyline where
  points x = mempty { attr_Polyline_points = Just (Points (map Point x)) }

polyline :: Attr_Polyline -> SVG -> SVG
polyline attr content
  = SVG (ptag "polyline" (toAttr attr) (xml content))

-- | 9.7 The ‘polygon’ element

newtype Points
  = Points [Point]
  deriving (Eq, Ord, Semigroup)

instance Show Points where
  show (Points ps) = concat (intersperse " " (map show ps))

newtype Point
  = Point (Double, Double)
  deriving (Eq, Ord)

instance Show Point where
  show (Point (x,y)) = show x ++ " " ++ show y

-- | 11.4 Stroke Properties

data PercentageLengthInherit
  = PLI_Percentage Percentage
  | PLI_Length Length
  | PLI_Inherit Inherit
  deriving (Eq, Ord)

instance Show PercentageLengthInherit where
  show (PLI_Percentage x) = show x
  show (PLI_Length x) = show x
  show (PLI_Inherit x) = show x

class ToPercentageLengthInherit a where
  toPercentageLengthInherit :: a -> PercentageLengthInherit

instance ToPercentageLengthInherit Percentage where
  toPercentageLengthInherit = PLI_Percentage

instance ToPercentageLengthInherit Length where
  toPercentageLengthInherit = PLI_Length

instance ToPercentageLengthInherit Inherit where
  toPercentageLengthInherit = PLI_Inherit

-- | 18.4.3 Document-level event attributes

data Attr_DocumentEvent
  = Attr_DocumentEvent
  deriving (Eq, Ord, Show)

instance Semigroup Attr_DocumentEvent where
  Attr_DocumentEvent <> Attr_DocumentEvent
    = Attr_DocumentEvent

instance Monoid Attr_DocumentEvent where
  mempty = Attr_DocumentEvent

instance ToAttr Attr_DocumentEvent where
  toAttr _ = []

-- | 18.4.2 Event attributes on graphics and container elements

data Attr_GraphicalEvent
  = Attr_GraphicalEvent
  deriving (Eq, Ord, Show)

instance Semigroup Attr_GraphicalEvent where
  Attr_GraphicalEvent <> Attr_GraphicalEvent
    = Attr_GraphicalEvent

instance Monoid Attr_GraphicalEvent where
  mempty = Attr_GraphicalEvent

instance ToAttr Attr_GraphicalEvent where
  toAttr _ = []
