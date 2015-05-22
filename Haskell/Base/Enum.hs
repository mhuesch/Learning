import Data.Bits

data Color = White
           | Red
           | Green
           | Blue
           deriving (Eq, Ord, Show, Enum)

allColors :: [Color]
allColors = enumFrom White

mixColors :: Color -> Color -> Color
mixColors c1 c2 = toEnum (fromEnum c1 .|. fromEnum c2)
