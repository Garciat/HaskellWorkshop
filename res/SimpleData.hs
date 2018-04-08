data Color
  = Red
  | Blue
  | Yellow


data AltColor
  = Dark Color
  | Light Color
  | Normal Color


colorName color =
  case color of
    Red -> "Red"
    Blue -> "Blue"
    Yellow -> "Yellow"

altColorName altColor =
  case altColor of
    Light Red  -> "Pink"
    Dark color -> "Dark " ++ colorName color
    Light color -> "Light " ++ colorName color
    Normal color -> colorName color


-- can also be written like:
colorName' Red = "Red"
colorName' Blue = "Blue"
colorName' Yellow = "Yellow"

altColorName' (Light Red) = "Pink"
altColorName' (Dark color) = "Dark " ++ colorName' color
altColorName' (Light color) = "Light " ++ colorName' color
altColorName' (Normal color) = colorName' color
