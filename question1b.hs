import System.IO


data Shape = Rect Int Int Int Int | 
    Circle Int Int Int |
    Line [(Int,Int)] |
    HashLine [(Int,Int)] deriving (Show)

data Image = Image { 
    width :: Int,
    height :: Int,
    shapes :: [Shape]
    } deriving(Show)
 

begin = "<svg xmlns='http://www.w3.org/2000/svg' width='500' height='500' version='1.1'>"
end = "</svg>"
file = "image.svg"

getWidth :: Image -> Int 
getWidth (Image w _ _) = w 

getHeight :: Image -> Int
getHeight (Image _ h _) = h

getShapes :: Image -> Int
getShapes (Image _ _ (h:t)) = h 
  

export :: Image -> String
export (Image w h _) = "Veuillez enregistrer des formes à dessiner"
--export (w h [(l)]) = 
  -- let img = Image ()
  --readFile file 
  --writeFile file begin
  --appendFile file end