module Drawing where 

import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Color     = (Int,Int,Int) 
data Shape     =  Circle Int Int Int | Rectangle Int Int Int Int | Link Int Int Int Int | BrokenLink Int Int Int Int | Polygon [(Float,Float)]   deriving (Show)
type DrawingArea = (Int,Int)

--Print des Shapes, fonction de test
printShape :: Shape -> String 
printShape s = ""++(show s)++""

--verif des Shapes
isCircle :: Shape -> Bool
isCircle (Circle _ _ _) = False
isCircle (Circle r s t) = True

isRect :: Shape -> Bool
isRect (Rectangle _ _ _ _) = False
isRect (Rectangle r s t u) = True

isLink :: Shape -> Bool
isLink (Link _ _ _ _) = False
isLink (Link r s t u) = True

isBrokenLink :: Shape -> Bool
isBrokenLink (Circle _ _ _) = False
isBrokenLink (Circle r s t) = True

isPolygon :: Shape -> Bool
isPolygon (Polygon (h:t)) = True 
isPolygon (Polygon _) = False 

--fonction pour colorier nos shapes
colorize :: Color -> [Shape] -> [(Color,Shape)] 
colorize = zip.repeat

--possibilité de rajouté des couleurs
rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

writePoints :: (Float,Float) -> String 
writePoints (x,y)= (show x)++","++(show y)++" "

writeType :: (Color,Shape) -> String 
writeType ((r,g,b),(Circle x y z )) = "<circle cx=\""++(show x)++"\" cy=\""++(show y)++"\" r=\""++(show z)++"\"fill:rgb("++(show r)++","++(show g)++","++(show b)++")\"/>" 
writeType ((r,g,b),(Rectangle x y z w)) = "<rect x=\""++(show x)++"\" y=\""++(show y)++"\" width=\""++(show z)++"\" height=\""++(show w)++"\" fill:rgb("++(show r)++","++(show g)++","++(show b)++")\"/>" 
writeType ((r,g,b),(Link x y z w)) = "<line x1=\""++(show x)++"\" y1=\""++(show y)++"\" x2=\""++(show z)++"\" y2=\""++(show w)++"\" stroke:rgb("++(show r)++","++(show g)++","++(show b)++")\"/>" 
writeType ((r,g,b),(BrokenLink x y z w)) = "<line stroke-dasharray= '5,5' x1=\""++(show x)++"\" y1=\""++(show y)++"\" x2=\""++(show z)++"\" y2=\""++(show w)++"\" stroke:rgb("++(show r)++","++(show g)++","++(show b)++")\"/>" 
writeType ((r,g,b),(Polygon (h:t))) = "<polygon points=\""++(writePoints h)++"\" style=\"fill:rgb("++(show r)++","++(show g)++","++(show b)++");stroke:black;stroke-width:2\"/>"

write :: DrawingArea -> [(Color,Shape)] -> String
write (height,width) s = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""++(show width)++"\" height=\""++(show height)++"\">"++(concatMap writeType s)++"</svg>"


--randomPolygon :: [(Color,Shape)] -> String
--randomPolygon ((r,g,b),(Polygon (h:t)))= writeType ((r,g,b),(Polygon h:t))


main :: IO()
main = do 
 
--  print $ write (400,400) (blue[(BrokenLink 15 12 13 15)])
--  print $ printShape (Circle 14 15 20)
  
  writeFile "img.svg" write (400,400) (blue[(BrokenLink 15 12 13 15)])

