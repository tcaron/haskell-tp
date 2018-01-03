import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Color     = (Int,Int,Int) 
data Shape     =  Circle Int Int Int | Rectangle Int Int Int Int | Link Int Int Int Int | BrokenLink Int Int Int Int | Polygon [(Float,Float)]   deriving (Show)
type DrawingArea = (Int,Int,[(Color,Shape)])

--Print des Shapes, fonction de test
printShape :: Shape -> String 
printShape s = ""++(show s)++""

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

writeShape :: [(Color,Shape)] -> String
writeShape s = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writeType s)++"</svg>"

writeImg :: DrawingArea -> String
writeImg (w,h,(ah:t)) = writeFile "img.svg" "<svg xmlns=\"http://www.w3.org/2000/svg\">
writeImg (w,h,(ah:t))= "test"

randomPolygon :: [(Color,Shape)] -> String
randomPolygon s = "test"

main = do 
 
  print $ writeShape (blue[(BrokenLink 15 12 13 15)])
  print $ printShape (Circle 14 15 20)


--    let writePoint :: Point -> String 
--      writePoint (x y) = (show x)++","++(show y)++" "

--  let writeShape :: (Color,Polygon) -> String 
--     writeShape ((r,g,b),p)  = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:rgb("++(show r)++","++(show g)++","++(show b)++");stroke:black;stroke-width:2\"/>"
  
--  let writeShapes :: [(Color,Polygon)] -> String 
--     writeShapes p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writeShape p)++"</svg>"
--  
--writeFile "img.svg" $ writeShapes (purple [[(100 100) (200 100) (200 200) (100 200)] [(200 200) (300 200) (300 300) (200 300)]])