import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Point     = (Float,Float)
type Circle    = (Int,Int,Int)
type Rectangle = (Int,Int,Int,Int)
type Triangle  = [Point] 
type Color     = (Int,Int,Int)
data Shape     =  Circle | Rectangle | Link | BrokenLink | Polygon deriving(Show)
data DrawingArea = Int Int [Shape]
type BrokenLink = [Point]
type Image     = (Int,Int,[Shape])
type Polygon   = [Point] -- carre / triangle
type Link      = [Point]


main = do 
  
  let writePoint :: Point -> String 
      writePoint (x,y) = (show x)++","++(show y)++" "

  let writeShape :: (Color,Polygon) -> String 
      writeShape ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"
  
  let writeShapes :: [(Color,Polygon)] -> String 
      writeShapes p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writeShape p)++"</svg>"
  
  let colorize :: Color -> [Polygon] -> [(Color,Polygon)] 
      colorize = zip.repeat

  let rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  writeFile "img.svg" $ writePolygons (purple [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]])