import Graphics.Gloss

import Drawing (Shape)
--première possibilité
type Symb  = Char
type Mot      = [Symb]
type Ordre   = Word
type Rules   = Symb -> Word
type  = [Word]
type Config = (EtatTortue ,Float ,Float ,Float ,[Symb]) 
type EtatDessin = (EtatTortue, Path)


-- autre possibilité
type Location = (Int,Int)
type Turtle = (Location,Direction,Pen)
data Direction = Haut | Bas | Gauche | Droite
data CrayonMove = Up | Down deriving (Show)
data Commandes = Avance Int Int | Crayon CrayonMove | RotR | RotL | Repeat
type EtatTortue = (CrayonMove, Int, Int)
type Links = [Shape]

-- première piste de fonctions (abandonner)
etat :: Config -> EtatTortue
pas :: Config -> Float
avance :: Config -> EtatTortue -> EtatTortue
AGauche :: Config -> EtatTortue -> EtatTortue
ADroite :: Config -> EtatTortue -> EtatTortue

-- donner un ordre à la tortue
executer_ordre :: Commandes -> EtatTortue -> (EtatTortue,Links)
executer_ordre (Repeat) (_,_,_) = Error 
executer_ordre (Crayon Up) (_,b,c)= ((Up,b,c),[])
executer_ordre (Crayon Down)(_,b,c)= ((Down,b,c),[])
executer_ordre (Avance i j) (Up,_,_) = ((Up,i,j),[])
executer_ordre (Aance a,b) (Down, c,d) = ((Down, a, b), [(a, b, c, d)])


-- partie vonKoch avec la librairie de dessin Gloss
vonKoch :: LSysteme
vonKoch = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
         regles  s  = [s]
-- deuxième test
vonKochs :: LSysteme
vonKochs = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]
