import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
type EtatDessin = (EtatTortue, Path)

etatInitial :: Config -> EtatTortue
longueurPas :: Config -> Float
facteurEchelle :: Config -> Float
angle :: Config -> Float
symbolesTortue :: Config -> [Symbole]
avance :: Config -> EtatTortue -> EtatTortue
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneADroite :: Config -> EtatTortue -> EtatTortue


vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]