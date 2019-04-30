import Data.Char
import Data.Maybe
import Debug.Trace

-- TIPUS DE DADES I INSTANCES

-- Color

data Color = Blanc | Negre deriving (Eq, Show)

-- TipusPeça

data TipusPeca = Rei | Reina | Torre | Alfil | Cavall | Peo deriving Eq

-- Peça

data Peca = Pec TipusPeca Color deriving Eq

-- Retorna el tipus d'una peça passada per paràmetre.
tipusPeca :: Peca -> TipusPeca
tipusPeca (Pec t _) = t

-- Retorna el color d'una peça passada per paràmetre.
colorPeca :: Peca -> Color
colorPeca (Pec _ c) = c

-- Si el color passat per paràmetre és "Negre", es
-- retorna el caràcter passat per paràmetre a minúscules.
mostrarColor :: Color -> Char -> Char
mostrarColor x c = if (x == Negre) then toLower c else c

-- Retorna els caràcters a mostrar per pantalla d'acord
-- amb la peça passada per paràmetre.
mostraPeca :: Peca -> Char
mostraPeca (Pec tipus color)
   | (tipus == Rei) = mostrarColor color 'R'
   | (tipus == Reina) = mostrarColor color 'D'
   | (tipus == Torre) = mostrarColor color 'T'
   | (tipus == Alfil) = mostrarColor color 'A'
   | (tipus == Cavall) = mostrarColor color 'C'
   | otherwise = mostrarColor color 'P'

-- Tauler que representa el tauler inicial del joc d'escacs.
taulerInicial = Tau [(('a' :/ 1), (Pec Torre Blanc)), (('b' :/ 1), (Pec Cavall Blanc)), (('c' :/ 1), (Pec Alfil Blanc)), (('d' :/ 1), (Pec Reina Blanc)), (('e' :/ 1), (Pec Rei Blanc)), (('f' :/ 1), (Pec Alfil Blanc)), (('g' :/ 1), (Pec Cavall Blanc)), (('h' :/ 1), (Pec Torre Blanc)),
                      (('a' :/ 8), (Pec Torre Negre)), (('b' :/ 8), (Pec Cavall Negre)), (('c' :/ 8), (Pec Alfil Negre)), (('d' :/ 8), (Pec Reina Negre)), (('e' :/ 8), (Pec Rei Negre)), (('f' :/ 8), (Pec Alfil Negre)), (('g' :/ 8), (Pec Cavall Negre)), (('h' :/ 8), (Pec Torre Negre)),
                      (('a' :/ 2), (Pec Peo Blanc)), (('b' :/ 2), (Pec Peo Blanc)), (('c' :/ 2), (Pec Peo Blanc)), (('d' :/ 2), (Pec Peo Blanc)), (('e' :/ 2), (Pec Peo Blanc)), (('f' :/ 2), (Pec Peo Blanc)), (('g' :/ 2), (Pec Peo Blanc)), (('h' :/ 2), (Pec Peo Blanc)),
                      (('a' :/ 7), (Pec Peo Negre)), (('b' :/ 7), (Pec Peo Negre)), (('c' :/ 7), (Pec Peo Negre)), (('d' :/ 7), (Pec Peo Negre)), (('e' :/ 7), (Pec Peo Negre)), (('f' :/ 7), (Pec Peo Negre)), (('g' :/ 7), (Pec Peo Negre)), (('h' :/ 7), (Pec Peo Negre))]

-- Retorna una peça d'acord amb el caràcter passat
-- per paràmetre.
llegirPeca :: Char -> Color -> Peca
llegirPeca p color = do
    if (p == 'P') then (Pec Peo color)
    else if (p == 'T') then (Pec Torre color)
    else if (p == 'C') then (Pec Cavall color)
    else if (p == 'A') then (Pec Alfil color)
    else if (p == 'D') then (Pec Reina color)
    else (Pec Rei color)

-- Crea una posició a partir de dos enters passats
-- per paràmetre dins d'una tupla, dels quals el primer
-- element de la tupla representa la columna i el segon
-- representa la fila.
ferPos :: (Int, Int) -> Posicio
ferPos (x, y) = (chr x :/ y)

mostraTauler :: Tauler -> IO()
mostraTauler t = do
    let tauler = taulerToString t
    putStrLn("   ============")
    putStrLn("8- | " ++ take 8 (drop 56 tauler) ++ " |")
    putStrLn("7- | " ++ take 8 (drop 48 tauler) ++ " |")
    putStrLn("6- | " ++ take 8 (drop 40 tauler) ++ " |")
    putStrLn("5- | " ++ take 8 (drop 32 tauler) ++ " |")
    putStrLn("4- | " ++ take 8 (drop 24 tauler) ++ " |")
    putStrLn("3- | " ++ take 8 (drop 16 tauler) ++ " |")
    putStrLn("2- | " ++ take 8 (drop 8 tauler) ++ " |")
    putStrLn("1- | " ++ take 8 tauler ++ " |")
    putStrLn("   ============")
    putStrLn("     abcdefgh")

taulerToString :: Tauler -> String
taulerToString t = _m(trobarPeces t pos) where
        list = [(x,y) | y <- [1..8], x <- [ord 'a'.. ord 'h']] 
        pos = map ferPos list
        _m [] = []
        _m (x:xs)
            | x == Nothing = "." ++ _m xs
            | otherwise = [mostraPeca (fromJust x)] ++ _m xs
    
instance Show Peca where
    show (Pec tipus color)
        | (tipus == Rei) = show (mostrarColor color 'R')
        | (tipus == Reina) = show (mostrarColor color 'D')
        | (tipus == Torre) = show (mostrarColor color 'T')
        | (tipus == Alfil) = show (mostrarColor color 'A')
        | (tipus == Cavall) = show (mostrarColor color 'C')
        | otherwise = show (mostrarColor color 'P')

-- Posició

data Posicio = Char :/ Int deriving Eq

instance Show Posicio where
    show (fila :/ col) = show fila ++ show col

-- Jugada

data Jugada = Jug Peca Posicio Posicio deriving Eq

instance Show Jugada where
    show (Jug p x0 x1) = show p ++ show x0 ++ show x1

-- JugadaGenerica 

data JugadaGenerica = Jugada Peca Posicio Posicio | EnrocCurt Peca Posicio Posicio | EnrocLlarg Peca Posicio Posicio | Escac Peca Posicio Posicio | EscacMat Peca Posicio Posicio deriving (Eq, Show)

-- Casella

type Casella = (Posicio, Peca)

mostraCasella :: Casella -> Char
mostraCasella (_, p) = mostraPeca p

-- Tauler

data Tauler = Tau [(Posicio, Peca)]
instance Show Tauler where
    show t = do
        let tauler = taulerToString t
        "\n" ++ show "  ============" ++ ""++"\n"++"" ++ "8- | " ++ take 8 (drop 56 tauler) ++ " |"++""++"\n"++""++"7- | " ++ take 8 (drop 48 tauler) ++ " |"++""++"\n"++""++"6- | " ++ take 8 (drop 40 tauler) ++ " |"++"\n"++"5- | " ++ take 8 (drop 32 tauler) ++ " |"++"\n"++"4- | " ++ take 8 (drop 24 tauler) ++ " |"++"\n"++"3- | " ++ take 8 (drop 16 tauler) ++ " |"++"\n"++"2- | " ++ take 8 (drop 8 tauler) ++ " |"++"\n"++"1- | " ++ take 8 tauler ++ " |"++"\n"++"   ============"++"\n"++"     abcdefgh"

-- Partida

data Partida = Par Tauler Color


-- "VARIABLES" PER FER PROVES

rei :: Peca
rei = Pec Rei Blanc

reina :: Peca
reina = Pec Reina Negre

cavall :: Peca
cavall = Pec Cavall Negre

torre :: Peca
torre = Pec Torre Negre

alfil :: Peca
alfil = Pec Alfil Negre

peo :: Peca
peo = Pec Peo Blanc

posA :: Posicio
posA = 'g' :/ 5

posB :: Posicio
posB = 'c' :/ 3

posC :: Posicio
posC = 'a' :/ 3

casA :: Casella
casA = (posA, peo)

casB :: Casella
casB = (posB, torre)

unaJugada :: Jugada
unaJugada = Jug torre ('a':/3) ('z':/3)

-- MÈTODES

-- Retorna la peça ("Just Peça") del tauler passat per paràmetre
-- que es troba a la posició passada per paràmetre. Si la posició
-- no existeix retorna "Nothing".
trobarPeca :: Tauler -> Posicio -> Maybe Peca
trobarPeca (Tau t) p = if (null trobat) then Nothing else Just (snd (trobat !! 0))
    where
        esCasella (pc :/ pf) (p, _) = p == (pc :/ pf)
        trobat = (filter (esCasella p) t)
        peca (_, x) = x

-- Retorna un conjunt de peces que tenen la posició passada per
-- paràmetre en una llista, d'acord amb el tauler passat per
-- paràmetre. Si una posició de les passades a la llista no es
-- troba, s'afegirà un "Nothing" a la llista a retornar; en cas
-- que es trobi, s'afegirà un "Just Peca".
trobarPeces :: Tauler -> [Posicio] -> [Maybe Peca]
trobarPeces t [] = []
trobarPeces t (p : ps) = (trobarPeca t p) : (trobarPeces t ps)

-- Retorna la fila d'una posició passada per paràmetre.
fila :: Posicio -> Int
fila (_ :/ x) = x

-- Retorna la columna d'una posició passada per paràmetre.
columna :: Posicio -> Char
columna (x :/ _) = x

-- Retorna cert si la posició passada per paràmetre
-- és vàlida (no surt fora del rang del tauler); fals
-- altrament.
posicioValida :: Posicio -> Bool
posicioValida (c :/ f) = (c >= 'a' && c <= 'h' && f >= 1 && f <= 8)

-- Retorna el desplaçament d'una posició més adalt 
-- respecte la posició passada per paràmetre.
posicioUp :: Posicio -> Posicio
posicioUp (col :/ fila) = col :/ (fila + 1)

-- Retorna el desplaçament d'una posició més adalt 
-- respecte la posició passada per paràmetre.
posicioDown :: Posicio -> Posicio
posicioDown (col :/ fila) = col :/ (fila - 1)

-- Retorna el desplaçament d'una posició més a la dreta 
-- respecte la posició passada per paràmetre.
posicioRight :: Posicio -> Posicio
posicioRight (col :/ fila) = (chr (ord col + 1)) :/ fila

-- Retorna el desplaçament d'una posició més a l'esquerra 
-- respecte la posició passada per paràmetre.
posicioLeft :: Posicio -> Posicio
posicioLeft (col :/ fila) = (chr (ord col - 1)) :/ fila

-- Retorna el desplaçament d'una posició més a la diagonal
-- superior esquerra respecte la posició passada per paràmetre.
posicioDiagSupEsq :: Posicio -> Posicio
posicioDiagSupEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila + 1)

-- Retorna el desplaçament d'una posició més a la diagonal
-- superior dreta respecte la posició passada per paràmetre.
posicioDiagSupDreta :: Posicio -> Posicio
posicioDiagSupDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila + 1)

-- Retorna el desplaçament d'una posició més a la diagonal
-- inferior esquerra respecte la posició passada per paràmetre.
posicioDiagInfEsq :: Posicio -> Posicio
posicioDiagInfEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila - 1)

-- Retorna el desplaçament d'una posició més a la diagonal
-- inferior dreta respecte la posició passada per paràmetre.
posicioDiagInfDreta :: Posicio -> Posicio
posicioDiagInfDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila - 1)

-- Aplica una funció passada per paràmetre a una posició
-- passada per paràmetre, i retorna, en un llistat, el resultat
-- d'aplicar la funció a la posició, de tornar a aplicar la funció
-- al previ resultat i així successivament fins trobar una
-- posició no vàlida.
aplicarFunc :: (Posicio -> Posicio) -> Posicio -> [Posicio]
aplicarFunc f x = if (valida) then aplic : (aplicarFunc f aplic) else []
    where
        aplic = f x
        valida = posicioValida aplic

-- Desplaça les columnes i/o files d'una posició passada
-- per paràmetre en un nombre determinat d'unitats, també
-- passats per paràmetre.
sumaCoords :: Posicio -> Int -> Int -> Posicio
sumaCoords (col :/ fila) x y = ((chr (ord col + x)) :/ (fila + y))

-- Genera les posicions on pot anar una peça passada per
-- paràmetre trobant-se en una posició passada per paràmetre,
-- d'acord amb les regles del joc i com es pot moure la peça.
generarMoviments :: Peca -> Posicio -> [Posicio]
generarMoviments (Pec t c) pos
    | t == Peo = 
        if (c == Blanc) -- El color del peó és blanc
            then [posicioUp pos, posicioDiagSupEsq pos, posicioDiagSupDreta pos, posicioUp (posicioUp pos)]
            else [posicioDown pos, posicioDiagInfEsq pos, posicioDiagInfDreta pos, posicioDown (posicioDown pos)]
    | t == Cavall = [sumaCoords pos 1 2, sumaCoords pos 2 1, sumaCoords pos 2 (-1), sumaCoords pos 1 (-2), 
                    sumaCoords pos (-1) 2, sumaCoords pos (-2) 1, sumaCoords pos (-2) (-1), sumaCoords pos (-1) (-2)]
    | t == Alfil = (aplicarFunc posicioDiagSupEsq pos) ++ (aplicarFunc posicioDiagSupDreta pos) ++ (aplicarFunc posicioDiagInfEsq pos) ++
                  (aplicarFunc posicioDiagInfDreta pos)
    | t == Torre = (aplicarFunc posicioUp pos) ++ (aplicarFunc posicioRight pos) ++ (aplicarFunc posicioDown pos) ++ (aplicarFunc posicioLeft pos)
    | t == Reina = (aplicarFunc posicioUp pos) ++ (aplicarFunc posicioRight pos) ++ (aplicarFunc posicioDown pos) ++ (aplicarFunc posicioLeft pos) ++
                  (aplicarFunc posicioDiagSupEsq pos) ++ (aplicarFunc posicioDiagSupDreta pos) ++ (aplicarFunc posicioDiagInfEsq pos) ++
                  (aplicarFunc posicioDiagInfDreta pos)
    | otherwise = [posicioUp pos, posicioDiagSupDreta pos, posicioRight pos, posicioDiagInfDreta pos, posicioDown pos, posicioDiagInfEsq pos, posicioLeft pos, posicioDiagSupEsq pos] -- Cas del Rei

-- Retorna les posicions on es pot desplaçar una peça passada per
-- paràmetre quan està situada en una posició passada per paràmetre.
moviment :: Peca -> Posicio -> [Posicio]
moviment (Pec t c) pos = if (t == Peo || t == Cavall || t == Rei) then filter (posicioValida) mov else mov
    where
        mov = generarMoviments (Pec t c) pos

-- Retorna les posicions on es poden desplaçar les peces que es troben
-- en una posició. Cada peça (segon element) i posició (primer element)
-- es troba en una tupla, que es troben dins d'una llista que és la que es
-- passa per paràmetre.
moviments :: [(Posicio, Peca)] -> [Posicio]
moviments [] = []
moviments ((p, c) : ll) = moviment c p ++ moviments ll

-- Compara la fila de dos posicions passades per paràmetre:
-- si les files són iguals retorna 0, si la fila de la
-- posició A és inferior a la de la posició B retorna -1, i
-- si la posició A és superior a la de la posició B retorna 1.
compararFila :: Posicio -> Posicio -> Int
compararFila (_ :/ fa) (_ :/ fb)
    | fa == fb = 0
    | fa < fb = -1
    | otherwise = 1

-- Compara la columna de dos posicions passades per paràmetre:
-- si les columnes són iguals retorna 0, si la columna de la
-- posició A és inferior a la de la posició B retorna -1, i
-- si la posició A és superior a la de la posició B retorna 1.
compararColumna :: Posicio -> Posicio -> Int
compararColumna (ca :/ _) (cb :/ _)
    | ca == cb = 0
    | (ord ca) < (ord cb) = -1
    | otherwise = 1

-- Retorna les posicions dins d'un interval passat per paràmetre
-- (posA, posB) aplicant una funció passada per paràmetre fins
-- trobar una posició no vàlida o que sigui la posició B.
-- Dins d'aquesta llista de posicions s'exclouen tant posA com posB.
generarPosicions :: Posicio -> Posicio -> (Posicio -> Posicio) -> [Posicio]
generarPosicions a b f = if (valida) then segCas : (generarPosicions segCas b f) else []
    where
        segCas = f a
        valida = (posicioValida segCas) && (segCas /= b) -- Posició dins del rang del tauler i no és posició destí (posició B)

-- Retorna les posicions que es troben dins de l'interval passat
-- per paràmetre (posA, posB). Dins d'aquesta llista de posicions
-- s'exclouen tant posA com posB.
posicionsEntre :: Posicio -> Posicio -> [Posicio]
posicionsEntre a b
    | compFila == 0 = -- Coincideix fila de la posició A i posició B
        if (compCol == 0) -- Coincideix columna de la posició A i posició B
            then [] -- No hi ha cap peça entre elles
            else if (compCol == -1) -- La columna A és inferior a la de la posició B
                then (generarPosicions a b posicioRight)
                else (generarPosicions a b posicioLeft) -- Columna A superior a la de la posició B
    | compFila == -1 = -- La fila de la posició A és inferior a la de la posició B
        if (compCol == 0) -- Coincideix columna de la posició A amb la posició B
            then (generarPosicions a b posicioUp) -- Retornem les caselles que hi ha des d'A fins a B partint des d'A i anant fins 
            else if (compCol == -1)
                then (generarPosicions a b posicioDiagSupDreta)
                else (generarPosicions a b posicioDiagSupEsq)
    | otherwise = 
        if (compCol == 0)
            then (generarPosicions a b posicioDown)
            else if (compCol == -1)
                then (generarPosicions a b posicioDiagInfDreta)
                else (generarPosicions a b posicioDiagInfEsq)
    where
        compFila = compararFila a b -- Comparació de les files de la posició A i B
        compCol = compararColumna a b -- Comparació de les columnes de la posició A i B

-- Retorna cert si hi ha alguna peça entre dues posicions
-- passades per paràmetre (excloent les dues posicions, es clar);
-- fals altrament.
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre t p q = algunaOcupada caselles
    where
        caselles = trobarPeces t (posicionsEntre p q)
        algunaOcupada [] = False
        algunaOcupada (c : cs) = if (isJust c) then True else algunaOcupada cs

-- Retorna el resultat d'aplicar una jugada passada per paràmetre
-- sobre el tauler passat per paràmetre. El booleà passat per paràmetre
-- indica si s'ha trobat la casella de la posició destí de la jugada o no.
aplicarJugada :: Tauler -> Jugada -> Bool -> [(Posicio, Peca)]
aplicarJugada (Tau []) (Jug pj x0 x1) trobat = if (not trobat) then [(x1, pj)] else [] -- Si hem iterat tot el tauler i no hem trobat la casella destí, la retornem per afegir-la al tauler que generarem; altrament, retornarem una llista buida
aplicarJugada (Tau ((p, c) : t)) (Jug pj x0 x1) trobat =
    if (p == x0) -- Hem trobat la casella origen de la jugada
        then aplicarJugada (Tau t) (Jug pj x0 x1) trobat -- Seguim iterant i generant el tauler sense considerar ("eliminant") la casella origen que ara estarà buida (per tant no formarà part del nou tauler)
        else if (p == x1) -- Si això és cert, s'ha capturat una peça de l'adversari (la casella formava part del tauler, i només emmagatzemem les caselles amb una peça); s'ha de comprovar amb una altra funció si la peça de la casella destí és de l'adversari
            then [(x1, pj)] ++ aplicarJugada (Tau t) (Jug pj x0 x1) True -- Afegirem la peça de la jugada a la casella destí a la resta de la llista de caselles del tauler
            else [(p, c)] ++ aplicarJugada (Tau t) (Jug pj x0 x1) trobat -- Afegirem la casella de la iteració actual al tauler juntament amb les demés caselles

-- Retorna un nou tauler resultant d'aplicar la jugada passada per
-- paràmetre, partint del tauler (estat actual del joc) passat per
-- paràmetre.
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t j = Tau (aplicarJugada t j False)

-- Retorna la posició del rei del bàndol del color passat per paràmetre
-- d'acord amb el tauler passat per paràmetre.
trobarRei :: Tauler -> Color -> Posicio
trobarRei (Tau ((p, (Pec tipus color)) : t)) bandol = if (tipus == Rei && color == bandol) then p else trobarRei (Tau t) bandol

-- Retorna una llista amb les posicions i peces del bàndol del color
-- passat per paràmetre d'acord amb el tauler passat per paràmetre. 
pecesDeColor :: Tauler -> Color -> [(Posicio, Peca)]
pecesDeColor (Tau []) color = []
pecesDeColor (Tau ((p, (Pec pt pc)) : t)) color = if (pc == color) then (p, (Pec pt pc)) : pecesDeColor (Tau t) color else pecesDeColor (Tau t) color

-- Retorna un llistat amb els moviments que poden fer les peces del
-- bàndol del color passat per paràmetre d'acord amb el tauler passat
-- per paràmetre.
movimentsColor :: Tauler -> Color -> [Posicio]
movimentsColor t color = moviments (pecesDeColor t color)

escac :: Tauler -> Color -> Bool
escac (Tau t) c = elem posRei movimentsContrincant
    where
        posRei = trobarRei (Tau t) c
        colorContrincant = if (c == Blanc) then Negre else Blanc
        movimentsContrincant = movimentsColor (Tau t) colorContrincant

-- Si la jugada és vàlida, retornarà 0 o 1. Això es determinarà a partir de
-- la jugada que es vol fer i l'estat del tauler, que són elements que es passen
-- per paràmetre. Retornarà 0 quan la jugada passada sigui vàlida i no es capturi
-- a cap peça contrincant. Si la jugada és vàlida però es captura una peça contrincant,
-- es retornarà 1. Si la jugada no és vàlida, retorna:
--      -1 -> "La casella origen de la jugada esta buida"
--      -2 -> "La peça de la casella origen no coincideix amb la peça de la jugada"
--      -3 -> "El moviment de la jugada no és vàlid d'acord amb els moviments que pot fer la peça"
--      -4 -> "Hi ha una peça pel mig entre la posició origen i la posició destí la jugada"
--      -5 -> "La posició destí de la jugada està ocupada per una peça del mateix jugador que fa la jugada"
--      -6 -> "La situació actual és d'escac, i la jugada segueix en escac"
jugadaLegal :: Tauler -> Jugada -> Int
jugadaLegal t (Jug p x0 x1)
    | origenLliure = -1
    | origenDiferent = -2
    | movimInvalid || ((tipusPeca p) == Peo && movPeoInvalid) = -3
    | destiMateixJugador = -5
    | ((tipusPeca p) == Cavall && pecaPelMig) = -4
    | segueixEnEscac = -6
    | otherwise = if (isJust desti && (not destiMateixJugador)) then 1 else 0
    where
        desti = trobarPeca t x1
        destiMateixJugador = (isJust desti) && ((colorPeca (fromJust desti)) == (colorPeca p))
        pecaPelMig = alguEntre t x0 x1
        origen = trobarPeca t x0
        origenLliure = isNothing origen
        origenDiferent = (fromJust origen) /= p
        movimInvalid = not (elem x1 (moviment p x0))
        movPeoInvalid =
            if (posicioDiagSupEsq x0) == x1 || (posicioDiagSupDreta x0) == x1 || (posicioDiagInfEsq x0) == x1 || (posicioDiagInfDreta x0) == x1
                then isNothing desti || ((isJust desti) && (colorPeca p) == (colorPeca (fromJust desti)))
                else isJust desti -- Peó no pot matar anant cap endavant, només mata en diagonal
        segueixEnEscac = (escac t (colorPeca p)) && escac (fesJugada t (Jug p x0 x1)) (colorPeca p)

jugadesColor :: Tauler -> Color -> [Jugada]
jugadesColor t c = jugsPeces (pecesDeColor t c)
    where
        jugs p [] = []
        jugs p (m : ms) = (Jug (snd p) (fst p) m) : jugs p ms
        jugLegal j = jugadaLegal t j == 0
        jugsPeces [] = []
        jugsPeces (p : ps) = (filter jugLegal (jugs p (moviment (snd p) (fst p)))) ++ jugsPeces ps

-- ESCAC MAT QUAN NO SIGUI POSSIBLE CAP DE LES SEGÜENTS:
-- i)   interposició d'una peça entre agresora-agredida
-- ii)  captura de la peça agresora
-- iii) moure peça agredida a un escac (fora de l'acció de les peces contràries)
escacMat :: Tauler -> Color -> Bool
escacMat (Tau t) c = (escac (Tau t) c) && escapatoria
    where
        jugades = jugadesColor (Tau t) c
        escacs [] = []
        escacs (j : js) = (escac (fesJugada (Tau t) j) c) : escacs js
        escapatoria = not (elem False (escacs jugades))

-- Llegeix una linia del tipus "1. Pe2e4 Pe7e5" i ho parseja
-- en forma de Tupla, tenint en compte si son 2 o 3 paràmetres
llegirLinia :: String -> (String, JugadaGenerica, Maybe JugadaGenerica)
llegirLinia x =
    if(length (words x) == 2)
        then tornaDos (words x)
        else tornaTres (words x)
    where
        tornaDos [num,j1] = (num, llegirJugada j1 Blanc, Nothing)
        tornaTres [num,j1,j2] = (num, llegirJugada j1 Blanc, Just(llegirJugada j2 Negre))

-- Donat un String com per exemple "Pe7e5" o "Dh5xf7++" o "0-0" 
-- retorna el tipus de JugadaGenerica concret que és      
llegirJugada :: String -> Color -> JugadaGenerica
llegirJugada jug color
    | (elem '0' jug) && length jug>3 = (EnrocCurt torre ('a':/3) ('z':/3)) --TODO: ENROC CURT
    | (elem '0' jug) = (EnrocLlarg torre ('a':/3) ('z':/3)) --TODO: ENROC LLARG
    | (elem '+' jug) = do
        let jugClean = [ x | x <- jug, not (x `elem` "+x") ]
        let p = take 1 jugClean !! 0
        let x1 = take 1 (drop 1 jugClean) !! 0
        let y1 = take 1 (drop 2 jugClean) !! 0
        let x2 = take 1 (drop 3 jugClean) !! 0
        let y2 = take 1 (drop 4 jugClean) !! 0
        if (length $ filter (== '+') jug)==1 
            then (Escac (llegirPeca p color) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))
            else (EscacMat (llegirPeca p color) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))
    | otherwise = do
        let jugClean = [ x | x <- jug, not (x `elem` "x") ]
        let p = take 1 jugClean !! 0
        let x1 = take 1 (drop 1 jugClean) !! 0
        let y1 = take 1 (drop 2 jugClean) !! 0
        let x2 = take 1 (drop 3 jugClean) !! 0
        let y2 = take 1 (drop 4 jugClean) !! 0
        (Jugada (llegirPeca p color) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))

--      -1 -> "La casella origen de la jugada esta buida"
--      -2 -> "La peça de la casella origen no coincideix amb la peça de la jugada"
--      -3 -> "El moviment de la jugada no és vàlid d'acord amb els moviments que pot fer la peça"
--      -4 -> "Hi ha una peça pel mig entre la posició origen i la posició destí la jugada"
--      -5 -> "La posició destí de la jugada està ocupada per una peça del mateix jugador que fa la jugada"
tractaUnaJugada :: String -> Tauler -> Color -> JugadaGenerica -> Tauler
tractaUnaJugada n tauler color (Jugada (Pec tip _) p q) = do
        let jug = (Jug (Pec tip color) p q)
        if jugadaLegal tauler jug == 0 then fesJugada tauler jug
        else if jugadaLegal tauler jug == -1 then error ("INVALID: Ronda "++ n ++" Jugador amb peces " ++ show color ++": La casella origen de la jugada esta buida")
        else if jugadaLegal tauler jug == -2 then error ("INVALID: Ronda "++ n ++" Jugador amb peces " ++ show color ++": La peça de la casella origen no coincideix amb la peça de la jugada")
        else if jugadaLegal tauler jug == -3 then error ("INVALID: Ronda "++ n ++" Jugador amb peces " ++ show color ++": El moviment de la jugada no és vàlid d'acord amb els moviments que pot fer la peça")
        else if jugadaLegal tauler jug == -4 then error ("INVALID: Ronda "++ n ++" Jugador amb peces " ++ show color ++": Hi ha una peça pel mig entre la posició origen i la posició destí la jugada")
        else error("INVALID: Ronda["++ n ++"] Jugador amb peces " ++ show color ++ ": La posició destí de la jugada està ocupada per una peça del mateix jugador que fa la jugada")
tractaUnaJugada n tauler color (EscacMat pe p q)     --TODO
        | jugadaLegal tauler (Jug pe p q) == 0 = fesJugada tauler (Jug pe p q)
        | otherwise = fesJugada tauler (Jug pe p q)
tractaUnaJugada n tauler color (EnrocLlarg pe p q)   --TODO
        |  True = fesJugada tauler (Jug peo posA posB)
        | otherwise = fesJugada tauler (Jug peo posA posB)
tractaUnaJugada n tauler color (Escac pe p q)        --TODO
        | jugadaLegal tauler (Jug pe p q) == 0 = fesJugada tauler (Jug pe p q)
        | otherwise = fesJugada tauler (Jug pe p q)  -- ...

evalua :: Tauler -> (String, JugadaGenerica, Maybe JugadaGenerica) -> Tauler
evalua t (n, j1, Nothing) = traceShow (tractaUnaJugada n t Blanc j1) (tractaUnaJugada n t Blanc j1)
evalua t (n, j1, Just j2) = traceShow ((tractaUnaJugada n t Blanc j1),(tractaUnaJugada n (tractaUnaJugada n t Blanc j1) Negre j2)) (tractaUnaJugada n (tractaUnaJugada n t Blanc j1) Negre j2)

-- Exemple d'ús: llegirPartida "pastor.txt"
-- Interpreta la partida i la tradueix a Jugades, s'evaluen a "evalua"
llegirPartida :: String -> IO()    
llegirPartida fitxer= do
    x <- readFile fitxer
    let rondes = map llegirLinia (lines x)
    mostraTauler (foldl evalua taulerInicial rondes)