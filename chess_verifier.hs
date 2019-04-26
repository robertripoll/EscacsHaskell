import Data.Char
import Data.Maybe

-- TIPUS DE DADES I INSTANCES

-- Color

data Color = Blanc | Negre deriving Eq

-- TipusPeça

data TipusPeca = Rei | Reina | Torre | Alfil | Cavall | Peo deriving Eq

-- Peça

data Peca = Pec TipusPeca Color deriving Eq

tipusPeca :: Peca -> TipusPeca
tipusPeca (Pec t _) = t

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

taulerInicial = Tau [(('a' :/ 1), (Pec Torre Blanc)), (('b' :/ 1), (Pec Cavall Blanc)), (('c' :/ 1), (Pec Alfil Blanc)), (('d' :/ 1), (Pec Reina Blanc)), (('e' :/ 1), (Pec Rei Blanc)), (('f' :/ 1), (Pec Alfil Blanc)), (('g' :/ 1), (Pec Cavall Blanc)), (('h' :/ 1), (Pec Torre Blanc)),
                      (('a' :/ 8), (Pec Torre Negre)), (('b' :/ 8), (Pec Cavall Negre)), (('c' :/ 8), (Pec Alfil Negre)), (('d' :/ 8), (Pec Reina Negre)), (('e' :/ 8), (Pec Rei Negre)), (('f' :/ 8), (Pec Alfil Negre)), (('g' :/ 8), (Pec Cavall Negre)), (('h' :/ 8), (Pec Torre Negre)),
                      (('a' :/ 2), (Pec Peo Blanc)), (('b' :/ 2), (Pec Peo Blanc)), (('c' :/ 2), (Pec Peo Blanc)), (('d' :/ 2), (Pec Peo Blanc)), (('e' :/ 2), (Pec Peo Blanc)), (('f' :/ 2), (Pec Peo Blanc)), (('g' :/ 2), (Pec Peo Blanc)), (('h' :/ 2), (Pec Peo Blanc)),
                      (('a' :/ 7), (Pec Peo Negre)), (('b' :/ 7), (Pec Peo Negre)), (('c' :/ 7), (Pec Peo Negre)), (('d' :/ 7), (Pec Peo Negre)), (('e' :/ 7), (Pec Peo Negre)), (('f' :/ 7), (Pec Peo Negre)), (('g' :/ 7), (Pec Peo Negre)), (('h' :/ 7), (Pec Peo Negre))]

-- Retorna una peça d'acord amb el caràcter passat
-- per paràmetre.
llegirPeca :: Char -> Peca
llegirPeca p = do
    let color = if(isUpper p) then Blanc else Negre
    let x = toUpper p
    if (x == 'P') then (Pec Peo color)
    else if (x == 'T') then (Pec Torre color)
    else if (x == 'C') then (Pec Cavall color)
    else if (x == 'A') then (Pec Alfil color)
    else if (x == 'D') then (Pec Reina color)
    else (Pec Rei color)


ferPos :: (Int,Int) -> Posicio
ferPos (x,y) = (chr x :/ y)

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

data JugadaGenerica = Jugada Peca Posicio Posicio | EnrocCurt Peca Posicio Posicio | EnrocLlarg Peca Posicio Posicio | Escac Peca Posicio Posicio | EscacMat Peca Posicio Posicio deriving Eq
instance Show JugadaGenerica where
    show (Jugada p x0 x1) = "Jugada"
    show (EnrocLlarg p x0 x1) = "EnrocLlarg"
    show (EnrocCurt p x0 x1) = "EnrocCurt"
    show (Escac p x0 x1) = "Escac"
    show (EscacMat p x0 x1) = "EscacMat"

-- Casella

type Casella = (Posicio, Peca)

mostraCasella :: Casella -> Char
mostraCasella (_, p) = mostraPeca p

-- Tauler

data Tauler = Tau [(Posicio, Peca)]

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

-- Retorna la peça ("Just Peça") del tauler que té la posició passada
-- per paràmetre. Si la posició no existeix retorna "Nothing".
trobarPeca :: Tauler -> Posicio -> Maybe Peca
trobarPeca (Tau t) p = if (null trobat) then Nothing else Just (snd (trobat !! 0))
    where
        esCasella (pc :/ pf) (p, _) = p == (pc :/ pf)
        trobat = (filter (esCasella p) t)
        peca (_, x) = x

-- Retorna un conjunt de caselles que tenen la posició passada
-- per paràmetre. Si alguna de les posicions no existeix, retorna error.
trobarPeces :: Tauler -> [Posicio] -> [Maybe Peca]
trobarPeces t [] = []
trobarPeces t (p : ps) = (trobarPeca t p) : (trobarPeces t ps)

-- Retorna la fila d'una posició.
fila :: Posicio -> Int
fila (_ :/ x) = x

-- Retorna la columna d'una posició.
columna :: Posicio -> Char
columna (x :/ _) = x

-- Retorna cert si la posició passada per paràmetre
-- és vàlida (no surt fora del rang del tauler); fals
-- altrament.
posicioValida :: Posicio -> Bool
posicioValida (c :/ f) = (c >= 'a' && c <= 'h' && f >= 1 && f <= 8)

-- Retorna el desplaçament una posició més adalt 
-- de la posició passada per paràmetre.
posicioUp :: Posicio -> Posicio
posicioUp (col :/ fila) = col :/ (fila + 1)

-- Retorna el desplaçament una posició més abaix 
-- de la posició passada per paràmetre.
posicioDown :: Posicio -> Posicio
posicioDown (col :/ fila) = col :/ (fila - 1)

-- Retorna el desplaçament una posició més a la dreta 
-- de la posició passada per paràmetre.
posicioRight :: Posicio -> Posicio
posicioRight (col :/ fila) = (chr (ord col + 1)) :/ fila

-- Retorna el desplaçament una posició més a l'esquerra 
-- de la posició passada per paràmetre.
posicioLeft :: Posicio -> Posicio
posicioLeft (col :/ fila) = (chr (ord col - 1)) :/ fila

-- Retorna el desplaçament una posició més a la diagonal
-- superior esquerra de la posició passada per paràmetre.
posicioDiagSupEsq :: Posicio -> Posicio
posicioDiagSupEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila + 1)

-- Retorna el desplaçament una posició més a la diagonal
-- superior dreta de la posició passada per paràmetre.
posicioDiagSupDreta :: Posicio -> Posicio
posicioDiagSupDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila + 1)

-- Retorna el desplaçament una posició més a la diagonal
-- inferior esquerra de la posició passada per paràmetre.
posicioDiagInfEsq :: Posicio -> Posicio
posicioDiagInfEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila - 1)

-- Retorna el desplaçament una posició més a la diagonal
-- inferior dreta de la posició passada per paràmetre.
posicioDiagInfDreta :: Posicio -> Posicio
posicioDiagInfDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila - 1)

-- Aplica una funció passada per paràmetre a una posició
-- passada per paràmetre, i retorna el resultat d'aplicar
-- la funció a la posició, de tornar a aplicar la funció
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

-- Genera els moviments (vàlids o no) que pot fer un tipus
-- de peça trobant-se en una posició concreta.
generarMoviments :: TipusPeca -> Posicio -> [Posicio]
generarMoviments t pos
   | t == Peo = [posicioUp pos, posicioDiagSupEsq pos, posicioDiagSupDreta pos, posicioUp (posicioUp pos)]
   | t == Cavall = [sumaCoords pos 1 2, sumaCoords pos 2 1, sumaCoords pos 2 (-1), sumaCoords pos 1 (-2), 
                    sumaCoords pos (-1) 2, sumaCoords pos (-2) 1, sumaCoords pos (-2) (-1), sumaCoords pos (-1) (-2)]
   | t == Alfil = (aplicarFunc posicioDiagSupEsq pos) ++ (aplicarFunc posicioDiagSupDreta pos) ++ (aplicarFunc posicioDiagInfEsq pos) ++
                  (aplicarFunc posicioDiagInfDreta pos)
   | t == Torre = (aplicarFunc posicioUp pos) ++ (aplicarFunc posicioRight pos) ++ (aplicarFunc posicioDown pos) ++ (aplicarFunc posicioLeft pos)
   | t == Reina = (aplicarFunc posicioUp pos) ++ (aplicarFunc posicioRight pos) ++ (aplicarFunc posicioDown pos) ++ (aplicarFunc posicioLeft pos) ++
                  (aplicarFunc posicioDiagSupEsq pos) ++ (aplicarFunc posicioDiagSupDreta pos) ++ (aplicarFunc posicioDiagInfEsq pos) ++
                  (aplicarFunc posicioDiagInfDreta pos)
   | otherwise = [posicioUp pos, posicioDiagSupDreta pos, posicioRight pos, posicioDiagInfDreta pos, posicioDown pos, posicioDiagInfEsq pos, posicioLeft pos, posicioDiagSupEsq pos] -- Cas del Rei

-- Retorna els moviments possibles que pot fer una peça
-- des d'una posició concreta.
moviment :: Peca -> Posicio -> [Posicio]
moviment (Pec t _) pos = if (t == Peo || t == Cavall || t == Rei) then filter (posicioValida) mov else mov
    where
        mov = generarMoviments t pos

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

-- Retorna les posicions dins d'un interval passat per
-- paràmetre (posA, posB) aplicant una funció passada
-- per paràmetre. Dins d'aquesta llista de posicions
-- s'exclouen tant posA com posB.
generarPosicions :: Posicio -> Posicio -> (Posicio -> Posicio) -> [Posicio]
generarPosicions a b f = if (valida) then segCas : (generarPosicions segCas b f) else []
    where
        segCas = f a
        valida = (posicioValida segCas) && (segCas /= b)

-- Retorna les posicions dins d'un interval passat per
-- paràmetre (posA, posB). Dins d'aquesta llista de
-- posicions s'exclouen tant posA com posB.
posicionsEntre :: Posicio -> Posicio -> [Posicio]
posicionsEntre a b
    | compFila == 0 =
        if (compCol == 0)
            then []
            else if (compCol == -1)
                then (generarPosicions a b posicioRight)
                else (generarPosicions a b posicioLeft)
    | compFila == -1 =
        if (compCol == 0)
            then (generarPosicions a b posicioUp)
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
        compFila = compararFila a b
        compCol = compararColumna a b

-- Retorna cert si hi ha alguna peça entre dues posicions
-- passades per paràmetre; fals altrament.
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre t p q = algunaOcupada caselles
    where
        caselles = trobarPeces t (posicionsEntre p q)
        algunaOcupada [] = False
        algunaOcupada (c : cs) = if (isJust c) then True else algunaOcupada cs

aplicarJugada :: Tauler -> Jugada -> Bool -> [(Posicio, Peca)]
aplicarJugada (Tau []) (Jug pj x0 x1) trobat = if (not trobat) then [(x1, pj)] else []
aplicarJugada (Tau ((p, c) : t)) (Jug pj x0 x1) trobat =
    if (p == x0)
        then aplicarJugada (Tau t) (Jug pj x0 x1) trobat
        else if (p == x1) -- Si això és cert, s'ha capturat una peça de l'adversari (s'ha de comprovar amb una altra funció si la peça de la casella destí és de l'adversari)
            then [(x1, pj)] ++ aplicarJugada (Tau t) (Jug pj x0 x1) True
            else [(p, c)] ++ aplicarJugada (Tau t) (Jug pj x0 x1) trobat

fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t j = Tau (aplicarJugada t j False)

trobarRei :: Tauler -> Color -> Posicio
trobarRei (Tau ((p, (Pec tipus color)) : t)) bandol = if (tipus == Rei && color == bandol) then p else trobarRei (Tau t) bandol

pecesDeColor :: Tauler -> Color -> [(Posicio, Peca)]
pecesDeColor (Tau []) color = []
pecesDeColor (Tau ((p, (Pec pt pc)) : t)) color = if (pc == color) then (p, (Pec pt pc)) : pecesDeColor (Tau t) color else pecesDeColor (Tau t) color

movimentsColor :: Tauler -> Color -> [Posicio]
movimentsColor t color = moviments (pecesDeColor t color)

escac :: Tauler -> Color -> Bool
escac (Tau t) c = elem posRei movimentsContrincant
    where
        posRei = trobarRei (Tau t) c
        colorContrincant = if (c == Blanc) then Negre else Blanc
        movimentsContrincant = movimentsColor (Tau t) colorContrincant

-- Ens fa falta una funció que accedeixi a una posició concreta del tauler... 
casellaLliure :: Tauler -> Posicio -> Bool
casellaLliure t p = isNothing (trobarPeca t p)

-- Retorna 0 si la jugada passada és vàlida d'acord amb l'estat
-- del tauler del paràmetre; si la jugada no és vàlida, retorna:
--      -1 -> "La casella origen de la jugada esta buida"
--      -2 -> "La peça de la casella origen no coincideix amb la peça de la jugada"
--      -3 -> "El moviment de la jugada no és vàlid d'acord amb els moviments que pot fer la peça"
--      -4 -> "Hi ha una peça pel mig entre la posició origen i la posició destí la jugada"
--      -5 -> "La posició destí de la jugada està ocupada per una peça del mateix jugador que fa la jugada"
jugadaLegal :: Tauler -> Jugada -> Int
jugadaLegal t (Jug p x0 x1)
    | origenLliure = -1
    | origenDiferent = -2
    | movimInvalid = -3
    | pecaPelMig = -4
    | destiMateixJugador = -5
    | otherwise = 0
    where
        desti = (trobarPeca t x1)
        destiMateixJugador = (isJust desti) && ((colorPeca (fromJust desti)) == (colorPeca p))
        pecaPelMig = alguEntre t x0 x1
        origen = (trobarPeca t x0)
        origenLliure = isNothing origen
        origenDiferent = (fromJust origen) /= p
        movimInvalid = not (elem x1 (moviment p x0))

-- ESCAC MAT QUAN NO SIGUI POSSIBLE CAP DE LES SEGÜENTS:
-- i)   interposició d'una peça entre agresora-agredida
-- ii)  captura de la peça agresora
-- iii) moure peça agredida a un escac (fora de l'acció de les peces contràries)
--escacMat :: Tauler -> Color -> Bool


-- Llegeix una linia del tipus "1. Pe2e4 Pe7e5" i ho parseja
-- en forma de Tupla, tenint en compte si son 2 o 3 paràmetres
llegirLinia :: String -> (String, JugadaGenerica, Maybe JugadaGenerica)
llegirLinia x =
    if(length (words x) == 2)
        then tornaDos (words x)
        else tornaTres (words x)
    where
        tornaDos [num,j1] = (num, llegirJugada j1, Nothing)
        tornaTres [num,j1,j2] = (num, llegirJugada j1, Just(llegirJugada j2))

-- Donat un String com per exemple "Pe7e5" o "Dh5xf7++" o "0-0" 
-- retorna el tipus de JugadaGenerica concret que és      
llegirJugada :: String -> JugadaGenerica
llegirJugada jug
    | (elem '0' jug) && length jug>3 = (EnrocCurt torre ('a':/3) ('z':/3)) --TODO: ENROC CURT
    | (elem '0' jug) = (EnrocLlarg torre ('a':/3) ('z':/3)) --TODO: ENROC LLARG
    | (elem '+' jug) = do
        let jugClean = [ x | x <- jug, not (x `elem` "+") ]
        let p = take 1 jugClean !! 0
        let x1 = take 1 (drop 1 jugClean) !! 0
        let y1 = take 1 (drop 2 jugClean) !! 0
        let x2 = take 1 (drop 3 jugClean) !! 0
        let y2 = take 1 (drop 4 jugClean) !! 0
        if (length $ filter (== '+') jug)==1 
            then (Escac (llegirPeca p) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))
            else (EscacMat (llegirPeca p) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))
    | otherwise = do
        let p = take 1 jug !! 0
        let x1 = take 1 (drop 1 jug) !! 0
        let y1 = take 1 (drop 2 jug) !! 0
        let x2 = take 1 (drop 3 jug) !! 0
        let y2 = take 1 (drop 4 jug) !! 0
        (Jugada (llegirPeca p) ( x1 :/ digitToInt y1 ) ( x2 :/ digitToInt y2 ))


-- Exemple d'ús: llegirPartida "pastor.txt"
-- Interpreta la partida i la tradueix a Jugades, s'evaluen a "evalua"
llegirPartida :: String -> IO()    
llegirPartida fitxer= do
    x <- readFile fitxer
    let rondes = map llegirLinia (lines x)
    print (map evalua rondes)
    where
        evalua :: (String, JugadaGenerica, Maybe JugadaGenerica) -> String
        evalua (n, j1, j2) = do
          n ++ " " ++ show j1 ++ ", " ++maybe "." show j2
