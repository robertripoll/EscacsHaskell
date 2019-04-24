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

taulerInicial = unlines ["tcadract"
                        ,"pppppppp"
                        ,"        "
                        ,"        "
                        ,"        "
                        ,"        "
                        ,"PPPPPPPP"
                        ,"TCADRACT"]

-- Retorna una casella d'acord amb el caràcter passat
-- per paràmetre.
llegirCasella :: Char -> Casella
llegirCasella c = (('z' :/ 9), llegirPeca c)

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

-- Retorna un tauler d'acord amb la cadena passada
-- per paràmetre.
--llegirTauler :: String -> Tauler
--llegirTauler = map llegirFila . lines
--    where llegirFila = map llegirCasella

-- Mostra el tauler per pantalla.
--mostraTauler :: Tauler -> IO()
--mostraTauler x = putStr(board2str x) where
--    board2str :: Tauler -> String
--    board2str = unlines . map mostraFila
--        where mostraFila x = map mostraCasella x

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

data Jugada = Jug Peca Posicio Posicio

instance Show Jugada where
    show (Jug p x0 x1) = show p ++ show x0 ++ show x1

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

unTauler :: Tauler
unTauler = Tau [casB, casB, casB, casB, casB, casB, casB, casB, casB, casB, casB, casA, casB]


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
        algunaOcupada (c : cs) = if (isJust c) then True else algunaOcupada cs

-- fesJugada :: Tauler -> Jugada -> Tauler
--fesJugada t (Jug tipus x0 x1)  = 

--escac :: Tauler -> Color -> Bool
--escac t c = False

-- Ens fa falta una funció que accedeixi a una posició concreta del tauler... 
casellaLliure :: Tauler -> Posicio -> Bool
casellaLliure t p = isNothing (trobarPeca t p)

-- Comprovar:
--  * Casella destí sigui del color contrari (es pugi matar la peça)
--  * No hi hagi cap peça de per mig 
--  * El moviment sigui possible (✓)
--  * Si a la casella origen està la peça que ens passen
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t (Jug p x0 x1) =
    if origenLliure
        then error "La casella origen de la jugada esta buida"
        else if origenDiferent
            then error "La peça de la casella origen no coincideix amb la peça de la jugada"
            else if pecaPelMig
                then error "Hi ha una peça pel mig entre la posició origen i la posició destí la jugada"
                else if destiMateixJugador
                    then error "La posició destí de la jugada està ocupada per una peça del mateix jugador que fa la jugada"
                    else True
    where
        desti = (trobarPeca t x1)
        destiMateixJugador = (isJust desti) && ((colorPeca (fromJust desti)) == (colorPeca p))
        pecaPelMig = alguEntre t x0 x1
        origen = (trobarPeca t x0)
        origenLliure = isNothing origen
        origenDiferent = (fromJust origen) /= p

--escacMat :: Tauler -> Color -> Bool
--escacMat t c = False