-- CLASSES:
--      Peça
--      Tauler
--      Partida
--      Jugada
--      Posició

import Data.Char

data Color = Blanc | Negre deriving Eq

data TipusPeca = Rei | Reina | Torre | Alfil | Cavall | Peo deriving Eq

data Peca = Pec TipusPeca Color

mostrarColor :: Color -> Char -> Char
mostrarColor x c = if (x == Negre) then toLower c else c

instance Show Peca where
    show (Pec tipus color)
        | (tipus == Rei) = show (mostrarColor color 'R')
        | (tipus == Reina) = show (mostrarColor color 'D')
        | (tipus == Torre) = show (mostrarColor color 'T')
        | (tipus == Alfil) = show (mostrarColor color 'A')
        | (tipus == Cavall) = show (mostrarColor color 'C')
        | otherwise = show (mostrarColor color 'P')

data Posicio = Char :/ Int deriving Eq

instance Show Posicio where
    show (fila :/ col) = show fila ++ show col

data Jugada = Jug Peca Posicio Posicio

instance Show Jugada where
    show (Jug p x0 x1) = show p ++ show x0 ++ show x1

data Casella = Cas Posicio (Maybe Peca)

instance Show Casella where
    show (Cas _ (Just p)) = show p

data Tauler = Tau [Casella] Color

instance Show Tauler where
    show (Tau (x : xs) c) = show "abc"

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
casA = Cas posA (Just peo)

casB :: Casella
casB = Cas posB (Just torre)

esCasella :: Posicio -> Casella -> Bool
esCasella (pc :/ pf) (Cas p _) = p == (pc :/ pf)

unTauler :: [Casella]
unTauler = [casB, casB, casB, casB, casB, casB, casB, casB, casB, casB, casB, casA]

trobarCasella :: [Casella] -> Posicio -> Casella
trobarCasella ll p = if (null trobat) then error "Posició no trobada" else trobat !! 0
    where
        trobat = (filter (esCasella p) ll)

unaJugada :: Jugada
unaJugada = Jug torre ('a':/3) ('z':/3)

fila :: Posicio -> Int
fila (_ :/ x) = x

columna :: Posicio -> Char
columna (x :/ _) = x

compararFila :: Posicio -> Posicio -> Int
compararFila (_ :/ fa) (_ :/ fb)
    | fa == fb = 0
    | fa < fb = -1
    | otherwise = 1

compararColumna :: Posicio -> Posicio -> Int
compararColumna (ca :/ _) (cb :/ _)
    | ca == cb = 0
    | (ord ca) < (ord cb) = -1
    | otherwise = 1

posicioValida :: Posicio -> Bool
posicioValida (c :/ f) = (c >= 'a' && c <= 'h' && f >= 1 && f <= 8)

posicioUp :: Posicio -> Posicio
posicioUp (col :/ fila) = col :/ (fila + 1)

posicioDown :: Posicio -> Posicio
posicioDown (col :/ fila) = col :/ (fila - 1)

posicioRight :: Posicio -> Posicio
posicioRight (col :/ fila) = (chr (ord col + 1)) :/ fila

posicioLeft :: Posicio -> Posicio
posicioLeft (col :/ fila) = (chr (ord col - 1)) :/ fila

posicioDiagSupEsq :: Posicio -> Posicio
posicioDiagSupEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila + 1)

posicioDiagSupDreta :: Posicio -> Posicio
posicioDiagSupDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila + 1)

posicioDiagInfEsq :: Posicio -> Posicio
posicioDiagInfEsq (col :/ fila) = (chr (ord col - 1)) :/ (fila - 1)

posicioDiagInfDreta :: Posicio -> Posicio
posicioDiagInfDreta (col :/ fila) = (chr (ord col + 1)) :/ (fila - 1)

posicioSeguent :: Posicio -> Posicio
posicioSeguent (col :/ fila) = if (segCol > 'h') then ('a' :/ segFila) else (segCol :/ fila)
    where
        segCol = chr (ord col + 1)
        segFila = fila + 1

pecaTauler :: Tauler -> Posicio -> Peca
pecaTauler t p = alfil

-- Retorna el resultat d'aplicar 
aplicarFunc :: (Posicio -> Posicio) -> Posicio -> [Posicio]
aplicarFunc f x = if (valida) then aplic : (aplicarFunc f aplic) else []
    where
        aplic = f x
        valida = posicioValida aplic

sumaCoords :: Posicio -> Int -> Int -> Posicio
sumaCoords (col :/ fila) x y = ((chr (ord col + x)) :/ (fila + y))

generarMoviments :: TipusPeca -> Posicio -> [Posicio]
generarMoviments x p
   | x == Peo = [posicioUp p, posicioDiagSupEsq p, posicioDiagSupDreta p, posicioUp (posicioUp p)]
   | x == Cavall = [sumaCoords p 1 2, sumaCoords p 2 1, sumaCoords p 2 (-1), sumaCoords p 1 (-2), 
                    sumaCoords p (-1) 2, sumaCoords p (-2) 1, sumaCoords p (-2) (-1), sumaCoords p (-1) (-2)]
   | x == Alfil = (aplicarFunc posicioDiagSupEsq p) ++ (aplicarFunc posicioDiagSupDreta p) ++ (aplicarFunc posicioDiagInfEsq p) ++ (aplicarFunc posicioDiagInfDreta p)
   | x == Torre = (aplicarFunc posicioUp p) ++ (aplicarFunc posicioRight p) ++ (aplicarFunc posicioDown p) ++ (aplicarFunc posicioLeft p)
   | x == Reina = (aplicarFunc posicioUp p) ++ (aplicarFunc posicioRight p) ++ (aplicarFunc posicioDown p) ++ (aplicarFunc posicioLeft p) ++ (aplicarFunc posicioDiagSupEsq p) ++ (aplicarFunc posicioDiagSupDreta p) ++ (aplicarFunc posicioDiagInfEsq p) ++ (aplicarFunc posicioDiagInfDreta p)
   | otherwise = [posicioUp p, posicioDiagSupDreta p, posicioRight p, posicioDiagInfDreta p, posicioDown p, posicioDiagInfEsq p, posicioLeft p, posicioDiagSupEsq p] -- Cas del Rei

-- En aquesta funció sobraria el filter posicioValida per "Alfil", "Torre", "Reina"
moviment :: Peca -> Posicio -> [Posicio]
moviment (Pec tipus _) p = filter (posicioValida) (generarMoviments tipus p)

generarPosicions :: Posicio -> Posicio -> (Posicio -> Posicio) -> [Posicio]
generarPosicions a b f = if (valida) then segCas : (generarPosicions segCas b f) else []
    where
        segCas = f a
        valida = (posicioValida segCas) && (segCas /= b)

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

-- Ens fa falta una funció que accedeixi a una posició concreta del tauler...
--alguEntre :: Tauler -> Posicio -> Posicio -> Bool
--alguEntre t p q = posicionsEntre p q

--fesJugada :: Tauler -> Jugada -> Tauler
--fesJugada t j = t

--escac :: Tauler -> Color -> Bool
--escac t c = False

-- Ens fa falta una funció que accedeixi a una posició concreta del tauler... 
casellaLliure :: Tauler -> Posicio -> Bool
casellaLliure t p = True

existeixElem :: Eq a => a -> [a] -> Bool
existeixElem a [] = False
existeixElem a (x : xs) = if (a == x) then True else existeixElem a xs

jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t (Jug (Pec tipus c) x0 x1) = (casellaLliure t x1) && (existeixElem x1 (moviment (Pec tipus c) x0))

--escacMat :: Tauler -> Color -> Bool
--escacMat t c = False