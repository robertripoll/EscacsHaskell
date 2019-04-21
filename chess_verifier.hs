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

data Casella = Peca | Buida

data Tauler = Tau [[Casella]] Color

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

unaPosicio :: Posicio
unaPosicio = 'a' :/ 4

unaAltraPos :: Posicio
unaAltraPos = 'a' :/ 1

unaJugada :: Jugada
unaJugada = Jug torre ('a':/3) ('b':/2)

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

-- Això fa 7 iteracions d'un moviment, afegint cada cop el resultat a una llista
llista7 :: [Posicio -> Posicio] -> Posicio -> [Posicio]
llista7 [] _ = []
llista7 (f:fs) x = (llista7 fs x) ++ take 7 (iterate f (f x))

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
   | x == Torre =   llista7 [posicioUp, posicioDown, posicioLeft, posicioRight] p
   | x == Alfil =   llista7 [posicioDiagSupDreta, posicioDiagSupEsq, posicioDiagInfDreta, posicioDiagInfEsq] p
   | x == Reina =   llista7 [posicioUp, posicioDown, posicioLeft, posicioRight] p ++
                    llista7 [posicioDiagSupDreta, posicioDiagSupEsq, posicioDiagInfDreta, posicioDiagInfEsq] p
   | x == Rei = [posicioUp p, posicioDown p, posicioLeft p, posicioRight p, 
                 posicioDiagSupDreta p, posicioDiagSupEsq p, posicioDiagInfDreta p, posicioDiagInfEsq p]
   | x == Cavall = [sumaCoords p 1 2, sumaCoords p 2 1, sumaCoords p 2 (-1), sumaCoords p 1 (-2), 
                    sumaCoords p (-1) 2, sumaCoords p (-2) 1, sumaCoords p (-2) (-1), sumaCoords p (-1) (-2)]                 
   | otherwise = []

moviment :: Peca -> Posicio -> [Posicio]
moviment (Pec tipus _) p = filter (posicioValida) (generarMoviments tipus p)


--alguEntre :: Tauler -> Posicio -> Posicio -> Bool
--alguEntre t p q = False

--fesJugada :: Tauler -> Jugada -> Tauler
--fesJugada t j = t

--escac :: Tauler -> Color -> Bool
--escac t c = False

--jugadaLegal :: Tauler -> Jugada -> Bool
--jugadaLegal t j = False

--escacMat :: Tauler -> Color -> Bool
--escacMat t c = False