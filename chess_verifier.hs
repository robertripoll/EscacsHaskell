-- CLASSES:
--      Peça
--      Tauler
--      Partida
--      Jugada
--      Posició?

data Color = Blanc | Negre

data Peca = Rei | Reina | Torre | Alfil | Cavall | Peo deriving Eq

instance Show Peca where
    show p
        | (p == Rei) = show 'R'
        | (p == Reina) = show 'D'
        | (p == Torre) = show 'T'
        | (p == Alfil) = show 'A'
        | (p == Cavall) = show 'C'
        | otherwise = show 'P'

rei :: Peca
rei = Rei

reina :: Peca
reina = Reina

torre :: Peca
torre = Torre

data Posicio = Char :/ Int

instance Show Posicio where
    show (fila :/ col) = show fila ++ show col      -- Estaria bé que la fila sortís sense les comilles simples 'a'

data Jugada = Jug Peca Posicio Posicio

instance Show Jugada where
    show (Jug p x0 x1) = show p ++ show x0 ++ show x1

unaPosicio :: Posicio
unaPosicio = 'a' :/ 1

unaJugada :: Jugada
unaJugada = Jug Torre ('a':/3) ('b':/2)
--moviment :: Peca -> Posicio -> [Posicions]
--moviment x p = []

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