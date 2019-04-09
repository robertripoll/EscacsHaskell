-- CLASSES:
-- 		Peça
-- 		Tauler
-- 		Partida
-- 		Jugada
--		Posició?

moviment :: Peca -> Posicio -> [Posicions]
moviment x p = []

alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre t p q = False

fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t j = t

escac :: Tauler -> Color -> Bool
escac t c = False

jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t j = False

escacMat :: Tauler -> Color -> Bool
escacMat t c = False