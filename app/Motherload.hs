{-|
Module      : Motherload
Description : Estado do jogo no Gloss.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

O estado que o Gloss faz circular entre o 'Desenhar.desenha', o 'Eventos.reageEventos' e o
'Tempo.reageTempo'.
-}
module Motherload where

-- | Estado do jogo no Gloss.
data Motherload = Motherload {}

-- | Estado inicial do jogo.
estadoInicial :: Motherload
estadoInicial = Motherload {}
