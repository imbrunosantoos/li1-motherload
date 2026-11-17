{-|
Module      : Tempo
Description : Passagem do tempo no Gloss.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para alterar o estado do jogo com a passagem do tempo.
-}
module Tempo where

import Motherload

-- | Tempo em segundos.
type Segundos = Float

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Motherload -> Motherload
reageTempo _ e = e
