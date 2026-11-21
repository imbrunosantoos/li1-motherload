{-|
Module      : Tarefa2
Description : Reação a jogadas.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2026\/27.
-}
module Tarefa2 where

import Labs2026
import Tarefa0_geral
import Tarefa0_2026

-- | Função principal da Tarefa 2. Aplica uma jogada a um jogo.
--
-- 'Parado' desliga os propulsores. É a única jogada que o faz: só um @Move Cima@ os liga.
efetuaJogada :: Jogada -> Jogo -> Jogo
efetuaJogada Parado        jogo = parado jogo
efetuaJogada _ jogo = jogo


-- * Funções auxiliares

desligaVoo :: Escavadora -> Escavadora
desligaVoo escavadora = escavadora { voandoEscavadora = False }

parado :: Jogo -> Jogo
parado jogo = atualizaEscavadora desligaVoo jogo