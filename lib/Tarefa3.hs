{-|
Module      : Tarefa3
Description : Passagem do tempo.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2026\/27.
-}
module Tarefa3 where

import Labs2026
import Tarefa0_2026

-- | Função principal da Tarefa 3. Avança o jogo um 'Ticks'.
avancaTempo :: Jogo -> Jogo
avancaTempo j
  | perdeuJogo j = j
  | otherwise = gravidade (avancaObjetos j)

-- * Fim do jogo

semVida :: Escavadora -> Bool
semVida escavadora = vidaEscavadora escavadora == 0

faltaCombustivel :: Escavadora -> Bool
faltaCombustivel escavadora = combustivelEscavadora escavadora < custoMovimento

podeAbastecer :: Jogo -> Bool
podeAbastecer jogo = edificioEm(posicaoEscavadora(escavadoraJogo jogo))jogo == Just Posto
                    && creditosJogo jogo >=((custoMovimento - combustivelEscavadora(escavadoraJogo jogo))* precoCombustivel) 

{- | Se o jogo deve terminar ou não.
-}
perdeuJogo :: Jogo -> Bool
perdeuJogo j = semVida (escavadoraJogo j)
              || faltaCombustivel (escavadoraJogo j)
              && not (podeAbastecer j)

            

-- * Objetos

{-| Avança todos os objetos.
-}
avancaObjetos :: Jogo -> Jogo
avancaObjetos j = undefined

-- * Gravidade

{-| A gravidade num 'Ticks'.
-}
gravidade :: Jogo -> Jogo
gravidade j = undefined
