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

-- | Verifica se a escavadora ficou sem vida, ou seja, se a vida chegou a 0
--
-- Como a 'sofreDano' nunca deixa a vida ficar negativa, basta comparar com 0
semVida :: Escavadora -> Bool
semVida escavadora = vidaEscavadora escavadora == 0

-- | Verifica se o combustível da escavadora já não chega para um movimento,
-- ou seja, se é menor do que o 'custoMovimento'
faltaCombustivel :: Escavadora -> Bool
faltaCombustivel escavadora = combustivelEscavadora escavadora < custoMovimento

-- | Verifica se a escavadora pode abastecer onde está- tem de estar no 'Posto',
-- e os créditos tem de chegar para pagar o combustível que falta até ao
-- 'custoMovimento', ao preço de 'precoCombustivel' por unidade
--OBS: só faz sentido quando falta combustível, com o depósito cheio, a conta
-- do que falta dá negativa e a condição dos créditos é sempre verdadeira
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
