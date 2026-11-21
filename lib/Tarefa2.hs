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
efetuaJogada LargaDinamite jogo = largaDinamite jogo
efetuaJogada Vende         jogo = vende jogo
efetuaJogada Abastece      jogo = abastece jogo
efetuaJogada _ jogo = jogo


-- * Funções auxiliares

desligaVoo :: Escavadora -> Escavadora
desligaVoo escavadora = escavadora { voandoEscavadora = False }

parado :: Jogo -> Jogo
parado jogo = atualizaEscavadora desligaVoo jogo

gastaDinamite :: Escavadora -> Escavadora
gastaDinamite escavadora = escavadora { dinamitesEscavadora  = dinamitesEscavadora escavadora - 1 } 

largaDinamite :: Jogo -> Jogo
largaDinamite jogo 
            | dinamitesEscavadora (escavadoraJogo jogo) <= 0 = jogo
            | otherwise = jogoNovo {objetosJogo = novo:objetosJogo jogo}
    
    where jogoNovo = atualizaEscavadora gastaDinamite jogo
          novo = Dinamite (posicaoEscavadora(escavadoraJogo jogo)) duracaoDinamite

estaEm :: TipoEdificio -> Jogo -> Bool
estaEm edificio jogo =  edificioEm (posicaoEscavadora(escavadoraJogo jogo)) jogo == Just edificio 

esvaziaCarga :: Escavadora -> Escavadora 
esvaziaCarga escavadora = escavadora {cargaEscavadora = []}

vende :: Jogo -> Jogo
vende jogo 
    | not (estaEm Refinaria jogo) = jogo
    | otherwise = jogoNovo {creditosJogo = valorCarga(escavadoraJogo jogo) + creditosJogo jogo}

  where jogoNovo = atualizaEscavadora esvaziaCarga jogo  

somaCombustivel :: Int -> Escavadora -> Escavadora
somaCombustivel unidades escavadora = escavadora { combustivelEscavadora = combustivelEscavadora escavadora + unidades }

faltaCombustivel :: Escavadora -> Int
faltaCombustivel escavadora = depositoEscavadora escavadora - combustivelEscavadora escavadora

unidadesAbastecer :: Jogo -> Int
unidadesAbastecer jogo = min (faltaCombustivel (escavadoraJogo jogo)) (creditosJogo jogo `div` precoCombustivel)

pagaCreditos :: Int -> Jogo -> Jogo
pagaCreditos preco jogo = jogo { creditosJogo = creditosJogo jogo - preco }

abastece :: Jogo -> Jogo
abastece jogo
    | not (estaEm Posto jogo) = jogo
    | otherwise = pagaCreditos (unidades * precoCombustivel) (atualizaEscavadora (somaCombustivel unidades) jogo)
  where unidades = unidadesAbastecer jogo