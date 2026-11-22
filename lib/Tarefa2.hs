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
efetuaJogada Repara        jogo = repara jogo
efetuaJogada (Compra i)    jogo = compra i jogo
efetuaJogada (Move _)      jogo = jogo

-- * Funções auxiliares

-- ** Parado

-- | Tira o voo da escavadora

desligaVoo :: Escavadora -> Escavadora
desligaVoo escavadora = escavadora { voandoEscavadora = False }

-- | Jogada 'Parado': tira o voo, Se a escavadora não estiver a voar, nada muda
parado :: Jogo -> Jogo
parado jogo = atualizaEscavadora desligaVoo jogo

-- ** LargaDinamite

-- | Tira uma dinamite do inventário da escavadora
gastaDinamite :: Escavadora -> Escavadora
gastaDinamite escavadora = escavadora { dinamitesEscavadora  = dinamitesEscavadora escavadora - 1 } 

-- | Jogada 'LargaDinamite': tira uma dinamite do inventário e mete no mapa, na posição da
-- escavadora
largaDinamite :: Jogo -> Jogo
largaDinamite jogo 
            | dinamitesEscavadora (escavadoraJogo jogo) <= 0 = jogo
            | otherwise = jogoNovo {objetosJogo = novo:objetosJogo jogo}
    
    where jogoNovo = atualizaEscavadora gastaDinamite jogo
          novo = Dinamite (posicaoEscavadora(escavadoraJogo jogo)) duracaoDinamite

-- ** Edifícios

-- | Verifica se a escavadora está num edifício do tipo dado
estaEm :: TipoEdificio -> Jogo -> Bool
estaEm edificio jogo =  edificioEm (posicaoEscavadora(escavadoraJogo jogo)) jogo == Just edificio 

-- | Tira dinheiro aos créditos do jogo.
pagaCreditos :: Int -> Jogo -> Jogo
pagaCreditos preco jogo = jogo { creditosJogo = creditosJogo jogo - preco }

-- *** Vende

-- | Deixa a carga da escavadora vazia
esvaziaCarga :: Escavadora -> Escavadora 
esvaziaCarga escavadora = escavadora {cargaEscavadora = []}

-- | Jogada 'Vende': na 'Refinaria', os créditos sobe o valor da carga e a carga fica vazia
vende :: Jogo -> Jogo
vende jogo 
    | not (estaEm Refinaria jogo) = jogo
    | otherwise = jogoNovo {creditosJogo = valorCarga(escavadoraJogo jogo) + creditosJogo jogo}

  where jogoNovo = atualizaEscavadora esvaziaCarga jogo  

-- *** Abastece

-- | Soma unidades ao combustível da escavadora
somaCombustivel :: Int -> Escavadora -> Escavadora
somaCombustivel unidades escavadora = escavadora { combustivelEscavadora = combustivelEscavadora escavadora + unidades }

-- | Quanto combustível falta para encher o depósito
faltaCombustivel :: Escavadora -> Int
faltaCombustivel escavadora = depositoEscavadora escavadora - combustivelEscavadora escavadora

-- | Quantas unidades de combustível se metem: o que falta para encher o depósito, limitado pelo
-- que os créditos conseguem pagar
unidadesAbastecer :: Jogo -> Int
unidadesAbastecer jogo = min (faltaCombustivel (escavadoraJogo jogo)) (creditosJogo jogo `div` precoCombustivel)

-- | Jogada 'Abastece': no posto mete o combustível que falta (ou o que os créditos pagam) e
-- paga o preco do combustivel por unidade
abastece :: Jogo -> Jogo
abastece jogo
    | not (estaEm Posto jogo) = jogo
    | otherwise = pagaCreditos (unidades * precoCombustivel) (atualizaEscavadora (somaCombustivel unidades) jogo)
  where unidades = unidadesAbastecer jogo

-- *** Repara

-- | Soma pontos à vida da escavadora
somaVida :: Int -> Escavadora -> Escavadora
somaVida vida escavadora = escavadora {vidaEscavadora = vidaEscavadora escavadora + vida}

-- | Quanta vida falta para chegar à vida máxima
faltaVida :: Escavadora -> Int
faltaVida escavadora = vidaMaximaEscavadora escavadora - vidaEscavadora escavadora

-- | Quantos pontos de vida se reparam: a vida que falta, limitada pelo que os créditos
-- conseguem pagar
unidadesReparar :: Jogo -> Int
unidadesReparar jogo = min(faltaVida (escavadoraJogo jogo)) (creditosJogo jogo `div` precoReparacao)

-- | Jogada 'Repara': na oficina, repara a vida que falta (ou o que os créditos pagam) e
-- paga o preço reparação por ponto
repara :: Jogo -> Jogo
repara jogo
    | not (estaEm Oficina jogo) = jogo
    | otherwise = pagaCreditos (vida * precoReparacao) (atualizaEscavadora(somaVida vida) jogo)

  where vida = unidadesReparar jogo

-- *** Compra

-- | Verifica se uma melhoria é maior do que o que a escavadora já tem
eMelhoria :: Melhoria -> Escavadora -> Bool
eMelhoria (Deposito d) escavadora = d > depositoEscavadora escavadora 
eMelhoria (Vida v)     escavadora = v > vidaMaximaEscavadora escavadora 
eMelhoria (Broca n)    escavadora = n > brocaEscavadora escavadora 
eMelhoria (Carga c)    escavadora = c > capacidadeEscavadora escavadora 
eMelhoria (Dinamites _) _         = True

-- | Aplica uma melhoria à escavadora: substitui o valor, exceto 'Dinamites', que se soma
aplicaMelhoria :: Melhoria -> Escavadora -> Escavadora
aplicaMelhoria (Deposito d)  escavadora = escavadora {depositoEscavadora = d}
aplicaMelhoria (Vida v)      escavadora = escavadora {vidaMaximaEscavadora = v}
aplicaMelhoria (Broca n)     escavadora = escavadora {brocaEscavadora = n}
aplicaMelhoria (Carga c)     escavadora = escavadora {capacidadeEscavadora = c}
aplicaMelhoria (Dinamites d) escavadora = escavadora {dinamitesEscavadora = dinamitesEscavadora escavadora + d}

-- | Compra um item da loja, se houver créditos para o preço e se for mesmo uma melhoria
compraMelhoria :: (Creditos, Melhoria) -> Jogo -> Jogo
compraMelhoria (preco, melhoria) jogo 
              | creditosJogo jogo < preco = jogo
              | not (eMelhoria melhoria (escavadoraJogo jogo)) = jogo
              | otherwise = pagaCreditos preco (atualizaEscavadora (aplicaMelhoria melhoria) jogo)

-- | Jogada 'Compra': na oficina, compra a melhoria com esse índice da loja - conta a partir de 0
compra :: Int -> Jogo -> Jogo
compra numero jogo 
     | not (estaEm Oficina jogo) = jogo
     | otherwise = case encontraIndiceLista numero (lojaJogo jogo) of
        Nothing   -> jogo
        Just item -> compraMelhoria item jogo

-- ** Move

-- | Desloca a escavadora um bloco na direção dada: muda a posição e a direção, e gasta o custo
-- em combustível
deslocaEscavadora :: Direcao -> Int -> Escavadora -> Escavadora
deslocaEscavadora d blocos escavadora = escavadora {posicaoEscavadora = movePosicao d (posicaoEscavadora escavadora),
                                                    direcaoEscavadora = d,
                                                    combustivelEscavadora = combustivelEscavadora escavadora - blocos}

-- | Move a escavadora para um bloco de ar, gastando o custo de movimento
anda :: Direcao -> Jogo -> Jogo
anda d jogo 
    | combustivelEscavadora(escavadoraJogo jogo) < custoMovimento  = jogo
    | otherwise = atualizaEscavadora (deslocaEscavadora d custoMovimento ) jogo

