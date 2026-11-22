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
efetuaJogada (Move d)      jogo = move d jogo

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

-- | Ativa voo da escavadora e põe a queda a 0
ligaVoo :: Escavadora -> Escavadora 
ligaVoo escavadora = escavadora {voandoEscavadora = True,
                                 quedaEscavadora = 0}

-- | Sobe a escavadora um bloco com o 'ligaVoo' , gastando e gasta o 'custoVoo'
voa :: Jogo -> Jogo
voa jogo 
   | combustivelEscavadora (escavadoraJogo jogo) < custoVoo = jogo
   | otherwise = atualizaEscavadora ligaVoo (atualizaEscavadora (deslocaEscavadora Cima custoVoo) jogo)

-- | O efeito do bloco escavado na escavadora: a 'Lava' tira 'danoLava' de vida, o minério entra
-- na carga se couber (senão perde-se) e a 'Terra' não faz nada
recolhe :: Bloco -> Escavadora -> Escavadora
recolhe Lava escavadora = sofreDano danoLava escavadora
recolhe (Minerio m) escavadora 
        |  (capacidadeEscavadora escavadora >= pesoCarga escavadora + pesoMinerio m) = escavadora {cargaEscavadora = m:cargaEscavadora escavadora}
        | otherwise = escavadora
recolhe _ escavadora = escavadora

-- | Escava o bloco na direção dada: gasta 'custoEscavacao', sofre o efeito do bloco ('recolhe'),
-- o bloco passa a 'Ar' e a escavadora move-se para lá
escava :: Direcao -> Bloco -> Jogo -> Jogo
escava d bloco jogo 
      | combustivelEscavadora (escavadoraJogo jogo) < custoEscavacao bloco = jogo
      | otherwise = jogoNovo { mapaJogo = atualizaBloco destino Ar (mapaJogo jogo)}

  where jogoNovo = atualizaEscavadora (recolhe bloco) (atualizaEscavadora (deslocaEscavadora d (custoEscavacao bloco)) jogo)
        destino = movePosicao d (posicaoEscavadora (escavadoraJogo jogo))

-- | Verifica se a broca da escavadora consegue escavar o bloco: o bloco tem de ter dureza e a
-- dureza não pode ser maior do que a broca, a rocha e o ar não se escavam
consegueEscavar :: Bloco -> Escavadora -> Bool
consegueEscavar bloco escavadora = case durezaBloco bloco of
    Nothing -> False
    Just n -> n <= brocaEscavadora escavadora 

-- | Decide o que fazer conforme o bloco de destino: voa para ar em cima, anda para ar ao lado,
-- escava se estiver apoiada e se a broca chegar, em todos os outros casos, o jogo fica igual
moveParaBloco :: Direcao -> Bloco -> Jogo -> Jogo
moveParaBloco d bloco jogo 
            | d == Cima && bloco == Ar = voa jogo
            | d == Cima = jogo
            | bloco == Ar && d /= Baixo = anda d jogo
            | apoiado jogo && consegueEscavar bloco (escavadoraJogo jogo) = escava d bloco jogo
            | otherwise = jogo

-- | Jogada 'Move': vê qual é o bloco de destino e decide com 'moveParaBloco'
-- se o destino tiver fora do mapa, o jogo fica igual
move :: Direcao -> Jogo -> Jogo
move d jogo = case blocoEm destino (mapaJogo jogo) of
    Nothing -> jogo
    Just bloco -> moveParaBloco d bloco jogo
    where destino = movePosicao d (posicaoEscavadora (escavadoraJogo jogo))
