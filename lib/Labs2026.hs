{-|
Module      : Labs2026
Description : Definições base do jogo.
Copyright   : Henrique Faria <d14410@di.uminho.pt>
              Emanuel Rodrigues <d14960@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Sérgio Alves <sfa@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1\/LP1 em 2026\/27.

O projeto é uma versão simplificada do jogo /Motherload/: uma escavadora explora
um mundo visto de lado, recolhe minérios, e vende-os à superfície.
-}
module Labs2026
  ( -- * Tipos de dados

    -- ** Básicos
    Combustivel
  , Creditos
  , Dimensao
  , Direcao (..)
  , Nivel
  , Peso
  , Posicao
  , Semente
  , Ticks

    -- ** Mapas
  , Matriz
  , Mapa
  , Bloco (..)
  , TipoMinerio (..)

    -- ** Entidades
  , Escavadora (..)
  , Edificio (..)
  , TipoEdificio (..)
  , Objeto (..)
  , Melhoria (..)
  , Loja

    -- ** Jogo
  , Jogo (..)
  , Jogada (..)

    -- * Constantes do jogo

    -- ** Minérios
  , valorMinerio
  , pesoMinerio

    -- ** Escavação e movimento
  , durezaBloco
  , custoEscavacao
  , custoMovimento
  , custoVoo

    -- ** Dano
  , danoLava
  , alturaQuedaSegura
  , danoQueda

    -- ** Edifícios
  , precoCombustivel
  , precoReparacao

    -- ** Dinamite
  , duracaoDinamite
  , raioDinamite
  , danoDinamite

    -- * Funções auxiliares
  , geraAleatorios
  ) where

import Data.Maybe (fromMaybe)
import System.Random (mkStdGen, randoms)

import Tarefa0_geral (Dimensao, Direcao (..), Matriz, Posicao)

-- * Tipos de dados

-- ** Básicos

-- | Unidade de tempo do jogo. Por tick a escavadora desloca-se, no máximo, um bloco.
type Ticks = Int

-- | Moeda do jogo.
type Creditos = Int

-- | Unidades de combustível.
type Combustivel = Int

-- | Peso de minérios na carga.
type Peso = Int

-- | Nível da broca \/ dureza de um bloco.
type Nivel = Int

-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

-- ** Mapas

-- | Minérios, por ordem crescente de valor.
data TipoMinerio = Ferro | Bronze | Prata | Ouro | Platina | Diamante
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Bloco do mundo.
data Bloco
  = -- | Espaço livre (céu ou túnel). A escavadora só ocupa blocos de ar.
    Ar
  | -- | Escavável, sem valor.
    Terra
  | -- | Inescavável.
    Rocha
  | -- | Escavável; ao ser escavado o minério passa para a carga da escavadora.
    Minerio TipoMinerio
  | -- | Escavável, mas causa 'danoLava' à escavadora quando escavado.
    Lava
  deriving (Eq, Ord, Show, Read)

-- | Mapa retangular de blocos, ou seja, uma 'Matriz' de 'Bloco's.
-- A linha 0 é a superfície e é toda 'Ar'.
type Mapa = Matriz Bloco

-- ** Entidades

-- | Escavadora controlada pelo jogador.
data Escavadora = Escavadora
  { -- | Posição no mapa (bloco de 'Ar').
    posicaoEscavadora :: Posicao
  , -- | Direção do último movimento bem sucedido (para desenhar).
    direcaoEscavadora :: Direcao
  , -- | Combustível atual, entre 0 e 'depositoEscavadora'.
    combustivelEscavadora :: Combustivel
  , -- | Capacidade do depósito.
    depositoEscavadora :: Combustivel
  , -- | Vida atual, entre 0 e 'vidaMaximaEscavadora'.
    vidaEscavadora :: Int
  , -- | Vida máxima.
    vidaMaximaEscavadora :: Int
  , -- | Dureza máxima de bloco que a broca consegue escavar.
    brocaEscavadora :: Nivel
  , -- | Minérios transportados.
    cargaEscavadora :: [TipoMinerio]
  , -- | Peso máximo transportável.
    capacidadeEscavadora :: Peso
  , -- | Blocos caídos consecutivamente (para dano de queda).
    quedaEscavadora :: Int
  , -- | Propulsores ligados (suspendem a gravidade).
    voandoEscavadora :: Bool
  , -- | Dinamites no inventário.
    dinamitesEscavadora :: Int
  }
  deriving (Eq, Ord, Show, Read)

-- | Tipo de edifício à superfície.
data TipoEdificio
  = -- | Abastecer combustível.
    Posto
  | -- | Vender a carga.
    Refinaria
  | -- | Reparar a escavadora e comprar melhorias.
    Oficina
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Edifício à superfície (linha 0).
data Edificio = Edificio
  { -- | Que serviço o edifício presta.
    tipoEdificio :: TipoEdificio
  , -- | Posição do edifício, sempre na linha 0.
    posicaoEdificio :: Posicao
  }
  deriving (Eq, Ord, Show, Read)

-- | Objeto colocado no mapa.
data Objeto = Dinamite
  { -- | Posição da dinamite.
    posicaoDinamite :: Posicao
  , -- | Ticks até explodir. Em cada tick o temporizador é decrementado e a dinamite explode
    -- quando chega a 0.
    tempoDinamite :: Ticks
  }
  deriving (Eq, Ord, Show, Read)

-- | Melhoria comprável na 'Oficina'. Os valores são absolutos (a nova capacidade), exceto
-- 'Dinamites', que é somada ao inventário.
data Melhoria
  = -- | Novo 'depositoEscavadora'.
    Deposito Combustivel
  | -- | Novo 'vidaMaximaEscavadora'.
    Vida Int
  | -- | Novo 'brocaEscavadora'.
    Broca Nivel
  | -- | Nova 'capacidadeEscavadora'.
    Carga Peso
  | -- | Acrescenta dinamites ao inventário.
    Dinamites Int
  deriving (Eq, Ord, Show, Read)

-- | Loja de melhorias com o respetivo preço. O stock é infinito: comprar não altera a loja.
type Loja = [(Creditos, Melhoria)]

-- ** Jogo

-- | Estado completo do jogo.
data Jogo = Jogo
  { -- | O mapa atual.
    mapaJogo :: Mapa
  , -- | A escavadora do jogador.
    escavadoraJogo :: Escavadora
  , -- | Edifícios à superfície.
    edificiosJogo :: [Edificio]
  , -- | Objetos no mapa.
    objetosJogo :: [Objeto]
  , -- | Créditos do jogador.
    creditosJogo :: Creditos
  , -- | Melhorias à venda.
    lojaJogo :: Loja
  }
  deriving (Eq, Ord, Show, Read)

-- | Ações do jogador.
data Jogada
  = -- | Ausência de ação que desliga propulsores ativos.
    Parado
  | -- | Deslocar-se ('Ar'), voar ('Cima') ou escavar (bloco escavável, só quando apoiado).
    Move Direcao
  | -- | Na 'Refinaria': vende toda a carga.
    Vende
  | -- | No 'Posto': enche o depósito, limitado pelos créditos.
    Abastece
  | -- | Na 'Oficina': repara a escavadora, limitado pelos créditos.
    Repara
  | -- | Na 'Oficina': compra a melhoria com este índice na 'Loja'.
    Compra Int
  | -- | Coloca uma dinamite na posição atual.
    LargaDinamite
  deriving (Eq, Ord, Show, Read)

-- * Constantes do jogo

-- ** Minérios

-- | Valor de venda de uma unidade de cada minério, na 'Refinaria'.
--
-- Cada minério vale cerca de duas vezes e meia o anterior, e o mesmo salto separa cada nível da
-- broca: o valor de uma viagem cresce com a profundidade a que se consegue chegar.
--
-- +------------+-------+
-- | Minério    | Valor |
-- +============+=======+
-- | 'Ferro'    |    25 |
-- +------------+-------+
-- | 'Bronze'   |    60 |
-- +------------+-------+
-- | 'Prata'    |   150 |
-- +------------+-------+
-- | 'Ouro'     |   400 |
-- +------------+-------+
-- | 'Platina'  |  1000 |
-- +------------+-------+
-- | 'Diamante' |  2500 |
-- +------------+-------+
valorMinerio :: TipoMinerio -> Creditos
valorMinerio Ferro = 25
valorMinerio Bronze = 60
valorMinerio Prata = 150
valorMinerio Ouro = 400
valorMinerio Platina = 1000
valorMinerio Diamante = 2500

-- | Peso de uma unidade de cada minério na carga da escavadora.
--
-- __NB:__ o 'Diamante' é o mais valioso /e/ o mais leve; a 'Platina' obriga a escolher.
pesoMinerio :: TipoMinerio -> Peso
pesoMinerio Ferro = 2
pesoMinerio Bronze = 2
pesoMinerio Prata = 3
pesoMinerio Ouro = 4
pesoMinerio Platina = 5
pesoMinerio Diamante = 1

-- ** Escavação e movimento

-- | Dureza de um bloco, ou seja, o nível mínimo de 'brocaEscavadora' que o consegue escavar.
--
-- Devolve @Nothing@ para os blocos que não são escaváveis: 'Ar' (não há nada para escavar) e
-- 'Rocha' (inescavável a qualquer nível).
durezaBloco :: Bloco -> Maybe Nivel
durezaBloco Ar = Nothing
durezaBloco Rocha = Nothing
durezaBloco Terra = Just 1
durezaBloco Lava = Just 1
durezaBloco (Minerio Ferro) = Just 1
durezaBloco (Minerio Bronze) = Just 1
durezaBloco (Minerio Prata) = Just 2
durezaBloco (Minerio Ouro) = Just 2
durezaBloco (Minerio Platina) = Just 3
durezaBloco (Minerio Diamante) = Just 3

-- | Combustível gasto a escavar um bloco: a sua 'durezaBloco'.
--
-- >>> custoEscavacao Terra
-- 1
-- >>> custoEscavacao Rocha
-- 0
custoEscavacao :: Bloco -> Combustivel
custoEscavacao b = fromMaybe 0 (durezaBloco b)

-- | Combustível gasto a deslocar-se um bloco na horizontal.
custoMovimento :: Combustivel
custoMovimento = 1

-- | Combustível gasto a subir um bloco com os propulsores.
custoVoo :: Combustivel
custoVoo = 2

-- ** Dano

-- | Vida perdida ao escavar um bloco de 'Lava'.
danoLava :: Int
danoLava = 30

-- | Número de blocos que a escavadora pode cair sem sofrer dano.
alturaQuedaSegura :: Int
alturaQuedaSegura = 2

-- | Vida perdida por cada bloco caído acima da 'alturaQuedaSegura'.
danoQueda :: Int
danoQueda = 10

-- ** Edifícios

-- | Créditos por unidade de combustível, no 'Posto'.
precoCombustivel :: Creditos
precoCombustivel = 1

-- | Créditos por ponto de vida reparado, na 'Oficina'.
precoReparacao :: Creditos
precoReparacao = 2

-- ** Dinamite

-- | Ticks entre largar uma dinamite e a sua explosão.
duracaoDinamite :: Ticks
duracaoDinamite = 3

-- | Raio da explosão de uma dinamite, em blocos (distância de Chebyshev).
raioDinamite :: Int
raioDinamite = 1

-- | Vida perdida numa explosão que apanhe a escavadora.
danoDinamite :: Int
danoDinamite = 40

-- * Funções auxiliares

{-| Função que gera uma lista de números aleatórios a partir de uma 'Semente'.

== Exemplos

>>> geraAleatorios 2627 3
[7884690122452951765,-2301222553746467507,2230926884526872602]
-}
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ randoms (mkStdGen s)
