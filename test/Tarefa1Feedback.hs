{-|
Module      : Main
Description : Testes e feedback da Tarefa 1.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 1. Correr com @cabal run t1-feedback@: corre a 'validaJogo' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa1

-- * Jogos base

-- | Mapa exemplo do enunciado- 7 linhas e 13 colunas
mapaExemplo :: Mapa
mapaExemplo =
  [ replicate 13 Ar
  , [Terra, Terra, Rocha, Ar, Terra, Terra, Rocha, Terra, Terra, Terra, Rocha, Terra, Terra]
  , [Terra, Minerio Ferro, Terra, Ar, Terra, Minerio Prata, Terra, Terra, Lava, Terra, Terra, Minerio Bronze, Terra]
  , [Terra, Terra, Terra, Ar, Terra, Minerio Ouro, Rocha, Terra, Terra, Minerio Platina, Terra, Terra, Terra]
  , [Terra, Minerio Diamante, Terra, Ar, Terra, Lava, Terra, Terra, Rocha, Terra, Terra, Minerio Ferro, Terra]
  , [Rocha, Terra, Terra, Terra, Ar, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Rocha]
  , [Minerio Ouro, Terra, Terra, Rocha, Terra, Terra, Terra, Terra, Minerio Platina, Terra, Terra, Rocha, Terra]
  ]

-- | Escavadora do exemplo do enunciado, em (4,3), com o depósito e a vida cheios e a carga vazia
escavadoraExemplo :: Escavadora
escavadoraExemplo = Escavadora
  { posicaoEscavadora = (4,3), direcaoEscavadora = Baixo
  , combustivelEscavadora = 200, depositoEscavadora = 200
  , vidaEscavadora = 100, vidaMaximaEscavadora = 100
  , brocaEscavadora = 1, cargaEscavadora = [], capacidadeEscavadora = 20
  , quedaEscavadora = 0, voandoEscavadora = False, dinamitesEscavadora = 0
  }

-- | Jogo do exemplo do enunciado-É válido
jogoExemplo :: Jogo
jogoExemplo = Jogo
  { mapaJogo = mapaExemplo
  , escavadoraJogo = escavadoraExemplo
  , edificiosJogo = [Edificio Posto (0,2), Edificio Refinaria (0,6), Edificio Oficina (0,10)]
  , objetosJogo = []
  , creditosJogo = 0
  , lojaJogo = [ (250, Dinamites 3), (500, Carga 40), (800, Deposito 400), (1200, Vida 200)
               , (1500, Broca 2), (3000, Carga 80), (3500, Deposito 800), (4000, Vida 400)
               , (5000, Broca 3) ]
  }

-- | Mapa pequeno (4x4), para testes focados em um detalhe
mapaPequeno :: Mapa
mapaPequeno =
  [ [Ar,    Ar,    Ar,           Ar   ]
  , [Terra, Ar,    Rocha,        Terra]
  , [Terra, Ar,    Minerio Ouro, Lava ]
  , [Rocha, Terra, Terra,        Terra]
  ]

-- * Testes

-- | Válido- combustível exatamente 0 
semCombustivel :: Jogo
semCombustivel = jogoExemplo { escavadoraJogo = escavadoraExemplo { combustivelEscavadora = 0 } }

-- | Inválido- a escavadora está num bloco de Terra
escavadoraEmTerra :: Jogo
escavadoraEmTerra = jogoExemplo { escavadoraJogo = escavadoraExemplo { posicaoEscavadora = (5,3) } }

-- | Inválido- créditos negativos 
creditosNegativos :: Jogo
creditosNegativos = jogoExemplo { creditosJogo = -1 }

-- | Inválido- há Terra na superfície, em (0,2) 
superficieComTerra :: Jogo
superficieComTerra = jogoPequeno
  { mapaJogo = [ [Ar,    Ar,    Terra,        Ar   ]    
               , [Terra, Ar,    Rocha,        Terra]
               , [Terra, Ar,    Minerio Ouro, Lava ]
               , [Rocha, Terra, Terra,        Terra]
               ]
  }

-- | Jogo válido no mapa pequeno-a escavadora está em (2,1), pousada em terra
jogoPequeno :: Jogo
jogoPequeno = jogoExemplo
  { mapaJogo = mapaPequeno
  , escavadoraJogo = escavadoraExemplo { posicaoEscavadora = (2,1) }
  , edificiosJogo = [Edificio Posto (0,0), Edificio Refinaria (0,1), Edificio Oficina (0,3)]
  }

-- | Os testes da Tarefa 1, que são avaliados pelos docentes.
testesTarefa1 :: [Jogo]
testesTarefa1 = [jogoExemplo, jogoPequeno, semCombustivel,
                escavadoraEmTerra, creditosNegativos, superficieComTerra]

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . validaJogo) testesTarefa1
  runFeedback (T1 testesTarefa1 resultados)
