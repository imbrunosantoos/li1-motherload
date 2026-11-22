{-|
Module      : Main
Description : Testes e feedback da Tarefa 2.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 2. Correr com @cabal run t2-feedback@: corre a 'efetuaJogada' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa2

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

-- | O jogo do exemplo, com outra escavadora
comEscavadora :: Escavadora -> Jogo
comEscavadora esc = jogoExemplo { escavadoraJogo = esc }

-- * Testes do Parado

-- | 'Parado' a voar: os propulsores desligam-se.
paradoAVoar :: (Jogada, Jogo)
paradoAVoar = (Parado, comEscavadora escavadoraExemplo { posicaoEscavadora = (3,3), voandoEscavadora = True })

-- | 'Parado' sem voar: nada muda.
paradoSemVoar :: (Jogada, Jogo)
paradoSemVoar = (Parado, jogoExemplo)

-- * Testes da LargaDinamite

-- | Larga uma dinamite com 2 no inventário.
largaComDinamites :: (Jogada, Jogo)
largaComDinamites = (LargaDinamite, comEscavadora escavadoraExemplo { dinamitesEscavadora = 2 })

-- | Sem dinamites no inventário: inválida.
largaSemDinamites :: (Jogada, Jogo)
largaSemDinamites = (LargaDinamite, jogoExemplo)

-- | Já há uma dinamite no mapa: a nova junta-se à que lá está.
largaComOutraNoMapa :: (Jogada, Jogo)
largaComOutraNoMapa =
  (LargaDinamite, (comEscavadora escavadoraExemplo { dinamitesEscavadora = 1 }) { objetosJogo = [Dinamite (3,3) 2] })

-- | Larga uma dinamite a voar: também é válido.
largaAVoar :: (Jogada, Jogo)
largaAVoar = (LargaDinamite, comEscavadora escavadoraExemplo { posicaoEscavadora = (3,3), voandoEscavadora = True, dinamitesEscavadora = 1 })

-- | O jogo do exemplo com a escavadora noutra posição e com uma dada quantidade de créditos.
-- Serve para os testes dos edifícios.
naPosicaoComCreditos :: Posicao -> Creditos -> Escavadora -> Jogo
naPosicaoComCreditos p creditos esc =
  jogoExemplo { escavadoraJogo = esc { posicaoEscavadora = p }, creditosJogo = creditos }

-- * Testes dos edifícios

-- | Vende a carga na 'Refinaria': os créditos sobem o valor da carga e a carga fica vazia.
vendeNaRefinaria :: (Jogada, Jogo)
vendeNaRefinaria = (Vende, naPosicaoComCreditos (0,6) 100 escavadoraExemplo { cargaEscavadora = [Ferro, Prata, Diamante] })

-- | Tenta vender no 'Posto': inválida.
vendeForaDaRefinaria :: (Jogada, Jogo)
vendeForaDaRefinaria = (Vende, naPosicaoComCreditos (0,2) 100 escavadoraExemplo { cargaEscavadora = [Ferro, Prata, Diamante] })

-- | Abastece no 'Posto' com créditos de sobra: o depósito fica cheio.
abasteceComCreditos :: (Jogada, Jogo)
abasteceComCreditos = (Abastece, naPosicaoComCreditos (0,2) 500 escavadoraExemplo { combustivelEscavadora = 50 })

-- | Abastece no 'Posto' com poucos créditos: abastece só o que consegue pagar.
abasteceComPoucosCreditos :: (Jogada, Jogo)
abasteceComPoucosCreditos = (Abastece, naPosicaoComCreditos (0,2) 30 escavadoraExemplo { combustivelEscavadora = 50 })

-- | Tenta abastecer na 'Oficina': inválida.
abasteceForaDoPosto :: (Jogada, Jogo)
abasteceForaDoPosto = (Abastece, naPosicaoComCreditos (0,10) 500 escavadoraExemplo { combustivelEscavadora = 50 })

-- | Repara na 'Oficina' com créditos de sobra: a vida fica no máximo.
reparaComCreditos :: (Jogada, Jogo)
reparaComCreditos = (Repara, naPosicaoComCreditos (0,10) 500 escavadoraExemplo { vidaEscavadora = 40 })

-- | Repara com 5 créditos: o preço é 2, por isso repara 2 pontos e sobra 1 crédito.
reparaComPoucosCreditos :: (Jogada, Jogo)
reparaComPoucosCreditos = (Repara, naPosicaoComCreditos (0,10) 5 escavadoraExemplo { vidaEscavadora = 40 })

-- | Tenta reparar na 'Refinaria': inválida.
reparaForaDaOficina :: (Jogada, Jogo)
reparaForaDaOficina = (Repara, naPosicaoComCreditos (0,6) 500 escavadoraExemplo { vidaEscavadora = 40 })


-- | Os testes da Tarefa 2, que são avaliados pelos docentes.
testesTarefa2 :: [(Jogada, Jogo)]
testesTarefa2 = [paradoAVoar, paradoSemVoar, largaComDinamites,
                 largaComOutraNoMapa, largaAVoar, largaSemDinamites,
                 vendeNaRefinaria, vendeForaDaRefinaria, abasteceComCreditos,
                 abasteceComPoucosCreditos, abasteceForaDoPosto, reparaComCreditos,
                 reparaComPoucosCreditos, reparaForaDaOficina]

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . uncurry efetuaJogada) testesTarefa2
  runFeedback (T2 testesTarefa2 resultados)
