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

-- | Mapa pequeno (5x5) com todos os tipos de bloco à volta das posições usadas nos testes
mapaPequeno :: Mapa
mapaPequeno =
  [ [Ar,    Ar,              Ar,    Ar,            Ar              ]
  , [Lava,  Ar,              Rocha, Ar,            Minerio Diamante]
  , [Terra, Minerio Platina, Terra, Minerio Prata, Terra           ]
  , [Terra, Ar,              Terra, Ar,            Terra           ]
  , [Rocha, Ar,              Terra, Ar,            Rocha           ]
  ]

-- | Jogo válido no mapa pequeno, com a escavadora dada
noMapaPequeno :: Escavadora -> Jogo
noMapaPequeno esc = jogoExemplo
  { mapaJogo = mapaPequeno
  , escavadoraJogo = esc
  , edificiosJogo = [Edificio Posto (0,0), Edificio Refinaria (0,2), Edificio Oficina (0,4)]
  }

-- | A escavadora do exemplo em (1,1) o mapa pequeno, apoiada em 'Platina'.
escavadoraPequena :: Escavadora
escavadoraPequena = escavadoraExemplo { posicaoEscavadora = (1,1) }

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

-- * Testes da Compra

-- | Compra a melhoria de índice 1 da loja ('Carga' 40, por 500).
compraCarga :: (Jogada, Jogo)
compraCarga = (Compra 1, naPosicaoComCreditos (0,10) 600 escavadoraExemplo)

-- | Compra a melhoria de índice 0 ('Dinamites' 3, por 250) com os créditos certos.
compraDinamitesNoLimite :: (Jogada, Jogo)
compraDinamitesNoLimite = (Compra 0, naPosicaoComCreditos (0,10) 250 escavadoraExemplo { dinamitesEscavadora = 1 })

-- | Índice depois do fim da loja: inválida.
compraIndiceInvalido :: (Jogada, Jogo)
compraIndiceInvalido = (Compra 9, naPosicaoComCreditos (0,10) 10000 escavadoraExemplo)

-- | Índice negativo: inválida.
compraIndiceNegativo :: (Jogada, Jogo)
compraIndiceNegativo = (Compra (-1), naPosicaoComCreditos (0,10) 10000 escavadoraExemplo)

-- | Falta 1 crédito para o 'Deposito' 400: inválida.
compraSemCreditos :: (Jogada, Jogo)
compraSemCreditos = (Compra 2, naPosicaoComCreditos (0,10) 799 escavadoraExemplo)

-- | O depósito já é 400, por isso o 'Deposito' 400 não é melhoria: inválida.
compraQueNaoMelhora :: (Jogada, Jogo)
compraQueNaoMelhora = (Compra 2, naPosicaoComCreditos (0,10) 1000 escavadoraExemplo { depositoEscavadora = 400 })

-- | Tenta comprar na 'Refinaria': inválida.
compraForaDaOficina :: (Jogada, Jogo)
compraForaDaOficina = (Compra 0, naPosicaoComCreditos (0,6) 1000 escavadoraExemplo)

-- | Compra a 'Broca' 2.
compraBroca :: (Jogada, Jogo)
compraBroca = (Compra 4, naPosicaoComCreditos (0,10) 2000 escavadoraExemplo)

-- | Compra a 'Vida' 200: a vida máxima sobe, mas a vida atual não muda.
compraVida :: (Jogada, Jogo)
compraVida = (Compra 3, naPosicaoComCreditos (0,10) 1200 escavadoraExemplo { vidaEscavadora = 50 })

-- * Testes do Move com a escavadora apoiada

-- | Exemplo do enunciado: escava a 'Terra' à direita e move-se para lá.
escavaTerraDireita :: (Jogada, Jogo)
escavaTerraDireita = (Move Direita, jogoExemplo)

-- | Escava a 'Terra' por baixo. A queda não muda.
escavaTerraBaixo :: (Jogada, Jogo)
escavaTerraBaixo = (Move Baixo, jogoExemplo)

-- | Apoiada, voa para o 'Ar' que está por cima.
voaCimaApoiada :: (Jogada, Jogo)
voaCimaApoiada = (Move Cima, jogoExemplo)

-- | Apoiada, anda para o 'Ar' ao lado, à superfície.
andaParaArApoiada :: (Jogada, Jogo)
andaParaArApoiada = (Move Esquerda, comEscavadora escavadoraExemplo { posicaoEscavadora = (0,4) })

-- | Apoiada, tenta voar contra 'Terra': inválida.
cimaContraBloco :: (Jogada, Jogo)
cimaContraBloco = (Move Cima, comEscavadora escavadoraExemplo { posicaoEscavadora = (5,4) })

-- | Escava 'Lava': perde 'danoLava' de vida.
escavaLava :: (Jogada, Jogo)
escavaLava = (Move Esquerda, noMapaPequeno escavadoraPequena)

-- | Escava 'Lava' com menos vida do que o dano: a vida fica em 0 e não negativa.
lavaComPoucaVida :: (Jogada, Jogo)
lavaComPoucaVida = (Move Esquerda, noMapaPequeno escavadoraPequena { vidaEscavadora = 20 })

-- | Tenta escavar 'Rocha': inválida.
rochaInvalida :: (Jogada, Jogo)
rochaInvalida = (Move Direita, noMapaPequeno escavadoraPequena)

-- | Tenta escavar 'Platina' com a broca 1: inválida.
minerioDuroDemais :: (Jogada, Jogo)
minerioDuroDemais = (Move Baixo, noMapaPequeno escavadoraPequena)

-- | Escava 'Platina' com a broca 3 e já com 'Ferro' na carga: a 'Platina' cabe e entra na carga.
minerioQueCabe :: (Jogada, Jogo)
minerioQueCabe = (Move Baixo, noMapaPequeno escavadoraPequena { brocaEscavadora = 3, cargaEscavadora = [Ferro] })

-- | Escava 'Platina' (peso 5) com a carga a pesar 16 de 20: não cabe e perde-se.
minerioQueNaoCabe :: (Jogada, Jogo)
minerioQueNaoCabe =
  (Move Baixo, noMapaPequeno escavadoraPequena { brocaEscavadora = 3, cargaEscavadora = [Ouro, Ouro, Ouro, Ouro] })

-- | Escava 'Platina' (peso 5) com a carga a pesar 15 de 20: fica exatamente cheia.
minerioNoLimite :: (Jogada, Jogo)
minerioNoLimite =
  (Move Baixo, noMapaPequeno escavadoraPequena { brocaEscavadora = 3, cargaEscavadora = [Platina, Platina, Platina] })

-- | Escava 'Diamante' para o lado, com a broca 3.
diamanteParaOLado :: (Jogada, Jogo)
diamanteParaOLado = (Move Direita, noMapaPequeno escavadoraPequena { posicaoEscavadora = (1,3), brocaEscavadora = 3 })

-- | Na última linha, tenta ir para baixo: o destino está fora do mapa.
baixoNaUltimaLinha :: (Jogada, Jogo)
baixoNaUltimaLinha = (Move Baixo, noMapaPequeno escavadoraPequena { posicaoEscavadora = (4,1) })

-- | Na última linha está apoiada, por isso pode escavar para o lado.
escavaNaUltimaLinha :: (Jogada, Jogo)
escavaNaUltimaLinha = (Move Direita, noMapaPequeno escavadoraPequena { posicaoEscavadora = (4,1) })

-- | Na última coluna, tenta ir para a direita: o destino está fora do mapa.
direitaNaUltimaColuna :: (Jogada, Jogo)
direitaNaUltimaColuna = (Move Direita, noMapaPequeno escavadoraPequena { posicaoEscavadora = (0,4) })

-- | À superfície, tenta subir: o destino está fora do mapa.
cimaNaSuperficie :: (Jogada, Jogo)
cimaNaSuperficie = (Move Cima, noMapaPequeno escavadoraPequena { posicaoEscavadora = (0,1) })

-- | A voar mas apoiada: escava para o lado e os propulsores continuam ligados.
escavaAVoar :: (Jogada, Jogo)
escavaAVoar = (Move Direita, comEscavadora escavadoraExemplo { voandoEscavadora = True })

-- * Testes do Move com a escavadora no ar

-- | No ar, tenta ir contra 'Terra': inválida, porque só se escava apoiado.
noArLadoBloco :: (Jogada, Jogo)
noArLadoBloco = (Move Esquerda, comEscavadora escavadoraExemplo { posicaoEscavadora = (3,3) })

-- | No ar, tenta ir para baixo: inválida, quem desce é a gravidade.
noArBaixo :: (Jogada, Jogo)
noArBaixo = (Move Baixo, comEscavadora escavadoraExemplo { posicaoEscavadora = (3,3) })

-- | No ar e a cair, sobe: liga os propulsores e a queda volta a 0.
noArCima :: (Jogada, Jogo)
noArCima = (Move Cima, comEscavadora escavadoraExemplo { posicaoEscavadora = (3,3), quedaEscavadora = 1 })

-- | No ar, à superfície, anda para o 'Ar' ao lado.
noArLadoAr :: (Jogada, Jogo)
noArLadoAr = (Move Direita, comEscavadora escavadoraExemplo { posicaoEscavadora = (0,3) })

-- * Testes do combustível

-- | Sem combustível, não consegue escavar.
semCombustivel :: (Jogada, Jogo)
semCombustivel = (Move Direita, comEscavadora escavadoraExemplo { combustivelEscavadora = 0 })

-- | Com o combustível igual ao 'custoVoo', consegue voar e fica com 0.
combustivelNoLimite :: (Jogada, Jogo)
combustivelNoLimite = (Move Cima, comEscavadora escavadoraExemplo { combustivelEscavadora = 2 })

-- | Com menos combustível do que o 'custoVoo', não consegue voar.
combustivelNaoChegaParaVoar :: (Jogada, Jogo)
combustivelNaoChegaParaVoar = (Move Cima, comEscavadora escavadoraExemplo { combustivelEscavadora = 1 })

-- | Escavar 'Diamante' custa 3: com 2 de combustível é inválida.
combustivelNaoChegaParaDiamante :: (Jogada, Jogo)
combustivelNaoChegaParaDiamante =
  (Move Direita, noMapaPequeno escavadoraPequena { posicaoEscavadora = (1,3), brocaEscavadora = 3, combustivelEscavadora = 2 })

-- | Os testes da Tarefa 2, que são avaliados pelos docentes.
testesTarefa2 :: [(Jogada, Jogo)]
testesTarefa2 = [paradoAVoar, paradoSemVoar, largaComDinamites,
                 largaComOutraNoMapa, largaAVoar, largaSemDinamites,
                 vendeNaRefinaria, vendeForaDaRefinaria, abasteceComCreditos,
                 abasteceComPoucosCreditos, abasteceForaDoPosto, reparaComCreditos,
                 reparaComPoucosCreditos, reparaForaDaOficina, compraCarga,
                 compraDinamitesNoLimite, compraIndiceInvalido, compraIndiceNegativo,
                 compraSemCreditos, compraQueNaoMelhora, compraForaDaOficina,
                 compraBroca, compraVida, escavaTerraDireita,
                 escavaTerraBaixo, voaCimaApoiada, andaParaArApoiada,
                 cimaContraBloco, escavaLava, lavaComPoucaVida,
                 rochaInvalida, minerioDuroDemais, minerioQueCabe,
                 minerioQueNaoCabe, minerioNoLimite, diamanteParaOLado,
                 baixoNaUltimaLinha, escavaNaUltimaLinha, direitaNaUltimaColuna,
                 cimaNaSuperficie, escavaAVoar, noArLadoBloco,
                 noArBaixo, noArCima, noArLadoAr,
                 semCombustivel, combustivelNoLimite, combustivelNaoChegaParaVoar,
                 combustivelNaoChegaParaDiamante]


-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . uncurry efetuaJogada) testesTarefa2
  runFeedback (T2 testesTarefa2 resultados)
