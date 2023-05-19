import Data.List (nub)
-- Questão 01
numDigits :: Int -> Int 
numDigits n
    | n <= 1 = 1
    | otherwise = 1 + numDigits (n `div` 10)

-- Questão 02
fatorialN :: Int -> Int
fatorialN n
    | n <= 1 = n
    | otherwise = n * fatorialN(n - 1)

-- Questão 03
fatorInverso :: Int -> Int -> Int
fatorInverso m n
    | fatorialN(n) == m = n
    | n == 0 = 1
    | otherwise = fatorInverso m (n + 1)
 
-- Questão 04
somaFat :: Int -> Int -> Int
somaFat n i
    | n < 0 = i 
    | otherwise = somaFat(n - 1) (i + fatorialN(n))

-- Questão 05-A
fibo :: Int -> Int
fibo n 
    | n <= 1 = n
    | otherwise = fibo (n - 1) + fibo (n - 2)

-- Questão 05-B   
fiboProduct :: Int -> Int
fiboProduct n
    | n == 0 = 1
    | otherwise = fiboProduct(n - 1) * fibo(n)

-- Questão 05-C    
divisorP :: Int -> Int -> Bool
divisorP n m
    | n == 1 = False
    | n == 2 = True
    | m == 1 = True
    | n `mod` m == 0 = False
    | n `mod` m /= 0 = divisorP n (m - 1)

primo :: Int -> Bool
primo n
    | n == 1 = False
    | n == 2 = True
    | otherwise = divisorP n (n - 1)

primoFibo :: Int -> Int
primoFibo n
    | n == 0 = 0
    | primo(fibo(n)) = fibo(n)
    | otherwise = primoFibo(n - 1)

-- Questão 06

sinal :: Int -> Int
sinal n 
    | mod n 2 == 0 = 1
    | otherwise = -1

somaFatAlt :: Int -> Int 
somaFatAlt n
    | n == 0 = 1
    | otherwise = fatorialN(n) * sinal(n) + somaFatAlt(n-1) 
    
  
-- Questão 08  
coletaDia :: Int -> Int
coletaDia n
    | n == 1 = 7
    | n == 0 = 0
    | otherwise = coletaDia(n - 1) + (7 * n)

fatAlt :: Int -> Int
fatAlt n
    | n <= 0 = 1
    | otherwise = ((-1) ^ n) * fatorialN(n) + fatAlt( n - 1)

-- Questão 09 soma digitos de um numero

somaD :: Int -> Int
somaD n
    | n <= 0 = 0
    | otherwise = (n `mod` 10) + somaD(n `div` 10)

-- Questão 10

modulo :: Int -> Int->Int
modulo dividendo divisor = dividendo `mod` divisor

-- Questão 11
mmc :: Int -> Int -> Int
mmc x y = (x * y) `div` mdc x y

-- Questão 12
mdc :: Int -> Int -> Int
mdc x y
    | x == y = x
    | x > y = mdc (x - y) y
    | otherwise = mdc x (y - x)

-- Questão 13 PRIMO DE SOPHIE GERMAIN
sophieGermain :: Int -> Bool
sophieGermain n
    | primo(n) && primo(2 * n + 1) = True
    | otherwise = False

contPrimoSophie :: Int -> Int
contPrimoSophie n
    | n == 0 = 0
    | sophieGermain(n) = 1 + contPrimoSophie(n - 1)
    | otherwise = contPrimoSophie(n - 1)

-- Questão 14 Receba dois números e retorne um valor booleano informando se eles constituem um par
--de números primos gêmeos. (1,0)
primoGemeos :: Int -> Int -> Bool
primoGemeos x y
    | x <= 0 || y <= 0 = False
    | primo(x) && primo(y) && (x - y == 2 || y - x == 2) = True
    | otherwise = False

-- Questão 16  Receba um determinado número p e retorne um valor booleano informando se ele
--pertence a algum par de número primo gêmeo. (1,0)
pertenceGemeo :: Int -> Bool
pertenceGemeo n
    | primoGemeos(n - 2) n  || primoGemeos(n + 2) n= True
    | otherwise = False

parGemeos :: Int -> Int-> Bool
parGemeos x y
    | x <= 0 || y <= 0 = False
    | primo(x) && primo(y) && (x - y == 2 || y - x == 2) = True
    | otherwise = False
 -- Questão 17 Contabilize quantos pares de primos gêmeos existem abaixo de um número n. (2,0)
contParGemeos :: Int -> Int
contParGemeos n
    | n < 5 = 0
    | parGemeos(n - 2) n  || parGemeos(n + 2) n= 1 + contParGemeos(n - 1)
    | otherwise = contParGemeos(n - 1)


--Questão 18 Calcule a soma de todos os primos gêmeos que aparecem entre 0 e n. (2,0)
somaGemeos :: Int -> Int
somaGemeos n
    | n == 0 = 0
    | pertenceGemeo(n) = n + somaGemeos(n - 1)
    | otherwise = somaGemeos(n - 1)

-- Retorna o n-ésimo primo
nPrimo :: Int -> Int
nPrimo n
    | n == 1 = 2
    | otherwise = auxNPrimo n 2 1

auxNPrimo :: Int -> Int -> Int -> Int
auxNPrimo n y i
    | n == i && primo y = y
    | primo y = auxNPrimo n (y + 1) (i + 1)
    | otherwise = auxNPrimo n (y + 1) i


-- Multiplica todos os primos até n incluso
multPrimo :: Int -> Int
multPrimo n
    | n == 1 = 1
    | primo n = n * multPrimo (n-1)
    | otherwise = multPrimo (n-1)

-- afortunado
afortunado :: Int -> Int
afortunado n = auxAfortunado (multPrimo (nPrimo n)) 2

-- Lógica: Buscamos o  n-ésimo primo e multiplicamos os primos até ele e passamos no primeiro parâmetro da auxiliar, e no segundo buscamos os números até achar um que somando dê um valor primo
auxAfortunado :: Int -> Int -> Int
auxAfortunado k i
    | primo (k + i) = i
    | otherwise = auxAfortunado k (i + 1) 

somaInt:: Int -> Int -> Int
somaInt n m
    |n == 1 && m == 1 = 1
    |n < m = n + somaInt(n+1) m
    |n == m = m
    |otherwise = n + somaInt (n-1) m


verificaPar :: Int -> Bool
verificaPar n
    | mod n 2 == 0 = True
    | otherwise = False

verficaImpar :: Int -> Bool
verficaImpar n
    | mod n 2 == 0 = False
    | otherwise = True

-- Soma dos algarimos pares
somaAlgarismosPares :: Int -> Int
somaAlgarismosPares n
    | n < 10 && verificaPar n = n
    | n < 10 && verficaImpar n = 0
    | verificaPar (mod n 10) = (mod n 10) + somaAlgarismosPares (div n 10)
    | otherwise = somaAlgarismosPares (div n 10)

-- Subtração dos algarimos impares
subtraiAlgarismosImpares :: Int -> Int
subtraiAlgarismosImpares n
    | n < 10 && verficaImpar n = n
    | n < 10 && verificaPar n = 0
    | verficaImpar (mod n 10) = (mod n 10) - subtraiAlgarismosImpares (div n 10)
    | otherwise = subtraiAlgarismosImpares (div n 10)

-- Multiplicação dos algarimos impares
multiplicaAlgarismosImpares :: Int -> Int
multiplicaAlgarismosImpares n
    | n < 10 && verficaImpar n = n
    | n < 10 && verificaPar n = 1
    | verficaImpar (mod n 10) = (mod n 10) * multiplicaAlgarismosImpares (div n 10)
    | otherwise = multiplicaAlgarismosImpares (div n 10)

-- Multiplicação dos algarimos pares
multiplicaAlgarismosPares :: Int -> Int
multiplicaAlgarismosPares n
    | n < 10 && verificaPar n = n
    | n < 10 && verficaImpar n = 1
    | verificaPar (mod n 10) = (mod n 10) * multiplicaAlgarismosPares (div n 10)
    | otherwise = multiplicaAlgarismosPares (div n 10)

-- Escreva uma função que calcule a função phi de Euler para um número (o número de inteiros positivos menores ou iguais a ele e coprimos com ele).
auxPhi :: Int -> Int -> Int
auxPhi n i
    | i > n = 0
    | mdc n i == 1 = 1 + auxPhi n (i + 1)
    | otherwise = auxPhi n (i + 1)

phi :: Int -> Int
phi n 
    | n == 1 = 1
    | otherwise = auxPhi n 1

-- Escreva uma função que calcule a função de Möbius para um número (1 se o número tiver um número par de fatores primos distintos, 
-- -1 se tiver um número ímpar de fatores primos distintos,
--  e 0 se tiver algum fator primo repetido)
