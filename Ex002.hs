import Data.List (sortBy)
--Lista em haskell

-- 1.Crie uma função em Haskell que receba uma lista de inteiros e retorne a soma dos elementos da lista.
somaLista :: [Int] -> Int
somaLista [] = 0 -- caso base para lista vazia
somaLista (x:xs) = x + somaLista xs -- caso recursivo

--2.Crie uma função em Haskell que receba uma lista de inteiros e retorne o maior valor da lista.
maiorValor :: [Int] -> Int
maiorValor [] = error "Lista vazia"
maiorValor [x] = x
maiorValor (x:y:xs)
  | x > y = maiorValor (x:xs)
  | otherwise = maiorValor (y:xs)

--3. Crie uma função em Haskell que receba uma lista de inteiros e retorne o menor valor da lista.
menorValor :: [Int] -> Int
menorValor [] = error "Lista vazia"
menorValor [x] = x
menorValor (x:y:xs)
  | x < y = menorValor (x:xs)
  | otherwise = menorValor (y:xs)

--4. Crie uma função em Haskell que receba uma lista de inteiros e retorne o tamanho da lista.
tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs

--5. Crie uma função em Haskell que receba uma lista de inteiros e retorne o elemento de uma determinada posição da lista.
elementoPosicao :: [Int] -> Int -> Int
elementoPosicao [] _ = error "Lista vazia"
elementoPosicao (x:xs) 0 = x
elementoPosicao (x:xs) n = elementoPosicao xs (n-1)

--6. Crie uma função em Haskell que receba uma lista de inteiros e retorne o reverso da lista.
reversoLista :: [Int] -> [Int]
reversoLista [] = []
reversoLista (x:xs) = reversoLista xs ++ [x]

--7.Crie uma função que receba uma lista de elementos e me retorne uma tupla com o maior e menor valor da lista
maiorMenor :: [Int] -> (Int, Int)
maiorMenor [] = error "Lista vazia"
maiorMenor [x] = (x, x)
maiorMenor (x:xs) = (maiorValor (x:xs), menorValor (x:xs)) -- primeira posição é o maior valor e a segunda posição o menor
--PROVA 2 ABAIXO:

-- --1. Considere a função salario que recebe um inteiro com o id de cada funcionário e retorna seu
-- respectivo salário. Escreva um programa em Haskell que implemente as funcionalidades
-- descritas em cada item abaixo.

-- a. Informar o quanto a empresa gasta com salários mensalmente.
salario::Int->Float
salario 1 = 772.25
salario 2 = 2375.0
salario 3 = 1778.5
salario 4 = 6520.0
salario 5 = 3447.35
salario 6 = 5225.75
salario 7 = 8932.0
salario 8 = 648.5
salario 9 = 1982.4
salario 10 = 2557.45
salario _ = 0

gastoSalario :: [Float] -> Float
gastoSalario [] = 0
gastoSalario (x:xs) = x + gastoSalario xs

-- b. Receber um determinado valor n e informar quantas pessoas recebem salário acima de n.
salarioAcima :: [Float] -> Float -> Int
salarioAcima [] _ = 0
salarioAcima (x:xs) n
  | x > n = 1 + salarioAcima xs n
  | otherwise = salarioAcima xs n
  
-- c. Receber um determinado valor n e informar qual salário mais se aproxima deste valor.
salarioAprox :: [Float] -> Float -> Float
salarioAprox [] _ = 0
salarioAprox (x:xs) n
  | x > n = x
  | otherwise = salarioAprox xs n

-- d. Considerando que existem contribuições previdenciárias variáveis para cada faixa salarial,

-- informe o total a ser custeado pela empresa sabendo que salários menores que
-- R$1.100,00 são isentos, salários entre R$ 1.100,01 e R$ 2.203,48 pagam 9%, entre R$
-- 2.203,49 e R$ 3.305,22 pagam 12%, entre R$ 3.305,23 e R$ 6.433,57 pagam 14% e
-- salários acima de R$ 6.433,57 contribuem com 22%.
totalContribuicao :: [Float] -> Float
totalContribuicao [] = 0
totalContribuicao (x:xs)
  | x <= 1100.0 = 0 + totalContribuicao xs
  | x > 1100.0 && x <= 2203.48 = (x * 0.09) + totalContribuicao xs
  | x > 2203.48 && x <= 3305.22 = (x * 0.12) + totalContribuicao xs
  | x > 3305.22 && x <= 6433.57 = (x * 0.14) + totalContribuicao xs
  | otherwise = (x * 0.22) + totalContribuicao xs

-- salario::Int->Float
-- salario 1 = 772.25
-- salario 2 = 2375.0
-- salario 3 = 1778.5
-- salario 4 = 6520.0
-- salario 5 = 3447.35
-- salario 6 = 5225.75
-- salario 7 = 8932.0
-- salario 8 = 648.5
-- salario 9 = 1982.4
-- salario 10 = 2557.45
-- salario _ = 0
-- *****************************************************************************************************************************************************

-- TESTE 02 ABAIXO
-- Você está desenvolvendo um novo algoritmo de compressão de dados em Haskell e deseja
-- analisar quais dados mais se repetem em uma determinada lista. Sendo assim, você resolve
-- criar a função analise que organiza os dados analisados em uma lista de tuplas contendo cada
-- elemento e sua respectiva contagem de repetições. (2,0)
-- Exemplo:
-- analise "IFMA CAXIAS" -> [('I',2),('F',1),('M',1),('A',3),(' ',1),('C',1),('A',3),('X',1),('I',2),('A',3), (’S’,1)]
contaOcorrencias :: Char -> [Char] -> Int
contaOcorrencias _ [] = 0
contaOcorrencias x (y:ys)
  | x == y = 1 + contaOcorrencias x ys
  | otherwise = contaOcorrencias x ys

removeOcorrencias :: Char -> [Char] -> [Char]
removeOcorrencias _ [] = []
removeOcorrencias x (y:ys)
  | x == y = removeOcorrencias x ys
  | otherwise = [y] ++ removeOcorrencias x ys

analise :: [Char] -> [(Char, Int)]
analise [] = []
analise (x:xs) = [(x, contaOcorrencias x (x:xs))] ++ analise (removeOcorrencias x xs)



-- 2. Suponha que tenhamos uma lista de caracteres e que desejamos ordenar seus elementos de
-- acordo com suas ocorrências. Neste caso, teremos os elementos mais frequentes no início da
-- lista e os mais raros posicionados no fim. (2,0)
-- Exemplo:
-- OrdenaFreq "IFMA CAXIAS" -> “AIFM CXS"

ordenaFreq :: [Char] -> [Char]
ordenaFreq xs = [x | (x,_) <- sortBy (\(_,c1) (_,c2) -> compare c2 c1) (analise xs)]

