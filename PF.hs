module PF where 
--1)
--a-Calcula o perimetro de uma circunferencia dando o raio
perimetro :: Float -> Float 
perimetro raio = 2*pi*raio  
--b-Calcula a distancia entre dois pontos
dist :: (Float,Float) -> (Float,Float) -> Float 
dist (x1,y1) (x2,y2) = sqrt((x2-x1)**2 + (y2-y1)**2)
--c-Recebe uma lista e devolve o primeiro e ultimo dessa lista
primUlt :: [Float] -> (Float,Float)
primUlt l = (head l,last l)
--d-Verifica se m é ou nao multiplo de n
multiplo :: Int -> Int -> Bool 
multiplo m n = (mod m n == 0) 
--e-Recebe uma lista, se o n de elementos for par repete a lista
--, se for impar retira o primeiro elemento da lista
truncaImpar :: [Int] -> [Int]
truncaImpar l = if(mod (length l) 2 /= 0) then tail l else l
--f-Calcula o maior elemento de dois numeros inteiros
max2 :: (Int,Int) -> Int 
max2 (a,b) = if(a > b) then a else b
--g-Calcula o maior elemento de dois numeros inteiros a partir de max2
max3 :: (Int,Int,Int) -> Int 
max3 (a,b,c) = max2(max2 (a,b), c)
--2)
--a 
nraizes :: Float -> Float -> Float -> Int
nraizes a b c = if d<0 then 0
	else if d==0 then 1
    else 2
    where d=b^2 -4*a*c

raizes :: Float -> Float -> Float -> [Float] 
raizes a b c | nraizes a b c ==0 = []
             | nraizes a b c ==1 = [-b/(2*a)]
             | nraizes a b c ==2 = [(-b + sqrt d)/(2*a) , (-b - sqrt d)/(2*a)] 

 where d=b^2 -4*a*c


--3)
--a
compTrin :: (Float,Float) -> (Float,Float) -> (Float,Float) ->(Float,Float,Float)
compTrin (x1,y1) (x2,y2) (x3,y3) = (sqrt(x1**2 + y1**2),sqrt(x2**2 + y2**2),sqrt(x3**2 + y3**2))
 --b 
periTrin :: (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float)
periTrin (x1,y1) (x2,y2) (x3,y3) = ((sqrt(x1**2 + y1**2)) + (sqrt(x2**2 + y2**2)) + (sqrt(x3**2 + y3**2)))
--c
vertiReta :: (Float,Float) -> (Float,Float) -> ((Float,Float),(Float,Float),(Float,Float),(Float,Float))
vertiReta (x1,y1) (x2,y2) = ((x1,y1),(x1,y2),(x2,y1),(x2,y2))
--4
--a- Verificar se o par de inteiros equivale a uma hora
veriHora :: (Int,Int) -> (Bool)
veriHora (h,m) = if(h>0 && h<24)&&(m>0 && m<60) then True else False
--b- Comparar se uma hora é ou nao depois da outra
compHora :: (Int,Int) -> (Int,Int) -> (Int,Int)
compHora (h1,m1) (h2,m2) = if(h2>h1)&&(m2>m1) then (h2,m2) else (h1,m1) 
--compHora (h1,m1) (h2,m2) = if(h2==h1)&&(m2>m1) then (h2,m2) else (h1,m1) 
--c - converter horas em minutos
converthm :: (Int,Int) -> (Int)
converthm (h,m) = h*60 + m 
--d - Converter minutos em horas
convertmh :: Int -> (Int,Int)
convertmh min = if((div min 60) == 0) then (min,0) else (div min 60,mod min 60)
--e -calcular a diferenca entre duas horas ( apresentar em minutos)
difhhm :: (Int,Int) -> (Int,Int) -> (Int)
difhhm (h1,m1) (h2,m2) = h1*60 + m1 -(h2*60 + m2)
--where x = (h1,m1)
--where y = (h2,m2)
--f adicionar minutos a uma hora
adimin :: Int -> (Int,Int) -> (Int,Int)
adimin x (y,z) = if(m>60) then (h+1,m-60) else (h,m)
                 where h = (div x 60) + y
                       m = (mod x 60) + z
--Ficha 2
--exercicio 2
--a recebe uma lista e produz outra com o dobro dos valores
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h : dobros t)
--b calcula o mumero de vezes que um caracter ocorre numa string
--numOcorre :: Char -> String -> Int 
numOcorre k [] = 0
numOcorre k (h:t) = if(k==h) then (1 + numOcorre k t) else (numOcorre k t)
--mOcorre k (h:t) = length (k:t)
--c Testa se uma lista so tem elementos positivos
positivos :: [Int] -> Bool 
positivos [] = True
positivos (h:t) =if(h>0)then positivos t else False 
--d Retira todos os elementos negativos de uma lista de inteiros
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if(h>=0) then h : (soPos t) else soPos t
--e soma todos os elementos negativos da lista de entrada
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if(h<0) then h + (somaNeg t) else somaNeg t
--f Devolve os ultimos tres elementos de uma lista 
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if(length (h:t)<=3) then (h:t) else tresUlt t
--g Recebe uma lista de pares e devolver a lista com as primeiras componentes desses pares
primeiros :: [(a,b)] -> [a]
primeiros ((x,y):t) = (x:primeiros t)




