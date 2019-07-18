
-- ---------------------------------------- Punto 1 y 2 ---------------------------------
-- digVerificador el cuil en tipo lista, le saco el último elemento y lo invierto
analiza :: Integer -> Bool
analiza cuil = longitud && prefijo && digVerificador
    where longitud = (length (digits cuil)) == 11
          prefijo = (   (obtenerPrefijo (digits cuil) == 20)
                    ||  (obtenerPrefijo (digits cuil) == 23)
                    ||  (obtenerPrefijo (digits cuil) == 24)
                    ||  (obtenerPrefijo (digits cuil) == 27)
                    )
          digVerificador = modulo11 (verificarCuil (reverse(init(digits cuil))) 2) (last(digits cuil))

-- Esto me convierte un int en una lista de ints
digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

-- Convierte de [Int] -> Int
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

obtenerPrefijo :: [Int] -> Int
obtenerPrefijo (x:y:xs) = fromDigits [x,y]

verificarCuil :: [Int] -> Int -> [Int]
verificarCuil [] i = []
verificarCuil (x:xs) i = if (i == 7)
                        then (x * i) : verificarCuil xs (i - 5)
                        else (x * i) : verificarCuil xs (i + 1)

modulo11 :: [Int] -> Int -> Bool
modulo11 xs i = ((11 - (mod(sum xs) 11)) == i) || (11 - (mod(sum xs) 11) == 11 && i==0)

-- ----------------------------------- Punto 3 ---------------------------------------

informa :: Integer -> String
informa cuit = personaOEmpresa ++ ", " ++ dniRepetido ++ ", " ++ hombreOMujer ++ "," ++ cuitSeparado 
    where personaOEmpresa = if (
                                (obtenerPrefijo (digits cuit)) == 20
                              || 
                                (obtenerPrefijo (digits cuit)) == 23
                              ||
                                (obtenerPrefijo (digits cuit)) == 27
                               )
                            then "Persona"
                            else "Empresa"
          dniRepetido = if ((obtenerPrefijo (digits cuit)) == 24)
                        then "DNI repetido"
                        else "DNI unico"
          hombreOMujer = if((personaOEmpresa) == "Persona")
                            then if ((obtenerPrefijo (digits cuit)) == 20)
                                    then "Masculino"
                                    else "Femenino"
                            else ""
          cuitSeparado = obtenerCuitString (digits cuit)


obtenerCuitString :: [Int] -> String
obtenerCuitString cuit = "Prefijo: " ++ show(obtenerPrefijo cuit) ++ 
                         ", DNI: " ++ show(fromDigits(obtenerDniList cuit)) ++ 
                         ", Digito Verificador: " ++ show(obtenerDigList cuit)

obtenerDniList :: [Int] -> [Int]
obtenerDniList cuit = reverse(tail(reverse(tail(tail(cuit)))))

obtenerDigList :: [Int] -> Int
obtenerDigList cuit = head(reverse(cuit))

-- ----------------------------------- Punto 4 ---------------------------------------

-- Persona tiene un género, un dni y un cuit válido
data Persona = Persona Genero Integer String deriving Show

-- Género masculino o femenino
data Genero = Femenino | Masculino deriving (Eq,Show)

genera :: Genero -> Integer -> String
genera g dni = (show genero) ++ (show dni) ++ (show digVerificador)
        where genero = obtenerGen dni g
              digVerificador = obtenerDig genero dni

obtenerGen :: Integer -> Genero -> Integer
obtenerGen dni g = if((obtenerDig 20 dni) /= 10 && g == Masculino)
                then 20
                else if ((obtenerDig 27 dni) /= 10 && g == Femenino)
                then 27
                else 23

obtenerDig :: Integer -> Integer -> Int
obtenerDig g dni = if((mod(sum(verificarCuil (reverse ((digits g) ++ (digits dni))) 2)) 11) == 11
                     || (mod(sum(verificarCuil (reverse ((digits g) ++ (digits dni))) 2)) 11) == 0                    
                     )
                    then 0
                    else 11 - (mod(sum(reverse(verificarCuil ( reverse ((digits g) ++ (digits dni))) 2))) 11)


----------------------------------------------- PUNTO 5 -------------------------------------------

informaLista :: [Integer] -> [String]
informaLista = map informa

----------------------------------------------- PUNTO 6 -------------------------------------------

generaLista :: [(Genero,Integer)] -> [String]
generaLista xs = map (\(a,b) -> (genera a b)) xs

-- -------------------------------------------- TEST PUNTO 7 ---------------------------------------

--------------------------------- TEST ANALIZA --------------------------------------
cuitsOk :: [Integer]
cuitsOk = [20929014740, 20391469807, 27247214300, 23273715789, 23393716134, 23393718064]

cuitsNOk :: [Integer]
cuitsNOk = [20929014741, 27929014740, 20929814740, 20391469800, 27247214305, 27247294300, 27273715789, 20393718064]

testAnaliza :: String
testAnaliza = if todoOk
              then "Todo Ok"
              else mensajeError
    where todoOk = (and(map analiza cuitsOk))
                && (not(or(map analiza cuitsNOk)))
          mensajeError = "Todo Mal! - CUIT buenos reconocidos malos: " ++ errCuitsOk ++ 
                                  " - CUIT malos reconocidos como buenos: " ++ errCuitsNOk
          errCuitsOk  = show $ filter (not.analiza) cuitsOk
          errCuitsNOk = show $ filter analiza cuitsNOk


---------------------------------- TEST GENERA -----------------------------------------
personas :: [Persona]
personas = [Persona Masculino 92901474 "20929014740",
            Persona Femenino 24721430  "27247214300",
            Persona Masculino 39146980 "20391469807",
            Persona Masculino 27371578 "23273715789",
            Persona Femenino 39371613  "23393716134",
            Persona Femenino 39371806  "23393718064"]

testGenera :: String
testGenera = if todoOk
            then "Todo Ok"
            else mensajeError
     where todoOk = and $ map cuitOk personas
           cuitOk (Persona g dni cuit) = (genera g dni) == cuit
           mensajeError = "Todo Mal! - Personas con CUIT mal generado: " ++ errCuits
           errCuits = show $ filter (not.cuitOk) personas

------------------------------ TEST INFORMA LISTA -----------------------------------------
testInformaLista :: String
testInformaLista = if todoOk
                then "Todo Ok"
                else mensajeError
      where todoOk = (informaLista cuitsOk) == ["Persona, DNI unico, Masculino,Prefijo: 20, DNI: 92901474, Digito Verificador: 0",
                                                "Persona, DNI unico, Masculino,Prefijo: 20, DNI: 39146980, Digito Verificador: 7",
                                                "Persona, DNI unico, Femenino,Prefijo: 27, DNI: 24721430, Digito Verificador: 0",
                                                "Persona, DNI unico, Femenino,Prefijo: 23, DNI: 27371578, Digito Verificador: 9",
                                                "Persona, DNI unico, Femenino,Prefijo: 23, DNI: 39371613, Digito Verificador: 4",
                                                "Persona, DNI unico, Femenino,Prefijo: 23, DNI: 39371806, Digito Verificador: 4"]
            mensajeError = "Todo mal! - No se pudieron analizar todos los CUITs"


cuitsGeneroOk = [(Masculino, 39146980), (Femenino, 24721430), (Masculino, 39146980), (Masculino, 27371578),(Femenino, 39371613), (Femenino, 39371806)]

-------------------------- TEST GENERA LISTA ---------------------------------------------
testGeneraLista :: String
testGeneraLista = if todoOk
                  then "Todo Ok"
                  else mensajeError
      where todoOk = (generaLista cuitsGeneroOk) == ["20391469807","27247214300","20391469807",
                                                     "23273715789","23393716134","23393718064"]
            mensajeError = "Todo mal! - No se pudieron generar todos los CUITs"