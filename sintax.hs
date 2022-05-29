module Sintax where

-- Definición del tipo de dato proposición
data Proposicion =       
      Constante    Bool
    | Variable     String
    | Negacion     Proposicion
    | Conjuncion   Proposicion Proposicion
    | Disyuncion   Proposicion Proposicion
    | Implicacion  Proposicion Proposicion
    | Equivalencia Proposicion Proposicion
    deriving (Eq, Show)
    -- Se declara que pueda acceder a los usos de los tipos de clases Eq y Show
    -- Permite hacer uso de ==, /= y Show

-- Imprime la información contenida en un tipo de dato Proposicion
--imprimir (Constante    False)       = "False"
--imprimir (Constante    True)        = "True"
--imprimir (Variable     var)         = var   
--imprimir (Negacion     prop1)       = "~" ++ imprimir prop1
--imprimir (Conjuncion   prop1 prop2) = "Conjuncion (" ++ imprimirPropAux " , " prop1 prop2 ++ ")"
--imprimir (Disyuncion   prop1 prop2) = "Disyuncion (" ++ imprimirPropAux " , " prop1 prop2 ++ ")"
--imprimir (Implicacion  prop1 prop2) = "Implicacion (" ++ imprimirPropAux " , " prop1 prop2 ++ ")"
--imprimir (Equivalencia prop1 prop2) = "Equivalencia (" ++ imprimirPropAux " , " prop1 prop2 ++ ")"

--Funcion Auxuliar para mostrar los datos
--imprimirPropAux :: String -> Proposicion -> Proposicion -> String
--imprimirPropAux operation prp1 prp2= imprimir prp1 ++ " " ++ operation ++ " " ++ imprimir prp2

infixl 7 &&:
(&&:) :: Proposicion -> Proposicion -> Proposicion
prop1 &&: prop2 = Conjuncion prop1 prop2

infixl 6 ||:
(||:) :: Proposicion -> Proposicion -> Proposicion
prop1 ||: prop2 = Disyuncion prop1 prop2

infixl 5 =>:
(=>:) :: Proposicion -> Proposicion -> Proposicion
prop1 =>: prop2 = Implicacion prop1 prop2

infixl 4 <=>:
(<=>:) :: Proposicion -> Proposicion -> Proposicion
prop1 <=>: prop2 = Equivalencia prop1 prop2

imprimir :: Proposicion -> [Char]
imprimir prop =
  case prop of
    (Constante False)          -> "false"
    (Constante True)           -> "true"
    (Variable nombre)          -> nombre
    (Negacion prop1)           -> "negacion (" ++ imprimir  prop1 ++ ")"
    (Conjuncion prop1 prop2)   -> "conjuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Disyuncion prop1 prop2)   -> "disyuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Implicacion prop1 prop2)  -> "implicacion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Equivalencia prop1 prop2) -> "equivalencia (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"