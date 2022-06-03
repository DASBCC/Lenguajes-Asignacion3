module Sintax where

-- Definición del tipo de dato Proposición
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

infix ~:
(~:) :: Proposicion -> Proposicion
(~:) = Negacion 

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

-- Imprime la información contenida en un tipo de dato Proposicion
imprimir :: Proposicion -> [Char]
imprimir prop =
  case prop of
    (Constante False)          -> "False"
    (Constante True)           -> "True"
    (Variable nombre)          -> nombre
    (Negacion prop1)           -> "Negacion (" ++ imprimir  prop1 ++ ")"
    (Conjuncion prop1 prop2)   -> "Conjuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Disyuncion prop1 prop2)   -> "Disyuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Implicacion prop1 prop2)  -> "Implicacion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    (Equivalencia prop1 prop2) -> "Equivalencia (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"