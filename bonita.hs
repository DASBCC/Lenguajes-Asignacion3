
module Bonita where

import Sintax

--Inserta los operadores lógicos en el string generado
insertarConectivos  conectivo  prop1 prop2 = prop1 ++ " " ++ conectivo ++ " " ++ prop2;

--Revisa si la proposición ingresada requiere de paréntesis
revisarCon (Disyuncion prop1 prop2) = True
revisarCon (Implicacion prop1 prop2) = True
revisarCon (Equivalencia prop1 prop2) = True
revisarCon _ = False;

--Genera la impresión de una proposición con los paréntesis estrictamente necesarios
bonita prop = 
    case prop of
        (Variable var)           -> var
        (Constante False)        -> "false"
        (Constante True)         -> "true"
        (Negacion prop1)         -> "~ " ++ bonita prop1
    --Caso de paréntesis necesarios
        (Conjuncion prop1 prop2) ->
            (let valor1 = if revisarCon prop1 then "(" ++ bonita prop1 ++ ")"
                    else bonita prop1 
            --Revisa si la primera proposición ocupa paréntesis      
                 valor2 = if revisarCon prop2 then "(" ++ bonita prop2 ++ ")"
            --Revisa si la primera proposición ocupa paréntesis
                    else bonita prop2
            in insertarConectivos "&&" valor1 valor2)
        (Disyuncion prop1 prop2)    -> 
                let valor1 = bonita prop1
                    valor2 = bonita prop2
                in insertarConectivos "||" valor1 valor2  
        (Implicacion prop1 prop2)   -> 
            let valor1 = bonita prop1
                valor2 = bonita prop2
            in insertarConectivos "=>" valor1 valor2 
        (Equivalencia prop1 prop2)  -> 
            let valor1 = bonita prop1
                valor2 = bonita prop2
            in insertarConectivos "<=>" valor1 valor2