module FNC where

import Sintax
import AsVals
import Vars
import GenBools
import EvalProp

--Coloca las variables con sus respectivos valores de verdad
impFnc [] _ _             = ")"
impFnc ((v,b) : vbs) largo i =  if i < largo then (if b then "~" else "") ++ v ++ " || " ++ impFnc  vbs largo (i+1)
    else (if b then "~" else "") ++ v ++ impFnc  vbs largo (i+1) 


--Llama a impFnd pasándole los parámetros que requiere
unificarFNC _ [] _ = ""
unificarFNC vars bools cantV = "(" ++ impFnc (as_vals vars bools) cantV 1


recorrer :: Proposicion -> [[Bool]] -> Int -> Int -> [Char]
recorrer _ [] _  _                = ""
recorrer prop listaBools i largoListComb =
        let
            variables = vars prop
            n = length variables
            asociacion = as_vals variables (head listaBools)
            evaluacion_es_verdadera = evalProp asociacion prop
            
            --Contexto definido y evaluado
        in
            if not evaluacion_es_verdadera then
                if i < largoListComb then
                    unificarFNC variables (head listaBools) n ++ " && "  ++ recorrer prop (drop 1 listaBools) (i+1) largoListComb
                    --Caso elemento de la lista no final
                else
                    unificarFNC variables (head listaBools) n  ++ recorrer prop (drop 1 listaBools) (i+1) largoListComb
                    --Caso elemento de la lista final
            else
                recorrer prop (drop 1 listaBools) (i+1) largoListComb
                --siguiente elemento

--Genera la forma normal disyuntiva de una proposición



fnc :: Proposicion -> [Char]
fnc prop =
    let
        variables = vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n
        --Arriba se definen las variables a utilizar
        --Recorrer recorre el total de combinaciones
        resultado = recorrer prop lista_combinaciones_booleanas 1 (length lista_combinaciones_booleanas)
        --Llamada a recorrer, definiendo contador en 1
    in
       if resultado == ""
           then "No hay una FNC de la proposicion dada, ya que es una Tautologia"
       else
           resultado