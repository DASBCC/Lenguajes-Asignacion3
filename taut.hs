module Taut where
import Sintax
import Vars
import GenBools
import AsVals
import EvalProp

--Recorre los distintos ambientes generados al evaluar la tabla de verdad
recorrer :: Proposicion -> [[Bool]]-> [[Bool]]
recorrer _ [[]] = []
recorrer prop listaBools = 
    let
        asociacion = as_vals (vars prop) (head (listaBools))
        n = length listaBools
    in
        if evalProp asociacion prop
            then 
                if n == 1
                    then
                        []
                else
                    recorrer prop (drop 1 listaBools)
        else [head (listaBools)]

--Función principal, verifica una tautología y si lo es lo indica.
--Si no es una tautología indica el motivo
taut :: Proposicion -> String
taut prop = 
    let 
        variables =  vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n
        resultado = recorrer prop lista_combinaciones_booleanas
    in
        if resultado == []
            then imprimir prop ++ "  es una tautologia "
        else
            imprimir prop ++ "  no es una tautologia, por que  " ++ show (as_vals variables (head resultado))
