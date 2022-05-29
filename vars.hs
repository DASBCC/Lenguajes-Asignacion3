module Vars where
import Sintax
import Data.List (nub, sort)


--Funcion encargada de mostrar las variables que se encuentran en una proposicion, omitiendo repetidas
vars prop = let 
   las_vars prop =
      case prop of
         (Constante    _)           -> []  
         (Variable     var)         -> [var]
         (Negacion     prop1)       -> las_vars prop1
         (Conjuncion   prop1 prop2) -> let vars1 = las_vars prop1
                                           vars2 = las_vars prop2
                                       in vars1 ++ vars2                     
         (Disyuncion   prop1 prop2) -> let vars1 = las_vars prop1
                                           vars2 = las_vars prop2
                                       in vars1 ++ vars2
         (Implicacion  prop1 prop2)  -> let vars1 = las_vars prop1
                                            vars2 = las_vars prop2
                                       in vars1 ++ vars2
         (Equivalencia prop1 prop2)  -> let vars1 = las_vars prop1
                                            vars2 = las_vars prop2
                                       in vars1 ++ vars2
   in nub (las_vars prop)

