module EvalProp where
import Data.Maybe
import Sintax

-- > evalProp

evalProp ambiente prop =
    case prop of
        (Constante valor)           -> valor
        (Variable var)              -> fromMaybe False (lookup var ambiente)
        (Negacion prop1)            -> not (evalProp ambiente prop1)
        (Conjuncion prop1 prop2)    -> let valor1 = evalProp ambiente prop1
                                           valor2 = evalProp ambiente prop2
                                       in (&&) valor1 valor2
        (Disyuncion prop1 prop2)    ->let valor1 = evalProp ambiente prop1
                                          valor2 = evalProp ambiente prop2
                                       in (||) valor1 valor2
        (Implicacion prop1 prop2)   ->let valor1 = evalProp ambiente prop1
                                          valor2 = evalProp ambiente prop2
                                       in case (valor1, valor2) of
                                           (True, False) -> False
                                           _ -> True
        (Equivalencia prop1 prop2)  ->let valor1 = evalProp ambiente prop1
                                          valor2 = evalProp ambiente prop2
                                       in valor1 == valor2

