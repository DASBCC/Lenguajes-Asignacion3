module EquivLog where

import Sintax

--IMPLICACIÓN Y DISYUNCIÓN
imd (Implicacion prop1 prop2) = Disyuncion (Negacion prop1) prop2

--CASO INVERSO
imd (Disyuncion (Negacion prop1) prop2) = Implicacion prop1 prop2
--CONTRAPOSITIVA
contraPositiva (Implicacion prop1 prop2) = Implicacion (Negacion prop2) (Negacion prop1)
--CASO INVERSO
contraPositiva (Implicacion (Negacion prop1) (Negacion prop2)) = Implicacion prop1 prop2

--DOBLE NEGACIÓN
dn (Negacion (Negacion prop1)) = prop1

--DE MORGAN
dm :: Proposicion -> Proposicion
dm (Negacion (Disyuncion prop1 prop2)) = Conjuncion (Negacion prop1) (Negacion prop2) 
dm (Negacion (Conjuncion prop1 prop2)) = Disyuncion (Negacion prop1) (Negacion prop2)
--CASO INVERSO
dm (Conjuncion (Negacion prop1) (Negacion prop2)) = Negacion (Disyuncion prop1 prop2) 
dm (Disyuncion (Negacion prop1) (Negacion prop2)) = Negacion (Conjuncion prop1 prop2)

--CONMUTATIVA
con (Disyuncion prop1 prop2) = Disyuncion prop2 prop1
con (Conjuncion prop1 prop2) = Conjuncion prop2 prop1
--CASO INVERSO


--ASOCIATIVA
aso (Disyuncion (Disyuncion prop1 prop2) prop3) = Disyuncion  prop1 (Disyuncion prop2 prop3)
aso (Conjuncion (Conjuncion prop1 prop2) prop3) = Conjuncion  prop1 (Conjuncion prop2 prop3)
--CASO INVERSO


--DISTRIBUTIVA
dis (Disyuncion prop1 (Conjuncion prop2 prop3)) = Conjuncion (Disyuncion prop1 prop2) (Disyuncion prop1 prop3)
dis (Conjuncion prop1 (Disyuncion prop2 prop3)) = Disyuncion (Conjuncion prop1 prop2) (Conjuncion prop1 prop3)
--CASO INVERSO
dis (Conjuncion (Disyuncion prop1 prop2) (Disyuncion prop3 prop4)) = if (prop1 == prop3) then Disyuncion prop1 (Conjuncion prop2 prop3)
else (Conjuncion (Disyuncion prop1 prop2) (Disyuncion prop3 prop4))
dis (Disyuncion (Conjuncion prop1 prop2) (Conjuncion prop3 prop4)) = if (prop1 == prop3) then Conjuncion prop1 (Disyuncion prop2 prop3)
else (Disyuncion (Conjuncion prop1 prop2) (Conjuncion prop3 prop4))

--IDEMPOTENCIA
ide (Conjuncion prop1 prop2) = if (prop1 == prop2) then prop1 else Conjuncion prop1 prop2
ide (Disyuncion prop1 prop2) = if (prop1 == prop2) then prop1 else Disyuncion prop1 prop2

--NEUTRO
ne (Disyuncion prop1 (Constante False)) = prop1
ne (Conjuncion prop1 (Constante True)) =  prop1
--CASO ALTERNO
--DBEERÍA EVALUARLO CON
--ne (Disyuncion (Constante prop1) prop2) = if (Constante(prop1) == Constante(False)) then prop2 else Disyuncion (Constante prop1) prop2
--ne (Conjuncion (Constante prop1) prop2) = if (Constante(prop1) == Constante(True)) then prop2 else Conjuncion (Constante prop1) prop2


--INVERSOS
inv (Disyuncion prop1 (Negacion prop2)) = if (prop1 == prop2) then Constante True else Disyuncion prop1 (Negacion prop2)
inv (Conjuncion prop1 (Negacion prop2)) = if (prop1 == prop2) then Constante False else Conjuncion prop1 (Negacion prop2)
--CASO ALTERNO


dom (Conjuncion prop1 (Constante False)) = Constante False
dom (Disyuncion prop1 (Constante True)) = Constante True

--ABSORCIÓN
--abs (Disyuncion prop1 (Conjuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Disyuncion prop1 (Conjuncion prop2 prop3)
--abs (Conjuncion prop1 (Disyuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Conjuncion prop1 (Disyuncion prop2 prop3)

--EXPORTACIÓN
exp (Implicacion prop1 (Implicacion prop2 prop3)) = Implicacion (Conjuncion prop1 prop2) prop3
--NOTA COLOCAR FALSE EN LOS ELSE PARA SABER QUE LA EVALUACIÓN DEBE CONTINUAR


absorcion (Disyuncion prop1 (Conjuncion prop2 prop3)) = if (prop1 == prop2 || prop1 == prop3) then prop1 else Disyuncion (simplProp prop1) (Conjuncion (simplProp prop2) (simplProp prop3)) -- P v (P ^ Q) --- P v (Q ^ P)
absorcion (Disyuncion (Conjuncion prop1 prop2) prop3) = if (prop1 == prop3 || prop2 == prop3) then prop1 else Disyuncion (Conjuncion (simplProp prop2) (simplProp prop3)) (simplProp prop1) -- (P ^ Q) v P --- (Q ^ P) v P
absorcion (Conjuncion prop1 (Disyuncion prop2 prop3)) = if (prop1 == prop2 || prop1 == prop3) then prop1 else Conjuncion (simplProp prop1) (Disyuncion (simplProp prop2) (simplProp prop3)) -- P ^ (P v Q) --- P ^ (Q v P)
absorcion (Conjuncion (Disyuncion prop1 prop2) prop3) = if (prop1 == prop3 || prop2 == prop3) then prop3 else Conjuncion (simplProp prop1) (Disyuncion (simplProp prop2) (simplProp prop3)) -- (P v Q) ^ P --- (Q v P) ^ P


simplProp prop =
    case prop of
        (Disyuncion prop1 (Conjuncion prop2 prop3)) -> --ABSORCIÓN REGLAS 1 Y 2
            absorcion prop
        (Disyuncion (Conjuncion prop1 prop2) prop3) -> --ABSORCIÓN REGLAS 3 Y 4
            absorcion prop
        (Conjuncion prop1 (Disyuncion prop2 prop3)) -> --ABSORCIÓN REGLAS 5 Y 6
            absorcion prop
        (Conjuncion (Disyuncion prop1 prop2) prop3) -> --ABSORCIÓN REGLAS 7 Y 8
            absorcion prop
        _ -> prop

simpl prop =
    let
        newProp = simplProp prop
    in
       if prop == newProp
           then prop
       else
           simpl newProp


p = Variable "p"
q = Variable "q"
r = Variable "r"
p1 = p ||: (p &&: q)
p2 = p ||: (q &&: p)
p3 = (p &&: q) ||: p
p4 = (q &&: p) ||: p
p5 = p &&: (p ||: q)
p6 = p &&: (q ||: p)
p7 = (p ||: q) &&: p
p8 = (q ||: p) &&: p
p9 = p ||: (((r ||: q) &&: r) &&: p)



--abs (Disyuncion prop1 (Conjuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Disyuncion prop1 (Conjuncion prop2 prop3)
--abs (Conjuncion prop1 (Disyuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Conjuncion prop1 (Disyuncion prop2 prop3)
