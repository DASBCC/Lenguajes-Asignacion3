module EquivLog where

import Sintax

--ABSORCIÓN

absorcion (Disyuncion prop1 (Conjuncion prop2 prop3)) = if (prop1 == prop2 || prop1 == prop3) then prop1 else Disyuncion (simplProp prop1) (Conjuncion (simplProp prop2) (simplProp prop3)) -- P v (P ^ Q) --- P v (Q ^ P)
absorcion (Disyuncion (Conjuncion prop1 prop2) prop3) = if (prop1 == prop3 || prop2 == prop3) then prop1 else Disyuncion (Conjuncion (simplProp prop2) (simplProp prop3)) (simplProp prop1) -- (P ^ Q) v P --- (Q ^ P) v P
absorcion (Conjuncion prop1 (Disyuncion prop2 prop3)) = if (prop1 == prop2 || prop1 == prop3) then prop1 else Conjuncion (simplProp prop1) (Disyuncion (simplProp prop2) (simplProp prop3)) -- P ^ (P v Q) --- P ^ (Q v P)
absorcion (Conjuncion (Disyuncion prop1 prop2) prop3) = if (prop1 == prop3 || prop2 == prop3) then prop3 else Conjuncion (simplProp prop1) (Disyuncion (simplProp prop2) (simplProp prop3)) -- (P v Q) ^ P --- (Q v P) ^ P

--DOMINACIÓN
dominacion (Conjuncion prop1 (Constante False)) = Constante False -- P ^ False
dominacion (Conjuncion (Constante False) prop1) = Constante False -- False ^ P
dominacion (Disyuncion prop1 (Constante True)) = Constante True   -- P v True
dominacion (Disyuncion (Constante True) prop1) = Constante True   -- True v P

--NEUTRO

neutro (Disyuncion prop1 (Constante False)) = prop1 -- P v False
neutro (Disyuncion (Constante False) prop1) = prop1 -- False v P
neutro (Conjuncion prop1 (Constante True)) =  prop1   -- P ^ True
neutro (Conjuncion (Constante True) prop1) =  prop1   -- True ^ P

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
        (Conjuncion prop1 (Constante False))        -> --  DOMINACIÓN REGLA 9
            dominacion prop
        (Conjuncion (Constante False) prop1)        -> -- DOMINACIÓN  REGLA 10
            dominacion prop
        (Disyuncion prop1 (Constante True))         -> --  DOMINACIÓN REGLA 11
            dominacion prop
        (Disyuncion (Constante True) prop1)         -> --  DOMINACIÓN REGLA 12
            dominacion prop
        (Disyuncion prop1 (Constante False))        -> --  NEUTRO REGLA 13
            neutro prop
        (Disyuncion (Constante False) prop1)        -> --  NEUTRO REGLA 14
            neutro prop
        (Conjuncion prop1 (Constante True))         -> --  NEUTRO REGLA 15
            neutro prop
        (Conjuncion (Constante True) prop1)         -> --  NEUTRO REGLA 16
            neutro prop
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
