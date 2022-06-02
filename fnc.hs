module Fnc where

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
ne (Disyuncion prop1 (Constante prop2)) = if (Constante(prop2) == Constante(False)) then prop1 else Disyuncion prop1 (Constante prop2)
ne (Conjuncion prop1 (Constante prop2)) = if (Constante(prop2) == Constante(True)) then prop1 else Conjuncion prop1 (Constante prop2)
--CASO ALTERNO
--DBEERÍA EVALUARLO CON
--ne (Disyuncion (Constante prop1) prop2) = if (Constante(prop1) == Constante(False)) then prop2 else Disyuncion (Constante prop1) prop2
--ne (Conjuncion (Constante prop1) prop2) = if (Constante(prop1) == Constante(True)) then prop2 else Conjuncion (Constante prop1) prop2


--INVERSOS
inv (Disyuncion prop1 (Negacion prop2)) = if (prop1 == prop2) then Constante(True) else Disyuncion prop1 (Negacion prop2)
inv (Conjuncion prop1 (Negacion prop2)) = if (prop1 == prop2) then Constante(False) else Conjuncion prop1 (Negacion prop2)
--CASO ALTERNO
--DEBERÍA EVALUARLO CON

--ABSORCIÓN
abs (Disyuncion prop1 (Conjuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Disyuncion prop1 (Conjuncion prop2 prop3)
abs (Conjuncion prop1 (Disyuncion prop2 prop3)) = if (prop1 == prop2) then prop1 else Conjuncion prop1 (Disyuncion prop2 prop3)

--EXPORTACIÓN
exp (Implicacion prop1 (Implicacion prop2 prop3)) = Implicacion (Conjuncion prop1 prop2) prop3
--NOTA COLOCAR FALSE EN LOS ELSE PARA SABER QUE LA EVALUACIÓN DEBE CONTINUAR