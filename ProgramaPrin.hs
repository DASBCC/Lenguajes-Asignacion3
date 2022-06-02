module Pruebas where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp
import Taut
import Bonita
import Fnc

import PruebaFNC
p = Variable "p"
q = Variable "q"
r = Variable "r"

prueba1 = p =>: q
prueba2 = (p ||: (~:) p) &&: (q ||: (~:)q)
prueba3 = (((~:) p ||: q) =>: (r ||: p)) ||: (p =>: q)

pruebafnc = (~:)(((~:) p &&: (~:) q) &&: ((~:)q =>: r)) &&: ((~:) r =>: p)

prueba (Disyuncion prop1 prop2) = Disyuncion prop1 (dis prop2)