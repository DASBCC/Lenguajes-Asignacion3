module Pruebas where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp
import Taut
import Bonita
import Fnc

p = Variable "p"
q = Variable "q"
r = Variable "r"

prueba1 = p =>: q
prueba2 = (p ||: (~:) p) &&: (q ||: (~:)q)
prueba3 = (((~:) p ||: q) =>: (r ||: p)) ||: (p =>: q)
