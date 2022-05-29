module Pruebas where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp


p = Variable "p"
q = Variable "q"
prueba = (p ||: (~:) p) &&: (q ||: (~:)q)