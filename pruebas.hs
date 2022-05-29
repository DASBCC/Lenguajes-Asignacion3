module Pruebas where
import Vars
import Sintax
--import AsVals
import GenBools
import AsVals
p = Variable "p"
q = Variable "q"
prueba = (p ||: (~:) p) &&: (q ||: (~:)q)