module ProgramaPrin where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp
import Taut
import Bonita
--import PruebaFNC
import Simpl
import FNC

p = Variable "p"
q = Variable "q"
r = Variable "r"

prueba1 = p =>: q
prueba2 = (p ||: (~:) p) &&: (q ||: (~:)q)
prueba3 = (((~:) p ||: q) =>: (r ||: p)) ||: (p =>: q)
prueba4 = ((~:) p ||: q) =>: ((~:) q &&: p)
prueba5 = (p =>: (q ||: r)) <=>: ((~:)p &&: r)
pruebaequivlog = (~:)(((~:) p &&: (~:) q) &&: ((~:)q =>: r)) &&: ((~:) r =>: p)

--PRUEBAS PARA SIMPL

p1 = p ||: (p &&: q)
p2 = p ||: (q &&: p)
p3 = (p &&: q) ||: p
p4 = (q &&: p) ||: p
p5 = p &&: (p ||: q)
p6 = p &&: (q ||: p)
p7 = (p ||: q) &&: p
p8 = (q ||: p) &&: p
p9 = p ||: (((r ||: q) &&: r) &&: p)