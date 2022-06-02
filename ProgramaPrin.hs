module ProgramaPrin where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp
import Taut
import Bonita
--import PruebaFNC
--import EquivLog
import FNC

p = Variable "p"
q :: Proposicion
q = Variable "q"
r = Variable "r"

prueba1 = p =>: q
prueba2 = (p ||: (~:) p) &&: (q ||: (~:)q)
prueba3 = (((~:) p ||: q) =>: (r ||: p)) ||: (p =>: q)
prueba4 = ((~:) p ||: q) =>: ((~:) q &&: p)
pruebaequivlog = (~:)(((~:) p &&: (~:) q) &&: ((~:)q =>: r)) &&: ((~:) r =>: p)