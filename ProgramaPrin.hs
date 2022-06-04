module ProgramaPrin where
import Vars
import Sintax
import GenBools
import AsVals
import EvalProp
import Taut
import Bonita
import Simpl
import FNC

p = Variable "p"
q = Variable "q"
r = Variable "r"
true = Constante True
false = Constante False


--CASOS DE PRUEBA FNC Y BONITA
prueba1 = (p ||: (~:) p) &&: (q ||: (~:)q)
prueba2 = (((~:) p ||: q) =>: (r ||: p)) ||: (p =>: q)
prueba3 = ((~:) p ||: q) =>: ((~:) q &&: p)
prueba4 = (p =>: (q ||: r)) <=>: ((~:)p &&: r)
pruebaequivlog = (~:)(((~:) p &&: (~:) q) &&: ((~:)q =>: r)) &&: ((~:) r =>: p)

--PRUEBAS PARA SIMPL
--CASOS BÁSICOS ABSORCIÓN
abs1 = p ||: (p &&: q)
abs2 = p ||: (q &&: p)
abs3 = (p &&: q) ||: p
abs4 = (q &&: p) ||: p
abs5 = p &&: (p ||: q)
abs6 = p &&: (q ||: p)
abs7 = (p ||: q) &&: p
abs8 = (q ||: p) &&: p

--CASOS BÁSICOS DOMINACIÓN
dom1 = p &&: false
dom2 = false &&: p
dom3 = p ||: true
dom4 = true ||: p

--CASOS BÁSICOS NEUTRO
ne1 = p ||: false
ne2 = false ||: p
ne3 = p &&: true
ne4 = true &&: p

--CASOS COMBINADOS
--COMBINA ABSORCIÓN ((r ||: q) &&: r) CON ABSORCIÓN p ||: (((r ||: q) &&: r) &&: p)
prueba5 = p ||: (((r ||: q) &&: r) &&: p)

--COMBINA NEUTRO (p &&: true) CON DOMINACIÓN (p &&: false) CON NEUTRO (p &&: true) ||: (p &&: false)
prueba6 = (p &&: true) ||: (p &&: false)

--COMBINA DOMINACIÓN (q ||: true) CON NEUTRO (p ||: false) CON ABSORCIÓN (r &&: (r ||: (p ||: false)))
--CON ABSORCIÓN (r &&: (q ||: (r &&: (r ||: (p ||: false)))))
prueba7 = (( p &&: r) ||: (q ||: true)) &&: (r &&: (q ||: (r &&: (r ||: (p ||: false)))))
