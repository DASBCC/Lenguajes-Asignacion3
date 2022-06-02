module PruebaFNC where

import Sintax

eliminaEquivalencias :: Proposicion -> Proposicion
eliminaEquivalencias (Variable f)   = 
    (Variable f)
eliminaEquivalencias (Negacion f)    = 
    Negacion (eliminaEquivalencias f) 
eliminaEquivalencias (Conjuncion f g) = 
    Conjuncion (eliminaEquivalencias f) (eliminaEquivalencias g) 
eliminaEquivalencias (Disyuncion f g) = 
    Disyuncion (eliminaEquivalencias f) (eliminaEquivalencias g) 
eliminaEquivalencias (Implicacion f g) = 
    Implicacion (eliminaEquivalencias f) (eliminaEquivalencias g) 
eliminaEquivalencias (Equivalencia f g) = 
    Conjuncion (Implicacion (eliminaEquivalencias f) (eliminaEquivalencias g))
         (Implicacion (eliminaEquivalencias g) (eliminaEquivalencias f))

eliminaImplicaciones :: Proposicion -> Proposicion
eliminaImplicaciones (Variable f)   = 
    (Variable f)
eliminaImplicaciones (Negacion f)    = 
    Negacion (eliminaImplicaciones f) 
eliminaImplicaciones (Conjuncion f g) = 
    Conjuncion (eliminaImplicaciones f) (eliminaImplicaciones g) 
eliminaImplicaciones (Disyuncion f g) = 
    Disyuncion (eliminaImplicaciones f) (eliminaImplicaciones g) 
eliminaImplicaciones (Implicacion f g) = 
    Disyuncion (Negacion (eliminaImplicaciones f)) (eliminaImplicaciones g) 

interiorizaNegacion :: Proposicion -> Proposicion
interiorizaNegacion (Variable f)   = 
    (Variable f)
interiorizaNegacion (Negacion f)    = 
    interiorizaNegacionAux f
interiorizaNegacion (Conjuncion f g) = 
    Conjuncion (interiorizaNegacion f) (interiorizaNegacion g) 
interiorizaNegacion (Disyuncion f g) = 
    Disyuncion (interiorizaNegacion f) (interiorizaNegacion g) 
 
interiorizaNegacionAux :: Proposicion -> Proposicion
interiorizaNegacionAux (Variable f)   = 
    Negacion (Variable f)
interiorizaNegacionAux (Negacion f)    = 
    interiorizaNegacion f 
interiorizaNegacionAux (Conjuncion f g) = 
    Disyuncion (interiorizaNegacionAux f) (interiorizaNegacionAux g) 
interiorizaNegacionAux (Disyuncion f g) = 
    Conjuncion (interiorizaNegacionAux f) (interiorizaNegacionAux g) 

formaNormalNegativa :: Proposicion -> Proposicion
formaNormalNegativa f =
    interiorizaNegacion (eliminaImplicaciones (eliminaEquivalencias f))

interiorizaDisyuncion :: Proposicion -> Proposicion
interiorizaDisyuncion (Disyuncion (Conjuncion f1 f2) g) =
    interiorizaDisyuncion 
    (Conjuncion (Disyuncion (interiorizaDisyuncion f1) (interiorizaDisyuncion g))
          (Disyuncion (interiorizaDisyuncion f2) (interiorizaDisyuncion g)))
interiorizaDisyuncion (Disyuncion f (Conjuncion g1 g2)) =
    interiorizaDisyuncion
    (Conjuncion (Disyuncion (interiorizaDisyuncion f) (interiorizaDisyuncion g1))
          (Disyuncion (interiorizaDisyuncion f) (interiorizaDisyuncion g2)))
interiorizaDisyuncion (Conjuncion f g) =
    Conjuncion (interiorizaDisyuncion f) (interiorizaDisyuncion g)
interiorizaDisyuncion f = f

formaNormalConjuntiva :: Proposicion -> Proposicion
formaNormalConjuntiva f =
    interiorizaDisyuncion (formaNormalNegativa f)