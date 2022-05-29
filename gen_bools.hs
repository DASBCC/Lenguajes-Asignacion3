module Gen_bools where
import Sintax
import Vars
import Data.Map (Map)

--Construye una lista curryficada
cons :: Bool -> [Bool]-> [Bool]
cons x xs = x : xs

-- Genera todas las combinaciones posibles de booleanos a partir de un n indicado
gen_bools :: Int -> [[Bool]]
gen_bools 0 = [[]]
gen_bools n = let anterior = gen_bools (n - 1)
                in (map (cons True) anterior) ++ (map (cons False) anterior)

