module AsVals where

-- dada una lista de variables proposicionales sin repeticiones, la combina  con una lista de valores booleanos
as_vals :: [var] -> [bool] -> [(var, bool)]
as_vals _     []     = []
as_vals []     _     = []
as_vals (x:xs) (y:ys) = (x, y) : as_vals xs ys