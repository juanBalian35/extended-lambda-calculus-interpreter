type Branch = (String, Exp)
data Exp = Var String | 
           Cons String | 
           Lambda [String] Exp | 
           Apli Exp [Exp] |
           Case Exp [Branch] |
           Rec Exp
           deriving (Show)

isWeak :: Exp -> Bool
isWeak (Cons s) = True
isWeak (Apli (Cons s) (x:xs)) = True
isWeak (Lambda xs e) = True
isWeak x = False

type Substitution = [(String, Exp)]

subst :: Exp -> Substitution -> Exp
subst (Var x) s
  | Just y <- lookup x s = y
  | otherwise = Var x
subst (Cons x) s = Cons x
subst (Lambda xs e) s = Lambda xs (subst e (filter (\(a,b) -> not (elem a xs)) s))
subst (Apli e xe) s = Apli (subst e s) (map (\a -> subst a s) xe)
subst (Case e xr) s = Case (subst e s) (map (\(ra,rb) -> (ra, subst rb s)) xr)
subst (Rec e) s = Rec (subst e s)

reduction :: Exp -> Exp
reduction (Apli (Lambda ss e) es)
  | length(ss) == length(es) = subst e (zip ss es)
  | otherwise = error("Cantidad de parametros debe ser igual a cantidad de argumentos.")
reduction (Apli (Apli (Cons s) esi) eso) = Apli (Cons s) (esi ++ eso)
reduction (Apli e es) = Apli (reduction e) es
reduction (Case (Cons s) rs)
  | Just y <- lookup s rs = y
  | otherwise =  error("Constructor " ++ s ++ " no se encuentra en ramas")
reduction (Case (Apli (Cons s) es) rs)
  | Just y <- lookup s rs = Apli y es
  | otherwise =  error("Constructor " ++ s ++ " no se encuentra en ramas")
reduction (Case e rs) = Case (reduction e) rs;
reduction (Rec e) =  Apli e [Rec e]

weval :: Exp -> Exp
weval e
  | isWeak e = e
  | otherwise = weval (reduction e)

eval :: Exp -> Exp
eval e
  | Lambda xs e <- weakeval = Lambda xs e
  | Cons s <- weakeval = Cons s
  | Apli (Cons s) (e:es) <- weakeval = Apli (Cons s) (map(\x -> eval x) (e:es))
  where weakeval = weval e

nnot = Lambda ["b"] (Case (Var "b") [("True", Cons "False"), ("False", Cons "True")])
par = Rec(Lambda ["par"] (Lambda ["n"] (Case (Var "n") [("0", Cons "True"),("S", Lambda ["x"] (Apli (nnot) [(Apli (Var "par") [Var "x"])]))])))

-- eval(Apli par [Cons "0"])                                           -> Cons "True"
-- eval(Apli par [Apli (Cons "S") [Cons "0"]])                         -> Cons "False"
-- eval(Apli par [Apli (Cons "S") [Apli (Cons "S") [Cons "0"]]])       -> Cons "True"