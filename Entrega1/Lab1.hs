module Lab1 where
    import Prelude
    
    type Var = String
    
    --EJERCICIO 1.1--
    
    data BinCon =  And| Or | Impl
        deriving (Show, Eq)
    data Form = L Var | Not Form | Bc BinCon Form Form
        deriving (Show, Eq)
    p = Not (Bc And (L "p") (L "q"))    
    --EJERCICIO 1.2--
    --a)
    uno :: Form
    uno = Bc Impl (L "p") ( Not ( Bc And (L "q") ( Not (Not(L "r")))))
    --b)
    dos :: Form
    dos = Bc Impl (Bc And (L "t") (Not (L "e"))) (Bc And (L "e") (Not (L "f")))
    --c)
    tres :: Form
    tres = Bc And (Bc Impl (L "p") (L "q")) (Bc Impl (L "q") (Bc And (Not (L "r")) (Not (L "q"))))
    
    --2.1--
    valores :: Form -> [(Var,Bool)]
    valores (L p) = [(p, True)]
    valores (Not r) = case r of {
        L q -> [(q,False)];
        Not k -> valores r;
        Bc bin m p -> (valores m) ++ valores p;
    }
    valores (Bc bin s t) = (valores s) ++(valores t) 
    
    {-
    --2.2--
    cantNeg :: Form -> Int
    cantNeg = undefined
    
    --2.3--
    cantBin :: Form -> Int
    cantBin = undefined
    
    --2.4--
    cambiar :: Form -> Form
    cambiar = undefined
    
    --2.5--
    cantProp :: Form -> Int
    cantProp = undefined
    
    --2.6--
    listarProp :: Form -> [Var]
    listarProp = undefined
    
    --2.7--
    sustCon :: BinCon -> BinCon -> Form ->  Form
    sustCon = undefined
    
    --2.8--
    swapCon :: BinCon -> BinCon -> Form -> Form
    swapCon = undefined
    
    --2.9--
    dobleNeg :: Form -> Form
    dobleNeg = undefined
    
    --2.10--
    sustSimp :: Var -> Form -> Form -> Form
    sustSimp = undefined
    
    --2.11--
    sustMult :: [(Var, Form)] -> Form -> Form
    sustMult = undefined
    -}