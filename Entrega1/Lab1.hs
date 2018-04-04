module Lab1 where
    import Prelude
    
    type Var = String
    
    --EJERCICIO 1.1--
    
    data BinCon =  And| Or | Impl
        deriving (Show, Eq)
    data Form = L Var | Not Form | Bc BinCon Form Form
        deriving (Show, Eq)
       
    --EJERCICIO 1.2--
    --a)
    uno :: Form
    uno = Bc Impl (L "p") ( Not ( Bc And (L "q") ( Not (Not(L "r")))))
    --b)
    dos :: Form
    dos = Bc Impl (Bc And (L "t") (Not (L "e"))) (Bc And (L "p") (Not (L "f")))
    --c)
    tres :: Form
    tres = Bc And (Bc Impl (L "p") (L "q")) (Bc Impl (L "q") (Bc And (Not (L "r")) (Not (L "q"))))
    
    --2.1--
    valores :: Form -> [(Var,Bool)]
    valores (L p) = [(p, True)]
    valores (Not r) = case r of {
        L q -> [(q,False)];
        Not k -> valores r;
        Bc bin m p -> valores m ++ valores p;
    }
    valores (Bc bin s t) = valores s ++ valores t 
    
    {-  data BinCon =  And| Or | Impl
        data Form = L Var | Not Form | Bc BinCon Form Form-}
    --2.2--
    cantNeg :: Form -> Int
    cantNeg (L q) = 0
    cantNeg (Not p) = 1 + cantNeg p
    cantNeg (Bc bin w r) = cantNeg w + cantNeg r
    
    --2.3--
    cantBin :: Form -> Int
    cantBin (L q) = 0
    cantBin (Not q) = cantBin q
    cantBin (Bc bin p q) = 1 + cantBin p + cantBin q
    
    --2.4--
    -- a ⊃ b≈ ¬a ∨ b
    cambiar :: Form -> Form
    cambiar (L q)        = L q
    cambiar (Not q)      = Not (cambiar q)
    cambiar (Bc bin q r) = case bin of {
        Impl -> Bc Or (Not (cambiar q)) (cambiar r);
        _ -> Bc bin (cambiar q) (cambiar r)
    }
    
    --2.5--
    cantProp :: Form -> Int
    cantProp (L q)        = 1
    cantProp (Not q)      = cantProp q
    cantProp (Bc bin q r) = cantProp q + cantProp r
    
    --2.6--
    listarRepe :: Form -> [Var]
    listarRepe (L q)        = [q]
    listarRepe (Not q)      = listarProp q
    listarRepe (Bc bin q r) = listarProp q ++ listarProp r
----- HACEr BIEN ESTO QUE ESTA MALLL ------------
    listarProp :: Form -> [Var]
    listarProp (L q)        = [q]
    listarProp (Not q)      = listarProp q
    listarProp (Bc bin q r) = listarProp q ++ listarProp r
    
    --2.7--
    sustCon :: BinCon -> BinCon -> Form ->  Form
    sustCon c1 c2 (L q)        = L q
    sustCon c1 c2 (Not q)      = Not (sustCon c1 c2 q)
    sustCon c1 c2 (Bc bin q r) = if bin == c1 then Bc c2 (sustCon c1 c2 q) (sustCon c1 c2 r) 
                                 else Bc bin (sustCon c1 c2 q) (sustCon c1 c2 r)
    {-
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

    inArray :: Var -> [Var] -> Bool
    inArray e arr = case arr of {
        [] -> False;
        x:xs -> (e == x) || inArray e xs;
    }