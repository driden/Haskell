-- Agustin Guerra - Luis Loureiro
-- 196344 - 191659
module Lab1 where

    import Prelude
    
    type Var = String
    
    --EJERCICIO 1.1--
    
    -- 1.1.1
    
    data BinCon = And | Or | Impl | Eq deriving (Eq, Show)
    
    
    data Form = L Var |
                Not Form | 
                Bc BinCon Form Form deriving (Eq, Show)
    
    -- Constantes para Debug
    p :: Form
    p = Not ( Bc And (L "p") ( Not (Not(L "p"))))
    
    q :: Form
    q = Bc Impl (L "q") (L "q")
    
    s :: Form
    s = Not (Bc And (L "s") (Not (Bc Or (L "s") (L "k") )))
    -- 1.1.2
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
    listarProp :: Form -> [Var]
    listarProp (L q)        = [q]
    listarProp (Not q)      = listarProp q
    listarProp (Bc bin q r) = mergeArray (listarProp q) (listarProp r)
    
    --2.7--
    sustCon :: BinCon -> BinCon -> Form ->  Form
    sustCon c1 c2 (L q)        = L q
    sustCon c1 c2 (Not q)      = Not (sustCon c1 c2 q)
    sustCon c1 c2 (Bc bin q r) = if bin == c1 then Bc c2 (sustCon c1 c2 q) (sustCon c1 c2 r) 
                                 else Bc bin (sustCon c1 c2 q) (sustCon c1 c2 r)
    
    --2.8--
    swapCon :: BinCon -> BinCon -> Form -> Form
    swapCon c1 c2 (L q)        = L q
    swapCon c1 c2 (Not q)      = Not (swapCon c1 c2 q)
    swapCon c1 c2 (Bc bin q r) = if bin == c1 then
                                    Bc c2 (swapCon c1 c2 q) (swapCon c1 c2 r)
                                else if bin == c2 then
                                    Bc c1 (swapCon c1 c2 q) (swapCon c1 c2 r)
                                else 
                                    Bc bin (swapCon c1 c2 q) (swapCon c1 c2 r)
    
    --2.9--
    dobleNeg :: Form -> Form
    dobleNeg = \e -> case e of {
        L p -> L p;
        Bc bin t s -> Bc bin (dobleNeg(t)) (dobleNeg(s));
        Not t -> case t of {
            L p -> Not (L p);
            Bc binn l m -> Not (Bc binn (dobleNeg(l)) (dobleNeg(m)));
            Not q -> dobleNeg(q);
        }
    }
    
    --2.10--
    sustSimp :: Var -> Form -> Form -> Form
    sustSimp = \e m n -> case n of {
        L k -> if k==e then m else L k;    
        Not p -> Not (sustSimp e m p);
        Bc bin t r -> Bc bin (sustSimp e m t) (sustSimp e m r);
    }
    
    
    buscarOcurrencia :: [(Var, Form)] -> Var -> (Form,Bool)
    buscarOcurrencia = \lista variable -> case lista of {
        [] -> (L "q",False);
        (v,f):xs -> if v==variable then (f,True) else buscarOcurrencia xs variable
    }
    
    arr :: [(Var, Form)]
    arr = [("q",Not (L "q")),("p",Not (L "p")),("t",Not (L "t")),("r",Not (L "r"))]
    
    --2.11--
    sustMult :: [(Var, Form)] -> Form -> Form
    sustMult = \lista formula  -> case formula of {
        L p -> case buscarOcurrencia lista p of {
            (r,True) ->    r;
            (m,False) ->  L p;
        };
        Not q -> Not (sustMult lista q);
        Bc bin z x -> Bc bin (sustMult lista z) (sustMult lista x);
    }
    
    --Auxiliares--
    
    inArray :: Var -> [Var] -> Bool
    inArray e arr = case arr of {
        [] -> False;
        x:xs -> (e == x) || inArray e xs;
    }
    
    mergeArray :: [Var] -> [Var] -> [Var]
    mergeArray [] ys = ys
    mergeArray xs [] = xs
    mergeArray (x:xs) ys = 
                    if inArray x xs || inArray x ys then 
                        mergeArray xs ys 
                    else x:mergeArray xs ys