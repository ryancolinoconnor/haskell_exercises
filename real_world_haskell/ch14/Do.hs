
doNotation1 = 
    do act

translated1 = 
    act

doNotation2 =
    do act1
       act2
       actN

finalTranslation2 =
    act1 >>
    act2 >>
    actN

doNotation3 = 
    do pattern <- act1
        act2
        ...
        actN

translated3 =
    let f pattern = do act1
        let f pattern = do act2
      f _ = fail "..."
    in act1 >>= f

robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
            return x
