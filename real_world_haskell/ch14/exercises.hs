-- 1.1 Rewrite getrandom to use do notation:TmuxNavigateRight
-- couldn't really test it as the state monad
-- didn't have usage samples
-- though this is ultimately basicall the same as the code on the next page
getRandomDo :: Random a => RandomState a
getRandomDo =
    do 
       gen <- get
       let (val,gen') = random gen
       put gen'
       return val
