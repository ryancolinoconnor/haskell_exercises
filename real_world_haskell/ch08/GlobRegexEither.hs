type GlobError =  String
globToRegex :: String -> Prelude.Either GlobError String
globToRegex a = if a=="Left" then (Left "Sample") else (Right "Other")
--
--globToRegex a = Right "other"
-- see exercises.hs 

sample_handler a = case (globToRegex a) of
                    Left b  ->"left" 
                    Right a ->"right" ++ a
