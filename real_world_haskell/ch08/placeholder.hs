
namesMatching :: String -> IO (Prelude.Either GlobError [FilePath])
namesMatching pat  
                    | not (isPattern pat) = do
                        exists <- doesNameExist pat
                        return (if exists  then Right [pat] else Right [])
                    | otherwise = do
                        case splitFileName pat of
                          ("",baseName) -> do
                              curDir <- getCurrentDirectory
                              listMatches curDir baseName
                          (dirName, baseName) -> do
                              dirs <- if isPattern dirName
                                then namesMatching (dropTrailingPathSeparator dirName)
                                else if (take 2 baseName)=="**"
                                    then
                                        do
                                        subdirs <- getSubDirs dirName
                                        return  (Right (subdirs))
                                    else return (Right [dirName])
                              let listDir = if take 2 baseName == "**" 
                                            then listMatches  . tail 
                                            else if isPattern baseName
                                                then listMatches
                                            else listMatches
                              pathNames <- (either_baseNames dirs listDir)
                              case pathNames of
                                Left err -> return $ Left err
                                Right pns -> return $Right (concat pathNames)
