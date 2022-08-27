data CannotShow = CannotShow
    deriving (Show)

data CannotDeriveShow = CannotDeriveShow
                        deriving (Show)

data OK = OK
instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK 
                deriving (Show)

    
