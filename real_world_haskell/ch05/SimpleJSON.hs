module SimpleJSON
    (
    JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where


    data JValue = JString String
                |JNumber Double
                |JBool Bool
                |JNull
                |JObject [(String, JValue)]
                | JArray [JValue]
                deriving (Eq, Ord, Show)


    getString:: JValue -> Maybe String
    getString (JString s) = Just s
    getString _ = Nothing


    getInt:: JValue -> Maybe Int
    getInt (JNumber s) = Just (truncate s)
    getInt _ = Nothing


    getDouble:: JValue -> Maybe Double
    getDouble (JNumber s) = Just (s)
    getDouble _ = Nothing

    getBool:: JValue -> Maybe Bool
    getBool (JBool s) = Just (s)
    getBool _ = Nothing

    getObject (JObject s) = Just (s)
    getObject _ = Nothing

    getArray (JArray s) = Just (s)
    getArray _ = Nothing

    isNull v = v==JNull
