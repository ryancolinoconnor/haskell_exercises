[("name", Just "Attila \" The Hun\""),
("occupation", Just "Khan")]

data MoviewReview = MoviewReview {
    revTitle :: String
  , revUser :: String
  , revReview :: String 
                                 }

simpleReview :: [(String, Maybe String)] -> Maybe MoviewReview
simpleReview alist = 
    case lookup "title" alist of
      Just (Just title@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
                Just (MoviewReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- No users
    _ -> Nothing -- no title

maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MoviewReview title user review)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing

liftedReview alist = 
    liftM3 MoviewReview (lookup1 "title" alist)
                        (lookup1 "user" alist)
                        (lookup1 "review" alist)

