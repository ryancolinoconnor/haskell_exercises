bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
base_nodesAreSame _           _ = Nothing

-- add in some guards
nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreSame _ _ = Nothing
