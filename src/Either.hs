module Either where
  
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

splitFstJoinSnd :: Either (a1, b) (a2, b) -> (Either a1 a2, b)
splitFstJoinSnd (Left (a,b)) = (Left a, b)
splitFstJoinSnd (Right (a,b)) = (Right a, b)

join :: Either a a -> a
join (Left e)=e
join (Right e)=e
