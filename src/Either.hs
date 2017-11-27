module Either (join) where

join :: Either a a -> a
join (Left e)=e
join (Right e)=e
