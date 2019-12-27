-- safer fromJust

fromJust' :: String -> Maybe a -> a
fromJust' s Nothing = error s
fromJust' s (Just x) = x
