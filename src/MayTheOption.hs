module MayTheOption where


then_::Maybe a -> (a -> Maybe b) -> Maybe b
then_  (Just x) y = y x
then_ Nothing _ = Nothing

flatten:: Maybe (  Maybe c ) -> Maybe c

flatten (Just  z) = z
flatten (Nothing) = Nothing