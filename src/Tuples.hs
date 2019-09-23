module Tuples where


nestem :: ( a, b, c ) -> (a , (b, c))
nestem (a, b, c ) = (a, (b,c))

unnestem :: (a, (b,c)) -> (a, b, c)
unnestem (a, (b,c)) = (a, b, c)

