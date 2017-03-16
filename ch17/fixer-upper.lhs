make the following typecheck

1. const <$> Just "Hello" <*> "World"

> foo = const <$> Just "Hello" <*> pure "World"

2. (,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]

> bar = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

