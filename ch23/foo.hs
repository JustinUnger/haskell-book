
xs <- get


get >>= (\xs -> put (result : xs))

State (\s -> (s,s)) >>= (\xs -> put (result:xs))

State (\s -> (s,s)) >>= (\xs -> State (\_ -> ((), (result:xs))))

f = (\s -> (s,s))
g = (\xs -> State (\_ -> ((), (result:xs))))

State $ (\s -> let (a,s') = (\s -> (s,s)) s in runState (g a) s'

a ~ s
s' ~ s

reduces to: 

State $ (\s -> runState (g s) s)

(g s) ~ (\xs -> State (\_ -> ((), (result:xs)))
(\s -> State (\_ -> ((), result:s)))
State (\_ -> ((), result:s))

(\_ -> ((), result:s))
((), result:s)


runState (g a) s'

runState ((\xs -> State (\_ -> ((), result:xs)) s) s

