module Utils
( offv, ofv)
where

offv :: (b->b->c) -> (a->b) -> (a->b) -> a -> c
offv f g h x = f (g x) (h x)

ofv :: (a->b->c) -> (a->b) -> a -> c
ofv f g x = f x $ g x
