module Aufgabe where 
   
   
   
    fun1 ::  (Eq t) => [t] -> Bool
    fun1 xs = (xs==[])

    fun2 :: (Foldable x) => (t->String->String)->s->(x(t)->String)
    fun2 f a = foldr f "a"