--- Salo supports typed polymorphism, allowing multiple values to be passed on
--- to one function. Salo type checks each polymorphic call before evaluation.
--- This example demonstrates the usage of Salo's polymorphism.
module examples.polymorphism

--- A funtion that takes an int and returns it.
fInt : Int -> Int
fInt x = x

fStr : String -> String
fStr x = x

-- ... and so on. Note that the function bodies are the same: the type
-- signature is the only difference. Wouldn't it be nice if we could have a
-- one-size-fits-all definition?

--- We can!
f : a -> a
f x = x

-- Salo polymorphism is very similar to Haskell's type families. Above, we
-- created a polymorphic type `a` with no constraints.

--- We can also constrain a polymorphic type.
g :: Show a : a -> String
-- In here, we can prove that `a` is `show`-able, and hence can run `show` on
-- it.
g = show a

()
