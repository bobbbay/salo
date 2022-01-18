--- A basic Salo file.
--- Three-dash comments are documentation comments. Easy!
--- Since this is at the top, it documents the whole module.
module configurations.somefile

{- Here's a multiline comment, by the way. -}

{-- This is a multiline doc comment. This is a type decl: --}
x : Int

-- And the type value:
x = 1

-- You can doc comment either, but not both.

-- You can also omit the type decl in situations where we can infer the type:
y = 5 -- clearly an Int

--- Apply a function
z = addOne 1

--- Here, we take some polymorphic argument x and add one to it. Hence, x needs to be available in the addition function.
addOne x = x + 1

-- If z is 2, then the types is True (a Bool). Else, it's 5 (an Int).
someVal = if z == 2 then True else 5

-- Product types:
myproduct : (Int, Int)

-- Sum types are represented as dependent types:
data MyType = Data String | Info Int

-- And can also be recursive, like natural numbers:
data Nat = Z | S Nat

-- We have a type for types!
-- The type for Nat is Type.
-- The type for Z or S is Nat.

-- At the end, we need to return *something* - that's the eval value of the file.
-- Let's return a unit for now:
()

-- That's all!
