# Syntax

This section outlines the basics of Salo's syntax.

## The REPL

Before starting, let's get familiar with Salo's REPL. Run `salo` with no arguments in the command line (once Salo is installed) to open up the prompt:

```salo-repl
> Welcome to the Salo REPL!
>
```

The REPL supports a few commands:

 * `:t expr`: get the type of `expr`
 * `:a expr`: get the generated AST of `expr`
 * `:q`: quit

Run `:h` for more information.

Now that we're familiar with the REPL, we can continue to learning Salo's configuration language. From this point on, every code block that begins with `>` is run in the REPL.

## Types

Salo has two primitive types: `Int` and `Str`. More types are defined in the standard library, such as:

 * Bool
 * Nat
 * Array
 * Derivation

Values are created with a type signature and value, as such:

```salo
a : String
a = "Hello, world!"
```

This is very similar to ML syntax. However, unlike many other ML languages, top-level values are not type inferred - you must explicitly write out the type of the value, whether or not it was possible for the computer to infer it.

Uninitialized values in Salo are invalid and will throw a compiler error. This means that code like this won't work:

```salo
b : String;
-- What is b's value?
```

But, if you enter Salo in edit mode, `b` will be valid - or rather, it will be a hole. Holes are uninitialized values that allow you to understand more about the type requirements, without writing an implementation. We'll talk more about holes in [[Holes]].

Of course, Salo is also okay with delegating assignment until later on, as long as that value is not used:

```salo
b : String;

-- Other things can happen here, as long as they don't call `b`...

b = "Hello, world!";
```

### A side note on declaring custom types

Salo has so little primitives thanks to its robust system of declaring types. To better understand how these work internally, let's examine how they're put to use in the standard library. Here's the simple declaration for natural numbers:

```salo
type Nat = Z | S Nat;
```

Here we see that a natural number can either be `Z` - Zero, or `S Nat` - a successor to another Nat. For example, =4= is a successor to =3=, which is a successor to =2=, which is a successor to =1=, which is a successor to =0=, which is =Z=. This allows users to construct non-primitive types that are as expressive as the language itself.

## Functions

Functions are defined in a slightly similar syntax:

```salo-repl
> f : String -> String;
> f x = x;
```

If you're unfamiliar with ML syntax, this defines a function that takes a String and returns a String. In the implementation, `f` takes `x` and returns `x` without modifications.

### Currying

Salo's functions curry. Take the following code example (note the REPL prompt):

```salo-repl
> :t f
f : String -> String

> g : String -> String -> String
> g x y = x + y

> :t g
g : String -> String -> String

> :t g "Hello, "
g "Hello, " : String -> String
```

In the beginning, we check the type of =f= (defined above), and see that it's a =String -> String=: straightforward enough. Next, we declare =g= to have a type of =String -> String -> String=, implementing it to take two strings and produces their concatenation. We know that the type of =g= is =String -> String -> String=, but what's cool is that the type of =g "Hello, "= is a =String -> String=!

### Pattern matching

Salo supports pattern matching, e.g.:

```salo
name : Bool -> String
name True = "Bob"
name False = "Jeffrey"
```

In this case, if the Bool given to `name` is true, it will evaluate to "Bob". If it is given false, then it will evaluate to "Jeffrey".

Salo pattern matches /must/ be exhaustive. This means that the following won't work:

```salo
isOne : Int -> Bool
isOne 1 = True
```

Salo will complain /during compile time/ that this match does not cover every variant. What if we pass on 5, 6, or 7? Salo has no idea what to evaluate to. This, however, will work:

```salo
isOne : Int -> Bool
isOne 1 = True
isOne _ = False
```

With the `_` character, Salo can match every other variant.

### Polymorphic arguments

Functions don't have to have strict types - with polymorphism, we're able to allow any type to pass into our program, as long as the function definition is "generic" enough:

```salo
genericAdder : a -> a -> a
genericAdder x y = x + y
```

This means that all of the following function calls are valid:

```salo
genericAdder 1 1         --> 2
genericAdder "Foo" "Baz" --> "FooBaz"
```

## Imports

Salo is also able to import other files using the `import` keyword. Imports can either bring a library file or a local file into scope. For example:

```
import Prelude
```

Will evaluate and bring everything in the `prelude` module of the standard library into scope. This line is actually automatically inserted into the beginning of every Salo file.

## Conclusion

That's it! We just outlined Salo's basic syntax, styling, and imports. However, there's a lot more documentation out there - just take a look!
