# 🏎️ Fast and Furious Intro to Haskell 🏁

You have already been programming for years, so you know well what a string and an `if`-`then`-`else` is. Not only that, your language of choice features `map`s, anonymous functions, and optionals. This is a great place to start using Haskell! For this intro it is enough to use an [online REPL](https://repl.it/languages/haskell).

## 🧮 Defining a simple function

The following `greet` function builds a greeting for a given name:

```haskell
greet :: String -> String -> String
greet moment name = "Good " ++ moment ++ ", " ++ name ++ "!"
```

Each function is declared by two elements:

- Its **type signature** `<function-name> :: type` is optional -- the compiler will infer it if missing -- but highly recommended. Note that each additional argument is separated by an additional `->`, this is related to the fact that Haskell functions take _one argument at a time_.
- A sequence of _equations_, _branches_, or _cases_ define its behavior.

Haskell comes with **strict** rules about the naming of things in source code:

- Function names (like `greet`) must start with a _lowercase_ letter;
- Variables (like `name`) must also start with a _lowercase_ letter, in fact for the compiler functions are just a kind of variables;
- Types must start with an _uppercase_ letter.

## 🎛️ More than one branch

Of course, you may inspect the contents of a variable to define what to do. Here is the usual Fibonacci:

```haskell
fib :: Integer -> Integer
fib n = if n <= 1
           then 1
           else fib (n-1) + fib (n-2)
```

`fib (n-1)` and `fib (n-2)` are (recursive) function applications of `fib` to `n-1` and to `n-2`. Because it is so frequent, in Haskell we write function application with whitespaces, and this has higher precedence than most operators. Thus, `fib n -1` would actually mean `(fib n) - 1`. There are no loop-like constructs in Haskell, either. Instead, we use **recursion**.

Once again: you **must not** use parentheses when calling functions in Haskell. This is the single most common source of problems for beginners. Use parentheses only if the argument to a function is itself another function call.

Haskell is an **expression**-oriented language; that means in practice that every path (like the two branches of an `if`) in your program must return a value (or throw an exception, but that's not material for a fast intro). However, you will never see a Haskeller write the Fibonacci function like above. Instead, more that one branch is used, each defining the operation for a subset of values. We call this **pattern matching**.

```haskell
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

In a sense, function definitions come with a top-level implicit `switch`/`match` over all the arguments. You can perform pattern matching over any value, primitive or user-defined, except for functions, and the matching may look at any depth. For example, here are two functions operating on (linked) lists: the empty list is represented as `[]` and a list with a value `x` and a rest of the list `xs` as `x:xs`.

```haskell
removeNegatives :: [Integer] -> [Integer]
removeNegatives [] = []
removeNegatives (n:ns)
  | n < 0          = removeNegatives ns
  | otherwise      = n : removeNegatives ns

incrementBy :: Integer -> [Integer] -> [Integer]
incrementBy _ []     = []
incrementBy n (m:ms) = (n + m) : incrementBy n ms
```

In addition to pattern matching, you may use **guards** to do additional checks on the values. That is the case of `n < 0` in `removeNegatives`. The last, catch-all, guard is defined using `otherwise`.

## 📜 The dreaded layout rules

Ok then, Haskell does not use `{`, `}`, and `;` like C-like languages. Is it then an indentation-based language, like Python? Well, it's complicated. Too complicated, you may say.

The two main rules imposed by the compiler are:

- Some definitions (function definitions and type signatures, among others) must **start on column 0**.
- After a keyword that opens a new block, all the elements must start on the **same column**. For example, you are free to indent `then` and `else` as much as you like, but they must be indented to the same column. Also, you can write the `|`s from guards wherever you like, but they all must agree.

These rules are collectively known as **layout rules**. There are also other rules which is not required, but followed by everybody in the community:

- Align pattern matches corresponding to the same argument,
- Align all `=` signs in function definitions.

## 🧶 Functions using functions using functions...

If one of your arguments is a function, you can directly call it without further ceremony. Here is a function which operates on optional values -- called `Maybe`s in Haskell -- and returns a default `def` if the value is missing -- known as `Nothing` -- or applies a function `f` otherwise.

```haskell
foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe def _ Nothing  = def
foldMaybe _   f (Just x) = f x
```

Notice that the type signature of `foldMaybe` has `(a -> b)`. This declares that the second argument is a function which takes one parameter. When calling this function, at that position we may use:

- The name of another function, defined either locally or globally:

  ```haskell
  maybeAddOne :: Maybe Integer -> Integer
  maybeAddOne x = foldMaybe 0 addOne x
    where addOne n = n + 1
  ```

- An **anonymous function**, which is defined using `\x y ... -> body`:

  ```haskell
  maybeAddOne x = foldMaybe 0 (\x -> x + 1) x
  ```

In `foldMaybe` we find elements with an uppercase letter where we previously only had lowercase ones, and viceversa. `Nothing` and `Just` are the names of the **constructors** of the `Maybe` data type; constructor names are not variables and must thus start with an uppercase letter. In pattern matching positions you are allowed to use these names to distinguish which constructor was used to build the value.

The type of `foldMaybe` features lowercase `a` and `b`: these are **type variables** (now you know the general rule: variables start with lowercase, the rest with uppercase). By using them, we tell the compiler that the definition below works for any choice of `a` and `b`. Most modern programming languages have this feature nowadays, usually called **generics**:

```scala
def foldMaybe[A,B](def: B, f: A => B, v: Option[A]): B
```

```kotlin
fun <A,B> foldMaybe(def: B, f: (A) -> B, v: A?): B
```

The main difference is in Haskell you do not have to state in your type signature which of the types are generic, they are always those which start with a lowercase letter.

As in other languages, not only functions may be generic, but also **data types**. `Maybe` is one of the most commonly-used examples. Like functions for values, in Haskell type application is also written with whitespace, unlike languages like Scala or Kotlin, where you use parens for values `f(x)` and brackets `Option[A]` or angles `List<Int>` for types.

Due to the academic roots of Haskell, we often use terms different from other languages:

- Instead of "generic" we use **polymorphic** (in fact, "generics" refer to a completely different feature).
- Instead of "anonymous function" we use **abstraction** or **lambda** (like the Greek letter λ).

## 🧱 Defining a simple data type

`Maybe` is part of the default library, but of course you can create your own data types. For example, a type of shapes, which can be a circle -- defined by its radius --, a rectangle -- defined by its height and width --, or a triangle -- defined by the length of its sides.

```haskell
data Shape = Circle    Float
           | Rectangle Float Float
           | Triangle  Float Float Float
```

The syntax is quite terse: constructors are separated by `|`, and each of them is defined by its name followed by the _types_ of its fields. By default there are no names associated to each of the fields, the only way to access the information is using pattern matching:

```haskell
area :: Shape -> Float
area (Circle r)       = pi * r * r
area (Rectangle h w)  = h * w
area (Triangle a b c) = -- see https://en.wikipedia.org/wiki/Heron%27s_formula
```

On the right-hand side of `=`, constructors work as regular functions. For example, we can define a way to build a square as a rectangle with the same width and height:

```haskell
square :: Float -> Shape
square s = Rectangle s s
```

Data types in Haskell work as a `sealed` hierarchy of `case` or `data` classes found in Scala or Kotlin. Note however that they only bundle _information_, never _behavior_. They are also _immutable_, in order to return information from a function you must always _create_ a new element.

## 🎭 Input and output

Up to now, all the functions we have defined are **pure**. That means that they have no other effect other than computing their output value from their input values. What other "effects" could a function have? In most languages you could also modify a global variable, print to the screen, or send some information over the wire. Not in Haskell!

For those cases in which you have to perform an effect, Haskell comes with some special notation. For example, let us ask the user for its name and then greet it through the console:

```haskell
consoleGreet = do putStrLn "What is your name?"
                  name <- getLine
                  putStrLn (greet name)
```

Within a `do` block we no longer have expressions, but a sequence of **statements** with effects. Some of the effectful functions we use, such as `putStrLn`, return no interesting value, whereas others such as `getLine` gives us back a `String`. In the latter case the syntax `x <- operation` binds the result to the variable `x`. This binding is **mandatory**, the following code does _not_ compile:

```haskell
consoleGreet = do putStrLn "What is your name?"
                  putStrLn (greet getLine)
```

How do you know whether a function has effects? The answer lies in its type. Notice the difference between a pure operation like `greet` and the impure console ones; the latter ones have `IO` before their return type.

```haskell
greet    :: String -> String
putStrLn :: String -> IO ()
getLine  ::           IO String
```

This is the reason why we cannot write `greet getLine`: the function `greet` expects something of type `String`, but `getLine` has a different type `IO String`. The binding syntax `x <- something` is the way we thread the result of `something` with type `IO a` to a variable `x` which is assigned type `a`.

**Disclaimer**: `do` notation is actually much more powerful than implied by this section. You can use it with anything that is a **monad** (`Maybe` and lists are other examples). However, at the start of your journey with Haskell it is enough to think of `do` as special syntax for effectful operations.

## 🧩 Of equality and type classes

Most languages feature some notion of concrete values and types (call it data types, call it classes), but also some notion of _interface_ or _protocol_ which allows the programmer to declare that one of the concrete types support a given functionality. In Haskell, this notion is called a **type class**.

The archetypal example of interface is equality. If you want to search an element in a list, for example, you need some way to check whether two values are equal or not. Without further delay, here is the type class:

```haskell
class Eq a where
  equals :: a -> a -> Bool
```

One important difference with interfaces as found in most programming languages is that you _explicitly_ include a type variable which represents each (future) type implementing this functionality. In this case, this variable is `a`. The major advantage we gain is the ability to refer to the same type more than once in a method, like we do in `equals`.

Now we can write the function which searchs an element in a list:

```haskell
search v []     = False
search v (x:xs)
  | equals v x  = True
  | otherwise   = search v xs
```

What is the type of this function? It cannot be `a -> [a] -> Bool`, because that would mean that the function works for _any_ type of elements. This is not true: it only works for those for which an implementation of `equals` has been defined. The correct type signature is:

```haskell
search :: Eq a => a -> [a] -> Bool
```

You should read it out loud as "`search` works for any type `a` which implements the class `Eq`, takes a value of that type and a list of that type, and returns a Boolean".

In a very confusing choice of terminology, each implementation of a type class for a specific type is called an **instance**. Here is how you define equality for `Shape`s:

```haskell
instance Eq Shape where
  equals (Circle r1) (Circle r2)
    = r1 == r2
  equals (Rectangle h1 w1) (Rectangle h2 w2)
    = h1 == h2 && w1 == w2
  equals (Triangle a1 b1 c1) (Triangle a2 b2 c2)
    = a1 == a2 && b1 == b2 && c1 == c2
```

## ⛽ Time to race!

Now you are ready to read and write simple Haskell functions. If you want to keep learning, the best free online resources are [_Learn You A Haskell_](http://learnyouahaskell.com/), [_OpenHaskell_](http://openhaskell.com/), and the [_Data61 course_](https://github.com/data61/fp-course).
