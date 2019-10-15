Exercise 4: Three Monads
========================

Understanding higher-kinded abstraction, that is, the fact that we can write polymorphic programs that operate within different effects, is one of the challenges in Haskell. The reason is that, whereas in other programming languages you can always go back to sequential execution with unrestricted side effects, in Haskell, you cannot escape from it.

As we would say in Spanish, "Las mónadas no son tan fieras como las pintan" (which roughly translates to, "Monads as not as fierce as they seem"). Furthermore, it is more important to be able to *use* monads and monadic functions properly than it is to write `Monad` instances. Yet, doing the latter for several cases helps a lot in building (and destroying the wrong) intuition.

In this exercise, you will write instances for the following type classes:

* `Functor`: Applying a function within the context;
* `Applicative`: Parallel execution;
* `Alternative`: Choice and failure;
* `Monad`: Sequential execution.

for the following data types:

* Optionals, which we call `Option` to not collide with `Maybe`,
* Lists, which we call `List` to not collide with `[]`,
* Environments, that is, with functions that may access an additional, global, and immutable value of a certain type. This is commonly used to represent configurations.

## Some advice

* Start by expanding the functions to cover all possible cases. For example, for `fmap` for `Option` you should have the cases in which `x` is `None`, and where `x` is `Some y`.
* Keep the types in mind. In many cases, once your patterns are exhaustive, you have only one (reasonable) implementation of the function.
  * GHC can help you with this. If you write `_` where an expression is expected, the compiler tells you which is the expected type and which are the available elements.

## Monoidal versus Applicatives

Most tutorials introduce `Applicative` as the intermediate step between `Functor` and `Monad`. However, the `(<*>)` method can be confusing at the beginning (so don't worry if you don't understand it):

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

There is another way to define "contexts with parallel composition," which is usually called `Monoidal` (the name comes from the [paper which introduced `Applicatives` to Haskell programming](http://strictlypositive.org/Idiom.pdf)):

```haskell
class Functor f => Monoidal f where
  unit  :: f ()
  tuple :: f a -> f b -> f (a, b)
```

In an interesting turn of events, it does not matter which one you choose! From an implementation of `Monoidal`, we can get one for `Applicative`. This is done at the end of the file, in functions `pureMo` and `apMo`. Feel free to have a look, or even try to write the functions yourself.
