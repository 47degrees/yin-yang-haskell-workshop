Exercise 0: Game using Gloss
============================

This first exercise has the goal of getting you comfortable with writing and building Haskell projects. We are going to do so by building a game using the [Gloss](http://hackage.haskell.org/package/gloss) library.

Building a project with Stack
-----------------------------

Haskell projects are described in a so-called Cabal file, always named as `<your-project-name>.cabal`. This file contains a lot of metadata, but also the information required to build your project:

* Where is the source folder,
* Whether you are building a library, executables, or both (each of this components is called a *stanza*),
* If you are building a library, which modules are *exposed* (public),
* If you are building an executable, which module defines the *entry point*.

The easiest way to start a project is to summon Cabal in the command line. If you do not have it yet, you can obtain this tool along with the compiler using the [Haskell Platform](https://www.haskell.org/platform/).

```bash
$ mkdir <your-project-name>
$ cd <your-project-name>
$ cabal init
```

You are then prompted with a bunch of questions, for which you can just use the defaults. The only important questions are:

```text
What does the package build:
   1) Library
   2) Executable
   3) Library and Executable
Your choice? 2

Source directory:
 * 1) (none)
   2) src
   3) Other (specify)
Your choice? [default: (none)] 2
```

This creates a bunch of files and folders, including a `src/Main.hs` file defining the entry point `main` function and a `<your-project-name>.cabal` file which builds it.

Now get things get a bit hairy. Although *Cabal file* is used unambiguously in the Haskell community to refer to a project definition, there are several tools to build it: one is called [*Cabal*](https://www.haskell.org/cabal/) (hence the possible misunderstandings) and the other is called [*Stack*](https://www.haskellstack.org). The main advantage of the latter is that it provides reproducible builds and takes care of setting up the environment for you. Please, take a moment now as install Stack following the [instructions](https://docs.haskellstack.org/en/stable/README/).

Unfortunately you need to run an additional command in order to make your project compatible with Stack:

```bash
$ stack init
```

The result is a new `stack.yaml` file which refers a *resolver*. Such a resolver specifies the version of GHC and the available dependencies for this project. You can finally build and run the program with:

```bash
$ stack build
$ stack run
Hello, Haskell!
```

Adding dependencies
-------------------

The next step in our journey is to add a dependency to Gloss in the Cabal file. The part of the file to focus on is the `executable` stanza, which should read along the lines of:

```cabal
executable example-game
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.12 && <4.13
  hs-source-dirs:      src
  default-language:    Haskell2010
```

As you might guess, `build-depends` is where you declare the dependencies. By default, you can choose any library available in [Stackage](https://www.stackage.org/lts). To add Gloss, simply change that line. I strongly suggest to relax the dependency on the standard library. The result is:

```cabal
  build-depends:       base >=4.12 && <5, gloss
```

Running `stack build` again takes care of downloading and installing the dependency. This is done in a *sandbox*, to prevent different projects from conflicting with each other.

A simple Gloss game
-------------------

Within the library, a game is defined by the arguments of the [`play`](https://www.stackage.org/haddock/lts/gloss/Graphics-Gloss.html#v:play) function. I recommend that you follow the link to get used to the documentation format of Haskell libraries, you can go to the documentation of other elements by clicking on them.

```haskell
play :: Display	-> Color -> Int	
     -> world                      -- initial world
     -> (world -> Picture)         -- view
     -> (Event -> world -> world)  -- handle input
     -> (Float -> world -> world)  -- advance time
     -> IO ()
```

This is one of the most common ways to "functionalize" a long-running task. You refactor the game loop logic into an impure function (here, `play`), which is parametrized by the behavior you want to get. The result is a React-like interface: you need to define how the state (which we call a `world` here) changes in response to events (in Gloss, those may come from user input or from the passing of time) and how to transform that state into a visualization.

One important note about syntax is that in Haskell variables are distinguished from actual values and types because they start with a lowecase letter. In Scala you would write the previous signature as:

```scala
def play[World](..., initial: World, ...)
```

and in Kotlin you would use:

```kotlin
fun play<World>(..., initial: World, ...)
```

but in Haskell you *must* use a lowercase identifier which is *implicitly* added as a variable.

The most important element in the design of a Gloss game is how to represent the *state* of the program. In the [example game](example-game/src/Main.hs) we provide you the "game" is simply a circle which changes its color as time goes, with the ability to pause it pressing 'p'. As we have discussed, *sum types* are very helpful in modelling this kind of states in a program:

```haskell
data World = Go Int | Pause Int
```

The rest of the code uses pattern matching over the events and the current state to either update the latter according to the program logic, or to generate the view. *Pattern matching* is always the preferred way of accessing information in a Haskell program, and functions usually look like a set of equations covering each case (the main exception to this rule are monadic programs, but we will get to that during the workshop). To learn more about this style of programming, check the sections [*Pattern matching*](http://learnyouahaskell.com/syntax-in-functions#pattern-matching) and [*Algebraic datatypes intro*](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types) from *Learn You A Haskell* (this is quite short, there is no need to read the entire chapter).

```haskell
input (EventKey (Char 'p') _ _ _) (Go n)    = Pause n
input (EventKey (Char 'r') _ _ _) (Pause n) = Go    n
```

In the functional world it is quite common to write functions which work as "pipelines", that is, applying different transformations one after the other. If you wrote such a pipeline using parentheses, the result would be difficult to read. For that reason, Haskellers are quite fond of the `($)` operator, which works as if a closing parenthesis was inserted at the end of the line.

```haskell
view (Pause _) = translate (-120) 0 
               $ scale 0.2 0.2
               $ color white
               $ text "Press r to resume"
```

It is now your turn!
--------------------

* Make the shape change to an square when the user presses 's' and back to a circle with 'c'.
  *Hint*: change the state type to read as follows.

  ```haskell
  data World = Go Shape Int | Pause Int
  data Shape = Circle | Square
  ```

* Make the pause/resume behavior also work with a mouse click.
  *Hint*: check the [`Key`](https://www.stackage.org/haddock/lts-14.5/gloss-1.13.0.1/Graphics-Gloss-Interface-IO-Game.html#t:Key) type, there is a `MouseButton` constructor there.

* Adapt the code to generate a new shape every X seconds (feel free to choose X). That is, during the first X seconds only one shape should appear on the screen, then two, then three, and so on.
  *Hint*: the [following functions](https://www.stackage.org/haddock/lts-14.5/base-4.12.0.0/Prelude.html#g:17) may be handy.