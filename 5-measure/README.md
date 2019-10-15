Exercise 5. Tossing coins in different ways
===========================================

The goal of this exercise is to implement the same function in four different ways, using the different primitives available in Haskell. The function in particular is:

```haskell
throwCoin :: Int -> IO Int
```

A call like `throwCoin n` should generate `n` Boolean values and report back how many of those are `True`. Hint: The already-defined `count` function is handy for this task.

You have to implement the function in four different styles:

* *Using a list*: Generate `n` random Boolean values in a list, and then count how many of them are `True`. Use the function `replicateM` to execute an `IO` action exactly `n` times.

    Follow-up question: Why can't we use `randomIO` to generate a random *list* of Booleans directly, instead of using `replicateM`?

* *Using a loop with an accumulator*: Instead of using list functions, implement a recursive function with two arguments: One that keeps track of how many times we still have to toss the coin, and another one that accumulates the number of `True` values. The skeleton of the function should be:

    ```haskell
    throwCoinLoop n = go n 0
      where go :: Int -> Int -> IO Int
            go n acc = ...
    ```

* *Using a list, asynchronously*: Use the same method as the first one, but execute the random generation in `n` different threads. The function `replicateConcurrently` is handy for this.

* *Using STM and a variable*: Generate `n` different threads, each generating one random Boolean value. Each thread should update a shared `TVar` if the value of the toss is `True`.

Measuring performance
---------------------

The go-to library for measuring performance of Haskell functions is [Criterion](http://www.serpentine.com/criterion/tutorial.html). This library not only shows you the numbers, but also generates nice reports with lots of graphics.

In the provided file, you can find a `main` function that already benchmarks the four functions you have just written. In order to start the benchmarks, run in a command line:

```
$ runghc MeasureRandom.hs --output random.html
```

If you cannot run it directly, ensure that you have the appropiate libraries installed globally:

```
$ cabal v1-install async stm random criterion
```

Apart from some numbers, the tool generates an HTML report. Try to figure out which is the fastest implementation of the four you have done, and what this tells us about the overhead of parallel execution and Software Transactional Memory.
