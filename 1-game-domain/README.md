Exercise 1.1: Modelling a Game
==============================

I highly recommend that you partner with one of the other attendants. Research shows that talking and interacting makes your brain work better!

This first exercise asks you to choose a game you like and write a model for it using algebraic data types in Haskell. This model should, in principle, be used as a basis for a Gloss game. As part of the model, you should include all possible states of the game (paused, playing, game over).

Pay special attention to the usage of sum types in the design. For example, if you are modelling the different bonuses you may get in a Mario-like game, think of the possible options and how each of them is parametrized. This is usually the point in object-oriented modelling when you would use a parent class with a bunch of derived classes. But here, we want to be as closed as possible.

If you have enough time, write the code for some of the events in the game. Going back to the Mario-like game, try to figure out whether Mario has been hit by an enemy, or how the stats in the game change when one of the bonuses is applied.
