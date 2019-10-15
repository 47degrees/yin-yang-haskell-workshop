Exercise 6. A chat server using streams
=======================================

In this last exercise, you are going to build a small chat server, complete using `conduit` and related libraries. Since this is the last exercise, you'll be a bit more on your own, so you can check whether you can put together all the pieces of a Haskell application.

The basic architecture consists of a multicast channel to which incoming messages are sent, and then broadcasted to all the participants, who read and send them over the wire.

* The multicast channel is represented by a [`TMChan`](https://www.stackage.org/haddock/lts/stm-chans/Control-Concurrent-STM-TMChan.html). A `TMChan` works like any other STM channel, but it has a magical function

    ```haskell
    dupTMChan :: TMChan a -> STM (TMChan a)
    ```

    which creates a copy of the channel. The idea is that everytime a connection begins its life, it receives a duplicate of the original channel, through which it communicates.

* You can turn both sides of a `TMChan` into conduits using the functions in [`Data.Conduit.TMChan`](https://www.stackage.org/haddock/lts/stm-conduit/Data-Conduit-TMChan.html#g:2).

* To start a TCP server, you should use [`runTCPServer` from `conduit-extra`](https://www.stackage.org/haddock/lts/conduit-extra/Data-Conduit-Network.html#v:runTCPServer). This function works very much like the one from Exercise 2, except that the handler receives an `AppData` (from which source and sink conduits can be recovered) instead of a `Handle`.

* To make the message separation a bit clearer, you may want to split the input in chunks delimited by end of line characters. The conduit `splitOnUnboundedE` is very useful in that respect.


Adding private messages
-----------------------

The next step is adding support for private messages. Since we now have different things we might want to do, we are going to introduce JSON commands, as we did in Exercise 2. We are only going to support three different messages:

* `{ "command": "username", "value": "name" }` sets the username of the connection receiving the message to the given one. Since that moment, private messages sent to that username should be sent over the wire.
* `{ "command": "broadcast", "value": "message" }` sends a message to all the participants.
* `{ "command": "private", "to": "user", "value": "message" }` sends the message only to the specified username.

The package that exposes `aeson` (the de facto JSON library in Haskell) as conduits is [`ndjson-conduit`](https://www.stackage.org/haddock/lts/ndjson-conduit/Data-Conduit-JSON-NewlineDelimited.html). As you can see, you can choose between silently dropping the parse errors or reify them as `Maybe` or `Either`.

The main complication of this exercise is to make the conduit that is listening to the broadcast channel aware that it now has to respond to a new username. There are several ways to handle this problem:

* The *pure* one is to create a new message type in the broadcast channel that specifies the renaming from name N to name M. Everybody will receive the message, and the connection whose name was previously N would update itself to name M.
* The *transactional* use is to create a custom conduit (using `await` and `yield`) that consumes the `"username"` messages and updates some shared variable (using STM). Then, on private message reception, the conduit that is listening to the broadcast channel checks whether the username in the message corresponds to that in the variable.
