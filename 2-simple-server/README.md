Exercise 2. A simple TCP server
=================================

The goal of this exercise is to get you comfortable with writing code in Haskell, which performs input and output. For this, you are going to write a small server that responds to a few commands. This server is going to be extended in subsequent exercises.

## Say "hello" and close

You can build a server at many different abstraction levels. The lower level for network access is provided by the `network` package: There you have to manage the sockets yourself. On the other side of the spectrum, you have packages such as Spock or Servant that provide a high-level interface to build REST services. We are going to lean towards the former, and use the `network-simple` package, which implements the most common scenarios for `network`.

The `V1SayHello.hs` file implements a very simple protocol: Each time somebody connects to the server, the server sends `"Hello"`, and closes the connection.

```haskell
main = serve (Host "127.0.0.1") "8080" $ \(socket, _) ->
         send socket "Hello\n"
```

You can check that it works by running this file, either directly via `runghc V1SayHello.hs` or by creating a Stack project, and then using `telnet` in other terminal (for Mac OS X users, you can get `telnet` from Homebrew):

```
$ telnet 127.0.0.1 8080
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello
Connection closed by foreign host.
```

### Many kinds of strings

If you look at the type of `send`, we find a type we have not introduced yet (well, actually two, but you can read `MonadIO` as `IO` for the time being).

```haskell
send :: MonadIO m => Socket -> ByteString -> m () 
```

The `String` type in Haskell is a synonym of `[Char]`. In other words, strings in Haskell are, by default, *linked lists* of characters. Needless to say, they are not the most efficient way to represent textual data, and they do not interact well with systems that use packed arrays of characters. To solve these problems, the Haskell ecosystem has settled on two packages:

* `text` provides the `Text` type, which represents textual data in Unicode format;
* `bytestring` provides `ByteString`, which represents a meaningless array of bytes.

Although `ByteString` provides some methods for string manipulation, only `Text` implements all the algorithms in accordance with the Unicode specification. Thus, if you are manipulating text to be shown to the user, use always `Text`.

There is one extra trick we are playing here. GHC comes with an extension, `OverloadedStrings`, which allows converting literal strings to other types at compile time. This is the reason we could write "Hello\n" directly in the code.

### *Exercise 1*: Say hello to somebody

Modify the previous server to read a name from the socket, and then greet that person through the wire. As before, close the connection once this is done. Some useful information:

* The [`recv` function](http://hackage.haskell.org/package/network-simple/docs/Network-Simple-TCP.html#v:recv) should be quite useful.
* Notice that `recv` return `Maybe ByteString`, this means that you have to pattern match on the result using a `case` statement to continue.
* To concatenate `ByteString`s you can use the `(<>)` operator.

### *Exercise 2*: The client

Using the [`connect` function](http://hackage.haskell.org/package/network-simple/docs/Network-Simple-TCP.html#v:connect) in `network-simple`, create a client for the server. The program should take two arguments from the command line: The server and the port to connect to. Hint: Use [`getArgs`](http://hackage.haskell.org/package/base/docs/System-Environment.html#v:getArgs) from `System.Environment` to obtain all the arguments.

There are two error conditions you should defend:

1. The user does not give enough, or gives too many arguments. This case should be dealt with using pattern matching.
2. The arguments are not correct, and that makes the call to `connect` fail. In this case, an exception would be thrown, and should be caught.

## Different commands

For the next step, we are going to make the server respond to different commands. And those commands are going to be encoded as... JSON! using the `aeson` library.

Fortunately, GHC has one awesome feature called "generics" that allows you to derive all the marshalling code for free:

1. Add the `{-# language DeriveGeneric #-}` pragma at the top of the file, and import `GHC.Generics`.
2. Ask the compiler to derive instances for the `Generic` type class for your data type. In our case, our data type represents the commands, with some optional fields for arguments:

    ```haskell
    data Command = Command { command :: String, name :: Maybe String }
             deriving (Eq, Show, Generic)
    ```
3. Get conversion from and to JSON for free! You need to import `Data.Aeson` and include these *empty* instances somewhere in the source file:
   
   ```haskell
   instance ToJSON Command
   instance FromJSON Command
   ```

Now our server has to read the socket, perform the conversion from a `ByteString` containing JSON to the proper data type (using `decodeStrict`), and then handle all possible inputs gracefully. The code becomes a bit longer:

```haskell
main :: IO ()
main = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         content <- recv socket 10000
         case content of
           Nothing -> putStrLn "connection closed"
           Just content -> do
            let cmd = decodeStrict content
            case cmd of
              Just (Command "greet" (Just name)) -> do
                let greeting = "Hello, " <> pack name
                send socket greeting
              _ -> putStrLn "unknown command"
```

Later in the workshop, we are going to see how to remove that terrible cascading of `Maybe`s, taking advantage of the fact that it is a monad. Here is a glimpse of how it could look:

```haskell
main2 :: IO ()
main2 = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         content <- recv socket 10000
         case content >>= decodeStrict of
            Just (Command "greet" (Just name)) -> do
              let greeting = "Hello, " <> pack name
              send socket greeting
            _ -> putStrLn "unknown command"
```

### *Exercise 3*: More commands

Make the server respond to two new commands:

1. `"random"` should produce a random integer. The [`randomIO` function](http://hackage.haskell.org/package/random/docs/System-Random.html#v:randomIO) should be helpful here. Note that random generation is not referentially transparent, and operates thus in `IO`.

2. `"file"`, with an additional argument `"filename"`, reads a file from the computer and sends the first 10 characters. Note that the `bytestring` library comes with [its own set of I/O functions](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#g:26) that operate directly on `ByteString`s.

### *Exercise 4*: Random one-to-one communication

*First listen to the explanation about Software Transactional Memory*.

We want to add a "random communication channel" to our server. In particular, we want to have a queue of messages and two new commands:

1. `"say"` adds a new message to the queue.
2. `"listen"` takes the first message from the queue, or, if the queue is empty, it waits until somebody says something.

Use a [`TQueue`](https://www.stackage.org/haddock/lts/stm/Control-Concurrent-STM-TQueue.html) to implement this functionality.
