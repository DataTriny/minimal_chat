# Minimal chat
A minimal chat server written in Erlang for learning purpose.

## Usage
To use this program, simply clone the repository, compile it and start the application like this:
```
git clone https://github.com/DataTriny/minimal_chat.git
cd minimal_chat
erl -make
erl -pa ebin
application:start(minimal_chat).
```
Once the server is started, use a Telnet client software (such as [PuTTY](https://www.putty.org/) on Windows) to access it.
The server will listen for incomming connections on port 4200.

## Resources
Here is a list of resources that have been helpful while building this project:
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com),
- [gen_server documentation page](http://erlang.org/doc/man/gen_server.html),
- [Another chat server implementation](https://github.com/CoffeMug/erlang-chat-server)