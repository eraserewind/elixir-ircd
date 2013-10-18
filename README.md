# Experimental Elixir IRCd

Goals:

* Learn Elixir
* Learn how to build distributed apps
* Link all servers using distributed features
* Try to do [mesh linking](http://wiki.inspircd.org/IRC_Meshing) between ircds

Current status: trying to implement the basis of IRC protocol, then i'll concentrate on linking the ircds together.

## Usage

    mix deps.get && mix deps.compile

Run with a console

    iex -S mix run

Useful things to do in the console

    # Remove a nickname from the server
    :gen_server.cast({:global,:server}, { :remove_nick, "nick" })
    # Print current server state (also /quote debug printstate)
    :gen_server.call({:global,server}, { :printstate })
    # Recompile & Reload code
    r(ModuleName) # e.g.: r(IRCd.User)

## Inspirations & stuff

Code:

* [Serveur IRC en Erlang](http://www.sp4ce.net/computer/2007/10/04/irc-server-with-erlang-language.fr.html),
* `irc.erl` from above server, credited to Samuel Tardieu,
* [another irc.erl](https://github.com/Twinside/Eis/blob/master/src/irc.erl),
* ...

* https://github.com/gmcabrita/elixirbot/blob/master/lib/irc.ex
* https://github.com/Twinside/Eis/blob/master/src/irc.erl

Readings that saved me:

* http://learnyousomeerlang.com/
* http://www.erlang.org/doc/design_principles/gen_server_concepts.html
* ...

IRC protocol:

* https://www.alien.net.au/irc/
* http://tools.ietf.org/html/rfc2812
