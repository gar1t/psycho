# Psycho

Yes, another Erlang web server!

## Technical Goals

- Small, focussed
- Bug free
- Reasonable performance
- Trivial to embed
- Support ecosystem of plugins/middleware
- No external dependencies

I'm not happy with the state of "small and focussed" in HTTP Erlang land. I'd
like something similar to Mochiweb that uses a more canonical interface.

This is also an experiment to create a useful HTTP middleware interface in
Erlang, which is similar to CGI, WSGI, Servlet, etc. in other languages. My
theory is that a simple HTTP interface that can be used for clients, servers
and middleware, we might be able to kick off a productive web ecosystem for
Erlang.

Performance is important, but is secondary to rock solidness. Above all, users
should be confident that the server will behave predictably under a wide range
of load/usage scenarios.

## Personal Goals

- Drunken Stumble
- Use proc (simplified gen_server)

I initially tried to get away with using only proc_lib but that's impractical
-- you need a minimal behavior to get proper OTP compliance.

gen_server however is bloated and a pain to use. So there's "proc" -- a
minimally viable proc_lib wrapper. See what happens!

## Non-Goals

- One stop shopping

In the interest of small-and-focussed plus ecosystem, this project will remain
only a web server -- in the spirit of Mochiweb and the CherryPy HTTP
server. Other features should come by way of separate projects. The idea is
that Psycho provides a drop-dead simple interface and makes adding new
functionality very easy. The rest is up to users to piece together as needed.

- Rubber Room Safety

For now, there's very minimal type checking. Request handlers should be
isolated from one another, so at worst only a particular request will blow
up. But it's easy to do (e.g. provide a status reason that contains CRLF). I'd
rather not spend the energy on the type checking and let things crash, either
with the server or via a protocol violation.

## Supported Ecosystem Features

Psycho will provide a bare bones HTTP server with perhaps some basic suppor for
serving static content. The rest will come by way of compatible apps.

Examples:

- Routing schemes
- Magic bindings with erlydtl (or other) templates
- Cookie based authentication
- Alternative static content serving
- Fancy error reporting and debugging tools
- Adapters to other servers or frameworks
- Virtual hosting
- RESTful mappers

## TODO

### Response Iterables

As an alternative to an iolist, the app should be able to return an iterable
style function that looks something like this:

``` erlang
app(Env) ->
    Db = lookup_db(Env),
    {?resp_ok, [], stream_fun(Db)}.

stream_fun(Db) ->
    fun(next) ->
            case db:lookup(Db, "data") of
                {ok, Data} -> {continue, Data};
                error -> stop
            end;
       (close) ->
            db:close(Db)
    end.
```

In fact, it should be possible to include a fun/1 as the body or as any part of
a body iolist. This would allow for something like this:

``` erlang
["<html>Loading: ", stream_fun(Db), "</html>"]
```

### SSL

Abstract the socket implementation to support SSL as well as TCP. This will
come after the basic HTTP support is in place.
