# Psycho

Yes, another Erlang web server!

## Technical Goals

- Small, focused
- Bug free
- Reasonable performance
- Trivial to embed
- Support ecosystem of plugins/middleware
- No external dependencies

I'm not happy with the state of "small and focused" in HTTP Erlang land. I'd
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

In the interest of small-and-focused plus ecosystem, this project will remain
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

Psycho will provide a bare bones HTTP server with perhaps some basic support for
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

## WSGI Inspiration

Psycho steals liberally from [WSGI](http://www.python.org/dev/peps/pep-0333/)
and [Python Web3 Interface](http://www.python.org/dev/peps/pep-0444/).

Previous attempts to implement a WSGI like standard in Erlang failed because
they didn't recognize that WSGI is a *protocol* -- not a library.

The WSGI Python interface is an odd duck, to be sure. So we'll clean things up
in Erlang.

Interface design notes:

- *No built in side effects* -- the interface should pure
- Handle streamed or deferred responses through function callbacks
- No server managed state (see Server State below)
- No need to provide semantic compatibility with CGI

## WSGI Check List

This is a scratch pad for documenting WSGI features vis-a-vis Psycho.

### Environ

#### REQUEST_METHOD

> The HTTP request method, such as "GET" or "POST". This cannot ever be an
> empty string, and so is always required.

In environ as `request_method` - same semantics.

#### SCRIPT_NAME

> The initial portion of the request URL's "path" that corresponds to the
> application object, so that the application knows its virtual
> "location". This may be an empty string, if the application corresponds to
> the "root" of the server.

Not implemented yet. Not sure about this one. It feels like this logic belongs
in middleware.

#### PATH_INFO

> The remainder of the request URL's "path", designating the virtual "location"
> of the request's target within the application. This may be an empty string,
> if the request URL targets the application root and does not have a trailing
> slash.

Counter part to `SCRIPT_NAME` - not implemented.

`request_path` is the full path provided in the HTTP request.

#### QUERY_STRING

> The portion of the request URL that follows the "?", if any. May be empty or
> absent.

Not implemented.

If we parse the request path at all, I'm inclined to provide a fully parsed
query string as proplist. This is also something that could be provided via
middleware

#### CONTENT_TYPE

> The contents of any Content-Type fields in the HTTP request. May be empty or
> absent.

If specified as a header, in environ as `content_type`.

#### CONTENT_LENGTH

> The contents of any Content-Length fields in the HTTP request. May be empty
> or absent.

If specified as a header, in environ as `content_length` integer value.

#### SERVER_NAME, SERVER_PORT

> When combined with SCRIPT_NAME and PATH_INFO, these variables can be used to
> complete the URL. Note, however, that HTTP_HOST, if present, should be used
> in preference to SERVER_NAME for reconstructing the request URL. See the URL
> Reconstruction section below for more detail. SERVER_NAME and SERVER_PORT can
> never be empty strings, and so are always required.

Need this to reconstruct URLs. Note in PEP 444 that port should be a string,
not an integer.

#### SERVER_PROTOCOL

> The version of the protocol the client used to send the request. Typically
> this will be something like "HTTP/1.0" or "HTTP/1.1" and may be used by the
> application to determine how to treat any HTTP request headers. (This
> variable should probably be called REQUEST_PROTOCOL, since it denotes the
> protocol used in the request, and is not necessarily the protocol that will
> be used in the server's response. However, for compatibility with CGI we have
> to keep the existing name.)

Available in envion as `request_protocol`. Same semantics as WSGI.

#### HTTP_ Variables

> Variables corresponding to the client-supplied HTTP request headers (i.e.,
> variables whose names begin with "HTTP_"). The presence or absence of these
> variables should correspond with the presence or absence of the appropriate
> HTTP header in the request.

As we have no need to look like CGI, these are in their own list in Env.

The complete list of HTTP request headers is available in environ as
`http_headers`.

#### wsgi.version

> The tuple (1, 0), representing WSGI version 1.0.wsgi.version

Not implemented. I'm not yet concerned about versioning this protocol.

#### wsgi.url_scheme

> A string representing the "scheme" portion of the URL at which the
> application is being invoked. Normally, this will have the value "http" or
> "https", as appropriate.

Maybe.

#### wsgi.input

> An input stream (file-like object) from which the HTTP request body can be
> read. (The server or gateway may perform reads on-demand as requested by the
> application, or it may pre- read the client's request body and buffer it
> in-memory or on disk, or use any other technique for providing such an input
> stream, according to its preference.)

We don't want this. We should avoid side effects built into the API. Instead,
we should let an application return a value that indicates it wants to receive
data from the request body.

At a minimum, the supported values should be:

    {recv_body, App, Env} | {recv_body, App, Env, Options}
	App = fun(Data, Env) -> Result
	Env = env()
	Options = [option()]
	option() = {recv_size, integer()}
	         | {recv_timeout, integer()}
	Data :: iolist()

And, very useful:

    {recv_form_data, App, Env} | {recv_form_data, App, Env, Options}
	App = fun(Data, Env) -> Result
	Env = env()
	Options = [option()]
	option() = {recv_size, integer()}
	         | {recv_timeout, integer()}
			 | {part_handler, PartHandler}
	PartHandler = fun(Part, Env) -> PartHandlerResult
	PartHandlerResult = {stop, Reason, Env} | {continue, Part, Env}
	Data :: iolist()

The later should "application/x-www-form-urlencoded" and "multipart/form-data"
content types.

#### wsgi.errors

> An output stream (file-like object) to which error output can be written, for
> the purpose of recording program or other errors in a standardized and
> possibly centralized location. This should be a "text mode" stream; i.e.,
> applications should use "\n" as a line ending, and assume that it will be
> converted to the correct line ending by the server/gateway.
>
> For many servers, wsgi.errors will be the server's main error
> log. Alternatively, this may be sys.stderr, or a log file of some sort. The
> server's documentation should include an explanation of how to configure this
> or where to find the recorded output. A server or gateway may supply
> different error streams to different applications, if this is desired.

I *think* we punt on this to the apps. I would recommend logging to
error_logger and letting something like lager intercept those calls, or not.

#### wsgi.multithread, wsgi.multiprocess

> This value should evaluate true if the application object may be
> simultaneously invoked by another thread in the same process, and should
> evaluate false otherwise.

Ah, no. Psycho apps, as Erlang functions, can always be called in parallel.

#### wsgi.run_once

> This value should evaluate true if the server or gateway expects (but does
> not guarantee!) that the application will only be invoked this one time
> during the life of its containing process. Normally, this will only be true
> for a gateway based on CGI (or something similar).

I don't think this applies.

### start_response

[PEP 444](http://www.python.org/dev/peps/pep-0444/) describes how to drop the
start_response function in favor of a functional "body" return value.

## Server State

If an app needs to manage state across requests, it must do so without the help
from Psycho. An app gets a call with a single Environ argument, which is create
anew on each request.

Some ideas for managing state:

- Use cookies!
- Use Erlang services
- Use an external server

Cookies are the best! You get nice round-trip state management that scales
perfectly across millions and millions -- event *billions* -- of users.

If you *need* to manage state across requests within Psycho, create a
registered service in your OTP app and use it -- so simple!

Though it's the worst option, if you *need* to share state across Erlang VMs,
use an external data service like memcache, MySQL, or even MongoDB!

## Response Iterables

As an alternative to an iolist, the app can return an iterable style function
that looks like this:

``` erlang
app(Env) ->
    Db = lookup_db(Env),
    {{200, "OK"}, [{"Content-Type", "text/plain"}], {fun stream/1, Db}}.

stream(Db) ->
    case db:lookup(Db, "data") of
        {ok, Data} ->
            {continue, Data, Db};
        eof ->
            db:close(Db),
            stop;
		{error, Err} ->
		    log_error(Err),
			db:close(Db),
			error
    end.
```

It's possible to include {fun/1, Arg0} as the body or as any part of a body
iolist. This allows for something like this:

``` erlang
["<html>Loading: ", {fun stream/1, Db}, "</html>"]
```

Spec: fun(State) -> {continue, Data, NextState} | stop | {stop, Data}

## Content-Length

The Content-Length header will be set automatically by Psycho if both these
conditions hold:

- Content-Length is not already present
- The response body is binary

PEP 333 has this to say about content length:

http://www.python.org/dev/peps/pep-0333/#handling-the-content-length-header

If the body is an iolist, we don't want to spend the cost of iolist_size at the
server level -- this is something the application should do.

For cases where a response is generated as a binary -- e.g. a template
rendering or json encoding -- we will automatically handle Content-Length.

## TODO

### SSL

Abstract the socket implementation to support SSL as well as TCP. This will
come after the basic HTTP support is in place.

### URL Parsing

Parsing the request path on each request seems reasonable. It's probably stupid
optimization thinking, but it might make sense to support disabling this via a
server option.

    psycho_server:start(8080, myapp, [disable_parse_request_path])

Unless disabled, the parsed path would land in environ as something like this:

``` erlang
{parsed_request_path, {Path, QueryString(), ParsedQueryString()}}
```

E.g. a path like this:

    /foo/bar/?baz=bam

would be parsed like this:

    {"/foo/bar", "baz=bam", [{"baz", "bam"}]}

This could be handled lazily as well -- i.e. the first function to use this
would call a helper function like this:

    app(Env0) ->
        Env = psycho_util:ensure_parsed_request_path(Env0),
        dispatch(Env).

This would be used primarily by routing applications.

### Explicit Chunking

From PEP 333:

> And, if the server and client both support HTTP/1.1 "chunked encoding" [3],
> then the server may use chunked encoding to send a chunk for each write()
> call or string yielded by the iterable, thus generating a Content-Length
> header for each chunk.

In Psycho, that would suggest that response:

    {{200, "OK"}, [], ["Hello, my name is ", Name, "!"]}

would be chunked in three parts! Of course we can't have this -- it breaks
iolists and forces apps to do absurd things like this:

    {{200, "OK"}, [], "Hello, my name is " ++ Name ++ "!"}

No!

But we need a way to get explicit chunking. I think maybe something like this:

    {{200, "OK"}, [], {chunks, [["Hello, my name is ", Name, "!"]]}}

Notes:

- The body response itself must be tagged with `chunks` -- elements within the
  chunks list cannot be further tagged
- Only elements in the chunks list will be chunked -- i.e. there's no deep list
  chunking
- Each element in the list will be evaluated separately in memory to build a
  complete chunk (perhaps up to some server configurable limit?)

The equivalent of WSGI's "iter" pattern in Psycho would look like this:

    {{200, "OK"}, [], {chunks, {fun iter/1, []}}}

### Content-Disposition in static

See CherryPy's static.py for how they do this.

### Use sendfile in psycho_static

This may also be premature optimization thinking, but it may be considerably
more efficient to use file:sendfile rather than read/write the file.

Measure first.

## Notes From CherryPy

CherryPy provides a straight forward WSGI server implementation.

It also provides an application level framework on top of it.

When using the CherryPy framework, you indirectly work with CPWSGIServer, which
wraps the vanilla WSGI server.

CPWSGIServer provides cherrpy.tree as the WSGI application, which is an
instance of _cptree.Tree. Tree implements __call__ according to the WSGI
interface. The call implementation is basically a pass through to one of the
tree's configured applications based on the script name.

Apps can be either raw WSGI apps or _cptree.Application, which uses a WSGI app
helper to implement the expected interface. The helper in turn supports a
pipeline of apps, including the provided app as well as any middleware.

This took a while to sort through. I'm inclined to provide a similar facility
within Psycho, but make it a bit more transparent.

## Multipart Messages

When a form is submitted with content type = "multipart/form-data;
boundary=---xxxx" what support do we give app developers?

Currently, we support recv_body, which returns chunks of the body to a
specified callback. These chunks are currently opaque binaries to psycho, and
stop up to the number of bytes specified by the content length request header.

Apps get this callback:

    app(Data, Env) -> Result

In the case where content type is multipart/form-data, a chunk may contain or
more parts, or no parts, if the chunk is strictly of a previous part.

Here's an example, which shows chunks and parts overlapping.

      +---  ---abcde
	  |     Content-Disposition: form-data; name="name"
	Chunk
      |     Garrett Smith
	  |     ---abcde
	  +---  Content-Disposition: form-data; name="email"
	  |
	Chunk   g@rre/tt
	  |     ---abcde
	  |     Content-Disposition: form-data; name="file1"; filename="Makefile"
	  |     Content-Type: application/octet-stream
	  +---
	  |     ...begin file...
	Chunk
	  |
	  |
	  +---
	  |
	Chunk
	  |
	  |
	  +---
	  |
	Chunk
	  |     ---abcde
	  +---

Boundaries indicate both the start and the end of a part.

Given a chunk, we should be able to enuermate:

- Data preceding the first boundary indicator (leading data)
- One or more parts
- Data following the last boundary indiator (trailing data)

----

What if we made recv_form_data smarter -- now it knows about
"multipart/form-data" content type, in addition to
"application/x-www-form-urlencoded".

    app(Env) ->
	    {recv_form_data, fun handle_data/2, Env}.

    %% Note this requires a default chunk size that the server uses.
	%% We could make this a part of the result tuple, but this is getting
	%% complicated.
	%%
	%% What about an options proplist?

    handle_data(Data, _Env) ->
	    do_something_with_data(Data),
		{{302, "See Other"}, [{"Location", "/"}]}.

This would be fine, if it weren't for the possibility of memory overruns from
huge files. We need a way to handle received chunks as they arrive and not save
these in the final Data proplist.

How?

What if, in addition to taking an App element in recv_form_data, we supported a
two tuple:

    app(Env) ->
        {recv_form_data, {fun handle_multipart/2, fun handle_data/2}, Env}.

Or:

	app(Env) ->
	    {recv_form_data, App, Env, [{part_handler, handle_part/2}]}.


Or:

	app(Env) ->
	    {recv_form_data, App, Env, [{part_handler, handle_part/2},
		                            {recv_timeout, 60000}
									{recv_size, 10240]}.

I definitely like this options patter - much easier to scale.
