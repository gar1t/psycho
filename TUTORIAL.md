# Psycho Tutorial

Psycho is a ridiculously kiss ass Erlang web server that implements a CGI/WSGI
style interface. This may not strike you now as world changing and it's okay
-- you're among friends.

You are about to embark on a journey that will change your life. If you don't
drink beer, now is the time to start. Let's pour something delicious and begin!

## Create a Psycho App Skeleton

Run `psycho-mkapp` to generate a new application skeleton:

    $ cd psycho
    $ ./psycho-mkapp psytut ~/psytut

This will create a project skeleton in a directory named "psytut" in your home
directory. If you're tempted to pronounce this name, it's "Sigh Tut" - or "Sigh
Toot". You can put a hard "P" in there if you want -- "PsEye Toot". Just have
fun with it!

If you want to create the project in a different directory, change "~/psytut"
to something else.

## Compile the Empty Project

Change to the new project location and run `make`:

    $ cd ~/psytut
    $ make

This will download the project dependencies (in this case, Psycho itself and
e2, another kick ass world changing library that simplifies writing OTP
compliant Erlang applications).

For the rest of this tutorial, you will use your editor to create and modify
Erlang source files, compile them using `make`, and test the result in the
Erlang shell and your browser.

## Run the Empty Project

To start the Erlang shell, which will start your psycho server, run `make` with
the `shell` target:

    $ make shell

The psycho project skeleton is configured to start a single server listening on
port 8080. If that port is already bound, you'll get a fancy Erlang error that
contains this fragment:

    {failed_to_start_child,psytut_http,
            {{listen,eaddrinuse}, ...

You can change the port the server runs on by editing
`src/psytut_http.erl`. E.g. change the value "8080" to something else, exit the
shell using CTRL-C CTRL-C (twice) and re-running `make shell`.

When the application is running and bound to the port, open it in a browser:

    http://localhost:8080

If you changed the port, using the new port value -- this applies to the rest
of the tutorial.

You should see this simple message:

    Hello psytut

## Our First Change

Let's change this to test the iterative process that we'll use in the
tutorial. Edit `psytut_http.erl` and modify the text "Hello psytut" to "Hello,
Psycho Tutorial!" -- your users will thank you.

Run `make` in a separate window or from your editor/IDE (not Eclipse)
to compile the modified source file.

Note that in the Erlang shell you'll see:

    Reloading psytut_http... ok

In the shell, Psycho watches for modified source files and recompiles them
automatically.

Reload your web browser to see the result.

## Psycho and Middleware

One of Psycho's goals is to enable a middleware ecosystem. Here's a simple
picture of what this means.

This is a typical request/response cycle in a web application:

     +------------+  request  +------------+
     |            |<---------+|            |
     |   Server   | response  |   Client   |
     |            |+--------->|            |
     +------------+           +------------+

At the moment, your web browser (the client) requests a path from the Psycho
application (the server). The response is a simple HTML page.

Let's inject some middleware into this cycle:

     +------------+  request  +------------+  request  +------------+
     |            |<---------+|            |<---------+|            |
     |   Server   | response  | Middleware | response  |   Client   |
     |            |+--------->|            |+--------->|            |
     +------------+           +------------+           +------------+

In this case, the client and server remain exactly the same and a new component
sits in between the two.

Enough ASCII art! Let's implement this to illustrate.

In `psytut_http.erl`, find the line that looks like this:

    psycho_server:start(?PORT, ?MODULE).

Modify this line to be:

    psycho_server:start(?PORT, apply_header_footer(?MODULE)).

This applies a yet-to-be defined function to add some middleware to our
application. Let's define that function now. Add the following to
`psytut_http.erl`:

    apply_header_footer(App) ->
        sample_middleware:header_footer("# Header\n\n", "\n\n# Footer", App).

Before you test the change in your browser, you need to restart the psytut
application (this particular change does not take effect automatically).

In the shell, call this function:

    > psytut:restart().

Reload your browser. You should see this:

    # Header

    Hello Psycho Tutorial!

    # Footer

Let's take a moment to understand what happened.

Our psytut application serves a simple text page with a message. This is our
server in the diagram above. Next we added some middleware that modifies the
page by adding a header and footer. That's the middleware in the diagram
above. This middleware takes the result of our application, modifies it, and
returns a new result to the client. It sits in the middle of the server and the
client -- thus the term "middle..." you get the idea.

The concept of middleware is like the Dark Side of the Force -- it's more
powerful than you can possibly imagine. You will see.

## Adding Authentication

Our middleware example so far is pretty simple -- we add some content to a
page. Let's do something a bit tricker.

In `psytut_http.erl` modify `start_link/0` to be:

    start_link() ->
        App = apply_auth(apply_header_footer(?MODULE)),
        psycho_server:start(?PORT, App).

In this change we're applying another application to the chain.

Add this function to the module:

    apply_auth(App) ->
        sample_middleware:basic_auth("admin", "sesame", App).

This function wraps an application with basic authentication functionality,
provided by the sample_middleware module. We'll use the credentials specified
in this call to log into our application.

Compile your changes and restart the application:

    > psytut:restart().

Visit the page again in your browser -- you will be prompted for a user name
and password. Experiment to see how it works. See if you can log in! (Hint: the
credentials you enter must *match* the correct credentials.)

Wasn't that easy? That's the idea behind middleware -- you simply tack on new
components to modify the way HTTP requests are handled. Can you feel the
telekinetic powers surging through your body now?

Before we move on to more functionality, let's look at one more aspect of
middleware. In `psytut_http.erl`, modify `app/1` function to be:

    app(Env) ->
        {{200, "OK"}, [{"Content-Type", "text/plain"}], page_body(Env)}.

Add these functions to the module:

    page_body(Env) ->
        ["Hello ", user(Env), ", welcome to the Psycho Tutorial!"].

    user(Env) ->
        psycho:env_val(remote_user, Env, "mystery user").

Compile your changes and refresh your browser. You should see this page:

    # Header

    Hello admin, welcome to the Psycho Tutorial!

    # Footer

What devilry is this?? The basic authentication middleware app modified the
`Env` value by adding a remote_user value, which is the authenticated user name
from the basic auth challenge. This illustrates an important concept:
middleware can modify the request env passed to downstream applications as well
as modify the result. We'll see how this simple facility can be used to
implement a huge variety of pluggable functionality for your Erlang based web
application.

What functionality you say? Here's a list of WSGI (the Python based Web gateway
interface that Psycho follows) libraries:

http://wsgi.readthedocs.org/en/latest/libraries.html

## Beyond Plain Text

What a lovely ASCII Web application! But can we do more? Can we serve, say,
HTML?

Oh yeah, we can. Big time.

In `psytut_http.erl`, modify `apply_header_footer/1` to be:

    apply_header_footer(App) ->
        sample_middleware:header_footer(page_header(), page_footer(), App).

Modify `app/1` to be:

    app(Env) ->
        {{200, "OK"}, [{"Content-Type", "text/html"}], page_body(Env)}.

Add these functions to the module:

    page_header() ->
        "<html><h1>Psycho Tutorial</h1>".

    page_footer() ->
        "<p><hr><small>Content on this site is licensed under a "
        "<a href=\"//creativecommons.org/licenses/by/4.0/\">"
        "Creative Commons Attribution 4.0 International license"
        "</a></small></p>".

Compile the changes and restart the application:

    > psytut:restart().

Reload your browser. You should see a beautiful circa 1995 Web 1.0 application!

## Using Templates

Psycho is *not* a full featured application platform. We don't need anything of
the sort to create full featured web applications! We add features
*incrementally* -- we don't need no stinking framework!

We currently have some HTML hacked directly into our application. And that's
okay. When someone tells you have to separate application logic from
presentation logic, here's what you say: When the comet crashes into Earth and
ends life as we know it, will it *really* make any difference that your
application logic is separate from your presentation logic? It won't.

Yet while there's no moral imperative to move UI code out of Erlang into a
template there are some practical benefits to it. Eh, forget that. Let's just
get some templates working so we can learn -- that's the point of this
tutorial.

ErlyDTL is one of the best templating libraries around period, much less the
best in the Erlang ecosystem. Let's get it.

Your Psycho project uses Loic Hoguin straight forward `erlang.mk` helper
thingy. Don't ask what it does -- it just works.

To add a new Erlang library to your project, you modify `Makefile`. To add
`erlydtl`, you need to make two changes. First, find the line:

    DEPS = e2 psycho

Change it to be:

    DEPS = e2 psycho erlydtl

This tells the make system that ErlyDTL is a project dependency (in addition to
the absurdly awesome e2 library and of course Psycho).

Next, add the this line before line "include erlang.mk":

    dep_erlydtl = https://github.com/erlydtl/erlydtl.git

This tells the make system to resolve the erlydtl dependency using the git
URL. Now *that's* straight forward.

Build the project by running `make`. This will download and compile the erlydtl
library.

We just added some power. Let's use it.

## Refactor Page Body

Let's take the simple step of moving the page body content out of the Erlang
module. We'll put it instead in a template that actually resembles an HTML
file.

By convention, non source resources used by an Erlang application are stored in
the project local "priv" directory. Create this directory from the system
terminal. Assuming you're in the psytut project directory, run:

    $ mkdir priv

Next, create the file `priv/body.html`:

    Hello {{user}}, welcome to the Psycho Tutorial!

Does this look familar? It's the page body, but in straight forward text rather
than Erlang code. We include a reference to a `user` variable -- just like the
current `page_body/1` function.

Next, modify `psytut_http.erl` to use the template. Change `page_body/1` to
be:

    page_body(Env) ->
        psycho_erlydtl:render("priv/body.html", [{user, user(Env)}]).

Compile your changes.

Tragically, because we added the ErlyDTL library after we ran `make shell`, we
need to restart the Erlang shell. Actually, we don't *need* to restart it --
this is Erlang after all. But it's easier.

Type CTRL-C CTRL-C to terminate the shell and then `make shell` at the terminal
prompt to start your application again.

Reload your browser to see what happens!

Well, it *should* be the same -- we merely replaced the Erlang implementation
of the body content with template content. But let's test our new powers!

Modify `priv/body.html`, save your changes, and reload the web page. Great
Odin's Ravens it changes dynamically!

## Refactor Header and Footer

Let's complete this refactor by moving the header and footer content out of the
Erlang module. In this step, we'll abandon the header/footer middleware as this
will be handled now by our templating scheme.

Create the file `priv/header.html`:

    <html><h1>Psycho Tutorial</h1>

Create the file `priv/footer.html`:

    <p>
      <hr>
      <small>Content on this site is licensed under a
        <a href="//creativecommons.org/licenses/by/4.0/">
          Creative Commons Attribution 4.0 International license</a>
      </small>
    </p>

Ah! Now we see the moral prerogative of moving HTML out of an Erlang
module. You can actually read it! True, readability never mattered to PHP and
it did fine -- but PHP never had erlydtl!

Next, modify `priv/body.html` to include these two files:

    {% include "header.html" %}

    Hello {{user}} welcome to the Psycho Tutorial!

    {% include "footer.html" %}

Finally, we don't need the header and footer support in `psytut_http.erl`.

Modify `start_link/1` to be:

    start_link() ->
        App = apply_auth(?MODULE),
        psycho_server:start(?PORT, App).

Delete these functions:

- `apply_header_footer/1`
- `page_header/0`
- `page_footer/0`

Compile your changes and restart `psytut`:

    > psytut:restart().

Reload the page to see the change. Nothing! But if you edit any of the various
templates, you'll see the changes take effect on the next page load.

## Handling Errors

You may have already seen this, but in case you haven't, we're going to
generate an error to see what happens.

Add the string "{{" anywhere to any of the templates. This is invalid Django
syntax because it doesn't contain a closing "}}" -- it will cause problems.

Reload the web page.

What happened? You got a blank page didn't you. That's confusing to a user, but
we're hard core Erlang hackers, so we're not slowed down one lick.

Using curl we can see what's going on:

    $ curl localhost:8080 --user admin:sesame -i

This will request the page specifying the correct basic auth credentials and
will print details from the response.

You should see something like this:

    HTTP/1.1 500 Internal Server Error
    Connection: keep-alive
    Server: psycho
    Date: Wed, 01 Jan 2014 02:24:47 GMT
    Transfer-Encoding: chunked

This means that something went wrong. A 500 should indicate that there's a bug
on the server. In fact there is -- we just introduced it.

What now? Well, check out the Erlang shell! You'll see something like this:

    ERROR REPORT==== 31-Dec-2013::20:24:47 ===
    {handler_error,<0.215.0>,
	{app_error,
	    {{erlydtl_compile,
		 {"priv/footer.html",
		  [{1,erlydtl_scanner,"Illegal character in column 6"}]}},
    ...

So this is helpful to us hackers, but what of our hapless users? They just see
this perplexing blank screen, which will prompt them to hit F5 on their browser
repeatedly until their hands are raw, resulting in a DoS attack on your server.

Let's fix this with psychology!

All we need to do here is set the `debug` flag on our application to `true` and
we're good to go!

Haaahhahaha!

This of course is joke -- there is no `debug` flag in Psycho! This is not some
cushy high level web framework that makes decisions for you. This is raw,
gritty, some would say totally psychotic!

Calm down, it's not that bad. Control is not bad. It's actually good -- even
great. Now, let's solve this problem with a little middleware.

## Error Handler Middleware

Create a new file `src/psytut_error_http.erl`:

    -module(psytut_error_http).

    -export([error_handler/1]).

    error_handler(App) ->
	fun(Env) -> error_handler_app(App, Env) end.

    error_handler_app(App, Env) ->
	handle_app_result(catch psycho:call_app(App, Env), Env).

    handle_app_result({'EXIT', Info}, Env) ->
        e2_log:error({Info, Env}),
	{{500, "Internal Error"},
	 [{"Content-Type", "text/html"}],
	 error_page(Info)};
    handle_app_result({_, _, _}=Result, _Env) -> Result.

    error_page(Err) ->
	psycho_erlydtl:render("priv/error.html", [{error, format_error(Err)}]).

    format_error(Err) ->
	io_lib:format("~p", [Err]).

All this code! What's it for?

This is a Psycho app that provides some application specific middleware powers.

This module is used to wrap another Psycho application with error handling
logic. The sole exported function `error_handler/1` returns a Psycho "app":

- It's function that takes a single `Env` argument

- It complies with the Psycho WSGI style API (this isn't formally defined yet,
  but it's described in some detail in the Psycho README and is resonably
  stable at this point)

The app logic is implemented by `error_handler_app/2`, which delegates the call
to the wrapped application. If an error occurs, it's logged and then formatted
for the user. If an error doesn't occur, the handler simply returns the wrapped
app's result.

It relies on `priv/error.html` so let's create that:

    <html>
      <h1>Something terrible happened, but might not be your fault!</h1>
      <h2>Can you spot the problem?</h2>
      <pre>{{error}}</pre>
    </html>

You might want to provide a different error message, but this has its virtues.

Compile the project and restart the application:

    > psytut:restart().

Refresh your browser. You should now see a nicely formatted error message,
rather than a confusing blank screen. And the psychology of this message will
stop a user dead in his tracks with a sense of foreboding guilt that he might
be breaking something. No more rapid fire F5 refreshes, so your site is safe
from DoS attacks.

Correct the error that you introduced (the "{{" characters), save the file and
refresh your browser. You should be back to your normal tutorial welcome
screen.

## A Word On Performance

We're using the term "middleware", which sounds scary -- a dark image from an
enterprise architure committee meeting. But this is Erlang! As we saw with our
custom error handler in the previous section, "middleware" is single function!

That should be the final word on performance: it costs a single extra function
call, plus whatever the middleware app does.

But what sort of operations can be performed, and does this design scheme make
your web apps slow? Let's take a quick look.

### Request Env Modification

The Psycho request `Env` value is a properties list -- a list of two-tuples
that serves as a multi-key key value store. Like any Erlang list, we can add
items by prepending to the list with the cons operator. This is an efficient
operation in Erlang. In addition to making new values available to the Env, the
cons operation effectively modifies values when you use the proplists or module
or (more efficient) lists:keyfind/3 function.

## Routes

So far our web application just serves a single page, regardless of the
requested path. This is pretty unusual for a web app -- the request method and
path should be used to return specific content. If an unrecognized path is
requested by a client, the server should return a 404 error to indicate that
the resource is "not found".

We can use Psycho's routing application to wire up our application to handle
specific URLs.

In its simplest form, the routes applications matches a parsed path as an exact
string and delegates the request to an associated application. The atom '_' can
be used to match any path. Here's a sample route that has three entries:

    [{"/", fun home_page/1},
	 {"/users", fun users_page/1},
	 {'_', fun other_page/1}]

Let's try this out! Modify `start_link/0` in `psytut_http.erl` to look like
this:

    start_link() ->
        App = apply_error_handler(apply_auth(routes_app())),
        psycho_server:start(?PORT, App).

And define the `routes/0` function this way:

	routes_app() ->
		Routes =
			[{"/", fun home_page/1},
			 {"/users", fun users_page/1},
			 {'_', fun other_page/1}],
		psycho_route:create_app(Routes).

This is the base application that we wrap with authentication and error
handlers. The three functions that correspond to the three paths aren't yet
defined. Let's define them now.

	home_page(Env) ->
		default_page("Welcome to the home page!", Env).

	users_page(Env) ->
		default_page("Welcome to the users page!", Env).

	other_page(Env) ->
		{Path, _, _} = psycho:env_val(parsed_request_path, Env),
		Msg = ["You asked for ", Path, " - not sure what this is :\\"],
		default_page(Msg, Env).

	default_page(Msg, Env) ->
		{{200, "OK"}, [{"Content-Type", "text/html"}], page_body(Msg, Env)}.

Notice that we've modified the `page_body` function to now take a `Msg`
argument. This will be displayed to the user. Modify `page_body` to look like
this:

	page_body(Msg, Env) ->
		Vars =
			[{msg, Msg},
			 {user, user(Env)}],
		psycho_erlydtl:render("priv/body.html", Vars).

And modify `priv/body.html` to look like this:

	{% include "header.html" %}

	Hello {{user}}, welcome to the Psycho Tutorial!

	<p>{{ msg }}</p>

	{% include "footer.html" %}

Restart `psytut` from the Erlang shell:

    > psytut:restart().

Notice now that the application responds specifically to `/` and `/users` and
generically to any other path. This is the basis for structuring your
application!

## Cookie Based Authentication

Earlier we saw how easy it is to add support for basic authentication -- you
just have to wrap an application in the right middleware!

Next, we'll step through a simple cookie based authentication. The idea is the
same -- insert some middleware into the chain.
