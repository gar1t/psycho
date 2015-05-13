PROJECT = psycho
COMPILE_FIRST = proc

ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars \
	    +warn_shadow_vars +warn_obsolete_guard
SHELL_OPTS = -s psycho_reloader

check::
	erl -pa ebin -eval 'psycho_tests:run()' -s init stop -noshell

include erlang.mk
