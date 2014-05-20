PROJECT = psycho
COMPILE_FIRST = proc

ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars \
	    +warn_shadow_vars +warn_obsolete_guard

include erlang.mk

check: app
	erl -pa ebin -eval 'psycho_tests:run()' -s init stop -noshell

shell:
	erl -pa ebin -s psycho_reloader
