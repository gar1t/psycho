PROJECT = psycho
COMPILE_FIRST = proc

ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars \
	    +warn_shadow_vars +warn_obsolete_guard

include erlang.mk
