PROJECT = app_id
DEPS = e2 psycho
dep_e2 = https://github.com/gar1t/e2.git
dep_psycho = https://github.com/gar1t/psycho.git

include erlang.mk

shell: all
	ERL_LIBS=deps; erl -pa ebin -s psycho_reloader -s app_id
