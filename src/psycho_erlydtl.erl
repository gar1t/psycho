-module(psycho_erlydtl).

-export([compile_priv_dir/3, compile_priv_dir/4, render/2]).

compile_priv_dir(AppMod, TemplateMod, Options) ->
    erlydtl:compile_dir(priv_dir(AppMod), TemplateMod, Options).

compile_priv_dir(AppMod, Subdir, TemplateMod, Options) ->
    erlydtl:compile_dir(priv_dir(AppMod, Subdir), TemplateMod, Options).

priv_dir(Mod) ->
    handle_mod_beam_priv_dir(code:which(Mod), Mod).

handle_mod_beam_priv_dir(non_existing, Mod) ->
    error({non_existing_module, Mod});
handle_mod_beam_priv_dir(BeamPath, _Mod) ->
    EbinDir = filename:dirname(BeamPath),
    AppDir = filename:dirname(EbinDir),
    filename:join(AppDir, "priv").

priv_dir(Mod, SubDir) ->
    filename:join(priv_dir(Mod), SubDir).

render(Template, Vars) ->
    TemplateMod = template_mod(Template),
    handle_template_compile(
      erlydtl:compile(Template, TemplateMod), TemplateMod, Vars).

template_mod(Template) ->
    Hash = erlang:phash2(Template, 100000),
    list_to_atom("dtl_" ++ integer_to_list(Hash)).

handle_template_compile(ok, Mod, Vars) ->
    handle_template_render(Mod:render(Vars), Mod, Vars);
handle_template_compile({error, Err}, _Mod, _Vars) ->
    error({erlydtl_compile, Err}).

handle_template_render({ok, Bin}, _Mod, _Vars) -> Bin;
handle_template_render({error, Err}, Mod, Vars) ->
    error({erlydtl_render, Err, Mod, Vars}).
