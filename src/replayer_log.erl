%%% vim: set ts=4 sts=4 sw=4 expandtab:
-module(replayer_log).
-export([
    http_get_log/3,
    http_post_log/4,
    rpc_log/4,
    log/3
]).

should_log_requests(Key) ->
    case application:get_env({replayer_log_request, Key}) of
        undefined -> false;
        {ok, V}   -> V
    end.

http_get_log(LogFile, Key, Url) ->
    Ts = os:timestamp(),
    CompKey = {{http, get}, Key},
    Term = term_to_binary({{http, get}, Ts, {Url}}),
    log(LogFile, CompKey, Term).

http_post_log(LogFile, Key, Url, Data) ->
    Ts = os:timestamp(),
    CompKey = {{http, post}, Key},
    Term = term_to_binary({{http, post}, Ts, {Url, Data}}),
    log(LogFile, CompKey, Term).

rpc_log(LogFile, Key, NodeInfo, MFA) ->
    Ts = os:timestamp(),
    CompKey = {{rpc, call}, Key},
    Term = term_to_binary({{rpc, call}, Ts, {NodeInfo, MFA}}),
    log(LogFile, CompKey, Term).


log(LogFile, CompKey, Term) when is_binary(Term) ->
    case should_log_requests(CompKey) of
        false -> nop;
        _ -> log_ll(LogFile, Term)
    end.

log_ll(LogFile_, Term) ->
    LogFile = case application:get_env(replayer_log_path) of
        {ok, V} -> V ++ LogFile_;
        _ -> "/tmp" ++ LogFile_
    end,

    case disk_log:balog(LogFile, Term) of
        ok -> nop;
        {error, no_such_log} ->
            tryopenlog(LogFile),
            disk_log:balog(LogFile, Term);
        Error -> lager:error("Error while writing to disk_log ~p: ~p", [LogFile, Error])
    end.

tryopenlog(LogFile) ->
    case disk_log:open([ 
                {name, LogFile},
                {format, internal},
                {file, LogFile}]) of
        {ok, _} -> nop;
        Error -> lager:error("Error while opening ~p: ~p~n", [LogFile, Error])
    end.
