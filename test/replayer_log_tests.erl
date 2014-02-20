-module(replayer_log_tests).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    ets:new(test_replayer_log, [set, named_table]),
    meck:new(disk_log),
    meck:expect(disk_log, open, fun(_) -> nop end),
    meck:expect(disk_log, balog, 
        fun(File, Term) ->
            Old = ets:lookup(test_replayer_log, File),
            ets:insert(test_replayer_log, {File, [Term | Old]})
        end).

unload() ->
    meck:unload(),
    ets:delete(test_replayer_log).

get_logs(File) ->
    ets:lookup(test_replayer_log, File).

global_test() -> 
    {setup, 
        fun setup/0,
        fun unload/1,
        fun() ->
            replayer_log:rpc_log("file1", key1, nodeinfo1, mfa1),
            replayer_log:http_get_log("file1", key2, url2),
            replayer_log:http_post_log("file1", key3, url3, data3),
            ?assertEqual([], get_logs("file1"))
        end.
    }.
