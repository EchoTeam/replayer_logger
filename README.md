Overview
--------

This is an erlang-client library to log all the requests for the [request replayer](https://github.com/EchoTeam/replayer).

Interface
---------

The client code is quite clear, it has the following exported functions:
    http_get_log/3,
    http_post_log/4,
    rpc_log/4,

Logging http GET requests:
    replayer_log:http_get_log("request-type-1", type1, "http://example.com/api/v1/resource1").
Logging http POST requests:
    replayer_log:http_post_log("request-type-1", type1, "http://example.com/api/v1/resource2/create", Data).
Logging erlang rpc calls to the current node:
    replayer_log:rpc_log("rpc-call-1", rpc1, node(), MFA).
    
