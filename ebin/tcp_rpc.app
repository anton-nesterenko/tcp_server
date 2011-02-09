%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, tcp_rpc,
 [{description, "RPC server for Erlang and OTP in Action"},
 {vsn, "0.1.0"},
 {modules, [tcp_app, tcp_root_sup, tcp_server]},
 {registered, [tcp_root_sup]},
 {applications, [kernel, stdlib]},
 {mod, {tcp_app, []}}
]}.
