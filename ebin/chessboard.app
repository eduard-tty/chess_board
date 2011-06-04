%% TODO: chessboard_app, chessboard_sup, registered??
{
    application, chessboard,
    [
        {description, "Server that maintains a chess board"},
        {vsn, "0.1.0"},
        {modules, [chessboard_app, chessboard_sup, chessboard_server]},
        {registered, [chessboard_sup, chessboard_server]},
        {applications, [kernel, stdlib]},
        {mod, {chessboard_app, []}}
    ]
}.
