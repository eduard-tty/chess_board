%%%---------------------------------------------------------------------
%%% @author Eduard Lohmann <eduard@tty.nl>
%%% @copyright 2011 Eduard Lohmann
%%% @doc Chess board superviser
%%% @end
%%%---------------------------------------------------------------------

-module(chessboard_sup).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Server = {
        chessboard_server,
        { chessboard_server, start_link, [] },
        permanent, 2000, worker, [chessboard_server]
    },
    Children = [ Server ],
    RestartStrategy = { one_for_one, 0, 1},
    {ok, {RestartStrategy, Children} }.



