%%%---------------------------------------------------------------------
%%% @author Eduard Lohmann <eduard@tty.nl>
%%% @copyright 2011 Eduard Lohmann
%%% @doc Chess board application.
%%% @end
%%%---------------------------------------------------------------------

-module(chessboard_app).

-behaviour(application).

-export([
      start/2
    , stop/1
]).

start(_Type, _StartArgs) ->
    case chessboard_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other     -> {error, Other}
    end.

stop(_State) -> ok.
