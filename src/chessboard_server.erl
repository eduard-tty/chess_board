%%%---------------------------------------------------------------------
%%% @author Eduard Lohmann <eduard@tty.nl>
%%% @copyright 2011 Eduard Lohmann
%%% @doc Chess board Server
%%% @end
%%%---------------------------------------------------------------------

% TODO:
% - make it an application
% - add @spec documentation to function.
% - split into more modules
% - error handling

-module(chessboard_server).

-behaviour(gen_server).

-export([ % chessboard functions
       setup/0
     , move/2
     , display/1
     , test/0
 ]).

-export([ % gen_server interface
        init/1
      , handle_call/3
      , handle_cast/2
      , handle_info/2
      , terminate/2
      , code_change/3
]).

-export([ % chessboard_server infterface
        start_link/0
      , start_link/1
      , stop/0
      , move/1
      , show/0
]).

-import(io).
-import(lists).


-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

%% -------------------- chessboard_server API --------------------------

start_link() ->
    start_link( ?DEFAULT_PORT ).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?SERVER, stop).

move(Move) ->
    gen_server:cast(?SERVER, { move, Move }).

show() ->
    { ok, Board } = gen_server:call(?SERVER, show ),
    display(Board).

%% -------------------- gen_server API ---------------------------------

init([_Port]) ->
    State = setup(),
    {ok, State}.

handle_call( show, _Sender, State) ->
    {reply, {ok, State }, State}.


handle_cast(stop, State) -> {stop, normal, State }; % Stop


handle_cast({move, Move}, State) -> % Move
    NewState = move(State, Move),
    {noreply, NewState}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% -------------------- chessboard API --------------------------------

setup() ->
    [
        [ 't', 'n', 'b', 'k', 'q', 'b', 'n', 't' ], % 8
        [ 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p' ], % 7
        [ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ], % 6
        [ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ], % 5
        [ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ], % 4
        [ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ], % 3
        [ 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P' ], % 2
        [ 'T', 'N', 'B', 'K', 'Q', 'B', 'N', 'T' ]  % 1
        %  a    b    c    d    e    f    g    h
    ].

replace( Board, BoardRowIndex, BoardColIndex, Piece ) ->
    RowIndex = 8 - BoardRowIndex,
    ColIndex = BoardColIndex -1,
    { BeforeRows, [ OldRow | AfterRows ] } = lists:split(RowIndex, Board ),
    { BeforePieces, [ OldPiece | AfterPieces ] } = lists:split(ColIndex, OldRow),
    NewRow = BeforePieces ++ [ Piece ] ++ AfterPieces,
    NewBoard = BeforeRows ++ [ NewRow ] ++ AfterRows,
    { NewBoard, OldPiece }.

move(Board, [ C1, R1, $- , C2 , R2 ] ) ->
    {Board2, Piece } = replace(Board, R1 - $0, C1 -$a +1, ' '),
    {Board3, _ }     = replace(Board2, R2 - $0, C2 -$a +1, Piece),
    Board3.

display_rows( I, [ Row | Rows ] ) ->
    io:format("    ~s~s~s~s~s~s~s~s", Row ),
    io:format("  ~B~n", [I] ),
    display_rows(I-1, Rows);
display_rows( I , [] ) -> I .

display(Board) ->
    io:nl(),
    display_rows(8,Board),
    io:format("~n    abcdefgh~n~n", []).

play(Board, []) -> Board;
play(Board, [Move | Moves ]) ->
    Board2 = move(Board, Move),
    play(Board2, Moves).

test() ->
    B = setup(),
    B2 = play(B, [ "e2-e4", "e7-e5", "b1-c3" ] ),
    display(B2).



