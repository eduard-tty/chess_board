%%%---------------------------------------------------------------------
%%% @author Eduard Lohmann <eduard@tty.nl> 
%%% @copyright 2011 Eduard Lohmann
%%% @doc Chess board utilitties. Excersize software.
%%% @end
%%%---------------------------------------------------------------------

-module(chess_board).

-export([
	 setup/0   
	 ,move/2
	 ,show/1    
	 ,test/0
 ]).

-import(io).
-import(lists).


setup() -> [ 
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

show_rows( I, [ Row | Rows ] ) ->
    io:format("    ~s~s~s~s~s~s~s~s", Row ),
    io:format("  ~B~n", [I] ),
    show_rows(I-1, Rows);	
show_rows( I , [] ) -> I . 

show(Board) -> 	
    io:format("~n", []),
    show_rows(8,Board),
    io:nl(),	
    io:format("    abcdefgh~n~n", []).

play(Board, []) -> Board;
play(Board, [Move | Moves ]) -> 
    Board2 = move(Board, Move),
    play(Board2, Moves).

test() -> 
    B = setup(),
    B2 = play(B, [ "e2-e4", "e7-e5", "b1-c3" ] ),
    show(B2).


	
