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
	[ 't', 'k', 'b', 'k', 'q', 'b', 'k', 't' ], % row 1
	[ 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p' ],
	[ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ],
	[ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ],
	[ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ],
	[ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ],
	[ 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P' ],
	[ 'T', 'K', 'B', 'K', 'Q', 'B', 'K', 'T' ]  % row 8
].

replace( Board, RowIndex, ColIndex, Piece ) -> 
	{ BeforeRows, [ OldRow | AfterRows ] } = lists:split(RowIndex, Board ),
	{ BeforePieces, [ OldPiece | AfterPieces ] } = lists:split(ColIndex, OldRow), 
	NewRow = BeforePieces ++ [ Piece ] ++ AfterPieces,
	NewBoard = BeforeRows ++ [ NewRow ] ++ AfterRows,
	{ NewBoard, OldPiece }
	.

move(Board, [ C1, R1, $- , C2 , R2 ] ) -> 
	{Board2, Piece } = replace(Board, R1 - $0, C1 -$a +1, ' '),
	{Board3, _ }     = replace(Board2, R2 - $0, C2 -$a +1, Piece), 
	Board3
	;

move(_, Move) -> { error, "Illegal move: " ++ Move }.


show(Board) -> 	io:nl(),
        	lists:foreach( fun(X) -> io:format("~s~s~s~s~s~s~s~s~n", X) end , Board ),
	        io:nl()
	        .

test() -> 
	B = setup(),
	replace(B, 0, 5, 'X')  
	.	
