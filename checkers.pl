:- use_module(library(lists)).


% initial_board(?Board).
% b = black piece
% w = white piece
% bq = black queen
% wq = white queen
% 0 = white place
% 1 = black place

empty_board([
	     [0,1,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0],
	     [0,1,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0],
	     [0,1,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0],
	     [0,1,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0]]).

initial_board([
	       [0,b,0,b,0,b,0,b],
	       [b,0,b,0,b,0,b,0],
	       [0,b,0,b,0,b,0,b],
	       [1,0,1,0,1,0,1,0],
	       [0,1,0,1,0,1,0,1],
	       [w,0,w,0,w,0,w,0],
	       [0,w,0,w,0,w,0,w],
	       [w,0,w,0,w,0,w,0]]).

test_board_1([
	      [0,b,0,b,0,b,0,b],
	      [b,0,b,0,b,0,b,0],
	      [0,b,0,b,0,1,0,b],
	      [1,0,w,0,1,0,1,0],
	      [0,b,0,b,0,1,0,1],
	      [w,0,w,0,w,0,w,0],
	      [0,w,0,w,0,w,0,w],
	      [w,0,w,0,w,0,w,0]]).

test_board_2([
	      [0,b,0,b,0,b,0,b],
	      [b,0,1,0,b,0,b,0],
	      [0,1,0,b,0,1,0,b],
	      [1,0,w,0,1,0,1,0],
	      [0,b,0,b,0,1,0,1],
	      [w,0,w,0,w,0,w,0],
	      [0,w,0,w,0,w,0,w],
	      [w,0,w,0,w,0,w,0]]).

test_board_3([
	     [0,1,0,1,0,1,0,1],
	     [1,0,w,0,1,0,1,0],
	     [0,1,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0],
	     [0,b,0,1,0,1,0,1],
	     [1,0,1,0,1,0,1,0],
	     [0,1,0,b,0,1,0,1],
	     [1,0,1,0,1,0,1,0]]).

% pos(+X, +Y, +Board, -Piece).
pos(X, Y, Board, Piece) :-
	nth0(Y, Board, XBoard),
	nth0(X, XBoard, Piece).


% place(+N, +Piece, +List, -NewList).
place_in_line(N, Piece, [_|List], [Piece|List]) :- N == 0, !.
place_in_line(N, Piece, [X|List], [X|NewList]) :-
	N1 is N-1,
	place_in_line(N1, Piece, List, NewList).

% replace(+X,, +Y, +Piece, +Board, -NewBoard).
% Places/Replaces a piece in the board
replace_in_board(X, Y, Piece, [Line|Board], [NewLine|Board]) :-
	Y == 0,
	place_in_line(X, Piece, Line, NewLine).
replace_in_board(X, Y, Piece, [Line|Board], [Line|NewBoard]) :-
	Y \= 0,
	Y1 is Y-1,
	replace_in_board(X, Y1, Piece, Board, NewBoard).


%move(+X1, +Y1, +X2, +Y2, +Board, -NewBoard).
% Removes piece from current position (X1, Y1) and places it in (X2, Y2)
move(X1, Y1, X2, Y2, Board, NewBoard) :-
	pos(X1, Y1, Board, Piece),
	empty_board(Empty),
	pos(X1, Y1, Empty, Place),
	replace_in_board(X1, Y1, Place, Board, Board1),
	replace_in_board(X2, Y2, Piece, Board1, NewBoard).

is_occupied(X, Y, Board) :-
	pos(X, Y, Board, Target),
	\+number(Target).

is_enemy(X, Y, Piece, Board) :-
	pos(X, Y, Board, Target),
	Target \= Piece.


remove_from_board(X, Y, Board, NewBoard) :-
	empty_board(Empty),
	pos(X, Y, Empty, Place),
	replace_in_board(X, Y, Place, Board, NewBoard).

% evaluate_board(+Board, -Evaluation).
% Simple evaluation: #white_checkers + #white_queens*2 - #black_checkers - #black_queens*2
evaluate_board([], 0).
evaluate_board([Line|Board], Evaluation) :-
	evaluate_line(Line, LineEval),
	evaluate_board(Board, BoardEval),
	Evaluation is LineEval + BoardEval.



evaluate_line([], 0).
evaluate_line([Piece|Line], Evaluation) :-
	Piece == w,
	evaluate_line(Line, Remaining),
	Evaluation is Remaining + 1.
evaluate_line([Piece|Line], Evaluation) :-
	Piece == b,
	evaluate_line(Line, Remaining),
	Evaluation is Remaining - 1.
evaluate_line([Piece|Line], Evaluation) :-
	Piece == wq, evaluate_line(Line, Remaining),
	Evaluation is Remaining + 2.
evaluate_line([Piece|Line], Evaluation) :-
	Piece == bq,
	evaluate_line(Line, Remaining),
	Evaluation is Remaining - 2.
evaluate_line([Piece|Line], Evaluation) :-
	Piece \= w,
	Piece \= b,
	Piece \= wq,
	Piece \= bq,
	evaluate_line(Line, Evaluation).


% next_move(+X, +Y, +Board, -NewBoard).

%%%%%%%%%%%%%%% WHITE %%%%%%%%%%%%%%% 
next_move(X, Y, w, Board, NewBoard) :-
	X1 is X - 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	morph(X2, Y2, Piece, NewBoard1, NewBoard2),
	remove_from_board(X1,Y1,NewBoard2, NewBoard).

next_move(X, Y, w, Board, NewBoard) :-
	X1 is X + 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	morph(X2, Y2, Piece, NewBoard1, NewBoard2),
	remove_from_board(X1,Y1,NewBoard2, NewBoard).

next_move(X, Y, w, Board, NewBoard) :-
	X1 is X - 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard1),
	morph(X1, Y1, Piece, NewBoard1, NewBoard).


next_move(X, Y, w, Board, NewBoard) :-
	X1 is X + 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard1),
	morph(X1, Y1, Piece, NewBoard1, NewBoard).


queen(w, wq).
queen(b, bq).

morph(X, Y, b, Board, NewBoard) :-
	length(Board, MaxY),
	Y == MaxY,
	queen(Piece, Queen),
	replace_in_board(X, Y, Queen, Board, NewBoard).
	
morph(X, Y, w, Board, NewBoard) :-
	Y == 0,
	queen(Piece, Queen),
	replace_in_board(X, Y, Queen, Board, NewBoard).

morph(_, _, Piece, Board, Board) :-
	Piece \= w,
	Piece \= b.


%%%%%%%%%%%%%%% BLACK %%%%%%%%%%%%%%%
next_move(X, Y, b, Board, NewBoard) :-
	X1 is X - 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	morph(X2, Y2, Piece, NewBoard1, NewBoard2),
	remove_from_board(X1,Y1,NewBoard2, NewBoard).

next_move(X, Y, b, Board, NewBoard) :-
	X1 is X + 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	morph(X2, Y2, Piece, NewBoard1, NewBoard2),
	remove_from_board(X1,Y1,NewBoard2, NewBoard).

next_move(X, Y, b, Board, NewBoard) :-
	X1 is X - 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard1),
	morph(X1, Y1, Piece, NewBoard1, NewBoard).



next_move(X, Y, b, Board, NewBoard) :-
	X1 is X + 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard1),
	morph(X1, Y1, Piece, NewBoard1, NewBoard).



%%%%%%%%%%%%%%% WHITE AND BLACK QUEEN %%%%%%%%%%%%%%% 
next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X - 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X + 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X + 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X - 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X - 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).


next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X + 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X - 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).


next_move(X, Y, Piece, Board, NewBoard) :-
	member(Piece,[wq,bq]),
	X1 is X + 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).

	
print_board([]) :- nl.
print_board([Line|Board]) :- print_line(Line), nl, print_board(Board).

print_line([]).
print_line([Piece|Line]) :- print(Piece), print_line(Line). 

maximizing(1).
minimizing(2).


next_player(1, 2).
next_player(2, 1).

player_piece(1, [w, wq]).
player_piece(2, [b, bq]).

all_next_moves(Pieces, Board, NewBoard) :-
	member(Y, [0,1,2,3,4,5,6,7]),
	member(X, [0,1,2,3,4,5,6,7]),
	member(Piece, Pieces),
	pos(X, Y, Board, Piece),
	next_move(X, Y, Piece, Board,  NewBoard).


minimax(Player, Board, NextBoard, Val, Depth) :-
	Depth < 4,
	NewDepth is Depth + 1,
	next_player(Player, OtherPlayer), !,
	player_piece(OtherPlayer, Pieces),
	bagof(NewBoard, all_next_moves(Pieces, Board, NewBoard), Moves),
	best(OtherPlayer, Moves, NextBoard, Val, NewDepth).

% Board doesnt have child
minimax(Player, Board, _, Val, Depth) :-
	evaluate_board(Board, Val).

best(Player, [Board], Board, Val, Depth) :-
	minimax(Player, Board, _, Val, Depth), !.

best(Player, [Board|Boards], BestBoard, BestVal, Depth) :-
	minimax(Player, Board, NextBoard, Val, Depth),
	best(Player, Boards, BestBoard1, BestVal1, Depth),
	better_of(Player, Board, Val, BestBoard1, BestVal1, BestBoard, BestVal).


better_of(Player, Board1, Val1, Board2, Val2, Board1, Val1) :-
	maximizing(Player),
	Val1 >= Val2, !.
better_of(Player, Board1, Val1, Board2, Val2, Board2, Val2) :-
	maximizing(Player),
	Val2 >= Val1, !.

better_of(Player, Board1, Val1, Board2, Val2, Board1, Val1) :-
	minimizing(Player),
	Val1 =< Val2, !.
better_of(Player, Board1, Val1, Board2, Val2, Board2, Val2) :-
	minimizing(Player),
	Val2 =< Val1, !.

