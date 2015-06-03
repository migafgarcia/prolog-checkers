:- use_module(library(lists)).

% I was here

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
% pos(+X, +Y, +Board, -Piece).
pos(X, Y, Board, Piece) :-
	nth0(Y, Board, XBoard),
	nth0(X, XBoard, Piece).


% replace(+N, +Piece, +List, -NewList).
replace_in_line(N, Piece, [X|List], [Piece|List]) :- N == 0.
replace_in_line(N, Piece, [X|List], [X|NewList]) :-
	N \= 0,
	N1 is N-1,
	replace_in_line(N1, Piece, List, NewList).

% replace(+X,, +Y, +Piece, +Board, -NewBoard).
% Places/Replaces a piece in the board
replace_in_board(X, Y, Piece, [Line|Board], [NewLine|Board]) :-
	Y == 0,
	replace_in_line(X, Piece, Line, NewLine).
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
evaluate_line([Piece|Line], Evaluation) :- Piece == w, evaluate_line(Line, Remaining), Evaluation is Remaining + 1.
evaluate_line([Piece|Line], Evaluation) :- Piece == b, evaluate_line(Line, Remaining), Evaluation is Remaining - 1.
evaluate_line([Piece|Line], Evaluation) :- Piece == wq, evaluate_line(Line, Remaining), Evaluation is Remaining + 2.
evaluate_line([Piece|Line], Evaluation) :- Piece == bq, evaluate_line(Line, Remaining), Evaluation is Remaining - 2.
evaluate_line([Piece|Line], Evaluation) :-
	Piece \= w,
	Piece \= b,
	Piece \= wq,
	Piece \= bq,
	evaluate_line(Line, Evaluation).


% next_move(+X, +Y, +Board, -NewBoard).

%%%%%%%%%%%%%%% WHITE %%%%%%%%%%%%%%% 
next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == w,
	X1 is X - 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == w,
	X1 is X + 1, Y1 is Y - 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y - 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == w,
	X1 is X - 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).


next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == w,
	X1 is X + 1, Y1 is Y - 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).




%%%%%%%%%%%%%%% BLACK %%%%%%%%%%%%%%%
next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == b,
	X1 is X - 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X - 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == b,
	X1 is X + 1, Y1 is Y + 1,
	is_occupied(X1, Y1, Board),
	X2 is X + 2, Y2  is Y + 2,
	\+is_occupied(X2, Y2, Board),
	move(X, Y, X2, Y2, Board, NewBoard1),
	remove_from_board(X1,Y1,NewBoard1, NewBoard).

next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == b,
	X1 is X - 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).


next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == b,
	X1 is X + 1, Y1 is Y + 1,
	\+is_occupied(X1, Y1, Board),
	move(X, Y, X1, Y1, Board, NewBoard).


%%%%%%%%%%%%%%% WHITE AND BLACK QUEEN %%%%%%%%%%%%%%% 
next_move(X, Y, Piece, Board, NewBoard) :-
	Piece == wq,
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

% max_board( [Board], Eval, Board) :-
% 	evaluate_board(Eval).
% max_board([Board|Boards], Eval, BestBoard) :-
% 	evaluate_board(Eval),
% 	max_board(Boards, Evals, BestBoard1),
	



% max(?Boards, ?Board).
% Given a list of boards, returns the one with maximum evaluation
% Aux: max_board/3, get_max_board/5
max_boards([Board], Board).
max_boards([Board|Boards], BestBoard) :-
	max_boards(Boards, BestBoard1),
	max_board(Board, BestBoard1, BestBoard).

max_board(Board1, Board2, BestBoard) :-
	evaluate_board(Board1, Eval1),
	evaluate_board(Board2, Eval2),
	get_max_board(Board1, Eval1, Board2, Eval2, BestBoard).
	
get_max_board(Board1, Eval1, Board2, Eval2, Board1) :-
	Eval1 >= Eval2.
get_max_board(Board1, Eval1, Board2, Eval2, Board2) :-
	Eval1 < Eval2.


min_boards([Board], Board).
min_boards([Board|Boards], BestBoard) :-
	min_boards(Boards, BestBoard1),
	min_board(Board, BestBoard1, BestBoard).

min_board(Board1, Board2, BestBoard) :-
	evaluate_board(Board1, Eval1),
	evaluate_board(Board2, Eval2),
	get_min_board(Board1, Eval1, Board2, Eval2, BestBoard).
	
get_min_board(Board1, Eval1, Board2, Eval2, Board1) :-
	Eval1 < Eval2.
get_min_board(Board1, Eval1, Board2, Eval2, Board2) :-
	Eval1 >= Eval2.
	
print_board([]) :- nl.
print_board([Line|Board]) :- print_line(Line), nl, print_board(Board).

print_line([]).
print_line([Piece|Line]) :- print(Piece), print_line(Line). 

maximizing(w).
maximizing(wq).
minimizing(b).
minimizing(bq).

next_player(1, 2).
next_player(2, 1).

player_piece(1, [w, wq]).
player_piece(2, [b, bq]).

all_next_moves([Pieces], Board, NewBoard) :-
	member(Y, [0,1,2,3,4,5,6,7]),
	member(X, [0,1,2,3,4,5,6,7]),
	member(Piece, Pieces),
	pos(X, Y, Board, Piece),
	next_move(X, Y, Piece, Board,  NewBoard).

% minimax(Board, BestBoard, Piece, Depth) :-
% 	maximizing(Piece),
% 	bagof(NewBoard, all_next_moves(Piece, Board, NewBoard), NewBoards),
% 	max_boards(NewBoards, BestBoard),
% 	minimax(BestBoard
	
% 	!.


% minimax(Board, BestBoard, Piece, Depth) :-
% 	minimizing(Piece),
% 	bagof(NewBoard, all_next_moves(Piece, Board, NewBoard), NewBoards),
% 	min_boards(NewBoards, BestBoard),
% 	Depth is Depth + 1,
% 	!.


minimax(Player, Board, BestBoard, Val) :-
	next_player(Player, OtherPlayer),
	player_piece(OtherPlayer, Pieces),
	bagof(NewBoard, all_next_moves(Pieces, Board, NewBoard), NewBoards), !,
	best_board(OtherPlayer, NewBoards, BestBoard, Val).

best_board(Player, [NewBoard|NewBoards], BestBoard, BestVal) :-
	minimax(Player, NewBoard, _, ChildVal), %Recurse into enemy player
	best_board(Player, NewBoards, NewBestBoard, NewVal), %
	betterof(Player, NewBoard,  
	
	