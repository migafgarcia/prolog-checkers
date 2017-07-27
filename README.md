# Prolog Checkers

A Player vs AI game of checkers implemented in Prolog. 

This project was an assignment for the Logic Programming course.

## To Run

The game is played on the console. To run simply load the code into your Prolog interpreter, for example, [SWI Prolog](http://www.swi-prolog.org/) or [Yap](https://www.dcc.fc.up.pt/~vsc/Yap/) and run **main.** .

## To play

The player is presented with a list of possible moves and must chose one of them. The AI is running the minimax algorithm with alpha-beta pruning based of piece and board position weights.

```
Prolog checkers
To play select one of the options available:
3. for example (the dot in the end is important!)
   1  2  3  4  5  6  7  8  
1     ⛂     ⛂     ⛂     ⛂  
2  ⛂     ⛂     ⛂     ⛂     
3     ⛂     ⛂     ⛂     ⛂  
4  _     _     _     _     
5     _     _     _     _  
6  ⛀     ⛀     ⛀     ⛀     
7     ⛀     ⛀     ⛀     ⛀  
8  ⛀     ⛀     ⛀     ⛀     
White (computer) turn to play.
Move evaluation: 11
(1,6) -> (2,5)
   1  2  3  4  5  6  7  8  
1     ⛂     ⛂     ⛂     ⛂  
2  ⛂     ⛂     ⛂     ⛂     
3     ⛂     ⛂     ⛂     ⛂  
4  _     _     _     _     
5     ⛀     _     _     _  
6  _     ⛀     ⛀     ⛀     
7     ⛀     ⛀     ⛀     ⛀  
8  ⛀     ⛀     ⛀     ⛀     
Black (human) turn to play.
1: (2,3) -> (1,4)
2: (2,3) -> (3,4)
3: (4,3) -> (3,4)
4: (4,3) -> (5,4)
5: (6,3) -> (5,4)
6: (6,3) -> (7,4)
7: (8,3) -> (7,4)
   |: 

```
