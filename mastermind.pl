% Mastermind

colormatch([], _, _, []).
colormatch([_|CodeT], [], Original, Res) :- 
    colormatch(CodeT, Original, Original, Res).
colormatch([H|CodeT], [H|_], Original, [0|Res]) :-
    colormatch(CodeT, Original, Original, Res).
colormatch([H|CodeT], [X|MoveT], Original, Res) :- 
    dif(H,X), 
    colormatch([H|CodeT], MoveT, Original, Res).
%Reads one char
readOneChar(I) :- readln([I|_]).

% init number of colors
% C: Number of colors (output)
initColor(C) :- write('Enter number of colors (min. 2, max. 10) '), readOneChar(A), checkColor(A,C).

% accumulator initilization
matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).
% check if color is out of bounds
% A: Number of colors (input), C: Number of colors (output)
checkColor(A,C) :- 1 < A, A < 11, C is A.
checkColor(A,C) :- 2 > A, initColor(C).
checkColor(A,C) :- A > 10, initColor(C).

% base case
posmatch([], [], CodeRes, MoveRes, A, Res) :- 
    colormatch(CodeRes, MoveRes, MoveRes, Res1), 
    append(A, Res1, Res).
% check each element at the same position with each other
posmatch([H|CodeT], [H|MoveT], CodeRes, MoveRes, A, Res) :- % when color and position match
    posmatch(CodeT, MoveT, CodeRes, MoveRes, [1|A], Res).
posmatch([X|CodeT], [Y|MoveT], CodeRes, MoveRes, A, Res) :- %when color and position don't match - 
    dif(X,Y),
    posmatch(CodeT, MoveT, [X|CodeRes], [Y|MoveRes], A, Res).
% init size of code
% S: Size of code (output)
initCodeSize(S) :- write('Enter size of code (min. 2, max. 10) '), readOneChar(A), checkSize(A,S).

% check if size of code is out of bounds
% A: Size of code (input), S: Size of code (output)
checkSize(A,S) :- 1 < A, A < 11, S is A.
checkSize(A,S) :- 2 > A, initCodeSize(S).
checkSize(A,S) :- A > 10, initCodeSize(S).


 

% Translate S colors to integers and checks if the line has S number of colors.
% Ln: Code in colors, C: Code in integers, Cnt: Counter starting from S going to 0, S: size of code
colToInt([], _, 0).
colToInt([H|T], [I|R], Cnt) :- Cnt1 is Cnt-1, color(I,H),colToInt(T, R, Cnt1). 

% Parse line from colors to integers. 
% S: Size of code, C: Code in integers
parseLine(S,C):-write('Enter your guess: '), readln(Ln), checkLine(Ln,S,C), colToInt(Ln,C,S).

% Checks if line has S colors.
% Ln: List of colors, S: Number of colors in code, C: Code in integers.
checkLine(Ln, S, _) :- length(Ln, S).
checkLine(Ln, S, C) :- length(Ln, D), dif(S,D), write('You need to provide ' + S + ' colors.'), parseLine(S,C).



%Guess
% S: Size of code, C: Code in integers
guess(S) :- parseLine(S,C), write(C).




































%Show color options
% C: Number of colors
showColoroptions(C) :-  write("Color options:"), nl, printColors(C).

%Print colors
% C: Number of colors
printColors(0) :- write('blue'), nl.
printColors(C) :- C1 is C-1, color(C,Color), write(Color), nl, printColors(C1).

%Color database
color(0,blue).
color(1,green).
color(2,brown).
color(3,red).
color(4,yellow).
color(5,orange).
color(6,white).
color(7,black).
color(8, grey).
color(9,purple).






:-run(intro)


% Game
% C: Number of colors S: Size of code
gameloop(C,S) :- C1 is C-1, showColoroptions(C1), guess(S).


% Start game
start :- write('Welcome to my universe - Let us play a very fun game!'),nl, initColor(C), initCodeSize(S),gameloop(C,S).
