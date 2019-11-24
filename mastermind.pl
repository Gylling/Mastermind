% Mastermind

%Reads one char
readOneChar(I) :- readln([I|_]).

% init number of colors
% C: Number of colors (output)
initColor(C) :- write('Enter number of colors (min. 2, max. 10) '), readOneChar(A), checkColor(A,C).

% check if color is out of bounds
% A: Number of colors (input), C: Number of colors (output)
checkColor(A,C) :- 1 < A, A < 11, C is A.
checkColor(A,C) :- 2 > A, initColor(C).
checkColor(A,C) :- A > 10, initColor(C).

% init size of code
% S: Size of code (output)
initCodeSize(S) :- write('Enter size of code (min. 2, max. 10) '), readOneChar(A), checkSize(A,S).

% check if size of code is out of bounds
% A: Size of code (input), S: Size of code (output)
checkSize(A,S) :- 1 < A, A < 11, S is A.
checkSize(A,S) :- 2 > A, initCodeSize(S).
checkSize(A,S) :- A > 10, initCodeSize(S).
 

% Generate max number of tries
% S: Size of code, C: Number of colors
genMaxT(_, _, MaxT) :- MaxT is 20.



% Translate S colors to integers.
% [H|T]: Code in colors, C: Code in integers
colToInt([], []).
colToInt([H|T], [I|R]) :- color(I,H),colToInt(T, R). 

%Exit command
%Ln: User input.
exitCmd(Ln) :- dif(Ln,[exit]).
exitCmd([exit]) :- halt.


% Parse line from colors to integers. 
% S: Size of code, Co: Number of different colors, C: Code in integers
% parseLine(_, _, _) :-write('Enter your guess: '), readln([exit]), halt.
parseLine(S, Co, C):-write('Enter your guess: '), readln(Ln), exitCmd(Ln), checkLine(Ln,S,Co, CLn), colToInt(CLn,C).

% Checks if line has S colors.
% Ln: List of colors, S: Number of colors in code,Co: Number of different colors, CLn: Correct input (Output).
checkLine(Ln, S, Co, Ln ) :- length(Ln, S), checkColors(Ln,Co).

checkLine(Ln, S, Co, CLn ) :- length(Ln, S), \+ checkColors(Ln,Co), 
write('Colors could not be recognized.'),nl, C1 is Co - 1,
showColoroptions(C1), write('Enter your new guess: '), readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).

checkLine(Ln, S, Co, CLn) :- length(Ln, D), dif(S,D), 
write('You need to provide '),write(S),write(' colors.'),nl, write('Enter your new guess: '), 
readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).


% Checks if colors in line is found.
% Ln: List of colors, Co: Number of different colors.
checkColors([],_).
checkColors([H|T],Co) :- color(CH,H), -1 < CH, CH < Co, checkColors(T,Co).


% accumulator initilization
matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).


remove([],_,[]).
remove([H|T],H,T).
remove([H|T],X,[H|R]):-dif(H,X),remove(T,X,R).


colormatch(_, _, [], []).                           % Base case Alpha omega super mand

colormatch([], [_], [_], []).                       % Base case Beta Batman

colormatch([_], [], _, []).                         % Base case Gamma Robin - Hende kender vi ikke, måske skal hun bare fjernes

colormatch([], [], [], []).                         % Base case Omega rosinen i pølseenden

colormatch([_|CodeT], [], Original, Res) :-        % Miss
    colormatch(CodeT, Original, Original, Res).

colormatch([H|CodeT], [H|_], Original, [0|Res]) :- % hit
    remove(Original,H,Ln), colormatch(CodeT, Ln, Ln, Res).

colormatch([H|CodeT], [X|MoveT], Original, Res) :- % Elements is not equal 
    dif(H,X), 
    colormatch([H|CodeT], MoveT, Original, Res). 
    

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


% Guess
% S: Size of code, C: Code in integers, Co: Number of different colors, Res: Result of guess
guess(S, C, Co, Res) :- parseLine(S,Co,G), matchinit(C, G, Res), write(Res), nl, write(G), nl, write(C), nl.







% No : the number of elements we want in the code
% Co : the number of differ colors
% Code : the resulting randomly generated code
generateCode(No, Co, Code) :- 
    length(Code, No), % the length of the result must match the number of input 
    maplist(random(0,Co), Code). % maps list of different 


%Show color options
% C: Number of colors
showColoroptions(Co) :-  write("Color options:"), nl, printColors(Co).

%Print colors
% C: Number of colors
printColors(0) :- write('blue'), nl.
printColors(Co) :- C1 is Co-1, color(Co,Color), write(Color), nl, printColors(C1).

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



% Check if the result of match is only 1s (meaning the guess is correct and the game has been won)
winningGuess([H|[]]) :- 
    H = 1.
winningGuess([H|T]) :- 
    H = 1,
    winningGuess(T).

checkGuess(Guess, Size) :- 
    length(Guess, Size), winningGuess(Guess).


% Game
% Co: Number of colors, S: Size of code, C: Code in integers, T: Number of tries used, MaxT: Total tries.
gameloop(Co, S, C, T, MaxT) :- guess(S, C, Co, Res), checkGuess(Res, S), T < MaxT. % You win

gameloop(Co, S, C, T, MaxT) :- guess(S, C, Co, Res), \+ checkGuess(Res, S), T1 is T+1, T1 < MaxT, gameloop(Co, S, Co, T1, MaxT). % Not the correct code, try again.

gameloop(Co, S, C, T, MaxT) :- guess(S,C, Co, Res), \+ checkGuess(Res, S), MaxT is T+1. % Not the correct code, try again.


% Start game
start :- write('Welcome to my universe - Let us play a very fun game!'),nl, initColor(Co), initCodeSize(S), genMaxT(S,Co,MaxT), generateCode(S, Co, C), C1 is Co-1, showColoroptions(C1), T is 0, gameloop(Co, S, C, T, MaxT).
