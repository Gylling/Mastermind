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
genMaxT(_, _, MaxT) :- MaxT is 5.



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
checkLine(Ln, S, Co, Ln ) :- length(Ln, S),  checkColors(Ln,Co).

checkLine(Ln, S, Co, CLn ) :- length(Ln, S), \+ checkColors(Ln,Co), 
write('Colors could not be recognized.'),nl,
showColoroptions(Co), write('Enter your new guess: '), readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).

checkLine(Ln, S, Co, CLn) :- length(Ln, D), dif(S,D), 
write('You need to provide '),write(S),write(' colors.'),nl, write('Enter your new guess: '), 
readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).


% Checks if colors in line is found.
% Ln: List of colors, Co: Number of different colors.
checkColors([],_).
checkColors([H|T],Co) :- C1 is Co+1, color(CH,H), 0 < CH, CH < C1, checkColors(T,Co).


% accumulator initilization
matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).


remove([],_,[]).
remove([H|T],H,T).
remove([H|T],X,[H|R]):-dif(H,X),remove(T,X,R).


% Base cases
colormatch([], [], [], []).
colormatch(_, _, [], []).
colormatch([], [_], [_], []).
colormatch([X|[]], [Y|[]], _, []) :- dif(X,Y).

colormatch([_|CodeT], [], Original, Res) :-        % Not at all to be found in the guess, try next element of the code
    colormatch(CodeT, Original, Original, Res).

colormatch([H|CodeT], [H|_], Original, [0|Res]) :- % Elements are equal
    remove(Original,H,Ln), colormatch(CodeT, Ln, Ln, Res).

colormatch([H|CodeT], [X|MoveT], Original, Res) :- % Elements are not equal 
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
guess(S, C, Co, Res) :- parseLine(S,Co,G), once(matchinit(C, G, Res)), write(Res), nl, write(G), nl, write(C), nl.







% No : the number of elements we want in the code
% Co : the number of differ colors
% Code : the resulting randomly generated code
generateCode(No, Co, Code) :- 
    length(Code, No), % the length of the result must match the number of input 
    maplist(random(1,Co), Code). % maps list of different 


%Show color options
% C: Number of colors
showColoroptions(Co) :-  write("Color options:"), nl, printColors(Co).

%Print colors
% C: Number of colors
printColors(0) :- nl.
printColors(Co) :- C1 is Co-1, color(Co,Color), write(Color), nl, printColors(C1).

%Color database
color(1,blue).
color(2,green).
color(3,brown).
color(4,red).
color(5,yellow).
color(6,orange).
color(7,white).
color(8,black).
color(9, grey).
color(10,purple).



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

gameloop(_, _, _, T, MaxT) :- write(T), MaxT is T+1, nl, write('No more tries left. You lose. Please try again.'). % All tries used, you lose.
gameloop(Co, S, C, T, MaxT) :- write(T), T1 is T+1, T1 < MaxT, nl, write('This is the normal game loop'), nl, guess(S, C, Co, Res), \+ checkGuess(Res, S), gameloop(Co, S, C, T1, MaxT). % Not the correct code, try again.
gameloop(_, _, _, T, MaxT) :- write(T), T < MaxT, nl, write('Congrats! You cracked the code!'). % You win



% Start game
start :- write('Welcome to my universe - Let us play a very fun game!'),nl, initColor(Co), initCodeSize(S), genMaxT(S,Co,MaxT), generateCode(S, Co, C), showColoroptions(Co), T is 0, gameloop(Co, S, C, T, MaxT).
