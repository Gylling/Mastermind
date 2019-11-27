% Mastermind

%Reads one char
readOneChar(I) :- readln([I|_]).

% init number of colors
% C: Number of colors (output)
initColor(C) :- nl, write('Enter number of colors (min. 2, max. 10) '), readOneChar(A), checkColor(A,C).

% check if color is out of bounds
% A: Number of colors (input), C: Number of colors (output)
checkColor(A,C) :- numberDB(A), C is A.
checkColor(A,C) :- \+ numberDB(A), nl, write('Wrong input'), nl, initColor(C).
checkColor(A,C) :- \+ numberDB(A), nl, write('Wrong input'), nl, initColor(C).

% init size of code
% S: Size of code (output)
initCodeSize(S) :- nl, write('Enter size of code (min. 2, max. 10) '), readOneChar(A), checkSize(A,S).

% check if size of code is out of bounds
% A: Size of code (input), S: Size of code (output)
checkSize(A,S) :- numberDB(A), S is A.
checkSize(A,S) :- \+ numberDB(A), nl, write('Wrong input'), nl, initCodeSize(S).
checkSize(A,S) :- \+ numberDB(A), nl, write('Wrong input'), nl, initCodeSize(S).
 

% Generate max number of tries
% S: Size of code, C: Number of colors
genMaxT(S, C, MaxT) :- MaxT is S*C-S, nl, write('You have '), write(MaxT), write(' tries to crack the code.'), nl.



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
parseLine(S, Co, C):-nl, write('Enter your guess: '), readln(Ln), exitCmd(Ln), checkLine(Ln,S,Co, CLn), colToInt(CLn,C).

% Checks if line has S colors.
% Ln: List of colors, S: Number of colors in code,Co: Number of different colors, CLn: Correct input (Output).
checkLine(Ln, S, Co, Ln ) :- length(Ln, S),  checkColors(Ln,Co).

checkLine(Ln, S, Co, CLn ) :- length(Ln, S), \+ checkColors(Ln,Co), 
nl,write('Colors could not be recognized.'),nl,
nl,showColoroptions(Co),nl, write('Enter your new guess: '), readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).

checkLine(Ln, S, Co, CLn) :- length(Ln, D), dif(S,D), 
nl,write('You need to provide '),write(S),write(' colors.'),nl,
write('Enter your new guess: '), 
readln(Ln1), exitCmd(Ln1), checkLine(Ln1,S, Co, CLn).


% Checks if colors in line is found.
% Ln: List of colors, Co: Number of different colors.
checkColors([],_).
checkColors([H|T],Co) :- C1 is Co+1, color(CH,H), 0 < CH, CH < C1, checkColors(T,Co).


% accumulator initilization
matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).

% Remove the first element of X from a list. 
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
    Co1 is Co+1, %increment so the biggest number is included
    maplist(random(1,Co1), Code). % maps list of different 


%Show color options
% C: Number of colors
showColoroptions(Co) :-  write("Color options:"), nl, printColors(Co).

%Print colors
% C: Number of colors
printColors(0).
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

%Number database
numberDB(2).
numberDB(3).
numberDB(4).
numberDB(5).
numberDB(6).
numberDB(7).
numberDB(8).
numberDB(9).
numberDB(10).



% Check if the result of match is only 1s (meaning the guess is correct and the game has been won)
winningGuess([H|[]]) :- 
    H = 1.
winningGuess([H|T]) :- 
    H = 1,
    winningGuess(T).

checkGuess(Guess, Size) :- 
    length(Guess, Size), winningGuess(Guess).


%Play again?
playagain :- nl, write('Do you want to play again? (yes/no) '),readln(Ln), checkPaInput(Ln). 

checkPaInput([H|_]) :- H = 'yes', init.
checkPaInput([H|_]) :- H = 'no'.
checkPaInput([H|_]) :- dif(H,'yes'), dif(H,'no'),nl,nl, write('Please write yes or no. '),readln(Ln),checkPaInput(Ln).

% Game
% Co: Number of colors, S: Size of code, C: Code in integers, T: Number of tries used, MaxT: Total tries.

gameloop(_, _, _, T, MaxT) :-  MaxT is T, write('No more tries left. You lose. Please try again.'),nl, playagain, halt. % All tries used, you lose.
gameloop(Co, S, C, T, MaxT) :- T < MaxT, guess(S, C, Co, Res), \+ checkGuess(Res, S), T1 is T+1, write('Number of tries: '), write(T1), nl, gameloop(Co, S, C, T1, MaxT). % Not the correct code, try again.
gameloop(_, _, _, T, MaxT) :- T < MaxT, write('Number of tries: '),T1 is T+1, write(T1), nl,nl, write('Congrats! You cracked the code!'), nl, playagain, halt. % You win

% Rules
printRules :- write('The rules are simple.'), nl,
write('You choose a size for a code and how many different colors the code can be made of.'), nl,
write('You have limited number of tries to break a code.'),nl, 
write('The number of tries is scaling with the size and number of colors you choose.'), nl,
write('You will enter a guess for each try. You shall write colors with lowercase letters seperated by spaces.'), nl, 
write('A code of the size 4 will expect your guesses to be of the format: "<color> <color> <color> <color>"'), nl, nl,

write('You will get a list of hints back for each guess.'), nl,
write('If you have a correct color but a wrong position then you will see a 0 in the list.'), nl,
write('If you have a correct color and position then you will see a 1 in the list.'), nl,
write('You will see an empty list, if you do not have anything correct'), nl,
write('Now you are ready to play.'), nl.

%init game
init :- initColor(Co), initCodeSize(S), genMaxT(S,Co,MaxT), generateCode(S, Co, C), nl, showColoroptions(Co), T is 0, gameloop(Co, S, C, T, MaxT).

% Start game
start :- write('Welcome to my universe - Let us play a very fun game!'),nl,nl,printRules,nl, init.
