-- Mastermind

colormatch([], _, _, []).
colormatch([_|CodeT], [], Original, Res) :- 
    colormatch(CodeT, Original, Original, Res).
colormatch([H|CodeT], [H|_], Original, [0|Res]) :-
    colormatch(CodeT, Original, Original, Res).
colormatch([H|CodeT], [X|MoveT], Original, Res) :- 
    dif(H,X), 
    colormatch([H|CodeT], MoveT, Original, Res).


% accumulator initilization
matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).

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





















































:-run(intro)
