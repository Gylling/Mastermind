
remove([],_,[]).
remove([H|T],H,T).
remove([H|T],X,[H|R]):-dif(H,X),remove(T,X,R).

matchinit(Code, Move, Res) :- posmatch(Code, Move, [], [], [], Res).


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