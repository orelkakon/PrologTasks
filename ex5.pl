%--------------------------------------------------------------------- ex5.pl -------------------------------------------------------------------------------
%Coded by Orel Kakon
%--------------------------------------------------------------------- Part 1 -------------------------------------------------------------------------------
% TASK 1
%% knightMove(+,+,-).
knightMove(N,cell(X,Y),cell(I,J)) :- allKnightMove(cell(X,Y), cell(I,J)), I > 0, J > 0, I =< N, J =< N.

allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 + 2 , Y2 is Y1 + 1. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 - 2 , Y2 is Y1 + 1. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 + 2 , Y2 is Y1 - 1. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 - 2 , Y2 is Y1 - 1. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 + 1 , Y2 is Y1 + 2. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 - 1 , Y2 is Y1 + 2. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 + 1 , Y2 is Y1 - 2. 
allKnightMove(cell(X1,Y1),cell(X2,Y2)) :- X2 is X1 - 1 , Y2 is Y1 - 2. 

% TASK 2
%% allKnightMoves(+,+,-).
allKnightMoves(N,cell(X,Y),List) :- findall(Cell,knightMove(N,cell(X,Y),Cell),List).


% TASK 3
%% multiKnightMoves(+,+,+,-).
multiKnightMoves(0,_,cell(I,J),[cell(I,J)]) :- !.
multiKnightMoves(1, N, cell(X,Y), Moves) :- allKnightMoves(N, cell(X,Y), Moves), !.
multiKnightMoves(K, N, cell(X,Y), Moves) :- K > 1, multiKnightMovesCont(K, N, [cell(X,Y)], Res), redundantDuplicates(Res,Moves).

multiKnightMovesCont(_,_,[],[]) :- !.
multiKnightMovesCont(1,N,[cell(X,Y)|RestCells],Moves):- allKnightMoves(N,cell(X,Y),C), multiKnightMovesCont(1,N,RestCells,RC), append(RC,C,Moves),!.
multiKnightMovesCont(K,N,[cell(X,Y)|RestCells],Moves) :- K > 1, allKnightMoves(N,cell(X,Y),NextCells), Knew is K - 1,
									multiKnightMovesCont(Knew,N,NextCells,NC), multiKnightMovesCont(K,N,RestCells,RC), append(NC,RC,Moves), !.

redundantDuplicates([],[]) :- !.
redundantDuplicates([H | T], List) :-    
     member(H, T),
     redundantDuplicates( T, List), !.
redundantDuplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      redundantDuplicates( T, T1), !.
%--------------------------------------------------------------------- Part 2 -------------------------------------------------------------------------------
% TASK 1
%% literal(+,-).
literal(X,(X=1)) :- var(X),!.
literal(-X,(X = -1)) :- var(X),!. 
literal(-X,A):- literal(-X,A,0),!.
literal(-X,A,MinusCount):- nonvar(X), MinusCountNew is MinusCount + 1, literal(X,A,MinusCountNew).
literal(-X,(X = -1),MinusCount):- MCN is mod(MinusCount,2),MCN == 0,!.
literal(-X,(X = 1),MinusCount):- MCN is mod(MinusCount,2),MCN == 1,!.

% TASK 2
%% true(+).
true([]) :- false, !.
true([X|Rest]) :- nonvar(X), (checkMinus(X,0) -> true ; true(Rest) ), !.
true([_|R]) :- true(R), !.
checkMinus(-X,N):-nonvar(X), Nnew is N + 1, checkMinus(X,Nnew), !.
checkMinus(X,N):- is_one(X), MinusNum is mod(N,2), (X == -1 , MinusNum == 1 ; X == 1 ,MinusNum == 0). 
is_one(1).
is_one(-1).

% TASK 3
%% unit(+,-).
unit(C,A) :- (\+ true(C)), checkC(C,0,[Res]), literal(Res,A).
checkC([X|R],I,Res) :- reduceLit(X), NewI is I + 1, append([X],[],Res),checkC(R,NewI,Res), !.
checkC([X|R],I,Res) :- reduceNum(X), checkC(R,I,Res), !.
checkC([],1,_).

reduceLit(X) :- var(X),!.
reduceLit(-X) :- var(X),!.
reduceLit(- X) :- reduceLit(X),!.
reduceNum(X) :- number(X),!.
reduceNum(- X) :- nonvar(X), reduceNum(X),!.  



% TASK 4
%% propagate(+,-).
propagate(Cnf,CNF):- select(C,Cnf,RestCnf),( ( true(C) ; unit(C,A) ) -> updateValues(A)), propagate(RestCnf,CNF),!.
propagate(Cnf,Cnf):- \+ (select(C,Cnf,_) ,( true(C) ; unit(C,_) )). % has no more true OR unit cnf that possible
updateValues((X = Y)) :- X = Y.		

%--------------------------------------------------------------------- Part 2 -------------------------------------------------------------------------------
:- use_module('../../satsolver/satsolver',[sat/1]).

% TASK 1
%% triplets(+,-).
triplets(N,Ts) :- findall((A,B,C), ( between(1,N,A), between(1,N,B), between(1,N,C), A=<B, A2 is A ** 2, B2 is B ** 2, A2B2 is A2 + B2 , A2B2 is C ** 2),Ts).


% TASK 2
%% verify(+,+,-).
verify(N,sol(Set1,Set2),Result):- 
							checkUnion(Set1,Set2,N,Result),!;
							checkIntersection(Set1,Set2,Result),!;
						    checkTs(Set1,Set2,Result),!;
							Result=ok,!.

checkTs(S1,_,(A,B,C):S1) :- member(A,S1),member(B,S1),member(C,S1), A2 is A**2, B2 is B**2 ,C2 is C**2, A2B2 is A2 + B2, C2 is A2B2,!.
checkTs(_,S2,(A,B,C):S2) :- member(A,S2),member(B,S2),member(C,S2), A2 is A**2, B2 is B**2 ,C2 is C**2, A2B2 is A2 + B2, C2 is A2B2.
checkUnion(S1,S2,N,union(S1,S2)\=Set) :- union(S1,S2,Union), numlist(1,N,Set),(\+ subtract(Set,Union,[])) .
checkIntersection(S1,S2,intersection(S1,S2,Inter)) :- intersection(S1,S2,Inter), length(Inter,Len), Len > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve(N,Sol) :-
    encode(N,Map,Cnf),
    (sat(Cnf) -> decode(Map,Sol) ; Sol=unsat),  
    verify(N,Sol,V),
    writeln(verify:V).


% TASK 3
%% encode(+,-,-).
encode(N,Map,CNF) :- length(Map,N), triplets(N,Ts), createCnf(Map,Ts,Cnf1), getRestNum(Ts,N,NumList), 
                                createRestCnf(Map,NumList,Cnf2), append(Cnf1,Cnf2,CNF).
getRestNum(Ts,N,List) :- getList(Ts,FlatTs), sort(FlatTs,SortFlatTs), numlist(1,N,SetN),subtract(SetN,SortFlatTs,List).                                                                   
getList([(A,B,C) | RestT],[A,B,C | Rest]) :- getList(RestT,Rest). 
getList([],[]) :- !.
createRestCnf(_,[],[]) :- !.
createRestCnf(Map,[X1|Xs],[[X2]|Rest]) :- nth1(X1,Map,X2), createRestCnf(Map,Xs,Rest).
createCnf(_,[],[]) :- !.
createCnf(Map,[(A,B,C)|Ts],CNF) :- createCnfCont(Map,(A,B,C),FirstCnf),createCnf(Map,Ts,RestCnf),append(FirstCnf,RestCnf,CNF).
createCnfCont(Map,(A,B,C),[[-A1,-B1,-C1],[A1,B1,C1]]) :- nth1(A,Map,A1), nth1(B,Map,B1), nth1(C,Map,C1).

% Part 4
%% decode(+,-).
decode(Map,sol(S1,S2)) :- findall(X1,nth1(X1,Map,1),S1), findall(X2,nth1(X2,Map,-1),S2).
