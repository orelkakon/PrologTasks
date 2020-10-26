%--------------------------------------------------------------------------- ex3.pl ---------------------------------------------------------------------------%
% coded by Orel Kakon.
% -------------------------------------------------------------------------- PART 1 -------------------------------------------------------------------------%
user:file_search_path(sat, './satsolver').
:- use_module(sat(satsolver)).

% I used in satsolver (Glucose 2.2)
% append(X,Y,Z)
%     Z is XUY
  append([],L,L).
  append([H|T],L2,[H|L3]) :- append(T,L2,L3).

% TASK1: sudoku_propagate(Instance, List) :- which unifies List with a list
% of as many assignment constraints possible
sudoku_propagate(sudoku(1,_), [cell(1,1) = 1]) :- !.
sudoku_propagate(sudoku(N, Hints), ResultList) :- 
						N > 1,
						Nnew is N * N,
						findall(cell(X,Y) = _, (between(1,Nnew,X), between(1,Nnew,Y)),Board), % build board
						build_struct(Board,Board,N,Structure), % build the data structure
						fill_struct(Hints,Structure), % insert the hints to board
						excl_func(full_values,Structure,Empty_values),
						do_propagate(Empty_values,Nnew),!, 
						incl_func(full_values,Structure,Res),
						exCells(Res,List), % extract just the cells
						reduction(List,Hints,ResultList). % delete the hints from final result 

% build the data structure
build_struct(_,[],_,[]).
build_struct(Board,[cell(X,Y) = Value|RestCells],N,[[cell(X,Y) = Value,First_val]|RestVals]):- 
						create_value(Board,X,Y,N,First_val),
						build_struct(Board,RestCells,N,RestVals).

create_value(Board,R,C,N,Val) :-
				neighborsColRow(Board,0,C,R,ResultNeighCol),
				neighborsColRow(Board,1,C,R,ResultNeighRow),
				neighborsBox(Board,N,C,R,ResultNeighBox),
				append(ResultNeighRow,ResultNeighCol,Combined),
				append(Combined,ResultNeighBox,Val).

% checks the columns and rows				
neighborsColRow(Board,0,C,R,Result) :- incl_func(neighborsColCont(C,R),Board,Result).
neighborsColRow(Board,1,C,R,Result) :- incl_func(neighborsRowCont(C,R),Board,Result).

neighborsColCont(C,R,cell(X,Y) = _ ):- Y = C, X \= R.	% ( X =:= 0 ->  Y = C, X \= R ;  X = R, Y \= C )	
neighborsRowCont(C,R,cell(X,Y) = _ ):- X = R, Y \= C.				

% checks the boxes
neighborsBox(Board,N,C,R,Result) :-  
				Nnew is N-1, 
				RowA is R-1, RowB is div(RowA,N), RowC is RowB * N,
				R1 is RowC+1, RN is R1 + Nnew,
				ColA is C-1, ColB is div(ColA,N), ColC is ColB * N,
				C1 is ColC+1, CN is C1 + Nnew,
				incl_func(neighborsBoxCont(C,R,C1,R1,CN,RN),Board,Result).
	
neighborsBoxCont(C,R,C1,R1,CN,RN,cell(X,Y) = _) :- between(R1,RN,X), between(C1,CN,Y), X \= R, Y \= C.

exCells([[cell(X,Y) = Value|_]|RestCells],[cell(X,Y) = Value| TheList]) :- exCells(RestCells,TheList).
exCells([],[]).

% prolog functions
incl_func(X,Y,Z) :- include(X,Y,Z).
excl_func(X,Y,Z) :- exclude(X,Y,Z).
reduction(X,Y,Z) :- subtract(X,Y,Z).

% insert the hints to sudoku board
fill_struct([],_).
fill_struct([cell(X,Y) = Value | RestCells], Structure) :- member([cell(X,Y) = Value,_],Structure),fill_struct(RestCells,Structure).

% do the propagate of board
do_propagate(Empty_values,N) :- select(Val,Empty_values,Res), fill_value(N,Val), do_propagate(Res,N).
do_propagate(_,_).

full_values([cell(_,_) = Val,_]) :- nonvar(Val).
value_is_var(cell(_,_) = Val) :- var(Val).

% insert the suitable value
fill_value(N,[cell(_,_) = Value , Neighbors]) :- Nnew is N-1, excl_func(value_is_var,Neighbors,NeighFullVal),
											     filter_duplicate_values(NeighFullVal,Result), length(Result,Nnew),
											     find_missing_value(Result,N,Value),!.

% create list without duplicate elements in the List NeighFullVal 
filter_duplicate_values(NeighFullVal,Result) :- take_just_values(NeighFullVal,ValResult), sort(ValResult,Result).

% for TASK2 ------------------
do_propagate2(Empty_values,N,[NewExplain|Explain]) :- select(Val,Empty_values,Res), fill_value2(N,Val,NewExplain), do_propagate2(Res,N,Explain). 
do_propagate2(_,_,[]).	

fill_value2(N,[cell(X,Y) = Value , Neighbors],Exp -> [cell(X,Y) = Value]) :-
												 Nnew is N-1, excl_func(value_is_var,Neighbors,NeighFullVal),
											     filter_duplicate_values(NeighFullVal,Result), length(Result,Nnew),
											     find_missing_value(Result,N,Value), !, getExp(NeighFullVal,Result,Exp).
												 
getExp(_,[],[]) :- !.												 
getExp([cell(X,Y) = Value | Neighbors],Ngroup,[cell(X,Y) = Value|Exp]) :- member(Value,Ngroup),delete(Ngroup,Value,Rest), getExp(Neighbors,Rest,Exp).
getExp([cell(_,_) = _ | Neighbors],Ngroup,Exp) :- getExp(Neighbors,Ngroup,Exp).
												 
% ----------------------------

% build list of all values from NeighFullVal list
take_just_values([cell(_,_) = Value | RestCells], [Value | RestValues]) :- take_just_values(RestCells,RestValues).
take_just_values([],[]).
												 
% find missing value in List (in domain 1...n)												 
find_missing_value(List,N,V) :-
		sumList(List,SumList), 
		sumN(N,SumN),
		V is SumN - SumList.

% calculate sum of 1...n
sumN(0,0) :- !. 
sumN(N,SumN) :- Nnew is N-1, sumN(Nnew,Sum1), SumN is N+Sum1.
		
% calculate the sum of elements list
sumList([],0).
sumList([Head|Tail], TotalSum):-
				sumList(Tail, Sum1),
				TotalSum is Head+Sum1.

% TASK2: sudoku_propagate_explaiמn(+,-) which given a Sudoku instance represented using the term.
% Instance = sudoku(Dimension, Hints)
sudoku_propagate_explain(sudoku(N, Hints), Explain) :- 
						Nnew is N * N,
						findall(cell(X,Y) = _, (between(1,Nnew,X), between(1,Nnew,Y)),Board), % build board
						build_struct(Board,Board,N,Structure), % build the data structure
						fill_struct(Hints,Structure), % insert the hints to board
						excl_func(full_values,Structure,Empty_values),
						do_propagate2(Empty_values,Nnew,Explain),!. 

% -------------------------------------------------------------------------- PART 2 -------------------------------------------------------------------------%
% TASK3: verify_killer(Instance,Solution,Verified) :- which unifies Verified with the term killer if and only if its
% second argument Solution represents a legal Killer Sudoku solution for the instance
% Instance in its first argument. 
verify_killer(killer(Hints),Solution,Verified) :- 
								check_hints(Hints,Solution,Verified),!;
								check_rows(Solution,Verified),!;
								check_columns(Solution,Verified),!;
								check_boxes(Solution,Verified),!;
								check_knight_move(Solution,Verified),!;
								check_king_move(Solution,Verified),!;
								check_adjacent_cells_horizontal(Solution,Verified),!;
								check_adjacent_cells_vertical(Solution,Verified),!;
								Verified = killer.
								
% check all numbers in rows are different
check_rows(Solution,[cell(X,Y1) = Val , cell(X,Y2) = Val]) :- 
									member(cell(X,Y1) = Val,Solution), member(cell(X,Y2) = Val, Solution),Y2 \= Y1, !.
% check all numbers in columns are different
check_columns(Solution,[cell(X1,Y) = Val , cell(X2,Y) = Val]) :- 
									member(cell(X1,Y) = Val,Solution), member(cell(X2,Y) = Val, Solution),X1 \= X2, !.

% check all numbers in boxes are different
check_boxes(Solution,[cell(X1,Y1) = Val, cell(X2,Y2) = Val]) :- (
						member(Row1,[1,4,7]),member(Column1,[1,4,7]),RowN is Row1 + 2, ColumnN is Column1 + 2,
						member(cell(X1,Y1) = Val, Solution),member(cell(X2,Y2) = Val, Solution), X1 \= X2, Y1 \= Y2,
						between(Row1,RowN,X1),between(Row1,RowN,X2),between(Column1,ColumnN,Y1),between(Column1,ColumnN,Y2)),!.


% check all numbers in knight move are different
check_knight_move(Solution,[cell(X1,Y1) = Val , cell(X2,Y2) = Val]) :- 						
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+2 , Y1 is Y2-1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+2 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-2 , Y1 is Y2-1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-2 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2+2),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2-2),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2+2),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2-2),!.
% check all numbers in knight move are different
check_king_move(Solution,[cell(X1,Y1) = Val , cell(X2,Y2) = Val]) :-
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2-1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2-1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2 , Y1 is Y2-1),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2),!;
									member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2),!.
% check all numbers in adjacent cells horizontal are different
check_adjacent_cells_horizontal(Solution,[cell(X1,Y1) = Val , cell(X2,Y2) = NewVal]) :-
									member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2-1 , Y1 is Y2),!;
									member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2+1 , Y1 is Y2),!.
% check all numbers in adjacent cells vertical are different
check_adjacent_cells_vertical(Solution,[cell(X1,Y1) = Val , cell(X2,Y2) = NewVal]) :-
									member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2 , Y1 is Y2+1),!;
									member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2 , Y1 is Y2-1),!.
% check all hints in solution are same
check_hints(Hints,Solution,[cell(X,Y) = Val1, cell(X,Y) = Val2]) :-															
						member(cell(X,Y) = Val1, Hints), member(cell(X,Y) = Val2, Solution), Val1 \= Val2.


% TASK4: encode_killer(+, -, -) :- which given a Killer Sudoku instance which is representedby a list of hints 
% (assignment constraints) Instance = killer(Hints)unifies Map with a representation of 
% the instance as a list of 81 terms of the formcell(I,J) = Value,
encode_killer(killer(Hints), Map, TOTAL_CNF) :- 
						findall(cell(X,Y) = [_,_,_,_,_,_,_,_,_], (between(1,9,X) , between(1,9,Y)),Map), % create Board
						create_rows(Map,1,Rows), create_CNF_rows(Rows,CNF1),
						create_columns(Map,1,Columns), create_CNF_columns(Columns,CNF2),
						create_boxes(Map,Boxes), create_CNF_boxes(Boxes,CNF3),
						create_CNF_kings_knight(Map,Map,CNF4),
						create_CNF_hints(Hints,Map,CNF5),
						create_CNF_of_legal_value(Map,CNF6),
						create_CNF_of_adjacent_cells(Map,Map,CNF7),
						append_all(CNF1,CNF2,CNF3,CNF4,CNF5,CNF6,CNF7,TOTAL_CNF).
%-------------
% analyze rows	
create_CNF_rows(Rows,CNF) :- create_CNF_cont(Rows,CNF).							 
cellsRows(Index,cell(Index,_) = _).
create_rows(Map,Index,Rows) :-
					Index < 10,
					incl_func(cellsRows(Index),Map,ResRows),
					take_just_values(ResRows,ResValRows),
					NewIndex is Index + 1,
					create_rows(Map,NewIndex,OtherRows),
					append([ResValRows],OtherRows,Rows).

create_rows(_,10,[]).
%--------------
% analyze columns
create_CNF_columns(Columns,CNF) :- create_CNF_cont(Columns,CNF).							 
cellsColumns(Index,cell(_,Index) = _).
create_columns(Map,Index,Columns) :-
					Index < 10,
					incl_func(cellsColumns(Index),Map,ResColumns),
					take_just_values(ResColumns,ResValColumns),
					NewIndex is Index + 1,
					create_columns(Map,NewIndex,OtherColumns),
					append([ResValColumns],OtherColumns,Columns).
create_columns(_,10,[]).
%---------------
% analyze boxes
create_CNF_boxes(Boxes,CNF) :- create_CNF_cont(Boxes,CNF).
create_boxes(Map,Boxes) :- incl_func(uniqueBox(1,1),Map,Box11),
						   incl_func(uniqueBox(1,4),Map,Box14),
						   incl_func(uniqueBox(1,7),Map,Box17),
						   incl_func(uniqueBox(4,1),Map,Box41),
						   incl_func(uniqueBox(4,4),Map,Box44),
						   incl_func(uniqueBox(4,7),Map,Box47),
						   incl_func(uniqueBox(7,1),Map,Box71),
						   incl_func(uniqueBox(7,4),Map,Box74),
						   incl_func(uniqueBox(7,7),Map,Box77),
						   create_boxes_cont([Box11,Box14,Box17,Box41,Box44,Box47,Box71,Box74,Box77],Boxes).

uniqueBox(Row1,Column1,cell(X,Y) = _) :- RowN is Row1 + 2, ColumnN is Column1 + 2, between(Row1,RowN,X),between(Column1,ColumnN,Y).

create_boxes_cont([CurrBox|RestBoxes],ValBoxes) :- take_just_values(CurrBox,ValBox),create_boxes_cont(RestBoxes,RestValBox), 
																		append([ValBox],RestValBox,ValBoxes).
create_boxes_cont([],[]).	
%---------------
% analyze hints
create_CNF_hints([cell(X,Y) = V | RestHints],Map,CNF) :- member(cell(X,Y) = Bits,Map),create_CNF_hints_cont(V,Bits,CurrCNF),
														 create_CNF_hints(RestHints,Map,RestCNF),
														 append([[CurrCNF]],RestCNF,CNF).
create_CNF_hints([],_,[]).													
create_CNF_hints_cont(Index, Bits, CNF) :- nth1(Index, Bits, CNF).
%---------------
% analyze king & knight
create_CNF_kings_knight(Map,[cell(X,Y) = V | RestMap],CNF):- 
													create_king_and_knights(V,Map,X,Y,KingKnights_neighbors),														
										            create_CNF_kings_knight_cont(V,KingKnights_neighbors,CurrCNF),
										            create_CNF_kings_knight(Map,RestMap,RestCNF),
											        append(CurrCNF,RestCNF,CNF).
create_CNF_kings_knight(_,[],[]).

create_king_and_knights(Values,Map,X,Y,KingKnights):-
											    XM is X - 2,XP is X + 2, YM is Y - 2, YP is Y + 2 ,
							                    incl_func(my_radious(XM,XP,YM,YP),Map,First_radious),
											    delete(First_radious, cell(X,Y) = Values, R1),
											    delete(R1,cell(XM,YM) = _, R2),
											    delete(R2,cell(XM,YP) = _, R3),
											    delete(R3,cell(XP,YM) = _, R4),
											    delete(R4,cell(XP,YP) = _, R5),
											    delete(R5,cell(XM,Y) = _, R6),
											    delete(R6,cell(XP,Y) = _, R7),
											    delete(R7,cell(X,YP) = _, R8),
											    delete(R8,cell(X,YM) = _, Neighbors),
										        take_just_values(Neighbors,KingKnights).
											   
my_radious(XM,XP,YM,YP, cell(X,Y) = _ ) :- between(XM,XP,X),between(YM,YP,Y).

create_CNF_kings_knight_cont(Bits,[NeighBits|RestKingKnights],CNF):-
												not_equalty(Bits,NeighBits,CurrCNF),
												create_CNF_kings_knight_cont(Bits,RestKingKnights,RestCNF),
												append(CurrCNF,RestCNF,CNF).
create_CNF_kings_knight_cont(_,[],[]).	

not_equalty([X | RestX],[Y | RestY],[[-X, -Y]|RestCNF]) :- not_equalty(RestX, RestY, RestCNF).
not_equalty([],[],[]).
%---------------
% analyze legal value
create_CNF_of_legal_value([cell(_,_) = V | RestMap],CNF) :- legalCNF(V,CurrCNF),
															create_CNF_of_legal_value(RestMap,RestCNF),
															append(CurrCNF,RestCNF,CNF).
create_CNF_of_legal_value([],[]). 
%---------------
% analyze adjacent cells
create_CNF_of_adjacent_cells(Map,[cell(X,Y) = V | RestMap],CNF) :- create_adjacent_cells(Map,X,Y,Neighbors),
																   create_CNF_of_adjacent_cells_cont(V,Neighbors,CurrCNF),
																   create_CNF_of_adjacent_cells(Map,RestMap,RestCNF),
																   append(CurrCNF,RestCNF,CNF).
create_CNF_of_adjacent_cells(_,[],[]).

create_adjacent_cells(Map,X,Y,Neighbors) :- XM is X - 1, XP is X + 1,
										    YM is Y - 1, YP is Y + 1,
						    			    incl_func(adjacent_neighbors(XM,XP,YM,YP,X,Y),Map,Result),
										    take_just_values(Result,Neighbors).

adjacent_neighbors(XM,_,_,_,_,Y,cell(XM,Y) = _).
adjacent_neighbors(_,XP,_,_,_,Y,cell(XP,Y) = _).
adjacent_neighbors(_,_,YM,_,X,_,cell(X,YM) = _).
adjacent_neighbors(_,_,_,YP,X,_,cell(X,YP) = _).


create_CNF_of_adjacent_cells_cont(_,[],[]) :- !.
create_CNF_of_adjacent_cells_cont(CellBits,[NeighBits | RestNeighBits],CNF) :-
												not_adjacent_cells(CellBits,NeighBits,CurrCNF),
												create_CNF_of_adjacent_cells_cont(CellBits,RestNeighBits,RestCNF),
												append(CurrCNF,RestCNF,CNF).

not_adjacent_cells([X1|RestXs],Ys,CNF) :- nth1(2,Ys,Y2), not_adjacent_cells_cont(2,RestXs,Ys,RestCNF), append([[-X1,-Y2]],RestCNF,CNF).
not_adjacent_cells_cont(IndexX,[X|RestXs],Ys,CNF):- IndexX < 9 ,PIndex is IndexX - 1 , NIndex is IndexX + 1,
												nth1(PIndex,Ys,Ym1),nth1(NIndex,Ys,Yp1),
												NextIndexX is IndexX +1,
												not_adjacent_cells_cont(NextIndexX,RestXs,Ys,RestCNF),
												append([[-X,-Ym1],[-X,-Yp1]],RestCNF,CNF).
not_adjacent_cells_cont(9,[X9],Ys,[[-X9,-Y8]]):- nth1(8,Ys,Y8).			
%---------------
append_all(C1,C2,C3,C4,C5,C6,C7,CNF) :- 
							append(C1,C2,C12), append(C12,C3,C123), append(C123,C4,C1234), append(C1234,C5,C12345),
						    append(C12345,C6,C123456), append(C123456,C7,CNF).
create_CNF_cont([Current | Rest],CNF):- create_CNF(Current,1,Cnf), create_CNF_cont(Rest,RestCNF), append(Cnf,RestCNF,CNF).
create_CNF_cont([],[]).

create_CNF(Line,Index,CNF):- 
						Index < 10 ,
						get_bits(Line,Index,Bits),
						legalCNF(Bits,Curr),
   					    NewIndex is Index + 1,
					    create_CNF(Line,NewIndex,RestCNF),
						append(Curr,RestCNF,CNF).
create_CNF(_,10,[]).

get_bits([Bits | RestBits],Index,[ChosenBit | RestChosenBits]):- nth1(Index,Bits,ChosenBit), get_bits(RestBits,Index,RestChosenBits).
get_bits([],_,[]).

legalCNF(Bits,CNF) :- append([CurrBit],RestBits,Bits),negative_CNF_pairs(CurrBit,RestBits,PairsCNF), append([Bits],PairsCNF,CNF).

negative_CNF_pairs(_,[],[]) :- !.
negative_CNF_pairs(Curr,[NextBit | RestBits], PairsCNF):- 
													negative_CNF_pairs_cont(Curr,[NextBit | RestBits], CurrPairs),
													negative_CNF_pairs(NextBit,RestBits,RestPairsCNF),
													append(CurrPairs,RestPairsCNF,PairsCNF).

negative_CNF_pairs_cont(X, [Y | Rest], [[-X,-Y] | RestPairs]) :- negative_CNF_pairs_cont(X, Rest, RestPairs).
negative_CNF_pairs_cont(_,[],[]).

%---------------					   

% TASK5:
decode_killer([],[]).
decode_killer([cell(X,Y) = V1 | RestMap],[cell(X,Y) = V2 | RestSol]) :- convert(V1,V2), decode_killer(RestMap,RestSol).
% convert 9 bits to number
convert([1,-1,-1,-1,-1,-1,-1,-1,-1],1):- !.  					   
convert([-1,1,-1,-1,-1,-1,-1,-1,-1],2):- !.  					   
convert([-1,-1,1,-1,-1,-1,-1,-1,-1],3):- !.  					   
convert([-1,-1,-1,1,-1,-1,-1,-1,-1],4):- !.  					   
convert([-1,-1,-1,-1,1,-1,-1,-1,-1],5):- !.  					   
convert([-1,-1,-1,-1,-1,1,-1,-1,-1],6):- !.  					   
convert([-1,-1,-1,-1,-1,-1,1,-1,-1],7):- !.  					   
convert([-1,-1,-1,-1,-1,-1,-1,1,-1],8):- !.  					   
convert([-1,-1,-1,-1,-1,-1,-1,-1,1],9):- !.  

solve_killer(Instance, Solution) :-
	encode_killer(Instance,Map,Cnf),
	sat(Cnf),
	decode_killer(Map, Solution),
	verify_killer(Instance, Solution, Verified),
	Verified = killer.

% TASK6:
legal_killer(Instance, IsLegal) :- 
	encode_killer(Instance,Map,CNF),
	satMulti(CNF,2,Sol,_),
	((Sol == 0 ) -> IsLegal = nosolution_to_board ; ((Sol==2) -> (find_different_example(Map,Dif_Cell),decode_killer(Dif_Cell,IsLegal) ); IsLegal = legal )).
% check the values in cell are different
is_different([X,Y]) :- X \= Y.
% find counter example 
find_different_example([cell(X,Y) = [[B11,B12],[B21,B22],[B31,B32],[B41,B42],[B51,B52],[B61,B62],[B71,B72],[B81,B82],[B91,B92]] 
									| RestCells], Dif_Cell) :-
									( ( is_different([B11,B12]) ; is_different([B21,B22]) ; is_different([B31,B32]) ; is_different([B41,B42]);
										is_different([B51,B52]) ; is_different([B61,B62]) ; is_different([B71,B72]) ; is_different([B81,B82]);
										is_different([B91,B92]) ) -> 
										create_cell(cell(X,Y) = [[B11,B12],[B21,B22],[B31,B32],[B41,B42],[B51,B52],[B61,B62],[B71,B72],[B81,B82],[B91,B92]],
										Dif_Cell) ; find_different_example(RestCells,Dif_Cell)).			 
% create the result cell
create_cell(cell(X,Y) = [[B11,B12],[B21,B22],[B31,B32],[B41,B42] , [B51,B52],[B61,B62],[B71,B72],[B81,B82],[B91,B92]],
			 [cell(X,Y) = [B11,B21,B31,B41,B51,B61,B71,B81,B91] , cell(X,Y) = [B12,B22,B32,B42,B52,B62,B72,B82,B92]]).							
						 
% TASK7: generate_killer(+,-) :- which given an integer
% K unifies Hints with a list of K hints such that killer(Hints) is  a  legal  Killer  Sudoku  instance.
% NOTE: WHEN K = 1 HAS NO LEGAL SOLUTION TO TASK.
generate_killer(K, Result) :- K > 1, findall(cell(X,Y) = _ , (between(1,9,X), between(1,9,Y)), SudokuBoard), !,
							  random_cells(K, SudokuBoard, Result), random_values(Result), check_solution(killer(Result)).

% random values to the chosen cells
random_values([cell(_,_) = V | RestChosenCells]):- between(1,9,V),random_values(RestChosenCells).
random_values([]) :- !.

% random cells to SudokuBoard
random_cells(K,Board,Result):- K > 0, select(Cell,Board,RestBoard), Knew is K-1 ,random_cells(Knew,RestBoard,RestCells), append([Cell],RestCells,Result).
random_cells(0,_,[]):-!.

% check if solution is 1 or 2 solutions 
check_solution(Instance) :- encode_killer(Instance,_,CNF),!,satMulti(CNF,2,1,_),!.
			