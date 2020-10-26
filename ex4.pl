%--------------------------------------------------------------------------- ex4.pl -------------------------------------------------------
% coded by Orel Kakon.
% -------------------------------------------------------------------------- PART 1 -------------------------------------------------------

% consult BEE compiler module
:- use_module('../../beeCompiler/bCompiler',[bCompile/2]).
:- use_module('../../satsolver/satsolver',[sat/1]).
:- use_module('../auxs/auxRunExpr',[runExpr/5, decodeIntArray/2,decodeInt/2]).
:- use_module('../auxs/auxRunExprAll',[runExprAll/5]).


% TASK1: kakuroVerify(Instance, Solution) :- which succeeds if and only if its first argument represents a legal
% Kakuro instance, and its second argument represents a legal solution to that instance
kakuroVerify(Instance, Solution) :- checkInstance(Instance,Solution), checkSol(Solution).

checkSol([]) :- !.
checkSol([Sum = FirstBlock | RestBlocks]):- sumlist(FirstBlock,Sum), allListIsDifferent(FirstBlock), 
											lessEqualNine(FirstBlock), checkSol(RestBlocks).

checkInstance([],[]) :- !.
checkInstance([Value = BlockI|Instance],[Value = BlockS|Solution]):- length(BlockI,N),length(BlockS,N),checkInstance(Instance,Solution).

% check if all the elements in list are diffrenet
allListIsDifferent([]).
allListIsDifferent([H|T]):-
    \+ member(H,T), allListIsDifferent(T).

% check that all elements in List are in domain 1...9
lessEqualNine([]) :- !.
lessEqualNine([CurrNum | RestList]) :- CurrNum > 0 , CurrNum < 10, lessEqualNine(RestList).  



% TASK2: kakuroEncode(+,-,-) :- he predicate takes an instance of Kakuro, and encodes
% it to a set of BEEconstraints Constraints
kakuroEncode(Instance,Map,Constraints) :-
							Instance = Map,
							getBlocks(Instance, Blocks), flatten(Blocks,UnionBlock), 
							sort(UnionBlock,SortUnionBlock), getDomainCon(SortUnionBlock,DomainCons),
							getConstraints(Instance,RestConstraints), append(DomainCons,RestConstraints,Constraints).

getBlocks([],[]) :- !.
getBlocks([_ = CurrBlock | RestBlocks], [CurrBlock | RestResult]) :- getBlocks(RestBlocks,RestResult).

getConstraints([],[]).
getConstraints([CurrVal = CurrNums | RestClues], ResultCon) :- 
										getAllDiff(CurrNums,AllDiffCons), getSumCons(CurrVal,CurrNums,SumCons), 
										append([AllDiffCons],[SumCons],AppendCons), getConstraints(RestClues,NewResultCon),
										append(AppendCons,NewResultCon,ResultCon).

getAllDiff(CurrNums,int_array_allDiff(CurrNums)).

getSumCons(CurrVal,CurrNums,int_array_plus(CurrNums,CurrVal)).

getDomainCon([],[]) :- !.
getDomainCon([First | RestNums],[new_int(First,1,9) | RestDomainCons]) :- getDomainCon(RestNums,RestDomainCons).

% TASK3: kakuroDecode(+,-) :- which decodes the Map of variables
kakuroDecode([],[]) :- !.
kakuroDecode([Value = Block | RestM],[Value = DecodeBlock | RestS]) :- 
	bDecode:decodeIntArray(Block,DecodeBlock), kakuroDecode(RestM , RestS).

% TASK4: kakuroSolve(+,-) :- that takes a Kakuro instance, solves it using BEE
kakuroSolve(Instance,Solution) :-
			writef('%w,',[Instance]),flush_output,
    		runExpr(Instance,Solution,
            		ex4:kakuroEncode,
            		ex4:kakuroDecode,
            		ex4:kakuroVerify).

% -------------------------------------------------------------------------- PART 2 -------------------------------------------------------
% TASK5: verify_killer(Instance,Solution,Verified) :- which unifies Verified with the term killer if and only if its
% second argument Solution represents a legal Killer Sudoku solution for the instance
% Instance in its first argument. 
verify_killer(killer(Hints),Solution) :- 
										\+(check_hints(Hints,Solution)),
										\+(check_rows(Solution)),
							            \+(check_columns(Solution)),
				    		            \+(check_boxes(Solution)),
										\+(check_king_move(Solution)),
										\+(check_adjacent_cells_horizontal(Solution)),										 
										\+(check_adjacent_cells_vertical(Solution)),
										\+(check_knight_move(Solution)).
								
% check all numbers in rows are different
check_rows(Solution) :- 
					member(cell(X,Y1) = Val,Solution), member(cell(X,Y2) = Val, Solution),Y2 \= Y1, !.
% check all numbers in columns are different
check_columns(Solution) :- 
					member(cell(X1,Y) = Val,Solution), member(cell(X2,Y) = Val, Solution),X1 \= X2, !.

% check all numbers in boxes are different
	check_boxes(Solution) :- (
						member(Row1,[1,4,7]),member(Column1,[1,4,7]),RowN is Row1 + 2, ColumnN is Column1 + 2,
						member(cell(X1,Y1) = Val, Solution),member(cell(X2,Y2) = Val, Solution), X1 \= X2, Y1 \= Y2,
						between(Row1,RowN,X1),between(Row1,RowN,X2),between(Column1,ColumnN,Y1),between(Column1,ColumnN,Y2)),!.


% check all numbers in knight move are different
check_knight_move(Solution) :- 						
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+2 , Y1 is Y2-1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+2 , Y1 is Y2+1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-2 , Y1 is Y2-1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-2 , Y1 is Y2+1),!;								member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2+2),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2-2),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2+2),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2-2),!.
% check all numbers in knight move are different
check_king_move(Solution) :-
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2-1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2+1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2-1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2+1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2 , Y1 is Y2+1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2 , Y1 is Y2-1),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2+1 , Y1 is Y2),!;
							member(cell(X1,Y1) = Val,Solution), member(cell(X2,Y2) = Val,Solution), (X1 is X2-1 , Y1 is Y2),!.
% check all numbers in adjacent cells horizontal are different
check_adjacent_cells_horizontal(Solution) :-
							member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2-1 , Y1 is Y2),!;
							member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2+1 , Y1 is Y2),!.
% check all numbers in adjacent cells vertical are different
check_adjacent_cells_vertical(Solution) :-
							member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2 , Y1 is Y2+1),!;
							member(cell(X1,Y1) = Val,Solution), NewVal is Val+1, member(cell(X2,Y2) = NewVal,Solution), (X1 is X2 , Y1 is Y2-1),!.
% check all hints in solution are same
check_hints(Hints,Solution) :-															
							member(cell(X,Y) = Val1, Hints), member(cell(X,Y) = Val2, Solution), Val1 \= Val2.

%------------------------------------------------------------------------------------------
% TASK6: encode_killer(+, -, -) :- which given a Killer Sudoku instance which is represented by a list of hints
%  (assignment constraints) Instance = killer(Hints) unifies Map with a rep - resentation of the instance as a list of 81 term
encode_killer(killer(Hints), Map, TOTAL_CONSTRAINTS) :- 
						findall(cell(X,Y) = _, (between(1,9,X) , between(1,9,Y)),Map), % create Board
						fill_struct(Hints,Map),
						incl_func(novalue,Map,Map_without_hints),
						create_CONSTRAINTS_of_legal_value(Map_without_hints,CONSTRAINTS1),
						create_rows(Map,1,Rows), create_CONSTRAINTS_rows(Rows,CONSTRAINTS2),
						create_columns(Map,1,Columns), create_CONSTRAINTS_columns(Columns,CONSTRAINTS3),
						create_boxes(Map,Boxes), create_CONSTRAINTS_boxes(Boxes,CONSTRAINTS4),
						create_CONSTRAINTS_kings_knight(Map,Map,CONSTRAINTS5),
						create_CONSTRAINTS_of_adjacent_cells(Map,Map,CONSTRAINTS6),
						append_all(CONSTRAINTS1,CONSTRAINTS2,CONSTRAINTS3,CONSTRAINTS4,CONSTRAINTS5,CONSTRAINTS6,TOTAL_CONSTRAINTS),!.
%-------------
novalue(cell(_,_) = Val) :- var(Val).
%-------------
fill_struct([],_) :- !.
fill_struct([cell(X,Y) = Value | RestCells], Map) :- member(cell(X,Y) = Value, Map),fill_struct(RestCells,Map).
%-------------
% analyze rows	
create_CONSTRAINTS_rows(Rows,CONSTRAINTS) :- create_CONSTRAINTS_cont(Rows,CONSTRAINTS).							 
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
create_CONSTRAINTS_columns(Columns,CONSTRAINTS) :- create_CONSTRAINTS_cont(Columns,CONSTRAINTS).							 
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
create_CONSTRAINTS_boxes(Boxes,CONSTRAINTS) :- create_CONSTRAINTS_cont(Boxes,CONSTRAINTS).
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

uniqueBox(Row1,Column1,cell(X,Y) = _) :- RowN is Row1 + 2, ColumnN is Column1 + 2, between(Row1,RowN,X), between(Column1,ColumnN,Y).

create_boxes_cont([CurrBox|RestBoxes],ValBoxes) :- take_just_values(CurrBox,ValBox),create_boxes_cont(RestBoxes,RestValBox), 
																		append([ValBox],RestValBox,ValBoxes).
create_boxes_cont([],[]).	
%---------------
create_CONSTRAINTS_cont([Line|RestLine],[int_array_allDiff(Line)|RestConstrains]):- 
														create_CONSTRAINTS_cont(RestLine,RestConstrains).
														
create_CONSTRAINTS_cont([],[]).
%---------------
% analyze king & knight
create_CONSTRAINTS_kings_knight(Map,[cell(X,Y) = Val| RestMap],CONSTRAINTS):- 
													create_king_and_knights(Val,Map,X,Y,KingKnights_neighbors),														
										            create_CONSTRAINTS_kings_knight_cont(Val,KingKnights_neighbors,CurrCONSTRAINTS),
										            create_CONSTRAINTS_kings_knight(Map,RestMap,RestCONSTRAINTS),
											        append(CurrCONSTRAINTS,RestCONSTRAINTS,CONSTRAINTS).
create_CONSTRAINTS_kings_knight(_,[],[]).

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

create_CONSTRAINTS_kings_knight_cont(Val,[Neighbor|RestKingKnights_Neighbors],[int_neq(Val,Neighbor) | RestConstrains]):-
									create_CONSTRAINTS_kings_knight_cont(Val,RestKingKnights_Neighbors,RestConstrains).
create_CONSTRAINTS_kings_knight_cont(_,[],[]).	
%---------------
% analyze legal value
create_CONSTRAINTS_of_legal_value([cell(_,_) = Val | RestMap],[new_int(Val,1,9) | RestConstrains]) :- 
															create_CONSTRAINTS_of_legal_value(RestMap,RestConstrains).
create_CONSTRAINTS_of_legal_value([],[]) :- !.
%---------------
% analyze adjacent cells
create_CONSTRAINTS_of_adjacent_cells(Map,[cell(X,Y) = Val | RestMap],CONSTRAINTS) :- create_adjacent_cells(Map,X,Y,Neighbors),
																   create_CONSTRAINTS_of_adjacent_cells_cont(Val,Neighbors,CurrCONSTRAINTS),
																   create_CONSTRAINTS_of_adjacent_cells(Map,RestMap,RestCONSTRAINTS),
																   append(CurrCONSTRAINTS,RestCONSTRAINTS,CONSTRAINTS).
create_CONSTRAINTS_of_adjacent_cells(_,[],[]).

create_adjacent_cells(Map,X,Y,Neighbors) :- XM is X - 1, XP is X + 1,
										    YM is Y - 1, YP is Y + 1,
						    			    incl_func(adjacent_neighbors(XM,XP,YM,YP,X,Y),Map,Result),
										    take_just_values(Result,Neighbors).

adjacent_neighbors(XM,_,_,_,_,Y,cell(XM,Y) = _).
adjacent_neighbors(_,XP,_,_,_,Y,cell(XP,Y) = _).
adjacent_neighbors(_,_,YM,_,X,_,cell(X,YM) = _).
adjacent_neighbors(_,_,_,YP,X,_,cell(X,YP) = _).


create_CONSTRAINTS_of_adjacent_cells_cont(_,[],[]) :- !.	
create_CONSTRAINTS_of_adjacent_cells_cont(Val, [Neighbor | RestAdjacentNeighbors],CONSTRAINTS):-
												Constrains = [
												new_int(Value1,2,10),int_plus(Val,1,Value1),int_neq(Value1,Neighbor),
												new_int(Value2,2,10),int_plus(Neighbor,1,Value2),int_neq(Value2,Val)],
												create_CONSTRAINTS_of_adjacent_cells_cont(Val,RestAdjacentNeighbors,RestConstrains),
												append(Constrains,RestConstrains,CONSTRAINTS).
%---------------
% prolog function
incl_func(X,Y,Z) :- include(X,Y,Z).

append_all(C1,C2,C3,C4,C5,C6,CONSTRAINTS) :- 
							append(C1,C2,C12), append(C12,C3,C123), append(C123,C4,C1234), append(C1234,C5,C12345),
						    append(C12345,C6,CONSTRAINTS).
%------------------------------------------------------------------------------------------
solve_killer(Instance, Solution) :-
	runExpr(Instance, Solution, encode_killer, decode_killer, verify_killer).

% TASK7: decode_killer(+, -) :- which creates from the given Map a corresponding solution 
% in the format which is a list of (81) assignment constraints
decode_killer([],[]) :- !.
decode_killer([cell(X,Y) = EncodeValue | RestM], [cell(X,Y) = DecodeValue | RestS]) :-
                                         bDecode:decodeInt(EncodeValue,DecodeValue),
                                         decode_killer(RestM,RestS).			


% TASK8: all_killer(+, -) :- which given an instance of a Killer Sudoku puzzle unifies Solutions
all_killer(Instance, Solutions):-
				runExprAll(Instance, Solutions, ex4:encode_killer, ex4:decode_killer, ex4:verify_killer).
% encode_killer(+,-,-,-) :- same like regular encode with addition of RelevantBools & RelevantInts to predicate 
encode_killer(killer(Hints),Map,(RelevantBools,RelevantInts),TOTAL_CONSTRAINTS) :-
						findall(cell(X,Y) = _, (between(1,9,X) , between(1,9,Y)),Map), % create Board
						fill_struct(Hints,Map),
						incl_func(novalue,Map,Map_without_hints),
						create_CONSTRAINTS_of_legal_value(Map_without_hints,CONSTRAINTS1),
						create_rows(Map,1,Rows), create_CONSTRAINTS_rows(Rows,CONSTRAINTS2),
						create_columns(Map,1,Columns), create_CONSTRAINTS_columns(Columns,CONSTRAINTS3),
						create_boxes(Map,Boxes), create_CONSTRAINTS_boxes(Boxes,CONSTRAINTS4),
						create_CONSTRAINTS_kings_knight(Map,Map,CONSTRAINTS5),
						create_CONSTRAINTS_of_adjacent_cells(Map,Map,CONSTRAINTS6),
						append_all(CONSTRAINTS1,CONSTRAINTS2,CONSTRAINTS3,CONSTRAINTS4,CONSTRAINTS5,CONSTRAINTS6,TOTAL_CONSTRAINTS),
						RelevantBools = [],
						take_just_values(Map_without_hints,ValueResult),
    					RelevantInts = ValueResult, !.

% build list of all values from NeighFullVal list
take_just_values([cell(_,_) = Value | RestCells], [Value | RestValues]) :- take_just_values(RestCells,RestValues).
take_just_values([],[]).