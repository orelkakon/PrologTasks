% coded by orel kakon

% append(X,Y,Z)
%     Z is XUY
  append([],L,L).
  append([H|T],L2,[H|L3]) :- append(T,L2,L3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------------------------PART 1----------------------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
create_symbols(T,[_|Rest]) :- T > 0, Tnew is T-1, create_symbols(Tnew,Rest).
create_symbols(0,[]).	
	
% Task 1: addition_circuit(N, Xs, Ys, Zs, Fs) :- mode addition_circuit(+,-,-,-,-) , 
% takes as input a positive number N and constructs an addition-circuit Fs on input bits
% Xs = [X1, . . . , XN] and Ys = [Y1, . . . , YN], and output bits Zs = [Z1, . . . , ZN, ZN+1]
addition_circuit(N,Xs,Ys,Zs,Fs) :- N > 0, Nup is N+1, Ndown is N-1, create_symbols(N,Xs), create_symbols(N,Ys), create_symbols(Nup,Zs), 
												create_symbols(Ndown,Cs), create_full_adders(N,Xs,Ys,Zs,Cs,Fs).		
addition_circuit(0,[],[],[],[]).

create_full_adders(1,[Xn],[Yn],[Zn,Znn],_,[full_adder(Xn,Yn,0,Zn,Znn)]).
create_full_adders(N,[X|Xrest],[Y|Yrest],[Z|Zrest],[CarryOut|Crest],[full_adder(X,Y,0,Z,CarryOut)|RestFullAdders]) :- N > 2, create_full_adders2(Xrest,Yrest,Zrest,[CarryOut|Crest],RestFullAdders).
create_full_adders(2,[X|Xrest],[Y|Yrest],[Z|Zrest],[CarryOut|_],[full_adder(X,Y,0,Z,CarryOut)|RestFullAdders]) :- create_full_adders2(Xrest,Yrest,Zrest,CarryOut,RestFullAdders).

create_full_adders2([NX|Xrest],[NY|Yrest],[NZ|Zrest],[CarryOut,NCarryOut], [full_adder(NX,NY,CarryOut,NZ,NCarryOut)|RestFullAdders]) :- length(Xrest,N), N > 0, create_full_adders2(Xrest,Yrest,Zrest,NCarryOut,RestFullAdders).
create_full_adders2([NX|Xrest],[NY|Yrest],[NZ|Zrest],[CarryOut,NCarryOut|RestCarryOut],[full_adder(NX,NY,CarryOut,NZ,NCarryOut)|RestFullAdders]) :- length(Xrest,N), N > 0, length(RestCarryOut,T), T>0, create_full_adders2(Xrest,Yrest,Zrest,[NCarryOut|RestCarryOut],RestFullAdders).
create_full_adders2([LX],[LY],[NZ,LZ],LastCarryOut,[full_adder(LX,LY,LastCarryOut,NZ,LZ)]).


% Task 2: eval_addition_circuit(Fs) :- with mode eval_addition(+)
% which takes as input the full-adder circuit Fs and evaluates each component in the circuit
eval_addition_circuit([full_adder(X,Y,0,Z,C)|RestFullAdders]) :- Ans is X + Y, Z is Ans mod 2, C is Ans div 2, eval_addition_circuit2(C,RestFullAdders).
eval_addition_circuit([]).
eval_addition_circuit2(OldCarry,[full_adder(X2,Y2,C2,Z2,Z3)|RestFullAdders]) :- C2 is OldCarry, Ans is X2 + Y2, Ans2 is Ans + C2, Z2 is Ans2 mod 2, Z3 is Ans2 div 2, eval_addition_circuit2(Z3,RestFullAdders).
eval_addition_circuit2(_,[]).
% same as eval_addition_circuit2 but without to save and use carry.
special_eval_for_task_3([full_adder(X2,Y2,C2,Z2,Z3)|RestFullAdders]) :- Ans is X2 + Y2, Ans2 is Ans + C2, Z2 is Ans2 mod 2, Z3 is Ans2 div 2, special_eval_for_task_3(RestFullAdders).
special_eval_for_task_3([]).

% compare_Zs :- compare 2 lists - true iff 2 lists are different.
compare_Zs([1|_],[0|_]).
compare_Zs([0|_],[1|_]).
compare_Zs([Same|T1],[Same|T2]) :- length(T1,Length), Length > 0, compare_Zs(T1,T2).

% generate number from ones and zeros  
is_member(0).
is_member(1).
assign([X|Rest]) :- is_member(X), assign(Rest).
assign([]).

% Task 3: not_an_addition_circuit(Xs,Ys,Zs,Fs)
not_an_addition_circuit(Xs,Ys,Zs,Fs) :- length(Xs,N),T is N + 1, create_symbols(T,Zs_new), addition_circuit(N,Xs,Ys,Zs_new,Fs_new), 
									assign(Xs),assign(Ys), special_eval_for_task_3(Fs), eval_addition_circuit(Fs_new), compare_Zs(Zs,Zs_new).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------------------------PART 2----------------------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comparator(X1,X2,X2,X1) :- X2 =< X1.
comparator(X1,X2,X1,X2) :- X1 < X2.
 
% rev_list(X,Y)
%     Y is List so return the reverse of X.
rev_list([],[]).
rev_list([X|Xs],Y) :- rev_list(Xs,Ys) , append(Ys,[X],Y).

  
% Task 4: sorting network(+,-,-,-)
% The predicate based on bubble-sort algorithm O(n^2).
sorting_network(1,[comparator(In,In,Out,Out)],In,Out) :- create_symbols(1,In), create_symbols(1,Out).
sorting_network(Number,Comps,Ins,Outs) :- Number > 1, create_symbols(Number,Ins), create_symbols(Number,Outs), sort_comps_rec(Number,Comps,Ins,Outs). 

sort_comps_rec(2,[comparator(In1,In2,Out1,Out2)],[In1,In2],[Out1,Out2]). 
sort_comps_rec(Number,Comps,Ins,Outs) :- NewNum is Number-1, specific_level(Number,SpeComps,Ins,NewIn), rev_list(NewIn,[Yn|NewOut]), 
							rev_list(NewOut,FinIN), rev_list(Outs,[Yn|OUT]),rev_list(OUT,FinOut), sort_comps_rec(NewNum,MoreComps,FinIN,FinOut), append(SpeComps,MoreComps,Comps).
																				
specific_level(Number, [comparator(A1,A2,B1,B2)|OtherComps], [A1,A2|RestA], [B1|Outs]) :- NewNum is Number-1, specific_level(NewNum,OtherComps,[B2|RestA],Outs).
specific_level(2,[comparator(A1,A2,B1,B2)],[A1,A2],[B1,B2]). 


%%% not support the Depth.....
% Task 5: measure_network(Cs,In,Out,Depth,Size)
measure_network(Cs,_,_,_,Size) :- length(Cs,Size). 

% Task 6: apply_network(Cs,In,Out)
apply_network([comparator(A1,A2,B1,B2)|OtherComps],In,Out) :- comparator(A1,A2,B1,B2), apply_network(OtherComps,In,Out).
apply_network([],_,_). 


% Task 7: apply network(Cs,In,Out)
not_a_sorting_network(CsInput,In,Out) :- length(In,Len),sorting_network(Len,RealCs,In,RealOut), assign(In), apply_network(CsInput,In,Out), 
											apply_network(RealCs,In,RealOut), compare_Zs(RealOut,Out).

										
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------------------------PART 3----------------------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_literal(X) :- var(X).
is_literal(X) :- nonvar(X), X = -Y, var(Y).  

is_clause([]).
is_clause([A|As]) :- is_literal(A), is_clause(As).
 
is_cnf([]).
is_cnf([C|Cs]) :- is_clause(C), is_cnf(Cs).

% Task 8: encode_full_adder(X,Y,Cin,Z,Cout,CNF) which given the Boolean variables 
% X, Y, Cin, Z and Cout, unifies CNF with a formula in conjunctive normal form.
encode_full_adder(X,Y,CarryIn,Z,CarryOut,[
										  [-X,-Y,CarryIn,-Z,-CarryOut],[X,Y,-CarryIn,-Z,-CarryOut],[-X,Y,-CarryIn,-Z,-CarryOut],[X,-Y,-CarryIn,-Z,-CarryOut],
									      [-X,Y, CarryIn,Z , CarryOut],[X,-Y, CarryIn, Z,CarryOut],[-X, -Y,CarryIn,Z, CarryOut],[X,Y,-CarryIn,Z,CarryOut],
										  [-X,Y, -CarryIn,Z, CarryOut],[X,Y,-CarryIn,Z ,-CarryOut],[X, -Y,-CarryIn,Z,-CarryOut],[-X,-Y,-CarryIn,Z,-CarryOut],
										  [X,Y, CarryIn,-Z, -CarryOut],[X,-Y,CarryIn,-Z,-CarryOut],[-X, -Y,-CarryIn,Z,CarryOut],[X,Y,CarryIn,-Z,CarryOut],
										  [-X,Y, CarryIn,-Z, CarryOut],[-X,-Y,CarryIn,-Z,CarryOut],[X,Y, -CarryIn,-Z, CarryOut],[X,-Y,-CarryIn,-Z,CarryOut],
										  [-X,-Y,-CarryIn,-Z,CarryOut],[X,Y ,CarryIn,Z, -CarryOut],[-X, Y,CarryIn,Z, -CarryOut],[-X,-Y,CarryIn,Z,-CarryOut]
										 ]).  

% Task 9: encode_binary_addition(+,-,-,-,-) :- which takes as input a positive number N, and unifies
% the arguments: Xs = [X1, . . . , XN], Ys = [Y1, . . . , YN], Zs = [Z1, . . . , ZN, ZN+1]
encode_binary_addition(N,Xs,Ys,Zs,Cnf) :- N > 0, addition_circuit(N,Xs,Ys,Zs,FullAdders), aux_enc_bin(FullAdders, Cnf). 									
encode_binary_addition(0,_,_,_,_).										

aux_enc_bin([full_adder(X,Y,CarryIn,Z,CarryOut)|OtherFullA], Res) :- encode_full_adder(X,Y,CarryIn,Z,CarryOut,CNF),aux_enc_bin(OtherFullA,NewRes),
																									append(NewRes,CNF,Res).
aux_enc_bin([],[]).

