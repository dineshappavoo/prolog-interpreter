
%==========================================================================
%                DCG RIGHT RECURSIVE GRAMMAR FOR REFERENCE
%==========================================================================
%P  ::= K.
%K  ::= begin D; C end
%D  ::= const I = N D' | var I D' %| I := N D'
%D' ::= ε | ; D D'
%C  ::= I := E C' | if B then C else C endif C' | while B do C endwhile C' | K C'
%C' ::= ε | ; C C'
%B  ::= true | false | E = E | not B
%E  ::= I := E E' |I E' | N E'
%E' ::= ε | + E E' | - E E' | * E E' | / E E'
%I  ::= x | y | z | u | v
%N  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9


%==========================================================================
%                    PROGRAM TO GENERATE PARSE TREE
%==========================================================================

program(pprog(K)) --> 	k(K),[.].

k(pblock(D,C)) --> 	[begin],d(D),[;],c(C),[end].

d(D) --> d_d(D).
d(pdeclaration(D1,D2)) --> 	d_d(D1),[;],d(D2).
d_d(pconstant(I,N)) --> [const],i(I),[=],n(N).
d_d(pvar(I)) --> [var],i(I).

c(C) --> 	c_c(C).
c(pcc(C1,C2)) --> 	c_c(C1),[;],c(C2).
c_c(passignie(I,E)) --> 	i(I),[:=],e(E).
c_c(pfullif(B,C1,C2)) --> 	[if],b(B),[then],c(C1),[else],c(C2),[endif].
c_c(pwhile(B,C)) --> [while],b(B),[do],c(C),[endwhile].

e(E) --> e_e(E).
e(passign(I,E)) --> i(I),[:=],e(E).
e(padd(E1,E2)) --> e_e(E1),[+],e(E2).
e(psubtract(E1,E2)) --> e_e(E1),[-],e(E2).
e(pmul(E1,E2)) --> e_e(E1),[*],e(E2).
e(fdiv(E1,E2)) --> e_e(E1),[/],e(E2).

e_e(pi(I)) --> i(I).
e_e(pn(N)) --> n(N).
e_e(pe(E)) --> ['('],e(E),[')'].

b(passignequal(E1,E2)) --> e(E1),[=],e(E2).
b(pnot(B)) --> [not], b(B).
b(pbbrace(B)) --> ['('], b(B),[')'].
b(ptrue(true)) --> [true].
b(pfalse(false)) --> [false].

i(x) --> [x].
i(y) --> [y].
i(z) --> [z].
i(u) --> [u].
i(v) --> [v].
i(v) --> [w].

n(0) --> [0].
n(1) --> [1].
n(2) --> [2].
n(3) --> [3].
n(4) --> [4].
n(5) --> [5].
n(6) --> [6].
n(7) --> [7].
n(8) --> [8].
n(9) --> [9].

%==========================================================================
%                   GRAMMAR TESTING
%==========================================================================
test_case1(P):- program(P,[begin, var, z, ; , var, x, ;, z, :=, x, end, .],[]).
test_case2(P):- program(P,[begin, var, x, ;, var, y, ;, var, z, ;, z, :=, x, +, y, end, .],[]).
test_case3(P):- program(P,[begin, var, x,;, var, y,;, var, z,;, z,:=,'(',z,:=,x,+,2,')',+,y, end,.],[]).
test_case4(P):- program(P,[begin, var, x,;, var, y,;, var, z,;,if, x,=,y ,then, z,:=,1 ,else ,z,:=,0 ,endif, end,.],[]).
test_case5(P):- program(P,[begin, var, x,;, var, y,;, var, z,;, if, x ,= ,0 ,then ,z,:=,x ,else ,z,:=,y ,endif ,end,.],[]).
test_case6(P):- program(P,[begin, var, x, ;, var, y, ;, var, z, ;, if, not, '(', x, =, y, ')', then, z, :=, x, else, z, :=, y, endif, end, .],[]).
test_case7(P):- program(P,[begin, var ,x,;, var, z,;, z,:=,0,;, while, not, x,=,0, do, z, :=, z,+,1,;, x,:=,x,-,1, endwhile, end,.],[]).
test_case8(P):- program(P,[begin, var, x,;, var, y,;, var ,z,;, z,:=,1,;, w,:=,x,;, while ,not ,w, =, 0 ,do, z, :=,z,*,y,;, w,:=,w,-,1, endwhile, end,.],[]).
test(P) :- program(P,[begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, x,+,2, ;, if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, +, 2, endwhile, end, .], []).

%==========================================================================
%                               EVALUATION
%==========================================================================
%newstore([(x,0),(y,2),(z,4),(w,2)]).
newstore([]).


%accessval(I,[(I,V) | T],V).
%accessval(I,[(J,_) | T ], V) :-  accessval(I, T, V).
%access(X,ST,V) :- accessval(X,ST,V).

%updateval(I, NV, [(I, V) | T], [(I, NV) | T]).
%updateval(I, NV, [(J,V) | T], [(J,V)| T1]) :- updateval(I, NV, T, T1).
%update(X,Y,ST, N_ST) :- updateval(X,Y,ST, N_ST).


access(_,[],0).
access(I,[(I,V)|_],V).
access(I,[_|T],V) :- access(I,T,V).

update(I,NV,[],[(I,NV)]).
update(I,NV,[(I,_)|T],[(I,NV)|T]).
update(I,NV,[P|T],[P|T1]) :- update(I,NV,T,T1).


psem(pprog(K), A, B, D) :-              newstore(S),
                                        update(x, A, S, S1),
                                        update(y, B, S1, S2),
                                        ksem(K, S2, S3),
                                        access(z, S3, D).

ksem(pblock(D,C),Sin,Sout) :-           dsem(D,Sin,Sin1),
                                        csem(C,Sin1,Sout).

dsem(pdeclaration(D1,D2),Sin,Sout):- 	dsem(D1,Sin,Sin1),
                                        dsem(D2,Sin1,Sout).

dsem(pconstant(I,N),Sin,Sout):-         update(I,N,Sin,Sout).
                                        dsem(pvar(_),Sin,Sin).

csem(pcc(C1,C2),Sin,Sout):-             csem(C1,Sin,Sin1),
                                        csem(C2,Sin1,Sout).

csem(passignie(I,E),Sin,Sout):-         esem(E,Sin,Sin1,Val),
                                        update(I,Val,Sin1,Sout).

csem(pfullif(B,C1,C2),Sin,Sout):-       bsem(B,Sin,Sin1,RV),(RV -> csem(C1,Sin1,Sout) ; csem(C2,Sin1,Sout)).

csem(pwhile(B,C),Sin,Sout):-            bsem(B,Sin,Sin1,RV),(RV -> csem(C,Sin1,Sin2),csem(pwhile(B,C),Sin2,Sout) ; Sout=Sin1).

esem(pi(I),Sin,Sin,Val):-               access(I,Sin,Val).

esem(pn(N),Sin,Sin,N).

esem(pe(E),Sin,Sout,Value):-            esem(E,Sin,Sout,Value).

esem(passign(I,E),Sin,Sout,Value):-     esem(E,Sin,Sin1,Value),
                                        update(I,Value,Sin1,Sout).

esem(padd(E1,E2),Sin,Sout,Value):-      esem(E1,Sin,Sin1,Value1),
                                        esem(E2,Sin1,Sout,Value2),
                                        Value is Value1 + Value2.

esem(psubtract(E1,E2),Sin,Sout,Value):- esem(E1,Sin,Sin1,Value1),
                                        esem(E2,Sin1,Sout,Value2),
                                        Value is Value1 - Value2.

esem(pmul(E1,E2),Sin,Sout,Value):-      esem(E1,Sin,Sin1,Value1),
                                        esem(E2,Sin1,Sout,Value2),
                                        Value is Value1 * Value2.

esem(pdiv(E1,E2),Sin,Sout,Value):-      esem(E1,Sin,Sin1,Value1),
                                        esem(E2,Sin1,Sout,Value2),
                                        Value is Value1 / Value2.

bsem(passignequal(E1,E2),Sin,Sout,RV):-	esem(E1,Sin,Sin1,Value1),
                                        esem(E2,Sin1,Sout,Value2),
                                        (Value1=Value2 -> RV = true ; RV = false).

bsem(pnot(B),Sin,Sout,RV):-             bsem(B,Sin,Sout,RV1),
                                        (RV1 -> RV = false ; RV = true).
bsem(pbbrace(B),Sin,Sout,RV):-          bsem(B,Sin,Sout,RV).
bsem(ptrue(true),_,_,true).
bsem(pfalse(false),_,_,false).

%==========================================================================
%                       EVALUATION TESTING
%==========================================================================
main_1(ValX, ValY, A) :- program(P,[begin, var, z, ; , var, x, ;, z, :=, x, end, .],[]),write(P),psem(P,ValX,ValY,A).

main_2(ValX, ValY, A) :- program(P,[begin, var, x, ;, var, y, ;, var, z, ;, z, :=, x, +, y, end, .],[]),write(P),psem(P,ValX,ValY,A).

main_3(ValX, ValY, A) :- program(P,[begin, var, x,;, var, y,;, var, z,;, z, :=, '(', z, :=, x, +, 2, ')', +, y, end, .],[]),write(P),psem(P,ValX,ValY,A).

main_4(ValX, ValY, A) :- program(P,[begin, var, x,;, var, y,;, var, z,;,if, x,=,y ,then, z,:=,1 ,else ,z,:=,0 ,endif, end,.],[]),write(P),psem(P,ValX,ValY,A).

main_5(ValX, ValY, A) :- program(P,[begin, var, x,;, var, y,;, var, z,;, if, x ,= ,0 ,then ,z,:=,x ,else ,z,:=,y ,endif ,end,.],[]),write(P),psem(P,ValX,ValY,A).

main_6(ValX, ValY, A) :- program(P,[begin, var, x, ;, var, y, ;, var, z, ;, if, not, '(', x, =, y, ')', then, z, :=, x, else, z, :=, y, endif, end, .],[]),write(P),psem(P,ValX,ValY,A).

main_7(ValX, ValY, A) :- program(P,[begin, var ,x,;, var, z,;, z,:=,0,;, while, not, x,=,0, do, z, :=, z,+,1,;, x,:=,x,-,1, endwhile, end,.],[]),write(P),psem(P,ValX,ValY,A).

main_8(ValX, ValY, A) :- program(P,[begin, var, x,;, var, y,;, var ,z,;, z,:=,1,;, w,:=,x,;, while ,not ,w, =, 0 ,do, z, :=,z,*,y,;, w,:=,w,-,1, endwhile, end,.],[]),write(P),psem(P,ValX,ValY,A).

main_f(ValX, ValY, A) :- program(P, [begin, var, x, ;, var, y, ;, var,z, ;, var, w, ;, z, :=, 1, ;, w, :=, x, ;, while, w, >, 0, do, z, :=,z, *, y, ;, w, :=, w, -, 1, endwhile, end, .],[]), write(P),
    psem(P,ValX,ValY,A).

%==========================================================================
%FOR TESTING
%==========================================================================

%test_p(P) :- program_right_rec(P,[begin, const, x, =, 8, ;, var, y, ;,var, z,;, z, :=, 0, ;,if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif, ;,end], []).

%test_p(P) :- program_right_rec(P,[begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif, ;,end], []).

%test_p(P) :- program_right_rec(P,[begin, const, x, =, 8, ;,if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif,end], []).

%==========================================================================










