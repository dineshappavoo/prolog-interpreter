
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









