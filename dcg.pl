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