/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_clpq,
          [ test_clpq/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(terms)).

test_clpq :-
    run_tests([ root,
                squares,
                simplex,
                eliminat
              ]).

:- begin_tests(root).

:- [root].

test(sqrt, L == 886731088897r627013566048) :-
    root(5, L).
test(e, L == 93405312000r34361893981) :-
    e(10, L).

:- end_tests(root).

:- begin_tests(squares).

:- [squares].

test(9, A-L == 33r32-[15r32, 9r16, 1r4, 7r32, 1r8, 7r16, 1r32, 5r16, 9r32]) :-
    length(L, 9),
    filled_rectangle(A,L),
    !.

:- end_tests(squares).

:- begin_tests(simplex).

:- [simplex].

term_expansion(ex(N,L,Z),
               ( test(N, Li-Zi =@= L-Z) :-
                     example(N, Li0, Zi),
                     copy_term_nat(Li0, Li))).

ex(1, [40,160], 17200).
ex(2, [1,0,3r2], 7).
ex(3, [38,170], 18850).
ex(4, [7,2], 310).
ex(5, [0,0,5r2], 5).
ex(6, [1,0,3r2], -7).
ex(7, [3,0,0,4,2,0,5], -70).
ex(8, [0,7r2,0,0,0,1r2,0], -39r2).
ex(9, [555967r58752,203455r39168,43955r3264,1175r272,705r68,
       0,60,3728575r58752,305075r4896,28375r408,2275r34,
       1200r17,0,0,0,0,0,24000r17],
   11429082625r9792).
ex(utility, [0,3400r21,200r7,1000r7,2000r3,0,0,0,1000,0,13400r7,
             13500r7,0,13900r7,4000,8350r21,45800r21,0,0,1000,500,
             2000,100,5000,250,600,800r7,0],
   977650r21).
% TBD: verify the residual constraints
ex(electricity, [0,3000r7,200r7,_,_,0,0,0,_,_,_,_,_,_,1000,500,2000,100],
   109400r7).
ex(water, [0,0,0,1000,0,0,50,_,1000r3,_,4000,0,7450r3,0,0,5000,250,600,_,0],
   23450).

:- end_tests(simplex).

:- begin_tests(eliminat).

:- [eliminat].

% TBD: these results have not verified to be correct.  They merely
% test consistent behavior of clp(Q).
test(1, L == [ x3-1r6*x4=<1r3,
               x3-13r18*x4>= -2r9,
               x3-8r9*x4=<1r9,
               x3-11r9*x4=<1r9,
               x3-1r3*x4>= -1r3
             ]) :-
    example(1, [_,_,X3,X4]),
    dump([X3,X4], [x3,x4], L).
test(2, C == [ x0-73r93*x1+91r93*x2-1r93*x3+23r93*x4 =< -29r31,
               x0+9r43*x1-14r43*x2-27r43*x3-40r43*x4 >= -39r43,
               x0+44r81*x1-19r81*x2+22r81*x3+73r81*x4>=17r81,
               x0-29r25*x1+1r50*x2-57r50*x3-2r5*x4 >= -3r25,
               x0-23r49*x1-31r49*x2-76r49*x3+27r49*x4=<3r49,
               x0+7r39*x1+62r39*x2+18r13*x3-2r3*x4 =< -9r13,
               x0+2r19*x1-64r95*x2-4r5*x3+24r95*x4 >= -33r95,
               x0+2r3*x1-38r45*x2-53r90*x3+4r15*x4 =< -34r45,
               x0-2r17*x1-35r68*x2-x3-35r68*x4>=5r4
             ]) :-
    example(2, L), dump(L, [x0, x1, x2, x3,x4], C).
test(3, C == [ x1+4r45*x2-2r5*x3+13r45*x4=<4r45,
               x1+1r18*x2-1r2*x3+7r18*x4>=1r18,
               x1+1r18*x2-1r2*x3+11r36*x4>=1r18,
               x2>=0,
               x1>0,
               x1+1r12*x2-1r4*x3+1r12*x4=<1r12
             ]) :-
    example(3, L), dump(L, [x1, x2, x3,x4], C).
% Note that we must check semantical equivalence instead of syntactical.
test(hull, L == [x+y>=2, y>=0, x>=1, x+1r2*y=<3, y=<2]) :-
    conv_hull([ [1,1], [2,0], [3,0], [1,2], [2,2] ], [X,Y]),
    dump([X,Y],[x,y],L).

:- end_tests(eliminat).
