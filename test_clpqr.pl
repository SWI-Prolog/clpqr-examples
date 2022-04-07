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

:- module(test_clpqr,
          [ test_clpqr/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(terms)).

test_clpqr :-
    run_tests([ mip,
                mg,
                root
              ]).

:- begin_tests(mip).

:- [mip].

run_example(N, Expect) :-
    example(N, O, _, I, _),
    bb_inf( I, O, Inf, _, 0.001),
    (   abs(Inf-Expect) < 0.001
    ->  true
    ;   print_message(error, clpqr_test_failure(mip, 1, Expect, Inf)),
        fail
    ).

test(1) :- run_example(1, 13).
test(2) :- run_example(2, -42).
test(flugpl) :- run_example(flugpl, 1201500).
test(flugplan) :- run_example(flugplan, 1201500).
test(stein15) :- run_example(stein15, 9).
test(sample2) :- run_example(sample2, 375).
% These tests do not terminate in a reasonable time, also not in SICStus.
%test(noswot) :- run_example(noswot, -43).
%test(bell3a) :- run_example(bell3a, 878430.32).
%test(bell3a_nonred) :- run_example(bell3a_nonred, 878430.32).

:- end_tests(mip).

:- begin_tests(mg).

:- [mg].

test(mg, [nondet]) :-
    mg(P,12,0.01,B,Mp),
    dump([P,B,Mp], [p,b,mp], G),
    assertion(float_eq(G,
                       [b=1.1268250301319698*p-12.682503013196973*mp],
                       0.0001)).

:- end_tests(mg).

:- begin_tests(root).

:- [root].

test(sqrt, L == 886731088897r627013566048) :-
    root(5, L).
test(e, L == 93405312000r34361893981) :-
    e(10, L).

:- end_tests(root).


		 /*******************************
		 *            HELPERS		*
		 *******************************/

float_eq(T1, T2, D) :-
    mapsubterms(float_eq_(D), T1, T2).

float_eq_(_, T1, T2) :-
    T1 =@= T2,
    !.
float_eq_(D, F1, F2) :-
    float(F1),
    float(F2),
    abs(F1-F2) < D.

:- multifile prolog:message//1.

prolog:message(clpqr_test_failure(Set,Test,Expect,Got)) -->
    [ 'clp(QR) test failure.  Test set ~p, test ~p, Expected ~p, Got ~p'-
      [Set,Test,Expect,Got]
    ].


