:- dynamic sister/2.

sister(X, Y) :-
    (   female(X),
        sibling(X, Y)
    ;   parent(Par, X),
        parent(Par, Y)
    ).

:- dynamic father/2.

father(X, Y) :-
    male(X),
    parent(X, Y).

:- dynamic mother/2.

mother(X, Y) :-
    female(X),
    parent(X, Y).

:- dynamic brother/2.

brother(X, Y) :-
    (   male(X),
        sibling(X, Y)
    ;   parent(Par, X),
        parent(Par, Y)
    ).

:- dynamic grandmother/2.

grandmother(X, Z) :-
    female(X),
    mother(X, Y),
    parent(Y, Z).

:- dynamic daughter/2.


:- dynamic uncle/2.


:- dynamic child/2.

child(alex, mom).
child(bro, mom).
child(tsu, mom).

:- dynamic son/2.


:- dynamic aunt/2.

aunt(X, Y) :-
    female(X),
    sister(X, Z),
    parent(Z, Y).

:- dynamic grandfather/2.

grandfather(X, Z) :-
    male(X),
    father(X, Y),
    parent(Y, Z).

:- dynamic sibling/2.


