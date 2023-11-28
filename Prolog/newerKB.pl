% TABLING FOR RECURSIVE RELATIONSHIPS
:- table sibling/2 as incremental.
:- table relatives/2 as incremental.
:- table predecessor/2 as incremental.
:- table successor/2 as incremental.

% GENERIC PREDICATES
:- dynamic parent/2.
:- dynamic grandparent/2.
:- dynamic child/2.
:- dynamic male/1.
:- dynamic female/1.

% SPECIFIC PREDICATES
:- dynamic grandfather/2.
:- dynamic grandmother/2.
:- dynamic father/2.
:- dynamic mother/2.
:- dynamic aunt/2.
:- dynamic uncle/2.
:- dynamic son/2.
:- dynamic daughter/2.
:- dynamic([sibling/2], [incremental(true)]).
:- dynamic brother/2.
:- dynamic sister/2.
:- dynamic([predecessor/2], [incremental(true)]).
:- dynamic([successor/2], [incremental(true)]).
:- dynamic direct_predecessor/2.
:- dynamic([relatives/2], [incremental(true)]).
% :- dynamic assign_gender/2.
:- dynamic spouse/2.
:- discontiguous infer/3.

% BASIC RELATIONSHIPS
parent(X,Y,Z):-
    parent(X,Z),
    parent(Y,Z).

aunt(X, Child):-
    dif(X,Child),
    female(X),
    parent(Y, Child),
    dif(X, Y),
    sibling(Y, X).

uncle(X, Child):-
    dif(X, Child),
    male(X),
    parent(Y, Child),
    dif(X, Y),
    sibling(Y, X).

mother(X,Y):-
    female(X),
    (parent(X,Y); child(Y, X)).

father(X,Y):-
    male(X),
    (parent(X,Y); child(Y, X)).

grandmother(X,Z):-
    female(X), grandparent(X,Z).

grandmother(X,Z):-
    mother(X,Y), parent(Y,Z).

grandfather(X,Z):-
    male(X), grandparent(X,Z).

grandfather(X,Z):-
    father(X,Y), parent(Y,Z).

brother(X,Y):-
    male(X), sibling(X,Y).

sister(X,Y):-
    female(X), sibling(X,Y).

sibling(X,Y):-
    dif(X,Y),
    parent(Z,X),
    parent(Z,Y).

sibling(X,Z):-
    sibling(X,Y),
    sibling(Y,Z),
    dif(X,Z).

son(X,Y):-
    dif(X,Y),
    male(X),
    child(X,Y).

daughter(X,Y):-
    dif(X,Y),
    female(X),
    child(X,Y).

parents([],_).
parents([X|Y], Z):-
    parent(X,Z),
    parent(Y,Z).

spouse(X,Y):-
    child(Z,Y),
    child(Z,X).

% END OF BASIC RELATIONSHIPS


% START COMPLEX RELATIONSHIPS
direct_succesor(X,Y):-
    dif(X,Y),
    (
        child(X,Y);
        (sibling(Z,Y), child(X, Z))
    ).

successor(X,Y):-
    direct_succesor(X,Y).

successor(X,Y):-
    direct_succesor(Z, X),
    successor(Y, Z).

% Direct predecessor
direct_predecessor(X,Y):-
    dif(X,Y),
    (   parent(X,Y);
        grandparent(X,Y);
        aunt(X,Y);
        uncle(X,Y)
    ).

predecessor(X,Y):-
    direct_predecessor(X,Y).

% Predecessor by inference
% If X has a predecessor Z who is a relative of Y, then Y is a predecessor of X.
% If X has a predecessor Z who's predecessor is Y, then X and Y are predecessors.
predecessor(X,Y):-
    direct_predecessor(X,Z),
    predecessor(Z,Y).

% Two people are relatives if they are siblings or have a common predecessor.
relatives(X,Y):-
    sibling(X,Y).

relatives(X,Y):-
    predecessor(X,Y);
    predecessor(Y,X).

relatives(X,Y):-
    predecessor(Z,X),
    predecessor(Z,Y).

children([],_).

children([X|TAIL],Y):-
    child(X,Y),
    children(TAIL, Y).

% END OF COMPLEX RELATIONSHIPS

% START OF INFERENCE LOGIC

infer(sibling, X, Y):-
    dif(X,Y),                       % A person is not their own sibling condition
    \+ spouse(X,Y),                 % Incest problem
    \+ (spouse(X,Z), sibling(Y,Z)), % You can't be siblings with both parents.
    \+ successor(Y,X),              % You can't be siblings with your sibling's child.
    \+ relatives(X,Y),
    asserta(sibling(X, Y)),
    asserta(sibling(Y, X)).

% Already siblings, gender is just not specified.
infer(sister, X, Y):-
    \+ male(X), sibling(X,Y), asserta(female(X)).

infer(sister, X, Y):-
    \+ male(X),
    infer(sibling, X, Y),
    assign_gender(female, X).

% Already siblings, gender is just not specified.
infer(brother, X, Y):-
    \+ female(X), sibling(X,Y), asserta(male(X)).

infer(brother, X, Y):-
    \+ female(X),
    infer(sibling, X, Y),
    assign_gender(male, X).

% You cannot be the aunt of someone who's your ancestor or sibling.
infer(aunt, X, Y):-
    dif(X,Y),
    \+ male(X),
    is_valid_ancestor(X, Y),
    assign_gender(female, X),
    asserta(aunt(X,Y)).

% You cannot be the aunt of someone who's your ancestor or sibling.
infer(uncle, X, Y):-
    dif(X,Y),
    \+ female(X),
    is_valid_ancestor(X, Y),
    assign_gender(male, X),
    asserta(uncle(X,Y)).

infer(parent, X, Y):-
    dif(X,Y),
    can_have_additional_parent(Y),
    is_valid_ancestor(X,Y),
    asserta(parent(X,Y)),
    asserta(child(Y,X)).

% If X is already the parent of Y, then just add gender.
infer(father, X, Y):-
    \+ female(X), parent(X,Y), asserta(male(X)).

% If they're not related, then check if it's possible.
infer(father, X, Y):-
    \+ female(X),
    \+ father(_, Y),  % Child should have no father yet.
    infer(parent, X, Y),
    assign_gender(male, X).

% If X is already the parent of Y, then just add gender.
infer(mother, X, Y):-
    \+ male(X), parent(X,Y), asserta(female(X)).

% If they're not related, then check if it's possible.
infer(mother, X, Y):-
    \+ male(X),
    \+ mother(_, Y),  % Child should have no mother yet.
    infer(parent, X, Y),
    assign_gender(female, X).

infer(child, Child, Parent):-
    dif(Child, Parent),
    can_have_additional_parent(Child),
    is_valid_ancestor(Parent, Child),
    asserta(parent(Parent, Child)),
    asserta(child(Child, Parent)).

% If child is just missing gender, add it.
infer(son, X, Parent):-
    \+ female(X), child(X, Parent), asserta(male(X)).

infer(son, X, Parent):-
    \+ female(X), infer(child, X, Parent),
    assign_gender(male, X).

% If child is just missing gender, add it.
infer(daughter, X, Parent):-
    \+ male(X), child(X, Parent), asserta(female(X)).

infer(daughter, X, Parent):-
    \+ male(X), infer(child, X, Parent),
    assign_gender(female, X).

infer(grandparent, X, Y):-
    dif(X, Y),
    is_valid_ancestor(X, Y),
    \+ child(Y, X),     % Your own child cannot be your grandchild.
    asserta(grandparent(X,Y)).

% If X is already the grandparent of Y, then just add gender.
infer(grandfather, X, Y):-
    \+ female(X), grandparent(X, Y), asserta(male(X)).

infer(grandfather, X, Y):-
    \+ female(X),
    infer(grandparent, X, Y),
    assign_gender(male, X).

% If X is already the grandparent of Y, then just add gender.
infer(grandmother, X, Y):-
    \+ male(X), grandparent(X, Y), asserta(female(X)).

infer(grandmother, X, Y):-
    \+ male(X),
    infer(grandparent, X, Y),
    assign_gender(female, X).

infer(parents, [], _).

infer(parents, [P1, P2|_], Child):-
    does_not_have_parents(child),
    \+ sibling(P1, P2),
    infer(parent, P1, Child),
    infer(parent, P2, Child).

infer(children, [], _).

infer(children, [Child|List], Parent):-
    (child(Child, Parent) ; infer(child, Child, Parent)),
    !, infer(children, List, Parent).

% END OF INFERENCE LOGIC

% HELPER FUNCTIONS SECTION

assign_gender(male, X):-
    (\+ male(X) -> (assertz(male(X)) ; true)) ; true.

assign_gender(female, X):-
    (\+ female(X) -> (assertz(female(X)) ; true)) ; true.

can_have_additional_parent(Child) :-
    findall(P, child(Child, P), Parents),
    length(Parents, Count),
    Count < 2.  % There are less than 2 parents

does_not_have_parents(Child) :-
    findall(P, child(Child, P), Parents),
    length(Parents, Count),
    Count < 1.  % There are no parents.

reset_tables:-
    abolish_all_tables.
    table sibling/2.
    table predecessor/2.
    table relatives/2.

is_valid_ancestor(X, Y):-
    \+ predecessor(X, Y),
    \+ predecessor(Y, X),
    \+ sibling(X, Y).