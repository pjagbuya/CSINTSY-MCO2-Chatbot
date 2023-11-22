:- dynamic parent/2.
:- dynamic grandparent/2.
:- dynamic grandfather/2.
:- dynamic grandmother/2.
:- dynamic male/1.
:- dynamic female/1.
:- dynamic father/2.
:- dynamic mother/2.
:- dynamic aunt/2.
:- dynamic uncle/2.
:- dynamic child/2.
:- dynamic son/2.
:- dynamic daughter/2.
:- dynamic daughter/2.
:- dynamic sibling/2.
:- dynamic brother/2.
:- dynamic sister/2.
:- discontiguous infer/3.

mother(X,Y):-
    female(X),
    parent(X,Y).

father(X,Y):-
    male(X),
    parent(X,Y).

grandmother(X,Z):-
    female(X),
    grandparent(X,Z);
    (mother(X,Y), mother(Y,Z)).

grandfather(X,Z):-
    male(X),
    grandparent(X,Z);
    (father(X,Y), father(Y,Z)).

brother(X,Y):-
    male(X),
    sibling(X,Y) ;
    (parent(Par, X), parent(Par, Y)).

sister(X,Y):-
    female(X),
    sibling(X,Y) ;
    (parent(Par, X), parent(Par, Y)).

% INFERENCE SECTION %
% Predicate Prototype
% infer(relationship, A, B)
% Cannot be a sibling of oneself
infer(sibling, X, Y):-
    (child(X, Par), child(Y, Par)) ; (parent(Par, X), parent(Par, Y)),
    \+ sibling(X, Y),           % Check if X and Y are NOT siblings
    \+ sibling(Y, X),           % Check if Y and X are NOT siblings
    X \= Y,                     % A person is not their own sibling condition
    assertz(siblings(X, Y)),
    assertz(siblings(Y, X)).

infer(sister, X, Y):-
    female(X),
    infer(sibling, X, Y),
    (   \+ sister(X, Y) -> assertz(sister(X, Y)) ; false).    % If they're not yet sisters, assert that they are sisters.

infer(brother, X, Y):-
    male(X),
    infer(sibling, X, Y),
    (  \+ brother(X, Y)  -> true; true).

% You cannot be an aunt of someone whom you are of similar or older age
infer(aunt, X, Y):-
      \+ (father(Y, X); mother(Y, X);
         brother(Y, X); sister(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X)),
         dif(X,Y),
    (   \+ aunt(X, Y) -> assertz(aunt(X, Y)) ;true).

% You cannot be an aunt of someone whom you are of similar or older age
infer(uncle, X, Y):-
    \+ (father(Y, X); mother(Y, X);
    brother(Y, X); sister(Y, X);
    uncle(Y, X); grandfather(Y, X);
    grandmother(Y, X)),
    dif(X,Y),
    (  \+ uncle(X, Y) -> assertz(uncle(X, Y)) ;true).

infer(father, X, Y):-
    dif(X,Y),
    (  \+ father(X, Y) -> assertz(father(X, Y)) ;true).

infer(mother, X, Y):-
    dif(X,Y),
    (  \+ mother(X, Y) -> assertz(mother(X, Y)) ;true).

infer(child, X, Parent):-
    dif(X, Parent),
    \+ child(X, Parent),
    at_most_two_unique_parents(X),
    assertz(child(X, Parent)).

infer(son, X, Parent):-
    dif(X, Parent),
    (  \+ son(X, Parent) -> assertz(son(X, Parent)) ;true).

infer(daughter, X, Parent):-
    (   \+ daughter(X, Parent) -> assertz(daughter(X, Parent)) ;true).

infer(grandfather, X, Y):-
    (\+  female(X) -> assertz(male()) ; false),
    (\+ grandfather(X, Y) -> assertz(grandfather(X, Y)) ;true).

infer(grandmother, X, Y):-
    (\+  male(X) -> assertz(female()) ; false),
    (\+ grandmother(X, Y) -> assertz(grandmother(X, Y)) ; true).

infer(children, Name1, Name2, Name3, Parent):-
    (valid_parent_child(Name1, Parent),
     valid_parent_child(Name2, Parent),
     valid_parent_child(Name3, Parent),
     assertz(child(Name1,Parent)),
     assertz(child(Name2,Parent)),
     assertz(child(Name3,Parent))).

infer(parents, P1, P2, Child):-
    add_parents_to_child(P1, P2, Child),

      (\+ clause(parents(P1, P2, Child), true) ->
        assertz(parents(P1, P2, Child))
    ;   true).


% Helper predicate to check if a parent-child assertion is valid
valid_parent_child(Child, Parent) :-
    \+ child(Child, Parent),  % The Parent is not already a parent of the Child
    can_have_additional_parent(Child).  % The Child can have an additional parent

% Check if a child can have an additional parent
can_have_additional_parent(Child) :-
    findall(P, child(Child, P), Parents),
    length(Parents, Count),
    Count < 2.  % There are less than 2 parents


% Helper functions to not allow facts to state more than two parents
at_most_two_unique_parents(Child) :-
    findall(Parent, child(Child, Parent), Parents),
     sort(Parents, UniqueParents),
     length(UniqueParents, Count),
     Count < 2.                       %Used in assert logic


add_parents_to_child(P1, P2, Child):-
    (   \+ at_most_two_unique_parents(Child) -> child(Child, P1), child(Child, P2); false ),
    (    at_most_two_unique_parents(Child), \+ child(Child, P1)
            -> assertz(child(Child, P1)); true
    ),
    (    at_most_two_unique_parents(Child), \+ child(Child, P2)
            -> assertz(child(Child, P1)); true
    ).

%     assertz(sibling(X, Y)), % make sure the running pl remembers in a file
%     assertz(sibling(Y, X)).
%    % save_sibling.           % Save to file, file must already exist

save_all :-
    tell('Prolog/saveFacts.pl'),
    listing(sister/2),
    listing(father/2),
    listing(mother/2),
    listing(brother/2),
    listing(grandmother/2),
    listing(daughter/2),
    listing(uncle/2),
    listing(child/2),
    listing(son/2),
    listing(aunt/2),
    listing(grandfather/2),
    listing(sibling/2),
    told.                   % Close the file