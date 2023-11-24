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
:- dynamic ancestor/2.
:- dynamic relatives/2.
:- discontiguous infer/3.
:- dynamic assign_gender/2.

parent(X,Y,Z):-
    parent(X,Z),
    parent(Y,Z).

aunt(Y, Child):-
    female(Y),
    dif(Y,Child),
    parent(X, Child),
    sibling(X, Y);
    \+ parent(Y, Child),
    (grandparent(G,Child), (child(Y,G) ; parent(G,Y))).

uncle(Y, Child):-
    male(Y),
    dif(Y,Child),
    parent(X, Child),
    sibling(Y, X);
    \+ parent(Y, Child),
    (grandparent(G,Child), (child(Y,G) ; parent(G,Y))).

mother(X,Y):-
    female(X),
    parent(X,Y);
    child(Y, X).

father(X,Y):-
    male(X),
    parent(X,Y);
    child(Y, X).

grandmother(X,Z):-
    female(X),
    grandparent(X,Z);
    (mother(X,Y), parent(Y,Z)).

grandfather(X,Z):-
    male(X),
    grandparent(X,Z);
    (father(X,Y), parent(Y,Z)).

brother(X,Y):-
    male(X),
    sibling(X,Y) ;
    (parent(Par, X), parent(Par, Y)).

sister(X,Y):-
    female(X),
    sibling(X,Y);
    (parent(Par, X), parent(Par, Y)).

sibling(X,Y):-
    dif(X,Y),
    parent(Z,X),
    parent(Z,Y).

% Concludes that they are related by any means is relatives, lacks
% backtracking through ancestors
% Recursive Check to conclude they are relatives or not
% Direct relationships

ancestor(X,Y):-
    parent(X,Y) ; grandparent(X,Y) ; aunt(X, Y) ; uncle(X,Y).

relatives(X, Y) :-
    direct_relative(X, Y).
    
relatives(X, Y) :-
    direct_relative(X, Z),
    X \= Z,
    Z \= Y,
    relatives(Y, Z).
    
direct_relative(X, Y) :-
    sister(X, Y);sister(Y, X);
    child(X, Y); child(Y, X);
    father(X, Y); father(Y, X);
    mother(X, Y); mother(Y, X);
    brother(X, Y); brother(Y, X);
    aunt(X, Y); aunt(Y, X);
    uncle(X, Y); uncle(Y, X);
    son(X, Y); son(Y, X);
    daughter(X, Y); daughter(Y, X);
    sibling(X, Y); sibling(Y, X);
    grandmother(X, Y); grandmother(Y, X);
    grandfather(X, Y); grandfather(Y, X).

% INFERENCE SECTION %
% Predicate Prototype
% infer(relationship, A, B)
% Cannot be a sibling of oneself
assign_gender(male, X):-
    infer_siblings,
    (\+ male(X) -> (assertz(male(X)) ; true)) ; true.

assign_gender(female, X):-
    infer_siblings,
    (\+ female(X) -> (assertz(female(X)) ; true)) ; true.

infer(sibling, X, Y):-
    sibling(X,Y);
    (child(X, Par), child(Y, Par)) ; (parent(Par, X), parent(Par, Y)),
    \+ sibling(X, Y),           % Check if X and Y are NOT siblings
    \+ sibling(Y, X),           % Check if Y and X are NOT siblings
    X \= Y,                     % A person is not their own sibling condition
    assertz(sibling(X, Y)),
    assertz(sibling(Y, X)).

infer(sister, X, Y):-
    sister(X,Y);
    female(X),
    infer(sibling, X, Y),
    (   \+ sister(X, Y) -> assertz(sister(X, Y)) ; false).    % If they're not yet sisters, assert that they are sisters.

infer(brother, X, Y):-
    brother(X,Y);
    male(X),
    infer(sibling, X, Y),
    (  \+ brother(X, Y)  -> assertz(brother(X, Y)); true).

% You cannot be the aunt of someone who's your ancestor or sibling.
infer(aunt, X, Y):-
    aunt(X,Y);
    dif(X,Y),
    \+ male(X), \+ ancestor(Y,X), \+ sibling(X,Y), \+ child(Y,X),
    (   \+ aunt(X, Y) -> (assertz(aunt(X, Y)) ; assign_gender(female,X)) ; true).

% You cannot be the uncle of someone who's your ancestor or sibling.
infer(uncle, X, Y):-
    uncle(X,Y);
    dif(X,Y),
    \+ female(X), \+ ancestor(Y,X), \+ sibling(Y,X), \+ child(Y,X),
    (  \+ uncle(X, Y) -> (assertz(uncle(X, Y)) ; assign_gender(male,X)) ;true).

infer(father, X, Y):-
    father(X,Y);
    dif(X,Y), (direct_relative(X,Y) -> (father(X,Y) -> true ; false) ; true),
    \+ female(X), \+ ancestor(Y,X), \+ sibling(X,Y), \+ grandparent(X,Y), infer(child, Y, X),
    (\+ parent(X,Y) -> (assertz(parent(X, Y)) ; assign_gender(male,X)) ;true).

infer(mother, X, Y):-
    dif(X,Y), (direct_relative(X,Y) -> (mother(X,Y) -> true ; false) ; true),
    \+ male(X),\+ ancestor(Y,X), \+ sibling(X,Y), \+ grandparent(X,Y), infer(child, Y, X),
    (\+ parent(X,Y) -> (assertz(parent(X, Y)) ; assign_gender(female,X)) ; true).

infer(child, X, Parent):-

    dif(X, Parent),
    % if child is already indicated as child, check if the parent gender is specific.
    (child(X, Parent) -> ((father(Parent,X) ; mother(Parent,X)) -> false ; true) ; true),
    at_most_two_unique_parents(X),
    (\+ child(X,Parent) -> (assertz(child(X, Parent)), assertz(parent(Parent, X))) ;  true).

infer(son, X, Parent):-
    dif(X, Parent),
    \+ female(X), \+ sibling(X,Parent), \+ ancestor(X, Parent), infer(child, X, Parent),
    (\+ son(X, Parent) ->  assign_gender(male,X) ; true).

infer(daughter, X, Parent):-
    dif(X, Parent),
    \+ male(X), \+ sibling(X,Parent), \+ ancestor(X, Parent), infer(child, X, Parent),
    (\+ daughter(X, Parent) ->  assign_gender(female, X) ; true).

infer(grandfather, X, Y):-
    \+ female(X), (direct_relative(X,Y) -> (grandparent(X,Y) -> true)),
    \+ ancestor(Y,X), \+ sibling(X,Y), \+ child(Y,X),
    (\+ grandfather(X, Y) -> (assertz(grandparent(X, Y)) ; assign_gender(male,X)) ; true).

infer(grandmother, X, Y):-
    \+ male(X), (direct_relative(X,Y) -> (grandparent(X,Y) -> true)),
    \+ ancestor(Y,X), \+ sibling(X,Y), \+ child(Y,X),
    (\+ grandmother(X, Y) -> (assertz(grandparent(X, Y)) ; assign_gender(female,X)) ; true).


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

assert_new_sibling_pair((X, Y)):-
    assertz(sibling(X,Y)).

% Given a transitive information, like john is siblings with carrie, and
% carrie is siblings with Janna. This would identfiy that john is
% siblings with Janna
infer_siblings :-
    findall((A,B), sibling(A,B), ThePairs), % parameter ('solution', 'goal'. OutputVariable)
    findall((X, Y),
    (
        member((P1, X), ThePairs), % (Somewhat tuple), finds all people whom are siblings with X
        member((P1, Y), ThePairs), % Finds all people who are siblings with Y
        X \= Y,                    % A person is not their own siblings
        \+ member((X, Y), ThePairs),% If the case that these two are not yet siblings
        \+ member((Y, X), ThePairs) % If the case that these to ware not siblings

     ), NewPairs),                 % Saved that case of not siblings to new pairs
     list_to_set(NewPairs, UniquePair), % Remove duplicate pairings
     maplist(assertz(sibling()), UniquePair). % save them back to the running pl

delete_all :-
    retractall(sister(_, _)),
    retractall(father(_, _)),
    retractall(mother(_, _)),
    retractall(brother(_, _)),
    retractall(grandmother(_, _)),
    retractall(daughter(_, _)),
    retractall(uncle(_, _)),
    retractall(child(_, _)),
    retractall(son(_, _)),
    retractall(aunt(_, _)),
    retractall(grandfather(_, _)),
    retractall(parents(_, _, _)),
    retractall(children(_, _, _, _)),
    retractall(sibling(_, _)),
    retractall(relatives(_, _)).

% will try to implement this soon.
check_children([], _, 'Yes!'). % Base case: All names checked
check_children([Child|Rest], Parent, Response):-
    (   child(Child, Parent) -> check_children(Rest, Parent, Response)
    ;   Response = 'No!', !).