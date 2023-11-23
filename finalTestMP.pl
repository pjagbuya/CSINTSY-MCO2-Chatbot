% ->pip install git+https://github.com/yuce/pyswip@master#egg=pyswip
% Declare your dynamically changing predicates
% ALWAYS Assign Response with 'This Type of string', because "This is
% considered byte string"
% Underscores are considere Anon Variables, meaning you dont care
% about their value and just there for syntax or list values that align
% with the criteria
% relatives is part of the requirement and not implemented yet

:- dynamic sister/2.
:- dynamic father/2.
:- dynamic mother/2.
:- dynamic sister/2.
:- dynamic brother/2.
:- dynamic grandmother/2.
:- dynamic daughter/2.
:- dynamic uncle/2.
:- dynamic child/2.
:- dynamic son/2.
:- dynamic aunt/2.
:- dynamic grandfather/2.
:- dynamic parents/3.
:- dynamic children/4.
:- dynamic siblings/2.
:- dynamic relatives/2.
:- dynamic parent/2.
:- dynamic male/1.
:- dynamic female/1.

% GENERALIST SECTION
% Already handles the type of sentence it is giving
assess_input(Sentence, Response):-
    string_chars(Sentence, Chars),
    last(Chars, LastChar),
    handle_type(LastChar, Sentence, Response).
handle_type('.', Sentence, Response):-
    remove_last_element(Sentence, NewSent),
    fact_case(NewSent, Response).
handle_type('?', Sentence, Response):-
    remove_last_element(Sentence, NewSent),
    query_case(NewSent, Response).
%handle_type(_, Sentence, Response):-
%    Sentence,
%    Response = 'I Can\'t guess if you are stating a fact or asking,
%    please appen a period or question mark Appropriately'.

% Removed the period or question mark after assessing what type it is
remove_last_element(Sentence, SentenceWithOutLast) :-
    string_chars(Sentence, Chars),
    reverse(Chars, [_Last|ReversedWithoutLast]),
    reverse(ReversedWithoutLast, ListWithoutLast),
    string_chars(SentenceWithOutLast, ListWithoutLast).

% removes thet last name
remove_last(List, ResultList) :-
    reverse(List, [_|ReversedTrimmed]),
    reverse(ReversedTrimmed, ResultList).

find_relation([Relation|_], Relation) :-
    map_predicate(Relation, _). % Checks if that relation exists

% Recursive calling of seeing if the predicate mapping exists
find_relation([_|Remaining], Relation) :-
    find_relation(Remaining, Relation). % Recursive calling to find the identical mapping to the predicates below


% Gets the name and relationship
extract_names_and_relation([FirstName|Rest], FirstName, LastName, Relation):-
    last(Rest, LastName),         % predicate of prolog that gives the last element in a variable name LastName
    remove_last(Rest, NoLastRest), % From the given remaining words of the sentence
    find_relation(NoLastRest, Relation). % finds the relation predicate to use

% Gets the one name and relationship
extract_names_and_relation([_|Rest], LastName, Relation):-
    last(Rest, LastName),         % predicate of prolog that gives the last element in a variable name LastName
    remove_last(Rest, NoLastRest), % From the given remaining words of the sentence
    find_relation(NoLastRest, Relation). % finds the relation predicate to use




% MAPPING SECTION

% Predicate mapping, modify some predicates if you feel like there is a
% specific case that can be mapped there
map_predicate("father", father).
map_predicate("mother", mother).
map_predicate("sister", sister).
map_predicate("brother", brother).
map_predicate("grandmother", grandmother).
map_predicate("daughter", daughter).
map_predicate("uncle", uncle).
map_predicate("child", child).
map_predicate("son", son).
map_predicate("aunt", aunt).
map_predicate("grandfather", grandfather).
map_predicate("parents", parents).
map_predicate("children", children).
map_predicate("siblings", siblings).

% Plural form for who questions
map_predicate("fathers", father).
map_predicate("mothers", mother).
map_predicate("sisters", sister).
map_predicate("brothers", brother).
map_predicate("grandmothers", grandmother).
map_predicate("daughters", daughter).
map_predicate("uncles", uncle).
map_predicate("children", child).
map_predicate("sons", son).
map_predicate("aunts", aunt).
map_predicate("grandfathers", grandfather).



% FACT CASES
%
% string_lower(input, output) - example functions for string
% manipulation
fact_case(Sentence, Response) :-
    string_lower(Sentence, LowerCaseSentence),
    split_string(LowerCaseSentence, " ,.", " ", Words), % Second parameter of space comma period ensures the substrings and passed words wont have punctuation
    (   (process_common_case(Words);  % Find the common sentence structure
         match_case(Words) ) -> Response = 'OK I learned something.';
    Response = 'That\'s impossible!').         % Find the common word

assert_fact(Predicate, FirstName, LastName) :-
    NewFact =.. [Predicate, FirstName, LastName],
    \+ clause(NewFact, true),  % Check if the fact is not already in the database
    assertz(NewFact).

% helper func for the process_common_case
construct_and_assert(Predicate, FirstName, LastName) :-
    Fact =.. [Predicate, FirstName, LastName],  % automatically create example: father(X, Y) into the database, no saving yet
    (  \+ clause(Fact, true) -> infer_logic(Predicate, FirstName, LastName); fail).  % If the fact





% CASES SECTION
% For all sentences that follow this pattern, modify respective cases on
% what way of thinking they would have to do given the information and
% predicate
process_common_case(Words) :-
    extract_names_and_relation(Words, FirstName, LastName, Relation),
    map_predicate(Relation, Predicate),
    construct_and_assert(Predicate, FirstName, LastName).


% _ and _ are siblings (Unfinished)
% Put down the following and see let PL identify which predicate aligns
match_case([Name1, "and", Name2, "are", "siblings"]):-
    learn_siblings(Name1, Name2).
    % infer_siblings will not be used anymore since it is possible to have mixed_siblings



% _ and _ are the parents of _ (Unfinished)
match_case([Name1, "and", Name2, "are", "the", "parents", "of", Name3]):-
    infer_logic(parents, Name1, Name2, Name3).


% _, _ and _ are children of _ (Unfinished)
match_case([Name1, Name2, "and", Name3, "are", "children", "of", Name4]):-
    infer_logic(children, Name1, Name2, Name3, Name4).




% QUESTION CASES
% Identifies a questtion sample question case
query_case(Sentence, Response) :-
    string_lower(Sentence, LowerCaseSentence),
    split_string(LowerCaseSentence, " .,", " ", Words),

    (   is_start_who(Words) -> process_common_who(Words, Response);
        process_common_query(Words, Response);
        match_case(Words, Response)).

process_common_query([_|Words], Response) :-
    extract_names_and_relation(Words, FirstName, LastName, Relation),
    map_predicate(Relation, Predicate),
    Fact =.. [Predicate, FirstName, LastName],
    (   (call(Fact) ; clause(Fact, true) ) -> Response = 'Yes!',  assert_fact(Predicate, FirstName, LastName)
        ; (\+ call(Fact)) -> Response = 'No!'
    ).
process_common_who(Words, Response):-
    extract_names_and_relation(Words, Name, Relation),
    map_predicate(Relation, Predicate),
    ( Relation = "children" -> map_predicate("child", Predicate); true),
    findall(Member, call(Predicate, Member, Name), Members),
    string_concat('Sorry, I don\'t recall them having ', Relation, Result),
   (   Members = [] -> Response = Result; Response = Members).



is_start_who([FirstWord|_]):-
    FirstWord = "who".

% Are _ and _ siblings? (Unfinished)
% Put down the following and see let PL identify which predicate aligns
match_case([ "are", Name1, "and", Name2, "siblings"], Response):-
    (   siblings(Name1, Name2) -> Response = 'Yes!';
        \+ siblings(Name1, Name2) -> Response = 'No!').
    % infer_siblings will not be used anymore since it is possible to have mixed_siblings


% Are  _ and _ parents of _ ?(Unfinished)
match_case(["are", Name1, "and", Name2, "parents", "of", Name3], Response):-
    (   parents(Name1, Name2, Name3) -> Response = 'Yes!';
        \+ parents(Name1, Name2, Name3) -> Response = 'No!').


% Are _, _, and _ children of _ ?(Unfinished)
match_case(["are", Name1, Name2, "and", Name3, "children", "of", Name4], Response):-
    (   children(Name1, Name2, Name3, Name4) -> Response = 'Yes!';
        \+ children(Name1, Name2, Name3, Name4) -> Response = 'No!').



% Are _ and _ relatives?(Unfinished)
match_case(["are", Name1, "and", Name2, "relatives"], Response):-
    (   relatives(Name1, Name2) -> Response = 'Yes!';
     \+ relatives(Name1, Name2) -> Response = 'No!').

match_case(Query, Response):-
    append(["are"|Children], ["children", "of", Parent], Query),
    check_children(Children, Parent, Response).

check_children([], _, 'Yes!'). % Base case: All names checked
check_children([Child|Rest], Parent, Response):-
    (   child(Child, Parent) -> check_children(Rest, Parent, Response)
    ;   Response = 'No!', !).


% QUERY LOGIC SECTION FOR INFERENCE LOGIC
% commas(',') are 'and'
% semicolons(';') are 'or'
% These predicates automatically asserts IF (:-) the following
% predicates are also true
% Read this as, if X is sister of Y, and Child
% is child of Y, X is aunt of Child
% logic below follow format, no need to assert, inference logic of bot
% should just understand based on your below recommendations




% Concludes he/she is the auntiee
aunt(Y, Child) :-
    female(Y),
    Y \= Child,
    parent(X, Child),
    siblings(X, Y).

uncle(Y, Child) :-
    male(Y),
    Y \= Child,
    brother(Y, X),
    parent(X, Child),
    siblings(X, Y).

child(X, Parent):-
    X \= Parent,
    (   son(X, Parent);
    daughter(X, Parent) ).

% Logic for son
son(X, Parent):-
    X \= Parent,
    ( male(X)
    , parent(Parent, X)).

%Logic for Daughter

daughter(X, Parent):-
    X \= Parent,
    ( female(X), parent(Parent, X)) .

% Logic for Father
father(Parent, Child):-
    Parent \= Child,
   ( male(Parent) ),
   parent(Parent, Child).

%Logic for Mother
mother(Parent, Child):-
    Parent \= Child,
    female(Parent),
    parent(Parent, Child).

% Logic for Grandfather
grandfather(X, Grandchild):-
    X \= Grandchild,
    male(X),
    father(X, Y),
    parent(Y, Grandchild).

%Logic for Grandmother
grandmother(X, Grandchild):-
    X \= Grandchild,
    female(X),
    mother(X, Y),
    parent(Y, Grandchild).


% Concludes they are siblings
siblings(X, Y):-
    X \= Y,
    (sister(X, Y); sister(Y, X);
    brother(X, Y); brother(Y, X));
    (   parent(Parent, X), parent(Parent, Y)).


% Concludes Child is a child of P1 and P2
parents(P1, P2, Child):-
    P1 \= P2,
    P2 \= Child,
    P1 \= Child,
    (   child(Child, P1),
        child(Child, P2));
    (  parent(P1, Child),
       parent(P2, Child)).

children(Name1, Name2, Name3, Parent) :-
    ( child(Name1, Parent),child(Name2, Parent), child(Name3, Parent) );
    (   parent(Parent, Name1), parent(Parent, Name2), parent(Parent, Name3)).


% Concludes that they are related by any means is relatives, lacks
% backtracking through ancestors
% Recursive Check to conclude they are relatives or not
% Direct relationships
relatives(X, Y) :-
    direct_relative(X, Y).
relatives(X, Y) :-
    direct_relative(X, Z),
    Z \= Y,
    relatives(Z, Y).
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
    siblings(X, Y); siblings(Y, X);
    grandmother(X, Y); grandmother(Y, X);
    grandfather(X, Y); grandfather(Y, X).



% FACT INFERENCE AND ASSERTION LOGIC SECTION
% When asserting facts, it should be able to deduce which facts are
% contradictory
% INference logic for SISTER, assumes that x is a sister
% of y and vice versa they are considered siblings half sibling or not
% is not accounted Age between each other is not accounted



% Cannot be a sibling of oneself
infer_logic(siblings, X, Y):-
    learn_siblings(X, Y).


infer_logic(sister, X, Y):-
    \+ male(X),
     learn_siblings(X, Y),
     \+ (father(Y, X); mother(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X)),
    \+ (father(X, Y); mother(X, Y);
         uncle(X, Y); grandfather(X, Y);
         grandmother(X, Y)),
     (   \+ sister(X, Y) -> assertz(sister(X, Y)); true),
     (   \+ female(X) -> assertz(female(X)); true).



% You cannot be an aunt of someone whom you are of similar or older age
infer_logic(aunt, X, Y):-
      \+ male(X),
      \+ (father(Y, X); mother(Y, X);
         brother(Y, X); sister(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X);parent(Y, X)),
        \+ (father(X, Y); mother(X, Y);
         brother(X, Y); sister(X, Y);
         uncle(X, Y); grandfather(X, Y);
         grandmother(X, Y);parent(X, Y)),

         X \= Y,
    (   \+ aunt(X, Y) -> assertz(aunt(X, Y)) ;true),
     (   \+ female(X) -> assertz(female(X)); true).



% You cannot be an aunt of someone whom you are of similar or older age
infer_logic(uncle, X, Y):-
      X \= Y,
      \+ parent(X, Y),
     \+ female(X),
     \+ siblings(X, Y),
     \+ uncle(X, Y),  % Ensure X is not already an uncle of Y
     \+ aunt(X, Y),
     \+ parent(Y, X),
     \+ siblings(Y, X),
     \+ uncle(Y, X),  % Ensure X is not already an uncle of Y
     \+ aunt(Y, X),

     assertz(uncle(X, Y)),
     assertz(male(X)).


infer_logic(brother, X, Y):-
    \+ female(X),
    learn_siblings(X, Y),
    \+ (father(Y, X); mother(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X)),
    \+ (father(X, Y); mother(X, Y);
         uncle(X, Y); grandfather(X, Y);
         grandmother(X, Y)),

    (  \+ brother(X, Y)  -> assertz(brother(X, Y)); true),
     (   \+ male(X) -> assertz(male(X)); true).



infer_logic(father, X, Y):-
     X \= Y,
     \+ female(X),
     \+ (father(Y, X); mother(Y, X);
         siblings(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X)),
     \+ (father(X, Y); mother(X, Y);
         siblings(X, Y);
         uncle(X, Y); grandfather(X, Y);
         grandmother(X, Y)),

    (  \+ father(X, Y) -> assertz(father(X, Y)) ;true),
    (  \+ parent(X, Y) -> assertz(parent(X, Y)) ;true),

     (   \+ male(X) -> assertz(male(X)); true).

infer_logic(mother, X, Y):-
     X \= Y,
     \+ male(X),
     \+ (father(Y, X); mother(Y, X);
         siblings(Y, X);
         uncle(Y, X); grandfather(Y, X);
         grandmother(Y, X)),
      \+ (father(X, Y); mother(X, Y);
         siblings(X, Y);
         uncle(X, Y); grandfather(X, Y);
         grandmother(Y, X)),
    (  \+ mother(X, Y) -> assertz(mother(X, Y)) ;true),
    (  \+ parent(X, Y) -> assertz(parent(X, Y)) ;true),
     (   \+ female(X) -> assertz(female(X)); true).


infer_logic(child, X, Parent):-
     X \= Parent,
    \+ grandfather(Parent, X),
    \+ grandmother(Parent, X),
    \+ siblings(Parent, X),
    \+ uncle(Parent, X),
    \+ aunt(Parent, X),
    \+ child(Parent, X),
    \+ grandfather(X, Parent),
    \+ grandmother(X, Parent),
    \+ siblings(X, Parent),
    \+ uncle(X, Parent),
    \+ aunt(X), Parent,

    at_most_two_unique_parents(X),
    assertz(child(X, Parent)),
    assertz(parent(Parent, X)).


infer_logic(son, X, Parent):-
     X \= Parent,
     \+ female(X),
    \+ grandfather(Parent, X),
    \+ grandmother(Parent, X),
    \+ siblings(Parent, X),
    \+ uncle(Parent, X),
    \+ aunt(Parent, X),
    \+ child(Parent, X),
    \+ grandfather(X, Parent),
    \+ grandmother(X, Parent),
    \+ siblings(X, Parent),
    \+ uncle( X, Parent),
    \+ aunt(X, Parent),

    (  \+ son(X, Parent) -> assertz(son(X, Parent)) ;true),
     (   \+ male(X) -> assertz(male(X)); true).

infer_logic(daughter, X, Parent):-
     X \= Parent,
    \+ male(X),
    \+ grandfather(Parent, X),
    \+ grandmother(Parent, X),
    \+ siblings(Parent, X),
    \+ uncle(Parent, X),
    \+ aunt(Parent, X),
    \+ child(Parent, X),
    \+ grandfather(X, Parent),
    \+ grandmother(X, Parent),
    \+ siblings(X, Parent),
    \+ uncle(X, Parent),
    \+ aunt(X, Parent),

    (   \+ daughter(X, Parent) -> assertz(daughter(X, Parent)) ;true),
     (   \+ female(X) -> assertz(female(X)); true).


infer_logic(grandfather, X, Y):-
    X \= Y,
    \+ female(X),
    \+ father(Y, X),
    \+ mother(Y, X),
    \+ siblings(Y, X),
    \+ uncle(Y, X),
    \+ aunt(Y, X),
    \+ child(Y, X),
    \+ father(X, Y),
    \+ mother(X, Y),
    \+ siblings(X, Y),
    \+ uncle(X, Y),
    \+ aunt(X, Y),
    \+ child(X, Y),

    (   \+ grandfather(X, Y) -> assertz(grandfather(X, Y)) ;true),
     (   \+ male(X) -> assertz(male(X)); true).

infer_logic(grandmother, X, Y):-
    X \= Y,
    \+ male(X),
    \+ father(Y, X),
    \+ mother(Y, X),
    \+ siblings(Y, X),
    \+ uncle(Y, X),
    \+ aunt(Y, X),
    \+ child(Y, X),

    \+ father(X, Y),
    \+ mother(X, Y),
    \+ siblings(X, Y),
    \+ uncle(X, Y),
    \+ aunt(X, Y),
    \+ child(X, Y),

    (   \+ grandmother(X, Y) -> assertz(grandmother(X, Y)) ;true),
     (   \+ female(X) -> assertz(female(X)); true).






infer_logic(children, Name1, Name2, Name3, Parent):-
    (valid_parent_child(Name1, Parent),
     valid_parent_child(Name2, Parent),
     valid_parent_child(Name3, Parent),
     assertz(child(Name1,Parent)),
     assertz(child(Name2,Parent)),
     assertz(child(Name3,Parent)),
     assertz(parent(Parent,Name1)),
     assertz(parent(Parent,Name2)),
     assertz(parent(Parent,Name3)) ).


% Helper predicate to check if a parent-child assertion is valid
valid_parent_child(Child, Parent) :-
    \+ child(Child, Parent),         % The Parent is not already a parent of the Child
    can_have_additional_parent(Child).  % The Child can have an additional parent
% Check if a child can have an additional parent
can_have_additional_parent(Child) :-
    findall(P, child(Child, P), Parents),
    length(Parents, Count),
    Count < 2.  % There are less than 2 parents

infer_logic(parents, P1, P2, Child):-
    add_parents_to_child(P1, P2, Child),

      (\+ clause(parents(P1, P2, Child), true) ->
        assertz(parents(P1, P2, Child))
    ;   true).



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





%END OF HELP HERE

% Interpret \+ siblings as check if X and Y are not siblings
learn_siblings(X, Y) :-
    \+ siblings(X, Y),       % Check if the result is NOT siblings on this case
    \+ siblings(Y, X),       % Check if the result is NOT ('\+')
    X \= Y,                  % A person is not their own sibling condition
    assertz(siblings(X, Y)), % make sure the running pl remembers in a file
    assertz(siblings(Y, X)).
   % save_siblings.           % Save to file, file must already exist



% Given a transitive information, like john is siblings with carrie, and
% carrie is siblings with Janna. This would identfiy that john is
% siblings with Janna
infer_siblings :-
    findall((A,B), siblings(A,B), ThePairs), % parameter ('solution', 'goal'. OutputVariable)
    findall((X, Y),
    (
        member((P1, X), ThePairs), % (Somewhat tuple), finds all people whom are siblings with X
        member((P1, Y), ThePairs), % Finds all people who are siblings with Y
        X \= Y,                    % A person is not their own siblings
        \+ member((X, Y), ThePairs),% If the case that these two are not yet siblings
        \+ member((Y, X), ThePairs) % If the case that these to ware not siblings

     ), NewPairs),                 % Saved that case of not siblings to new pairs
     list_to_set(NewPairs, UniquePair), % Remove duplicate pairings
     maplist(assert_new_siblings_pair, UniquePair). % save them back to the running pl



% Saves into one file all relations
% versa if you want to overwrite
save_all :-
    tell('kBase/allDB.pl'),
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
    listing(parents/3),
    listing(children/4),
    listing(siblings/2),
    listing(relatives/2),
    told.                   % Close the file




% loads the database of siblingsDB containing all people who are
% siblings
load_all :-
    consult('kBase/allDB.pl').

% deletes all data of siblings
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
    retractall(siblings(_, _)),
    retractall(relatives(_, _)).
