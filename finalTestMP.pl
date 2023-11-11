% Update your python with
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
    (   process_common_case(Words);  % Find the common sentence structure
         match_case(Words)),         % Find the common word
    Response = 'OK I learned something.'.

% helper func for the process_common_case
construct_and_assert(Predicate, FirstName, LastName) :-
    Fact =.. [Predicate, FirstName, LastName],  % automatically create example: father(X, Y) into the database, no saving yet
    (  \+ clause(Fact, true) -> infer_logic(Predicate, FirstName, LastName); true).  % Check first if in database, else just declare true


% Save facts once user exits
save_facts_to_file :-
    tell('kBase/factsDB.pl'),
    listing(sister/2),
    listing(siblings/2),
    told.




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
    (   clause(Fact, true) -> Response = 'Yes!'
        ; \+ clause(Fact, true) -> Response = 'That\'s impossible!'
    ).
process_common_who(Words, Response):-
    extract_names_and_relation(Words, Name, Relation),
    map_predicate(Relation, Predicate),
    findall(Member, call(Predicate, Member, Name), Members),
    string_concat('Sorry, I don\'t recall them having ', Relation, Result),
   (   Members = [] -> Response = Result; Response = Members).



is_start_who([FirstWord|_]):-
    FirstWord = "who".
% Are _ and _ siblings? (Unfinished)
% Put down the following and see let PL identify which predicate aligns
match_case([ "are", Name1, "and", Name2, "siblings"], Response):-
    (   siblings(Name1, Name2) -> Response = 'Yes!';
        \+ siblings(Name1, Name2) -> Response = 'That\'s Impossible!').
    % infer_siblings will not be used anymore since it is possible to have mixed_siblings


% Are  _ and _ parents of _ ?(Unfinished)
match_case(["are", Name1, "and", Name2, "parents", "of", Name3], Response):-
    infer_logic(parents, Name1, Name2, Name3),
    Response = 'temp'.   % Temporary response section

% Are _, _, and _ children of _ ?(Unfinished)
match_case(["are", Name1, Name2, "and", Name3, "children", "of", Name4], Response):-
    infer_logic(children, Name1, Name2, Name3, Name4),
    Response = 'temp'.
% Are _ and _ relatives?(Unfinished)
match_case(["are", Name1, "and", Name2, "relatives"], Response):-
    infer_logic(children, Name1, Name2),
    Response = 'temp'.


% INFERENCE LOGIC SECTION
%
% INference logic for SISTER, assumes that x is a sister of y and vice
% versa
% they are considered siblings
% half sibling or not is not accounted
% Age between each other is not accounted
infer_logic(sister, X, Y):-
    (   \+ sister(X, Y) -> assertz(sister(X, Y)) ;true),
    learn_siblings(X, Y).

% HELP HERE, provide more inference logic below follow format
% infer_logic(predicate, X, Y).
infer_logic(children, Name1, Name2, Name3, Parent):-
    assertz(children(Name1, Name2, Name3, Parent)),
    assertz(child(Name1,Parent)),
    assertz(child(Name2,Parent)),
    assertz(child(Name3,Parent)).


infer_logic(parents, P1, P2, Child):-
    assertz(parents(P1, P2, Child)).








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

% Helper function to accept the tuple
assert_new_siblings_pair((X, Y)) :-
    learn_siblings(X, Y).

% Creates kBase directory if it does not exist yet
ensure_directory_exists(Directory) :-
    exists_directory(Directory)-> true;
    make_directory(Directory).

% Saves into one file by appending, change 'append' to 'tell' or vice
% versa if you want to overwrite
save_siblings :-
    ensure_directory_exists('kBase'),
    tell('kBase/siblingsDB.pl'),
    listing(siblings),
    told.




% loads the database of siblingsDB containing all people who are
% siblings
load_siblings :-
    consult('kBase/siblingsDB.pl').

% deletes all data of siblings
delete_siblings :-
    retractall(siblings(_, _)),
    (   exists_file('kBase/siblingsDB.pl')
    ->  open('kBase/siblingsDB.pl', write, Stream),
        close(Stream); % This is what happens when if is true
    false).            % This is what happens if it is false, prints false



% This is nothing
newgame(X) :- retractall(secret_num(_)), asserta(secret_num(X)).
