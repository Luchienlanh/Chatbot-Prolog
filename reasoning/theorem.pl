% ========================================
% REASONING MODULE: THEOREM
% Theorem Proving Engine with Resolution
% ========================================

:- module(theorem, [
    prove/1,
    prove_with_trace/1,
    find_entities/2,
    find_values/2,
    initialize_prover/0
]).

:- use_module('../knowledge/repository').

% ========================================
% INITIALIZATION
% ========================================

initialize_prover :-
    % Initialize theorem proving engine
    true.

% ========================================
% PROVE - Main proving interface
% ========================================

prove(true) :- !.
prove(false) :- !, fail.

prove(pred(P, Args)) :- !,
    repository:fact(pred(P, Args)).

prove(and(A, B)) :- !,
    prove(A),
    prove(B).

prove(or(A, B)) :- !,
    ( prove(A) ; prove(B) ).

prove(neg(A)) :- !,
    \+ prove(A).

prove(impl(A, B)) :- !,
    ( \+ prove(A) ; prove(B) ).

prove(exists(Var, Body)) :- !,
    prove_exists(Var, Body).

prove(forall(Var, Body)) :- !,
    prove_forall(Var, Body).

% ========================================
% EXISTENTIAL QUANTIFICATION
% ========================================

prove_exists(Var, Body) :-
    find_substitution(Var, Body, Value),
    substitute_and_prove(Var, Value, Body).

find_substitution(Var, Body, Value) :-
    extract_type(Body, Type),
    repository:entity(Value, Type).

extract_type(and(pred(Type, [Var]), _), Type) :- !.
extract_type(and(_, Rest), Type) :- !,
    extract_type(Rest, Type).
extract_type(pred(Type, [Var]), Type).

substitute_and_prove(Var, Value, Body) :-
    substitute_in_body(Var, Value, Body, SubBody),
    prove(SubBody).

substitute_in_body(Var, Value, Var, Value) :- !.

substitute_in_body(Var, Value, and(A, B), and(NA, NB)) :- !,
    substitute_in_body(Var, Value, A, NA),
    substitute_in_body(Var, Value, B, NB).

substitute_in_body(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute_arg(Var, Value), Args, NewArgs).

substitute_in_body(_, _, X, X).

substitute_arg(Var, Value, Var, Value) :- !.
substitute_arg(_, _, X, X).

% ========================================
% UNIVERSAL QUANTIFICATION
% ========================================

prove_forall(Var, Body) :-
    \+ (
        find_substitution(Var, Body, Value),
        \+ substitute_and_prove(Var, Value, Body)
    ).

% ========================================
% FIND ENTITIES (for WHO questions)
% ========================================

find_entities(exists(Var, Body), Entities) :-
    findall(Var, 
        ( find_substitution(Var, Body, Var),
          substitute_and_prove(Var, Var, Body)
        ),
        Entities).

find_entities(and(pred(Type, [Var]), Condition), Entities) :-
    findall(Entity,
        ( repository:entity(Entity, Type),
          substitute_in_body(Var, Entity, Condition, SubCond),
          prove(SubCond)
        ),
        Entities).

% ========================================
% FIND VALUES (for WHAT questions)
% ========================================

find_values(exists(Var, Body), Values) :-
    findall(Var,
        substitute_and_prove(Var, Var, Body),
        Values).

find_values(pred(Relation, [Subject, Var]), Values) :-
    findall(Value,
        repository:fact(pred(Relation, [Subject, Value])),
        Values).

% ========================================
% PROVE WITH TRACE
% ========================================

prove_with_trace(Goal) :-
    format('Proving: ~w~n', [Goal]),
    prove_with_depth(Goal, 0).

prove_with_depth(Goal, Depth) :-
    indent(Depth),
    format('→ ~w~n', [Goal]),
    ( Goal = and(A, B) ->
        NewDepth is Depth + 1,
        prove_with_depth(A, NewDepth),
        prove_with_depth(B, NewDepth)
    ; Goal = pred(P, Args) ->
        ( repository:fact(Goal) ->
            indent(Depth),
            writeln('  ✓ Found in KB')
        ;
            indent(Depth),
            writeln('  ✗ Not found'),
            fail
        )
    ;
        prove(Goal)
    ).

indent(0) :- !.
indent(N) :-
    N > 0,
    write('  '),
    N1 is N - 1,
    indent(N1).
