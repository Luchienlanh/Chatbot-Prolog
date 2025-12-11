% ========================================
% LOGIC MODULE: FIRST-ORDER
% First-Order Logic Conversion & Operations
% ========================================

:- module(firstorder, [
    convert/2,
    simplify/2,
    to_cnf/2,
    negate/2
]).

% ========================================
% DRS TO FOL CONVERSION
% ========================================

% Empty DRS → true
convert(drs([], []), true) :- !.

% DRS with no referents, single condition
convert(drs([], [Cond]), FOL) :- !,
    convert_condition(Cond, FOL).

% DRS with no referents, multiple conditions
convert(drs([], Conds), FOL) :- !,
    convert_conditions(Conds, FOL).

% DRS with referents → existential quantification
convert(drs([Ref|Refs], Conds), exists(Ref, RestFOL)) :- !,
    convert(drs(Refs, Conds), RestFOL).

% ========================================
% CONDITION CONVERSION
% ========================================

convert_condition(pred(P, Args), pred(P, Args)) :- !.

convert_condition(type(X, T), pred(T, [X])) :- !.

convert_condition(neg(DRS), neg(FOL)) :- !,
    convert(DRS, FOL).

convert_condition(impl(DRS1, DRS2), impl(FOL1, FOL2)) :- !,
    convert(DRS1, FOL1),
    convert(DRS2, FOL2).

convert_condition(drs(Refs, Conds), FOL) :- !,
    convert(drs(Refs, Conds), FOL).

convert_condition(Cond, Cond).

% ========================================
% MULTIPLE CONDITIONS
% ========================================

convert_conditions([], true) :- !.

convert_conditions([Cond], FOL) :- !,
    convert_condition(Cond, FOL).

convert_conditions([Cond|Conds], and(FOL, RestFOL)) :-
    convert_condition(Cond, FOL),
    convert_conditions(Conds, RestFOL).

% ========================================
% FOL SIMPLIFICATION
% ========================================

% true and X → X
simplify(and(true, X), Result) :- !,
    simplify(X, Result).

simplify(and(X, true), Result) :- !,
    simplify(X, Result).

% false and X → false
simplify(and(false, _), false) :- !.
simplify(and(_, false), false) :- !.

% true or X → true
simplify(or(true, _), true) :- !.
simplify(or(_, true), true) :- !.

% false or X → X
simplify(or(false, X), Result) :- !,
    simplify(X, Result).

simplify(or(X, false), Result) :- !,
    simplify(X, Result).

% not(not(X)) → X
simplify(neg(neg(X)), Result) :- !,
    simplify(X, Result).

% Recursive simplification
simplify(and(X, Y), and(SX, SY)) :- !,
    simplify(X, SX),
    simplify(Y, SY).

simplify(or(X, Y), or(SX, SY)) :- !,
    simplify(X, SX),
    simplify(Y, SY).

simplify(neg(X), neg(SX)) :- !,
    simplify(X, SX).

simplify(impl(X, Y), impl(SX, SY)) :- !,
    simplify(X, SX),
    simplify(Y, SY).

simplify(exists(Var, Body), exists(Var, SBody)) :- !,
    simplify(Body, SBody).

simplify(forall(Var, Body), forall(Var, SBody)) :- !,
    simplify(Body, SBody).

simplify(X, X).

% ========================================
% NEGATION
% Apply De Morgan's laws
% ========================================

negate(and(X, Y), or(NX, NY)) :- !,
    negate(X, NX),
    negate(Y, NY).

negate(or(X, Y), and(NX, NY)) :- !,
    negate(X, NX),
    negate(Y, NY).

negate(neg(X), X) :- !.

negate(exists(Var, Body), forall(Var, NBody)) :- !,
    negate(Body, NBody).

negate(forall(Var, Body), exists(Var, NBody)) :- !,
    negate(Body, NBody).

negate(impl(X, Y), and(X, NY)) :- !,
    negate(Y, NY).

negate(true, false) :- !.
negate(false, true) :- !.

negate(X, neg(X)).

% ========================================
% CONJUNCTIVE NORMAL FORM (CNF)
% ========================================

to_cnf(Formula, CNF) :-
    eliminate_implications(Formula, NoImpl),
    move_negations_inward(NoImpl, NNF),
    distribute_or_over_and(NNF, CNF).

% Eliminate implications: A → B ≡ ¬A ∨ B
eliminate_implications(impl(A, B), or(NA, NB)) :- !,
    eliminate_implications(A, NA1),
    negate(NA1, NA),
    eliminate_implications(B, NB).

eliminate_implications(and(A, B), and(NA, NB)) :- !,
    eliminate_implications(A, NA),
    eliminate_implications(B, NB).

eliminate_implications(or(A, B), or(NA, NB)) :- !,
    eliminate_implications(A, NA),
    eliminate_implications(B, NB).

eliminate_implications(neg(A), neg(NA)) :- !,
    eliminate_implications(A, NA).

eliminate_implications(exists(V, B), exists(V, NB)) :- !,
    eliminate_implications(B, NB).

eliminate_implications(forall(V, B), forall(V, NB)) :- !,
    eliminate_implications(B, NB).

eliminate_implications(X, X).

% Move negations inward (Negation Normal Form)
move_negations_inward(neg(and(A, B)), or(NA, NB)) :- !,
    move_negations_inward(neg(A), NA),
    move_negations_inward(neg(B), NB).

move_negations_inward(neg(or(A, B)), and(NA, NB)) :- !,
    move_negations_inward(neg(A), NA),
    move_negations_inward(neg(B), NB).

move_negations_inward(neg(neg(A)), NA) :- !,
    move_negations_inward(A, NA).

move_negations_inward(neg(exists(V, B)), forall(V, NB)) :- !,
    move_negations_inward(neg(B), NB).

move_negations_inward(neg(forall(V, B)), exists(V, NB)) :- !,
    move_negations_inward(neg(B), NB).

move_negations_inward(and(A, B), and(NA, NB)) :- !,
    move_negations_inward(A, NA),
    move_negations_inward(B, NB).

move_negations_inward(or(A, B), or(NA, NB)) :- !,
    move_negations_inward(A, NA),
    move_negations_inward(B, NB).

move_negations_inward(exists(V, B), exists(V, NB)) :- !,
    move_negations_inward(B, NB).

move_negations_inward(forall(V, B), forall(V, NB)) :- !,
    move_negations_inward(B, NB).

move_negations_inward(X, X).

% Distribute OR over AND: (A ∧ B) ∨ C ≡ (A ∨ C) ∧ (B ∨ C)
distribute_or_over_and(or(and(A, B), C), and(AC, BC)) :- !,
    distribute_or_over_and(or(A, C), AC),
    distribute_or_over_and(or(B, C), BC).

distribute_or_over_and(or(A, and(B, C)), and(AB, AC)) :- !,
    distribute_or_over_and(or(A, B), AB),
    distribute_or_over_and(or(A, C), AC).

distribute_or_over_and(and(A, B), and(NA, NB)) :- !,
    distribute_or_over_and(A, NA),
    distribute_or_over_and(B, NB).

distribute_or_over_and(or(A, B), or(NA, NB)) :- !,
    distribute_or_over_and(A, NA),
    distribute_or_over_and(B, NB).

distribute_or_over_and(X, X).

% ========================================
% SKOLEMIZATION
% Eliminate existential quantifiers
% ========================================

skolemize(exists(Var, Body), Skolemized) :- !,
    gensym(sk, SkolemConst),
    substitute_var(Var, SkolemConst, Body, SubBody),
    skolemize(SubBody, Skolemized).

skolemize(forall(Var, Body), forall(Var, Skolemized)) :- !,
    skolemize(Body, Skolemized).

skolemize(and(A, B), and(SA, SB)) :- !,
    skolemize(A, SA),
    skolemize(B, SB).

skolemize(or(A, B), or(SA, SB)) :- !,
    skolemize(A, SA),
    skolemize(B, SB).

skolemize(X, X).

substitute_var(Var, Value, Var, Value) :- !.

substitute_var(Var, Value, and(A, B), and(NA, NB)) :- !,
    substitute_var(Var, Value, A, NA),
    substitute_var(Var, Value, B, NB).

substitute_var(Var, Value, or(A, B), or(NA, NB)) :- !,
    substitute_var(Var, Value, A, NA),
    substitute_var(Var, Value, B, NB).

substitute_var(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute_in_arg(Var, Value), Args, NewArgs).

substitute_var(_, _, X, X).

substitute_in_arg(Var, Value, Var, Value) :- !.
substitute_in_arg(_, _, X, X).
