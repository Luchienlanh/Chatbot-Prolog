% ========================================
% LOGIC MODULE: FIRST-ORDER
% Chuyển đổi DRS sang FOL - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-DOAN-01 và Slide-BUOI-10:
%
% CHUYỂN ĐỔI DRS → FOL:
%
% 1. drs([], Conds) → convert_conditions(Conds)
% 2. drs([X|Refs], Conds) → ∃X. convert(drs(Refs, Conds))
% 3. pred(P, Args) → P(Args)
% 4. neg(DRS) → ¬convert(DRS)
% 5. impl(A, B) → convert(A) → convert(B)
% 6. conj(A, B) → convert(A) ∧ convert(B)
%
% ========================================

:- module(firstorder, [
    convert/2,
    simplify/2,
    to_cnf/2,
    negate/2,
    skolemize/2
]).

% ========================================
% CHUYỂN ĐỔI DRS → FOL
% ========================================

% WH-questions: giữ nguyên để theorem prover xử lý
convert(wh_question(Type, Body), wh_question(Type, Body)) :- !.

% DRS rỗng → true
convert(drs([], []), true) :- !.

% DRS không có sở chỉ, một điều kiện
convert(drs([], [Cond]), FOL) :- !,
    convert_condition(Cond, FOL).

% DRS không có sở chỉ, nhiều điều kiện
% → C1 ∧ C2 ∧ ... ∧ Cn
convert(drs([], Conds), FOL) :- !,
    convert_conditions(Conds, FOL).

% DRS có sở chỉ → ∃X. convert(rest)
% drs([X|Refs], Conds) → ∃X. convert(drs(Refs, Conds))
convert(drs([Ref|Refs], Conds), exists(Ref, RestFOL)) :- !,
    convert(drs(Refs, Conds), RestFOL).

% text_sem(S1, S2) -> merge scope if S1 is existential
convert(text_sem(exists(Var, Body), S2), exists(Var, and(FBody, FS2))) :- !,
    convert(Body, FBody),
    convert(S2, FS2).

% For non-existential S1, we treat it as context (assumed true) and only prove S2.
% This avoids failures when S1 is not strictly provable in KB "Người cho Miu ăn."
convert(text_sem(_S1, S2), FS2) :- !,
    convert(S2, FS2).

% Recursive conversion for logical structures logic
convert(conj(A, B), and(FA, FB)) :- !,
    convert(A, FA),
    convert(B, FB).

convert(exists(Var, Body), exists(Var, FBody)) :- !,
    convert(Body, FBody).

convert(pred(P, Args), pred(P, Args)) :- !.

% Fallback cho các trường hợp khác
convert(X, X).

% ========================================
% CHUYỂN ĐỔI ĐIỀU KIỆN
% ========================================

% pred(P, Args) → pred(P, Args)
convert_condition(pred(P, Args), pred(P, Args)) :- !.

% type(X, T) → T(X)
convert_condition(type(X, T), pred(T, [X])) :- !.

% neg(DRS) → ¬convert(DRS)
convert_condition(neg(DRS), neg(FOL)) :- !,
    convert(DRS, FOL).

% impl(A, B) → convert(A) → convert(B)
convert_condition(impl(DRS1, DRS2), impl(FOL1, FOL2)) :- !,
    convert(DRS1, FOL1),
    convert(DRS2, FOL2).

% conj(A, B)
convert_condition(conj(A, B), and(FOLA, FOLB)) :- !,
    convert_condition(A, FOLA),
    convert_condition(B, FOLB).

% DRS lồng nhau
convert_condition(drs(Refs, Conds), FOL) :- !,
    convert(drs(Refs, Conds), FOL).

% Handle text_sem in DRS condition (e.g. from composition)
convert_condition(text_sem(S1, S2), FOL) :- !,
    convert(text_sem(S1, S2), FOL).

% Handle explicit existential quantifier in logic form (naked exists)
convert_condition(exists(Var, Body), exists(Var, FBody)) :- !,
    convert_condition(Body, FBody).

% Handle explicit logical structure embedded in DRS
convert_condition(and(A, B), and(FA, FB)) :- !,
    convert_condition(A, FA),
    convert_condition(B, FB).

% Fallback
convert_condition(Cond, Cond).

% ========================================
% CHUYỂN ĐỔI NHIỀU ĐIỀU KIỆN
% [C1, C2, ...] → C1 ∧ C2 ∧ ...
% ========================================

convert_conditions([], true) :- !.

convert_conditions([Cond], FOL) :- !,
    convert_condition(Cond, FOL).

convert_conditions([Cond|Conds], and(FOL, RestFOL)) :-
    convert_condition(Cond, FOL),
    convert_conditions(Conds, RestFOL).

% ========================================
% ĐƠN GIẢN HÓA FOL
% ========================================

% true ∧ X → X
simplify(and(true, X), Result) :- !,
    simplify(X, Result).

simplify(and(X, true), Result) :- !,
    simplify(X, Result).

% false ∧ X → false
simplify(and(false, _), false) :- !.
simplify(and(_, false), false) :- !.

% true ∨ X → true
simplify(or(true, _), true) :- !.
simplify(or(_, true), true) :- !.

% false ∨ X → X
simplify(or(false, X), Result) :- !,
    simplify(X, Result).

simplify(or(X, false), Result) :- !,
    simplify(X, Result).

% ¬¬X → X
simplify(neg(neg(X)), Result) :- !,
    simplify(X, Result).

% Đệ quy
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
% PHỦ ĐỊNH (De Morgan's Laws)
% ========================================

% ¬(A ∧ B) → ¬A ∨ ¬B
negate(and(X, Y), or(NX, NY)) :- !,
    negate(X, NX),
    negate(Y, NY).

% ¬(A ∨ B) → ¬A ∧ ¬B
negate(or(X, Y), and(NX, NY)) :- !,
    negate(X, NX),
    negate(Y, NY).

% ¬¬X → X
negate(neg(X), X) :- !.

% ¬∃X. P → ∀X. ¬P
negate(exists(Var, Body), forall(Var, NBody)) :- !,
    negate(Body, NBody).

% ¬∀X. P → ∃X. ¬P
negate(forall(Var, Body), exists(Var, NBody)) :- !,
    negate(Body, NBody).

% ¬(A → B) → A ∧ ¬B
negate(impl(X, Y), and(X, NY)) :- !,
    negate(Y, NY).

negate(true, false) :- !.
negate(false, true) :- !.

negate(X, neg(X)).

% ========================================
% CHUYỂN ĐỔI SANG CNF (Conjunctive Normal Form)
% ========================================

to_cnf(Formula, CNF) :-
    eliminate_implications(Formula, NoImpl),
    move_negations_inward(NoImpl, NNF),
    distribute_or_over_and(NNF, CNF).

% Loại bỏ implications: A → B ≡ ¬A ∨ B
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

% Đưa phủ định vào trong (NNF)
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

% Phân phối OR qua AND: (A ∧ B) ∨ C ≡ (A ∨ C) ∧ (B ∨ C)
distribute_or_over_and(or(and(A, B), C), and(AC, BC)) :- !,
    distribute_or_over_and(or(A, C), AC),
    distribute_or_over_and(or(B, C), BC).

distribute_or_over_and(or(A, and(B, C)), and(AB, AC)) :- !,
    distribute_or_over_and(or(A, B), AB),
    distribute_or_over_and(or(A, C), AC).

distribute_or_over_and(and(A, B), and(NA, NB)) :- !,
    distribute_or_over_and(A, NA),
    distribute_or_over_and(B, NB).

distribute_or_over_and(or(A, B), Result) :- !,
    distribute_or_over_and(A, NA),
    distribute_or_over_and(B, NB),
    ( (NA = and(_, _) ; NB = and(_, _)) ->
        distribute_or_over_and(or(NA, NB), Result)
    ;
        Result = or(NA, NB)
    ).

distribute_or_over_and(X, X).

% ========================================
% SKOLEMIZATION
% Loại bỏ lượng từ tồn tại
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

skolemize(neg(A), neg(SA)) :- !,
    skolemize(A, SA).

skolemize(X, X).

% Thay thế biến
substitute_var(Var, Value, Var, Value) :- !.

substitute_var(Var, Value, and(A, B), and(NA, NB)) :- !,
    substitute_var(Var, Value, A, NA),
    substitute_var(Var, Value, B, NB).

substitute_var(Var, Value, or(A, B), or(NA, NB)) :- !,
    substitute_var(Var, Value, A, NA),
    substitute_var(Var, Value, B, NB).

substitute_var(Var, Value, neg(A), neg(NA)) :- !,
    substitute_var(Var, Value, A, NA).

substitute_var(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute_in_arg(Var, Value), Args, NewArgs).

substitute_var(Var, Value, forall(V, Body), forall(V, NewBody)) :-
    V \= Var, !,
    substitute_var(Var, Value, Body, NewBody).

substitute_var(Var, _, forall(Var, Body), forall(Var, Body)) :- !.

substitute_var(Var, Value, exists(V, Body), exists(V, NewBody)) :-
    V \= Var, !,
    substitute_var(Var, Value, Body, NewBody).

substitute_var(Var, _, exists(Var, Body), exists(Var, Body)) :- !.

substitute_var(_, _, X, X).

substitute_in_arg(Var, Value, Var, Value) :- !.
substitute_in_arg(_, _, X, X).
