% ========================================
% LOGIC MODULE: FIRST-ORDER
% Chuyển đổi DRS sang FOL - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-BUOI-11, Trang 38-43:
%
% 6 QUY TẮC DỊCH DRS SANG FOL (giả sử f là hàm dịch):
%
% Quy tắc 1 (Trang 39):
%   f({X1,X2,..,Xn},{γ1,γ2,..,γm}) = ∃X1,X2,..,Xn. f(γ1) ∧ f(γ2) ∧...∧ f(γm)
%
% Quy tắc 2 (Trang 40):
%   Nếu R là quan hệ trên các biến/hằng X1,..,Xn thì:
%   f(R(X1,X2,..,Xn)) = R(X1,X2,..,Xn)
%
% Quy tắc 3 (Trang 41):
%   Nếu X1 và X2 là hằng hoặc biến thì:
%   f(X1 = X2) = X1 = X2
%
% Quy tắc 4 (Trang 42):
%   f(¬B) = ¬f(B)
%
% Quy tắc 5 (Trang 42):
%   f(B ∨ C) = f(B) ∨ f(C)
%
% Quy tắc 6 (Trang 43):
%   f({X1,X2,..,Xn},{γ1,...,γm} → B) = ∀X1,X2,..,Xn. f(γ1)∧...∧f(γm) → f(B)
%
% ========================================

:- module(firstorder, [
    convert/2,
    simplify/2,
    to_cnf/2,
    negate/2
]).

% ========================================
% CHUYỂN ĐỔI DRS → FOL
% ========================================

% WH-questions: giữ nguyên để theorem prover xử lý
% Format cũ: wh_question(Type, Body)
convert(wh_question(Type, Body), wh_question(Type, ConvBody)) :- !,
    convert(Body, ConvBody).



% Format mới: wh_question(Type, QueryVar, Body) - giữ QueryVar để tìm kiếm
convert(wh_question(Type, QueryVar, Body), wh_question(Type, QueryVar, ConvBody)) :- !,
    convert(Body, ConvBody).

% Pronoun DRS: giữ nguyên nếu chưa resolve
convert(pronoun_drs(Var, Body), pronoun_drs(Var, ConvBody)) :- !,
    convert(Body, ConvBody).

% Format mới 3 args: bỏ qua wrapper, convert body
convert(pronoun_drs(_, _, Body), FOL) :- !,
    convert(Body, FOL).

% ----------------------------------------
% QUY TẮC 1 (Trang 39):
% f({X1,..,Xn},{γ1,..,γm}) = ∃X1,..,Xn. f(γ1) ∧...∧ f(γm)
%
% DRS có referents → lượng từ tồn tại cho tất cả referents
% ----------------------------------------

% DRS rỗng → true
convert(drs([], []), true) :- !.

% DRS không có sở chỉ, chỉ có điều kiện
convert(drs([], Conds), FOL) :- !,
    convert_conditions(Conds, FOL).

% Rule 1: DRS with referents
convert(drs([Ref|Refs], Conds), BodyFOL) :-
    vocabulary:word_semantics(Ref, noun_proper, _), !,
    convert(drs(Refs, Conds), BodyFOL).

% DRS có sở chỉ → ∃X1. ∃X2. ... f(conditions)
convert(drs([Ref|Refs], Conds), exists(Ref, BodyFOL)) :-
    convert(drs(Refs, Conds), BodyFOL).

% Existential quantifier (từ WH-words format λP.∃x(P(x)))
convert(exists(Var, Body), exists(Var, ConvBody)) :- !,
    convert(Body, ConvBody).

% Merge -> And
convert(merge(A, B), and(FA, FB)) :- !,
    convert(A, FA),
    convert(B, FB).

% ----------------------------------------
% QUY TẮC 2 (Trang 40):
% f(R(X1,..,Xn)) = R(X1,..,Xn)
% Predicate giữ nguyên
% ----------------------------------------

% Force Equality Check
convert_condition(X = Y, X = Y) :- !.

convert_condition(Pred, pred(Functor, Args)) :- 
    Pred =.. [Functor|Args],
    Args \= [],
    Args \= [],
    \+ member(Functor, [neg, impl, or, drs, pronoun_drs, exists, =]),
    !.

% ----------------------------------------
% QUY TẮC 3 (Trang 41):
% f(X1 = X2) = X1 = X2
% Điều kiện đẳng thức giữ nguyên
% ----------------------------------------

convert_condition(X = Y, X = Y) :- !.

% ----------------------------------------
% QUY TẮC 4 (Trang 42):
% f(¬B) = ¬f(B)
% ----------------------------------------

convert_condition(neg(DRS), neg(FOL)) :- !,
    convert(DRS, FOL).

% ----------------------------------------
% QUY TẮC 5 (Trang 42):
% f(B ∨ C) = f(B) ∨ f(C)
% ----------------------------------------

convert_condition(or(A, B), or(FOLA, FOLB)) :- !,
    convert(A, FOLA),
    convert(B, FOLB).

% ----------------------------------------
% QUY TẮC 6 (Trang 43):
% f({X1,..,Xn},{γ1,...} → B) = ∀X1,..,Xn. f(γ1)∧... → f(B)
% 
% Implication với DRS condition (từ "mọi")
% ----------------------------------------

convert_condition(impl(drs([Ref|Refs], Conds), Consequent), forall(Ref, RestFOL)) :- !,
    convert_condition(impl(drs(Refs, Conds), Consequent), RestFOL).

convert_condition(impl(drs([], Conds), Consequent), impl(CondFOL, ConsFOL)) :- !,
    convert_conditions(Conds, CondFOL),
    convert(Consequent, ConsFOL).

% Implication không có DRS
convert_condition(impl(A, B), impl(FOLA, FOLB)) :- !,
    convert(A, FOLA),
    convert(B, FOLB).

% DRS lồng nhau
convert_condition(drs(Refs, Conds), FOL) :- !,
    convert(drs(Refs, Conds), FOL).

% Pronoun DRS lồng nhau
convert_condition(pronoun_drs(_, _, Body), FOL) :- !,
    convert(Body, FOL).

% Existential Quantifier lồng nhau
convert_condition(exists(Var, Body), exists(Var, FOLBody)) :- !,
    convert(Body, FOLBody).

% Fallback
convert_condition(Cond, Cond).

% ========================================
% CHUYỂN ĐỔI NHIỀU ĐIỀU KIỆN
% [γ1, γ2, ...] → f(γ1) ∧ f(γ2) ∧ ...
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
simplify(and(X, Y), Result) :- !,
    simplify(X, SX),
    simplify(Y, SY),
    ( SX = true -> Result = SY
    ; SY = true -> Result = SX
    ; Result = and(SX, SY)
    ).

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

% Phân phối OR qua AND
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
