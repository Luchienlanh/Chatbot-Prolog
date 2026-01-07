% ========================================
% LINGUISTIC MODULE: COMPOSITION
% Lambda Calculus & Semantic Composition - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-BUOI-11, Trang 16-22:
%
% QUY TẮC TÍNH TOÁN DRS:
%
% 1) Ngữ chỉ gồm một thành tố (Trang 17):
%    X → Y  =>  ||X|| = ||Y||
%
% 2) Câu đơn (Trang 18):
%    S → NP VP  =>  ||S|| = ||NP|| ⊕ ||VP|| = ||NP||@||VP||
%
% 3) Danh ngữ chứa danh từ chỉ loại (Trang 19):
%    NP → NNC NN  =>  ||NP|| = ||NNC|| ⊕ ||NN|| = ||NN||
%
% 4) Danh ngữ chứa số từ/định từ (Trang 20):
%    NP → CD NP  =>  ||NP|| = ||CD||@||NP||
%
% 5) Động ngữ có tân ngữ trực tiếp (Trang 21):
%    VP → VP NP  =>  ||VP|| = ||VP||@||NP||
%
% 6) Động ngữ có tân ngữ trực tiếp và gián tiếp (Trang 22):
%    VP → VP NP-IOB NP-DOB  =>  ||VP|| = (||VP||@||NP-DOB||)@||NP-IOB||
%
% ========================================

:- module(composition, [
    compose/2,
    beta_reduce/2,
    beta_reduce_full/2,
    substitute/4
]).

:- use_module('vocabulary').

:- discontiguous compose/2.
:- discontiguous beta_reduce/2.

% ========================================
% COMPOSE - TÍNH TOÁN NGỮ NGHĨA TỪ CÂY CÚ PHÁP
% ========================================

% -------------------------------------------
% QUY TẮC 2 (Trang 18): Câu đơn
% S → NP VP  =>  ||S|| = ||NP||@||VP||
% -------------------------------------------

compose(tree(s, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% TÍNH TOÁN DRS VĂN BẢN (Trang 24)
% DRS văn bản = Φ1 ⊕ Φ2 ⊕ ... ⊕ Φn
% -------------------------------------------

compose(tree(text, [S1, S2]), Semantics) :- !,
    compose(S1, S1Sem),
    compose(S2, S2Sem),
    % Merge DRS của hai câu
    Semantics = merge(S1Sem, S2Sem).

% NP Statement (Một con mèo.)
% Treated as existential sentence: "There is a cat."
compose(tree(np_statement, [NP]), Semantics) :- !,
    compose(NP, NPSem),
    % Apply to "true" property (lambda x. true) to force existential quantification
    Prop = lambda(x, drs([],[])),
    RawSem = app(NPSem, Prop),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% CÂU HỎI WH (Slide-DOAN-01, Trang 12)
% "Ai thích X?" => Vị_từ(?, X)
% "X thích gì?" => Vị_từ(X, ?)
% -------------------------------------------

% SBARQ → WH VP (Ai thích hoa?)
% WH-word format: λP. ∃x. (P@x) - KHÔNG có wh_question wrapper
% Loại câu hỏi (who/what/where) xác định từ query type input, không phải từ semantics
compose(tree(sbarq, [WH, VP]), Semantics) :-
    is_wh_node(WH), !,
    compose(WH, WHSem),
    compose(VP, VPSem),
    % WHSem = lambda(p, exists(who_var, app(p, who_var)))
    % VPSem = lambda(x, drs([...],[verb(x, obj)]))
    RawSem = app(WHSem, VPSem),
    beta_reduce_full(RawSem, Semantics).
    % KHÔNG wrap trong wh_question - trả về biểu thức thuần túy

% SBARQ → NP VP (Linh thích gì?)
compose(tree(sbarq, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% SQ -> NP VP QM (Linh thích hoa không?)
% Ignor QM, return prop: NP@VP
compose(tree(sq, [NP, VP, _QM]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% SQ -> NP VP (Nó là của Linh?) - 2 args
compose(tree(sq, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% Helper: check if node is WH (có thể wrap trong NP)
is_wh_node(tree(wp, _)).
is_wh_node(tree(wrb, _)).
is_wh_node(tree(np, [WH])) :- is_wh_node(WH).

% -------------------------------------------
% QUY TẮC 1 (Trang 17): Ngữ một thành tố
% X → Y  =>  ||X|| = ||Y||
% -------------------------------------------

% QP lifting cho Common Noun (xe đạp => một xe đạp)
% Rule: NP -> NN  =>  ||NP|| = λP. ∃x. (||NN||@x & P@x)
compose(tree(np, [NN]), Semantics) :-
    NN = tree(nn, _), !,
    compose(NN, NNSem),
    % NNSem = lambda(x, drs([],[noun(x)]))
    % Tạo QP: lambda(p, exists(x, merge(noun(x), app(p,x))))
    gensym(x, Var),
    % NNSem @ Var
    NounApp = app(NNSem, Var),
    beta_reduce_full(NounApp, NounConds),
    % Result
    Semantics = lambda(p, exists(Var, merge(NounConds, app(p, Var)))).


compose(tree(np, [Child]), Semantics) :- !,
    compose(Child, Semantics).

compose(tree(vp, [Child]), Semantics) :- !,
    compose(Child, Semantics).

% -------------------------------------------
% QUY TẮC 3 (Trang 19): Danh ngữ với loại từ
% NP → NNC NN  =>  ||NP|| = ||NN||
% -------------------------------------------



compose(tree(np, [tree(cl, _), NN]), Semantics) :- !,
    compose(NN, Semantics).

% -------------------------------------------
% QUY TẮC 4 (Trang 20): Danh ngữ với số từ/định từ
% NP → CD NP  =>  ||NP|| = ||CD||@||NP||
% -------------------------------------------

% NP -> PN DT (Anh ấy) - Apply DT to PN
compose(tree(np, [PN, DT]), Semantics) :-
    ( PN = tree(pn, _) ; PN = tree(nn, _) ), !,
    compose(PN, PNSem),
    compose(DT, DTSem),
    RawSem = app(DTSem, PNSem),
    beta_reduce_full(RawSem, Semantics).

compose(tree(np, [DT, NP]), Semantics) :-
    ( DT = tree(dt, _) ; DT = tree(cd, _) ), !,
    compose(DT, DTSem),
    compose(NP, NPSem),
    RawSem = app(DTSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% NP → DT CL NN (Một con mèo)
compose(tree(np, [DT, tree(cl, _), NN]), Semantics) :- !,
    compose(DT, DTSem),
    compose(NN, NNSem),
    RawSem = app(DTSem, NNSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% QUY TẮC 5 (Trang 21): Động ngữ có tân ngữ
% VP → VP NP  =>  ||VP|| = ||VP||@||NP||
% -------------------------------------------

% VP với WH-word "gì" (thích gì?)
% Format mới: WH-word = λP. wh_question(what, ∃x. (P@x))
% Apply VBSem với WHSem
compose(tree(vp, [VB, NP]), Semantics) :-
    is_wh_node(NP), !,
    compose(VB, VBSem),
    compose(NP, WHSem),
    % VBSem = lambda(p, lambda(x, app(p, lambda(y, drs([],[verb(x,y)])))))
    % WHSem = lambda(p, wh_question(what, exists(y, app(p, y))))
    RawSem = app(VBSem, WHSem),
    beta_reduce_full(RawSem, Semantics).

% VP với NP thông thường (không phải WH-word)
compose(tree(vp, [VB, NP]), Semantics) :-
    NP = tree(np, _),
    \+ is_wh_node(NP), !,
    compose(VB, VBSem),
    compose(NP, NPSem),
    RawSem = app(VBSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% QUY TẮC 6 (Trang 22): Động ngữ 3 ngôi
% VP → VP NP-IOB NP-DOB  =>  ||VP|| = (||VP||@||NP-DOB||)@||NP-IOB||
% -------------------------------------------

compose(tree(vp, [VB, NP1, NP2]), Semantics) :- !,
    compose(VB, VBSem),
    compose(NP1, NP1Sem),  % Direct object
    compose(NP2, NP2Sem),  % Indirect object
    RawSem = app(app(VBSem, NP1Sem), NP2Sem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% VP với copula "là" (Trang 15)
% Từ "là" bỏ qua về mặt ngữ nghĩa
% -------------------------------------------

compose(tree(vp_copula, [_LA, NP]), Semantics) :- !,
    compose(NP, Semantics).

compose(tree(vp_copula, [_LA, NP, PP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(PP, PPSem),
    RawSem = app(NPSem, PPSem),
    beta_reduce_full(RawSem, Semantics).

compose(tree(vp_copula, [_VB, PP]), Semantics) :- !,
    compose(PP, Semantics).

% VP -> ADV_NEG VP (Phủ định)
compose(tree(vp_neg, [Adv, VP]), Semantics) :- !,
    compose(Adv, AdvSem),
    compose(VP, VPSem),
    RawSem = app(AdvSem, VPSem),
    beta_reduce_full(RawSem, Semantics).
% VP -> VB VB NP (Serial verb: thích ngắm hoa)
compose(tree(vp_serial, [VB1, VB2, NP]), Semantics) :- !,
    compose(VB1, V1Sem),
    compose(VB2, V2Sem),
    compose(NP, NPSem),
    % VP1 = VB1(NP)
    RawVP1 = app(V1Sem, NPSem),
    % VP2 = VB2(NP)
    RawVP2 = app(V2Sem, NPSem),
    % Combined: lambda(x, VP1(x) & VP2(x))
    RawSem = lambda(x, merge(app(RawVP1, x), app(RawVP2, x))),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% PP - Prepositional Phrase
% -------------------------------------------

compose(tree(pp, [P, NP]), Semantics) :- !,
    compose(P, PSem),
    compose(NP, NPSem),
    RawSem = app(PSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% Terminal nodes - Tra từ điển
% -------------------------------------------

compose(tree(nnp, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(nn, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(vb, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(wp, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(wrb, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(dt, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(cd, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(cl, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(pn, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(p, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(adj, [Word]), Semantics) :- !, compose(Word, Semantics).
compose(tree(adv_neg, [Word]), Semantics) :- !, compose(Word, Semantics).

% Word - Tra từ điển vocabulary
compose(word(Word, Category), Semantics) :-
    vocabulary:word_semantics(Word, Category, Semantics), !.

compose(word(Word, _), drs([Word], [])) :- !.

% Fallback
compose(X, X) :- atomic(X), !.

% ========================================
% BETA REDUCTION
% Theo nguyên tắc λ-calculus chuẩn
% ========================================

% (λx.M)N => M[x:=N]
beta_reduce(app(lambda(Var, Body), Arg), Result) :- !,
    substitute(Var, Arg, Body, Result).

% Reduce trong lambda body
beta_reduce(lambda(Var, Body), lambda(Var, RBody)) :- !,
    beta_reduce(Body, RBody).

% Reduce function trước
beta_reduce(app(F, A), app(RF, A)) :-
    beta_reduce(F, RF),
    RF \= F, !.

% Reduce argument
beta_reduce(app(F, A), app(F, RA)) :-
    beta_reduce(A, RA),
    RA \= A, !.

% Reduce trong merge
beta_reduce(merge(A, B), Result) :- !,
    beta_reduce(A, RA),
    beta_reduce(B, RB),
    % Nếu cả hai đều là DRS, merge chúng
    ( RA = drs(RefsA, CondsA), RB = drs(RefsB, CondsB) ->
        % QUAN TRỌNG: Không union refs nếu có biến trùng tên nhưng khác vai trò
        % Giữ nguyên cả hai danh sách refs
        append(RefsA, RefsB, Refs),
        append(CondsA, CondsB, Conds),
        Result = drs(Refs, Conds)
    ;
        Result = merge(RA, RB)
    ).

% Reduce trong DRS
beta_reduce(drs(Refs, Conds), drs(Refs, RConds)) :- !,
    maplist(beta_reduce_cond, Conds, RConds).

beta_reduce_cond(Cond, RCond) :-
    ( Cond =.. [Functor|Args], Args \= [] ->
        maplist(beta_reduce, Args, RArgs),
        RCond =.. [Functor|RArgs]
    ;
        RCond = Cond
    ).

% Reduce trong impl
beta_reduce(impl(A, B), impl(RA, RB)) :- !,
    beta_reduce(A, RA),
    beta_reduce(B, RB).

% Reduce trong exists (cho WH-words format λP.∃x(P(x)))
beta_reduce(exists(Var, Body), exists(Var, RBody)) :- !,
    beta_reduce(Body, RBody).

% Reduce trong wh_question
beta_reduce(wh_question(Type, Body), wh_question(Type, RBody)) :- !,
    beta_reduce(Body, RBody).

% Reduce trong neg
beta_reduce(neg(A), neg(RA)) :- !,
    beta_reduce(A, RA).

% Reduce trong pronoun_drs (format mới 3 arg: Var, Gender, Body)
beta_reduce(pronoun_drs(Var, Gender, Body), pronoun_drs(Var, Gender, RBody)) :- !,
    beta_reduce(Body, RBody).

% Fallback - không thay đổi
beta_reduce(X, X).

% Beta reduce đến khi không còn gì để reduce
beta_reduce_full(Term, Result) :-
    beta_reduce(Term, Reduced),
    ( Reduced == Term ->
        Result = Term
    ;
        beta_reduce_full(Reduced, Result)
    ).

% ========================================
% SUBSTITUTION M[x:=N]
% Thay thế biến x bằng N trong M
% ========================================

substitute(Var, Value, Var, Value) :- !.

% Không thay thế trong lambda nếu biến bị bound
substitute(Var, _, lambda(Var, Body), lambda(Var, Body)) :- !.

substitute(Var, Value, lambda(V, Body), lambda(V, NewBody)) :- !,
    substitute(Var, Value, Body, NewBody).

substitute(Var, Value, app(F, A), app(NF, NA)) :- !,
    substitute(Var, Value, F, NF),
    substitute(Var, Value, A, NA).

substitute(Var, Value, merge(A, B), merge(NA, NB)) :- !,
    substitute(Var, Value, A, NA),
    substitute(Var, Value, B, NB).

substitute(Var, Value, drs(Refs, Conds), drs(Refs, NConds)) :- !,
    maplist(substitute_in_cond(Var, Value), Conds, NConds).

substitute(Var, Value, impl(A, B), impl(NA, NB)) :- !,
    substitute(Var, Value, A, NA),
    substitute(Var, Value, B, NB).

substitute(Var, Value, neg(A), neg(NA)) :- !,
    substitute(Var, Value, A, NA).

substitute(Var, Value, wh_question(Type, Body), wh_question(Type, NBody)) :- !,
    substitute(Var, Value, Body, NBody).

% Substitute trong exists (cho WH-words format λP.∃x(P(x)))
% Không thay thế nếu biến bị bound bởi exists
substitute(Var, _, exists(Var, Body), exists(Var, Body)) :- !.
substitute(Var, Value, exists(V, Body), exists(V, NBody)) :- !,
    substitute(Var, Value, Body, NBody).

substitute(Var, Value, pronoun_drs(V, Gender, Body), pronoun_drs(V, Gender, NBody)) :-
    V \= Var, !,
    substitute(Var, Value, Body, NBody).

substitute(_, _, X, X).

substitute_in_cond(Var, Value, Cond, NCond) :-
    Cond =.. [Functor|Args],
    maplist(substitute_arg(Var, Value), Args, NArgs),
    NCond =.. [Functor|NArgs].

substitute_arg(Var, Value, Var, Value) :- !.
substitute_arg(_, _, X, X).
