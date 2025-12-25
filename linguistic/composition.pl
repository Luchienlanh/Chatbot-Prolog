% ========================================
% LINGUISTIC MODULE: COMPOSITION
% Lambda Calculus & Semantic Composition
% Mở rộng cho 22 test cases
% ========================================

:- module(composition, [
    compose/2,
    beta_reduce/2,
    beta_reduce_full/2,
    substitute/4
]).

:- use_module('vocabulary').

:- discontiguous compose/2.

% ========================================
% COMPOSE - TÍNH TOÁN NGỮ NGHĨA TỪ CÂY CÚ PHÁP
% ========================================

% -------------------------------------------
% CÂU ĐƠN (S -> NP VP)
% ||S|| = ||NP||@||VP||
% -------------------------------------------

compose(tree(s, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% TEXT - ĐA CÂU (cho đồng sở chỉ)
% -------------------------------------------

compose(tree(text, [S1, S2]), Semantics) :- !,
    compose(S1, S1Sem),
    % Extract referents from S1 for coreference
    extract_referents(S1Sem, Referents),
    % Resolve pronouns in S2 using referents
    compose_with_context(S2, Referents, S2Sem),
    Semantics = text_sem(S1Sem, S2Sem).

compose_with_context(tree(sq, [NP, VP, _QM]), Referents, Semantics) :- !,
    compose_np_with_context(NP, Referents, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

compose_with_context(tree(sq, [NP, VP]), Referents, Semantics) :- !,
    compose_np_with_context(NP, Referents, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

compose_np_with_context(tree(np, [tree(pn, [word(no, pronoun)])]), [Ref|_], Sem) :- !,
    Sem = lambda(p, app(p, const(Ref))).
compose_np_with_context(tree(np, [tree(pn, [word(do, pronoun)])]), [Ref|_], Sem) :- !,
    Sem = lambda(p, app(p, const(Ref))).
compose_np_with_context(NP, _, Sem) :-
    compose(NP, Sem).

extract_referents(pred(so_huu, [_, Object]), [Object]) :- !.
extract_referents(pred(_, [Subject|_]), [Subject]) :- !.
extract_referents(_, []).

% -------------------------------------------
% SBARQ - Câu hỏi WH
% -------------------------------------------

% SBARQ với WP ở subject cho "là" (Ai là em gái của Nhân?)
% Must be before general WHO rule
compose(tree(sbarq, [tree(wp, _), VP]), Semantics) :-
    VP = tree(vp_copula, [_, NP, PP]),
    extract_entity_from_np(NP, Relation),
    extract_entity_from_pp(PP, Target), !,
    Semantics = wh_question(who, pred(Relation, [_, Target])).

% SBARQ với WP ở subject (Ai thích hoa?)
compose(tree(sbarq, [NP, VP]), Semantics) :-
    contains_wh(NP, who), !,
    extract_predicate_from_vp(VP, Pred),
    extract_object_from_vp(VP, Object),
    Semantics = wh_question(who, pred(Pred, [_, Object])).

% SBARQ với WP ở object (Linh thích gì?)
compose(tree(sbarq, [NP, VP]), Semantics) :-
    contains_wh(VP, what), !,
    compose(NP, NPSem),
    extract_entity(NPSem, Subject),
    extract_predicate_from_vp(VP, Pred),
    Semantics = wh_question(what, pred(Pred, [Subject, _])).

% SBARQ với "là gì của" (Linh là gì của Nhân?)
compose(tree(sbarq, [NP, VP]), Semantics) :-
    VP = tree(vp_copula, [_, tree(wp, _), PP]), !,
    compose(NP, NPSem),
    extract_entity(NPSem, Subject),
    extract_entity_from_pp(PP, Target),
    Semantics = wh_question(what_relation, relation(Subject, _, Target)).

% SBARQ với "là của ai" (Xe đạp là của ai?)
compose(tree(sbarq, [NP, VP]), Semantics) :-
    VP = tree(vp_copula, [_, tree(pp, [_, tree(np, [tree(wp, _)])])]), !,
    compose(NP, NPSem),
    extract_entity(NPSem, Object),
    Semantics = wh_question(who, pred(so_huu, [_, Object])).

% SBARQ với WRB (Miu ở đâu?)
compose(tree(sbarq, [NP, VP]), Semantics) :-
    contains_wh(VP, where), !,
    compose(NP, NPSem),
    extract_entity(NPSem, Subject),
    Semantics = wh_question(where, Subject).

% SBARQ với VP chứa WRB (Khu vườn ở đâu?)
compose(tree(sbarq, [NP, tree(vp, [_, tree(np, [tree(wrb, _)])])]), Semantics) :- !,
    compose(NP, NPSem),
    extract_entity(NPSem, Subject),
    Semantics = wh_question(where, Subject).

% SBARQ mặc định
compose(tree(sbarq, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% SQ - Câu hỏi Yes/No
% -------------------------------------------

compose(tree(sq, [NP, VP, _QM]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% -------------------------------------------
% NP - DANH NGỮ
% -------------------------------------------

% NP -> NNP | NN | WP | WRB | PN
compose(tree(np, [Child]), Semantics) :- !,
    compose(Child, Semantics).

% NP -> DT NN
compose(tree(np, [DT, NN]), Semantics) :- !,
    compose(DT, DTSem),
    compose(NN, NNSem),
    RawSem = app(DTSem, NNSem),
    beta_reduce_full(RawSem, Semantics).

% NP -> DT CL NN (Một con mèo)
compose(tree(np, [DT, _CL, NN]), Semantics) :- !,
    compose(DT, DTSem),
    compose(NN, NNSem),
    RawSem = app(DTSem, NNSem),
    beta_reduce_full(RawSem, Semantics).

% NP -> DT CL NN ADJ (Một chiếc xe màu xanh)
compose(tree(np, [DT, _CL, NN, ADJ]), Semantics) :- !,
    compose(DT, DTSem),
    compose(NN, NNSem),
    compose(ADJ, ADJSem),
    % ADJ modifies NN
    ModifiedN = app(ADJSem, NNSem),
    beta_reduce_full(ModifiedN, ModN),
    RawSem = app(DTSem, ModN),
    beta_reduce_full(RawSem, Semantics).

% NP_POSS -> Tên nó
compose(tree(np_poss, [_, PN]), Semantics) :- !,
    compose(PN, PNSem),
    extract_entity(PNSem, Entity),
    Semantics = possession(ten, Entity).

% NP_REL -> Người cho Miu ăn (relative clause)
compose(tree(np_rel, [_, VP]), Semantics) :- !,
    compose(VP, VPSem),
    Semantics = relative(nguoi, VPSem).

% -------------------------------------------
% VP - ĐỘNG NGỮ
% -------------------------------------------

% VP -> VB (Intransitive)
compose(tree(vp, [VB]), Semantics) :- !,
    compose(VB, Semantics).

% VP -> VB PP (ngủ trong phòng khách) -> vi_tri(x, location)
% Must be before VB NP to match correctly
compose(tree(vp, [_VB, PP]), Semantics) :-
    PP = tree(pp, _), !,
    compose(PP, PPSem),
    extract_location_from_pp(PPSem, Location),
    Semantics = lambda(x, pred(vi_tri, [x, Location])).

% VP -> VB NP (Transitive)
compose(tree(vp, [VB, NP]), Semantics) :-
    NP = tree(np, _), !,
    compose(VB, VBSem),
    compose(NP, NPSem),
    RawSem = app(VBSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% VP -> VB NP NP (Ditransitive)
compose(tree(vp, [VB, NP1, NP2]), Semantics) :- !,
    compose(VB, VBSem),
    compose(NP1, NP1Sem),
    compose(NP2, NP2Sem),
    extract_entity(NP1Sem, Entity1),
    extract_entity(NP2Sem, Entity2),
    extract_predicate(VBSem, Pred),
    Semantics = pred(Pred, [_, Entity1, Entity2]).

% VP_SERIAL -> VB VB NP (thích ngắm hoa)
compose(tree(vp_serial, [VB1, VB2, NP]), Semantics) :- !,
    compose(VB2, VB2Sem),
    compose(NP, NPSem),
    % First compute VB2 @ NP
    InnerSem = app(VB2Sem, NPSem),
    beta_reduce_full(InnerSem, InnerReduced),
    % Then VB1 @ (VB2 @ NP)
    compose(VB1, VB1Sem),
    RawSem = app(VB1Sem, lambda(p, app(p, InnerReduced))),
    beta_reduce_full(RawSem, Semantics).

% VP_COPULA -> VB-copula NP (là em gái)
compose(tree(vp_copula, [_VB, NP]), Semantics) :- !,
    compose(NP, NPSem),
    Semantics = NPSem.

% VP_COPULA -> VB-copula PP (là của Linh)
compose(tree(vp_copula, [_VB, PP]), Semantics) :- !,
    compose(PP, PPSem),
    Semantics = PPSem.

% VP_COPULA -> VB-copula NP PP (là em gái của Nhân)
compose(tree(vp_copula, [_VB, NP, PP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(PP, PPSem),
    extract_entity(NPSem, Relation),
    extract_entity_from_pp(PP, Target),
    Semantics = lambda(x, pred(Relation, [x, Target])).

% VP_COPULA -> VB-copula WP PP (là gì của Nhân)
compose(tree(vp_copula, [_VB, tree(wp, _), PP]), Semantics) :- !,
    extract_entity_from_pp(PP, Target),
    Semantics = wh_relation(Target).

% -------------------------------------------
% PP - PREPOSITIONAL PHRASE
% -------------------------------------------

compose(tree(pp, [P, NP]), Semantics) :- !,
    compose(P, PSem),
    compose(NP, NPSem),
    extract_entity(NPSem, Entity),
    extract_prep(PSem, Prep),
    Semantics = pp_sem(Prep, Entity).

% -------------------------------------------
% ADJ PHRASE
% -------------------------------------------

compose(tree(adjp, [ADJ]), Semantics) :- !,
    compose(ADJ, Semantics).

compose(tree(adjp, [_, ADJ]), Semantics) :- !,
    compose(ADJ, Semantics).

% -------------------------------------------
% POS Tags - Terminal nodes
% -------------------------------------------

compose(tree(nnp, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(nn, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(vb, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(wp, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(wrb, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(dt, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(cl, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(pn, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(p, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(adj, [Word]), Semantics) :- !,
    compose(Word, Semantics).

compose(tree(qm, [_]), question) :- !.

% -------------------------------------------
% WORD - Tra từ điển
% -------------------------------------------

compose(word(Word, Category), Semantics) :-
    vocabulary:word_semantics(Word, Category, Semantics), !.

compose(word(Word, _), const(Word)) :- !.

compose(const(X), const(X)) :- !.
compose(X, X) :- atomic(X), !.

% ========================================
% HELPERS
% ========================================

contains_wh(tree(np, [tree(wp, _)]), who) :- !.
contains_wh(tree(np, [tree(wp, [word(gi, _)])]), what) :- !.
contains_wh(tree(np, [tree(wrb, _)]), where) :- !.
contains_wh(tree(vp, [_, NP]), Type) :- !, contains_wh(NP, Type).
contains_wh(tree(vp, [_, NP, _]), Type) :- !, contains_wh(NP, Type).
contains_wh(tree(vp_copula, [_, _, tree(pp, [_, tree(wp, _)])]), who) :- !.

extract_entity(const(E), E) :- !.
extract_entity(lambda(_, app(_, const(E))), E) :- !.
extract_entity(app(_, const(E)), E) :- !.
extract_entity(E, E) :- atomic(E), !.
extract_entity(_, unknown).

extract_entity_from_np(tree(np, [tree(nnp, [word(W, _)])]), W) :- !.
extract_entity_from_np(tree(np, [tree(nn, [word(W, _)])]), W) :- !.
extract_entity_from_np(_, unknown).

extract_entity_from_pp(tree(pp, [_, NP]), Entity) :- !,
    extract_entity_from_np(NP, Entity).
extract_entity_from_pp(pp_sem(_, Entity), Entity) :- !.

extract_predicate_from_vp(tree(vp, [VB, _]), Pred) :- !,
    VB = tree(vb, [word(W, _)]),
    word_to_pred(W, Pred).
extract_predicate_from_vp(tree(vp, [VB]), Pred) :- !,
    VB = tree(vb, [word(W, _)]),
    word_to_pred(W, Pred).
extract_predicate_from_vp(tree(vp, [VB, _, _]), Pred) :- !,
    VB = tree(vb, [word(W, _)]),
    word_to_pred(W, Pred).
extract_predicate_from_vp(tree(vp_serial, [_, VB2, _]), Pred) :- !,
    VB2 = tree(vb, [word(W, _)]),
    word_to_pred(W, Pred).
extract_predicate_from_vp(_, unknown).

extract_object_from_vp(tree(vp, [_, NP]), Object) :- !,
    extract_entity_from_np(NP, Object).
extract_object_from_vp(tree(vp, [_, NP, _]), Object) :- !,
    extract_entity_from_np(NP, Object).
extract_object_from_vp(tree(vp_serial, [_, _, NP]), Object) :- !,
    extract_entity_from_np(NP, Object).
extract_object_from_vp(_, unknown).

extract_location_from_pp(pp_sem(_, Location), Location) :- !.
extract_location_from_pp(_, unknown).

extract_predicate(lambda(_, lambda(_, app(_, lambda(_, pred(P, _))))), P) :- !.
extract_predicate(lambda(_, pred(P, _)), P) :- !.
extract_predicate(pred(P, _), P) :- !.
extract_predicate(_, unknown).

extract_prep(lambda(_, lambda(_, app(_, lambda(_, pred(P, _))))), P) :- !.
extract_prep(pred(P, _), P) :- !.
extract_prep(const(P), P) :- !.
extract_prep(_, unknown).

word_to_pred(thich, thich).
word_to_pred(so_huu, so_huu).
word_to_pred(co, so_huu).
word_to_pred(cho_an, cho_an).
word_to_pred(o, vi_tri).
word_to_pred(song_cung, song_cung).
word_to_pred(song, song_tai).
word_to_pred(song_tai, song_tai).
word_to_pred(ngam, ngam).
word_to_pred(choi, choi_voi).
word_to_pred(choi_voi, choi_voi).
word_to_pred(ngu, vi_tri).
word_to_pred(nam_ngu, vi_tri).
word_to_pred(tang, tang).
word_to_pred(W, W).

% ========================================
% BETA REDUCTION
% ========================================

beta_reduce(app(lambda(Var, Body), Arg), Result) :- !,
    substitute(Var, Arg, Body, Result).

beta_reduce(lambda(Var, Body), lambda(Var, RBody)) :- !,
    beta_reduce(Body, RBody).

beta_reduce(app(F, A), app(RF, A)) :-
    beta_reduce(F, RF),
    RF \= F, !.

beta_reduce(app(F, A), app(F, RA)) :-
    beta_reduce(A, RA),
    RA \= A, !.

beta_reduce(X, X).

beta_reduce_full(Term, Result) :-
    beta_reduce(Term, Reduced),
    ( Reduced == Term ->
        Result = Term
    ;
        beta_reduce_full(Reduced, Result)
    ).

% ========================================
% SUBSTITUTION
% ========================================

substitute(Var, Value, Var, Value) :- !.
substitute(Var, _, lambda(Var, Body), lambda(Var, Body)) :- !.
substitute(Var, Value, lambda(V, Body), lambda(V, NewBody)) :- !,
    substitute(Var, Value, Body, NewBody).
substitute(Var, Value, app(F, A), app(NF, NA)) :- !,
    substitute(Var, Value, F, NF),
    substitute(Var, Value, A, NA).
substitute(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(sub_arg(Var, Value), Args, NewArgs).
substitute(_Var, _Value, const(X), const(X)) :- !.
substitute(Var, Value, conj(A, B), conj(NA, NB)) :- !,
    substitute(Var, Value, A, NA),
    substitute(Var, Value, B, NB).
substitute(Var, Value, exists(V, Body), exists(V, NewBody)) :- 
    Var \= V, !,
    substitute(Var, Value, Body, NewBody).
substitute(_, _, X, X).

sub_arg(Var, Value, Var, Value) :- !.
sub_arg(_, _, X, X).

