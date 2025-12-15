% ========================================
% LINGUISTIC MODULE: VOCABULARY
% Lambda-based Lexical Semantics with Type System
% ========================================

:- module(vocabulary, [
    word_semantics/3,
    semantic_type/2,
    type_check/2
]).

% ========================================
% TYPE SYSTEM
% e: entities, t: truth values
% e->t: properties, e->e->t: relations
% (e->t)->t: generalized quantifiers
% ========================================

semantic_type(entity, e).
semantic_type(property, e->t).
semantic_type(relation, e->e->t).
semantic_type(quantifier, (e->t)->t).
semantic_type(verb_phrase, e->t).
semantic_type(sentence, t).

% ========================================
% PROPER NOUNS - Type: (e->t)->t
% Semantics: λP.P(entity)
% ========================================

% --- Người ---
word_semantics(nhan, noun_proper, 
    lambda(p, app(p, const(nhan)))).

word_semantics(linh, noun_proper,
    lambda(p, app(p, const(linh)))).

word_semantics(bo_nhan, noun_proper,
    lambda(p, app(p, const(bo_nhan)))).

word_semantics(bo, noun_proper,
    lambda(p, app(p, const(bo_nhan)))).

% --- Động vật ---
word_semantics(miu, noun_proper,
    lambda(p, app(p, const(miu)))).

word_semantics(meo, noun_proper,
    lambda(p, app(p, const(miu)))).  % meo -> miu

% --- Đồ vật ---
word_semantics(xe_dap, noun_proper,
    lambda(p, app(p, const(xe_dap)))).

word_semantics(xe, noun_proper,
    lambda(p, app(p, const(xe_dap)))).

word_semantics(ghe_go, noun_proper,
    lambda(p, app(p, const(ghe_go)))).

word_semantics(ghe, noun_proper,
    lambda(p, app(p, const(ghe_go)))).

% --- Địa điểm ---
word_semantics(nha, noun_proper,
    lambda(p, app(p, const(nha)))).

word_semantics(vuon, noun_proper,
    lambda(p, app(p, const(vuon)))).

word_semantics(truong, noun_proper,
    lambda(p, app(p, const(truong)))).

word_semantics(phong_khach, noun_proper,
    lambda(p, app(p, const(phong_khach)))).

% --- Thực vật ---
word_semantics(hoa, noun_proper,
    lambda(p, app(p, const(hoa)))).

% ========================================
% COMMON NOUNS - Type: e->t
% Semantics: λx.property(x)
% ========================================

word_semantics(dong_vat, noun_common,
    lambda(x, pred(animal, [x]))).

word_semantics(nguoi, noun_common,
    lambda(x, pred(person, [x]))).

word_semantics(thu_cung, noun_common,
    lambda(x, pred(pet, [x]))).

% ========================================
% INTRANSITIVE VERBS - Type: e->t
% Semantics: λx.property(x)
% ========================================

word_semantics(hien, verb_intrans,
    lambda(x, pred(gentle, [x]))).

word_semantics(de_thuong, verb_intrans,
    lambda(x, pred(cute, [x]))).

word_semantics(ngu, verb_intrans,
    lambda(x, pred(sleep, [x]))).

word_semantics(chay, verb_intrans,
    lambda(x, pred(run, [x]))).

word_semantics(lon, verb_intrans,
    lambda(x, pred(big, [x]))).

word_semantics(nho, verb_intrans,
    lambda(x, pred(small, [x]))).

% ========================================
% TRANSITIVE VERBS - Type: e->e->t
% Semantics: λy.λx.relation(x,y)
% ========================================

% --- Động từ có sẵn ---
word_semantics(dat, verb_trans,
    lambda(y, lambda(x, pred(walk, [x, y])))).

word_semantics(yeu, verb_trans,
    lambda(y, lambda(x, pred(love, [x, y])))).

word_semantics(an, verb_trans,
    lambda(y, lambda(x, pred(eat, [x, y])))).

word_semantics(thuoc, verb_trans,
    lambda(y, lambda(x, pred(belong, [x, y])))).

% --- Động từ mới cho data Nhân/Linh ---
word_semantics(cho_an, verb_trans,
    lambda(y, lambda(x, pred(cho_an, [x, y])))).

word_semantics(thich, verb_trans,
    lambda(y, lambda(x, pred(thich, [x, y])))).

word_semantics(so_huu, verb_trans,
    lambda(y, lambda(x, pred(so_huu, [x, y])))).

word_semantics(co, verb_trans,
    lambda(y, lambda(x, pred(so_huu, [x, y])))).

word_semantics(choi_voi, verb_trans,
    lambda(y, lambda(x, pred(choi_voi, [x, y])))).

word_semantics(choi, verb_trans,
    lambda(y, lambda(x, pred(choi_voi, [x, y])))).

word_semantics(song_cung, verb_trans,
    lambda(y, lambda(x, pred(song_cung, [x, y])))).

word_semantics(song_voi, verb_trans,
    lambda(y, lambda(x, pred(song_cung, [x, y])))).

word_semantics(ten, verb_trans,
    lambda(y, lambda(x, pred(ten, [x, y])))).

word_semantics(ngam, verb_trans,
    lambda(y, lambda(x, pred(ngam, [x, y])))).

word_semantics(tang, verb_trans,
    lambda(y, lambda(x, pred(tang, [x, y])))).

word_semantics(cho, verb_trans,
    lambda(y, lambda(x, pred(cho, [x, y])))).

% ========================================
% ADJECTIVES - Type: (e->t)->(e->t)
% Semantics: λP.λx.(P(x) ∧ property(x))
% ========================================

word_semantics(lon, adjective,
    lambda(p, lambda(x, 
        conj(app(p, x), pred(big, [x]))))).

word_semantics(nho, adjective,
    lambda(p, lambda(x,
        conj(app(p, x), pred(small, [x]))))).

word_semantics(dep, adjective,
    lambda(p, lambda(x,
        conj(app(p, x), pred(beautiful, [x]))))).

% ========================================
% DETERMINERS - Type: (e->t)->((e->t)->t)
% ========================================

word_semantics(mot, determiner,
    lambda(n, lambda(vp, 
        exists(x, conj(app(n, x), app(vp, x)))))).

word_semantics(moi, determiner,
    lambda(n, lambda(vp,
        forall(x, impl(app(n, x), app(vp, x)))))).

word_semantics(khong, determiner,
    lambda(n, lambda(vp,
        neg(exists(x, conj(app(n, x), app(vp, x))))))).

% ========================================
% WH-WORDS - Special semantics
% ========================================

word_semantics(ai, wh_word,
    lambda(p, wh_question(who, p))).

word_semantics(gi, wh_word,
    lambda(p, wh_question(what, p))).

word_semantics(dau, wh_word,
    lambda(p, wh_question(where, p))).

word_semantics(nao, wh_word,
    lambda(p, wh_question(which, p))).

% ========================================
% PREPOSITIONS - Type: e->(e->t)->t
% ========================================

word_semantics(o, preposition,
    lambda(loc, lambda(subj,
        pred(at, [subj, loc])))).

word_semantics(cua, preposition,
    lambda(owner, lambda(thing,
        pred(of, [thing, owner])))).

% ========================================
% PROPERTIES (for complex NPs)
% ========================================

word_semantics(mau, property_noun,
    lambda(obj, lambda(col,
        pred(color, [obj, col])))).

word_semantics(long, body_part,
    const(fur)).

word_semantics(mat, body_part,
    const(eye)).

word_semantics(duoi, body_part,
    const(tail)).

% ========================================
% COLORS
% ========================================

word_semantics(den, color, const(black)).
word_semantics(trang, color, const(white)).
word_semantics(nau, color, const(brown)).
word_semantics(xanh, color, const(blue)).
word_semantics(do, color, const(red)).
word_semantics(vang, color, const(yellow)).

% ========================================
% QUESTION MARKERS
% ========================================

word_semantics(khong, question_marker, question).
word_semantics(ko, question_marker, question).
word_semantics('?', question_marker, question).

% ========================================
% TYPE CHECKING
% ========================================

type_check(const(_), e).
type_check(pred(_, _), t).
type_check(lambda(_, Body), Type) :-
    type_check(Body, BodyType),
    Type = _->BodyType.
type_check(app(Fun, Arg), ResultType) :-
    type_check(Fun, ArgType->ResultType),
    type_check(Arg, ArgType).
type_check(conj(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(disj(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(neg(A), t) :-
    type_check(A, t).
type_check(exists(_, Body), t) :-
    type_check(Body, t).
type_check(forall(_, Body), t) :-
    type_check(Body, t).

% ========================================
% LOOKUP UTILITIES
% ========================================

lookup_word(Word, Category, Semantics) :-
    word_semantics(Word, Category, Semantics), !.

lookup_word(Word, unknown, const(Word)) :-
    format('Warning: Unknown word ~w~n', [Word]).
