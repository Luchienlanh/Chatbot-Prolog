% ========================================
% LINGUISTIC MODULE: VOCABULARY
% Lambda-based Lexical Semantics - THEO SLIDES MÔN HỌC
% ========================================
%
% Format biểu thức Lambda theo Slide-BUOI-08, 09, 11:
%
% 1. Danh từ riêng:     λP. P@const        (ví dụ: λP. P@nam)
% 2. Danh từ chung:     λX. danh_từ(X)
% 3. Động từ nội động:  λX. động_từ(X)
% 4. Động từ ngoại động: λP. λX. P@(λY. động_từ(X,Y))
% 5. Động từ 3 ngôi:    λP. λQ. λX. (Q@(λY. P@(λZ. động_từ(X,Z,Y))))
% 6. Định từ "một":     λP. λQ. exists(X, P@X ∧ Q@X)
% 7. Định từ "mọi":     λP. λQ. forall(X, P@X → Q@X)
% 8. Đại từ:            λP. X{X},P@X (cần xác định sở chỉ)
%
% Notation trong Prolog:
%   lambda(Var, Body)  = λVar. Body
%   app(F, A)          = F@A  
%   pred(Name, Args)   = vị từ với các tham số
%   const(E)           = hằng thực thể E
%   exists(X, Body)    = ∃X. Body
%   forall(X, Body)    = ∀X. Body
%   conj(A, B)         = A ∧ B
%   impl(A, B)         = A → B
%
% ========================================

:- encoding(utf8).

:- module(vocabulary, [
    word_semantics/3,
    semantic_type/2,
    type_check/2
]).

% ========================================
% 1. DANH TỪ RIÊNG (PROPER NOUNS)
% Kiểu: (e→t)→t
% Format: λP. P@const
% ========================================

% Người
word_semantics(nhan, noun_proper, lambda(p, app(p, const(nhan)))).
word_semantics(linh, noun_proper, lambda(p, app(p, const(linh)))).
word_semantics(bo_nhan, noun_proper, lambda(p, app(p, const(bo_nhan)))).
word_semantics(bo, noun_proper, lambda(p, app(p, const(bo_nhan)))).

% Động vật
word_semantics(miu, noun_proper, lambda(p, app(p, const(miu)))).
word_semantics(meo, noun_proper, lambda(p, app(p, const(miu)))).

% Đồ vật
word_semantics(xe_dap, noun_proper, lambda(p, app(p, const(xe_dap)))).
word_semantics(xe, noun_proper, lambda(p, app(p, const(xe_dap)))).
word_semantics(ghe_go, noun_proper, lambda(p, app(p, const(ghe_go)))).
word_semantics(ghe, noun_proper, lambda(p, app(p, const(ghe_go)))).

% Địa điểm
word_semantics(nha, noun_proper, lambda(p, app(p, const(nha)))).
word_semantics(nha_nho, noun_proper, lambda(p, app(p, const(nha_nho)))).
word_semantics(phong_khach, noun_proper, lambda(p, app(p, const(phong_khach)))).
word_semantics(vuon, noun_proper, lambda(p, app(p, const(vuon)))).
word_semantics(khu_vuon, noun_proper, lambda(p, app(p, const(vuon)))).
word_semantics(truong, noun_proper, lambda(p, app(p, const(truong)))).
word_semantics(ngoai_o, noun_proper, lambda(p, app(p, const(ngoai_o)))).
word_semantics(sau_nha, noun_proper, lambda(p, app(p, const(sau_nha)))).

% Thực vật
word_semantics(hoa, noun_proper, lambda(p, app(p, const(hoa)))).

% Màu sắc (dùng như danh từ)
word_semantics(xanh, noun_proper, lambda(p, app(p, const(xanh)))).

% ========================================
% 2. DANH TỪ CHUNG (COMMON NOUNS)
% Kiểu: e→t
% Format: λX. danh_từ(X)
% ========================================

word_semantics(nguoi, noun_common, lambda(x, pred(nguoi, [x]))).
word_semantics(dong_vat, noun_common, lambda(x, pred(dong_vat, [x]))).
word_semantics(con_meo, noun_common, lambda(x, pred(meo, [x]))).
word_semantics(thu_cung, noun_common, lambda(x, pred(thu_cung, [x]))).
word_semantics(mon_qua, noun_common, lambda(x, pred(qua_tang, [x]))).
word_semantics(phuong_tien, noun_common, lambda(x, pred(phuong_tien, [x]))).
word_semantics(noi, noun_common, lambda(x, pred(dia_diem, [x]))).
word_semantics(do_vat, noun_common, lambda(x, pred(do_vat, [x]))).
word_semantics(xe, noun_common, lambda(x, pred(xe, [x]))).
word_semantics(ten, noun_common, lambda(x, pred(ten, [x]))).
word_semantics(bong, noun_common, lambda(x, pred(bong, [x]))).

% Relationship nouns
word_semantics(em_gai, noun_relation, lambda(p, app(p, const(em_gai)))).

% ========================================
% CLASSIFIERS (Loại từ)
% Kiểu: transparent (không ảnh hưởng ngữ nghĩa)
% ========================================

word_semantics(chiec, classifier, lambda(x, x)).
word_semantics(con, classifier, lambda(x, x)).
word_semantics(cai, classifier, lambda(x, x)).

% ========================================
% 3. ĐỘNG TỪ NỘI ĐỘNG (INTRANSITIVE VERBS)
% Kiểu: e→t
% Format: λX. động_từ(X)
% ========================================

word_semantics(ngu, verb_intrans, lambda(x, pred(ngu, [x]))).
word_semantics(nam_ngu, verb_intrans, lambda(x, pred(nam_ngu, [x]))).
word_semantics(chay, verb_intrans, lambda(x, pred(chay, [x]))).
word_semantics(di, verb_intrans, lambda(x, pred(di, [x]))).
word_semantics(cuoi, verb_intrans, lambda(x, pred(cuoi, [x]))).
word_semantics(dung, verb_intrans, lambda(x, pred(dung, [x]))).
word_semantics(de_thuong, verb_intrans, lambda(x, pred(de_thuong, [x]))).
word_semantics(hien, verb_intrans, lambda(x, pred(hien, [x]))).
word_semantics(la_meo, verb_intrans, lambda(x, pred(la_meo, [x]))).

% ========================================
% 4. ĐỘNG TỪ NGOẠI ĐỘNG (TRANSITIVE VERBS)
% Kiểu: ((e→t)→t) → (e→t)
% Format theo Slide-BUOI-09: λP. λX. P@(λY. động_từ(X,Y))
%
% Giải thích:
% - P là NP (object), có kiểu (e→t)→t
% - X là subject (entity)
% - Khi áp dụng P vào λY.động_từ(X,Y), ta được động_từ(X, object)
% ========================================

word_semantics(thich, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(thich, [x, y])))))).

word_semantics(so_huu, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(so_huu, [x, y])))))).

word_semantics(co, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(so_huu, [x, y])))))).

word_semantics(cho_an, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(cho_an, [x, y])))))).

word_semantics(choi_voi, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(choi_voi, [x, y])))))).

word_semantics(song_tai, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(song_tai, [x, y])))))).

word_semantics(choi, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(choi_voi, [x, y])))))).

word_semantics(song_cung, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(song_cung, [x, y])))))).

word_semantics(song_voi, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(song_cung, [x, y])))))).

word_semantics(ngam, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(ngam, [x, y])))))).

word_semantics(quen, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(quen, [x, y])))))).

word_semantics(ten, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(ten, [x, y])))))).

% Động từ "ở" với nghĩa vị trí
word_semantics(o, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, pred(vi_tri, [x, y])))))).

% ========================================
% 5. ĐỘNG TỪ 3 NGÔI (DITRANSITIVE VERBS)
% Kiểu: NP → NP → (e→t)
% Format theo Slide-BUOI-09: λP. λQ. λX. (Q@(λY. P@(λZ. động_từ(X,Z,Y))))
%
% Trong đó:
% - P là direct object (cái gì)
% - Q là indirect object (cho ai)  
% - X là subject
% ========================================

word_semantics(tang, verb_ditrans,
    lambda(p, lambda(q, lambda(x, 
        app(q, lambda(y, app(p, lambda(z, pred(tang, [x, y, z]))))))))).

word_semantics(cho, verb_ditrans,
    lambda(p, lambda(q, lambda(x, 
        app(q, lambda(y, app(p, lambda(z, pred(cho, [x, y, z]))))))))).

word_semantics(gui, verb_ditrans,
    lambda(p, lambda(q, lambda(x, 
        app(q, lambda(y, app(p, lambda(z, pred(gui, [x, y, z]))))))))).

% ========================================
% 6. ĐỊNH TỪ (DETERMINERS)
% ========================================

% "Một" - Existential quantifier
% Kiểu: (e→t) → ((e→t)→t)
% Format theo Slide-BUOI-09: λP. λQ. ∃X. (P@X ∧ Q@X)
word_semantics(mot, determiner,
    lambda(p, lambda(q, 
        exists(x, conj(app(p, x), app(q, x)))))).

% "Mọi", "Mỗi" - Universal quantifier
% Format: λP. λQ. ∀X. (P@X → Q@X)
word_semantics(moi, determiner,
    lambda(p, lambda(q, 
        forall(x, impl(app(p, x), app(q, x)))))).

word_semantics(moi_mot, determiner,
    lambda(p, lambda(q, 
        forall(x, impl(app(p, x), app(q, x)))))).

% "Không" - Negation
% Format: λP. λQ. ¬∃X. (P@X ∧ Q@X)
word_semantics(khong_co, determiner,
    lambda(p, lambda(q, 
        neg(exists(x, conj(app(p, x), app(q, x))))))).

% ========================================
% 7. ĐẠI TỪ (PRONOUNS)
% Format theo Slide-BUOI-11: λP. X{X},{} ⊕ P@X
% (cần xác định sở chỉ cho X)
% ========================================

word_semantics(no, pronoun, 
    lambda(p, pronoun_ref(x, app(p, x), features(third, singular, any)))).

word_semantics(anh_ay, pronoun,
    lambda(p, pronoun_ref(x, app(p, x), features(third, singular, male)))).

word_semantics(co_ay, pronoun,
    lambda(p, pronoun_ref(x, app(p, x), features(third, singular, female)))).

word_semantics(ho, pronoun,
    lambda(p, pronoun_ref(x, app(p, x), features(third, plural, any)))).

% ========================================
% 8. CÁC TỪ ĐẶC BIỆT
% ========================================

% Từ "là", "thì", "ở" - identity/copula
% Theo Slide-BUOI-09: λP. P (bỏ qua về mặt ngữ nghĩa)
word_semantics(la, copula, lambda(p, p)).
word_semantics(thi, copula, lambda(p, p)).

% ========================================
% 9. TÍNH TỪ (ADJECTIVES)
% Kiểu: (e→t) → (e→t)
% Format: λP. λX. (P@X ∧ tính_từ(X))
% ========================================

word_semantics(nho, adjective,
    lambda(p, lambda(x, conj(app(p, x), pred(nho, [x]))))).

word_semantics(lon, adjective,
    lambda(p, lambda(x, conj(app(p, x), pred(lon, [x]))))).

word_semantics(dep, adjective,
    lambda(p, lambda(x, conj(app(p, x), pred(dep, [x]))))).

word_semantics(xanh_adj, adjective,
    lambda(p, lambda(x, conj(app(p, x), pred(mau_xanh, [x]))))).

% ========================================
% 10. GIỚI TỪ (PREPOSITIONS)
% Kiểu: NP → (e→t) → (e→t)
% Format theo Slide-BUOI-09: λP. λX. P@(λY. giới_từ(X,Y))
% ========================================

word_semantics(tren, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(tren, [x, y])))))).

word_semantics(trong, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(trong, [x, y])))))).

word_semantics(sau, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(sau, [x, y])))))).

word_semantics(truoc, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(truoc, [x, y])))))).

word_semantics(cua, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(cua, [x, y])))))).

word_semantics(tai, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(tai, [x, y])))))).

word_semantics(o, preposition,
    lambda(p, lambda(x, app(p, lambda(y, pred(vi_tri, [x, y])))))).

% ========================================
% 11. TỪ ĐỂ HỎI (WH-WORDS)
% Kiểu đặc biệt cho câu hỏi
% ========================================

% "Ai" - Who question
word_semantics(ai, wh_word, wh(who)).

% "Gì" - What question  
word_semantics(gi, wh_word, wh(what)).

% "Đâu", "Ở đâu" - Where question
word_semantics(dau, wh_word, wh(where)).
word_semantics(o_dau, wh_word, wh(where)).

% "Nào" - Which question
word_semantics(nao, wh_word, wh(which)).

% ========================================
% 12. QUESTION MARKERS
% ========================================

word_semantics(khong, question_marker, question).
word_semantics(ko, question_marker, question).
word_semantics(phai_khong, question_marker, question).
word_semantics(phai, question_marker, question).
word_semantics(chua, question_marker, question).

% ========================================
% 13. THUỘC TÍNH ĐẶC BIỆT
% ========================================

word_semantics(mau, attribute, attr(mau_sac)).
word_semantics(ten_la, attribute, attr(ten)).

% ========================================
% SEMANTIC TYPES
% Hệ thống kiểu ngữ nghĩa theo Montague
% ========================================

% e - entity type
% t - truth value type
% e→t - property type (từ entity đến truth)
% (e→t)→t - generalized quantifier type (cho NP)
% (e→t)→(e→t) - modifier type

semantic_type(noun_proper, '(e->t)->t').
semantic_type(noun_common, 'e->t').
semantic_type(verb_intrans, 'e->t').
semantic_type(verb_trans, '((e->t)->t)->(e->t)').
semantic_type(verb_ditrans, '((e->t)->t)->((e->t)->t)->(e->t)').
semantic_type(determiner, '(e->t)->((e->t)->t)').
semantic_type(adjective, '(e->t)->(e->t)').
semantic_type(preposition, '((e->t)->t)->(e->t)->(e->t)').
semantic_type(wh_word, 'special').

% ========================================
% TYPE CHECKING
% Kiểm tra biểu thức hợp lệ
% ========================================

type_check(const(_), e).
type_check(pred(_, _), t).
type_check(lambda(_, Body), _) :-
    type_check(Body, _).
type_check(app(F, A), _) :-
    type_check(F, _),
    type_check(A, _).
type_check(exists(_, Body), t) :-
    type_check(Body, t).
type_check(forall(_, Body), t) :-
    type_check(Body, t).
type_check(conj(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(impl(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(neg(A), t) :-
    type_check(A, t).
