% ========================================
% LINGUISTIC MODULE: VOCABULARY
% Lambda-based Lexical Semantics - THEO SLIDES MÔN HỌC
% ========================================
%
% Format biểu thức Lambda DRS theo Slide-BUOI-11:
%
% 1. Danh từ riêng (Trang 4):     λP. {danh_từ},{} ⊕ (P@danh_từ)
% 2. Danh từ chung (Trang 6):     λX. {},{danh_từ(X)}
% 3. Tính từ (Trang 7):           λX. {},{tính_từ(X)}
% 4. Động từ nội động (Trang 8):  λX. {},{động_từ(X)}
% 5. Động từ ngoại động (Trang 9): λP. λX. P@ λY. {},{động_từ(X,Y)}
% 6. Động từ 3 ngôi (Trang 10):   λP. λQ. λX. (Q@ (λY. P@ λZ. {},{động_từ(X,Z,Y)}))
% 7. Giới từ (Trang 11):          λP. λX. P@ λY. {},{giới_từ(X,Y)}
% 8. Số từ "một" (Trang 13):      λP. λQ. {X},{} ⊕ P@X ⊕ Q@X
% 9. Định từ "mọi" (Trang 13):    λP. λQ. {},{({X},{} ⊕ P@X) → Q@X}
% 10. Đại từ (Trang 14):          λP. ˣ{X},{} ⊕ (P@X)
% 11. Từ "là", "thì" (Trang 15):  Bỏ qua về mặt ngữ nghĩa
%
% Notation trong Prolog:
%   lambda(Var, Body)     = λVar. Body
%   app(F, A)             = F@A  
%   drs(Refs, Conds)      = {Refs},{Conds}
%   merge(A, B)           = A ⊕ B
%   impl(A, B)            = A → B
%   pronoun_drs(Var, DRS) = ˣDRS (cần xác định sở chỉ cho Var)
%
% ========================================

:- encoding(utf8).

:- module(vocabulary, [
    word_semantics/3,
    semantic_type/2,
    type_check/2
]).

% ========================================
% 1. DANH TỪ RIÊNG (Slide-BUOI-11, Trang 4)
% Format: λP. {danh_từ},{} ⊕ (P@danh_từ)
% ========================================

% Người
word_semantics(nhan, noun_proper, 
    lambda(p, merge(drs([nhan],[]), app(p, nhan)))).
word_semantics(linh, noun_proper, 
    lambda(p, merge(drs([linh],[]), app(p, linh)))).
word_semantics(bo_nhan, noun_proper, 
    lambda(p, merge(drs([bo_nhan],[]), app(p, bo_nhan)))).
word_semantics(bo, noun_proper, 
    lambda(p, merge(drs([bo_nhan],[]), app(p, bo_nhan)))).
word_semantics(nam, noun_proper,
    lambda(p, merge(drs([nam],[]), app(p, nam)))).
word_semantics(binh, noun_proper,
    lambda(p, merge(drs([binh],[]), app(p, binh)))).
word_semantics(ly, noun_proper,
    lambda(p, merge(drs([ly],[]), app(p, ly)))).

% Động vật (pet with name)
word_semantics(miu, noun_proper, 
    lambda(p, merge(drs([miu],[]), app(p, miu)))).

% Địa điểm
word_semantics(truong, noun_proper, 
    lambda(p, merge(drs([truong],[]), app(p, truong)))).

% ========================================
% 2. DANH TỪ CHUNG (Slide-BUOI-11, Trang 6)
% Format: λX. {},{danh_từ(X)}
% ========================================

% Người
word_semantics(nguoi, noun_common, lambda(x, drs([],[nguoi(x)]))).
word_semantics(hoc_sinh, noun_common, lambda(x, drs([],[hoc_sinh(x)]))).
word_semantics(ban, noun_common, lambda(x, drs([],[ban(x)]))).

% Động vật
word_semantics(meo, noun_common, lambda(x, drs([],[meo(x)]))).
word_semantics(con_meo, noun_common, lambda(x, drs([],[meo(x)]))).
word_semantics(cho, noun_common, lambda(x, drs([],[cho(x)]))).
word_semantics(con_cho, noun_common, lambda(x, drs([],[cho(x)]))).

% Đồ vật
word_semantics(xe, noun_common, lambda(x, drs([],[xe(x)]))).
word_semantics(xe_dap, noun_common, lambda(x, drs([],[xe_dap(x)]))).
word_semantics(but, noun_common, lambda(x, drs([],[but(x)]))).
word_semantics(cay_but, noun_common, lambda(x, drs([],[but(x)]))).
word_semantics(vo, noun_common, lambda(x, drs([],[vo(x)]))).
word_semantics(quyen_vo, noun_common, lambda(x, drs([],[vo(x)]))).
word_semantics(sach, noun_common, lambda(x, drs([],[sach(x)]))).
word_semantics(qua, noun_common, lambda(x, drs([],[qua(x)]))).
word_semantics(mon_qua, noun_common, lambda(x, drs([],[qua(x)]))).
word_semantics(ghe, noun_common, lambda(x, drs([],[ghe(x)]))).
word_semantics(chuong, noun_common, lambda(x, drs([],[chuong(x)]))).
word_semantics(gio, noun_common, lambda(x, drs([],[gio(x)]))).

% Thực vật
word_semantics(hoa, noun_common, lambda(x, drs([],[hoa(x)]))).

% Nơi chốn
word_semantics(nha, noun_common, lambda(x, drs([],[nha(x)]))).
word_semantics(vuon, noun_common, lambda(x, drs([],[vuon(x)]))).
word_semantics(phong_khach, noun_common, lambda(x, drs([],[phong_khach(x)]))).
word_semantics(xom, noun_common, lambda(x, drs([],[xom(x)]))).

% ========================================
% 3. DANH TỪ CHỈ QUAN HỆ 2 NGÔI (Slide-BUOI-11, Trang 5)
% Format: λP. λX. P@ λY. {},{quan_hệ(X, Y)}
% Ví dụ: "X là bạn của Y" → bạn(X, Y)
% ========================================

word_semantics(em_gai, noun_relation, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[em_gai(x, y)])))))).
word_semantics(anh, noun_relation,
    lambda(p, lambda(x, app(p, lambda(y, drs([],[anh(x, y)])))))).
word_semantics(cha, noun_relation,
    lambda(p, lambda(x, app(p, lambda(y, drs([],[cha(x, y)])))))).
word_semantics(ban_cua, noun_relation,
    lambda(p, lambda(x, app(p, lambda(y, drs([],[ban(x, y)])))))).
word_semantics(chu, noun_relation,
    lambda(p, lambda(x, app(p, lambda(y, drs([],[chu(x, y)])))))).

% ========================================
% 4. TÍNH TỪ (Slide-BUOI-11, Trang 7)
% Format: λX. {},{tính_từ(X)}
% ========================================

word_semantics(dep, adjective, lambda(x, drs([],[dep(x)]))).
word_semantics(tot, adjective, lambda(x, drs([],[tot(x)]))).
word_semantics(xanh, adjective, lambda(x, drs([],[xanh(x)]))).
word_semantics(nho, adjective, lambda(x, drs([],[nho(x)]))).
word_semantics(lon, adjective, lambda(x, drs([],[lon(x)]))).
word_semantics(cu, adjective, lambda(x, drs([],[cu(x)]))).
word_semantics(moi, adjective, lambda(x, drs([],[moi_adj(x)]))).

% ========================================
% 5. ĐỘNG TỪ NỘI ĐỘNG (Slide-BUOI-11, Trang 8)
% Format: λX. {},{động_từ(X)}
% ========================================

word_semantics(ngu, verb_intrans, lambda(x, drs([],[ngu(x)]))).
word_semantics(dung, verb_intrans, lambda(x, drs([],[dung(x)]))).
word_semantics(cuoi, verb_intrans, lambda(x, drs([],[cuoi(x)]))).
word_semantics(chay, verb_intrans, lambda(x, drs([],[chay(x)]))).
word_semantics(di, verb_intrans, lambda(x, drs([],[di(x)]))).

% ========================================
% 6. ĐỘNG TỪ NGOẠI ĐỘNG (Slide-BUOI-11, Trang 9)
% Format: λP. λX. P@ λY. {},{động_từ(X, Y)}
% ========================================

word_semantics(co, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[co(x, y)])))))).
word_semantics(thich, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[thich(x, y)])))))).
word_semantics(quen, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[quen(x, y)])))))).
word_semantics(cho, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[cho(x, y)])))))).
word_semantics(an, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[an(x, y)])))))).
word_semantics(thay, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[thay(x, y)])))))).
word_semantics(goi, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[goi(x, y)])))))).
word_semantics(can, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[can(x, y)])))))).

% Động từ từ KB
word_semantics(cho_an, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[cho_an(x, y)])))))).
word_semantics(so_huu, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[so_huu(x, y)])))))).
word_semantics(song_cung, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[song_cung(x, y)])))))).
word_semantics(choi_voi, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[choi_voi(x, y)])))))).
word_semantics(ngam, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[ngam(x, y)])))))).
word_semantics(em_gai, verb_trans, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[em_gai(x, y)])))))).

% ========================================
% 7. ĐỘNG TỪ 3 NGÔI (Slide-BUOI-11, Trang 10)
% Format: λP. λQ. λX. (Q@ (λY. P@ λZ. {},{động_từ(X, Z, Y)}))
% ========================================

word_semantics(cho_ditrans, verb_ditrans,
    lambda(p, lambda(q, lambda(x, 
        app(q, lambda(y, app(p, lambda(z, drs([],[cho(x, z, y)]))))))))).
word_semantics(tang, verb_ditrans,
    lambda(p, lambda(q, lambda(x, 
        app(q, lambda(y, app(p, lambda(z, drs([],[tang(x, z, y)]))))))))).

% ========================================
% 8. GIỚI TỪ CHỈ VỊ TRÍ (Slide-BUOI-11, Trang 11)
% Format: λP. λX. P@ λY. {},{giới_từ(X, Y)}
% ========================================

word_semantics(tren, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[tren(x, y)])))))).
word_semantics(duoi, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[duoi(x, y)])))))).
word_semantics(truoc, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[truoc(x, y)])))))).
word_semantics(sau, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[sau(x, y)])))))).
word_semantics(trong, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[trong(x, y)])))))).
word_semantics(cua, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[cua(x, y)])))))).
word_semantics(o, preposition, 
    lambda(p, lambda(x, app(p, lambda(y, drs([],[o(x, y)])))))).

% ========================================
% 9. LIÊN TỪ (Slide-BUOI-11, Trang 12)
% Và: λP. λQ. λX. P@X ⊕ Q@X
% Hoặc: λP. λQ. λX. {},{P@X ∨ Q@X}
% ========================================

word_semantics(va, conjunction, 
    lambda(p, lambda(q, lambda(x, merge(app(p, x), app(q, x)))))).
word_semantics(hoac, conjunction, 
    lambda(p, lambda(q, lambda(x, drs([],[or(app(p, x), app(q, x))]))))).

% ========================================
% 10. SỐ TỪ "MỘT" VÀ ĐỊNH TỪ "MỌI" (Slide-BUOI-11, Trang 13)
% Một: λP. λQ. {X},{} ⊕ P@X ⊕ Q@X
% Mọi: λP. λQ. {},{({X},{} ⊕ P@X) → Q@X}
% ========================================

word_semantics(mot, determiner,
    lambda(p, lambda(q, merge(merge(drs([x],[]), app(p, x)), app(q, x))))).

word_semantics(moi, determiner,
    lambda(p, lambda(q, drs([],[impl(merge(drs([x],[]), app(p, x)), app(q, x))])))).

word_semantics(moi_mot, determiner,
    lambda(p, lambda(q, drs([],[impl(merge(drs([x],[]), app(p, x)), app(q, x))])))).

% ========================================
% 11. ĐẠI TỪ (Slide-BUOI-11, Trang 14)
% Format: λP. ˣ{X},{} ⊕ (P@X)
% Ký hiệu ˣ cho biết cần xác định sở chỉ cho X
% ========================================
% "Nó" - neutral (vật/động vật)
word_semantics(no, pronoun, 
    lambda(p, pronoun_drs(x, neutral, app(p, x)))).

% "Anh" - Identity (User Request)
% Warning: Loses gender info if used alone
word_semantics(anh, pronoun, lambda(p, p)).

% "Cô" - Identity (User Request)
word_semantics(co, pronoun, lambda(p, p)).

% "Ấy" - User Request: lambda P. P@X
% Implementation: lambda P. pronoun_drs(x, _Gender, P@x)
% Uses Unbound Gender to match ANY antecedent (since gender info is lost from "anh"/"co")
word_semantics(ay, determiner, 
    lambda(p, pronoun_drs(x, _Gender, app(p, x)))).

% "Họ" - plural
word_semantics(ho, pronoun,
    lambda(p, pronoun_drs(x, plural, app(p, x)))).

% ========================================
% 12. TỪ "LÀ", "THÌ", "Ở" (Slide-BUOI-11, Trang 15)
% Bỏ qua về mặt ngữ nghĩa - chỉ có chức năng ngữ pháp
% ========================================

word_semantics(la, copula, lambda(p, p)).
word_semantics(thi, copula, lambda(p, p)).

% ========================================
% 13. LOẠI TỪ (CLASSIFIERS)
% Bỏ qua về mặt ngữ nghĩa
% ========================================

word_semantics(con, classifier, lambda(x, x)).
word_semantics(chiec, classifier, lambda(x, x)).
word_semantics(cai, classifier, lambda(x, x)).
word_semantics(cay, classifier, lambda(x, x)).
word_semantics(quyen, classifier, lambda(x, x)).
word_semantics(bong, classifier, lambda(x, x)).

% ========================================
% 14. TỪ NGHI VẤN (WH-WORDS)
% THEO SLIDE-BUOI-11: λP. {X},{} ⊕ P@X
% Query type được cung cấp từ input query(_, Type)
% => KHÔNG cần biến đặc biệt, chỉ cần DRS thuần túy
% ========================================

% "Ai" (Who) - Tạo biến mới để tránh conflict
word_semantics(ai, wh_word, 
    lambda(p, merge(drs([wh_x], []), app(p, wh_x)))).

% "Gì" (What) - Tạo biến mới để tránh conflict  
word_semantics(gi, wh_word, 
    lambda(p, merge(drs([wh_y], []), app(p, wh_y)))).

% "Đâu" (Where) - Tạo biến mới để tránh conflict
word_semantics(dau, wh_word, 
    lambda(p, merge(drs([wh_z], []), app(p, wh_z)))).

% ========================================
% SEMANTIC TYPES
% ========================================

semantic_type(noun_proper, '(e->t)->t').
semantic_type(noun_common, 'e->t').
semantic_type(noun_relation, '((e->t)->t)->(e->t)').
semantic_type(adjective, 'e->t').
semantic_type(verb_intrans, 'e->t').
semantic_type(verb_trans, '((e->t)->t)->(e->t)').
semantic_type(verb_ditrans, '((e->t)->t)->((e->t)->t)->(e->t)').
semantic_type(determiner, '(e->t)->((e->t)->t)').
semantic_type(preposition, '((e->t)->t)->(e->t)->(e->t)').
semantic_type(pronoun, '(e->t)->t').
semantic_type(wh_word, '(e->t)->t').

% ========================================
% TYPE CHECKING
% ========================================

type_check(drs(_, _), t).
type_check(merge(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(impl(A, B), t) :-
    type_check(A, t),
    type_check(B, t).
type_check(lambda(_, Body), _) :-
    type_check(Body, _).
type_check(app(F, A), _) :-
    type_check(F, _),
    type_check(A, _).
type_check(wh_question(_, Body), t) :-
    type_check(Body, _).
type_check(pronoun_drs(_, Body), t) :-
    type_check(Body, t).
type_check(X, e) :- atom(X).

% ========================================
% 15. TỪ PHỦ ĐỊNH (NEGATION)
% Format: λP. λX. ¬(P@X)
% ========================================

word_semantics(khong, adv_neg, 
    lambda(p, lambda(x, neg(app(p, x))))).

semantic_type(adv_neg, '(e->t)->(e->t)').
