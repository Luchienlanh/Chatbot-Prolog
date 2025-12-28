% ========================================
% PARSING MODULE: STRUCTURES
% Văn phạm DCG mở rộng theo Penn Treebank
% Hỗ trợ đầy đủ 22 test cases trong queries_cs229.pl
% ========================================
%
% TẬP LUẬT SẢN SINH MỞ RỘNG:
%
% SENTENCE RULES:
%    SBARQ -> NP VP               (Câu hỏi wh)
%    SQ -> NP VP QM               (Câu hỏi Yes/No)
%    TEXT -> S PUNCT S            (Đa câu với đồng sở chỉ)
%
% PHRASE RULES:
%    NP -> NNP                    (Proper noun)
%    NP -> NN                     (Common noun)
%    NP -> WP                     (Wh-pronoun: ai, gì)
%    NP -> WRB                    (Wh-adverb: đâu)
%    NP -> DT NN                  (Determiner + Noun)
%    NP -> DT CL NN               (Determiner + Classifier + Noun)
%    NP -> DT CL NN ADJ           (Determiner + Classifier + Noun + Adjective)
%    NP -> PN                     (Pronoun: nó, đó)
%    VP -> VB                     (Intransitive)
%    VP -> VB NP                  (Transitive)
%    VP -> VB NP NP               (Ditransitive)
%    VP -> VB PP                  (Verb + PP: ngủ trong phòng)
%    VP -> VB-copula NP           (là em gái)
%    VP -> VB-copula PP           (là của Linh)
%    VP -> VB VB NP               (Serial verb: thích ngắm hoa)
%    PP -> P NP                   (Prepositional phrase)
%
% ========================================

:- module(structures, [
    parse/3,
    parse_text/3
]).

:- use_module('../linguistic/vocabulary').

% ========================================
% PARSE - INTERFACE CHÍNH
% ========================================

parse(Tokens, yn, Tree) :- 
    sq(Tree, Tokens, []), !.

parse(Tokens, who, Tree) :- 
    sbarq(Tree, Tokens, []), !.

parse(Tokens, what, Tree) :- 
    sbarq(Tree, Tokens, []), !.

parse(Tokens, where, Tree) :- 
    sbarq(Tree, Tokens, []), !.

parse(Tokens, statement, Tree) :- 
    s(Tree, Tokens, []), !.

parse(Tokens, auto, Tree) :-
    ( sq(Tree, Tokens, []) -> true
    ; sbarq(Tree, Tokens, []) -> true
    ; s(Tree, Tokens, [])
    ).

% Parse multi-sentence text (for coreference)
parse_text(Tokens, yn, Tree) :-
    text_yn(Tree, Tokens, []), !.

% ========================================
% TEXT - ĐA CÂU (cho đồng sở chỉ)
% TEXT -> S PUNCT S QM
% ========================================

text_yn(tree(text, [S1, S2])) -->
    statement_part(S1),
    punct,
    question_part(S2).

% Statement can be just NP (Một chiếc xe màu xanh.) - Try this first to capture NP_REL
statement_part(tree(np_statement, [NP])) -->
    np(NP).

statement_part(tree(s, [NP, VP])) -->
    np(NP),
    vp(VP).

question_part(tree(sq, [NP, VP, QM])) -->
    np(NP),
    vp(VP),
    qm(QM).

question_part(tree(sq, [NP, VP])) -->
    np(NP),
    vp(VP),
    [?].

% Question without explicit QM (Nó là của Linh? - ? is removed by tokenizer)
question_part(tree(sq, [NP, VP])) -->
    np(NP),
    vp(VP).

% Question with copula (Nó là của ai)
question_part(tree(sq, [NP, VP_COP])) -->
    np(NP),
    vp_copula(VP_COP).

punct --> ['.'].
punct --> [','].

% ========================================
% S - CÂU KHẲNG ĐỊNH
% ========================================

s(tree(s, [NP, VP])) -->
    np(NP),
    vp(VP).

% ========================================
% SQ - CÂU HỎI YES/NO
% ========================================

% SQ -> NP VP QM
sq(tree(sq, [NP, VP, QM])) -->
    np(NP),
    vp(VP),
    qm(QM).

% SQ -> NP VP "phai" QM
sq(tree(sq, [NP, VP, QM])) -->
    np(NP),
    vp(VP),
    [phai],
    qm(QM).

% ========================================
% SBARQ - CÂU HỎI WH
% ========================================

% SBARQ -> NP VP (WH in NP or VP)
sbarq(tree(sbarq, [NP, VP])) -->
    np(NP),
    vp(VP).

% SBARQ -> NP VP-copula PP-wh: "Xe đạp là của ai?"
sbarq(tree(sbarq, [NP, VP])) -->
    np(NP),
    vp_copula_pp_wh(VP).

% SBARQ -> WP VP-copula NP PP: "Ai là em gái của Nhân?"
sbarq(tree(sbarq, [WP, VP])) -->
    wp(WP),
    vp_copula_np_pp(VP).

% SBARQ -> NP VP-copula WP PP: "Linh là gì của Nhân?"
sbarq(tree(sbarq, [NP, VP])) -->
    np_no_wh(NP),
    vp_copula_wh_pp(VP).

% ========================================
% NP - NOUN PHRASE
% ========================================

% NP -> NNP (Proper noun)
np(tree(np, [NNP])) -->
    nnp(NNP).

% NP -> compound noun (khu_vuon, xe_dap)
np(tree(np, [NNP])) -->
    compound_noun(NNP).

% NP -> compound common noun (khu_vuon as type)
np(tree(np, [NN])) -->
    compound_common_noun(NN).

% NP -> NN (Common noun)  
np(tree(np, [NN])) -->
    nn(NN).

% NP -> WP (Wh-pronoun: ai, gì)
np(tree(np, [WP])) -->
    wp(WP).

% NP -> WRB (Wh-adverb: đâu)
np(tree(np, [WRB])) -->
    wrb(WRB).

% NP -> PN (Pronoun: nó, đó)
np(tree(np, [PN])) -->
    pn(PN).

% NP -> DT NN (Determiner + Noun)
np(tree(np, [DT, NN])) -->
    dt(DT),
    nn(NN).

% NP -> DT CL NN (Một con mèo)
np(tree(np, [DT, CL, NN])) -->
    dt(DT),
    cl(CL),
    nn(NN).

% NP -> DT CL NN ADJ (Một chiếc xe màu xanh)
np(tree(np, [DT, CL, NN, ADJ])) -->
    dt(DT),
    cl(CL),
    nn(NN),
    adj_phrase(ADJ).

% NP -> "những" CL NN (những bông hoa)
np(tree(np, [tree(dt, [word(nhung, determiner)]), CL, NN])) -->
    [nhung],
    cl(CL),
    nn(NN).

% NP -> "Tên" PN (Tên nó)
np(tree(np_poss, [tree(nn, [word(ten, noun_common)]), PN])) -->
    [ten],
    pn(PN).

% NP -> "Người" VP (Người cho Miu ăn) - Relative clause
np(tree(np_rel, [tree(nn, [word(nguoi, noun_common)]), VP])) -->
    [nguoi],
    vp(VP).

% NP without WH (for specific patterns)
np_no_wh(tree(np, [NNP])) -->
    nnp(NNP).
np_no_wh(tree(np, [NN])) -->
    nn(NN).

% ========================================
% VP - VERB PHRASE
% ========================================

% VP -> VB (Intransitive)
vp(tree(vp, [VB])) -->
    vb_intrans(VB).

% VP -> VB NP (Transitive)
vp(tree(vp, [VB, NP])) -->
    vb_trans(VB),
    np(NP).

% VP -> VB NP NP (Ditransitive: tặng Linh xe đạp)
vp(tree(vp, [VB, NP1, NP2])) -->
    vb_ditrans(VB),
    np(NP1),
    np(NP2).

% VP -> VB PP (ngủ trong phòng khách)
vp(tree(vp, [VB, PP])) -->
    vb_intrans(VB),
    pp(PP).

% VP -> VB PP (sống tại ngoại ô)
vp(tree(vp, [VB, PP])) -->
    vb_trans_pp(VB),
    pp(PP).

% VP -> VB VB NP (Serial verb: thích ngắm hoa)
vp(tree(vp_serial, [VB1, VB2, NP])) -->
    vb_trans(VB1),
    vb_trans(VB2),
    np(NP).

% VP -> VB-copula NP (là em gái)
vp(tree(vp_copula, [VB, NP])) -->
    vb_copula(VB),
    np(NP).

% VP -> VB-copula PP (là của Linh)
vp(tree(vp_copula, [VB, PP])) -->
    vb_copula(VB),
    pp(PP).

% VP-copula with PP ending in WH: là của ai
vp_copula_pp_wh(tree(vp_copula, [VB, PP])) -->
    vb_copula(VB),
    pp_wh(PP).

% VP-copula with NP and PP: là em gái của Nhân
vp_copula_np_pp(tree(vp_copula, [VB, NP, PP])) -->
    vb_copula(VB),
    np_no_wh(NP),
    pp(PP).

% VP-copula with WH and PP: là gì của Nhân
vp_copula_wh_pp(tree(vp_copula, [VB, WP, PP])) -->
    vb_copula(VB),
    wp(WP),
    pp(PP).

% ========================================
% PP - PREPOSITIONAL PHRASE
% ========================================

% PP -> P NP (trong phòng khách, của Nhân)
pp(tree(pp, [P, NP])) -->
    p(P),
    np(NP).

% PP with WH: của ai
pp_wh(tree(pp, [P, WP])) -->
    p(P),
    wp(WP).

% ========================================
% ADJ PHRASE
% ========================================

adj_phrase(tree(adjp, [ADJ])) -->
    adj(ADJ).

% màu xanh
adj_phrase(tree(adjp, [tree(nn, [word(mau, noun_common)]), ADJ])) -->
    [mau],
    adj(ADJ).

% ========================================
% COMPOUND NOUNS - Only for true named entities
% ========================================

compound_noun(tree(nnp, [word(xe_dap_nhan, noun_proper)])) --> [xe_dap_nhan].
compound_noun(tree(nnp, [word(nha_nhan_linh, noun_proper)])) --> [nha_nhan_linh].
compound_noun(tree(nnp, [word(khu_vuon, noun_proper)])) --> [khu_vuon].
compound_noun(tree(nnp, [word(khu_vuon, noun_proper)])) --> [khu, vuon].
compound_noun(tree(nnp, [word(cac_bong_hoa, noun_proper)])) --> [cac_bong_hoa].
compound_noun(tree(nnp, [word(em_gai, noun_common)])) --> [em, gai].

% ========================================
% COMPOUND COMMON NOUNS - Type predicates
% ========================================

% sau_nha as location phrase
compound_common_noun(tree(nn, [word(sau_nha, noun_common)])) --> [sau, nha].
compound_common_noun(tree(nn, [word(sau_nha, noun_common)])) --> [sau_nha].

% ========================================
% TERMINALS - POS Tags
% ========================================

% NNP - Proper Noun
nnp(tree(nnp, [word(W, noun_proper)])) --> [W], {is_proper_noun(W)}.

% NN - Common Noun  
nn(tree(nn, [word(W, noun_common)])) --> [W], {is_common_noun(W)}.

% VB - Verb (intransitive)
vb_intrans(tree(vb, [word(W, verb_intrans)])) --> [W], {is_intrans_verb(W)}.

% VB - Verb (transitive)
vb_trans(tree(vb, [word(W, verb_trans)])) --> [W], {is_trans_verb(W)}.

% VB - Verb (ditransitive)
vb_ditrans(tree(vb, [word(W, verb_ditrans)])) --> [W], {is_ditrans_verb(W)}.

% VB - Verb (transitive with PP)
vb_trans_pp(tree(vb, [word(W, verb_trans)])) --> [W], {is_trans_pp_verb(W)}.

% VB - Copula (là)
vb_copula(tree(vb, [word(W, copula)])) --> [W], {is_copula(W)}.

% WP - Wh-Pronoun (ai, gì)
wp(tree(wp, [word(W, wh_word)])) --> [W], {is_wh_pronoun(W)}.

% WRB - Wh-Adverb (đâu)
wrb(tree(wrb, [word(W, wh_word)])) --> [W], {is_wh_adverb(W)}.

% DT - Determiner
dt(tree(dt, [word(W, determiner)])) --> [W], {is_determiner(W)}.

% CL - Classifier
cl(tree(cl, [word(W, classifier)])) --> [W], {is_classifier(W)}.

% PN - Pronoun
pn(tree(pn, [word(W, pronoun)])) --> [W], {is_pronoun(W)}.

% P - Preposition
p(tree(p, [word(W, preposition)])) --> [W], {is_preposition(W)}.

% ADJ - Adjective
adj(tree(adj, [word(W, adjective)])) --> [W], {is_adjective(W)}.

% QM - Question Marker
qm(tree(qm, [word(W, qm)])) --> [W], {is_question_marker(W)}.

% ========================================
% LEXICON (Từ điển)
% ========================================

% Proper nouns - ONLY true named entities
is_proper_noun(linh).
is_proper_noun(nhan).
is_proper_noun(miu).
is_proper_noun(bo_nhan).
is_proper_noun(xe_dap_nhan).
is_proper_noun(nha_nhan_linh).
is_proper_noun(cac_bong_hoa).
is_proper_noun(truong).
is_proper_noun(ngoai_o).
is_proper_noun(xanh).

% Common nouns - Type predicates
is_common_noun(nguoi).
is_common_noun(meo).
is_common_noun(con_meo).
is_common_noun(xe).
is_common_noun(xe_dap).  % Type predicate!
is_common_noun(ghe).
is_common_noun(nha).
is_common_noun(phong_khach).  % Type predicate!
is_common_noun(vuon).  % Type predicate!
is_common_noun(sau_nha).  % Location
is_common_noun(ten).
is_common_noun(em_gai).
is_common_noun(bong).
is_common_noun(hoa).  % Type predicate!

% Intransitive verbs
is_intrans_verb(ngu).
is_intrans_verb(hien).
is_intrans_verb(nam_ngu).

% Transitive verbs
is_trans_verb(thich).
is_trans_verb(so_huu).
is_trans_verb(cho_an).
is_trans_verb(ten_la).
is_trans_verb(o).
is_trans_verb(song_cung).
is_trans_verb(song_tai).
is_trans_verb(ngam).
is_trans_verb(choi).
is_trans_verb(choi_voi).
is_trans_verb(co).

% Transitive verbs that take PP
is_trans_pp_verb(song).
is_trans_pp_verb(song_tai).
is_trans_pp_verb(o).
is_trans_pp_verb(ngu).
is_trans_pp_verb(nam_ngu).

% Ditransitive verbs
is_ditrans_verb(tang).
is_ditrans_verb(cho).

% Copula
is_copula(la).

% Wh-pronouns
is_wh_pronoun(ai).
is_wh_pronoun(gi).

% Wh-adverbs
is_wh_adverb(dau).
is_wh_adverb(o_dau).

% Determiners
is_determiner(mot).
is_determiner(moi).
is_determiner(cac).
is_determiner(nhung).

% Classifiers
is_classifier(chiec).
is_classifier(con).
is_classifier(cai).
is_classifier(bong).

% Pronouns
is_pronoun(no).
is_pronoun(do).
is_pronoun(chung).  % chúng (plural they/them)
is_pronoun(anh_ay).
is_pronoun(co_ay).

% Prepositions
is_preposition(trong).
is_preposition(tren).
is_preposition(tai).
is_preposition(cua).
is_preposition(sau).
is_preposition(o).

% Adjectives
is_adjective(xanh).
is_adjective(do).
is_adjective(nho).
is_adjective(lon).
is_adjective(dep).

% Question markers
is_question_marker(khong).
is_question_marker(phai_khong).
is_question_marker(chua).

