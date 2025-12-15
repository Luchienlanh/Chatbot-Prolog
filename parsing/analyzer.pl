% ========================================
% PARSING MODULE: ANALYZER
% Tokenization & Morphological Analysis
% ========================================

:- module(analyzer, [
    tokenize/2,
    normalize/2,
    analyze_morphology/2
]).

:- use_module('../linguistic/vocabulary').
:- use_module('../knowledge/repository').

% ========================================
% TOKENIZATION
% ========================================

tokenize(String, Tokens) :-
    normalize_string(String, Normalized),
    split_string(Normalized, " \t\n", "", TokenStrings),
    maplist(string_atom, TokenStrings, RawTokens),
    filter_empty(RawTokens, FilteredTokens),
    merge_compound_words(FilteredTokens, Tokens).

% Merge compound words like "so huu" -> "so_huu"
merge_compound_words([], []).
merge_compound_words([W1, W2 | Rest], [Merged | MergedRest]) :-
    atom_concat(W1, '_', Temp),
    atom_concat(Temp, W2, Merged),
    is_known_word(Merged), !,
    merge_compound_words(Rest, MergedRest).
merge_compound_words([W | Rest], [W | MergedRest]) :-
    merge_compound_words(Rest, MergedRest).

% Check if word exists in vocabulary
is_known_word(Word) :-
    vocabulary:word_semantics(Word, _, _), !.
is_known_word(Word) :-
    repository:entity(Word, _).

normalize_string(String, Normalized) :-
    string_lower(String, Lower),
    % Remove punctuation at end
    re_replace("\\?$|!$|\\.$"/g, "", Lower, Normalized).

filter_empty([], []).
filter_empty([''|Rest], Filtered) :- !,
    filter_empty(Rest, Filtered).
filter_empty([Token|Rest], [Token|Filtered]) :-
    filter_empty(Rest, Filtered).

string_atom(String, Atom) :-
    atom_string(Atom, String).

% ========================================
% NORMALIZATION
% Handle variations and synonyms
% ========================================

normalize(Word, Normalized) :-
    synonym(Word, Normalized), !.

normalize(Word, Word).

% Synonyms - Từ đồng nghĩa cũ
synonym(hien_lanh, hien).
synonym(tot_bung, hien).
synonym(dang_yeu, de_thuong).
synonym(xinh, dep).
synonym(nho_nhan, nho).
synonym(to_lon, lon).
synonym(ko, khong).

% Synonyms - Data mới (Nhân, Linh, Miu)
synonym(con_meo, miu).
synonym(meo_con, miu).
synonym(xe, xe_dap).
synonym(xe_dap_xanh, xe_dap).
synonym(bo, bo_nhan).
synonym(ba, bo_nhan).
synonym(nha_cua, nha).
synonym(can_nha, nha).
synonym(vuon_hoa, vuon).
synonym(khu_vuon, vuon).
synonym(em, linh).
synonym(em_gai, linh).
synonym(anh, nhan).
synonym(truong_hoc, truong).

% ========================================
% MORPHOLOGICAL ANALYSIS
% ========================================

analyze_morphology(Token, Analysis) :-
    ( is_proper_noun(Token) ->
        Analysis = morph(Token, proper_noun, [])
    ; is_common_noun(Token) ->
        Analysis = morph(Token, common_noun, [])
    ; is_verb(Token) ->
        analyze_verb(Token, Analysis)
    ; is_adjective(Token) ->
        Analysis = morph(Token, adjective, [])
    ; is_question_word(Token) ->
        Analysis = morph(Token, question_word, [])
    ;
        Analysis = morph(Token, unknown, [])
    ).

is_proper_noun(Token) :-
    vocabulary:word_semantics(Token, noun_proper, _).

is_common_noun(Token) :-
    vocabulary:word_semantics(Token, noun_common, _).

is_verb(Token) :-
    ( vocabulary:word_semantics(Token, verb_intrans, _)
    ; vocabulary:word_semantics(Token, verb_trans, _)
    ).

analyze_verb(Token, morph(Token, verb, [transitivity(Trans)])) :-
    ( vocabulary:word_semantics(Token, verb_trans, _) ->
        Trans = transitive
    ;
        Trans = intransitive
    ).

is_adjective(Token) :-
    vocabulary:word_semantics(Token, adjective, _).

is_question_word(Token) :-
    vocabulary:word_semantics(Token, wh_word, _).

% ========================================
% WORD SEGMENTATION (for compound words)
% ========================================

segment_compound(Word, Segments) :-
    atom_chars(Word, Chars),
    segment_chars(Chars, SegmentChars),
    maplist(atom_chars, Segments, SegmentChars).

segment_chars([], []).
segment_chars(Chars, [Seg|Rest]) :-
    append(Seg, Remaining, Chars),
    Seg \= [],
    is_valid_word(Seg),
    segment_chars(Remaining, Rest).

is_valid_word(Chars) :-
    atom_chars(Word, Chars),
    vocabulary:word_semantics(Word, _, _).
