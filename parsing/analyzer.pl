% ========================================
% PARSING MODULE: ANALYZER
% Tokenization & Morphological Analysis
% ========================================

:- encoding(utf8).

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
    process_punctuation(FilteredTokens, CleanTokens),
    merge_compound_words(CleanTokens, Tokens).

% Process punctuation - keep periods for sentence splitting, remove question marks
process_punctuation([], []).
process_punctuation(['.'|Rest], ['.'|Processed]) :- !,
    process_punctuation(Rest, Processed).
process_punctuation(['?'|Rest], Processed) :- !,
    process_punctuation(Rest, Processed).
process_punctuation(['!'|Rest], Processed) :- !,
    process_punctuation(Rest, Processed).
process_punctuation([','|Rest], Processed) :- !,
    process_punctuation(Rest, Processed).
process_punctuation([Token|Rest], [Token|Processed]) :-
    process_punctuation(Rest, Processed).

% Merge compound words like "so huu" -> "so_huu"
merge_compound_words([], []).
% Special: "phai khong" -> "phai_khong"
merge_compound_words([phai, khong | Rest], [phai_khong | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "khu vuon" -> "khu_vuon"
merge_compound_words([khu, vuon | Rest], [khu_vuon | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "xe dap" -> "xe_dap"
merge_compound_words([xe, dap | Rest], [xe_dap | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "em gai" -> "em_gai"
merge_compound_words([em, gai | Rest], [em_gai | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "phong khach" -> "phong_khach"
merge_compound_words([phong, khach | Rest], [phong_khach | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "sau nha" -> "sau_nha"
merge_compound_words([sau, nha | Rest], [sau_nha | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "ngoai o" -> "ngoai_o"
merge_compound_words([ngoai, o | Rest], [ngoai_o | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "cho an" -> "cho_an" (adjacent)
merge_compound_words([cho, an | Rest], [cho_an | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "cho X an" -> "cho_an X" (discontinuous compound verb)
merge_compound_words([cho, X, an | Rest], [cho_an, X | MergedRest]) :- 
    X \= an, !,
    merge_compound_words(Rest, MergedRest).
% Special: "song cung" -> "song_cung"
merge_compound_words([song, cung | Rest], [song_cung | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "song tai" -> "song_tai"
merge_compound_words([song, tai | Rest], [song_tai | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "so huu" -> "so_huu"
merge_compound_words([so, huu | Rest], [so_huu | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "con meo" -> "con_meo"
merge_compound_words([con, meo | Rest], [con_meo | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "choi voi" -> "choi_voi"
merge_compound_words([choi, voi | Rest], [choi_voi | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: "mau xanh" -> "mau, xanh" (for adjective phrase)
merge_compound_words([mau, xanh | Rest], [mau, xanh | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% Special: Don't merge "o dau" - keep separate for VP WRB parsing
merge_compound_words([o, dau | Rest], [o, dau | MergedRest]) :- !,
    merge_compound_words(Rest, MergedRest).
% General case: try to merge if known
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

% Normalize: lowercase, remove accents, add spaces around punctuation
normalize_string(String, Normalized) :-
    string_lower(String, Lower),
    remove_accents(Lower, NoAccents),
    % Add space before and after periods to split sentences
    re_replace("\\."/g, " ", NoAccents, NoPeriods),
    % Remove ALL question marks and exclamation marks (handles Unicode better)
    re_replace("[?!¿¡]"/g, "", NoPeriods, Normalized).

remove_accents(String, Result) :-
    string_chars(String, Chars),
    maplist(map_char, Chars, MappedChars),
    string_chars(Result, MappedChars).

map_char(Char, Mapped) :- accent_map(Char, Mapped), !.
map_char(Char, Char).

accent_map('á', 'a'). accent_map('à', 'a'). accent_map('ả', 'a'). accent_map('ã', 'a'). accent_map('ạ', 'a').
accent_map('ă', 'a'). accent_map('ắ', 'a'). accent_map('ằ', 'a'). accent_map('ẳ', 'a'). accent_map('ẵ', 'a'). accent_map('ặ', 'a').
accent_map('â', 'a'). accent_map('ấ', 'a'). accent_map('ầ', 'a'). accent_map('ẩ', 'a'). accent_map('ẫ', 'a'). accent_map('ậ', 'a').
accent_map('đ', 'd').
accent_map('é', 'e'). accent_map('è', 'e'). accent_map('ẻ', 'e'). accent_map('ẽ', 'e'). accent_map('ẹ', 'e').
accent_map('ê', 'e'). accent_map('ế', 'e'). accent_map('ề', 'e'). accent_map('ể', 'e'). accent_map('ễ', 'e'). accent_map('ệ', 'e').
accent_map('í', 'i'). accent_map('ì', 'i'). accent_map('ỉ', 'i'). accent_map('ĩ', 'i'). accent_map('ị', 'i').
accent_map('ó', 'o'). accent_map('ò', 'o'). accent_map('ỏ', 'o'). accent_map('õ', 'o'). accent_map('ọ', 'o').
accent_map('ô', 'o'). accent_map('ố', 'o'). accent_map('ồ', 'o'). accent_map('ổ', 'o'). accent_map('ỗ', 'o'). accent_map('ộ', 'o').
accent_map('ơ', 'o'). accent_map('ớ', 'o'). accent_map('ờ', 'o'). accent_map('ở', 'o'). accent_map('ỡ', 'o'). accent_map('ợ', 'o').
accent_map('ú', 'u'). accent_map('ù', 'u'). accent_map('ủ', 'u'). accent_map('ũ', 'u'). accent_map('ụ', 'u').
accent_map('ư', 'u'). accent_map('ứ', 'u'). accent_map('ừ', 'u'). accent_map('ử', 'u'). accent_map('ữ', 'u'). accent_map('ự', 'u').
accent_map('ý', 'y'). accent_map('ỳ', 'y'). accent_map('ỷ', 'y'). accent_map('ỹ', 'y'). accent_map('ỵ', 'y').

filter_empty([], []).
filter_empty([''|Rest], Filtered) :- !,
    filter_empty(Rest, Filtered).
filter_empty([Token|Rest], [Token|Filtered]) :-
    filter_empty(Rest, Filtered).

% (filter_punctuation replaced by process_punctuation above)

string_atom(String, Atom) :-
    atom_string(Atom, String).

% ========================================
% NORMALIZATION
% ========================================

normalize(Word, Normalized) :-
    synonym(Word, Normalized), !.
normalize(Word, Word).

synonym(hien_lanh, hien).
synonym(tot_bung, hien).
synonym(dang_yeu, de_thuong).
synonym(xinh, dep).
synonym(nho_nhan, nho).
synonym(to_lon, lon).
synonym(ko, khong).
synonym(con_meo, miu).
synonym(meo_con, miu).

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
