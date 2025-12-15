% ========================================
% PARSING MODULE: STRUCTURES
% DCG Grammar with Compositional Semantics
% ========================================

:- module(structures, [
    parse/3
]).

:- use_module('../linguistic/vocabulary').

% ========================================
% MAIN PARSE INTERFACE
% ========================================

parse(Tokens, Type, Tree) :-
    ( Type = yn ->
        phrase(sentence_yn(Tree), Tokens)
    ; Type = who ->
        phrase(sentence_who(Tree), Tokens)
    ; Type = what ->
        phrase(sentence_what(Tree), Tokens)
    ; Type = where ->
        phrase(sentence_where(Tree), Tokens)
    ;
        phrase(sentence(Tree), Tokens)
    ).

% ========================================
% YES/NO QUESTIONS
% "Gau hien khong?" → S[yn]
% ========================================

sentence_yn(tree(s_yn, [NP, VP, QM])) -->
    noun_phrase(NP),
    verb_phrase(VP),
    question_marker(QM).

sentence_yn(tree(s_yn, [NP, VP])) -->
    noun_phrase(NP),
    verb_phrase(VP).

% ========================================
% WH-QUESTIONS: WHO
% "Ai dat Gau?" → S[wh:who]
% ========================================

% Pattern 1: "Ai [Verb] [Object]" - standard
sentence_who(tree(s_wh, [WH, VP])) -->
    wh_word_who(WH),
    verb_phrase_trans(VP).

% Pattern 2: "Ai [V1] [V2] [Object]" - compound verb
% Example: "Ai cho an miu" = "Ai cho_an miu"
sentence_who(tree(s_wh, [WH, V1Word, V2Word, NP])) -->
    wh_word_who(WH),
    [V1Word],
    [V2Word],
    noun_phrase(NP),
    { 
        % Check if V1 + V2 forms a valid compound verb
        atom_concat(V1Word, '_', Temp),
        atom_concat(Temp, V2Word, CompoundVerb),
        vocabulary:word_semantics(CompoundVerb, verb_trans, _)
    }.

sentence_who(tree(s_wh, [WH, VP])) -->
    wh_word_who(WH),
    verb_phrase(VP).

wh_word_who(tree(wh, [word(ai, wh_word)])) -->
    [ai].

% ========================================
% WH-QUESTIONS: WHAT
% "Meo an gi?" → S[wh:what]
% ========================================

sentence_what(tree(s_wh, [NP, V, WH])) -->
    noun_phrase(NP),
    verb_trans(V),
    wh_word_what(WH).

sentence_what(tree(s_wh, [WH, Prop, PP])) -->
    wh_word_what(WH),
    property_phrase(Prop),
    pp_of(PP).

wh_word_what(tree(wh, [word(gi, wh_word)])) -->
    [gi].

% ========================================
% WH-QUESTIONS: WHERE
% "Huy o dau?" → S[wh:where]
% ========================================

sentence_where(tree(s_wh, [NP, Prep, WH])) -->
    noun_phrase(NP),
    [o],
    wh_word_where(WH).

wh_word_where(tree(wh, [word(dau, wh_word)])) -->
    [dau].

% ========================================
% NOUN PHRASES
% ========================================

noun_phrase(tree(np, [word(N, noun_proper)])) -->
    [N],
    { vocabulary:word_semantics(N, noun_proper, _) }.

noun_phrase(tree(np, [Det, N])) -->
    determiner(Det),
    noun(N).

noun_phrase(tree(np, [Adj, N])) -->
    adjective(Adj),
    noun(N).

% Proper nouns
noun(tree(n, [word(N, noun_proper)])) -->
    [N],
    { vocabulary:word_semantics(N, noun_proper, _) }.

% Common nouns
noun(tree(n, [word(N, noun_common)])) -->
    [N],
    { vocabulary:word_semantics(N, noun_common, _) }.

% ========================================
% DETERMINERS
% ========================================

determiner(tree(det, [word(D, determiner)])) -->
    [D],
    { vocabulary:word_semantics(D, determiner, _) }.

% ========================================
% ADJECTIVES
% ========================================

adjective(tree(adj, [word(A, adjective)])) -->
    [A],
    { vocabulary:word_semantics(A, adjective, _) }.

% ========================================
% VERB PHRASES
% ========================================

verb_phrase(tree(vp, [V])) -->
    verb_intrans(V).

verb_phrase(tree(vp, [V, NP])) -->
    verb_trans(V),
    noun_phrase(NP).

verb_phrase_trans(tree(vp, [V, NP])) -->
    verb_trans(V),
    noun_phrase(NP).

% Intransitive verbs
verb_intrans(tree(v, [word(V, verb_intrans)])) -->
    [V],
    { vocabulary:word_semantics(V, verb_intrans, _) }.

% Transitive verbs
verb_trans(tree(v, [word(V, verb_trans)])) -->
    [V],
    { vocabulary:word_semantics(V, verb_trans, _) }.

% ========================================
% PREPOSITIONAL PHRASES
% ========================================

pp_of(tree(pp, [word(cua, preposition), NP])) -->
    [cua],
    noun_phrase(NP).

pp_location(tree(pp, [word(o, preposition), NP])) -->
    [o],
    noun_phrase(NP).

% ========================================
% PROPERTY PHRASES
% "mau long" → property phrase
% ========================================

property_phrase(tree(prop, [word(mau, property_noun), word(Part, body_part)])) -->
    [mau],
    [Part],
    { vocabulary:word_semantics(Part, body_part, _) }.

% ========================================
% QUESTION MARKERS
% ========================================

question_marker(tree(qm, [word(khong, question_marker)])) -->
    [khong].

question_marker(tree(qm, [word(ko, question_marker)])) -->
    [ko].

question_marker(tree(qm, [word('?', question_marker)])) -->
    ['?'].

question_marker(tree(qm, [])) -->
    [].

% ========================================
% GENERAL SENTENCE (fallback)
% ========================================

sentence(tree(s, [NP, VP])) -->
    noun_phrase(NP),
    verb_phrase(VP).

% ========================================
% UTILITIES
% ========================================

% Pretty print parse tree
pretty_print_tree(tree(Label, Children), Indent) :-
    format('~w~w~n', [Indent, Label]),
    atom_concat(Indent, '  ', NewIndent),
    forall(member(Child, Children),
           pretty_print_tree(Child, NewIndent)).

pretty_print_tree(word(Word, Cat), Indent) :-
    format('~w~w:~w~n', [Indent, Word, Cat]).
