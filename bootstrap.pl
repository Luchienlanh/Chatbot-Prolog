% ========================================
% VIETNAMESE Q&A SYSTEM - COMPOSITIONAL SEMANTICS
% THEO SLIDES MÔN HỌC NNHTT
% ========================================
%
% Hệ thống hỏi đáp tiếng Việt sử dụng:
% - Lambda Calculus cho ngữ nghĩa từ vựng
% - DRS (Discourse Representation Structure)
% - FOL (First-Order Logic)
% - Theorem Proving cho suy diễn
%
% Theo Slide-DOAN-01, pipeline xử lý:
% 1. Tách câu, tách từ (Tokenization)
% 2. Phân tích cú pháp (Parsing) → Cây cú pháp
% 3. Tổng hợp ngữ nghĩa (Lambda composition)
% 4. Xây dựng DRS
% 5. Chuyển DRS sang FOL
% 6. Theorem proving / Trả lời câu hỏi
%
% ========================================

:- encoding(utf8).

% Load các module
:- use_module('linguistic/vocabulary').
:- use_module('linguistic/composition').
:- use_module('parsing/analyzer').
:- use_module('parsing/structures').
:- use_module('logic/discourse').
:- use_module('logic/firstorder').
:- use_module('reasoning/inference').
:- use_module('reasoning/theorem').
:- use_module('knowledge/repository').

% ========================================
% INITIALIZATION
% ========================================

initialize :-
    writeln('========================================'),
    writeln('VIETNAMESE Q&A SYSTEM'),
    writeln('Compositional Semantics - NNHTT'),
    writeln('========================================'),
    nl,
    
    write('Loading knowledge base... '),
    repository:initialize_kb,
    writeln('OK'),
    
    write('Initializing discourse context... '),
    discourse:init_discourse,
    writeln('OK'),
    
    write('Running forward inference... '),
    inference:infer,
    writeln('OK'),
    
    write('Initializing theorem prover... '),
    theorem:initialize_prover,
    writeln('OK'),
    
    nl,
    writeln('System ready!'),
    writeln('Commands:'),
    writeln('  query("Question", Type).  - Query with type: yn, who, what, where'),
    writeln('  trace_query("Q", Type).   - Query with detailed trace'),
    writeln('  demo.                     - Run demo queries'),
    writeln('  interactive.              - Start interactive mode'),
    nl.

% ========================================
% MAIN QUERY INTERFACE
% query(Question, Type) - Type: yn, who, what, where
% ========================================


query(Question, Type) :-
    discourse:init_discourse,
    writeln(''),
    writeln('========================================'),
    format('Query: ~w~n', [Question]),
    format('Type: ~w~n', [Type]),
    writeln('========================================'),
    nl,
    
    % Stage 1: Tokenization
    write('Stage 1: Tokenization... '),
    analyzer:tokenize(Question, Tokens),
    format('~w~n', [Tokens]),
    
    % Stage 2: Parsing (try multi-sentence first, then single)
    write('Stage 2: Parsing... '),
    ( memberchk('.', Tokens), structures:parse_text(Tokens, Type, Tree) ->
        writeln('OK (multi-sentence)'),
        format('   Parse Tree: ~w~n', [Tree])
    ; structures:parse(Tokens, Type, Tree) ->
        writeln('OK'),
        format('   Parse Tree: ~w~n', [Tree])
    ;
        writeln('FAILED'),
        writeln('Could not parse the question.'),
        fail
    ),
    
    % Stage 3: Lambda Composition
    write('Stage 3: Lambda Composition... '),
    ( composition:compose(Tree, Lambda) ->
        writeln('OK'),
        format('   Lambda: ~w~n', [Lambda])
    ;
        writeln('FAILED'),
        fail
    ),
    
    % Stage 4: DRS Construction & Resolution
    write('Stage 4: DRS Construction... '),
    ( discourse:build_drs(Lambda, RawDRS) ->
        % Resolve Coreference
        discourse:resolve_drs(RawDRS, DRS),
        writeln('OK'),
        format('   DRS (Raw): ~w~n', [RawDRS]),
        format('   DRS (Resolved): ~w~n', [DRS])
    ;
        writeln('FAILED'),
        fail
    ),
    
    % Stage 5: FOL Conversion
    write('Stage 5: FOL Conversion... '),
    ( firstorder:convert(DRS, FOL) ->
        writeln('OK'),
        format('   FOL: ~w~n', [FOL])
    ;
        writeln('FAILED'),
        fail
    ),
    
    % Stage 6: Theorem Proving / Answer
    nl,
    write('Stage 6: Proving... '),
    answer_question(Type, FOL).

% ========================================
% ANSWER QUESTION BASED ON TYPE
% ========================================

% Yes/No question
answer_question(yn, FOL) :-
    ( theorem:prove(FOL) ->
        writeln('OK'),
        nl,
        writeln('========================================'),
        writeln('Answer: YES / CÓ'),
        writeln('========================================')
    ;
        writeln('OK'),
        nl,
        writeln('========================================'),
        writeln('Answer: NO / KHÔNG'),
        writeln('========================================')
    ).

% Who question - FOL thuần túy, không có wh_question wrapper
answer_question(who, FOL) :-
    theorem:find_entities(who, FOL, Entities),
    writeln('OK'),
    nl,
    writeln('========================================'),
    ( Entities = [] ->
        writeln('Answer: Không tìm thấy')
    ;
        format('Answer: ~w~n', [Entities])
    ),
    writeln('========================================').

% What question - FOL thuần túy
answer_question(what, FOL) :-
    theorem:find_values(what, FOL, Values),
    writeln('OK'),
    nl,
    writeln('========================================'),
    ( Values = [] ->
        writeln('Answer: Không tìm thấy')
    ;
        format('Answer: ~w~n', [Values])
    ),
    writeln('========================================').

% Where question - FOL thuần túy
answer_question(where, FOL) :-
    theorem:find_values(where, FOL, Values),
    writeln('OK'),
    nl,
    writeln('========================================'),
    ( Values = [] ->
        writeln('Answer: Không tìm thấy')
    ;
        format('Answer: ~w~n', [Values])
    ),
    writeln('========================================').

% ========================================
% TRACE QUERY - WITH DETAILED OUTPUT
% ========================================

trace_query(Question, Type) :-
    writeln(''),
    writeln('========================================'),
    writeln('TRACE MODE'),
    writeln('========================================'),
    
    % Tokenization
    writeln(''),
    writeln('=== STAGE 1: TOKENIZATION ==='),
    analyzer:tokenize(Question, Tokens),
    format('Input: "~w"~n', [Question]),
    format('Tokens: ~w~n', [Tokens]),
    
    % Parsing
    writeln(''),
    writeln('=== STAGE 2: PARSING ==='),
    ( structures:parse(Tokens, Type, Tree) ->
        format('Parse Tree:~n'),
        print_tree(Tree, 0)
    ;
        writeln('Parsing failed!')
    ),
    
    % Lambda Composition
    writeln(''),
    writeln('=== STAGE 3: LAMBDA COMPOSITION ==='),
    ( composition:compose(Tree, Lambda) ->
        format('Lambda Expression: ~w~n', [Lambda])
    ;
        writeln('Composition failed!')
    ),
    
    % DRS
    writeln(''),
    writeln('=== STAGE 4: DRS CONSTRUCTION ==='),
    ( discourse:build_drs(Lambda, DRS) ->
        discourse:pretty_print_drs(DRS)
    ;
        writeln('DRS construction failed!')
    ),
    
    % FOL
    writeln(''),
    writeln('=== STAGE 5: FOL CONVERSION ==='),
    ( firstorder:convert(DRS, FOL) ->
        format('FOL: ~w~n', [FOL])
    ;
        writeln('FOL conversion failed!')
    ),
    
    % Proving
    writeln(''),
    writeln('=== STAGE 6: THEOREM PROVING ==='),
    ( Type = yn ->
        ( theorem:prove(FOL) ->
            writeln('Result: TRUE')
        ;
            writeln('Result: FALSE')
        )
    ;
        writeln('Finding answers...'),
        answer_question(Type, FOL)
    ).

% Print tree với indentation
print_tree(tree(Type, Children), Depth) :- !,
    indent_print(Depth),
    format('~w~n', [Type]),
    NewDepth is Depth + 1,
    maplist(print_child(NewDepth), Children).

print_tree(word(W, Cat), Depth) :- !,
    indent_print(Depth),
    format('~w (~w)~n', [W, Cat]).

print_tree(X, Depth) :-
    indent_print(Depth),
    format('~w~n', [X]).

print_child(Depth, Child) :- print_tree(Child, Depth).

indent_print(0) :- !.
indent_print(N) :- 
    N > 0, 
    write('  '), 
    N1 is N - 1, 
    indent_print(N1).

% ========================================
% INTERACTIVE MODE
% ========================================

interactive :-
    initialize,
    interactive_loop.

interactive_loop :-
    writeln(''),
    write('> '),
    read_line_to_string(user_input, Input),
    ( Input = "exit" ->
        writeln('Goodbye!')
    ; Input = "quit" ->
        writeln('Goodbye!')
    ; Input = "help" ->
        show_help,
        interactive_loop
    ; Input = "" ->
        interactive_loop
    ;
        process_interactive(Input),
        interactive_loop
    ).

process_interactive(Input) :-
    detect_question_type(Input, Type),
    ( query(Input, Type) ->
        true
    ;
        writeln('Could not process the question.')
    ).

% Detect question type from input
detect_question_type(Input, Type) :-
    atom_string(AtomInput, Input),
    ( ( sub_atom(AtomInput, _, _, _, 'khong')
      ; sub_atom(AtomInput, _, _, _, 'ko')
      ; sub_atom(AtomInput, _, _, _, 'phai')
      ; sub_atom(AtomInput, _, _, _, 'chua')
      ) ->
        Type = yn
    ; sub_atom(AtomInput, 0, _, _, 'ai') ->
        Type = who
    ; sub_atom(AtomInput, _, _, 0, 'gi') ->
        Type = what
    ; ( sub_atom(AtomInput, _, _, _, 'dau')
      ; sub_atom(AtomInput, _, _, _, 'o dau')
      ) ->
        Type = where
    ;
        Type = yn  % default
    ).

show_help :-
    writeln(''),
    writeln('=== HELP ==='),
    writeln('Enter a question in Vietnamese (no diacritics).'),
    writeln(''),
    writeln('Examples:'),
    writeln('  Linh thich gi'),
    writeln('  Ai so huu xe dap'),
    writeln('  Linh thich hoa khong'),
    writeln('  Miu o dau'),
    writeln(''),
    writeln('Commands:'),
    writeln('  help - Show this help'),
    writeln('  exit - Exit interactive mode'),
    writeln('').

% ========================================
% DEMO
% ========================================

demo :-
    initialize,
    writeln(''),
    writeln('========================================'),
    writeln('RUNNING DEMO QUERIES'),
    writeln('========================================'),
    
    % Demo 1: What question
    writeln(''),
    writeln('--- Demo 1: WHAT Question ---'),
    query("Linh thich gi", what),
    
    % Demo 2: Who question
    writeln(''),
    writeln('--- Demo 2: WHO Question ---'),
    query("Ai so huu xe dap", who),
    
    % Demo 3: Yes/No question (true)
    writeln(''),
    writeln('--- Demo 3: YES/NO Question (True) ---'),
    query("Linh thich hoa khong", yn),
    
    % Demo 4: Yes/No question (false)
    writeln(''),
    writeln('--- Demo 4: YES/NO Question (False) ---'),
    query("Nhan thich hoa khong", yn),
    
    % Demo 5: Who question with transitive verb
    writeln(''),
    writeln('--- Demo 5: WHO + Transitive Verb ---'),
    query("Ai cho an miu", who),
    
    writeln(''),
    writeln('========================================'),
    writeln('DEMO COMPLETE'),
    writeln('========================================').

% ========================================
% QUICK TEST
% ========================================

test :-
    writeln('Running quick tests...'),
    nl,
    
    % Test tokenization
    write('Test tokenize: '),
    analyzer:tokenize("Linh thich gi", T1),
    format('~w~n', [T1]),
    
    % Test parsing
    write('Test parse: '),
    structures:parse([linh, thich, gi], what, Tree1),
    format('~w~n', [Tree1]),
    
    % Test composition
    write('Test compose: '),
    composition:compose(Tree1, Lambda1),
    format('~w~n', [Lambda1]),
    
    % Test full query
    nl,
    writeln('Test full query:'),
    query("Linh thich gi", what),
    
    nl,
    writeln('All tests passed!').
