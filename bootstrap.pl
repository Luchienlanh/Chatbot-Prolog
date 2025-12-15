% ========================================
% VIETNAMESE QA SYSTEM - COMPOSITIONAL SEMANTICS
% Lambda Calculus + DRS + FOL + Theorem Proving
% ========================================

:- use_module('linguistic/vocabulary').
:- use_module('linguistic/composition').
:- use_module('parsing/analyzer').
:- use_module('parsing/structures').
:- use_module('logic/discourse').
:- use_module('logic/firstorder').
:- use_module('reasoning/inference').
:- use_module('reasoning/theorem').
:- use_module('knowledge/repository').
:- use_module('interface/shell').

% ========================================
% SYSTEM INITIALIZATION
% ========================================

initialize :-
    writeln('========================================'),
    writeln('  VIETNAMESE COMPOSITIONAL QA SYSTEM'),
    writeln('  Lambda Calculus · DRS · FOL'),
    writeln('========================================'),
    nl,
    
    write('Loading knowledge base... '),
    repository:initialize_kb,
    writeln('✓'),
    
    write('Initializing inference engine... '),
    theorem:initialize_prover,
    writeln('✓'),
    
    nl,
    writeln('System ready!'),
    writeln('Type: query("Your question", Type).'),
    nl.

% ========================================
% MAIN QUERY INTERFACE
% ========================================

query(Question, Type) :-
    writeln(''),
    writeln('========================================'),
    format('Query: ~w~n', [Question]),
    writeln('========================================'),
    nl,
    
    % Stage 1: Tokenization
    write('-> Tokenization... '),
    analyzer:tokenize(Question, Tokens),
    format('~w~n', [Tokens]),
    
    % Stage 2: Syntactic Analysis
    write('-> Parsing... '),
    structures:parse(Tokens, Type, Tree),
    writeln('OK'),
    
    % Stage 3: Semantic Composition (Lambda)
    write('-> Lambda Composition... '),
    composition:compose(Tree, Lambda),
    writeln('OK'),
    
    % Stage 4: DRS Construction
    write('-> Building DRS... '),
    discourse:build_drs(Lambda, DRS),
    writeln('OK'),
    
    % Stage 5: FOL Conversion
    write('-> Converting to FOL... '),
    firstorder:convert(DRS, FOL),
    writeln('OK'),
    
    % Stage 6: Theorem Proving
    nl,
    write('-> Proving... '),
    ( Type = yn ->
        ( theorem:prove(FOL) ->
            writeln('TRUE'),
            nl,
            writeln('========================================'),
            writeln('Answer: YES / CO'),
            writeln('========================================')
        ;
            writeln('FALSE'),
            nl,
            writeln('========================================'),
            writeln('Answer: NO / KHONG'),
            writeln('========================================')
        )
    ; Type = who ->
        theorem:find_entities(FOL, Entities),
        writeln('OK'),
        nl,
        writeln('========================================'),
        format('Answer: ~w~n', [Entities]),
        writeln('========================================')
    ; Type = what ->
        theorem:find_values(FOL, Values),
        writeln('OK'),
        nl,
        writeln('========================================'),
        format('Answer: ~w~n', [Values]),
        writeln('========================================')
    ),
    nl.

% ========================================
% INTERACTIVE SHELL
% ========================================

interactive :-
    initialize,
    shell:start_repl.

% ========================================
% DEBUGGING & TRACING
% ========================================

trace_query(Question, Type) :-
    writeln(''),
    writeln('═══════════════════════════════════════'),
    writeln('  FULL TRACE'),
    writeln('═══════════════════════════════════════'),
    nl,
    
    analyzer:tokenize(Question, Tokens),
    format('Tokens: ~w~n~n', [Tokens]),
    
    structures:parse(Tokens, Type, Tree),
    format('Parse Tree:~n~w~n~n', [Tree]),
    
    composition:compose(Tree, Lambda),
    format('Lambda Expression:~n~w~n~n', [Lambda]),
    
    composition:beta_reduce_full(Lambda, Reduced),
    format('After Beta Reduction:~n~w~n~n', [Reduced]),
    
    discourse:build_drs(Reduced, DRS),
    format('DRS:~n'),
    discourse:pretty_print(DRS),
    nl,
    
    firstorder:convert(DRS, FOL),
    format('FOL:~n~w~n~n', [FOL]),
    
    ( Type = yn ->
        ( theorem:prove_with_trace(FOL) ->
            writeln('Proof: SUCCESS ✓')
        ;
            writeln('Proof: FAILED ✗')
        )
    ;
        writeln('Finding bindings...')
    ).

% ========================================
% UTILITIES
% ========================================

demo :-
    initialize,
    writeln('Chạy các câu hỏi demo với data mới...'),
    nl,
    
    writeln('--- Câu hỏi GÌ (what) ---'),
    query("Meo ten gi", what),
    query("Linh thich gi", what),
    
    writeln('--- Câu hỏi AI (who) ---'),
    query("Ai cho an Miu", who),
    query("Ai so huu xe dap", who),
    
    writeln('--- Câu hỏi CÓ/KHÔNG (yn) ---'),
    query("Linh thich hoa khong", yn).

% ========================================
% STARTUP
% ========================================

:- initialization(initialize, main).
