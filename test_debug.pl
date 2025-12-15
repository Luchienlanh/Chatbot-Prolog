% Debug script để test "Ai cho an Miu"

:- consult('bootstrap.pl').

test_query :-
    writeln('=== DEBUG: Ai cho an Miu ==='),
    nl,
    
    % Step 1: Tokenization
    writeln('Step 1: Tokenization'),
    analyzer:tokenize("Ai cho an Miu", Tokens),
    format('  Tokens: ~w~n', [Tokens]),
    nl,
    
    % Step 2: Parsing
    writeln('Step 2: Parsing'),
    ( structures:parse(Tokens, who, Tree) ->
        format('  Tree: ~w~n', [Tree])
    ;
        writeln('  ERROR: Parsing failed!'),
        halt
    ),
    nl,
    
    % Step 3: Composition
    writeln('Step 3: Composition'),
    ( composition:compose(Tree, Lambda) ->
        format('  Lambda: ~w~n', [Lambda])
    ;
        writeln('  ERROR: Composition failed!'),
        halt
    ),
    nl,
    
    % Step 4: DRS
    writeln('Step 4: DRS'),
    ( discourse:build_drs(Lambda, DRS) ->
        format('  DRS: ~w~n', [DRS])
    ;
        writeln('  ERROR: DRS failed!'),
        halt
    ),
    nl,
    
    % Step 5: FOL
    writeln('Step 5: FOL'),
    ( firstorder:convert(DRS, FOL) ->
        format('  FOL: ~w~n', [FOL])
    ;
        writeln('  ERROR: FOL conversion failed!'),
        halt
    ),
    nl,
    
    % Step 6: Find entities
    writeln('Step 6: Find Entities'),
    ( theorem:find_entities(FOL, Entities) ->
        format('  Entities: ~w~n', [Entities])
    ;
        writeln('  ERROR: Find entities failed!'),
        halt
    ),
    nl,
    
    % Step 7: Check repository
    writeln('Step 7: Repository Check'),
    ( repository:fact(pred(cho_an, [nhan, miu])) ->
        writeln('  ✓ Fact exists: cho_an(nhan, miu)')
    ;
        writeln('  ✗ Fact NOT found!')
    ),
    nl,
    
    writeln('=== END DEBUG ===').

:- initialization(test_query, main).
