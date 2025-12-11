% ========================================
% INTERFACE MODULE: SHELL
% Interactive REPL Interface
% ========================================

:- module(shell, [
    start_repl/0
]).

% ========================================
% REPL - Read-Eval-Print Loop
% ========================================

start_repl :-
    writeln('Interactive mode. Type your question or "exit" to quit.'),
    writeln('Examples:'),
    writeln('  > Gau hien khong'),
    writeln('  > Ai dat Gau'),
    writeln('  > Meo an gi'),
    nl,
    repl_loop.

repl_loop :-
    write('> '),
    read_line_to_string(user_input, Input),
    ( Input = "exit" ->
        writeln('Goodbye!')
    ; Input = "quit" ->
        writeln('Goodbye!')
    ; Input = "" ->
        repl_loop
    ;
        process_input(Input),
        nl,
        repl_loop
    ).

% ========================================
% INPUT PROCESSING
% ========================================

process_input(Input) :-
    ( detect_type(Input, Type) ->
        catch(
            bootstrap:query(Input, Type),
            Error,
            (format('Error: ~w~n', [Error]))
        )
    ;
        writeln('Could not determine question type.'),
        writeln('Please use: "X Y khong" (yes/no) or "Ai ..." (who) or "... gi" (what)')
    ).

% ========================================
% QUESTION TYPE DETECTION
% ========================================

detect_type(Input, yn) :-
    ( sub_string(Input, _, _, _, " khong")
    ; sub_string(Input, _, _, _, " ko")
    ; sub_string(Input, _, _, 0, "khong")
    ; sub_string(Input, _, _, 0, "ko")
    ), !.

detect_type(Input, who) :-
    ( sub_string(Input, 0, 2, _, "Ai")
    ; sub_string(Input, 0, 2, _, "ai")
    ), !.

detect_type(Input, what) :-
    ( sub_string(Input, _, _, _, " gi")
    ; sub_string(Input, _, _, 0, "gi")
    ), !.

detect_type(_, yn).  % Default to yes/no
