% ========================================
% INTERFACE MODULE: SHELL
% Interactive REPL Interface with Discourse
% ========================================

:- module(shell, [
    start_repl/0
]).

% Load the main bootstrap which contains query/2
:- include('../bootstrap').
:- use_module('../logic/discourse').

% ========================================
% REPL - Read-Eval-Print Loop
% ========================================

start_repl :-
    writeln('Chế độ tương tác. Gõ "exit" để thoát.'),
    writeln(''),
    writeln('Các loại câu hỏi hỗ trợ:'),
    writeln('  [what]  Subject + Verb + gi     : Linh thich gi'),
    writeln('  [who]   Ai + Verb + Object      : Ai cho an Miu'),
    writeln('  [who]   X la cua ai             : Xe dap la cua ai'),
    writeln('  [yn]    S + V + O + khong       : Linh thich hoa khong'),
    writeln(''),
    writeln('Câu phức tạp với đại từ:'),
    writeln('  > Xe dap mau xanh. No la cua ai'),
    writeln('  > Miu de thuong khong'),
    writeln(''),
    writeln('Ví dụ với data hiện tại:'),
    writeln('  > Meo ten gi'),
    writeln('  > Ai so huu xe dap'),
    writeln('  > Xe dap la cua ai'),
    writeln('  > Nhan song cung ai'),
    nl,
    discourse:init_discourse,
    repl_loop.

repl_loop :-
    write('> '),
    read_line_to_string(user_input, Input),
    ( Input = "exit" ->
        writeln('Goodbye!')
    ; Input = "quit" ->
        writeln('Goodbye!')
    ; Input = "clear" ->
        discourse:clear_discourse,
        writeln('Discourse context cleared.'),
        repl_loop
    ; Input = "context" ->
        show_discourse_context,
        repl_loop
    ; Input = "" ->
        repl_loop
    ;
        process_input(Input),
        nl,
        repl_loop
    ).

% Show current discourse entities
show_discourse_context :-
    discourse:get_discourse_entities(Entities),
    ( Entities = [] ->
        writeln('No entities in discourse context.')
    ;
        writeln('Current discourse entities:'),
        forall(member(entity(E, T, F), Entities),
               format('  ~w (~w) - ~w~n', [E, T, F]))
    ).

% ========================================
% INPUT PROCESSING
% ========================================

process_input(Input) :-
    % Check if input contains multiple sentences
    ( sub_string(Input, _, _, _, ".") ->
        process_multi_input(Input)
    ;
        process_single_input(Input)
    ).

% Process multiple sentences
process_multi_input(Input) :-
    split_string(Input, ".", "", Parts),
    exclude(empty_part, Parts, NonEmpty),
    process_sentence_list(NonEmpty).

empty_part("").
empty_part(S) :- 
    string_codes(S, Codes),
    maplist(is_whitespace, Codes).

is_whitespace(32).
is_whitespace(9).
is_whitespace(10).
is_whitespace(13).

process_sentence_list([]).
process_sentence_list([Sent|Rest]) :-
    string_trim_ws(Sent, Trimmed),
    ( Trimmed \= "" ->
        process_single_input(Trimmed)
    ;
        true
    ),
    process_sentence_list(Rest).

string_trim_ws(S, Trimmed) :-
    split_string(S, "", " \t\n\r", [Trimmed]).

% Process single sentence
process_single_input(Input) :-
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
% Matches all patterns from CS229 test suite
% ========================================

% YES/NO: "... khong", "... phai khong", "... ko"
detect_type(Input, yn) :-
    ( sub_string(Input, _, _, _, "khong")
    ; sub_string(Input, _, _, _, "ko")
    ; sub_string(Input, _, _, _, "phai")
    ), !.

% WHERE: "... o dau", "... ở đâu"
detect_type(Input, where) :-
    ( sub_string(Input, _, _, _, "o dau")
    ; sub_string(Input, _, _, _, "dau")
    ), !.

% WHO: "Ai ...", "... la cua ai", "... của ai"
detect_type(Input, who) :-
    ( sub_string(Input, 0, 2, _, "Ai")
    ; sub_string(Input, 0, 2, _, "ai")
    ; sub_string(Input, _, _, _, "cua ai")
    ; sub_string(Input, _, _, _, "la cua ai")
    ), !.

% WHAT: "... gi", "... gì", "... la gi"
detect_type(Input, what) :-
    ( sub_string(Input, _, _, _, " gi")
    ; sub_string(Input, _, _, 0, "gi")
    ; sub_string(Input, _, _, _, "la gi")
    ), !.

% Default: try as statement (yes/no verification)
detect_type(_, yn).

