
:- encoding(utf8).
:- consult('bootstrap.pl').

% Define test cases based on CS229 dataset
% Focusing on direct questions about the story without context preambles.

% --- Location Questions ---
test_case('nq1', where, "Miu ở đâu?").
test_case('nq2', where, "Khu vườn ở đâu?"). ## 3

% --- Ownership Questions ---
test_case('nq3', who, "Xe đạp là của ai?"). ## 2
test_case('nq4', who, "Ai sở hữu xe đạp?").

% --- Relationship Questions ---
test_case('nq6', who, "Ai là em gái của Nhân?").
test_case('nq7', what, "Linh là gì của Nhân?"). ## 1
test_case('nq8', who, "Ai sống cùng Linh?").

% --- Action/Preference Questions ---
test_case('nq9', who, "Ai cho Miu ăn?").
test_case('nq10', who, "Ai chơi với Miu?").
test_case('nq11', what, "Linh thích gì?").
test_case('nq12', what, "Linh ngắm gì?").
test_case('nq13', who, "Ai thích ngắm hoa?").


% --- Yes/No Questions ---
test_case('nq14', yn, "Linh thích xe đạp phải không?").
test_case('nq15', yn, "Nhân tặng Linh xe đạp phải không?"). % Expect No (Dad gave it)
test_case('nq16', yn, "Miu ngủ trong phòng khách phải không?").
test_case('nq17', yn, "Khu vườn ở sau nhà phải không?"). ## 4 
test_case('nq18', yn, "Nhân sống tại ngoại ô phải không?"). ## 5
test_case('nq19', yn, "Mot chiec xe mau xanh. No la cua Linh?").
test_case('nq20', yn, "Linh co mot con meo. No ten la Miu?").
test_case('nq21', yn, "Nguoi cho Miu an. Do la Nhan?").
test_case('nq22', yn, "Linh thich ngam hoa. Chung o trong vuon?").

% Main execution loop
run_all_tests :-
    initialize,
    nl,
    writeln('========================================'),
    writeln('STARTING CS229 QUERY SUITE'),
    writeln('========================================'),
    nl,
    findall(test(Id, Type, Text), test_case(Id, Type, Text), Tests),
    run_tests_list(Tests),
    nl,
    writeln('========================================'),
    writeln('SUITE COMPLETED'),
    writeln('========================================'),
    halt.

run_tests_list([]).
run_tests_list([test(Id, Type, Text)|Rest]) :-
    format('~n--------------------------------------------------------------------------------~n', []),
    format('TEST [~w] Type: ~w~n', [Id, Type]),
    format('Question: ~s~n', [Text]),
    ( catch(query(Text, Type), Error, format('ERROR in test ~w: ~w~n', [Id, Error]))
    -> true
    ;  format('FAILURE in test ~w: query returned false~n', [Id])
    ),
    run_tests_list(Rest).
