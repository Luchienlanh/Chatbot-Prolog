% ========================================
% QUERIES CS229 - CÂU HỎI THEO FORMAT SLIDES
% ========================================
%
% Theo Slide-DOAN-01, Trang 12-14:
%
% DẠNG CÂU HỎI ĐƯỢC HỖ TRỢ:
% 1. Who: "Ai thích X?" → Vị_từ(?, X)
% 2. What: "X thích gì?" → Vị_từ(X, ?)
% 3. Yes/No (đa câu): "Một chiếc xe tốt. Nó là của Bình?" → đồng sở chỉ
%
% QUAN HỆ VÀ CONSTANTS CÓ TRONG KNOWLEDGE BASE:
% Constants: nhan, linh, miu, bo_nhan, xe_dap_nhan, cac_bong_hoa, khu_vuon
% Relations: thich/2, so_huu/2, cho_an/2, song_cung/2, choi_voi/2, ngam/2
%
% ========================================

:- encoding(utf8).
:- consult('bootstrap.pl').

% ========================================
% CÂU HỎI DẠNG WHO (Slide-DOAN-01, Trang 12)
% Pattern: Ai + VP => Vị_từ(X, a)
% ========================================

% Sử dụng proper nouns (constants) từ KB
test_case('who1', who, "Ai thich Miu?").           % thich(?, miu) -> ?
test_case('who2', who, "Ai cho Miu an?").          % cho_an(?, miu) -> [nhan]
test_case('who3', who, "Ai choi voi Miu?").        % choi_voi(?, miu) -> [nhan]
test_case('who4', who, "Ai song cung Linh?").      % song_cung(?, linh) -> [nhan]     % song_cung(?, nhan) -> [linh]

% ========================================
% CÂU HỎI DẠNG WHAT (Slide-DOAN-01, Trang 12)
% Pattern: NP + VP + gì? => Vị_từ(a, X)
% ========================================

test_case('what1', what, "Linh thich gi?").        % thich(linh, ?) -> [xe_dap_nhan, cac_bong_hoa, khu_vuon]
test_case('what2', what, "Linh ngam gi?").         % ngam(linh, ?) -> [cac_bong_hoa]       % so_huu(linh, ?) -> [miu]

% ========================================
% CÂU HỎI DẠNG YES/NO - ĐA CÂU (Slide-DOAN-01, Trang 5)
% Pattern: "Một X tốt. Nó là của Y?" → đồng sở chỉ
% ========================================

% Format đa câu với đồng sở chỉ
test_case('yn1', yn, "Mot con meo. No la cua Linh?").
test_case('yn2', yn, "Mot chiec xe dap. No la cua Nhan?").
test_case('yn3', yn, "Linh thích ngắm xe đạp?").
test_case('yn4', yn, "Linh thích hoa?").
test_case('yn5', yn, "Nhan so huu xe dap?").
test_case('yn6', yn, "Nhan song cung Linh?").
test_case('yn7', yn, "Nhan song cung Nhan?").

% ========================================
% MAIN EXECUTION
% ========================================

run_all_tests :-
    initialize,
    nl,
    writeln('========================================'),
    writeln('CS229 QUERY SUITE - THEO SLIDES'),
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
    ( catch(query(Text, Type), Error, format('ERROR: ~w~n', [Error]))
    -> true
    ;  format('FAILURE: query returned false~n', [])
    ),
    run_tests_list(Rest).

% Run single test
run_test(Id) :-
    initialize,
    test_case(Id, Type, Text),
    format('TEST [~w] Type: ~w~n', [Id, Type]),
    format('Question: ~s~n', [Text]),
    query(Text, Type).
% Negation Test
test_case('neg1', yn, "Linh khong thich hoa?").
