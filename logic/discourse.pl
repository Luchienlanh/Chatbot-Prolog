% ========================================
% LOGIC MODULE: DISCOURSE
% DRS (Discourse Representation Structure) - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-BUOI-10, 11:
%
% CẤU TRÚC DRS gồm 2 phần (Trang 26):
%    drs(Referents, Conditions)
%    - Referents: Danh sách các sở chỉ (biến, hằng)
%    - Conditions: Danh sách các điều kiện (vị từ)
%
% Ký hiệu: {x, y, z}, {P(x), Q(x,y), ...}
%
% PHÉP KẾT HỢP DRS ⊕ (Trang 35):
%    A ⊕ B = drs(DA ∪ DB, CA ∪ CB)
%    trong đó A = drs(DA, CA) và B = drs(DB, CB)
%
% XÁC ĐỊNH ĐỒNG SỞ CHỈ (Trang 40):
%    Cho Y là hồi chỉ, có thể thêm điều kiện Y=a
%    vào tập điều kiện khi a là sở chỉ phù hợp
%
% ========================================

:- module(discourse, [
    build_drs/2,
    merge_drs/3,
    negate_drs/2,
    pretty_print_drs/1,
    % Discourse context
    init_discourse/0,
    update_discourse/1,
    resolve_coreference/3,
    init_discourse/0,
    update_discourse/1,
    resolve_coreference/3,
    get_discourse_entities/1,
    resolve_drs/2]).

% Dynamic predicates for discourse context
:- dynamic discourse_entity/2.   % discourse_entity(Entity, Position)
:- dynamic discourse_position/1. % Current position counter

% ========================================
% KHỞI TẠO DISCOURSE CONTEXT
% ========================================

init_discourse :-
    retractall(discourse_entity(_, _)),
    retractall(discourse_position(_)),
    assertz(discourse_position(0)).

% ========================================
% CẬP NHẬT DISCOURSE CONTEXT
% Thêm entity mới vào context để dùng cho coreference resolution
% Theo Slide-BUOI-10 Trang 40
% ========================================

update_discourse(Entity) :-
    discourse_position(Pos),
    NewPos is Pos + 1,
    retract(discourse_position(Pos)),
    assertz(discourse_position(NewPos)),
    ( discourse_entity(Entity, _) -> 
        true  % Entity đã tồn tại
    ; 
        assertz(discourse_entity(Entity, NewPos))
    ).

% Lấy tất cả entities trong discourse
get_discourse_entities(Entities) :-
    findall(E, discourse_entity(E, _), Entities).

% ========================================
% PHÉP KẾT HỢP DRS ⊕ (Slide-BUOI-10, Trang 35)
%
% "Cho A={DA,CA} và B={DB,CB} là hai DRS.
%  Phép toán kết hợp DRS cho kết quả là một DRS T={DT,CT} sao cho:
%  DT = DA ∪ DB
%  CT = CA ∪ CB"
% ========================================

merge_drs(drs(RefsA, CondsA), drs(RefsB, CondsB), drs(MergedRefs, MergedConds)) :- !,
    union(RefsA, RefsB, MergedRefs),
    append(CondsA, CondsB, MergedConds).

% Merge với non-DRS terms
merge_drs(drs(Refs, Conds), Term, drs(Refs, [Term|Conds])) :- 
    Term \= drs(_, _), !.
merge_drs(Term, drs(Refs, Conds), drs(Refs, [Term|Conds])) :- 
    Term \= drs(_, _), !.
merge_drs(A, B, merge(A, B)).

% ========================================
% XÂY DỰNG DRS TỪ BIỂU THỨC LAMBDA
% Theo Slide-BUOI-11
% ========================================

% DRS đã là dạng chuẩn
build_drs(drs(Refs, Conds), drs(Refs, Conds)) :- !.

% Phép kết hợp merge (⊕)
build_drs(merge(A, B), Result) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB),
    merge_drs(DRSA, DRSB, Result).

% Implication (cho định từ "mọi")
% Theo Slide-BUOI-11 Trang 13
build_drs(impl(A, B), drs([], [impl(DRSA, DRSB)])) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB).

% Negation
build_drs(neg(A), drs([], [neg(DRSA)])) :- !,
    build_drs(A, DRSA).

% Disjunction (cho "hoặc")
% Theo Slide-BUOI-11 Trang 12
build_drs(or(A, B), drs([], [or(DRSA, DRSB)])) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB).

% Pronoun DRS - cần xác định sở chỉ
% Theo Slide-BUOI-11 Trang 14: ˣ{X},{} ⊕ (P@X)
build_drs(pronoun_drs(Var, Gender, Body), pronoun_drs(Var, Gender, DRSBody)) :- !,
    build_drs(Body, DRSBody).

% WH-question - giữ nguyên để theorem prover xử lý
build_drs(wh_question(Type, Body), wh_question(Type, Body)) :- !.

% Predicate condition: R(X1,..,Xn) -> drs([],[R(X1,..,Xn)])
% Theo Slide-BUOI-10 Trang 29
build_drs(Pred, drs([], [Pred])) :- 
    Pred =.. [Functor|Args],
    Args \= [],
    \+ member(Functor, [drs, merge, impl, neg, or, lambda, app, wh_question, pronoun_drs]),
    !.

% Atom (constant reference)
build_drs(X, drs([X], [])) :- 
    atom(X), 
    \+ member(X, [true, false]),
    !.

% Application - reduce first
build_drs(app(F, A), DRS) :- !,
    build_drs(F, _),
    build_drs(A, _),
    DRS = app(F, A).  % Keep as app for beta reduction

% Lambda - build body
build_drs(lambda(Var, Body), lambda(Var, DRSBody)) :- !,
    build_drs(Body, DRSBody).

% Fallback
build_drs(X, X).

% ========================================
% XÁC ĐỊNH ĐỒNG SỞ CHỈ (Slide-BUOI-10, Trang 40)
%
% "Cho Y là hồi chỉ, DRS Φ2={Y,...},{...}, a là một sở chỉ
%  xuất hiện trong tất cả DRS có thể xác định từ Φ2.
%  Khi đó, có thể thêm điều kiện Y=a vào tập điều kiện của Φ2."
%
% Các khía cạnh cần xét:
% - Chỉ người hay chỉ vật
% - Giới tính
% - Số nhiều hay số ít
% - Vai trò chủ từ hay tân từ
% ========================================

% resolve_coreference(+PronounDRS, +AvailableReferents, -ResolvedDRS)
% Xác định đồng sở chỉ cho đại từ
% Có kiểm tra Gender (Slide-BUOI-10 Trang 40)
resolve_coreference(pronoun_drs(Var, Gender, drs(Refs, Conds)), AvailableRefs, 
                    drs([Var|Refs], [Var=Antecedent|Conds])) :-
    % Tìm sở chỉ phù hợp giới tính trong danh sách (ưu tiên gần nhất)
    find_matching_antecedent(Gender, AvailableRefs, Antecedent), !.

resolve_coreference(pronoun_drs(Var, Gender, DRS), _, DRS) :-
    % Fallback: không tìm được tiền ngữ
    DRS = drs(Refs, Conds),
    format('Warning: Cannot resolve coreference for ~w (~w)~n', [Var, Gender]).

resolve_coreference(DRS, _, DRS) :-   % Non-pronoun DRS
    DRS \= pronoun_drs(_, _, _).

% Helper: Tìm Antecedent phù hợp giới tính
find_matching_antecedent(plural, [Ref|_], Ref). % Tạm chấp nhận mọi thứ cho số nhiều
find_matching_antecedent(Gender, [Ref|Rest], Result) :-
    ( check_gender(Ref, Gender) ->
        Result = Ref
    ;
        find_matching_antecedent(Gender, Rest, Result)
    ).

% Check gender trong KB
check_gender(Entity, Gender) :-
    repository:fact(pred(gender, [Entity, Gender])), !.
% Fallback nếu không biết giới tính (assume match để tránh lỗi)
check_gender(Entity, _) :-
    \+ repository:fact(pred(gender, [Entity, _])).

% ========================================
% RESOLVE DRS (Recursive)
% ========================================

resolve_drs(DRS, ResolvedDRS) :-
    get_discourse_entities(GlobalRefs),
    resolve_drs_rec(DRS, GlobalRefs, ResolvedDRS).

resolve_drs_rec(drs(Refs, Conds), Context, drs(Refs, ResolvedConds)) :-
    append(Context, Refs, LocalContext),
    maplist(resolve_cond(LocalContext), Conds, ResolvedConds).
resolve_drs_rec(X, _, X).

resolve_cond(Context, pronoun_drs(Var, Gender, Body), Result) :-
    build_drs(Body, drs(BRefs, BConds)),
    ( find_matching_antecedent(Gender, Context, Antecedent) ->
        % Found: Add Var=Antecedent condition
        NewConds = [Var=Antecedent | BConds],
        resolve_drs_rec(drs(BRefs, NewConds), Context, ResolvedBody),
        Result = pronoun_drs(Var, Gender, ResolvedBody)
    ;
        % Not found: Keep as is (or warn)
        resolve_drs_rec(Body, Context, ResolvedBody),
        Result = pronoun_drs(Var, Gender, ResolvedBody)
    ).

resolve_cond(Context, neg(DRS), neg(ResolvedDRS)) :-
    resolve_drs_rec(DRS, Context, ResolvedDRS).

resolve_cond(Context, impl(A, B), impl(RA, RB)) :-
    resolve_drs_rec(A, Context, RA),
    resolve_drs_rec(B, Context, RB).

resolve_cond(Context, or(A, B), or(RA, RB)) :-
    resolve_drs_rec(A, Context, RA),
    resolve_drs_rec(B, Context, RB).

resolve_cond(_, Cond, Cond).

% ========================================
% PHỦ ĐỊNH DRS
% ========================================

negate_drs(drs(Refs, Conds), drs([], [neg(drs(Refs, Conds))])) :- !.
negate_drs(X, neg(X)).

% ========================================
% IN DRS ĐẸP (PRETTY PRINT)
% Theo format trong Slide-BUOI-10 Trang 26
% ========================================

pretty_print_drs(drs(Refs, Conds)) :- !,
    writeln('+------------------------------------------+'),
    write('| '),
    print_refs(Refs),
    writeln(' |'),
    writeln('+------------------------------------------+'),
    print_conditions(Conds),
    writeln('+------------------------------------------+').

pretty_print_drs(wh_question(Type, Body)) :- !,
    format('WH-Question (~w):~n', [Type]),
    write('  '), writeln(Body).

pretty_print_drs(pronoun_drs(Var, DRS)) :- !,
    format('Pronoun ^~w:~n', [Var]),
    pretty_print_drs(DRS).

pretty_print_drs(X) :-
    format('~w~n', [X]).

% In danh sách sở chỉ
print_refs([]) :- !.
print_refs([R]) :- !, write(R).
print_refs([R|Rs]) :-
    write(R), write(', '),
    print_refs(Rs).

% In các điều kiện
print_conditions([]) :- !.
print_conditions([Cond|Conds]) :-
    write('| '),
    print_condition(Cond),
    writeln(' |'),
    print_conditions(Conds).

print_condition(impl(A, B)) :- !,
    print_mini_drs(A),
    write(' → '),
    print_mini_drs(B).

print_condition(neg(A)) :- !,
    write('¬'),
    print_mini_drs(A).

print_condition(or(A, B)) :- !,
    print_mini_drs(A),
    write(' ∨ '),
    print_mini_drs(B).

print_condition(X=Y) :- !,
    format('~w = ~w', [X, Y]).

print_condition(Pred) :-
    Pred =.. [Functor|Args],
    format('~w(', [Functor]),
    print_args(Args),
    write(')').

print_mini_drs(drs(Refs, Conds)) :- !,
    write('{'),
    print_refs(Refs),
    write('},{'),
    print_conds_inline(Conds),
    write('}').

print_mini_drs(X) :- write(X).

print_conds_inline([]) :- !.
print_conds_inline([C]) :- !, print_condition(C).
print_conds_inline([C|Cs]) :-
    print_condition(C),
    write(', '),
    print_conds_inline(Cs).

print_args([]) :- !.
print_args([A]) :- !, write(A).
print_args([A|As]) :-
    write(A), write(', '),
    print_args(As).
