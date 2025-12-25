% ========================================
% LOGIC MODULE: DISCOURSE
% DRS (Discourse Representation Structure) - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-BUOI-10, 11:
%
% CẤU TRÚC DRS gồm 2 phần:
%    drs(Referents, Conditions)
%    - Referents: Danh sách các sở chỉ (biến, hằng)
%    - Conditions: Danh sách các điều kiện (vị từ)
%
% Ký hiệu:
%    {x, y, z}, {P(x), Q(x,y), ...}
%
% PHÉP KẾT HỢP DRS (⊕):
%    A ⊕ B = drs(Ra ∪ Rb, Ca ∪ Cb)
%    trong đó A = drs(Ra, Ca) và B = drs(Rb, Cb)
%
% XÁC ĐỊNH ĐỒNG SỞ CHỈ:
%    - Đại từ "nó" cần tìm tiền ngữ (antecedent)
%    - Ưu tiên: vật gần nhất phù hợp về giới tính/số lượng
%
% ========================================

:- module(discourse, [
    build_drs/2,
    merge_drs/3,
    negate_drs/2,
    pretty_print_drs/1,
    simplify_drs/2,
    % Discourse context
    init_discourse/0,
    update_discourse/1,
    resolve_pronoun/2,
    get_discourse_entities/1
]).

:- use_module('../knowledge/repository').

% Dynamic predicates for discourse context
:- dynamic discourse_entity/3.   % discourse_entity(Entity, Type, Features)
:- dynamic discourse_order/2.    % discourse_order(Entity, Position) - thứ tự xuất hiện

% ========================================
% KHỞI TẠO DISCOURSE CONTEXT
% ========================================

init_discourse :-
    retractall(discourse_entity(_, _, _)),
    retractall(discourse_order(_, _)).

% ========================================
% CẬP NHẬT DISCOURSE CONTEXT
% Thêm entity mới vào context để dùng cho anaphora resolution
% ========================================

update_discourse(entity(Entity, Type)) :-
    ( discourse_order(_, MaxPos) ->
        aggregate_all(max(P), discourse_order(_, P), MaxPos),
        NewPos is MaxPos + 1
    ;
        NewPos = 1
    ),
    get_entity_features(Entity, Type, Features),
    ( discourse_entity(Entity, _, _) ->
        true  % Entity đã tồn tại
    ;
        assertz(discourse_entity(Entity, Type, Features)),
        assertz(discourse_order(Entity, NewPos))
    ).

% Lấy đặc điểm của entity
get_entity_features(Entity, person, features(third, singular, human)) :- 
    repository:entity(Entity, person), !.
get_entity_features(Entity, animal, features(third, singular, animate)) :- 
    repository:entity(Entity, animal), !.
get_entity_features(_, object, features(third, singular, inanimate)) :- !.
get_entity_features(_, _, features(third, singular, any)).

% Lấy tất cả entities trong discourse
get_discourse_entities(Entities) :-
    findall(E, discourse_entity(E, _, _), Entities).

% ========================================
% XÁC ĐỊNH ĐỒNG SỞ CHỈ (ANAPHORA RESOLUTION)
% Theo Slide-BUOI-10 trang 20-24
% ========================================

% resolve_pronoun(PronounRef, ResolvedEntity)
resolve_pronoun(pronoun_ref(_, _, Features), Entity) :-
    Features = features(Person, Number, Category),
    findall(E-Pos,
        ( discourse_entity(E, _, features(Person, Number, ECategory)),
          matches_category(Category, ECategory),
          discourse_order(E, Pos)
        ),
        Matches),
    % Lấy entity gần nhất (position cao nhất)
    sort(2, @>=, Matches, [Entity-_|_]).

% Matching category cho đại từ
matches_category(any, _) :- !.
matches_category(human, human) :- !.
matches_category(animate, human) :- !.
matches_category(animate, animate) :- !.
matches_category(inanimate, inanimate) :- !.
matches_category(X, X) :- !.

% ========================================
% XÂY DỰNG DRS TỪ BIỂU THỨC LAMBDA
% Theo Slide-BUOI-10, 11
% ========================================

% --- DRS cho pred(P, Args) ---
% Kết quả: drs([], [pred(P, Args)])
% Không có sở chỉ mới, chỉ có điều kiện
build_drs(pred(P, Args), drs([], [pred(P, Args)])) :- !.

% --- DRS cho const(E) ---
% Hằng được thêm vào sở chỉ
build_drs(const(E), drs([E], [])) :- !.

% --- DRS cho exists(X, Body) ---
% ∃X. Body → drs([X|Refs], Conds)
% X được thêm vào phần sở chỉ
build_drs(exists(X, Body), drs([X|BodyRefs], BodyConds)) :- !,
    build_drs(Body, drs(BodyRefs, BodyConds)).

% --- DRS cho forall(X, Body) ---
% ∀X. Body → điều kiện phức: drs([X],{}) → Body
build_drs(forall(X, Body), drs([], [impl(drs([X], []), BodyDRS)])) :- !,
    build_drs(Body, BodyDRS).

% --- DRS cho conj(A, B) ---
% A ∧ B → A ⊕ B (phép kết hợp DRS)
build_drs(conj(A, B), MergedDRS) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB),
    merge_drs(DRSA, DRSB, MergedDRS).

% --- DRS cho impl(A, B) ---
% A → B → drs([], [impl(DRS_A, DRS_B)])
build_drs(impl(A, B), drs([], [impl(DRSA, DRSB)])) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB).

% --- DRS cho neg(A) ---
% ¬A → drs([], [neg(DRS_A)])
build_drs(neg(A), drs([], [neg(DRSA)])) :- !,
    build_drs(A, DRSA).

% --- DRS cho wh_question ---
% Câu hỏi: giữ nguyên để theorem prover xử lý
build_drs(wh_question(Type, Body), wh_question(Type, Body)) :- !.

% --- DRS cho application (sau beta reduction) ---
build_drs(app(F, A), DRS) :- !,
    build_drs(F, DRSF),
    build_drs(A, DRSA),
    merge_drs(DRSF, DRSA, DRS).

% --- DRS cho lambda (chưa được reduce hết) ---
build_drs(lambda(_, Body), DRS) :- !,
    build_drs(Body, DRS).

% --- Fallback ---
build_drs(X, drs([], [X])) :- !.

% ========================================
% PHÉP KẾT HỢP DRS (⊕)
% A ⊕ B = drs(Ra ∪ Rb, Ca ∪ Cb)
% Theo Slide-BUOI-10 trang 35
% ========================================

merge_drs(drs(RefsA, CondsA), drs(RefsB, CondsB), drs(MergedRefs, MergedConds)) :- !,
    append(RefsA, RefsB, AllRefs),
    sort(AllRefs, MergedRefs),  % Loại bỏ duplicates
    append(CondsA, CondsB, MergedConds).

% Handle non-DRS cases
merge_drs(wh_question(Type, Body), _, wh_question(Type, Body)) :- !.
merge_drs(_, wh_question(Type, Body), wh_question(Type, Body)) :- !.
merge_drs(X, Y, conj(X, Y)).

% ========================================
% PHỦ ĐỊNH DRS
% ========================================

negate_drs(drs(Refs, Conds), drs([], [neg(drs(Refs, Conds))])) :- !.
negate_drs(X, neg(X)).

% ========================================
% ĐƠN GIẢN HÓA DRS
% ========================================

simplify_drs(drs(Refs, Conds), drs(SimpRefs, SimpConds)) :- !,
    sort(Refs, SimpRefs),
    simplify_conditions(Conds, SimpConds).

simplify_drs(X, X).

simplify_conditions([], []) :- !.
simplify_conditions([Cond|Rest], [SimpCond|SimpRest]) :-
    simplify_condition(Cond, SimpCond),
    simplify_conditions(Rest, SimpRest).

simplify_condition(pred(P, Args), pred(P, SimpArgs)) :- !,
    maplist(simplify_arg, Args, SimpArgs).
simplify_condition(neg(DRS), neg(SimpDRS)) :- !,
    simplify_drs(DRS, SimpDRS).
simplify_condition(impl(A, B), impl(SimpA, SimpB)) :- !,
    simplify_drs(A, SimpA),
    simplify_drs(B, SimpB).
simplify_condition(X, X).

simplify_arg(const(X), X) :- !.
simplify_arg(X, X).

% ========================================
% IN DRS ĐẸP (PRETTY PRINT)
% Theo format trong slides
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
    pretty_print_drs_body(Body).

pretty_print_drs(X) :-
    format('~w~n', [X]).

pretty_print_drs_body(pred(P, Args)) :- !,
    format('  ~w(', [P]),
    print_args(Args),
    writeln(')').
pretty_print_drs_body(lambda(X, Body)) :- !,
    format('  λ~w. ', [X]),
    pretty_print_drs_body(Body).
pretty_print_drs_body(X) :-
    format('  ~w~n', [X]).

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

print_condition(pred(P, Args)) :- !,
    write(P), write('('),
    print_args(Args),
    write(')').

print_condition(neg(DRS)) :- !,
    write('¬'),
    print_condition_drs(DRS).

print_condition(impl(A, B)) :- !,
    print_condition_drs(A),
    write(' → '),
    print_condition_drs(B).

print_condition(X) :-
    write(X).

print_condition_drs(drs(Refs, Conds)) :-
    write('{'),
    print_refs(Refs),
    write('}, {'),
    print_cond_list(Conds),
    write('}').

print_cond_list([]) :- !.
print_cond_list([C]) :- !,
    print_condition(C).
print_cond_list([C|Cs]) :-
    print_condition(C),
    write(', '),
    print_cond_list(Cs).

% In các arguments
print_args([]) :- !.
print_args([A]) :- !,
    print_arg(A).
print_args([A|As]) :-
    print_arg(A), write(', '),
    print_args(As).

print_arg(const(X)) :- !, write(X).
print_arg(X) :- write(X).

% ========================================
% UTILITIES
% ========================================

% Lấy các referent tự do trong DRS
free_referents(drs(Refs, Conds), Free) :-
    findall(R, 
        ( member(Cond, Conds),
          referent_in_condition(R, Cond),
          \+ member(R, Refs)
        ),
        FreeList),
    sort(FreeList, Free).

referent_in_condition(Ref, pred(_, Args)) :-
    member(Ref, Args),
    \+ is_const(Ref).

referent_in_condition(Ref, neg(DRS)) :-
    free_referents(DRS, Free),
    member(Ref, Free).

referent_in_condition(Ref, impl(A, B)) :-
    ( free_referents(A, FreeA), member(Ref, FreeA)
    ; free_referents(B, FreeB), member(Ref, FreeB)
    ).

is_const(const(_)) :- !.
is_const(X) :- atom(X), !.
