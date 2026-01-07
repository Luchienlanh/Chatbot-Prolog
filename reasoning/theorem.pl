% ========================================
% REASONING MODULE: THEOREM
% Theorem Proving Engine - THEO SLIDES MÔN HỌC
% ========================================
%
% Theo Slide-DOAN-01:
%
% CHỨC NĂNG:
% 1. prove/1 - Chứng minh một công thức FOL
% 2. find_entities/2 - Tìm các entity thỏa mãn wh_question(who, ...)
% 3. find_values/2 - Tìm các giá trị thỏa mãn wh_question(what/where, ...)
%
% ========================================

:- module(theorem, [
    prove/1,
    prove_with_trace/1,
    find_entities/2,
    find_entities/3,  % find_entities(QueryType, FOL, Entities) - FOL thuần túy
    find_values/2,
    find_values/3,    % find_values(QueryType, FOL, Values) - FOL thuần túy
    initialize_prover/0
]).

:- discontiguous find_entities/2.
:- discontiguous find_entities/3.
:- discontiguous find_values/2.
:- discontiguous find_values/3.

:- use_module('../knowledge/repository').

% ========================================
% INITIALIZATION
% ========================================

initialize_prover :- true.

% ========================================
% HELPER: STRIP CONST WRAPPER
% Chuyển const(X) thành X để khớp với KB
% ========================================

strip_const(const(X), X) :- !.
strip_const(X, X).

strip_const_args([], []).
strip_const_args([A|As], [SA|SAs]) :-
    strip_const(A, SA),
    strip_const_args(As, SAs).

% ========================================
% PROVE - CHỨNG MINH CÔNG THỨC FOL
% ========================================

% true luôn đúng
prove(true) :- !.

% false luôn sai
prove(false) :- !, fail.

% Equality check
prove(X = Y) :- !, X = Y.

% pred(P, Args) - Kiểm tra trong knowledge base
% Strip const() wrapper trước khi tìm
prove(pred(P, Args)) :- 
    strip_const_args(Args, StrippedArgs),
    ( repository:fact(pred(P, StrippedArgs))
    ; repository:rule(pred(P, StrippedArgs), Body),
      prove_rule_body(Body)
    ).

prove_rule_body([]).
prove_rule_body([Cond|Conds]) :-
    prove(Cond),
    prove_rule_body(Conds).

% Inference rules
% vi_tri(X, Y) :- chua(Y, X). (Garden contains flowers -> flowers in garden)
prove(pred(vi_tri, [X, Y])) :-
    strip_const(X, SX), strip_const(Y, SY),
    prove(pred(chua, [const(SY), const(SX)])).

% and(A, B) - A ∧ B đúng khi cả A và B đều đúng
prove(and(A, B)) :- !,
    prove(A),
    prove(B).

% or(A, B) - A ∨ B đúng khi A hoặc B đúng
prove(or(A, B)) :- !,
    ( prove(A) ; prove(B) ).

% neg(A) - ¬A đúng khi A sai
prove(neg(A)) :- !,
    \+ prove(A).

% impl(A, B) - A → B ≡ ¬A ∨ B
prove(impl(A, B)) :- !,
    ( \+ prove(A) ; prove(B) ).

% exists(Var, Body) - ∃X. Body đúng khi tồn tại X thỏa mãn Body
prove(exists(Var, Body)) :- !,
    prove_exists(Var, Body).

% forall(Var, Body) - ∀X. Body đúng khi mọi X thỏa mãn Body
prove(forall(Var, Body)) :- !,
    prove_forall(Var, Body).

% ========================================
% PROVE EXISTENTIAL (∃X. Body)
% Tìm một giá trị X thỏa mãn Body
% ========================================

prove_exists(Var, Body) :-
    find_candidate_entity(Var, Body, Value),
    substitute_and_prove(Var, Value, Body).

find_candidate_entity(_, Body, Value) :-
    extract_type_from_body(Body, Type),
    repository:entity(Value, Type).

find_candidate_entity(_, _, Value) :-
    repository:entity(Value, _).

extract_type_from_body(and(pred(Type, [_]), _), Type) :- !.
extract_type_from_body(and(_, Rest), Type) :- !,
    extract_type_from_body(Rest, Type).
extract_type_from_body(pred(Type, [_]), Type).

substitute_and_prove(Var, Value, Body) :-
    substitute_in_body(Var, Value, Body, SubBody),
    prove(SubBody).

substitute_in_body(Var, Value, Var, Value) :- !.

substitute_in_body(Var, Value, and(A, B), and(NA, NB)) :- !,
    substitute_in_body(Var, Value, A, NA),
    substitute_in_body(Var, Value, B, NB).

substitute_in_body(Var, Value, or(A, B), or(NA, NB)) :- !,
    substitute_in_body(Var, Value, A, NA),
    substitute_in_body(Var, Value, B, NB).

substitute_in_body(Var, Value, neg(A), neg(NA)) :- !,
    substitute_in_body(Var, Value, A, NA).

substitute_in_body(Var, Value, exists(V, B), exists(V, NB)) :- !,
    ( Var == V -> NB = B  % Shadowing: don't substitute if variable is shadowed
    ; substitute_in_body(Var, Value, B, NB)
    ).

substitute_in_body(Var, Value, forall(V, B), forall(V, NB)) :- !,
    ( Var == V -> NB = B
    ; substitute_in_body(Var, Value, B, NB)
    ).

substitute_in_body(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute_arg(Var, Value), Args, NewArgs).

substitute_in_body(Var, Value, A=B, NA=NB) :- !,
    substitute_arg(Var, Value, A, NA),
    substitute_arg(Var, Value, B, NB).

substitute_in_body(_, _, X, X).

substitute_arg(Var, Value, Var, Value) :- !.
substitute_arg(Var, Value, const(Var), const(Value)) :- !.
substitute_arg(_, _, X, X).

% ========================================
% PROVE UNIVERSAL (∀X. Body)
% ========================================

prove_forall(Var, Body) :-
    \+ (
        find_candidate_entity(Var, Body, Value),
        \+ substitute_and_prove(Var, Value, Body)
    ).

% ========================================
% FIND_ENTITIES/3 và FIND_VALUES/3 - FOL THUẦN TÚY
% Query type được truyền từ input của người dùng
% FOL không có wh_question wrapper - theo đúng format thầy
% ========================================

% --- WHO: find_entities(who, FOL, Entities) ---
% Query type = who => tìm subject (argument thứ 1)
% FOL: exists(..., pred(Predicate, [QueryVar, Object]))
find_entities(who, FOL, Entities) :-
    extract_innermost_pred(FOL, pred(Pred, [_, Object])),
    strip_const(Object, StrippedObj),
    findall(Entity,
        repository:fact(pred(Pred, [Entity, StrippedObj])),
        RawEntities),
    sort(RawEntities, Entities).

% Fallback cho WHO
find_entities(who, _, []).

% --- WHAT: find_values(what, FOL, Values) ---
% Query type = what => tìm object (argument thứ 2)  
% FOL: exists(..., pred(Predicate, [Subject, QueryVar]))
find_values(what, FOL, Values) :-
    extract_innermost_pred(FOL, pred(Pred, [Subject, _])),
    strip_const(Subject, StrippedSubj),
    findall(Value,
        repository:fact(pred(Pred, [StrippedSubj, Value])),
        RawValues),
    sort(RawValues, Values).

% Fallback cho WHAT
find_values(what, _, []).

% --- WHERE: find_values(where, FOL, Values) ---
find_values(where, FOL, Values) :-
    find_values(what, FOL, Values).

% Fallback cho WHERE  
find_values(where, _, []).

% ========================================
% HELPER: Trích xuất thông tin
% Query type đã biết từ query(_, Type) => chỉ cần tìm biến trong FOL
% ========================================

% Extract predicate and find the query variable 
% FOL format: exists(..., pred(Name, [Arg1, Arg2]))
extract_wh_query_info(FOL, Pred, QueryVar) :-
    extract_innermost_pred(FOL, pred(Pred, Args)),
    % Tìm biến (không phải constant)
    member(Arg, Args),
    (var(Arg) ; \+ is_known_constant(Arg)),
    QueryVar = Arg, !.

extract_innermost_pred(exists(_, Body), Pred) :- !,
    extract_innermost_pred(Body, Pred).
extract_innermost_pred(pred(P, Args), pred(P, Args)) :- !.
extract_innermost_pred(and(A, _), Pred) :- 
    extract_innermost_pred(A, Pred), !.
extract_innermost_pred(and(_, B), Pred) :- 
    extract_innermost_pred(B, Pred).

% Check if constant  
is_known_constant(C) :- 
    atom(C),
    (repository:constant(C, _) ; repository:entity(C, _)), !.

% --- WHO format mới: wh_question(who, QueryVar, Body) ---
% FOL Body có dạng: exists(Object, so_huu(QueryVar, Object))
% Tìm tất cả Entities sao cho so_huu(Entity, Object) đúng
find_entities(wh_question(who, QueryVar, Body), Entities) :- 
    nonvar(QueryVar), !,
    % Trích xuất predicate và object từ Body
    extract_pred_object_from_body(Body, Pred, Object),
    strip_const(Object, StrippedObj),
    findall(Entity,
        repository:fact(pred(Pred, [Entity, StrippedObj])),
        RawEntities),
    sort(RawEntities, Entities).

% Helper: Trích xuất predicate và object từ FOL body cho WHO
extract_pred_object_from_body(exists(Object, Rest), Pred, Object) :-
    atom(Object), !,
    extract_pred_from_inner_who(Rest, Pred).
extract_pred_object_from_body(exists(_, Rest), Pred, Object) :- !,
    extract_pred_object_from_body(Rest, Pred, Object).
extract_pred_object_from_body(Pred, FuncName, _) :- 
    Pred =.. [FuncName|_].

extract_pred_from_inner_who(exists(_, Rest), Pred) :- !,
    extract_pred_from_inner_who(Rest, Pred).
extract_pred_from_inner_who(and(A, _), Pred) :- !,
    extract_pred_from_inner_who(A, Pred).
extract_pred_from_inner_who(Pred, Functor) :-
    Pred =.. [Functor|_].

% --- WHO với lambda và app: lambda(x, app(VPSem, x)) (format cũ) ---
find_entities(wh_question(who, lambda(_X, app(VPSem, _))), Entities) :-
    nonvar(VPSem), !,
    extract_pred_and_object_from_lambda(VPSem, Pred, Object),
    strip_const(Object, StrippedObj),
    findall(Entity,
        repository:fact(pred(Pred, [Entity, StrippedObj])),
        RawEntities),
    sort(RawEntities, Entities).

% --- WHO với pred trực tiếp ---
find_entities(wh_question(who, pred(Pred, [_, Object])), Entities) :- 
    nonvar(Object), !,
    strip_const(Object, StrippedObj),
    findall(Entity,
        repository:fact(pred(Pred, [Entity, StrippedObj])),
        RawEntities),
    sort(RawEntities, Entities).

% --- WHO 3-argument ---
find_entities(wh_question(who, pred(Pred, [_, Recipient, Object])), Entities) :-
    nonvar(Recipient), nonvar(Object), !,
    strip_const(Recipient, StrippedRecip),
    strip_const(Object, StrippedObj),
    findall(Entity,
        repository:fact(pred(Pred, [Entity, StrippedRecip, StrippedObj])),
        RawEntities),
    sort(RawEntities, Entities).

% --- WHO với lambda(X, pred(...)) ---
find_entities(wh_question(who, lambda(_X, pred(P, Args))), Entities) :- !,
    strip_const_args(Args, StrippedArgs),
    findall(E,
        ( StrippedArgs = [_, Obj] ->
            repository:fact(pred(P, [E, Obj]))
        ; StrippedArgs = [_, R, Obj] ->
            repository:fact(pred(P, [E, R, Obj]))
        ;
            fail
        ),
        RawEntities),
    sort(RawEntities, Entities).

% Fallback for WHO
find_entities(wh_question(who, Body), Entities) :- !,
    find_entities_from_body(Body, Entities).

find_entities_from_body(pred(Pred, [_, Object]), Entities) :-
    nonvar(Object), !,
    strip_const(Object, StrippedObj),
    findall(Entity, repository:fact(pred(Pred, [Entity, StrippedObj])), RawEntities),
    sort(RawEntities, Entities).

find_entities_from_body(pred(Pred, [Subject, _]), Entities) :-
    nonvar(Subject), !,
    strip_const(Subject, StrippedSubj),
    findall(Entity, repository:fact(pred(Pred, [StrippedSubj, Entity])), RawEntities),
    sort(RawEntities, Entities).

find_entities_from_body(lambda(_, Body), Entities) :- !,
    find_entities_from_body(Body, Entities).

find_entities_from_body(app(_, Body), Entities) :- !,
    find_entities_from_body(Body, Entities).

find_entities_from_body(_, []).

% Helper: extract predicate and object from VP semantics
extract_pred_and_object_from_lambda(lambda(_, pred(P, [_, O])), P, O) :- !.
extract_pred_and_object_from_lambda(lambda(_, pred(P, [_, _, O])), P, O) :- !.
extract_pred_and_object_from_lambda(pred(P, [_, O]), P, O) :- !.
extract_pred_and_object_from_lambda(app(_, Inner), P, O) :- !,
    extract_pred_and_object_from_lambda(Inner, P, O).

% ========================================
% FIND VALUES - TÌM GIÁ TRỊ CHO CÂU HỎI WHAT/WHERE
% ========================================

% --- WHAT format mới: wh_question(what, QueryVar, Body) ---
% FOL Body có dạng: exists(Subject, exists(QueryVar, thich(Subject, QueryVar)))
% Tìm tất cả Values sao cho thic(Subject, Value) đúng
find_values(wh_question(what, QueryVar, Body), Values) :- 
    nonvar(QueryVar), !,
    % Trích xuất predicate và subject từ Body
    extract_pred_subject_from_body(Body, Pred, Subject),
    strip_const(Subject, StrippedSubj),
    findall(Value,
        repository:fact(pred(Pred, [StrippedSubj, Value])),
        RawValues),
    sort(RawValues, Values).

% Helper: Trích xuất predicate và subject từ FOL body
extract_pred_subject_from_body(exists(Subject, Rest), Pred, Subject) :-
    atom(Subject), !,
    extract_pred_from_inner(Rest, Pred).
extract_pred_subject_from_body(exists(_, Rest), Pred, Subject) :- !,
    extract_pred_subject_from_body(Rest, Pred, Subject).
extract_pred_subject_from_body(Pred, Pred, _) :- atom(Pred).

extract_pred_from_inner(exists(_, Rest), Pred) :- !,
    extract_pred_from_inner(Rest, Pred).
extract_pred_from_inner(and(A, _), Pred) :- !,
    extract_pred_from_inner(A, Pred).
extract_pred_from_inner(Pred, Functor) :-
    Pred =.. [Functor|_].

% --- WHAT: "NP V gì" → find object (format cũ) ---
find_values(wh_question(what, pred(Pred, [Subject, _])), Values) :- 
    nonvar(Subject), !,
    strip_const(Subject, StrippedSubj),
    findall(Value,
        repository:fact(pred(Pred, [StrippedSubj, Value])),
        RawValues),
    sort(RawValues, Values).

% --- WHAT với Subject là biến ---
find_values(wh_question(what, pred(Pred, [_, Object])), Values) :- 
    nonvar(Object), !,
    strip_const(Object, StrippedObj),
    findall(Value,
        repository:fact(pred(Pred, [Value, StrippedObj])),
        RawValues),
    sort(RawValues, Values).

% --- WHERE: "NP ở đâu" → find location ---
find_values(wh_question(where, Subject), Values) :- 
    ( atom(Subject) ; Subject = const(_) ), !,
    strip_const(Subject, StrippedSubj),
    findall(Loc,
        ( repository:fact(pred(vi_tri, [StrippedSubj, Loc]))
        ; repository:fact(pred(song_tai, [StrippedSubj, Loc]))
        ; repository:fact(pred(nam_ngu, [StrippedSubj, Place])),
          repository:fact(pred(vi_tri, [Place, Loc]))
        ; repository:fact(pred(chua, [Container, StrippedSubj])),
          repository:fact(pred(vi_tri, [Container, Loc]))
        ),
        RawValues),
    sort(RawValues, Values).

% --- WHERE với pred ---
find_values(wh_question(where, pred(vi_tri, [Subject, _])), Values) :- !,
    find_values(wh_question(where, Subject), Values).

% --- WHAT RELATION: "NP là gì của NP" → find predicate name ---
% Example: "Linh là gì của Nhân?" → wh_question(what_relation, pred(Rel, [linh, nhan]))
% Filter: Only accept valid kinship/social relationships, exclude situational acts like song_cung
find_values(wh_question(what_relation, pred(_RelationVar, [Subj, Obj])), Values) :- 
    nonvar(Subj), nonvar(Obj), !,
    strip_const(Subj, SSubj),
    strip_const(Obj, SObj),
    findall(Relation,
        ( repository:fact(pred(Relation, [SSubj, SObj])),
          Relation \= unknown,
          is_valid_relationship(Relation)
        ),
        RawValues),
    sort(RawValues, Values).

% Helper: Check valid relationships
is_valid_relationship(em_gai).
is_valid_relationship(anh_trai).
is_valid_relationship(chi_gai).
is_valid_relationship(bo).
is_valid_relationship(me).
is_valid_relationship(ban).
is_valid_relationship(vo).
is_valid_relationship(chong).
% Add other relationships here as needed

% --- Fallback ---
find_values(wh_question(_, _), []).

% ========================================
% PROVE WITH TRACE - DEBUG
% ========================================

prove_with_trace(Goal) :-
    format('~n=== PROVING: ~w ===~n', [Goal]),
    prove_with_depth(Goal, 0).

prove_with_depth(Goal, Depth) :-
    indent(Depth),
    format('→ ~w~n', [Goal]),
    ( Goal = and(A, B) ->
        NewDepth is Depth + 1,
        prove_with_depth(A, NewDepth),
        prove_with_depth(B, NewDepth)
    ; Goal = or(A, B) ->
        NewDepth is Depth + 1,
        ( prove_with_depth(A, NewDepth) 
        ; prove_with_depth(B, NewDepth)
        )
    ; Goal = neg(A) ->
        NewDepth is Depth + 1,
        ( prove_with_depth(A, NewDepth) ->
            indent(Depth),
            writeln('  ✗ Negation failed'),
            fail
        ;
            indent(Depth),
            writeln('  ✓ Negation succeeded')
        )
    ; Goal = pred(P, Args) ->
        strip_const_args(Args, StrippedArgs),
        ( repository:fact(pred(P, StrippedArgs)) ->
            indent(Depth),
            format('  ✓ Found: ~w(~w)~n', [P, StrippedArgs])
        ;
            indent(Depth),
            format('  ✗ Not found: ~w(~w)~n', [P, StrippedArgs]),
            fail
        )
    ;
        prove(Goal)
    ).

indent(0) :- !.
indent(N) :-
    N > 0,
    write('  '),
    N1 is N - 1,
    indent(N1).
