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
    find_values/2,
    initialize_prover/0
]).

:- discontiguous find_entities/2.
:- discontiguous find_values/2.

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

% pred(P, Args) - Kiểm tra trong knowledge base
% Strip const() wrapper trước khi tìm
prove(pred(P, Args)) :- 
    strip_const_args(Args, StrippedArgs),
    repository:fact(pred(P, StrippedArgs)).

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

substitute_in_body(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute_arg(Var, Value), Args, NewArgs).

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
% FIND ENTITIES - TÌM ENTITY CHO CÂU HỎI WHO
% ========================================

% --- WHO với lambda và app: lambda(x, app(VPSem, x)) ---
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

% --- WHAT: "NP V gì" → find object ---
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
find_values(wh_question(what_relation, pred(RelationVar, [Subj, Obj])), Values) :- 
    nonvar(Subj), nonvar(Obj), !,
    strip_const(Subj, SSubj),
    strip_const(Obj, SObj),
    findall(Relation,
        ( repository:fact(pred(Relation, [SSubj, SObj])),
          Relation \= unknown
        ),
        RawValues),
    sort(RawValues, Values).

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
