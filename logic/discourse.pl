% ========================================
% LOGIC MODULE: DISCOURSE
% Discourse Representation Structures (DRS)
% Based on Kamp & Reyle (1993)
% ========================================

:- module(discourse, [
    build_drs/2,
    merge_drs/3,
    negate_drs/2,
    pretty_print/1,
    simplify_drs/2
]).

% ========================================
% DRS CONSTRUCTION FROM LAMBDA TERMS
% ========================================

% Yes/No question: pred(P, Args) → drs([], [pred(P, Args)])
build_drs(pred(P, Args), drs([], [pred(P, Args)])) :- !.

% Conjunction
build_drs(conj(A, B), DRS) :- !,
    build_drs(A, DRS1),
    build_drs(B, DRS2),
    merge_drs(DRS1, DRS2, DRS).

% Existential quantification
build_drs(exists(X, Body), drs([X|Refs], Conds)) :- !,
    build_drs(Body, drs(Refs, Conds)).

% Universal quantification → complex DRS
build_drs(forall(X, Body), drs([], [impl(drs([X], []), DRS)])) :- !,
    build_drs(Body, DRS).

% Negation
build_drs(neg(Body), drs([], [neg(DRS)])) :- !,
    build_drs(Body, DRS).

% Implication
build_drs(impl(A, B), drs([], [impl(DRS1, DRS2)])) :- !,
    build_drs(A, DRS1),
    build_drs(B, DRS2).

% WH-question: who - trả về câu hỏi để find_entities xử lý
build_drs(wh_question(who, pred(P, Args)), wh_question(who, pred(P, Args))) :- !.

% WH-question: what - trả về câu hỏi để find_values xử lý  
build_drs(wh_question(what, pred(P, Args)), wh_question(what, pred(P, Args))) :- !.

% WH-question: where - trả về câu hỏi để find_values xử lý
build_drs(wh_question(where, pred(P, Args)), wh_question(where, pred(P, Args))) :- !.

% Application (after beta reduction)
build_drs(app(F, A), DRS) :-
    build_drs(F, DRS1),
    build_drs(A, DRS2),
    merge_drs(DRS1, DRS2, DRS).

% Constant
build_drs(const(C), drs([], [const(C)])) :- !.

% Lambda (shouldn't occur after reduction)
build_drs(lambda(_, _), _) :-
    throw(error(unreduced_lambda, 'Lambda not fully reduced')).

% Default
build_drs(Term, drs([], [Term])).

% ========================================
% WH-QUESTION CONDITIONS EXTRACTION
% ========================================

extract_wh_conditions(pred(Rel, [_, Obj]), X, Type, [type(X, Type), pred(Rel, [X, Obj])]) :- !.

extract_wh_conditions(pred(Rel, [Subj, _]), X, Type, [type(X, Type), pred(Rel, [Subj, X])]) :- !.

extract_wh_conditions(Body, X, Type, [type(X, Type), Body]).

% ========================================
% DRS OPERATIONS
% ========================================

% Merge two DRS
merge_drs(drs(Refs1, Conds1), drs(Refs2, Conds2), drs(Refs, Conds)) :-
    union(Refs1, Refs2, Refs),
    append(Conds1, Conds2, Conds).

% Negate DRS
negate_drs(drs([], Conds), drs([], [neg(drs([], Conds))])) :- !.

negate_drs(drs(Refs, Conds), drs([], [neg(drs(Refs, Conds))])).

% Simplify DRS
simplify_drs(drs(Refs, Conds), drs(SimpRefs, SimpConds)) :-
    sort(Refs, SimpRefs),
    simplify_conditions(Conds, SimpConds).

simplify_conditions([], []).
simplify_conditions([Cond|Rest], [SimpCond|SimpRest]) :-
    simplify_condition(Cond, SimpCond),
    simplify_conditions(Rest, SimpRest).

simplify_condition(neg(drs(R, C)), neg(drs(R, SC))) :- !,
    simplify_conditions(C, SC).

simplify_condition(impl(DRS1, DRS2), impl(SDRS1, SDRS2)) :- !,
    simplify_drs(DRS1, SDRS1),
    simplify_drs(DRS2, SDRS2).

simplify_condition(Cond, Cond).

% ========================================
% DRS PRETTY PRINTING
% ========================================

pretty_print(drs([], [])) :- !,
    writeln('⟨ ⟩').

pretty_print(drs(Refs, Conds)) :-
    writeln('┌───────────────────────┐'),
    write('│ '),
    print_refs(Refs),
    writeln(' │'),
    writeln('├───────────────────────┤'),
    print_conditions(Conds, '│ '),
    writeln('└───────────────────────┘').

print_refs([]) :- !.
print_refs([R]) :- !, write(R).
print_refs([R|Rs]) :-
    write(R), write(', '),
    print_refs(Rs).

print_conditions([], _).
print_conditions([Cond|Rest], Prefix) :-
    write(Prefix),
    print_condition(Cond),
    writeln(' │'),
    print_conditions(Rest, Prefix).

print_condition(pred(P, Args)) :-
    write(P), write('('),
    print_args(Args),
    write(')').

print_condition(type(X, T)) :-
    write(X), write(':'), write(T).

print_condition(neg(DRS)) :-
    write('¬ '),
    pretty_print(DRS).

print_condition(impl(DRS1, DRS2)) :-
    pretty_print(DRS1),
    write(' ⇒ '),
    pretty_print(DRS2).

print_condition(Cond) :-
    write(Cond).

print_args([]).
print_args([A]) :- !, write(A).
print_args([A|As]) :-
    write(A), write(', '),
    print_args(As).

% ========================================
% DRS UTILITIES
% ========================================

% Get all referents from DRS
drs_referents(drs(Refs, _), Refs).

% Get all conditions from DRS
drs_conditions(drs(_, Conds), Conds).

% Check if DRS is empty
is_empty_drs(drs([], [])).

% Add referent to DRS
add_referent(Ref, drs(Refs, Conds), drs([Ref|Refs], Conds)).

% Add condition to DRS
add_condition(Cond, drs(Refs, Conds), drs(Refs, [Cond|Conds])).

% Find free referents (referents used but not introduced)
free_referents(drs(Refs, Conds), Free) :-
    findall(Ref, 
        (member(Cond, Conds), 
         referent_in_condition(Ref, Cond),
         \+ member(Ref, Refs)),
        FreeList),
    sort(FreeList, Free).

referent_in_condition(Ref, pred(_, Args)) :-
    member(Ref, Args).

referent_in_condition(Ref, type(Ref, _)).

referent_in_condition(Ref, neg(DRS)) :-
    free_referents(DRS, Free),
    member(Ref, Free).

referent_in_condition(Ref, impl(DRS1, DRS2)) :-
    ( free_referents(DRS1, Free1), member(Ref, Free1)
    ; free_referents(DRS2, Free2), member(Ref, Free2)
    ).
