% ========================================
% LINGUISTIC MODULE: COMPOSITION
% Lambda Calculus Operations & Beta Reduction
% ========================================

:- module(composition, [
    compose/2,
    beta_reduce/2,
    beta_reduce_full/2,
    alpha_convert/3,
    substitute/4,
    free_variables/2
]).

% ========================================
% SEMANTIC COMPOSITION
% Combine semantics according to syntactic structure
% ========================================

compose(tree(s, [NP, VP]), Semantics) :- !,
    compose(NP, NPSem),
    compose(VP, VPSem),
    Semantics = app(NPSem, VPSem).

compose(tree(np, [Det, N]), Semantics) :- !,
    compose(Det, DetSem),
    compose(N, NSem),
    Semantics = app(DetSem, NSem).

compose(tree(vp, [V, NP]), Semantics) :- !,
    compose(V, VSem),
    compose(NP, NPSem),
    Semantics = app(VSem, NPSem).

compose(tree(vp, [V]), Semantics) :- !,
    compose(V, Semantics).

compose(word(Word, Category), Semantics) :-
    vocabulary:word_semantics(Word, Category, Semantics).

compose(Terminal, Terminal).

% ========================================
% BETA REDUCTION
% (λx.M) N → M[x := N]
% ========================================

% One-step beta reduction
beta_reduce(app(lambda(Var, Body), Arg), Result) :- !,
    substitute(Var, Arg, Body, Result).

beta_reduce(app(Fun, Arg), app(Fun2, Arg2)) :- !,
    beta_reduce(Fun, Fun2),
    beta_reduce(Arg, Arg2).

beta_reduce(lambda(Var, Body), lambda(Var, Body2)) :- !,
    beta_reduce(Body, Body2).

beta_reduce(conj(A, B), conj(A2, B2)) :- !,
    beta_reduce(A, A2),
    beta_reduce(B, B2).

beta_reduce(disj(A, B), disj(A2, B2)) :- !,
    beta_reduce(A, A2),
    beta_reduce(B, B2).

beta_reduce(neg(A), neg(A2)) :- !,
    beta_reduce(A, A2).

beta_reduce(exists(V, B), exists(V, B2)) :- !,
    beta_reduce(B, B2).

beta_reduce(forall(V, B), forall(V, B2)) :- !,
    beta_reduce(B, B2).

beta_reduce(pred(P, Args), pred(P, Args2)) :- !,
    maplist(beta_reduce, Args, Args2).

beta_reduce(Term, Term).

% ========================================
% FULL BETA REDUCTION
% Reduce until normal form
% ========================================

beta_reduce_full(Term, Result) :-
    beta_reduce(Term, Reduced),
    ( Term = Reduced ->
        Result = Term
    ;
        beta_reduce_full(Reduced, Result)
    ).

% ========================================
% SUBSTITUTION
% substitute(Var, Value, Term, Result)
% Term[Var := Value] = Result
% ========================================

% Variable matches - substitute
substitute(Var, Value, Var, Value) :- !.

% Variable doesn't match - keep
substitute(Var, _, OtherVar, OtherVar) :-
    atom(OtherVar),
    Var \= OtherVar, !.

% Lambda with same variable - stop (variable shadowing)
substitute(Var, _, lambda(Var, Body), lambda(Var, Body)) :- !.

% Lambda with different variable - continue
substitute(Var, Value, lambda(V, Body), lambda(V, NewBody)) :-
    Var \= V, !,
    ( free_variables(Value, FreeVars),
      member(V, FreeVars) ->
        % Alpha conversion needed to avoid capture
        gensym(V, FreshVar),
        substitute(V, FreshVar, Body, Body1),
        substitute(Var, Value, Body1, NewBody)
    ;
        substitute(Var, Value, Body, NewBody)
    ).

% Application
substitute(Var, Value, app(Fun, Arg), app(NewFun, NewArg)) :- !,
    substitute(Var, Value, Fun, NewFun),
    substitute(Var, Value, Arg, NewArg).

% Conjunction
substitute(Var, Value, conj(A, B), conj(NewA, NewB)) :- !,
    substitute(Var, Value, A, NewA),
    substitute(Var, Value, B, NewB).

% Disjunction
substitute(Var, Value, disj(A, B), disj(NewA, NewB)) :- !,
    substitute(Var, Value, A, NewA),
    substitute(Var, Value, B, NewB).

% Negation
substitute(Var, Value, neg(A), neg(NewA)) :- !,
    substitute(Var, Value, A, NewA).

% Existential
substitute(Var, _, exists(Var, Body), exists(Var, Body)) :- !.

substitute(Var, Value, exists(V, Body), exists(V, NewBody)) :-
    Var \= V, !,
    ( free_variables(Value, FreeVars),
      member(V, FreeVars) ->
        gensym(V, FreshVar),
        substitute(V, FreshVar, Body, Body1),
        substitute(Var, Value, Body1, NewBody)
    ;
        substitute(Var, Value, Body, NewBody)
    ).

% Universal
substitute(Var, _, forall(Var, Body), forall(Var, Body)) :- !.

substitute(Var, Value, forall(V, Body), forall(V, NewBody)) :-
    Var \= V, !,
    substitute(Var, Value, Body, NewBody).

% Predicate
substitute(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(substitute(Var, Value), Args, NewArgs).

% Constants
substitute(_, _, const(C), const(C)) :- !.

% Default
substitute(_, _, Term, Term).

% ========================================
% ALPHA CONVERSION
% Rename bound variables to avoid capture
% ========================================

alpha_convert(lambda(Var, Body), NewVar, lambda(NewVar, NewBody)) :-
    substitute(Var, NewVar, Body, NewBody).

% ========================================
% FREE VARIABLES
% Find all free variables in a term
% ========================================

free_variables(Var, [Var]) :-
    atom(Var), !.

free_variables(const(_), []) :- !.

free_variables(lambda(Var, Body), FreeVars) :- !,
    free_variables(Body, BodyVars),
    delete(BodyVars, Var, FreeVars).

free_variables(app(Fun, Arg), FreeVars) :- !,
    free_variables(Fun, FunVars),
    free_variables(Arg, ArgVars),
    union(FunVars, ArgVars, FreeVars).

free_variables(conj(A, B), FreeVars) :- !,
    free_variables(A, AVars),
    free_variables(B, BVars),
    union(AVars, BVars, FreeVars).

free_variables(disj(A, B), FreeVars) :- !,
    free_variables(A, AVars),
    free_variables(B, BVars),
    union(AVars, BVars, FreeVars).

free_variables(neg(A), FreeVars) :- !,
    free_variables(A, FreeVars).

free_variables(exists(Var, Body), FreeVars) :- !,
    free_variables(Body, BodyVars),
    delete(BodyVars, Var, FreeVars).

free_variables(forall(Var, Body), FreeVars) :- !,
    free_variables(Body, BodyVars),
    delete(BodyVars, Var, FreeVars).

free_variables(pred(_, Args), FreeVars) :- !,
    findall(Vars, 
        (member(Arg, Args), free_variables(Arg, Vars)),
        VarsList),
    flatten(VarsList, AllVars),
    sort(AllVars, FreeVars).

free_variables(_, []).

% ========================================
% NORMALIZATION
% Reduce to normal form
% ========================================

normalize(Term, Normal) :-
    beta_reduce_full(Term, Reduced),
    eta_reduce(Reduced, Normal).

% Eta reduction: λx.(f x) → f (when x not free in f)
eta_reduce(lambda(X, app(F, X)), F) :-
    \+ free_variables(F, FVars),
    \+ member(X, FVars), !.

eta_reduce(lambda(X, Body), lambda(X, ReducedBody)) :- !,
    eta_reduce(Body, ReducedBody).

eta_reduce(app(F, A), app(RF, RA)) :- !,
    eta_reduce(F, RF),
    eta_reduce(A, RA).

eta_reduce(Term, Term).
