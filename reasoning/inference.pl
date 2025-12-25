% ========================================
% REASONING MODULE: INFERENCE
% Inference Rules & Forward Chaining
% ========================================

:- module(inference, [
    infer/0,
    add_rule/2,
    apply_rule/2
]).

:- use_module('../knowledge/repository').

% ========================================
% INFERENCE RULES
% Format: rule(Conclusion, [Premise1, Premise2, ...])
% ========================================

% --- Rules cho data mới (Nhân, Linh, Miu) ---

% Nếu X cho_an Y thì X chăm sóc Y
rule(pred(cham_soc, [X, Y]), [pred(cho_an, [X, Y])]).

% Nếu X choi_voi Y thì X thích Y
rule(pred(thich, [X, Y]), [pred(choi_voi, [X, Y])]).

% Nếu X so_huu Y thì Y thuộc về X
rule(pred(thuoc_ve, [Y, X]), [pred(so_huu, [X, Y])]).

% Nếu X song_cung Y thì X là gia đình của Y
rule(pred(gia_dinh, [X, Y]), [pred(song_cung, [X, Y])]).

% Nếu X ngam Y và Y ở vườn thì X thích vườn
rule(pred(thich, [X, vuon]), [pred(ngam, [X, Y]), pred(chua, [vuon, Y])]).

% Nếu X tang Y Z thì Y so_huu Z
rule(pred(so_huu, [Y, Z]), [pred(tang, [_X, Y, Z])]).

% --- Rules tổng quát ---

% Nếu X cho_an Y thì X yêu Y
rule(pred(yeu, [X, Y]), [pred(cho_an, [X, Y])]).

% Nếu X thich Y thì X quan tâm Y
rule(pred(quan_tam, [X, Y]), [pred(thich, [X, Y])]).

% --- Rules mapping Entity Types to Predicates ---
% These are essential for generic queries like "Một món quà..."

% Nếu X là type T thì pred(T, [X]) là true
% Note: Prolog variable matching requires specific rules per type or a generic mechanism
rule(pred(gift, [X]), [entity(X, object), pred(qua_tang, [X])]).
rule(pred(vehicle, [X]), [entity(X, object), pred(dung_de, [X, _])]).
rule(pred(animal, [X]), [entity(X, animal)]).
rule(pred(person, [X]), [entity(X, person)]).
rule(pred(object, [X]), [entity(X, object)]).
rule(pred(place, [X]), [entity(X, place)]).
rule(pred(plant, [X]), [entity(X, plant)]).
rule(pred(flower, [X]), [entity(X, plant)]).

% --- Rules for% Room inference based on contents
rule(pred(phong_khach, [X]), [entity(X, place), pred(so_huu, [X, ghe_go])]).
rule(pred(phong_khach, [X]), [entity(X, place), pred(chua, [X, ghe_go])]).
rule(pred(khu_vuon, [X]), [entity(X, place), pred(chua, [X, hoa])]).

% Location Transitivity
% If X lives at Place, and Place is at Loc, then X is at Loc
rule(pred(vi_tri, [X, Loc]), [pred(song_tai, [X, Place]), pred(vi_tri, [Place, Loc])]).

% If X sleeps at Place, and Place is at Loc, then X is at Loc
rule(pred(vi_tri, [X, Loc]), [pred(nam_ngu, [X, Place]), pred(vi_tri, [Place, Loc])]).

% Inferred song_tai location: if X lives at Place and Place is at Loc, X song_tai Loc
rule(pred(song_tai, [X, Loc]), [pred(song_tai, [X, Place]), pred(vi_tri, [Place, Loc])]).

% If X is in Container, and Container is at Loc, then X is at Loc
rule(pred(vi_tri, [X, Loc]), [pred(chua, [Container, X]), pred(vi_tri, [Container, Loc])]).
rule(pred(khu_vuon, [X]), [entity(X, place), pred(so_huu, [X, hoa])]).
rule(pred(khu_vuon, [X]), [entity(X, place), pred(co, [X, hoa])]).


% ========================================
% FORWARD CHAINING
% Infer new facts from existing facts and rules
% ========================================

% ========================================
% FORWARD CHAINING
% Infer new facts from existing facts and rules
% ========================================

infer :-
    rule(Conclusion, Premises),
    \+ repository:fact(Conclusion),
    check_premises(Premises),
    assertz(repository:fact(Conclusion)),
    fail.
infer.

check_premises([]).
check_premises([Premise|Rest]) :-
    check_premise(Premise),
    check_premises(Rest).

check_premise(entity(X, T)) :-
    repository:entity(X, T).
check_premise(pred(P, Args)) :-
    repository:fact(pred(P, Args)).


% ========================================
% RULE MANAGEMENT
% ========================================

add_rule(Conclusion, Premises) :-
    assertz(rule(Conclusion, Premises)).

apply_rule(Rule, NewFacts) :-
    rule(Conclusion, Premises),
    Rule = rule(Conclusion, Premises),
    findall(Conclusion,
        ( forall(member(P, Premises), repository:fact(P)),
          \+ repository:fact(Conclusion)
        ),
        NewFacts).
