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
rule(pred(so_huu, [Y, Z]), [pred(tang, [X, Y, Z])]).

% --- Rules tổng quát ---

% Nếu X cho_an Y thì X yêu Y
rule(pred(yeu, [X, Y]), [pred(cho_an, [X, Y])]).

% Nếu X thich Y thì X quan tâm Y
rule(pred(quan_tam, [X, Y]), [pred(thich, [X, Y])]).

% ========================================
% FORWARD CHAINING
% Infer new facts from existing facts and rules
% ========================================

infer :-
    rule(Conclusion, Premises),
    \+ repository:fact(Conclusion),
    forall(member(Premise, Premises), 
           repository:fact(Premise)),
    assertz(repository:fact(Conclusion)),
    fail.
infer.

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
