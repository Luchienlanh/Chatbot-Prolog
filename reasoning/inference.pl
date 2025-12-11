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

% If X walks Y, then X loves Y
rule(pred(love, [X, Y]), [pred(walk, [X, Y])]).

% If X feeds Y, then X loves Y  
rule(pred(love, [X, Y]), [pred(feed, [X, Y])]).

% If X loves Y and Y is animal, then X is animal lover
rule(pred(animal_lover, [X]), 
     [pred(love, [X, Y]), pred(animal, [Y])]).

% If X is gentle and cute, then X is lovable
rule(pred(lovable, [X]),
     [pred(gentle, [X]), pred(cute, [X])]).

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
