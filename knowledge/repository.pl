% ========================================
% KNOWLEDGE MODULE: REPOSITORY
% Knowledge Base - CHANGE DATA HERE!
% ========================================

:- module(repository, [
    fact/1,
    entity/2,
    initialize_kb/0,
    add_fact/1,
    remove_fact/1
]).

:- dynamic fact/1.
:- dynamic entity/2.

% ========================================
% ENTITIES - C�c thực thể
% Format: entity(Name, Type)
% ========================================

% Animals
entity(gau, animal).
entity(meo, animal).
entity(cho, animal).
entity(vit, animal).

% People
entity(huy, person).
entity(linh, person).
entity(minh, person).
entity(an, person).

% ========================================
% FACTS - Các sự thật
% Format: fact(pred(Predicate, [Arguments]))
% ========================================

% Properties
fact(pred(gentle, [gau])).
fact(pred(cute, [gau])).
fact(pred(cute, [meo])).
fact(pred(small, [meo])).
fact(pred(big, [cho])).

% Relations: walk(Person, Animal)
fact(pred(walk, [huy, gau])).
fact(pred(walk, [linh, cho])).
fact(pred(walk, [minh, meo])).

% Relations: love(Person, Animal)
fact(pred(love, [huy, gau])).
fact(pred(love, [linh, meo])).
fact(pred(love, [minh, cho])).
fact(pred(love, [an, meo])).

% Relations: eat(Animal, Food)
fact(pred(eat, [meo, ca])).
fact(pred(eat, [cho, thit])).
fact(pred(eat, [gau, mat_ong])).
fact(pred(eat, [vit, com])).

% Relations: belong(Animal, Person)
fact(pred(belong, [gau, huy])).
fact(pred(belong, [meo, linh])).
fact(pred(belong, [cho, minh])).

% Properties: color(Animal, Part, Color)
fact(pred(color, [gau, fur, brown])).
fact(pred(color, [meo, fur, white])).
fact(pred(color, [cho, fur, black])).

% ========================================
% HƯỚNG DẪN THAY ĐỔI DATA
% ========================================

/*
TO ADD NEW DATA:

1. ADD ENTITY:
   entity(ten_entity, loai).

   Example:
   entity(rua, animal).
   entity(nam, person).

2. ADD FACT:
   fact(pred(predicate, [arg1, arg2, ...])).

   Example:
   fact(pred(gentle, [rua])).
   fact(pred(walk, [nam, rua])).
   fact(pred(eat, [rua, rau])).

3. RESTART SYSTEM:
   Just restart Prolog or reload:
   ?- make.

PREDICATE TYPES:
- Properties (1 arg): gentle, cute, small, big
- Relations (2 args): walk, love, eat, belong
- Complex (3 args): color(Animal, Part, Color)

EXAMPLES:
*/

% Example 1: Add a turtle
% entity(rua, animal).
% fact(pred(slow, [rua])).
% fact(pred(walk, [nam, rua])).

% Example 2: Add feeding relation
% fact(pred(feed, [huy, gau])).

% ========================================
% DYNAMIC OPERATIONS
% ========================================

initialize_kb :-
    % KB is already loaded via facts above
    true.

add_fact(Fact) :-
    \+ fact(Fact),
    assertz(fact(Fact)).

remove_fact(Fact) :-
    retract(fact(Fact)).
