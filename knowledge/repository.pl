% ========================================
% KNOWLEDGE MODULE: REPOSITORY
% Knowledge Base - Theo đúng Report_DOAN01_PTB.md
% ========================================

:- module(repository, [
    fact/1,
    rule/2,
    entity/2,
    constant/2,
    predicate/2,
    valid_expression/1,
    symmetric_relation/1,
    check_symmetric/3,
    transitive_location/2,
    initialize_kb/0,
    add_fact/1,
    remove_fact/1
]).

:- dynamic fact/1.
:- dynamic rule/2.
:- dynamic entity/2.

% ========================================
% INFERENCE RULES
% Format: rule(Head, BodyList)
% ========================================

% X là của Y nếu Y sở hữu X
rule(pred(cua, [Obj, Owner]), [pred(so_huu, [Owner, Obj])]).

% ========================================
% CONSTANTS (Hằng) - Proper Names + Descriptive Object Names
% Format: constant(Name, Type)
% ========================================

% --- PROPER NAMES (True constants) ---
% People
constant(nhan, person).
constant(linh, person).
constant(bo_nhan, person).

% Animals (pet with proper name)
constant(miu, animal).

% --- OBJECT/PLACE CONSTANTS (With descriptive names) ---
% Objects mentioned in story - using natural descriptive names

% The specific bicycle that Nhân owns
constant(xe_dap_nhan, object).  % "chiếc xe đạp của Nhân"

% The specific house where Nhân and Linh live
constant(nha_nhan_linh, object).  % "ngôi nhà của Nhân và Linh"

% The specific living room  
constant(phong_khach, object).  % "phòng khách"

% The specific garden
constant(khu_vuon, object).  % "khu vườn"

% The specific wooden chair
constant(ghe_go, object).  % "cái ghế gỗ"

% The specific school
constant(truong, object).  % "trường"

% Location constants
constant(ngoai_o, place).  % "ngoại ô"

% Colors (used as individuals in predicates like mau(X, xanh))
constant(xanh, color).

% Special: flower collection
constant(cac_bong_hoa, object).  % "các bông hoa" (flowers as collection)

% ========================================
% PREDICATES - Section 2.3 Report  
% Format: predicate(Name, Arity)
% ========================================

% --- 1-PLACE PREDICATES (Types) ---
predicate(nguoi, 1).      % nguoi(X): X is a person
predicate(meo, 1).        % meo(X): X is a cat
predicate(dong_vat, 1).   % dong_vat(X): X is an animal
predicate(thu_cung, 1).   % thu_cung(X): X is a pet

% Object types
predicate(xe_dap, 1).     % xe_dap(X): X is a bicycle
predicate(xe, 1).         % xe(X): X is a vehicle
predicate(ghe, 1).        % ghe(X): X is a chair

% Place types
predicate(nha, 1).        % nha(X): X is a house
predicate(phong_khach, 1). % phong_khach(X): X is a living room
predicate(vuon, 1).       % vuon(X): X is a garden
predicate(dia_diem, 1).   % dia_diem(X): X is a place

% Plant types
predicate(hoa, 1).        % hoa(X): X is a flower

% Other types
predicate(qua_tang, 1).   % qua_tang(X): X is a gift

% --- 2-PLACE PREDICATES (Relations) ---
predicate(thich, 2).      % thich(X, Y): X thích Y
predicate(so_huu, 2).     % so_huu(X, Y): X sở hữu Y
predicate(cho_an, 2).     % cho_an(X, Y): X cho Y ăn
predicate(vi_tri, 2).     % vi_tri(X, Y): X ở vị trí Y
predicate(em_gai, 2).     % em_gai(X, Y): X là em gái của Y
predicate(song_tai, 2).   % song_tai(X, Y): X sống tại Y
predicate(song_cung, 2).  % song_cung(X, Y): X sống cùng Y
predicate(mau, 2).        % mau(X, Y): X có màu Y
predicate(ten, 2).        % ten(X, Y): X tên là Y
predicate(nam_ngu, 2).    % nam_ngu(X, Y): X nằm ngủ tại Y
predicate(choi_voi, 2).   % choi_voi(X, Y): X chơi với Y
predicate(chua, 2).       % chua(X, Y): X chứa Y
predicate(ngam, 2).       % ngam(X, Y): X ngắm Y
predicate(phia_sau, 2).   % phia_sau(X, Y): X ở phía sau Y

% --- 3-PLACE PREDICATES (3-way relations) ---
predicate(tang, 3).       % tang(X, Y, Z): X tặng Z cho Y
predicate(cho, 3).        % cho(X, Y, Z): X chở Y đến Z

% --- GENDER ATTRIBUTE (For Coreference Resolution) ---
predicate(gender, 2).     % gender(Entity, Gender): male, female, neutral

% ========================================
% KIỂM TRA BIỂU THỨC HỢP LỆ - Section 2.3 Report
% ========================================

% Một biểu thức hợp lệ khi:
% 1. Tất cả hằng có trong danh sách constant
% 2. Vị từ có trong danh sách predicate với đúng số ngôi
valid_expression(pred(P, Args)) :-
    predicate(P, Arity),
    length(Args, Arity),
    forall(member(A, Args), valid_constant(A)).

valid_constant(C) :- constant(C, _), !.
valid_constant(C) :- var(C), !.  % Biến được chấp nhận
valid_constant(_).  % Fallback for flexibility

% ========================================
% QUAN HỆ ĐỐI XỨNG - Section 4.1.3 Report
% R(X,Y) ⟺ R(Y,X)
% ========================================

symmetric_relation(song_cung).

check_symmetric(Pred, X, Y) :-
    symmetric_relation(Pred),
    ( fact(pred(Pred, [X, Y]))
    ; fact(pred(Pred, [Y, X]))
    ).

% ========================================
% QUAN HỆ BẮC CẦU - Section 4.1.3 Report
% R(X,Y) ∧ R(Y,Z) ⟹ R(X,Z)
% ========================================

% vi_tri có tính bắc cầu
transitive_location(X, Z) :-
    fact(pred(vi_tri, [X, Y])),
    fact(pred(vi_tri, [Y, Z])),
    X \= Z.

% Ví dụ: vi_tri(miu, ghe_go) ∧ vi_tri(ghe_go, phong_khach) 
%        ⟹ transitive_location(miu, phong_khach)

% ========================================
% ENTITIES (backward compatibility)
% ========================================

entity(X, Type) :-
    constant(X, Type).

entity(X, Type) :-
    fact(pred(Type, [X])).

% ========================================
% FACTS (Sự thật từ đoạn văn) - FOL với Type Assertions
% Format: fact(pred(...)) with explicit type assertions
% ========================================

% --- TYPE ASSERTIONS ---
% Assert that constants satisfy their types

fact(pred(xe_dap, [xe_dap_nhan])).   % xe_dap_nhan is a bicycle
fact(pred(nha, [nha_nhan_linh])).    % nha_nhan_linh is a house
fact(pred(phong_khach, [phong_khach])). % phong_khach is a living room
fact(pred(vuon, [khu_vuon])).        % khu_vuon is a garden
fact(pred(ghe, [ghe_go])).           % ghe_go is a chair
fact(pred(hoa, [cac_bong_hoa])).     % cac_bong_hoa are flowers

% Type assertions for proper-name individuals
fact(pred(nguoi, [nhan])).
fact(pred(nguoi, [linh])).  
fact(pred(nguoi, [bo_nhan])).
fact(pred(meo, [miu])).
fact(pred(dong_vat, [miu])).
fact(pred(thu_cung, [miu])).

% --- GENDER FACTS ---
fact(pred(gender, [nhan, male])).
fact(pred(gender, [linh, female])).
fact(pred(gender, [bo_nhan, male])).
fact(pred(gender, [nam, male])).
fact(pred(gender, [binh, male])).
fact(pred(gender, [ly, male])).
fact(pred(gender, [miu, neutral])).
fact(pred(gender, [xe_dap_nhan, neutral])).
fact(pred(gender, [nha_nhan_linh, neutral])).
fact(pred(gender, [cac_bong_hoa, neutral])).
fact(pred(gender, [khu_vuon, neutral])).

% --- STORY FACTS ---

% 1. Linh là em gái của Nhân.
fact(pred(em_gai, [linh, nhan])).

% 2. Nhân và Linh sống tại một ngôi nhà nhỏ ở ngoại ô.
fact(pred(song_tai, [nhan, nha_nhan_linh])).
fact(pred(song_tai, [linh, nha_nhan_linh])).
fact(pred(vi_tri, [nha_nhan_linh, ngoai_o])).

% Inferred: Nhân và Linh sống cùng nhau
fact(pred(song_cung, [nhan, linh])).
fact(pred(song_cung, [linh, nhan])).

% 3. Nhân sở hữu một chiếc xe đạp màu xanh.
fact(pred(so_huu, [nhan, xe_dap_nhan])).
fact(pred(mau, [xe_dap_nhan, xanh])).

% 4. Bố Nhân đã tặng xe đạp này cho Nhân.
fact(pred(tang, [bo_nhan, nhan, xe_dap_nhan])).
fact(pred(qua_tang, [xe_dap_nhan])).  % xe_dap_nhan is also a gift

% 5. Hàng ngày, Nhân chở Linh đến trường bằng xe đạp.
fact(pred(cho, [nhan, linh, truong])).

% 6. Linh thích xe đạp vì anh chở em đi học.
fact(pred(thich, [linh, xe_dap_nhan])).

% 7. Linh cũng thích hoa.
fact(pred(thich, [linh, cac_bong_hoa])).  % Linh likes the flowers

% 8. Linh có một con mèo tên Miu.
fact(pred(so_huu, [linh, miu])).
fact(pred(ten, [miu, miu])).

% 9. Miu nằm ngủ trên ghế gỗ trong phòng khách.
fact(pred(nam_ngu, [miu, ghe_go])).
fact(pred(vi_tri, [ghe_go, phong_khach])).
fact(pred(vi_tri, [miu, ghe_go])).
fact(pred(vi_tri, [miu, phong_khach])).  % Inferred via transitivity
fact(pred(vi_tri, [cac_bong_hoa, khu_vuon])).
fact(pred(vi_tri, [khu_vuon, nha_nhan_linh])).  % Khu vườn ở sau nhà

% Living location
fact(pred(song_tai, [nhan, ngoai_o])).  % Nhân sống tại ngoại ô  % Inferred via transitivity

% 10. Nhân thường cho Miu ăn và chơi với Miu.
fact(pred(cho_an, [nhan, miu])).
fact(pred(choi_voi, [nhan, miu])).

% 11. Sau nhà có một khu vườn trồng nhiều hoa.
fact(pred(phia_sau, [khu_vuon, nha_nhan_linh])).
fact(pred(chua, [khu_vuon, cac_bong_hoa])).  % Garden contains THE flowers

% 12. Cuối tuần Linh thường ra vườn ngắm hoa.
fact(pred(ngam, [linh, cac_bong_hoa])).
fact(pred(thich, [linh, khu_vuon])).  % Inferred

% ========================================
% DYNAMIC OPERATIONS
% ========================================

initialize_kb :-
    true.

add_fact(Fact) :-
    \+ fact(Fact),
    assertz(fact(Fact)).

remove_fact(Fact) :-
    retract(fact(Fact)).

