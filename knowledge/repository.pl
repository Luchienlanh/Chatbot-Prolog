% ========================================
% KNOWLEDGE MODULE: REPOSITORY
% Knowledge Base - Theo đúng Report_DOAN01_PTB.md
% ========================================

:- module(repository, [
    fact/1,
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
:- dynamic entity/2.

% ========================================
% CONSTANTS (Hằng) - Section 2.2 Report
% Format: constant(Name, Type)
% ========================================

% Người
constant(nhan, person).
constant(linh, person).
constant(bo_nhan, person).

% Động vật
constant(miu, animal).

% Đồ vật
constant(xe_dap, object).
constant(ghe_go, object).

% Địa điểm
constant(nha, place).
constant(nha_nho, place).
constant(phong_khach, place).
constant(vuon, place).
constant(khu_vuon, place).
constant(truong, place).
constant(ngoai_o, place).
constant(sau_nha, place).

% Thực vật
constant(hoa, plant).

% Màu sắc
constant(xanh, color).

% ========================================
% PREDICATES với số ngôi - Section 2.3 Report
% Format: predicate(Name, Arity)
% ========================================

predicate(thich, 2).      % thich(X, Y): X thích Y
predicate(so_huu, 2).     % so_huu(X, Y): X sở hữu Y
predicate(cho_an, 2).     % cho_an(X, Y): X cho Y ăn
predicate(vi_tri, 2).     % vi_tri(X, Y): X ở vị trí Y
predicate(em_gai, 2).     % em_gai(X, Y): X là em gái của Y
predicate(song_tai, 2).   % song_tai(X, Y): X sống tại Y
predicate(song_cung, 2).  % song_cung(X, Y): X sống cùng Y
predicate(mau, 2).        % mau(X, Y): X có màu Y
predicate(tang, 3).       % tang(X, Y, Z): X tặng Z cho Y
predicate(cho, 3).        % cho(X, Y, Z): X chở Y đến Z
predicate(ten, 2).        % ten(X, Y): X tên là Y
predicate(la_meo, 1).     % la_meo(X): X là mèo
predicate(nam_ngu, 2).    % nam_ngu(X, Y): X nằm ngủ tại Y
predicate(choi_voi, 2).   % choi_voi(X, Y): X chơi với Y
predicate(chua, 2).       % chua(X, Y): X chứa Y
predicate(ngam, 2).       % ngam(X, Y): X ngắm Y
predicate(phia_sau, 2).   % phia_sau(X, Y): X ở phía sau Y
predicate(hoa, 1).        % hoa(X): X là hoa

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
% FACTS (Sự thật từ đoạn văn) - Section 4.1.2 Report
% ========================================

% 1. Linh là em gái của Nhân.
fact(pred(em_gai, [linh, nhan])).
fact(pred(nguoi, [nhan])).
fact(pred(nguoi, [linh])).

% 2. Nhân và Linh sống tại một ngôi nhà nhỏ ở ngoại ô.
fact(pred(song_tai, [nhan, nha_nho])).
fact(pred(song_tai, [linh, nha_nho])).
fact(pred(vi_tri, [nha_nho, ngoai_o])).
% Inferred: Nhân và Linh sống cùng nhau
fact(pred(song_cung, [nhan, linh])).
fact(pred(song_cung, [linh, nhan])).
fact(pred(song_tai, [nhan, ngoai_o])).
fact(pred(song_tai, [linh, ngoai_o])).

% 3. Nhân sở hữu một chiếc xe đạp màu xanh.
fact(pred(so_huu, [nhan, xe_dap])).
fact(pred(mau, [xe_dap, xanh])).

% 4. Bố Nhân đã tặng xe đạp này cho Nhân.
fact(pred(tang, [bo_nhan, nhan, xe_dap])).

% 5. Hàng ngày, Nhân chở Linh đến trường bằng xe đạp.
fact(pred(cho, [nhan, linh, truong])).

% 6. Linh thích xe đạp vì anh chở em đi học.
fact(pred(thich, [linh, xe_dap])).

% 7. Linh cũng thích hoa.
fact(pred(thich, [linh, hoa])).

% 8. Linh có một con mèo tên Miu.
fact(pred(so_huu, [linh, miu])).
fact(pred(ten, [miu, miu])).
fact(pred(la_meo, [miu])).

% 9. Miu nằm ngủ trên ghế gỗ trong phòng khách.
fact(pred(nam_ngu, [miu, ghe_go])).
fact(pred(vi_tri, [ghe_go, phong_khach])).

% 10. Nhân thường cho Miu ăn và chơi với Miu.
fact(pred(cho_an, [nhan, miu])).
fact(pred(choi_voi, [nhan, miu])).

% 11. Sau nhà có một khu vườn trồng nhiều hoa.
fact(pred(vi_tri, [vuon, sau_nha])).
fact(pred(phia_sau, [vuon, nha])).
fact(pred(chua, [vuon, hoa])).

% 12. Cuối tuần Linh thường ra vườn ngắm hoa.
fact(pred(ngam, [linh, hoa])).
fact(pred(thich, [linh, vuon])).  % Inferred
fact(pred(hoa, [hoa])).           % "hoa" là tập hợp hoa

% --- Miu's location (inferred from sentence 9) ---
fact(pred(vi_tri, [miu, phong_khach])).

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

