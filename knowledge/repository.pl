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
% ENTITIES
% Format: entity(Name, Type)
% ========================================

% People - Người
entity(nhan, person).
entity(linh, person).
entity(bo_nhan, person).

% Animals - Động vật
entity(miu, animal).

% Objects - Đồ vật
entity(xe_dap, object).
entity(ghe_go, object).

% Places - Địa điểm
entity(nha, place).
entity(phong_khach, place).
entity(vuon, place).
entity(truong, place).

% Plants - Thực vật
entity(hoa, plant).

% ========================================
% FACTS - Các sự thật
% Format: fact(pred(Predicate, [Arguments]))
% ========================================

% --- Quan hệ gia đình ---
% em_gai(Person1, Person2): Person2 là em gái của Person1
fact(pred(em_gai, [nhan, linh])).

% --- Nơi ở ---
% song_tai(Person, Place): Person sống tại Place
fact(pred(song_tai, [nhan, nha])).
fact(pred(song_tai, [linh, nha])).

% song_cung(Person1, Person2): Person1 sống cùng Person2
fact(pred(song_cung, [nhan, linh])).

% --- Thuộc tính địa điểm ---
% thuoc_tinh(Place, Property)
fact(pred(nho, [nha])).
fact(pred(ngoai_o, [nha])).

% --- Sở hữu ---
% so_huu(Owner, Object): Owner sở hữu Object
fact(pred(so_huu, [nhan, xe_dap])).
fact(pred(so_huu, [linh, miu])).

% --- Thuộc tính đồ vật ---
% mau_sac(Object, Color)
fact(pred(mau_sac, [xe_dap, xanh])).

% --- Quan hệ tặng ---
% tang(Giver, Receiver, Gift): Giver tặng Gift cho Receiver
fact(pred(tang, [bo_nhan, nhan, xe_dap])).

% qua_tang(Object): Object là món quà
fact(pred(qua_tang, [xe_dap])).

% --- Hành động ---
% cho(Person1, Person2, Destination): Person1 chở Person2 đến Destination
fact(pred(cho, [nhan, linh, truong])).

% dung_de(Object, Purpose): Object được dùng để Purpose
fact(pred(dung_de, [xe_dap, cho_linh_den_truong])).

% --- Cảm xúc/Sở thích ---
% thich(Person, Object/Thing)
fact(pred(thich, [linh, xe_dap])).
fact(pred(thich, [linh, hoa])).

% ly_do_thich(Person, Object, Reason)
fact(pred(ly_do_thich, [linh, xe_dap, cho_em])).

% --- Động vật và hành vi ---
% ten(Animal, Name)
fact(pred(ten, [miu, miu])).
fact(pred(la_meo, [miu])).

% nam_ngu(Animal, Location)
fact(pred(nam_ngu, [miu, ghe_go])).

% vi_tri(Object, Location)
fact(pred(vi_tri, [ghe_go, phong_khach])).

% --- Chăm sóc ---
% cho_an(Person, Animal)
fact(pred(cho_an, [nhan, miu])).

% choi_voi(Person, Animal)
fact(pred(choi_voi, [nhan, miu])).

% --- Vườn ---
% vi_tri_tuong_doi(Place1, Relation, Place2)
fact(pred(phia_sau, [vuon, nha])).

% chua(Place, Thing)
fact(pred(chua, [vuon, hoa])).

% ngam(Person, Object)
fact(pred(ngam, [linh, hoa])).
fact(pred(noi_ngam, [linh, hoa, vuon])).

% ========================================
% HƯỚNG DẪN THAY ĐỔI DATA
% ========================================

/*
HƯỚNG DẪN THÊM DATA MỚI:

1. THÊM ENTITY:
   entity(ten_entity, loai).
   
   Loại entity: person, animal, object, place, plant
   
   Ví dụ:
   entity(an, person).
   entity(cho, animal).
   entity(ban, object).

2. THÊM FACT:
   fact(pred(predicate, [arg1, arg2, ...])).

   Ví dụ:
   fact(pred(so_huu, [an, cho])).
   fact(pred(thich, [an, cho])).

3. RESTART SYSTEM:
   Khởi động lại Prolog hoặc reload:
   ?- make.

DANH SÁCH PREDICATE HIỆN CÓ:

- Quan hệ gia đình (2 args): em_gai(Person1, Person2)
- Nơi ở (2 args): song_tai(Person, Place), song_cung(Person1, Person2)
- Thuộc tính (1 arg): nho, ngoai_o, la_meo, qua_tang
- Sở hữu (2 args): so_huu(Owner, Object)
- Màu sắc (2 args): mau_sac(Object, Color)
- Tặng (3 args): tang(Giver, Receiver, Gift)
- Hành động (3 args): cho(Person1, Person2, Destination)
- Mục đích (2 args): dung_de(Object, Purpose)
- Cảm xúc (2 args): thich(Person, Thing)
- Lý do (3 args): ly_do_thich(Person, Object, Reason)
- Động vật (2 args): ten(Animal, Name), nam_ngu(Animal, Location)
- Vị trí (2 args): vi_tri(Object, Location), phia_sau(Place1, Place2)
- Chăm sóc (2 args): cho_an(Person, Animal), choi_voi(Person, Animal)
- Chứa (2 args): chua(Place, Thing), ngam(Person, Object)
- Phức tạp (3 args): noi_ngam(Person, Object, Place)

VÍ DỤ MỞ RỘNG:
*/

% Ví dụ 1: Thêm bạn của Nhân
% entity(an, person).
% fact(pred(ban_cua, [nhan, an])).
% fact(pred(song_tai, [an, nha_an])).

% Ví dụ 2: Thêm một con vật mới
% entity(cho, animal).
% fact(pred(so_huu, [an, cho])).
% fact(pred(cho_an, [an, cho])).

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
