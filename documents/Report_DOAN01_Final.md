# BÁO CÁO ĐỒ ÁN 01

## HỆ THỐNG HỎI ĐÁP TỰ ĐỘNG - NGỮ NGHĨA HỌC TÍNH TOÁN

**Môn học:** Ngữ Nghĩa Học Tính Toán (NNHTT)  
**Trường:** Đại học Công nghệ Thông tin - ĐHQG-HCM

---

# 1. GIỚI THIỆU ĐỒ ÁN

## 1.1 Mục tiêu

Xây dựng hệ thống hỏi đáp tự động:

- Nhận câu hỏi dạng xác nhận (Yes/No) hoặc hỏi về người/đồ vật
- Dựa vào tri thức sẵn có để tìm câu trả lời
- Đưa ra thông tin trả lời

## 1.2 Đoạn văn mô tả tri thức (10 câu)

> Nhân sống cùng em gái là Linh trong một nhà nhỏ ở ngoại ô. Nhân có một chiếc xe đạp màu xanh. Chiếc xe đạp đó là món quà mà bố Nhân tặng cho cậu. Nhân dùng chiếc xe để chở Linh đến trường. Linh rất thích chiếc xe của Nhân vì nó chở rất êm. Trong nhà, Linh có một con mèo tên là Miu. Miu thường nằm ngủ trên chiếc ghế gỗ ở phòng khách. Miu được Nhân cho ăn và chơi với nó. Ở sau nhà có một khu vườn nhỏ với những loài hoa. Linh thích ngắm hoa trong vườn.

---

# 2. THU THẬP CÂU HỎI

## 2.1 Số dạng câu hỏi: 4 dạng

| Dạng   | Cấu trúc                 | Mục đích                     | Ví dụ                                 |
| ------ | ------------------------ | ---------------------------- | ------------------------------------- |
| Yes/No | NP VP QM                 | Xác nhận mệnh đề đúng/sai    | Linh thích xe đạp phải không?         |
| Who    | WP VP / NP VB-cop PP(WP) | Tìm người/vật thỏa điều kiện | Ai sở hữu xe đạp? / Xe đạp là của ai? |
| What   | NP VP WP                 | Tìm đối tượng của hành động  | Linh thích gì?                        |
| Where  | NP VB WRB                | Tìm vị trí                   | Miu ở đâu?                            |

## 2.2 Danh sách 18 câu hỏi và trả lời (theo queries_cs229.pl)

### Câu hỏi Where (Location)

| ID  | Câu hỏi         | Dạng  | Trả lời       |
| --- | --------------- | ----- | ------------- |
| nq1 | Miu ở đâu?      | Where | [phong_khach] |
| nq2 | Khu vườn ở đâu? | Where | [sau_nha]     |

### Câu hỏi Who (Ownership & Relationship)

| ID   | Câu hỏi                | Dạng | Trả lời |
| ---- | ---------------------- | ---- | ------- |
| nq3  | Xe đạp là của ai?      | Who  | [nhan]  |
| nq4  | Ai sở hữu xe đạp?      | Who  | [nhan]  |
| nq6  | Ai là em gái của Nhân? | Who  | [linh]  |
| nq8  | Ai sống cùng Linh?     | Who  | [nhan]  |
| nq9  | Ai cho Miu ăn?         | Who  | [nhan]  |
| nq10 | Ai chơi với Miu?       | Who  | [nhan]  |
| nq13 | Ai thích ngắm hoa?     | Who  | [linh]  |

### Câu hỏi What

| ID   | Câu hỏi              | Dạng | Trả lời           |
| ---- | -------------------- | ---- | ----------------- |
| nq7  | Linh là gì của Nhân? | What | [em_gai]          |
| nq11 | Linh thích gì?       | What | [hoa,vuon,xe_dap] |
| nq12 | Linh ngắm gì?        | What | [hoa]             |

### Câu hỏi Yes/No

| ID   | Câu hỏi                               | Dạng   | Trả lời |
| ---- | ------------------------------------- | ------ | ------- |
| nq14 | Linh thích xe đạp phải không?         | Yes/No | YES     |
| nq15 | Nhân tặng Linh xe đạp phải không?     | Yes/No | NO      |
| nq16 | Miu ngủ trong phòng khách phải không? | Yes/No | YES     |
| nq17 | Khu vườn ở sau nhà phải không?        | Yes/No | YES     |
| nq18 | Nhân sống tại ngoại ô phải không?     | Yes/No | YES     |

### Tổng kết

| Dạng câu hỏi | Số câu |
| ------------ | ------ |
| Where        | 2      |
| Who          | 7      |
| What         | 3      |
| Yes/No       | 5      |
| Phức (Coref) | 4      |
| **TỔNG**     | **21** |

## 2.3 Các câu hỏi phức (Cross-Sentence Coreference) - MỞ RỘNG

Hệ thống được mở rộng để xử lý các câu hỏi liên quan đến đồng sở chỉ (anaphora), trong đó đại từ trong câu hỏi tham chiếu đến thực thể ở câu trước đó.

| ID   | Câu hỏi                                  | Dạng   | Trả lời | Giải thích                                            |
| ---- | ---------------------------------------- | ------ | ------- | ----------------------------------------------------- |
| nq19 | Một chiếc xe màu xanh. Nó là của Linh?   | Yes/No | NO      | "Nó" = xe đạp (của Nhân). Xe của Nhân != xe của Linh. |
| nq20 | Linh có một con mèo. Nó tên là Miu?      | Yes/No | YES     | "Nó" = con mèo. Mèo tên Miu.                          |
| nq21 | Người cho Miu ăn. Đó là Nhân?            | Yes/No | YES     | "Đó" = Người cho Miu ăn. Nhân cho Miu ăn.             |
| nq22 | Linh thích ngắm hoa. Chúng ở trong vườn? | Yes/No | YES     | "Chúng" = hoa. Hoa ở trong vườn.                      |

---

# 3. TỪ VỰNG BẬC MỘT

## 3.1 Lý thuyết: Logic vị từ bậc một

### 3.1.1 Các khái niệm cơ bản (Slide-BUOI-08)

**Ngữ nghĩa của câu:**

- Nghĩa của câu S, ký hiệu ‖S‖, là ngoại diên (extension) của S
- Ngoại diên của câu là giá trị chân lý (truth-value): True hoặc False

**Các kiểu ngữ nghĩa:**

- **e** (entity): Kiểu thực thể - Miền De
- **t** (truth-value): Kiểu chân trị - Miền Dt = {True, False}
- **⟨A,B⟩** (function): Kiểu hàm ánh xạ từ A sang B

### 3.1.2 Từ vựng bậc một

**Thành phần từ vựng bậc một:**

1. **Hằng (Constants):** Các thực thể cụ thể trong tri thức
2. **Quan hệ (Relations/Predicates):** Các vị từ với số ngôi xác định
3. **Hàm (Functions):** Các ánh xạ từ thực thể sang thực thể

**Nhiệm vụ:**

- Xác định các tên hằng, hàm và quan hệ với số ngôi tương ứng
- Kiểm tra tính hợp lệ của một biểu thức

---

## 3.2 Phương pháp xác định từ vựng bậc một

### 3.2.1 Xác định hằng

Rà soát đoạn văn, thu thập tất cả các thực thể được đề cập:

| Nhóm     | Hằng        | Kiểu   | Nguồn                      |
| -------- | ----------- | ------ | -------------------------- |
| Người    | nhan        | person | "Nhân sống cùng em gái..." |
| Người    | linh        | person | "...em gái là Linh"        |
| Người    | bo_nhan     | person | "bố Nhân tặng..."          |
| Động vật | miu         | animal | "con mèo tên là Miu"       |
| Đồ vật   | xe_dap      | object | "chiếc xe đạp màu xanh"    |
| Đồ vật   | ghe_go      | object | "ghế gỗ ở phòng khách"     |
| Địa điểm | phong_khach | place  | "phòng khách"              |
| Địa điểm | vuon        | place  | "khu vườn nhỏ"             |
| Địa điểm | truong      | place  | "đến trường"               |
| Địa điểm | ngoai_o     | place  | "ở ngoại ô"                |
| Địa điểm | sau_nha     | place  | "sau nhà"                  |
| Thực vật | hoa         | plant  | "những loài hoa"           |
| Màu sắc  | xanh        | color  | "màu xanh"                 |

### 3.2.2 Xác định quan hệ với số ngôi

| Quan hệ        | Ngôi | Kiểu          | Nguồn trong đoạn văn       |
| -------------- | ---- | ------------- | -------------------------- |
| em_gai(X,Y)    | 2    | ⟨e,⟨e,t⟩⟩     | "Linh là em gái của Nhân"  |
| song_tai(X,Y)  | 2    | ⟨e,⟨e,t⟩⟩     | "sống tại một ngôi nhà"    |
| song_cung(X,Y) | 2    | ⟨e,⟨e,t⟩⟩     | "Nhân sống cùng em gái"    |
| so_huu(X,Y)    | 2    | ⟨e,⟨e,t⟩⟩     | "Nhân có một chiếc xe đạp" |
| mau(X,Y)       | 2    | ⟨e,⟨e,t⟩⟩     | "xe đạp màu xanh"          |
| tang(X,Y,Z)    | 3    | ⟨e,⟨e,⟨e,t⟩⟩⟩ | "bố tặng xe cho Nhân"      |
| cho(X,Y,Z)     | 3    | ⟨e,⟨e,⟨e,t⟩⟩⟩ | "chở Linh đến trường"      |
| thich(X,Y)     | 2    | ⟨e,⟨e,t⟩⟩     | "Linh thích xe đạp"        |
| cho_an(X,Y)    | 2    | ⟨e,⟨e,t⟩⟩     | "Nhân cho Miu ăn"          |
| vi_tri(X,Y)    | 2    | ⟨e,⟨e,t⟩⟩     | "Miu ở phòng khách"        |
| la_meo(X)      | 1    | ⟨e,t⟩         | "con mèo tên là Miu"       |

### 3.2.3 Phương pháp kiểm tra biểu thức hợp lệ

**Điều kiện hợp lệ:**
Một biểu thức logic vị từ E = P(a₁, a₂, ..., aₙ) hợp lệ khi:

1. P thuộc tập vị từ đã định nghĩa
2. Số đối số n bằng số ngôi của P
3. Mọi đối số aᵢ là hằng trong từ vựng hoặc là biến

**Thuật toán kiểm tra:**

```
VALID_EXPRESSION(E):
  Input: E = pred(P, [A₁, ..., Aₙ])

  1. IF P ∉ Predicates THEN RETURN False
  2. IF arity(P) ≠ n THEN RETURN False
  3. FOR i = 1 TO n:
       IF Aᵢ is not variable AND Aᵢ ∉ Constants:
         RETURN False
  4. RETURN True
```

---

## 3.3 Cài đặt Prolog

```prolog
% ========================================
% CONSTANTS (Hằng)
% ========================================
constant(nhan, person).
constant(linh, person).
constant(bo_nhan, person).
constant(miu, animal).
constant(xe_dap, object).
constant(ghe_go, object).
constant(phong_khach, place).
constant(vuon, place).
constant(hoa, plant).

% ========================================
% PREDICATES với số ngôi
% ========================================
predicate(em_gai, 2).
predicate(song_tai, 2).
predicate(song_cung, 2).
predicate(so_huu, 2).
predicate(thich, 2).
predicate(cho_an, 2).
predicate(vi_tri, 2).
predicate(tang, 3).
predicate(la_meo, 1).

% ========================================
% KIỂM TRA BIỂU THỨC HỢP LỆ
% ========================================
valid_constant(C) :- constant(C, _).
valid_constant(C) :- var(C).

valid_expression(pred(P, Args)) :-
    predicate(P, Arity),
    length(Args, Arity),
    forall(member(A, Args), valid_constant(A)).
```

**Ví dụ kiểm tra:**

```prolog
?- valid_expression(pred(thich, [linh, hoa])).
true.   % Hợp lệ

?- valid_expression(pred(thich, [linh])).
false.  % Không hợp lệ: thich cần 2 đối số

?- valid_expression(pred(bay, [miu])).
false.  % Không hợp lệ: bay không là vị từ
```

---

# 4. CÁC QUY TẮC NGỮ NGHĨA

## 4.1 PHÂN TÍCH CÚ PHÁP

### 4.1.1 Lý thuyết: Văn phạm phi ngữ cảnh

**Định nghĩa văn phạm phi ngữ cảnh G = (V, Σ, R, S):**

- V: Tập ký hiệu không kết thúc (non-terminals)
- Σ: Tập ký hiệu kết thúc (terminals)
- R: Tập luật sản sinh A → α
- S: Ký hiệu bắt đầu

**Đệ quy trái:**
Văn phạm có đệ quy trái nếu tồn tại luật A → Aα

**Khử đệ quy trái:**
Cho văn phạm có đệ quy trái: A → Aα | β

Khử thành:

```
A → βA'
A' → αA' | ε
```

---

### 4.1.2 Phương pháp xây dựng văn phạm

**Bước 1: Xác định các thành phần câu**

- NP (Noun Phrase): Danh ngữ
- VP (Verb Phrase): Động ngữ
- PP (Prepositional Phrase): Giới ngữ
- QM (Question Marker): Từ hỏi (không, phải không)
- WP (Wh-Pronoun): Đại từ nghi vấn (ai, gì, đâu)

**Bước 2: Viết luật sản sinh ban đầu (có đệ quy trái)**

```
S  → NP VP          (Câu khẳng định)
S  → NP VP QM       (Câu hỏi Yes/No)

NP → NP PP          (Danh ngữ + Giới ngữ) ← ĐỆ QUY TRÁI
NP → N              (Danh từ đơn)
NP → WP             (Đại từ nghi vấn)
NP → Det N          (Định từ + Danh từ)

VP → VP NP          (Động ngữ + Tân ngữ) ← ĐỆ QUY TRÁI
VP → VP PP          (Động ngữ + Giới ngữ) ← ĐỆ QUY TRÁI
VP → V              (Động từ đơn)

PP → P NP           (Giới từ + Danh ngữ)
```

**Bước 3: Khử đệ quy trái**

**Cho NP:**

```
Trước: NP → NP PP | N | WP | Det N

Sau:   NP  → N NP'
       NP  → WP NP'
       NP  → Det N NP'
       NP' → PP NP'
       NP' → ε
```

**Cho VP:**

```
Trước: VP → VP NP | VP PP | V

Sau:   VP  → V VP'
       VP' → NP VP'
       VP' → PP VP'
       VP' → ε
```

---

### 4.1.3 Cây cú pháp của mỗi câu

#### Câu 1: "Linh thích gì"

**Tập luật áp dụng:**

```
S   → NP VP
NP  → N NP'
NP' → ε
VP  → V VP'
VP' → NP VP'
NP  → WP NP'
NP' → ε
VP' → ε
```

**Cây cú pháp:**

```
                       S
                ┌──────┴──────┐
               NP             VP
            ┌──┴──┐        ┌──┴──┐
           N    NP'       V     VP'
           │     │        │   ┌──┴──┐
         Linh    ε      thích NP   VP'
                              │     │
                          ┌──┴──┐   ε
                         WP   NP'
                          │     │
                         gì     ε
```

#### Câu 2: "Ai sở hữu xe đạp"

**Tập luật áp dụng:**

```
S   → NP VP
NP  → WP NP'
NP' → ε
VP  → V VP'
VP' → NP VP'
NP  → N NP'
VP' → ε
```

**Cây cú pháp:**

```
                       S
                ┌──────┴──────┐
               NP             VP
            ┌──┴──┐        ┌──┴──┐
           WP   NP'       V     VP'
            │     │        │   ┌──┴──┐
           ai     ε     sở_hữu NP   VP'
                              │     │
                          ┌──┴──┐   ε
                         N    NP'
                          │     │
                       xe_đạp   ε
```

#### Câu 3: "Linh thích hoa không" (Yes/No)

**Tập luật áp dụng:**

```
S   → NP VP QM
NP  → N NP'
NP' → ε
VP  → V VP'
VP' → NP VP'
NP  → N NP'
VP' → ε
```

**Cây cú pháp:**

```
                          S
              ┌───────────┼───────────┐
             NP           VP          QM
          ┌──┴──┐      ┌──┴──┐         │
         N    NP'     V     VP'      không
         │     │      │   ┌──┴──┐
       Linh    ε    thích NP   VP'
                          │     │
                      ┌──┴──┐   ε
                     N    NP'
                      │     │
                     hoa    ε
```

#### Câu 4: "Miu ở đâu?" (Where Question)

**Tập luật áp dụng:**

```
SBARQ → NP VP
NP    → NNP
VP    → VB NP
NP    → WRB
```

**Cây cú pháp (Penn Treebank format):**

```
                  SBARQ
            ┌───────┴───────┐
           NP               VP
            │          ┌────┴────┐
          NNP         VB        NP
            │          │         │
          miu          ở       WRB
                                 │
                               đâu
```

#### Câu 5: "Xe đạp là của ai?" (Copula + PP + WH)

**Tập luật áp dụng:**

```
SBARQ → NP VP_COPULA
NP    → NNP
VP_COPULA → VB PP
PP    → P NP
NP    → WP
```

**Cây cú pháp:**

```
                      SBARQ
               ┌────────┴────────┐
              NP            VP_COPULA
               │          ┌─────┴─────┐
             NNP         VB          PP
               │          │      ┌────┴────┐
            xe_đạp       là      P        NP
                                  │         │
                                của       WP
                                           │
                                          ai
```

#### Câu 6: "Ai là em gái của Nhân?" (WH + Copula + NP + PP)

**Tập luật áp dụng:**

```
SBARQ → WP VP_COPULA
VP_COPULA → VB NP PP
NP → NN
PP → P NP
NP → NNP
```

**Cây cú pháp:**

```
                          SBARQ
                    ┌───────┴────────┐
                   WP           VP_COPULA
                    │       ┌────┬────────┐
                   ai      VB   NP        PP
                            │    │    ┌───┴───┐
                           là   NN    P      NP
                                 │     │      │
                              em_gái của    NNP
                                             │
                                           nhân
```

#### Câu 7: "Ai cho Miu ăn?" (WHO + Compound Verb)

**Tập luật áp dụng:**

```
SBARQ → NP VP
NP    → WP
VP    → VB NP
```

**Lưu ý:** Tokenizer chuyển "cho X ăn" → "cho_an X"

**Cây cú pháp:**

```
                    SBARQ
             ┌────────┴────────┐
            NP                 VP
             │           ┌─────┴─────┐
            WP          VB          NP
             │           │           │
            ai        cho_an       NNP
                                    │
                                   miu
```

#### Câu 8: "Miu ngủ trong phòng khách phải không?" (Yes/No + VP + PP)

**Tập luật áp dụng:**

```
SQ → NP VP QM
NP → NNP
VP → VB PP
PP → P NP
NP → NNP
```

**Cây cú pháp:**

```
                        SQ
           ┌─────────────┼─────────────┐
          NP             VP            QM
           │        ┌────┴────┐         │
         NNP       VB        PP     phai_khong
           │        │    ┌───┴───┐
          miu      ngủ   P      NP
                          │      │
                        trong  NNP
                                │
                          phong_khach
```

#### Câu 9: "Nhân tặng Linh xe đạp phải không?" (Ditransitive)

**Tập luật áp dụng:**

```
SQ → NP VP QM
NP → NNP
VP → VB NP NP
```

**Cây cú pháp:**

```
                           SQ
          ┌────────────────┼────────────────┐
         NP                VP              QM
          │       ┌────────┼────────┐        │
        NNP      VB       NP       NP    phai_khong
          │       │        │        │
        nhân    tặng     NNP      NNP
                          │        │
                        linh    xe_đạp
```

#### Câu 10: "Nhân sống tại ngoại ô phải không?" (VP + NP với compound verb)

**Cây cú pháp:**

```
                      SQ
         ┌────────────┼────────────┐
        NP            VP           QM
         │       ┌────┴────┐        │
       NNP      VB        NP    phai_khong
         │       │         │
       nhân  song_tai    NNP
                          │
                       ngoai_o
```

**Lưu ý:** "sống tại" được tokenize thành "song_tai" và parse như VB transitive

---

### 4.1.4 Kết quả tổng hợp tập luật sản sinh

**Tập luật ĐÃ KHỬ ĐỆ QUY TRÁI và MỞ RỘNG:**

#### A. Luật cấu trúc câu (Sentence Rules)

```
# Câu hỏi WH (What, Who, Where)
SBARQ → NP VP                    # Linh thích gì?
SBARQ → WP VP                    # Ai thích hoa?
SBARQ → WP VP_COPULA             # Ai là em gái của Nhân?
SBARQ → NP VP_COPULA             # Xe đạp là của ai?

# Câu hỏi Yes/No
SQ → NP VP QM                    # Linh thích hoa không?
```

#### B. Luật Noun Phrase (đã khử đệ quy trái)

```
NP  → NNP                        # Proper Noun: Linh, Nhân, Miu
NP  → NN                         # Common Noun: mèo, xe
NP  → WP                         # Wh-Pronoun: ai, gì
NP  → WRB                        # Wh-Adverb: đâu
NP  → PN                         # Pronoun: nó, đó
NP  → DT NN                      # Determiner + Noun
NP  → DT CL NN                   # Det + Classifier + Noun
```

#### C. Luật Verb Phrase (đã khử đệ quy trái)

```
VP  → VB                         # Intransitive: ngủ
VP  → VB NP                      # Transitive: thích hoa
VP  → VB NP NP                   # Ditransitive: tặng Linh xe_đạp
VP  → VB PP                      # VP + PP: ngủ trong phòng_khách

# Copula
VP_COPULA → VB-copula PP         # là của ai
VP_COPULA → VB-copula NP PP      # là em_gái của Nhân
VP_COPULA → VB-copula WP PP      # là gì của Nhân
```

#### D. Luật Prepositional Phrase

```
PP  → P NP                       # trong phòng_khách, của Nhân
```

#### E. Lexical Categories

```
# Proper Nouns (NNP)
NNP → linh | nhan | miu | hoa | xe_dap | vuon | phong_khach | ngoai_o | sau_nha

# Common Nouns (NN)
NN  → nguoi | meo | em_gai | ten | bong

# Transitive Verbs (VB)
VB  → thich | so_huu | cho_an | song_cung | song_tai | ngam | choi_voi | o

# Ditransitive Verbs
VB  → tang | cho

# Copula
VB-copula → la

# Wh-Pronouns (WP)
WP  → ai | gi

# Wh-Adverbs (WRB)
WRB → dau

# Prepositions (P)
P   → trong | cua | tai | sau | tren

# Question Markers (QM)
QM  → khong | phai_khong

# Determiners (DT)
DT  → mot | moi

# Classifiers (CL)
CL  → con | chiec | cai
```

#### F. Quy tắc khử đệ quy trái

**Lý thuyết:** Khử đệ quy trái sử dụng luật:

```
X  → Xα | β
```

Chuyển thành:

```
X  → βX'
X' → αX' | ε
```

**Áp dụng:**

```
# NP ban đầu có đệ quy trái:
NP → NP PP | N | WP | ...

# Sau khi khử:
NP  → N NP' | WP NP' | ...
NP' → PP NP' | ε

# VP ban đầu:
VP → VP NP | VP PP | V

# Sau khi khử:
VP  → V VP'
VP' → NP VP' | PP VP' | ε
```

---

### 4.1.5 Cài đặt Prolog với DCG

**Lưu ý:** Trong cài đặt thực tế, sử dụng văn phạm mở rộng theo format Penn Treebank (PTB) để xử lý đầy đủ các loại câu hỏi.

#### A. Luật cấu trúc câu

```prolog
% ========================================
% VĂN PHẠM DCG MỞ RỘNG THEO PENN TREEBANK
% ========================================

% SBARQ - Câu hỏi WH
sbarq(tree(sbarq, [NP, VP])) -->
    np(NP),
    vp(VP).

% SBARQ với WP ở đầu (Ai là em gái của Nhân?)
sbarq(tree(sbarq, [WP, VP])) -->
    wp(WP),
    vp_copula(VP).

% SQ - Câu hỏi Yes/No
sq(tree(sq, [NP, VP, QM])) -->
    np(NP),
    vp(VP),
    qm(QM).
```

#### B. Luật Noun Phrase (NP)

```prolog
% NP -> NNP (Proper Noun: Linh, Nhân, Miu)
np(tree(np, [NNP])) --> nnp(NNP).

% NP -> NN (Common Noun: mèo, xe)
np(tree(np, [NN])) --> nn(NN).

% NP -> WP (Wh-Pronoun: ai, gì)
np(tree(np, [WP])) --> wp(WP).

% NP -> WRB (Wh-Adverb: đâu)
np(tree(np, [WRB])) --> wrb(WRB).

% NP -> PN (Pronoun: nó, đó)
np(tree(np, [PN])) --> pn(PN).

% NP -> DT NN (Determiner + Noun: một con mèo)
np(tree(np, [DT, NN])) --> dt(DT), nn(NN).

% NP -> DT CL NN (Determiner + Classifier + Noun)
np(tree(np, [DT, CL, NN])) --> dt(DT), cl(CL), nn(NN).

% Compound Noun (khu vườn, xe đạp, phòng khách)
compound_noun(tree(nnp, [word(khu_vuon, noun_proper)])) --> [khu, vuon].
compound_noun(tree(nnp, [word(xe_dap, noun_proper)])) --> [xe, dap].
compound_noun(tree(nnp, [word(em_gai, noun_common)])) --> [em, gai].
```

#### C. Luật Verb Phrase (VP)

```prolog
% VP -> VB (Intransitive: ngủ, chạy)
vp(tree(vp, [VB])) --> vb_intrans(VB).

% VP -> VB NP (Transitive: thích hoa, sở hữu xe đạp)
vp(tree(vp, [VB, NP])) --> vb_trans(VB), np(NP).

% VP -> VB NP NP (Ditransitive: tặng Linh xe đạp)
vp(tree(vp, [VB, NP1, NP2])) --> vb_ditrans(VB), np(NP1), np(NP2).

% VP -> VB PP (Verb + PP: ngủ trong phòng khách)
vp(tree(vp, [VB, PP])) --> vb_intrans(VB), pp(PP).

% VP_COPULA -> VB-copula PP (là của Linh)
vp_copula(tree(vp_copula, [VB, PP])) --> vb_copula(VB), pp(PP).

% VP_COPULA -> VB-copula NP PP (là em gái của Nhân)
vp_copula(tree(vp_copula, [VB, NP, PP])) --> vb_copula(VB), np(NP), pp(PP).
```

#### D. Luật Prepositional Phrase (PP)

```prolog
% PP -> P NP (trong phòng khách, của Nhân)
pp(tree(pp, [P, NP])) --> p(P), np(NP).
```

#### E. Terminals với POS tags

```prolog
% NNP - Proper Noun
nnp(tree(nnp, [word(W, noun_proper)])) --> [W], {is_proper_noun(W)}.

% NN - Common Noun
nn(tree(nn, [word(W, noun_common)])) --> [W], {is_common_noun(W)}.

% VB - Transitive Verb
vb_trans(tree(vb, [word(W, verb_trans)])) --> [W], {is_trans_verb(W)}.

% VB - Ditransitive Verb
vb_ditrans(tree(vb, [word(W, verb_ditrans)])) --> [W], {is_ditrans_verb(W)}.

% VB - Copula (là)
vb_copula(tree(vb, [word(W, copula)])) --> [W], {is_copula(W)}.

% WP - Wh-Pronoun
wp(tree(wp, [word(W, wh_word)])) --> [W], {is_wh_pronoun(W)}.

% WRB - Wh-Adverb
wrb(tree(wrb, [word(W, wh_word)])) --> [W], {is_wh_adverb(W)}.

% P - Preposition
p(tree(p, [word(W, preposition)])) --> [W], {is_preposition(W)}.

% QM - Question Marker
qm(tree(qm, [word(W, qm)])) --> [W], {is_question_marker(W)}.
```

#### F. Lexicon (Từ điển)

```prolog
% Proper Nouns
is_proper_noun(linh). is_proper_noun(nhan). is_proper_noun(miu).
is_proper_noun(hoa). is_proper_noun(xe_dap). is_proper_noun(vuon).
is_proper_noun(phong_khach). is_proper_noun(ngoai_o). is_proper_noun(sau_nha).

% Common Nouns
is_common_noun(nguoi). is_common_noun(meo). is_common_noun(em_gai).

% Transitive Verbs
is_trans_verb(thich). is_trans_verb(so_huu). is_trans_verb(cho_an).
is_trans_verb(song_cung). is_trans_verb(song_tai). is_trans_verb(ngam).

% Ditransitive Verbs
is_ditrans_verb(tang). is_ditrans_verb(cho).

% Copula
is_copula(la).

% Wh-Pronouns & Adverbs
is_wh_pronoun(ai). is_wh_pronoun(gi).
is_wh_adverb(dau).

% Prepositions
is_preposition(trong). is_preposition(cua). is_preposition(tai). is_preposition(sau).

% Question Markers
is_question_marker(khong). is_question_marker(phai_khong).
```

---

## 4.2 TỔNG HỢP QUY TẮC NGỮ NGHĨA

### 4.2.1 Lý thuyết: Biểu thức Lambda (Slide-BUOI-08)

**Ký hiệu Lambda:**

Biểu diễn hàm f nhận tham số x ∈ D và trả về f(x):

```
λx. f(x)
```

**Truyền đối số (Beta Reduction):**

```
(λx. f(x))@a = f(a)
```

Khi truyền đối số a, thay thế tất cả x trong f(x) bằng a.

**Ví dụ:**

```
(λx. x+1)@3 = 3+1 = 4
(λP. P@nam)@(λX. nguoi(X)) = (λX. nguoi(X))@nam = nguoi(nam)
```

---

### 4.2.2 Lý thuyết: Quy tắc tính toán ngữ nghĩa (Heim & Kratzer, 1998)

**Quy tắc 1: Áp dụng hàm (Functional Application)**

Nếu X là một nút có hai nút con Y và Z, trong đó Y là hàm có miền xác định chứa ‖Z‖ thì:

```
‖X‖ = ‖Y‖(‖Z‖) = ‖Y‖@‖Z‖
```

**Quy tắc 2: Nút không phân nhánh (Non-branching Node)**

Nếu X là nút cha có một nút con duy nhất Y thì:

```
‖X‖ = ‖Y‖
```

**Quy tắc 3: Ký hiệu kết thúc (Terminal)**

Nếu X là ký hiệu kết thúc, ‖X‖ được xác định từ từ điển.

---

### 4.2.3 Lý thuyết: Biểu diễn nghĩa của từ (Slide-BUOI-09)

**1. Danh từ riêng (Proper Noun)**

Kiểu: ⟨⟨e,t⟩,t⟩

```
λP. (P@entity)
```

Ví dụ: Nam → λP. (P@nam)

**2. Danh từ chung (Common Noun)**

Kiểu: ⟨e,t⟩

```
λX. noun(X)
```

Ví dụ: mèo → λX. mèo(X)

**3. Động từ nội động (Intransitive Verb)**

Kiểu: ⟨e,t⟩

```
λX. verb(X)
```

Ví dụ: ngủ → λX. ngủ(X)

**4. Động từ ngoại động (Transitive Verb)**

Kiểu: ⟨⟨⟨e,t⟩,t⟩, ⟨e,t⟩⟩

```
λP. λX. P@(λY. verb(X, Y))
```

Ví dụ: ăn → λP. λX. P@(λY. ăn(X, Y))

**5. Động từ 3 ngôi (Ditransitive Verb)**

Kiểu: ⟨NP, ⟨NP, ⟨e,t⟩⟩⟩

```
λP. λQ. λX. (Q@(λY. P@(λZ. verb(X, Z, Y))))
```

Ví dụ: tặng → λP. λQ. λX. (Q@(λY. P@(λZ. tặng(X, Z, Y))))

**6. Định từ "một" (Existential)**

Kiểu: ⟨⟨e,t⟩, ⟨⟨e,t⟩,t⟩⟩

```
λP. λQ. ∃X. (P@X ∧ Q@X)
```

**7. Định từ "mọi" (Universal)**

Kiểu: ⟨⟨e,t⟩, ⟨⟨e,t⟩,t⟩⟩

```
λP. λQ. ∀X. (P@X → Q@X)
```

---

### 4.2.4 Phương pháp tính toán ngữ nghĩa

**Ví dụ chi tiết: "Linh thích hoa"**

**Bước 1: Tra nghĩa từ vựng**

```
‖Linh‖ = λP. (P@linh)           Kiểu: ⟨⟨e,t⟩,t⟩
‖thích‖ = λP. λX. P@(λY. thích(X,Y))    Kiểu: ⟨⟨⟨e,t⟩,t⟩, ⟨e,t⟩⟩
‖hoa‖  = λP. (P@hoa)            Kiểu: ⟨⟨e,t⟩,t⟩
```

**Bước 2: Tính ‖VP‖ = ‖thích‖@‖hoa‖**

```
‖VP‖ = (λP. λX. P@(λY. thích(X,Y)))@(λP. (P@hoa))

Beta Reduction (thay P bằng λP. (P@hoa)):
= λX. (λP. (P@hoa))@(λY. thích(X,Y))

Beta Reduction (thay P bằng λY. thích(X,Y)):
= λX. ((λY. thích(X,Y))@hoa)

Beta Reduction (thay Y bằng hoa):
= λX. thích(X, hoa)
```

**Bước 3: Tính ‖S‖ = ‖Linh‖@‖VP‖**

```
‖S‖ = (λP. (P@linh))@(λX. thích(X, hoa))

Beta Reduction (thay P bằng λX. thích(X, hoa)):
= (λX. thích(X, hoa))@linh

Beta Reduction (thay X bằng linh):
= thích(linh, hoa)
```

**Kết quả:** ‖Linh thích hoa‖ = thích(linh, hoa)

---

### 4.2.5 Danh sách luật sản sinh kèm quy tắc ngữ nghĩa

#### A. Luật cơ bản

| Luật sản sinh | Quy tắc ngữ nghĩa | Giải thích                   |
| ------------- | ----------------- | ---------------------------- |
| S → NP VP     | ‖S‖ = ‖NP‖@‖VP‖   | Functional Application       |
| S → NP VP QM  | ‖S‖ = ‖NP‖@‖VP‖   | QM không ảnh hưởng ngữ nghĩa |
| NP → NNP      | ‖NP‖ = ‖NNP‖      | Non-branching                |
| NP → WP       | ‖NP‖ = wh(‖WP‖)   | Tạo câu hỏi                  |
| NP → DT NN    | ‖NP‖ = ‖DT‖@‖NN‖  | Functional Application       |
| VP → VB       | ‖VP‖ = ‖VB‖       | Non-branching                |
| VP → VB NP    | ‖VP‖ = ‖VB‖@‖NP‖  | Functional Application       |

#### B. Luật mở rộng cho cấu trúc phức tạp

| Luật sản sinh        | Quy tắc ngữ nghĩa                   | Ví dụ                       |
| -------------------- | ----------------------------------- | --------------------------- |
| VP → VB NP NP        | ‖VP‖ = pred(‖VB‖, [_, NP1, NP2])    | tặng Linh xe đạp            |
| VP → VB PP           | ‖VP‖ = λx. vi_tri(x, ‖PP‖.location) | ngủ trong phòng khách       |
| VP → VB-copula PP    | ‖VP‖ = ‖PP‖                         | là của Linh                 |
| VP → VB-copula NP PP | ‖VP‖ = λx. pred(‖NP‖, [x, ‖PP‖])    | là em gái của Nhân          |
| PP → P NP            | ‖PP‖ = pp_sem(‖P‖, ‖NP‖)            | trong phòng khách, của Nhân |

#### C. Luật cho câu hỏi WH mở rộng

| Pattern             | Quy tắc ngữ nghĩa                        | Ví dụ               |
| ------------------- | ---------------------------------------- | ------------------- |
| WP VB-copula NP PP  | wh*question(who, pred(NP, [*, PP.obj]))  | Ai là em gái của X? |
| NP VB-copula PP(WP) | wh*question(who, pred(so_huu, [*, NP]))  | X là của ai?        |
| NP VB-copula WP PP  | wh*question(what_relation, rel(NP,*,PP)) | X là gì của Y?      |

---

### 4.2.6 Phương pháp xác định đồng sở chỉ (Anaphora Resolution)

**Lý thuyết:**

Đồng sở chỉ là hiện tượng một từ (anaphor) tham chiếu đến thực thể đã được đề cập trước đó (antecedent).

**Thuật toán:**

```
RESOLVE_ANAPHORA(sentence):
  1. discourse_entities = []

  2. FOR EACH word IN sentence:
       IF word là NP mới:
         Thêm (word, features) vào discourse_entities
       ELSE IF word là pronoun:
         candidates = tìm entities phù hợp features
         antecedent = chọn gần nhất
         Thay thế pronoun bằng antecedent

  3. RETURN sentence với đại từ đã được resolve
```

**Ví dụ:**

```
"Linh có một con mèo. Nó ngủ."

Bước 1: Xử lý "Linh có một con mèo"
  - linh → discourse_entity(linh, person)
  - mèo → discourse_entity(meo, animal)

Bước 2: Xử lý "Nó ngủ"
  - "Nó" là pronoun (3rd, singular)
  - Candidates: [linh, meo]
  - Chọn gần nhất: meo

Kết quả: "Nó" = meo → ngủ(meo)
```

---

### 4.2.7 Cài đặt Prolog

```prolog
% ========================================
% NGHĨA TỪ VỰNG THEO LAMBDA
% ========================================

% Danh từ riêng: λP. (P@entity)
word_semantics(linh, noun_proper, lambda(p, app(p, const(linh)))).
word_semantics(nhan, noun_proper, lambda(p, app(p, const(nhan)))).
word_semantics(miu, noun_proper, lambda(p, app(p, const(miu)))).
word_semantics(hoa, noun_proper, lambda(p, app(p, const(hoa)))).

% Động từ ngoại động: λP. λX. P@(λY. verb(X,Y))
word_semantics(thich, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(thich, [x, y])))))).
word_semantics(so_huu, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(so_huu, [x, y])))))).
word_semantics(cho_an, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(cho_an, [x, y])))))).

% Định từ "một": λP. λQ. ∃X. (P@X ∧ Q@X)
word_semantics(mot, determiner,
    lambda(p, lambda(q, exists(x, conj(app(p, x), app(q, x)))))).

% ========================================
% BETA REDUCTION
% (λX. Body)@Arg → Body[X := Arg]
% ========================================

beta_reduce_step(app(lambda(Var, Body), Arg), Result) :- !,
    substitute(Var, Arg, Body, Result).

beta_reduce_step(X, X).

beta_reduce_full(Term, Result) :-
    beta_reduce_step(Term, Stepped),
    ( Stepped == Term ->
        Result = Term
    ;
        beta_reduce_full(Stepped, Result)
    ).

% ========================================
% SUBSTITUTION
% ========================================

substitute(Var, Value, Var, Value) :- !.
substitute(Var, _, lambda(Var, Body), lambda(Var, Body)) :- !.
substitute(Var, Value, lambda(V, Body), lambda(V, NewBody)) :- !,
    substitute(Var, Value, Body, NewBody).
substitute(Var, Value, app(F, A), app(NF, NA)) :- !,
    substitute(Var, Value, F, NF),
    substitute(Var, Value, A, NA).
substitute(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(sub_arg(Var, Value), Args, NewArgs).
substitute(_, _, X, X).

sub_arg(Var, Value, Var, Value) :- !.
sub_arg(_, _, X, X).

% ========================================
% SEMANTIC COMPOSITION
% ========================================

% S → NP VP
compose(tree(s, [NP, VP]), Semantics) :-
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% VP → V VP' (VP' = NP VP')
compose(tree(vp, [V, tree(vp_prime, [NP, _])]), Semantics) :-
    compose(V, VSem),
    compose(NP, NPSem),
    RawSem = app(VSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% Terminal
compose(tree(n, [word(W)]), Sem) :- word_semantics(W, _, Sem).
compose(tree(v, [word(W)]), Sem) :- word_semantics(W, _, Sem).
```

---

# 5. CÀI ĐẶT VỚI PROLOG

## 5.1 TRI THỨC CHUNG

### 5.1.1 Lý thuyết: Biểu diễn tri thức (Slide-DOAN-01)

Tri thức chung cần được thể hiện trên Prolog để suy diễn và trả lời câu hỏi:

1. **Từ vựng bậc một:** Dùng để kiểm tra tính hợp lệ của biểu thức
2. **Các vị từ:** Thể hiện nội dung đoạn văn
3. **Tính chất và quan hệ giữa các vị từ:** Dùng để suy diễn

---

### 5.1.2 Phương pháp biểu diễn

**Biểu diễn hằng:**

```
constant(Name, Type).
```

**Biểu diễn vị từ:**

```
predicate(Name, Arity).
```

**Biểu diễn sự kiện:**

```
fact(pred(Predicate, [Arg1, Arg2, ...])).
```

---

### 5.1.3 Cài đặt Prolog: Từ vựng bậc một

```prolog
% ========================================
% CONSTANTS (Hằng)
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
constant(nha_nho, place).
constant(phong_khach, place).
constant(vuon, place).
constant(truong, place).
constant(ngoai_o, place).
constant(sau_nha, place).

% Thực vật
constant(hoa, plant).

% Màu sắc
constant(xanh, color).

% ========================================
% PREDICATES (Vị từ với số ngôi)
% Format: predicate(Name, Arity)
% ========================================

predicate(em_gai, 2).      % X là em gái của Y
predicate(song_tai, 2).    % X sống tại Y
predicate(song_cung, 2).   % X sống cùng Y
predicate(so_huu, 2).      % X sở hữu Y
predicate(mau, 2).         % X có màu Y
predicate(tang, 3).        % X tặng Z cho Y
predicate(cho, 3).         % X chở Y đến Z
predicate(thich, 2).       % X thích Y
predicate(ten, 2).         % X tên là Y
predicate(la_meo, 1).      % X là mèo
predicate(nam_ngu, 2).     % X nằm ngủ tại Y
predicate(cho_an, 2).      % X cho Y ăn
predicate(choi_voi, 2).    % X chơi với Y
predicate(vi_tri, 2).      % X ở vị trí Y
predicate(chua, 2).        % X chứa Y
predicate(ngam, 2).        % X ngắm Y

% ========================================
% KIỂM TRA BIỂU THỨC HỢP LỆ
% ========================================

valid_constant(C) :- constant(C, _).
valid_constant(C) :- var(C).

valid_expression(pred(P, Args)) :-
    predicate(P, Arity),
    length(Args, Arity),
    forall(member(A, Args), valid_constant(A)).
```

---

### 5.1.4 Cài đặt Prolog: Tri thức từ đoạn văn

```prolog
% ========================================
% FACTS (Sự kiện từ đoạn văn)
% Format: fact(pred(Predicate, [Args]))
% ========================================

% Câu 1: Linh là em gái của Nhân.
fact(pred(em_gai, [linh, nhan])).

% Câu 2: Nhân và Linh sống tại một ngôi nhà nhỏ ở ngoại ô.
fact(pred(song_tai, [nhan, nha_nho])).
fact(pred(song_tai, [linh, nha_nho])).
fact(pred(vi_tri, [nha_nho, ngoai_o])).

% Suy diễn: Nhân sống cùng Linh (và ngược lại)
fact(pred(song_cung, [nhan, linh])).
fact(pred(song_cung, [linh, nhan])).

% Câu 3: Nhân sở hữu một chiếc xe đạp màu xanh.
fact(pred(so_huu, [nhan, xe_dap])).
fact(pred(mau, [xe_dap, xanh])).

% Câu 4: Bố Nhân đã tặng xe đạp này cho Nhân.
fact(pred(tang, [bo_nhan, nhan, xe_dap])).

% Câu 5: Hàng ngày, Nhân chở Linh đến trường.
fact(pred(cho, [nhan, linh, truong])).

% Câu 6: Linh thích xe đạp.
fact(pred(thich, [linh, xe_dap])).

% Câu 7: Linh thích hoa.
fact(pred(thich, [linh, hoa])).

% Câu 8: Linh có một con mèo tên Miu.
fact(pred(so_huu, [linh, miu])).
fact(pred(ten, [miu, miu])).
fact(pred(la_meo, [miu])).

% Câu 9: Miu nằm ngủ trên ghế gỗ trong phòng khách.
fact(pred(nam_ngu, [miu, ghe_go])).
fact(pred(vi_tri, [ghe_go, phong_khach])).
fact(pred(vi_tri, [miu, phong_khach])).

% Câu 10: Nhân cho Miu ăn và chơi với Miu.
fact(pred(cho_an, [nhan, miu])).
fact(pred(choi_voi, [nhan, miu])).

% Câu 11: Sau nhà có một khu vườn.
fact(pred(vi_tri, [vuon, sau_nha])).
fact(pred(chua, [vuon, hoa])).

% Câu 12: Linh thích ngắm hoa trong vườn.
fact(pred(ngam, [linh, hoa])).
fact(pred(thich, [linh, vuon])).  % Suy diễn
```

---

### 5.1.5 Lý thuyết: Quan hệ đối xứng và bắc cầu

**Quan hệ đối xứng (Symmetric):**

```
R(X, Y) ⟺ R(Y, X)
```

Ví dụ: song_cung(nhan, linh) ⟺ song_cung(linh, nhan)

**Quan hệ bắc cầu (Transitive):**

```
R(X, Y) ∧ R(Y, Z) ⟹ R(X, Z)
```

Ví dụ: vi_tri(ghe_go, phong_khach) ∧ vi_tri(phong_khach, nha) ⟹ vi_tri(ghe_go, nha)

---

### 5.1.6 Cài đặt Prolog: Quan hệ đối xứng và bắc cầu

```prolog
% ========================================
% QUAN HỆ ĐỐI XỨNG
% Định nghĩa: R(X,Y) ⟺ R(Y,X)
% ========================================

symmetric_relation(song_cung).

% Kiểm tra quan hệ đối xứng
check_symmetric(Pred, X, Y) :-
    symmetric_relation(Pred),
    ( fact(pred(Pred, [X, Y]))
    ; fact(pred(Pred, [Y, X]))
    ).

% Ví dụ:
% ?- check_symmetric(song_cung, nhan, linh).
% true.
% ?- check_symmetric(song_cung, linh, nhan).
% true.

% ========================================
% QUAN HỆ BẮC CẦU
% Định nghĩa: R(X,Y) ∧ R(Y,Z) ⟹ R(X,Z)
% ========================================

% vi_tri có tính bắc cầu
transitive_location(X, Z) :-
    fact(pred(vi_tri, [X, Y])),
    fact(pred(vi_tri, [Y, Z])),
    X \= Z.

% Ví dụ:
% vi_tri(nha_nho, ngoai_o) có thể suy diễn thêm

% song_tai bắc cầu qua vi_tri
infer_lives_at(X, Z) :-
    fact(pred(song_tai, [X, Y])),
    fact(pred(vi_tri, [Y, Z])).

% Ví dụ:
% song_tai(nhan, nha_nho) ∧ vi_tri(nha_nho, ngoai_o)
% ⟹ infer_lives_at(nhan, ngoai_o)
```

---

## 5.2 DỊCH CÂU HỎI SANG FOL

### 5.2.1 Lý thuyết: Quy trình dịch (Slide-DOAN-01)

Dịch câu hỏi sang FOL theo trình tự:

1. Tách câu
2. Tách từ (Tokenization)
3. Dịch câu hỏi sang DRS và giải quyết đồng sở chỉ
4. Dịch DRS sang FOL

---

### 5.2.2 Cài đặt Prolog: Nghĩa từ vựng theo DRS với Lambda

```prolog
% ========================================
% NGHĨA TỪ VỰNG THEO LAMBDA DRS
% ========================================

% --- 1. Danh từ riêng: λP. (P@entity) ---
% Kiểu: ⟨⟨e,t⟩,t⟩
word_semantics(linh, noun_proper, lambda(p, app(p, const(linh)))).
word_semantics(nhan, noun_proper, lambda(p, app(p, const(nhan)))).
word_semantics(miu, noun_proper, lambda(p, app(p, const(miu)))).
word_semantics(hoa, noun_proper, lambda(p, app(p, const(hoa)))).
word_semantics(xe_dap, noun_common, lambda(p, app(p, const(xe_dap)))).
word_semantics(vuon, noun_common, lambda(p, app(p, const(vuon)))).

% --- 2. Động từ ngoại động: λP. λX. P@(λY. pred(X,Y)) ---
% Kiểu: ⟨⟨⟨e,t⟩,t⟩, ⟨e,t⟩⟩
word_semantics(thich, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(thich, [x, y])))))).
word_semantics(so_huu, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(so_huu, [x, y])))))).
word_semantics(cho_an, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(cho_an, [x, y])))))).
word_semantics(o, verb_trans,
    lambda(p, lambda(x, app(p, lambda(y, pred(vi_tri, [x, y])))))).

% --- 3. Định từ "một": λP. λQ. ∃X. (P@X ∧ Q@X) ---
word_semantics(mot, determiner,
    lambda(p, lambda(q, exists(x, conj(app(p, x), app(q, x)))))).

% --- 4. Từ nghi vấn ---
word_semantics(ai, wh_word, wh(who)).
word_semantics(gi, wh_word, wh(what)).
word_semantics(dau, wh_word, wh(where)).
```

---

### 5.2.3 Cài đặt Prolog: Quy tắc tính toán ngữ nghĩa

```prolog
% ========================================
% SEMANTIC COMPOSITION RULES
% Theo Heim & Kratzer (1998)
% ========================================

% Quy tắc 1: S → NP VP
% ‖S‖ = ‖NP‖@‖VP‖
compose(tree(s, [NP, VP]), Semantics) :-
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% Quy tắc 2: S → NP VP QM (Yes/No question)
% ‖S‖ = ‖NP‖@‖VP‖ (QM không ảnh hưởng)
compose(tree(s, [NP, VP, _QM]), Semantics) :-
    compose(NP, NPSem),
    compose(VP, VPSem),
    RawSem = app(NPSem, VPSem),
    beta_reduce_full(RawSem, Semantics).

% Quy tắc 3: NP → N NP' (NP' = ε)
% ‖NP‖ = ‖N‖
compose(tree(np, [N, tree(np_prime, [])]), Semantics) :-
    compose(N, Semantics).

% Quy tắc 4: NP → WP NP'
% ‖NP‖ = wh_marker(‖WP‖)
compose(tree(np, [WP, _]), wh_marker(Type)) :-
    WP = tree(wp, [word(W)]),
    word_semantics(W, wh_word, wh(Type)).

% Quy tắc 5: VP → V VP' (với VP' = NP VP'')
% ‖VP‖ = ‖V‖@‖NP‖
compose(tree(vp, [V, tree(vp_prime, [NP, _])]), Semantics) :-
    compose(V, VSem),
    compose(NP, NPSem),
    RawSem = app(VSem, NPSem),
    beta_reduce_full(RawSem, Semantics).

% Quy tắc 6: VP → V VP' (VP' = ε)
% ‖VP‖ = ‖V‖
compose(tree(vp, [V, tree(vp_prime, [])]), Semantics) :-
    compose(V, Semantics).

% Terminal nodes
compose(tree(n, [word(W)]), Semantics) :-
    word_semantics(W, _, Semantics).
compose(tree(v, [word(W)]), Semantics) :-
    word_semantics(W, _, Semantics).
```

---

### 5.2.4 Cài đặt Prolog: Xác định và tính toán biểu thức Lambda

**Ví dụ chi tiết: "Linh thích hoa"**

```prolog
% Input: [linh, thich, hoa]
% Parse tree:
%   tree(s, [
%     tree(np, [tree(n, [word(linh)]), tree(np_prime, [])]),
%     tree(vp, [
%       tree(v, [word(thich)]),
%       tree(vp_prime, [
%         tree(np, [tree(n, [word(hoa)]), tree(np_prime, [])]),
%         tree(vp_prime, [])
%       ])
%     ])
%   ])

% Bước 1: Tra nghĩa từ vựng
% ‖linh‖ = lambda(p, app(p, const(linh)))
% ‖thích‖ = lambda(p, lambda(x, app(p, lambda(y, pred(thich, [x, y])))))
% ‖hoa‖ = lambda(p, app(p, const(hoa)))

% Bước 2: Tính ‖VP‖ = ‖thích‖@‖hoa‖
% app(lambda(p, lambda(x, app(p, lambda(y, pred(thich, [x, y]))))),
%     lambda(p, app(p, const(hoa))))
%
% Beta reduce (thay p):
% → lambda(x, app(lambda(p, app(p, const(hoa))),
%                 lambda(y, pred(thich, [x, y]))))
%
% Beta reduce (thay p trong inner):
% → lambda(x, app(lambda(y, pred(thich, [x, y])), const(hoa)))
%
% Beta reduce (thay y):
% → lambda(x, pred(thich, [x, const(hoa)]))

% Bước 3: Tính ‖S‖ = ‖linh‖@‖VP‖
% app(lambda(p, app(p, const(linh))),
%     lambda(x, pred(thich, [x, const(hoa)])))
%
% Beta reduce (thay p):
% → app(lambda(x, pred(thich, [x, const(hoa)])), const(linh))
%
% Beta reduce (thay x):
% → pred(thich, [const(linh), const(hoa)])

% Kết quả cuối cùng:
% ‖Linh thích hoa‖ = pred(thich, [const(linh), const(hoa)])
```

**Cài đặt Beta Reduction:**

```prolog
% ========================================
% BETA REDUCTION
% (λX. Body)@Arg → Body[X := Arg]
% ========================================

beta_reduce_step(app(lambda(Var, Body), Arg), Result) :- !,
    substitute(Var, Arg, Body, Result).

beta_reduce_step(lambda(Var, Body), lambda(Var, NewBody)) :- !,
    beta_reduce_step(Body, NewBody).

beta_reduce_step(app(F, A), Result) :- !,
    beta_reduce_step(F, RF),
    beta_reduce_step(A, RA),
    ( RF = lambda(_, _) ->
        beta_reduce_step(app(RF, RA), Result)
    ;
        Result = app(RF, RA)
    ).

beta_reduce_step(X, X).

% Full reduction
beta_reduce_full(Term, Result) :-
    beta_reduce_step(Term, Stepped),
    ( Stepped == Term ->
        Result = Term
    ;
        beta_reduce_full(Stepped, Result)
    ).

% ========================================
% SUBSTITUTION
% substitute(Var, Value, Term, Result)
% ========================================

substitute(Var, Value, Var, Value) :- !.
substitute(Var, _, lambda(Var, Body), lambda(Var, Body)) :- !.
substitute(Var, Value, lambda(V, Body), lambda(V, NewBody)) :- !,
    substitute(Var, Value, Body, NewBody).
substitute(Var, Value, app(F, A), app(NF, NA)) :- !,
    substitute(Var, Value, F, NF),
    substitute(Var, Value, A, NA).
substitute(Var, Value, pred(P, Args), pred(P, NewArgs)) :- !,
    maplist(sub_arg(Var, Value), Args, NewArgs).
substitute(Var, Value, const(X), const(X)) :- !.
substitute(Var, Value, conj(A, B), conj(NA, NB)) :- !,
    substitute(Var, Value, A, NA),
    substitute(Var, Value, B, NB).
substitute(Var, Value, exists(V, Body), exists(V, NewBody)) :-
    Var \= V, !,
    substitute(Var, Value, Body, NewBody).
substitute(_, _, X, X).

sub_arg(Var, Value, Var, Value) :- !.
sub_arg(Var, Value, const(X), NewVal) :- !,
    substitute(Var, Value, const(X), NewVal).
sub_arg(_, _, X, X).
```

---

### 5.2.5 Cài đặt Prolog: DRS Construction

**Lý thuyết DRS (Discourse Representation Structure):**

```
DRS = ⟨Referents, Conditions⟩
```

Trong đó:

- Referents: Tập các thực thể được giới thiệu
- Conditions: Tập các điều kiện/quan hệ

**Cài đặt:**

```prolog
% ========================================
% DRS CONSTRUCTION
% Format: drs(Referents, Conditions)
% ========================================

% pred → drs([], [pred])
build_drs(pred(P, Args), drs([], [pred(P, Args)])) :- !.

% const → drs([Entity], [])
build_drs(const(E), drs([E], [])) :- !.

% exists(X, Body) → drs([X|Refs], Conds)
build_drs(exists(X, Body), drs([X|BodyRefs], BodyConds)) :- !,
    build_drs(Body, drs(BodyRefs, BodyConds)).

% conj(A, B) → merge(DRS_A, DRS_B)
build_drs(conj(A, B), MergedDRS) :- !,
    build_drs(A, DRSA),
    build_drs(B, DRSB),
    merge_drs(DRSA, DRSB, MergedDRS).

% ========================================
% DRS MERGE (⊕)
% drs(Ra, Ca) ⊕ drs(Rb, Cb) = drs(Ra ∪ Rb, Ca ∪ Cb)
% ========================================

merge_drs(drs(RefsA, CondsA), drs(RefsB, CondsB), drs(MergedRefs, MergedConds)) :-
    append(RefsA, RefsB, AllRefs),
    sort(AllRefs, MergedRefs),
    append(CondsA, CondsB, MergedConds).
```

**Ví dụ DRS:**

```
Câu: "Linh thích hoa"

Semantics: pred(thich, [const(linh), const(hoa)])

DRS:
┌──────────────────────────────────────┐
│                                      │
├──────────────────────────────────────┤
│ thich(linh, hoa)                     │
└──────────────────────────────────────┘

Prolog: drs([], [pred(thich, [const(linh), const(hoa)])])
```

---

### 5.2.6 Cài đặt Prolog: DRS mức văn bản

```prolog
% ========================================
% DRS CỦA TOÀN BỘ ĐOẠN VĂN
% ========================================

build_discourse_drs(DRS) :-
    findall(pred(P, Args), fact(pred(P, Args)), AllFacts),
    findall(E, constant(E, _), AllEntities),
    DRS = drs(AllEntities, AllFacts).

% Kết quả:
% drs(
%   [nhan, linh, bo_nhan, miu, xe_dap, hoa, vuon, phong_khach, ...],
%   [
%     pred(em_gai, [linh, nhan]),
%     pred(song_tai, [nhan, nha_nho]),
%     pred(song_tai, [linh, nha_nho]),
%     pred(so_huu, [nhan, xe_dap]),
%     pred(thich, [linh, hoa]),
%     pred(thich, [linh, xe_dap]),
%     pred(so_huu, [linh, miu]),
%     pred(cho_an, [nhan, miu]),
%     pred(vi_tri, [miu, phong_khach]),
%     ...
%   ]
% )
```

---

### 5.2.7 Cài đặt Prolog: DRS sang FOL

```prolog
% ========================================
% DRS TO FOL CONVERSION
% ========================================

% drs([], Conds) → conjunction of conditions
convert(drs([], [Cond]), FOL) :- !,
    convert_condition(Cond, FOL).

convert(drs([], Conds), FOL) :- !,
    convert_conditions(Conds, FOL).

% drs([X|Refs], Conds) → ∃X. convert(drs(Refs, Conds))
convert(drs([Ref|Refs], Conds), exists(Ref, RestFOL)) :- !,
    convert(drs(Refs, Conds), RestFOL).

% Convert conditions
convert_condition(pred(P, Args), pred(P, Args)) :- !.
convert_condition(neg(DRS), neg(FOL)) :- !,
    convert(DRS, FOL).
convert_condition(impl(DRSA, DRSB), impl(FOLA, FOLB)) :- !,
    convert(DRSA, FOLA),
    convert(DRSB, FOLB).

% Convert list of conditions
convert_conditions([], true) :- !.
convert_conditions([C], FOL) :- !,
    convert_condition(C, FOL).
convert_conditions([C|Cs], conj(FC, Rest)) :-
    convert_condition(C, FC),
    convert_conditions(Cs, Rest).
```

---

### 5.2.8 Cài đặt Prolog: Kết quả trả lời câu hỏi

**Pipeline hoàn chỉnh:**

```prolog
% ========================================
% QUERY PROCESSING PIPELINE
% ========================================

query(Question, Type) :-
    format('~n========================================~n'),
    format('Query: ~w~n', [Question]),
    format('Type: ~w~n', [Type]),
    format('========================================~n~n'),

    % Stage 1: Tokenization
    tokenize(Question, Tokens),
    format('Stage 1: Tokenization... ~w~n', [Tokens]),

    % Stage 2: Parsing
    parse(Tokens, Type, Tree),
    format('Stage 2: Parsing... OK~n   Tree: ~w~n', [Tree]),

    % Stage 3: Lambda Composition
    compose(Tree, Lambda),
    format('Stage 3: Lambda Composition... OK~n   Lambda: ~w~n', [Lambda]),

    % Stage 4: DRS Construction
    build_drs(Lambda, DRS),
    format('Stage 4: DRS Construction... OK~n   DRS: ~w~n', [DRS]),

    % Stage 5: FOL Conversion
    convert(DRS, FOL),
    format('Stage 5: FOL Conversion... OK~n   FOL: ~w~n', [FOL]),

    % Stage 6: Theorem Proving
    format('~nStage 6: Proving... '),
    answer_question(Type, FOL, Answer),
    format('OK~n'),

    format('~n========================================~n'),
    format('Answer: ~w~n', [Answer]),
    format('========================================~n').

% ========================================
% ANSWER QUESTION BY TYPE
% ========================================

% Yes/No question
answer_question(yn, FOL, Answer) :-
    ( prove(FOL) ->
        Answer = 'TRUE / CÓ'
    ;
        Answer = 'FALSE / KHÔNG'
    ).

% Who question
answer_question(who, wh_question(who, Condition), Answer) :-
    findall(X, (
        constant(X, _),
        check_condition_for(X, Condition)
    ), Entities),
    Answer = Entities.

% What question
answer_question(what, wh_question(what, pred(P, [Subject, _])), Answer) :-
    findall(Y, fact(pred(P, [Subject, Y])), Values),
    Answer = Values.

% Where question
answer_question(where, wh_question(where, Subject), Answer) :-
    findall(L, fact(pred(vi_tri, [Subject, L])), Locations),
    Answer = Locations.

% ========================================
% THEOREM PROVER
% ========================================

prove(pred(P, Args)) :-
    strip_const(Args, StrippedArgs),
    fact(pred(P, StrippedArgs)).

prove(conj(A, B)) :-
    prove(A),
    prove(B).

prove(exists(_, Body)) :-
    prove(Body).

% Strip const() wrappers
strip_const([], []).
strip_const([const(X)|Rest], [X|RestStripped]) :- !,
    strip_const(Rest, RestStripped).
strip_const([X|Rest], [X|RestStripped]) :-
    strip_const(Rest, RestStripped).
```

---

### 5.3 Implement Phân giải đồng sở chỉ (Coreference Resolution)

Để xử lý các câu hỏi phức liên quan đến nhiều câu (nq19-22), hệ thống được mở rộng với các module sau:

#### 1. Tokenizer & Parser Mở rộng

- **Tokenizer:** Cập nhật để tách câu dựa trên dấu chấm (`.`), giữ lại dấu chấm làm separator.
- **Parser:** Thêm luật `TEXT -> S1 . S2` để parse văn bản gồm 2 câu.
- **NP_Statement:** Thêm luật `np_statement` để xử lý các câu dạng NP ("Người cho Miu ăn") như một mệnh đề độc lập.

#### 2. Composition Rules cho Coreference

- **Pronoun Resolution:** Xử lý các đại từ `nó`, `đó`, `chúng`.
  - Hệ thống trích xuất context (danh sách thực thể/mệnh đề) từ câu trước (S1).
  - Đại từ trong câu sau (S2) sẽ được thay thế bằng lambda function tham chiếu đến context này.
  - Ví dụ: `nó` -> `lambda(p, app(p, const(Ref)))`.

#### 3. Xử lý Logic và Beta Reduction

- **Relative Clause as Reference:** Xử lý mệnh đề quan hệ ("Người cho Miu ăn") như một định danh.
  - Rule `extract_referents` giữ nguyên cấu trúc `relative(...)`.
  - Thêm symmetric `beta_reduce` để xử lý `app(Entity, relative)` cho các câu hỏi dạng "Đó là Nhân?".
- **Existential Scope:** Với câu S1 có lượng từ tồn tại ("Linh có một con mèo"), logic S2 được đưa vào scope của S1: `exists(x, S1(x) & S2(x))`.

---

### 5.4 Phân tích chi tiết quy trình xử lý 4 câu hỏi phức

Phần này trình bày chi tiết quy trình từ Tokenization, Parsing, đến Logic derivation và Result cho 4 câu hỏi phức (nq19-nq22).

#### Case 1: nq19 - "Một chiếc xe màu xanh. Nó là của Linh?"

**Câu hỏi gốc:** `Một chiếc xe màu xanh. Nó là của Linh?`
**Phân tích Logic:**

- **S1:** "Một chiếc xe màu xanh" (NP Statement)
  - Semantics: `exists(e, conj(pred(xe,[e]), pred(mau,[e, xanh])))`
  - Entity: `e` (xe_dap)
- **S2:** "Nó là của Linh?"
  - Pronoun "Nó" resolved to `e` (context from S1).
  - Semantics: `pred(so_huu, [linh, e])`
- **Combined Logic:** `exists(e, xe(e) & mau(e,xanh) & so_huu(linh, e))`
  **Kết quả:**
- **Fact check:** KB có `so_huu(nhan, xe_dap)` nhưng không có `so_huu(linh, xe_dap)`.
- **Output:** `NO` (Chính xác).

#### Case 2: nq20 - "Linh có một con mèo. Nó tên là Miu?"

**Câu hỏi gốc:** `Linh có một con mèo. Nó tên là Miu?`
**Phân tích Logic:**

- **S1:** "Linh có một con mèo"
  - Logic: `exists(m, meo(m) & so_huu(linh, m))`
  - Entity: `m` (con mèo của Linh)
- **S2:** "Nó tên là Miu?"
  - Pronoun "Nó" resolved to `m`.
  - Logic: `pred(ten, [m, miu])`
- **Combined Logic:** `exists(m, meo(m) & so_huu(linh, m) & ten(m, miu))`
  **Kết quả:**
- **Fact check:** KB có `fact(pred(so_huu, [linh, miu]))` và `fact(pred(la_meo, [miu]))` (tên là miu).
- **Output:** `YES`.

#### Case 3: nq21 - "Người cho Miu ăn. Đó là Nhân?"

**Câu hỏi gốc:** `Người cho Miu ăn. Đó là Nhân?`
**Phân tích Logic:**

- **S1:** "Người cho Miu ăn" (Relative Clause NP)
  - Logic: `relative(nguoi, lambda(x, cho_an(x, miu)))`
  - Referent: Structure `relative(...)`
- **S2:** "Đó là Nhân?"
  - Pronoun "Đó" resolved to `relative(nguoi, cho_an(..., miu))`.
  - Logic (Symmetric Beta Reduce): `app(const(nhan), relative(nguoi, cho_an(..., miu)))`
  - Reduces to: `conj(pred(nguoi, [nhan]), pred(cho_an, [nhan, miu]))`
- **Combined Logic:** `nguoi(nhan) & cho_an(nhan, miu)`
  **Kết quả:**
- **Fact check:** KB có `fact(pred(cho_an, [nhan, ami]))` -> Đúng.
- **Output:** `YES`.

#### Case 4: nq22 - "Linh thích ngắm hoa. Chúng ở trong vườn?"

**Câu hỏi gốc:** `Linh thích ngắm hoa. Chúng ở trong vườn?`
**Phân tích Logic:**

- **S1:** "Linh thích ngắm hoa"
  - Logic: `pred(thich, [linh, hoa])`
  - Entity: `hoa` (Common Noun defined as Proper Noun in implementation context) or extracted from semantic structure.
- **S2:** "Chúng ở trong vườn?"
  - Pronoun "Chúng" (plural) resolved to `hoa`.
  - Logic: `pred(vi_tri, [hoa, vuon])`
- **Combined Logic:** `thich(linh, hoa) & vi_tri(hoa, vuon)`
  **Kết quả:**
- **Fact check:** KB có `fact(pred(vi_tri, [hoa, vuon]))`.
- **Output:** `YES`.

---

# 6. KẾT QUẢ THỬ NGHIỆM

## 6.1 Bảng kết quả test suite mở rộng (queries_cs229.pl)

### Câu hỏi Where

| ID  | Câu hỏi         | Lambda                   | Kết quả       | ✓   |
| --- | --------------- | ------------------------ | ------------- | --- |
| nq1 | Miu ở đâu?      | wh_question(where, miu)  | [phong_khach] | ✓   |
| nq2 | Khu vườn ở đâu? | wh_question(where, vuon) | [sau_nha]     | ✓   |

### Câu hỏi Who (đơn giản)

| ID   | Câu hỏi            | Lambda                                    | Kết quả | ✓   |
| ---- | ------------------ | ----------------------------------------- | ------- | --- |
| nq3  | Xe đạp là của ai?  | wh*question(who,pred(so_huu,[*,xe_dap]))  | [nhan]  | ✓   |
| nq4  | Ai sở hữu xe đạp?  | wh*question(who,pred(so_huu,[*,xe_dap]))  | [nhan]  | ✓   |
| nq8  | Ai sống cùng Linh? | wh*question(who,pred(song_cung,[*,linh])) | [nhan]  | ✓   |
| nq9  | Ai cho Miu ăn?     | wh*question(who,pred(cho_an,[*,miu]))     | [nhan]  | ✓   |
| nq10 | Ai chơi với Miu?   | wh*question(who,pred(choi_voi,[*,miu]))   | [nhan]  | ✓   |

### Câu hỏi Who (với copula)

| ID  | Câu hỏi                | Lambda                                 | Kết quả | ✓   |
| --- | ---------------------- | -------------------------------------- | ------- | --- |
| nq6 | Ai là em gái của Nhân? | wh*question(who,pred(em_gai,[*,nhan])) | [linh]  | ✓   |

### Câu hỏi What

| ID   | Câu hỏi            | Lambda                                 | Kết quả           | ✓   |
| ---- | ------------------ | -------------------------------------- | ----------------- | --- |
| nq11 | Linh thích gì?     | wh*question(what,pred(thich,[linh,*])) | [hoa,vuon,xe_dap] | ✓   |
| nq12 | Linh ngắm gì?      | wh*question(what,pred(ngam,[linh,*]))  | [hoa]             | ✓   |
| nq13 | Ai thích ngắm hoa? | wh_question(who, ...)                  | [linh]            | ✓   |

### Câu hỏi Yes/No (đơn giản)

| ID   | Câu hỏi                           | Lambda                        | Kết quả | ✓   |
| ---- | --------------------------------- | ----------------------------- | ------- | --- |
| nq14 | Linh thích xe đạp phải không?     | pred(thich,[linh,xe_dap])     | YES     | ✓   |
| nq15 | Nhân tặng Linh xe đạp phải không? | pred(tang,[nhan,linh,xe_dap]) | NO      | ✓   |

### Câu hỏi Yes/No (với PP)

| ID   | Câu hỏi                               | Lambda                         | Kết quả | ✓   |
| ---- | ------------------------------------- | ------------------------------ | ------- | --- |
| nq16 | Miu ngủ trong phòng khách phải không? | pred(vi_tri,[miu,phong_khach]) | YES     | ✓   |
| nq17 | Khu vườn ở sau nhà phải không?        | pred(vi_tri,[vuon,sau_nha])    | YES     | ✓   |
| nq18 | Nhân sống tại ngoại ô phải không?     | pred(song_tai,[nhan,ngoai_o])  | YES     | ✓   |

### Câu hỏi Phức (Coreference)

| ID   | Câu hỏi                                  | Logic (Sơ lược)                       | Kết quả | ✓   |
| ---- | ---------------------------------------- | ------------------------------------- | ------- | --- |
| nq19 | Một chiếc xe màu xanh. Nó là của Linh?   | exists(x, xe(x) & mau(x,xanh) & ...)  | NO      | ✓   |
| nq20 | Linh có một con mèo. Nó tên là Miu?      | exists(x, meo(x) & ten(x,miu) ...)    | YES     | ✓   |
| nq21 | Người cho Miu ăn. Đó là Nhân?            | query(nguoi(nhan) & cho_an(nhan,miu)) | YES     | ✓   |
| nq22 | Linh thích ngắm hoa. Chúng ở trong vườn? | query(vi_tri(hoa, vuon))              | YES     | ✓   |

## 6.2 Tổng kết kết quả

| Loại câu hỏi          | Số câu | Pass   | Tỷ lệ    |
| --------------------- | ------ | ------ | -------- |
| Where                 | 2      | 2      | 100%     |
| Who (đơn giản)        | 5      | 5      | 100%     |
| Who (copula)          | 1      | 1      | 100%     |
| What                  | 3      | 3      | 100%     |
| Yes/No (đơn giản)     | 2      | 2      | 100%     |
| Yes/No (với PP)       | 3      | 3      | 100%     |
| Yes/No (ditransitive) | 1      | 1      | 100%     |
| Phức (Coreference)    | 4      | 4      | 100%     |
| **TỔNG**              | **21** | **21** | **100%** |

## 6.3 Các pattern ngữ pháp được hỗ trợ

| Pattern             | Ví dụ                      | Mô tả               |
| ------------------- | -------------------------- | ------------------- |
| NP VP WP            | Linh thích gì?             | Object WH question  |
| WP VP NP            | Ai sở hữu xe đạp?          | Subject WH question |
| NP VB-copula PP(WP) | Xe đạp là của ai?          | Ownership question  |
| WP VB-copula NP PP  | Ai là em gái của Nhân?     | Relation question   |
| NP VB PP QM         | Miu ngủ trong phòng khách? | Location Yes/No     |
| NP VB NP NP QM      | Nhân tặng Linh xe đạp?     | Ditransitive Yes/No |
| Compound noun       | khu vườn, xe đạp           | Multi-word entity   |
| Compound verb       | cho X ăn                   | Discontinuous verb  |

## 6.4 Nhận xét

### Ưu điểm

1. **Áp dụng đúng lý thuyết từ môn học:**

   - Logic vị từ bậc một
   - Biểu thức Lambda theo slides
   - Quy tắc tính toán ngữ nghĩa (Heim & Kratzer)
   - DRS (Discourse Representation Structure)

2. **Xử lý đầy đủ các dạng câu hỏi:**

   - Where, Who, What, Yes/No
   - Copula constructions (là của ai, là em gái của)
   - Prepositional phrases (trong, tại, sau)
   - Ditransitive verbs (tặng X cho Y)

3. **Văn phạm mở rộng theo PTB:**

   - NP với WP, WRB, PN
   - VP với copula, PP
   - Compound nouns và discontinuous verbs

4. **Pipeline 6 bước hoàn chỉnh:**
   - Tokenization → Parsing → Lambda → DRS → FOL → Proving

### Hạn chế

1. Chưa xử lý đồng sở chỉ liên câu (cross-sentence coreference)
2. Câu hỏi "X là gì của Y" chưa trả về quan hệ
3. Chưa hỗ trợ tiếng Việt có dấu Unicode đầy đủ

### Hướng phát triển

1. Bổ sung xử lý đồng sở chỉ liên câu (pronoun resolution)
2. Mở rộng prover cho what_relation queries
3. Tích hợp giao diện web/chatbot

---

# HẾT BÁO CÁO
