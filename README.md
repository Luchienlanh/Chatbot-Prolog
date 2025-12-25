# Vietnamese Q&A System - Compositional Semantics

Hệ thống hỏi đáp tiếng Việt sử dụng ngữ nghĩa học thành phần (Compositional Semantics) được xây dựng trên Prolog.

## 📋 Mô tả

Hệ thống xử lý câu hỏi tiếng Việt thông qua pipeline:

1. **Tokenization** - Tách câu, tách từ
2. **Parsing** - Phân tích cú pháp → Cây cú pháp
3. **Lambda Composition** - Tổng hợp ngữ nghĩa
4. **DRS Construction** - Xây dựng Discourse Representation Structure
5. **FOL Conversion** - Chuyển DRS sang First-Order Logic
6. **Theorem Proving** - Suy diễn và trả lời câu hỏi

## 🗂️ Cấu trúc thư mục

```
NNHTT/
├── bootstrap.pl              # File khởi tạo chính
├── queries_cs229.pl          # Bộ test cases
├── linguistic/
│   ├── vocabulary.pl         # Từ điển và ngữ nghĩa từ vựng
│   └── composition.pl        # Lambda composition
├── parsing/
│   ├── analyzer.pl           # Tokenizer
│   └── structures.pl         # Cấu trúc cú pháp và parser
├── logic/
│   ├── discourse.pl          # DRS (Discourse Representation Structure)
│   └── firstorder.pl         # Chuyển đổi sang FOL
├── reasoning/
│   ├── inference.pl          # Forward inference
│   └── theorem.pl            # Theorem prover
└── knowledge/
    └── repository.pl         # Knowledge base
```

## 🛠️ Yêu cầu

- **SWI-Prolog** phiên bản 8.0 trở lên
  - Download: https://www.swi-prolog.org/Download.html

## 🚀 Cách chạy

### 1. Cài đặt SWI-Prolog

**Windows:**

- Tải installer từ https://www.swi-prolog.org/download/stable
- Chạy installer và làm theo hướng dẫn

**Linux (Ubuntu/Debian):**

```bash
sudo apt-get install swi-prolog
```

**macOS:**

```bash
brew install swi-prolog
```

### 2. Clone repository

```bash
git clone <repository-url>
cd NNHTT
```

### 3. Chạy hệ thống

#### Chế độ Interactive (Tương tác)

```bash
swipl -s bootstrap.pl -g interactive
```

Sau đó nhập câu hỏi trực tiếp:

```
> Linh thich gi
> Ai so huu xe dap
> exit
```

#### Chế độ Demo

```bash
swipl -s bootstrap.pl -g demo -t halt
```

#### Chạy Test Suite

```bash
swipl -s queries_cs229.pl -g run_all_tests
```

#### Chế độ Console

```bash
swipl -s bootstrap.pl
```

Sau đó gọi các lệnh:

```prolog
?- initialize.
?- query("Linh thich gi", what).
?- query("Ai so huu xe dap", who).
?- trace_query("Miu o dau", where).
```

## 📝 Các loại câu hỏi hỗ trợ

| Loại            | Ví dụ                   | Type    |
| --------------- | ----------------------- | ------- |
| **Yes/No**      | "Linh thich hoa khong?" | `yn`    |
| **Who (Ai)**    | "Ai so huu xe dap?"     | `who`   |
| **What (Gì)**   | "Linh thich gi?"        | `what`  |
| **Where (Đâu)** | "Miu o dau?"            | `where` |

## 💡 Các lệnh hữu ích

```prolog
% Khởi tạo hệ thống
?- initialize.

% Truy vấn
?- query("Câu hỏi", Type).

% Truy vấn với trace chi tiết
?- trace_query("Câu hỏi", Type).

% Chạy demo
?- demo.

% Chế độ tương tác
?- interactive.

% Chạy test
?- test.
```

## ⚠️ Lưu ý

- Câu hỏi có thể nhập **không dấu** hoặc **có dấu** (UTF-8)
- Kết thúc mỗi lệnh Prolog bằng dấu `.`
- Trong chế độ interactive, gõ `exit` hoặc `quit` để thoát
- Gõ `help` để xem hướng dẫn

## 📚 Tài liệu tham khảo

- Theo slides môn học **Ngôn ngữ học tính toán (NNHTT)**
- Sử dụng các kỹ thuật: Lambda Calculus, DRS, FOL, Theorem Proving

## 👤 Tác giả

Đồ án môn học NNHTT (Ngôn ngữ học tính toán)
