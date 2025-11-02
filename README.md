# ureflect

**Header-only C++23 compile-time reflection library**
Generates field names, types, and tuples directly from user-defined structs — no macros, no runtime cost, no external
dependencies.

---

## ✨ Features

* **Compile-time reflection** for plain structs and enums
* **Up to 128 fields** supported per struct
* Generates:
    * `member_names<T>` — array of field names
    * `member_types<T>` — tuple of field types
    * `to_tie(obj)` — tie of field references for iteration or mapping
* Works **without RTTI or macros**
* Zero runtime overhead — pure constexpr
* Compatible with **modern compilers (Clang ≥16, GCC ≥13, MSVC ≥19.36)**

---

## ⚙️ Example

```cpp
#include <ureflect/ureflect_auto.h>
#include <iostream>

struct User {
    int id;
    std::string name;
    double balance;
};

int main() {
    User u{1, "Alice", 42.5};

    ureflect::for_each_field(u, [](std::string_view name, auto &value) {
        std::cout << name << ": " << value << '\n';
    });

    ureflect::for_each_field(u, [](std::string_view, auto &v) {
        if constexpr (std::is_same_v<std::remove_reference_t<decltype(v)>, double>)
            v += 10.0;
    });

    ureflect::for_each_field(u, [](std::string_view name, auto &value) {
        std::cout << name << ": " << value << '\n';
    });
}
```

**Output:**

```
id: 1
name: Alice
balance: 42.5
```

---

## 🧩 Integration

### CMake

```cmake
find_package(ureflect REQUIRED)
target_link_libraries(your_target PRIVATE ureflect)
```

or simply:

```cpp
#include <ureflect/ureflect_auto.h>
```

since the library is **header-only**.

---

## 🏗️ Build & Install

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
sudo cmake --install build
```

---

## 🧠 Typical Use Cases

* Automatic JSON/SQL/GraphQL mapping
* PostgreSQL row-to-struct conversion
* OpenAPI schema generation
* Serialization and deserialization frameworks
* ORM/DTO meta generation

---

## 📜 License

MIT License © [Usub Development](https://github.com/Usub-development)