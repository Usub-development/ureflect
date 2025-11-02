# ureflect

**Header-only C++23 compile-time reflection library**  
Zero-runtime, macro-free, dependency-free.

---

## Key features

- Compile-time introspection for structs and enums
- Generates:
    - `member_names<T>` — field names
    - `member_types<T>` — field types
    - `to_tie(obj)` — tuple of references
- No RTTI, no macros, no allocations
- Up to **128 fields** supported
- Fully constexpr and header-only
- Compatible with Clang ≥16, GCC ≥13, MSVC ≥19.36

---

```cpp
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
## Output:
```
id: 1
name: Alice
balance: 42.5
```