# Core Concepts

`ureflect` performs constexpr-level parsing of compiler-generated function signatures (`__PRETTY_FUNCTION__` / `__FUNCSIG__`).

It detects field count and types via aggregate construction probing and builds name tables using token slicing inside mangled function names.

## Member Enumeration

```cpp
constexpr auto names = ureflect::member_names<MyStruct>;
constexpr auto types = ureflect::member_types<MyStruct>;
```

## Field Access
```cpp
auto t = ureflect::to_tie(obj);
auto std_tuple = ureflect::as_std_tuple(t);
std::apply([](auto&... fields){ /* ... */ }, std_tuple);
```
## Field Iteration
```cpp
ureflect::for_each_field(obj, [](std::string_view name, auto& ref) {
    std::cout << name << " = " << ref << '\n';
});
```