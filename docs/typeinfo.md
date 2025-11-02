# Type Metadata

`type_name<T>()` returns a compile-time demangled name:

```cpp
std::cout << ureflect::type_name<std::vector<int>>() << '\n';
```

`mangled_name()` gives the raw compiler signature (useful for debugging reflection).