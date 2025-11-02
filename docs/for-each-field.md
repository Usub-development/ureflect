# Field Iteration

`for_each_field(obj, fn)` iterates through each field at compile-time.

```cpp
struct User { int id; std::string name; double balance; };

ureflect::for_each_field(u, [](std::string_view name, auto& value) {
    std::cout << name << ": " << value << '\n';
});
```

You can use it to build serializers, database mappers, or UI inspectors.

Example: simple JSON output
```cpp
std::cout << "{ ";
bool first = true;
ureflect::for_each_field(u, [&](auto name, auto& val){
    if (!first) std::cout << ", ";
    std::cout << '"' << name << "\": \"" << val << '"';
    first = false;
});
std::cout << " }";
```