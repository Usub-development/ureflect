# Tie & Field Access

`to_tie(obj)` returns a compile-time tuple of field references:

```cpp
auto t = ureflect::to_tie(u);
auto std_tuple = ureflect::as_std_tuple(t);

std::apply([](auto&... f){
    ((std::cout << f << ' '), ...);
}, std_tuple);
```

This is equivalent to `std::tie(obj.field1, obj.field2, ...)` but generated automatically.