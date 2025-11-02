# Enum Reflection

`enum_name<E>()` extracts the enumerator name at compile-time.

```cpp
enum class Status { Ok, Error, Timeout };

constexpr auto s1 = ureflect::enum_name<Status::Ok>(); // "Ok"
constexpr auto s2 = ureflect::type_name<Status>();     // "Status"
```

All without macros or registration.