# Member Names & Types

Each reflected struct produces compile-time arrays describing its layout.

```cpp
constexpr auto names = ureflect::member_names<User>;
constexpr auto types = ureflect::member_types<User>;
```

`names` — `std::array<std::string_view, N>`
`types` — tuple of `type_name<Ti>()` results.

You can iterate or use them in meta-code generation.

Example:
```cpp
for (auto n : ureflect::member_names<User>)
    std::cout << n << '\n';
```