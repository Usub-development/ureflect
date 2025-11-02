# Performance Notes

- All computations are done at compile-time (`consteval`/`constexpr`).
- Zero allocations, zero RTTI, zero runtime scanning.
- Generated code equals manual field access.
- Compile-time overhead grows linearly with field count.