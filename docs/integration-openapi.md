# Integration: OpenAPI Schema

`ureflect` can power automatic OpenAPI schema generation:

```cpp
template<class T>
void emit_schema() {
    std::cout << "type: object\nproperties:\n";
    ureflect::for_each_field(ureflect::uref_external<T>, [](auto name, auto&){
        std::cout << "  " << name << ":\n    type: string\n";
    });
}
```