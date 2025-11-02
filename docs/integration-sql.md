# Integration: SQL Mapping

Combine with your database layer (`upq`, `libpqxx`, etc.):

```cpp
template<class T>
void bind_fields(PGconn* c, const T& obj) {
    ureflect::for_each_field(const_cast<T&>(obj), [&](auto name, auto& value){
        PQbindParameter(c, std::string(name).c_str(), value);
    });
}
```
This allows writing table-struct bindings with zero boilerplate.