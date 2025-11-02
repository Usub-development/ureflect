# Integration: JSON

Use `for_each_field` to implement constexpr JSON serialization:

```cpp
template<class T>
std::string to_json(const T& obj) {
    std::string out = "{";
    bool first = true;
    ureflect::for_each_field(const_cast<T&>(obj), [&](auto name, auto& value){
        if (!first) out += ",";
        out += "\"" + std::string(name) + "\":\"" + std::to_string(value) + "\"";
        first = false;
    });
    out += "}";
    return out;
}
```