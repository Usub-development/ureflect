#include "ureflect/ureflect_auto.h"
#include "json_to_struct.h"
#include <iostream>
#include <string>
#include <cstdint>

struct Money
{
    double amount;
    std::string currency;
};

struct Order
{
    uint64_t id;
    std::string merchant;
    bool active;
    Money total;
};

struct User
{
    int id;
    std::string name;
    double balance;
};

template <class T>
void debug_names()
{
    constexpr auto names = ureflect::member_names<T>;
    for (auto sv : names) std::cout << '[' << sv << "]\n";
}


int main()
{
    debug_names<Order>();

    auto o = uj::from_json<Order>(R"({
        "id":"42",
        "merchant":"m1",
        "active":true,
        "total":7,
        "ignored":123
    })");

    std::cout << o.id << " " << o.merchant << " " << std::boolalpha << o.active
        << " " << o.total.amount << " " << o.total.currency << "\n";

    User u{1, "Alice", 42.5};

    ureflect::for_each_field(u, [](std::string_view name, auto& value)
    {
        std::cout << name << ": " << value << '\n';
    });

    ureflect::for_each_field(u, [](std::string_view, auto& v)
    {
        if constexpr (std::is_same_v<std::remove_reference_t<decltype(v)>, double>)
            v += 10.0;
    });

    ureflect::for_each_field(u, [](std::string_view name, auto& value)
    {
        std::cout << name << ": " << value << '\n';
    });
}
