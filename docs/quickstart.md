# Quickstart

## Installation

```bash
git clone https://github.com/Usub-development/ureflect
cmake -B build -DCMAKE_BUILD_TYPE=Release
sudo cmake --install build
```

## Add to your project:

```cmake
find_package(ureflect REQUIRED)
target_link_libraries(your_target PRIVATE ureflect)
```

Or simply include the header:

```cpp
#include <ureflect/ureflect_auto.h>
```

## Basic usage

```cpp
struct Item { int code; float price; };

Item i{42, 3.14};
ureflect::for_each_field(i, [](auto name, auto& value){
    std::cout << name << ": " << value << '\n';
});
```