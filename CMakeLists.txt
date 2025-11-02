cmake_minimum_required(VERSION 3.22)
cmake_policy(SET CMP0144 NEW)
project(ureflect VERSION 0.1.0 LANGUAGES CXX)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT DEFINED DEV_STAGE)
    set(DEV_STAGE 0)
endif()

message(STATUS "DEV_STAGE is set to ${DEV_STAGE}")

file(GLOB_RECURSE UREFLECT_HEADERS CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/include/ureflect/*.h
        ${CMAKE_CURRENT_SOURCE_DIR}/include/ureflect/*.hpp
)
file(GLOB_RECURSE UREFLECT_SOURCES CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ureflect/*.cpp
)

add_library(ureflect ${UREFLECT_SOURCES} ${UREFLECT_HEADERS}
        src/ureflect/ureflect_auto.cpp)
add_library(usub::ureflect ALIAS ureflect)

target_compile_definitions(ureflect PUBLIC DEV_STAGE=${DEV_STAGE})

target_include_directories(ureflect
        PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/ureflect>
)

set_target_properties(ureflect PROPERTIES
        EXPORT_NAME ureflect
        VERSION ${PROJECT_VERSION}
        SOVERSION ${PROJECT_VERSION_MAJOR}
)

option(UREFLECT_BUILD_EXAMPLES "Build UREFLECT example executables" ON)
if (UREFLECT_BUILD_EXAMPLES)
    add_executable(ureflect_example examples/main.cpp examples/json_to_struct.h)
    target_link_libraries(ureflect_example PRIVATE ureflect)
    target_compile_definitions(ureflect_example PRIVATE DEV_STAGE=${DEV_STAGE})
endif()

install(TARGETS ureflect
        EXPORT ureflectTargets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/ureflect
)

install(DIRECTORY include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/ureflect
        FILES_MATCHING
        PATTERN "*.h"
        PATTERN "*.hpp"
)

configure_package_config_file(
        cmake/ureflectConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/ureflectConfig.cmake
        INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ureflect
        NO_CHECK_REQUIRED_COMPONENTS_MACRO
)

write_basic_package_version_file(
        ${CMAKE_CURRENT_BINARY_DIR}/ureflectConfigVersion.cmake
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion
)

install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/ureflectConfig.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/ureflectConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ureflect
)

install(EXPORT ureflectTargets
        NAMESPACE ureflect::
        FILE ureflectTargets.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ureflect
)