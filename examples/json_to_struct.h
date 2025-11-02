//
// Created by Kirill Zhukov on 01.11.2025.
//

#ifndef UREFLECT_TEST_JSON_TO_STRUCT_H
#define UREFLECT_TEST_JSON_TO_STRUCT_H

#include <string_view>
#include <string>
#include <cctype>
#include <cstdlib>
#include <cerrno>
#include <cstring>
#include <stdexcept>
#include <type_traits>
#include "../include/ureflect/ureflect_auto.h"

namespace uj {
    [[noreturn]] inline void fail(const char *msg) { throw std::runtime_error(msg); }

    [[noreturn]] inline void fail_here(const char *head, const char *p, const char *e) {
        const char *q = p;
        int n = 0;
        while (q < e && *q != '\n' && n < 48) {
            ++q;
            ++n;
        }
        std::string ctx(p, q - p);
        std::string msg = std::string(head) + " near: \"" + ctx + "\"";
        throw std::runtime_error(msg);
    }

    struct R {
        const char *p;
        const char *e;

        explicit R(std::string_view s) : p(s.data()), e(s.data() + s.size()) {
        }

        bool eof() const { return p >= e; }
        void ws() { while (p < e && std::isspace((unsigned char) *p)) ++p; }

        bool take(char c) {
            ws();
            if (p < e && *p == c) {
                ++p;
                return true;
            }
            return false;
        }

        void expect(char c) {
            ws();
            if (eof() || *p != c) {
                char exp[64];
                std::snprintf(exp, sizeof(exp), "json: expected '%c'", c);
                fail_here(exp, p, e);
            }
            ++p;
        }

        char peek() {
            ws();
            if (eof()) fail("json: unexpected eof");
            return *p;
        }

        void parse_null() {
            ws();
            if (e - p >= 4 && std::memcmp(p, "null", 4) == 0) { p += 4; } else fail_here("json: null", p, e);
        }

        bool parse_bool_token() {
            ws();
            if (e - p >= 4 && std::memcmp(p, "true", 4) == 0) {
                p += 4;
                return true;
            }
            if (e - p >= 5 && std::memcmp(p, "false", 5) == 0) {
                p += 5;
                return false;
            }
            fail_here("json: bool", p, e);
        }

        std::pair<const char *, const char *> scan_number_strict() {
            ws();
            const char *b = p;
            const char *q = p;

            if (q < e && *q == '-') ++q;

            if (q >= e || !std::isdigit((unsigned char) *q)) {
                fail_here("json: number start", p, e);
            }
            if (*q == '0') { ++q; } else { while (q < e && std::isdigit((unsigned char) *q)) ++q; }

            if (q < e && *q == '.') {
                ++q;
                if (q >= e || !std::isdigit((unsigned char) *q)) fail_here("json: fraction", p, e);
                while (q < e && std::isdigit((unsigned char) *q)) ++q;
            }

            if (q < e && (*q == 'e' || *q == 'E')) {
                ++q;
                if (q < e && (*q == '+' || *q == '-')) ++q;
                if (q >= e || !std::isdigit((unsigned char) *q)) fail_here("json: exponent", p, e);
                while (q < e && std::isdigit((unsigned char) *q)) ++q;
            }

            return {b, q};
        }

        template<class T>
        T parse_number_span(const char *b, const char *q) {
            std::string tmp(b, q - b);
            if constexpr (std::is_floating_point_v<T>) {
                errno = 0;
                char *end = nullptr;
                double d = std::strtod(tmp.c_str(), &end);
                if (errno != 0 || end == tmp.c_str()) fail_here("json: float", b, q);
                return static_cast<T>(d);
            } else if constexpr (std::is_integral_v<T> && std::is_signed_v<T>) {
                errno = 0;
                char *end = nullptr;
                long long v = std::strtoll(tmp.c_str(), &end, 10);
                if (errno != 0 || end == tmp.c_str()) fail_here("json: int", b, q);
                return static_cast<T>(v);
            } else {
                errno = 0;
                char *end = nullptr;
                unsigned long long v = std::strtoull(tmp.c_str(), &end, 10);
                if (errno != 0 || end == tmp.c_str()) fail_here("json: uint", b, q);
                return static_cast<T>(v);
            }
        }

        template<class T>
        T parse_number() {
            auto [b, q] = scan_number_strict();
            T val = parse_number_span<T>(b, q);
            p = q;
            return val;
        }

        std::string parse_string() {
            ws();
            if (eof() || *p != '"') fail_here("json: string", p, e);
            ++p;
            std::string out;
            bool closed = false;
            while (p < e) {
                char c = *p++;
                if (c == '"') {
                    closed = true;
                    break;
                }
                if (c == '\\') {
                    if (eof()) fail_here("json: bad escape", p, e);
                    char e1 = *p++;
                    switch (e1) {
                        case '"': out.push_back('"');
                            break;
                        case '\\': out.push_back('\\');
                            break;
                        case '/': out.push_back('/');
                            break;
                        case 'b': out.push_back('\b');
                            break;
                        case 'f': out.push_back('\f');
                            break;
                        case 'n': out.push_back('\n');
                            break;
                        case 'r': out.push_back('\r');
                            break;
                        case 't': out.push_back('\t');
                            break;
                        default: fail_here("json: escape", p - 1, e);
                    }
                } else {
                    out.push_back(c);
                }
            }
            if (!closed) fail_here("json: unclosed string", p, e);
            return out;
        }

        void skip_value() {
            ws();
            if (eof()) fail("json: eof");
            char c = *p;
            if (c == '"') {
                (void) parse_string();
                return;
            }
            if (c == '{') {
                skip_object();
                return;
            }
            if (c == '[') {
                skip_array();
                return;
            }
            if (c == 't' || c == 'f') {
                (void) parse_bool_token();
                return;
            }
            if (c == 'n') {
                parse_null();
                return;
            }
            if (c == '-' || std::isdigit((unsigned char) c)) {
                (void) parse_number<double>();
                return;
            }
            fail_here("json: unknown token", p, e);
        }

        void skip_array() {
            expect('[');
            ws();
            if (take(']')) return;
            while (true) {
                skip_value();
                ws();
                if (take(']')) return;
                if (take(',')) continue;
                fail_here("json: expected ',' or ']'", p, e);
            }
        }

        void skip_object() {
            expect('{');
            ws();
            if (take('}')) return;
            while (true) {
                (void) parse_string();
                expect(':');
                skip_value();
                ws();
                if (take('}')) return;
                if (take(',')) continue;
                fail_here("json: expected ',' or '}'", p, e);
            }
        }
    };

    template<class T>
    concept has_uref_count_members =
            requires { ureflect::count_members<std::remove_cvref_t<T> >; };

    template<class T>
    void parse_value(R &r, T &out);

    template<>
    inline void parse_value<bool>(R &r, bool &out) {
        char c = r.peek();
        if (c == 't' || c == 'f') {
            out = r.parse_bool_token();
            return;
        }
        if (c == '"') {
            std::string s = r.parse_string();
            if (s == "true" || s == "1") {
                out = true;
                return;
            }
            if (s == "false" || s == "0") {
                out = false;
                return;
            }
            fail("json: bool string");
        }
        if (c == '-' || std::isdigit((unsigned char) c)) {
            long long v = r.parse_number<long long>();
            out = (v != 0);
            return;
        }
        if (c == 'n') {
            r.parse_null();
            return;
        }
        fail_here("json: bool token", r.p, r.e);
    }

    template<>
    inline void parse_value<std::string>(R &r, std::string &out) {
        if (r.peek() == 'n') {
            r.parse_null();
            out.clear();
            return;
        }
        out = r.parse_string();
    }

    template<class T>
        requires(std::is_integral_v<T> && std::is_signed_v<T>)
    inline void parse_value(R &r, T &out) {
        char c = r.peek();
        if (c == 'n') {
            r.parse_null();
            return;
        }
        if (c == '"') {
            std::string s = r.parse_string();
            out = r.parse_number_span<T>(s.data(), s.data() + s.size());
            return;
        }
        if (c == 't' || c == 'f') {
            out = r.parse_bool_token() ? 1 : 0;
            return;
        }
        if (c == '-' || std::isdigit((unsigned char) c)) {
            out = r.parse_number<T>();
            return;
        }
        fail_here("json: number expected", r.p, r.e);
    }

    template<class T>
        requires(std::is_integral_v<T> && std::is_unsigned_v<T>)
    inline void parse_value(R &r, T &out) {
        char c = r.peek();
        if (c == 'n') {
            r.parse_null();
            return;
        }
        if (c == '"') {
            std::string s = r.parse_string();
            out = r.parse_number_span<T>(s.data(), s.data() + s.size());
            return;
        }
        if (c == 't' || c == 'f') {
            out = r.parse_bool_token() ? 1u : 0u;
            return;
        }
        if (c == '-' || std::isdigit((unsigned char) c)) {
            out = r.parse_number<T>();
            return;
        }
        fail_here("json: number expected", r.p, r.e);
    }

    template<class T>
        requires(std::is_floating_point_v<T>)
    inline void parse_value(R &r, T &out) {
        char c = r.peek();
        if (c == 'n') {
            r.parse_null();
            return;
        }
        if (c == '"') {
            std::string s = r.parse_string();
            out = r.parse_number_span<T>(s.data(), s.data() + s.size());
            return;
        }
        if (c == '-' || std::isdigit((unsigned char) c)) {
            out = r.parse_number<T>();
            return;
        }
        fail_here("json: number expected", r.p, r.e);
    }

    // Агрегаты: поддержка примитива -> первый член (универсальный «Money-лайк» даункаст)
    template<class T>
        requires has_uref_count_members<T>
    inline void parse_value(R &r, T &out) {
        auto tiev = ureflect::to_tie(out);
        using First = std::remove_reference_t<decltype(ureflect::get<0>(tiev))>;

        char c = r.peek();
        if (c != '{') {
            if (c == 'n') {
                r.parse_null();
                return;
            }

            // число -> если First арифметический
            if (c == '-' || std::isdigit((unsigned char) c)) {
                if constexpr (std::is_arithmetic_v<First>) {
                    First tmp{};
                    parse_value(r, tmp); // использует числовые ветки
                    ureflect::get<0>(tiev) = tmp;
                    return;
                } else {
                    r.skip_value();
                    return;
                }
            }

            // строка -> если First = std::string
            if (c == '"') {
                if constexpr (std::is_same_v<First, std::string>) {
                    ureflect::get<0>(tiev) = r.parse_string();
                    return;
                } else {
                    r.skip_value();
                    return;
                }
            }

            // bool -> если First = bool
            if (c == 't' || c == 'f') {
                if constexpr (std::is_same_v<First, bool>) {
                    bool b = r.parse_bool_token();
                    ureflect::get<0>(tiev) = b;
                    return;
                } else {
                    r.skip_value();
                    return;
                }
            }

            // иное — пропуск
            r.skip_value();
            return;
        }

        // полноценный объект
        r.expect('{');
        r.ws();
        if (r.take('}')) return;

        constexpr std::size_t N = ureflect::count_members<T>;

        while (true) {
            std::string key = r.parse_string();
            r.expect(':');

            bool matched = false;
            [&]<std::size_t... I>(std::index_sequence<I...>) {
                ((!matched && key == ureflect::member_nameof<I, T>
                      ? ((parse_value(r, ureflect::get<I>(tiev))), matched = true, void())
                      : void()), ...);
            }(std::make_index_sequence<N>{});

            if (!matched) r.skip_value();

            r.ws();
            if (r.take('}')) break;
            if (r.take(',')) continue;
            fail_here("json: expected ',' or '}'", r.p, r.e);
        }
    }

    template<class T>
    T from_json(std::string_view s) {
        R r{s};
        T v{};
        try {
            parse_value(r, v);
            r.ws();
            if (!r.eof()) fail_here("json: trailing", r.p, r.e);
        } catch (const char *msg) {
            fail(msg);
        }
        return v;
    }
} // namespace uj

#endif // UREFLECT_TEST_JSON_TO_STRUCT_H
