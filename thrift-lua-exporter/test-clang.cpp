/* test file */
#include <stdio.h>
//#include <Luna.h>
#include <functional>

#ifdef CLANG_PARSER
#define LUA_EXPORT __attribute__((annotate("xlua")))
#else
#define LUA_EXPORT
#endif


struct lua_State;

namespace detail
{

struct Object {};

struct LUA_EXPORT Storage : Object {
    Storage() = default;

    LUA_EXPORT int a;
    LUA_EXPORT void print();
    LUA_EXPORT inline void print2();
    inline LUA_EXPORT void print3();
};

template <typename Ty, typename... Args>
struct Identity {
    typedef Ty type;
};

LUA_EXPORT auto do_test(int a, Storage* l) -> int {
    return 1;
}
} // namespace detail

LUA_EXPORT auto do_work(int a, lua_State* l) -> int {
    return 1;
}

LUA_EXPORT auto do_work_1(int a, detail::Storage* l = nullptr) -> int {
    return 1;
}

LUA_EXPORT int do_work2();

typedef void (*fnCallback)(void* d);
LUA_EXPORT int do_work3(fnCallback call) { return 1; }

LUA_EXPORT void CallBack(const std::function<void(int)>& call) {
}

int main(int argc, char* argv[])
{
    printf("hello world\n");
    return 0;
}


