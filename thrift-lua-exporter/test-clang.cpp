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
} // namespace detail

LUA_EXPORT auto do_work(int a, lua_State* l) -> int {
    return 1;
}

LUA_EXPORT int do_work2();

LUA_EXPORT void CallBack(const std::function<void(int)>& call) {
}

int main(int argc, char* argv[])
{
    printf("hello world\n");
    return 0;
}


