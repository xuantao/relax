/* test file */
#include <stdio.h>
//#include <Luna.h>

#ifdef CLANG_PARSER
#define LUA_EXPORT __attribute__((annotate("xlua")))
#else
#define LUA_EXPORT
#endif


struct lua_State;

struct LUA_EXPORT Storage {
    LUA_EXPORT int a;
    LUA_EXPORT void print();
};

template <typename Ty, typename... Args>
struct Identity {
    typedef Ty type;
};

LUA_EXPORT auto do_work(int a, lua_State* l) -> int {
    return 1;
}

int main(int argc, char* argv[])
{
    printf("hello world\n");
    return 0;
}


