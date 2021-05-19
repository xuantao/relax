#include "def.h"
#include <vector>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <typeinfo>
#include <functional>

#ifdef  __GNUC__
#define KG_FUNCTION __PRETTY_FUNCTION__
#else
#define KG_FUNCTION __FUNCTION__
#endif

#define EXPAND(...) __VA_ARGS__
#define _KG_STRING2(x) #x
#define _KG_STRING(x) _KG_STRING2(x)

#define KGFUNC_SOURCE   "in (" EXPAND(KG_FUNCTION) ":" _KG_STRING(__LINE__) ")"
#define KGFILE_SOURCE   "in (" __FILE__ ":" _KG_STRING(__LINE__) ")"
//#define KGFILE_SOURCE_1 KGFUNC_SOURCE


void DoPrint(const char* s) { printf("%s\n", s); }

template <typename... Args>
void Do(Args&&... args)
{
    using its = int[];
    (void)its {0, (DoPrint(args), 0)...};
    printf("do work\n");
}

#define __MAX_PATH 256

static void ParseEnum(const char* str, std::vector<int>& v)
{
    int i = 0;// atoi(str);
    v.push_back(i);
}

static void sParseDatas(const char* first, const char* last, std::vector<int>& vals)
{
    char szTmp[__MAX_PATH];
    while (first <= last)
    {
        const auto* next = strchr(first, ',');
        if (next == nullptr || next >= last)
            next = last;

        assert(__MAX_PATH > next - first);
        size_t len = next - first;
        memcpy(szTmp, first, len);
        szTmp[len] = 0;
        ParseEnum(szTmp, vals);
        first = next + 1;
    }
}

static void sParseMultiDatas(const char* first, const char* last, std::vector<std::vector<int>>& mvals)
{
    while (first < last)
    {
        const auto* next = strchr(first, ';');
        if (next == nullptr || next >= last)
            next = last;

        std::vector<int> v;
        sParseDatas(first, next, v);
        mvals.push_back(std::move(v));

        first = next + 1;
    }
}

void Test(const char* s)
{
    std::vector<std::vector<int>> vals;
    const char* last = s + strlen(s);

    sParseMultiDatas(s, last, vals);
    for (const auto& v : vals)
    {
        printf("{");
        for (auto i : v)
            printf("%d,", i);
        printf("}\n");
    }
}

struct KGAttributeArg
{
    union {
        bool bBoolean;
        float fFloat;
        int32_t nInteger;
        uint32_t uInteger = 0;
    };

    explicit inline operator bool() const { return bBoolean; }
    explicit inline operator float() const { return fFloat; }
    explicit inline operator int32_t() const { return nInteger; }
    explicit inline operator uint32_t() const { return uInteger; }
};


void func(int a, uint32_t d, float f, bool b)
{
    assert(a == 1);
    assert(d == 2);
    assert(f == 3.0f);
    assert(b == false);
}

struct Storage
{
    uint8_t _s[8];
};

namespace TestNs {
    struct TestDecl {
        TestDecl();
    };
}

TestNs::TestDecl::TestDecl() {}


template <typename Ty = int>
struct IntegerOrEnum {
    IntegerOrEnum() = default;
    IntegerOrEnum(Ty val) : value(val) {}
    template <typename E, typename std::enable_if<std::is_enum<E>::value, int>::type = 0>
    IntegerOrEnum(E val) : value((typename std::underlying_type<E>::type)val){}

    inline operator Ty() const { return value; }

    Ty value;
};

void TestInteger(IntegerOrEnum<> key)
{
    int v = key;
    printf("%d, %d\n", (int)key, v);
}

enum E_A
{
    aA,
    aB,
    aC,
};

enum class E_B
{
    bA,
    bB,
    bC,
};

enum SCENE_OBJ_RELATION_TYPE
{
    sortInvalid = 0,

    sortNone = 1,
    sortSelf = 2,
    sortAlly = 4,
    sortFoe = 8,
    sortEnemy = 16,
    sortNeutrality = 32,
    sortParty = 64,

    sortAll = sortNone | sortAlly | sortFoe | sortEnemy | sortSelf | sortNeutrality | sortParty,

    sortTotal,
};

constexpr const char* str =
"123" /*sss*/
"456" // what
/*111*/ "789";

const char* s2 = "abd" R"(
ab
bc)" R"(
cd
)";

enum class TestEnumDef : int32_t
{

};

#define _DEBUG__
#define _SERVER

#if define _DEBUG__ \
    && define _SERVER

#endif

//std::function<void()> func;

#define test_ml /*fdagadg
dgjladgja
*/ "sssafdaf"   \
"dfaga"

struct BaseObj
{
    using MyType = BaseObj;
    BaseObj() { printf("BaseObj()\n"); }
    BaseObj(int) {}
    ~BaseObj() { printf("~BaseObj()\n"); }
    void Print(int);// {}

    int get_int() { return 0; }

    int a = 0;
};


void ::BaseObj::Print(int) { printf("BaseObj::Print called\n"); }


struct LuaFunc;// : BaseObj, ;

struct LuaFunc : public BaseObj
{
    template <typename Ry, typename... Args>
    operator std::function<Ry(Args...)> () const
    {
        return std::function<Ry(Args...)>([this](Args... args) {
            printf("111111111\n");
            return Ry();
        });
    }

    int m_a = 0, m_b = 1;

    void Print () {}

    struct InnerType {
        int v = 0;
    };

    mutable struct ty final : BaseObj {
        int a = 1;
    } sssa1;

    void operator + (int) {}

    inline explicit operator const int BaseObj::* const () const { return &BaseObj::a; }
    inline explicit operator const BaseObj* const () const { return this; }

    const int BaseObj::* get_member_ptr() const { return &BaseObj::a; }

    // p1 == p2 == p3 == p4
    int BaseObj::* (BaseObj::* p_1)();// = nullptr;
    int BaseObj::* ((BaseObj::* p_2))();// = nullptr;
    int BaseObj::* ((BaseObj::* p_3)());// = nullptr;
    int(BaseObj::* ((BaseObj::* p_4)()));

    //int m_c, m_d = 1, 2; error
};

//LuaFunc::operator const BaseObj *() { return this; }

template <int l, int r>
struct TCmp : std::integral_constant<bool, l < r>
{};
template <int l, int r>
struct TCmp2 : std::integral_constant<bool, l >= r>
{};

constexpr int c_a_1 = 1;
constexpr int c_a_2 = 1;
//c_a_1 > c_a_2;


template <typename T>
struct OutterType {
    template <typename U>
    struct InnerType {

    };
};

extern int global_a;
int global_a = 1;

void test_default(BaseObj obj = {})
{

}

void test_default2(const int ar[])
{

}

namespace test_namespace
{
    const struct test_namespace {
        test_namespace(int);
        int a;
    };

    struct test_construct
    {
        test_construct() {}
        test_construct(int);

        int a{1};
        int b = 3;
        int c[10] ={0};
        int d[10]{0};

        test_namespace tn{1};

        //BaseObj obj(1);

        //int f[];
    };
}

//test_namespace::test_construct::test_construct(int)
//{
//
//}

test_namespace::test_construct::test_construct(int)
{

}

test_namespace::test_namespace::test_namespace(int) {}

test_namespace::test_construct global_tc_1();
auto global_tc_2 = test_namespace::test_construct(1);
test_namespace::test_construct global_tc_3{3};

void test_const_void_ptr(const void*) { }

void test_const_void_1(const void* const) { }
// error
//void test_const_void_2(const void) { }
//void test_const_void_3(void const) { }

//LuaFunc LuaFunc;

void test_member_ptr(int (BaseObj::*)){}

void test_func_ptr_1(int()) {}
void test_func_ptr_2(int(*)()) {}
void test_callback(int (call)()) { call(); }

struct test_anymouse_struct_member {
    struct {
         int a, *pb;   //warning C4228: nonstandard extension used: qualifiers after comma in declarator list are ignored
        int b;
    } const;

    int a1 = 0, f2();
    BaseObj (b1), b2();

    BaseObj(*pobj);// = nullptr;
};

BaseObj g_base_obj_1();     // declare function
//BaseObj g_base_obj_2(1);    // define variate

constexpr static int kIntValue_1 = 101;
constexpr int static kIntValue_2 = 102;
int constexpr static kIntValue_3 = 103;
int static constexpr kIntValue_4 = 104;
static int constexpr * kIntValue_7 = nullptr;
//int static kIntValue_5 constexpr = 105;    //error
//int constexpr kIntValue_6 static = 106;    //error
//static int* constexpr kIntValue_7 = nullptr;  //error


BaseObj getBaseObj() {
    BaseObj b;
    return std::move(b);
}

int test_get_int() { return 1; }
void teset_set_int(int) {}

//void test_param(int... args) {}

void test_default_value(int s, int e/* = s [not allow]*/) {}

void test_param_func_ptr_1(int(void)) {}
void test_param_func_ptr_2(int(*)(void)/* = nullptr*/) {}
void test_param_func_ptr_3(int(int)) {}
void test_param_func_ptr_4(int(a)) { printf("test_param_func_ptr_4:%d\n", a); }
void test_param_func_ptr_5(int(* p_int)) {}
void test_param_func_ptr_6(int(r_int)) {}
void test_param_func_ptr_7(int(&r_int)()) {
    printf("test_param_func_ptr_7: %s\n", typeid(r_int).name());
}
void test_param_func_ptr_8(int(&&r_int)()) {
    printf("test_param_func_ptr_8: %s\n", typeid(r_int).name());
}

void test_param_func_ptr_9(BaseObj&&r_int) {
    printf("test_param_func_ptr_9: %s\n", typeid(r_int).name());
}


struct abs_struct {
    struct obj_var {};
    struct obj { int a = 1; };
    static obj obj;

    int b = 1;
    int c = 2;
    int(obj_var)/* = 1*/;   // obj_var Ϊint���ͱ���
};

struct abs_struct::obj abs_struct::obj;

// ����Ϊ����ָ��
// �ڲ����б������Ƚ���Ϊ��������
void test_param_func_ptr_10(int(abs_struct)) {
    //printf("1111 %s\", )
}

// abs_struct Ϊint���ͱ���
int(abs_struct);

//typename int f_get_int_4();

constexpr int g_int_value_1(sizeof(abs_struct) + 1);
static_assert(g_int_value_1 == 5, "abs_struct is a int value");

// error, can not return function type
//int() get_func_ptr() { return nullptr; }
//auto get_func_ptr_1() -> int(*) { return nullptr; }
auto get_func_ptr_2() -> auto(*)()->int { return nullptr; }

struct test_bitfield_1 {
    uint32_t kind : 4;
    uint32_t reverse : 20, v : 8;

    using MyType = test_bitfield_1;
    //MyType() {}
};

template <typename T>
struct test_template_construct {
    using MyType = test_template_construct<T>;
    //using MyType = test_template_construct;

    //MyType() {}

    int a = 0;
};

int main(int argc, char* argv[])
{
    // struct abs_struct abs_struct;
    // ������Ч����ͬ
    struct abs_struct (abs_struct);   // ����ͬ������������Ҫ��struct
    // must need struct
    struct abs_struct::obj abs_struct_obj_1;
    // error
    //typename abs_struct::obj abs_struct_obj_2;

    test_template_construct<int> t;
    test_param_func_ptr_10(nullptr);
    //test_param_func_ptr_10(1);
    //printf("%d\n", t.a);

    test_callback(test_get_int);
    test_func_ptr_1(test_get_int);
    test_func_ptr_2(test_get_int);
    test_param_func_ptr_4(41);
    test_param_func_ptr_7(test_get_int);
    test_param_func_ptr_8(test_get_int);
    test_param_func_ptr_9(BaseObj());


    int(& p_r_int_1)() = test_get_int;
    int(&& p_r_int_2)() = test_get_int;
    printf("1. %s\n", typeid(p_r_int_1).name());
    printf("2. %s\n", typeid(p_r_int_2).name());

    //auto d = 0.1f;
    //auto dd = 0.1;
    //auto i2 = 0b110ul;
    //float f5 = .3e1f;
    //float f6 = .3e1l;
    //printf("s1:%s\n", str);
    //printf("s2:%s\n", s2);
    //printf("%s\n", test_ml);
    //LuaFunc LuaFunc1;

    ////int test_log(const char* l) { return printf("%s\n", l); }
    ////test_log("1");

    //test_namespace::test_construct tnc(2);

    //typedef void (*funcType)(const int*);
    //funcType fptr = &test_default2;

    //bool b1 = TCmp<1, 2>::value;
    //bool b2 = TCmp<2, 1>::value;

    //bool b3 = TCmp2<1, 2>::value;
    //bool b4 = TCmp2<2, 1>::value;

    //struct LuaFunc const * const f10 = nullptr;
    ////struct const LuaFunc *pf11 = nullptr;
    //std::function<int()> f1 = LuaFunc1;
    //f1();
    //printf("%p, %p", f10, &f11);

    //decltype(f1)::result_type f_ret = 1;
    //const struct LuaFunc::InnerType innerVal;

    //struct decltype(f)::InnerType in2;

    //printf("%d\n", innerVal.v);

    //BaseObj&& rfBaseObj = getBaseObj();

    //OutterType<int>::struct template InnerType<char> v;
    const int *p1 = nullptr, *p2 = nullptr;

    //auto mem_func = &LuaFunc::Print;
    //printf("%s, %d\n", typeid(rfBaseObj).name(), rfBaseObj.a);

    test_anymouse_struct_member test_anymouse_struct_member_;
    test_anymouse_struct_member_.a = 0;
    test_anymouse_struct_member_.b = 1;

    int a = 1, *p = nullptr, f(), (*pf)(double) = nullptr;
    //f = []()->int { return 1; };

    //int(abs_struct) = 0;

    typedef int (f_get_int)();
    typedef int f_get_int_2();
    typedef int BaseObj::* mem_ptr_get_int();
    typedef int *f_get_int_3 ();

    short s_a = (short)a;
    short s_b = short(a);

    //f = test_get_int;
    f();

    //teset_set_int((*p));
    //teset_set_int((1));

    //BaseObj base_obj_1{1};

    auto p_p_p = &LuaFunc::operator+;

    printf("0. %s\n", typeid(p_p_p).name());

    //f_get_int fg;
    printf("1. %s\n", typeid(f).name());
    printf("2. %s\n", typeid(f_get_int).name());
    //(void)(fg);

    using func_ptr_1 = int (*)();
    func_ptr_1 test_func_ptr_1 = &test_get_int;

    using mem_ptr_1 = int BaseObj::*(); // function, ret:(int BaseObj::*) param:()
    mem_ptr_1* t_mem_ptr =[]()->int BaseObj::* {
        return &BaseObj::a;
    };

    using mem_ptr_2 = int (BaseObj::*)();   // member function ptr
    mem_ptr_2 t_mem_ptr_2 = &BaseObj::get_int;

    // int(*p_int_3 = nullptr); // not allow assign in bracket

    //int(*pf__)(int *pa = nullptr);        // not allow default value
    int(*const (*decl_ptr_var)) = nullptr;     // define variate
    decl_ptr_var = &p;
    //int(*var_1, var_2);   //error
    printf("3. %s\n", typeid(decl_ptr_var).name());
    int(/*const not allow here*/ decl_const_var) = 100;     // define variate
    printf("4. %s\n", typeid(decl_const_var).name());

    //int(*(*(*foo_0)(double))) = nullptr;

    int(*(*(*foo_1)(double)noexcept)) = nullptr;
    printf("5. %s\n", typeid(foo_1).name());

    //int(*(foo_2)(double));
    int(*((foo_2))(double));
    printf("6. %s\n", typeid(foo_2).name());

    int(*(*foo_3)(double))[3] = nullptr;
    printf("7. %s\n", typeid(foo_3).name());

    int(*(*(*const foo_4)(void))) = nullptr;
    printf("7.1.{int** (*)(void)} = %s\n", typeid(foo_4).name());

    int(*(*(*(foo_5))(void))) = nullptr;
    printf("7.2.%s\n", typeid(foo_5).name());

    int* ((*(foo_6))(void)) = nullptr;
    printf("7.3.%s\n", typeid(foo_6).name());

    int(*foo_7());// = nullptr;
    printf("7.4.%s\n", typeid(foo_7).name());
    //foo_4 = &test_get_int;

    int(foo_8());// = nullptr;
    printf("7.5.%s\n", typeid(foo_8).name());

    auto (*foo_9)() -> int;         // �������
    auto (*foo_10)() -> int(*)();   // �������
    auto foo_11() -> int(*)();      // ��������
    // auto foo_12() -> int();      // error, can not return function type

    //int ((*foo_7)(void))(void) = nullptr; //error, can not declare a function not return a function

    int *p_int = nullptr, BaseObj::*p_md_int = nullptr, (BaseObj::* p_mf_int)() = nullptr;
    printf("8. %s\n", typeid(p_int).name());
    printf("9. %s\n", typeid(p_md_int).name());
    printf("10.%s\n", typeid(p_mf_int).name());

    int (&p_int_2) = a;

    int(*BaseObj::*(*p_get_mem_ptr_1)) = nullptr;
    int(*BaseObj::*(*p_get_mem_ptr_2)()) = nullptr;
    printf("11.%s\n", typeid(p_get_mem_ptr_1).name());
    printf("12.%s\n", typeid(p_get_mem_ptr_2).name());

    //(int ((BaseObj::*)())) (*p_get_mem_ptr_3)();

    //int(void) (*int_value_1)() = nullptr;

    //int(*(BaseObj::*)(p_get_mem_ptr_3)()) = nullptr;

    int BaseObj::* (*p_get_mem_ptr_3)() = nullptr;
    printf("13.%s\n", typeid(p_get_mem_ptr_3).name());  // ret{int BaseObj::*}

    int (BaseObj::* (*p_get_mem_ptr_4)()) = nullptr;
    printf("14.%s\n", typeid(p_get_mem_ptr_4).name());  // == 13

    int (BaseObj::* (*p_get_mem_ptr_5))() = nullptr;
    printf("15.%s\n", typeid(p_get_mem_ptr_5).name());  // ret{int}

    printf("16.%s\n", typeid(get_func_ptr_1()).name());
    printf("17.%s\n", typeid(get_func_ptr_2()).name());

    int const c_a_1 = 0, *c_p_a_1 = &c_a_1;
    //c_a_1 = 1;        error, c_a_1 is const
    //*c_p_a_1 = 2;     error, *c_p_a_1 is const
    return 0;
}

int f() { printf("1111111111\n"); return 0; }

//int* foo_7(void) { return nullptr; }
