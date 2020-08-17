#include <clang-c/Index.h>
#include <lua.hpp>

#define LCM_INDEX  "ClangIndex"
#define LCM_TU     "ClangTU"
#define LCM_CURSOR "ClangCursor"
#define LCM_TYPE   "ClangType"
#define LCM_FILE   "ClangFile"

static CXIndex * newIndex(lua_State *L) {
    CXIndex *idx = (CXIndex*) lua_newuserdata(L, sizeof(CXIndex));
    luaL_getmetatable(L, LCM_INDEX);
    lua_setmetatable(L, -2);
    return idx;
}

static CXIndex toIndex(lua_State *L, int n) {
    CXIndex * idx = (CXIndex*) luaL_checkudata(L, n, LCM_INDEX);
    return *idx;
}

static CXTranslationUnit * newTU(lua_State *L) {
    CXTranslationUnit *tu = (CXTranslationUnit*) lua_newuserdata(L, sizeof(CXTranslationUnit));
    luaL_getmetatable(L, LCM_TU);
    lua_setmetatable(L, -2);
    return tu;
}

static CXTranslationUnit toTU(lua_State *L, int n) {
    CXTranslationUnit * tu = (CXTranslationUnit*) luaL_checkudata(L, n, LCM_TU);
    return *tu;
}

static CXCursor * newCursor(lua_State *L) {
    CXCursor *c = (CXCursor*) lua_newuserdata(L, sizeof(CXCursor));
    luaL_getmetatable(L, LCM_CURSOR);
    lua_setmetatable(L, -2);
    return c;
}

static CXCursor toCursor(lua_State *L, int n) {
    CXCursor * c = (CXCursor*) luaL_checkudata(L, n, LCM_CURSOR);
    return *c;
}

static CXType * newType(lua_State *L) {
    CXType *t = (CXType*) lua_newuserdata(L, sizeof(CXType));
    luaL_getmetatable(L, LCM_TYPE);
    lua_setmetatable(L, -2);
    return t;
}

static CXType toType(lua_State *L, int n) {
    CXType *t = (CXType*) luaL_checkudata(L, n, LCM_TYPE);
    return *t;
}

static void pushCXString(lua_State *L, CXString s) {
    if (s.data == NULL)
        lua_pushnil(L);
    else {
        lua_pushstring(L, clang_getCString(s));
        clang_disposeString(s);
    }
}

static void pushDiagnostic(lua_State *L, CXDiagnostic diag) {
    lua_createtable(L, 0, 0);

    pushCXString(L, clang_getDiagnosticCategoryText(diag));
    lua_setfield(L, -2, "category");

    pushCXString(L, clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions()));
    lua_setfield(L, -2, "text");

    clang_disposeDiagnostic(diag);
}

/****** CLANG ******/

static int l_createIndex(lua_State *L) {
    int excludePch = lua_toboolean(L, 1);
    int diagnostics = lua_toboolean(L, 2);
    CXIndex *idx = newIndex(L);
    *idx = clang_createIndex(excludePch, diagnostics);
    return 1;
}

static luaL_Reg clang_functions[] = {
    {"createIndex", l_createIndex},
    {NULL, NULL}
};

/****** INDEX ******/

static int l_loadTU(lua_State *L) {
    CXIndex idx = toIndex(L, 1);
    const char *astFile = luaL_checkstring(L, 2);

    CXTranslationUnit *tu = newTU(L);
    *tu = clang_createTranslationUnit(idx, astFile);
    if (*tu == NULL) {
        lua_pushnil(L);
        lua_pushliteral(L, "Failed to open file: ");
        lua_pushvalue(L, 2);
        lua_concat(L, 2);
        return 2;
    }

    return 1;
}

static int l_parseTU(lua_State *L) {
    CXIndex idx = toIndex(L, 1);
    int tabIndex;
    const char *sourceFile;
    unsigned options = CXTranslationUnit_SkipFunctionBodies;

    if (lua_type(L, 2) == LUA_TTABLE) {
        sourceFile = NULL;
        tabIndex = 2;
    } else {
        sourceFile = luaL_checkstring(L, 2);
        luaL_checktype(L, 3, LUA_TTABLE);
        tabIndex = 3;
    }

    if (lua_type(L, tabIndex + 1) == LUA_TNUMBER)
        options = lua_tointeger(L, tabIndex + 1);

    int nArgs = lua_rawlen(L, tabIndex);
    lua_checkstack(L, nArgs);
    char const ** args = new char const *[nArgs];
    for (int i=0; i<nArgs; i++) {
        lua_rawgeti(L, tabIndex, i+1);
        args[i] = lua_tostring(L, -1);
    }

    CXTranslationUnit *tu = newTU(L);
    *tu = clang_parseTranslationUnit(idx, sourceFile, args, nArgs, NULL, 0, CXTranslationUnit_SkipFunctionBodies);
    delete [] args;

    if (*tu == NULL) {
        lua_pushnil(L);
        lua_pushliteral(L, "Failed to parse");
        return 2;
    }

    return 1;
}

static int l_indexGc(lua_State *L) {
    CXIndex idx = toIndex(L, 1);
    clang_disposeIndex(idx);
    return 0;
}

static luaL_Reg index_functions[] = {
    {"load", l_loadTU},
    {"parse", l_parseTU},
    {"__gc", l_indexGc},
    {NULL, NULL}
};

/****** TRANSLATION UNIT ******/

static int l_tuGc(lua_State *L) {
    CXTranslationUnit tu = toTU(L, 1);
    clang_disposeTranslationUnit(tu);
    return 0;
}

static int l_cursor(lua_State *L) {
    CXTranslationUnit tu = toTU(L, 1);
    CXCursor *cur = newCursor(L);
    *cur = clang_getTranslationUnitCursor(tu);
    if (clang_Cursor_isNull(*cur)) {
        lua_pushnil(L);
    }
    return 1;
}

static int l_file(lua_State *L) {
    CXTranslationUnit tu = toTU(L, 1);
    const char * file = luaL_checkstring(L, 2);
    CXFile f = clang_getFile(tu, file);
    pushCXString(L, clang_getFileName(f));
    lua_pushnumber(L, clang_getFileTime(f));
    return 2;
}

static int l_diagnostics(lua_State *L) {
    CXTranslationUnit tu = toTU(L, 1);
    int nDiag = clang_getNumDiagnostics(tu);
    lua_createtable(L, nDiag, 0);
    for (int i=0; i<nDiag; i++) {
        pushDiagnostic(L, clang_getDiagnostic(tu, i));
        lua_rawseti(L, -2, i+1);
    }
    return 1;
}

static int l_codeComplete(lua_State *L) {
    CXTranslationUnit tu = toTU(L, 1);
    const char *file = luaL_checkstring(L, 2);
    int line = (int)luaL_checkinteger(L, 3);
    int col = (int)luaL_checkinteger(L, 4);

    CXCodeCompleteResults* results = clang_codeCompleteAt(
        tu, file, line, col, NULL, 0, clang_defaultCodeCompleteOptions());

    unsigned int nResults = results ? results->NumResults : 0;
    lua_createtable(L, nResults, 0);
    for (int i=0; results && i<nResults; i++) {
        CXCompletionResult res = results->Results[i];
        CXCompletionString com = res.CompletionString;

        lua_createtable(L, 0, 2);

        lua_pushinteger(L, clang_getCompletionPriority(com));
        lua_setfield(L, -2, "priority");

        int nChunks = clang_getNumCompletionChunks(com);
        if (nChunks > 0) {
            lua_createtable(L, nChunks, 0);
            for (int j=0; j<nChunks; j++) {
                lua_createtable(L, 0, 2);
                pushCXString(L, clang_getCompletionChunkText(com, j));
                lua_setfield(L, -2, "text");
                lua_pushinteger(L, clang_getCompletionChunkKind(com, j));
                lua_setfield(L, -2, "kind");
                lua_rawseti(L, -2, j+1);
            }
            lua_setfield(L, -2, "chunks");
        }

        int nAnnot = clang_getCompletionNumAnnotations(com);
        if (nAnnot > 0) {
            lua_createtable(L, nAnnot, 0);
            for (int j=0; j<nAnnot; j++) {
                pushCXString(L, clang_getCompletionAnnotation(com, j));
                lua_rawseti(L, -2, j+1);
            }
            lua_setfield(L, -2, "annotations");
        }

        lua_rawseti(L, -2, i+1);
    }

    unsigned int nDiagnostics = results ? clang_codeCompleteGetNumDiagnostics(results) : 0;
    lua_createtable(L, nDiagnostics, 0);
    for (int i=0; results && i<nDiagnostics; i++) {
        CXDiagnostic diag = clang_codeCompleteGetDiagnostic(results, i);
        pushDiagnostic(L, diag);
        lua_rawseti(L, -2, i+1);
    }

    if (results)
        clang_disposeCodeCompleteResults(results);

    return 2;
}

static luaL_Reg tu_functions[] = {
    {"cursor", l_cursor},
    {"file", l_file},
    {"diagnostics", l_diagnostics},
    {"codeCompleteAt", l_codeComplete},
    {"__gc", l_tuGc},
    {NULL, NULL}
};

/****** CURSOR ******/

enum CXChildVisitResult LuaTableVisitor(CXCursor cursor,
                                   CXCursor parent,
                                   CXClientData client_data)
{
    lua_State *L = (lua_State*) client_data;
    CXCursor *c = newCursor(L);
    *c = cursor;
    int tablen = lua_rawlen(L, -2);
    lua_rawseti(L, -2, tablen+1);
    return CXChildVisit_Continue;
}

static int l_children(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    lua_createtable(L,0,0);
    clang_visitChildren(cur, LuaTableVisitor, L);
    return 1;
}

static int l_kind(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    lua_pushinteger(L, clang_getCursorKind(cur));
    return 1;
}

static int l_spelling(lua_State* L) {
    CXCursor cur = toCursor(L, 1);
    CXString str = clang_getCursorSpelling(cur);
    lua_pushstring(L, clang_getCString(str));
    clang_disposeString(str);
    return 1;
}

static int l_name(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXString name = clang_getCursorSpelling(cur);
    lua_pushstring(L, clang_getCString(name));
    clang_disposeString(name);
    return 1;
}

static int l_displayName(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXString name = clang_getCursorDisplayName(cur);
    lua_pushstring(L, clang_getCString(name));
    clang_disposeString(name);
    return 1;
}

static int l_parent(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXCursor *parent = newCursor(L);
    *parent = clang_getCursorSemanticParent(cur);
    if (clang_Cursor_isNull(*parent))
        lua_pushnil(L);
    return 1;
}

static int l_arguments(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    int nArgs = clang_Cursor_getNumArguments(cur);
    if (nArgs == -1)
        return 0;

    lua_createtable(L, nArgs, 0);
    for (int i=0; i<nArgs; i++) {
        CXCursor *arg = newCursor(L);
        *arg = clang_Cursor_getArgument(cur, i);
        lua_rawseti(L, -2, i+1);
    }
    return 1;
}

static int l_specializedTemplate(lua_State* L) {
    CXCursor cur = toCursor(L, 1);
    CXCursor tmp = clang_getSpecializedCursorTemplate(cur);
    if (clang_Cursor_isNull(tmp))
        lua_pushnil(L);
    else
        *newCursor(L) = tmp;
    return 1;
}

static int l_templateArguments(lua_State* L) {
    CXCursor cur = toCursor(L, 1);
    int nArgs = clang_Cursor_getNumTemplateArguments(cur);
    if (nArgs == -1)
        return 0;

    lua_createtable(L, nArgs, 0);
    for (int i=0; i<nArgs; i++) {
        CXType type;
        CXTemplateArgumentKind kind = clang_Cursor_getTemplateArgumentKind(cur, i);
        lua_createtable(L, 2, 0);
        lua_pushinteger(L, kind);
        lua_setfield(L, -2, "kind");

        switch (kind)
        {
        case CXTemplateArgumentKind_Integral:
            lua_pushinteger(L, clang_Cursor_getTemplateArgumentValue(cur, i));
            break;
        case CXTemplateArgumentKind_Null:
        case CXTemplateArgumentKind_Invalid:
            lua_pushnil(L);
            break;
        default:
            type = clang_Cursor_getTemplateArgumentType(cur, i);
            if (type.kind != CXType_Invalid)
                *newType(L) = type;
            else
                lua_pushnil(L);
            break;
        }
        lua_setfield(L, -2, "value");
        lua_rawseti(L, -2, i+1);
    }
    return 1;
}

static int l_type(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXType *type = newType(L);
    *type = clang_getCursorType(cur);
    if (type->kind == CXType_Invalid)
        lua_pushnil(L);
    return 1;
}

static int l_access(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    lua_pushinteger(L, clang_getCXXAccessSpecifier(cur));
    return 1;
}

static int l_location(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXSourceRange range = clang_getCursorExtent(cur);

    CXFile file;
    unsigned int line, col;

    CXSourceLocation loc = clang_getRangeStart(range);
    clang_getSpellingLocation(loc, &file, &line, &col, NULL);
    CXString fileName = clang_getFileName(file);
    lua_pushstring(L, clang_getCString(fileName));
    clang_disposeString(fileName);
    lua_pushinteger(L, line);
    lua_pushinteger(L, col);

    loc = clang_getRangeEnd(range);
    clang_getSpellingLocation(loc, &file, &line, &col, NULL);
    lua_pushinteger(L, line);
    lua_pushinteger(L, col);
    return 5;
}

static int l_usr(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXString str = clang_getCursorUSR(cur);
    lua_pushstring(L, clang_getCString(str));
    clang_disposeString(str);
    return 1;
}

static int l_referenced(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXCursor *res = newCursor(L);
    *res = clang_getCursorReferenced(cur);
    if (clang_Cursor_isNull(*res))
        lua_pushnil(L);
    return 1;
}

static int l_definition(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXCursor *res = newCursor(L);
    *res = clang_getCursorDefinition(cur);
    if (clang_Cursor_isNull(*res))
        lua_pushnil(L);
    return 1;
}

static int l_isStatic(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    lua_pushboolean(L, clang_CXXMethod_isStatic(cur));
    return 1;
}

static int l_isVirtual(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    lua_pushboolean(L, clang_CXXMethod_isVirtual(cur));
    return 1;
}

static int l_resultType(lua_State *L) {
    CXCursor cur = toCursor(L, 1);
    CXType *type = newType(L);
    *type = clang_getCursorResultType(cur);
    if (type->kind == CXType_Invalid)
        lua_pushnil(L);
    return 1;
}

static int l_cursorEqual(lua_State *L) {
    CXCursor cur1 = toCursor(L, 1);
    CXCursor cur2 = toCursor(L, 2);
    lua_pushboolean(L, clang_equalCursors(cur1, cur2));
    return 1;
}

static int l_underlyingType(lua_State* L) {
    CXCursor cur = toCursor(L, 1);
    CXType type = clang_getTypedefDeclUnderlyingType(cur);
    if (type.kind != CXType_Invalid)
        *newType(L) = type;
    else
        lua_pushnil(L);
    return 1;
}

static luaL_Reg cursor_functions[] = {
    {"children", l_children},
    {"kind", l_kind},
    {"spelling", l_spelling},
    {"name", l_name},
    {"__tostring", l_name},
    {"displayName", l_displayName},
    {"parent", l_parent},
    {"arguments", l_arguments},
    {"specializedTemplate", l_specializedTemplate},
    {"templateArguments", l_templateArguments},
    {"type", l_type},
    {"access", l_access},
    {"location", l_location},
    {"usr", l_usr},
    {"referenced", l_referenced},
    {"definition", l_definition},
    {"isStatic", l_isStatic},
    {"isVirtual", l_isVirtual},
    {"resultType", l_resultType},
    {"underlyingType", l_underlyingType},
    {"__eq", l_cursorEqual},
    {NULL, NULL}
};

/******** TYPE ********/

static int l_typeToString(lua_State *L) {
    CXType type = toType(L, 1);
    CXTypeKind kind = type.kind;
    CXString str = clang_getTypeKindSpelling(kind);
    lua_pushstring(L, clang_getCString(str));
    clang_disposeString(str);
    return 1;
}

static int l_typeKind(lua_State* L) {
    CXType type = toType(L, 1);
    lua_pushinteger(L, type.kind);
    return 1;
}

static int l_typeSpelling(lua_State* L) {
    CXType type = toType(L, 1);
    CXString str = clang_getTypeSpelling(type);
    lua_pushstring(L, clang_getCString(str));
    clang_disposeString(str);
    return 1;
}

static int l_canonical(lua_State *L) {
    CXType type = toType(L, 1);
    CXType *can = newType(L);
    if (type.kind == CXType_Pointer) {
        *can = clang_getPointeeType(type);
    } else
        *can = clang_getCanonicalType(type);
    if (can->kind == CXType_Invalid)
        lua_pushnil(L);
    return 1;
}

static int l_isPod(lua_State *L) {
    CXType type = toType(L, 1);
    lua_pushboolean(L, clang_isPODType(type));
    return 1;
}

static int l_typeEq(lua_State *L) {
    CXType type = toType(L, 1);
    CXType type2 = toType(L, 2);
    lua_pushboolean(L, clang_equalTypes(type, type2));
    return 1;
}

static int l_declaration(lua_State *L) {
    CXType type = toType(L, 1);
    CXCursor *cur = newCursor(L);
    *cur = clang_getTypeDeclaration(type);
    if (clang_Cursor_isNull(*cur))
        lua_pushnil(L);
    return 1;
}

static int l_pointee(lua_State *L) {
    CXType type = toType(L, 1);
    CXType *res = newType(L);
    *res = clang_getPointeeType(type);
    if (res->kind == CXType_Invalid)
        lua_pushnil(L);
    return 1;
}

static int l_elementType(lua_State* L) {
    CXType type = toType(L, 1);
    CXType element = clang_getElementType(type);
    if (element.kind == CXType_Invalid)
        lua_pushnil(L);
    else
        *newType(L) = element;
    return 1;
}

static int l_typeResultType(lua_State *L) {
    CXType type = toType(L, 1);
    CXType result = clang_getResultType(type);
    if (result.kind == CXType_Invalid)
        lua_pushnil(L);
    else
        *newType(L) = result;
    return 1;
}

static int l_typeArguments(lua_State* L) {
    CXType type = toType(L, 1);
    int nArgs = clang_getNumArgTypes(type);
    if (nArgs != -1) {
        lua_createtable(L, nArgs, 0);
        for (int i=0; i<nArgs; i++) {
            *newType(L) = clang_getArgType(type, i);
            lua_rawseti(L, -2, i+1);
        }
    } else {
        lua_pushnil(L);
    }
    return 1;
}

static int l_typeTemplateArguments(lua_State* L) {
    CXType type = toType(L, 1);
    int nArgs = clang_Type_getNumTemplateArguments(type);
    if (nArgs != -1) {
        lua_createtable(L, nArgs, 0);
        for (int i=0; i<nArgs; i++) {
            *newType(L) = clang_Type_getTemplateArgumentAsType(type, i);
            lua_rawseti(L, -2, i+1);
        }
    } else {
        lua_pushnil(L);
    }
    return 1;
}

static int l_namedType(lua_State* L) {
    CXType type = toType(L, 1);
    CXType named = clang_Type_getNamedType(type);
    if (named.kind == CXType_Invalid)
        lua_pushnil(L);
    else
        *newType(L) = named;
    return 1;
}

static int l_arraySize(lua_State* L) {
    CXType type = toType(L, 1);
    lua_pushinteger(L, clang_getArraySize(type));
    return 1;
}

static int l_isConst(lua_State *L) {
    CXType type = toType(L, 1);
    int isConst = clang_isConstQualifiedType(type);
    lua_pushboolean(L, isConst);
    return 1;
}

static int l_isVolatile(lua_State* L) {
    CXType type = toType(L, 1);
    int isVolatile = clang_isVolatileQualifiedType(type);
    lua_pushboolean(L, isVolatile);
    return 1;
}

static luaL_Reg type_functions[] = {
    {"__tostring", l_typeToString},
    {"name", l_typeToString},
    {"kind", l_typeKind},
    {"spelling", l_typeSpelling},
    {"canonical", l_canonical},
    {"pointee", l_pointee},
    {"element", l_elementType},
    {"resultType", l_typeResultType},
    {"arguments", l_typeArguments},
    {"templateArguments", l_typeTemplateArguments},
    {"namedType", l_namedType},
    {"arraySize", l_arraySize},
    {"isPod", l_isPod},
    {"isConst", l_isConst},
    {"isVolatile", l_isVolatile},
    {"declaration", l_declaration},
    {"__eq", l_typeEq},
    {NULL, NULL}
};

/******** API ********/

void newMetatable(lua_State *L, const char * name, luaL_Reg *reg) {
    luaL_newmetatable(L, name);
    luaL_checkversion(L);
    lua_createtable(L, 0, 0);
    luaL_setfuncs(L, reg, 0);
    lua_setfield(L, -2, "__index");
}

extern "C" int luaopen_parser(lua_State *L) {
    int top = lua_gettop(L);
    newMetatable(L, LCM_INDEX, index_functions);
    newMetatable(L, LCM_TU, tu_functions);
    newMetatable(L, LCM_CURSOR, cursor_functions);
    newMetatable(L, LCM_TYPE, type_functions);

    lua_newtable(L);
    luaL_newlib(L, clang_functions);
    return 1;
}
