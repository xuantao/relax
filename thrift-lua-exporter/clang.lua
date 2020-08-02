-- clang
-- entrace
-- CXIndex = clang.createIndex(excludeDeclarationsFromPCH, displayDiagnostics)
local clang = require "luaclang-parser"

--[[ metatables
  CXIndex = {
    CXTranslationUnit = load(astFile),
    CXTranslationUnit = parse({/* args */})
  },

  CXTranslationUnit = {
    CXCursor = cursor(),
    file, time = file(filename),
    {} = diagnostics(),
    {} = codeCompleteAt(filename, line, col),
  },

  CXCursor = {
    {CXCursor, ...} = children(),
    CXCursorKind = kind(),
    string = name(),
    string = displayName(),
    string = spelling(),
    CXCursor = parent(),
    {CXCursor, ...} = arguments(),
    CXType = type(),
    CX_CXXAccessSpecifier = access(),
    file, line_b, col_b, line_e, col_e = location(),
    string = usr(),
    CXCursor = referenced(),
    CXCursor = definition(),
    boolean = isStatic(),
    boolean = isVirtual(),
    CXType = resultType(),
  },

  CXType = {
    string = name(),
    CXTypeKind = kind(),
    CXType = canonical(),
    CXType = pointee(),
    boolean = isPod(),
    boolean = isConst(),
    CXCursor = declaration(),
  }
]]

--[[
 * Error codes returned by libclang routines.
 *
 * Zero (\c CXError_Success) is the only error code indicating success.  Other
 * error codes, including not yet assigned non-zero values, indicate errors.
]]
clang.ErrorCode = {
  --[[* No error.]]
  Success = 0,
  --[[
   * A generic error code, no further details are available.
   *
   * Errors of this kind can get their own specific error codes in future
   * libclang versions.
  ]]
  Failure = 1,
  --[[* libclang crashed while performing the requested operation.]]
  Crashed = 2,
  --[[
   * The function detected that the arguments violate the function
   * contract.
  ]]
  InvalidArguments = 3,
  --[[* An AST deserialization error has occurred.]]
  ASTReadError = 4,
}

--[[
 * Describes the availability of a particular entity, which indicates
 * whether the use of this entity will result in a warning or error due to
 * it being deprecated or unavailable.
]]
clang.AvailabilityKind = {
  --[[* The entity is available.]]
  Available = 0,
  --[[
   * The entity is available, but has been deprecated (and its use is
   * not recommended).
  ]]
  Deprecated = 1,
  --[[* The entity is not available; any use of it will be an error.]]
  NotAvailable = 2,
  --[[
   * The entity is available, but not accessible; any use of it will be
   * an error.
  ]]
  NotAccessible = 3,
}

--[[
 * Describes the exception specification of a cursor.
 *
 * A negative value indicates that the cursor is not a function declaration.
]]
clang.Cursor_ExceptionSpecificationKind = {
  --[[* The cursor has no exception specification.]]
  ExceptionSpecificationKind_None = 0,
  --[[* The cursor has exception specification throw()]]
  ExceptionSpecificationKind_DynamicNone = 1,
  --[[* The cursor has exception specification throw(T1, T2)]]
  ExceptionSpecificationKind_Dynamic = 2,
  --[[* The cursor has exception specification throw(...).]]
  ExceptionSpecificationKind_MSAny = 3,
  --[[* The cursor has exception specification basic noexcept.]]
  ExceptionSpecificationKind_BasicNoexcept = 4,
  --[[* The cursor has exception specification computed noexcept.]]
  ExceptionSpecificationKind_ComputedNoexcept = 5,
  --[[* The exception specification has not yet been evaluated.]]
  ExceptionSpecificationKind_Unevaluated = 6,
  --[[* The exception specification has not yet been instantiated.]]
  ExceptionSpecificationKind_Uninstantiated = 7,
  --[[* The exception specification has not been parsed yet.]]
  ExceptionSpecificationKind_Unparsed = 8,
  --[[* The cursor has a __declspec(nothrow) exception specification.]]
  ExceptionSpecificationKind_NoThrow = 9,
}

--[[* Describes the severity of a particular diagnostic.]]
clang.DiagnosticSeverity = {
  --[[
   * A diagnostic that has been suppressed, e.g., by a command-line
   * option.
  ]]
  Ignored = 0,
  --[[
   * This diagnostic is a note that should be attached to the
   * previous (non-note) diagnostic.
  ]]
  Note = 1,
  --[[
   * This diagnostic indicates suspicious code that may not be
   * wrong.
  ]]
  Warning = 2,
  --[[* This diagnostic indicates that the code is ill-formed.]]
  Error = 3,
  --[[
   * This diagnostic indicates that the code is ill-formed such
   * that future parser recovery is unlikely to produce useful
   * results.
  ]]
  Fatal = 4,
}

--[[
 * Describes the kind of error that occurred (if any) in a call to
 * \c clang_loadDiagnostics.
]]
clang.LoadDiag_Error = {
  --[[* Indicates that no error occurred.]]
  None = 0,
  --[[
   * Indicates that an unknown error occurred while attempting to
   * deserialize diagnostics.
  ]]
  Unknown = 1,
  --[[
   * Indicates that the file containing the serialized diagnostics
   * could not be opened.
  ]]
  CannotLoad = 2,
  --[[
   * Indicates that the serialized diagnostics file is invalid or
   * corrupt.
  ]]
  InvalidFile = 3,
}

--[[
 * Options to control the display of diagnostics.
 *
 * The values in this enum are meant to be combined to customize the
 * behavior of \c clang_formatDiagnostic().
]]
clang.DiagnosticDisplayOptions = {
  --[[
   * Display the source-location information where the
   * diagnostic was located.
   *
   * When set, diagnostics will be prefixed by the file, line, and
   * (optionally) column to which the diagnostic refers. For example,
   *
   * \code
   * test.c:28: warning: extra tokens at end of #endif directive
   * \endcode
   *
   * This option corresponds to the clang flag \c -fshow-source-location.
  ]]
  DisplaySourceLocation = 1,
  --[[
   * If displaying the source-location information of the
   * diagnostic, also include the column number.
   *
   * This option corresponds to the clang flag \c -fshow-column.
  ]]
  DisplayColumn = 2,
  --[[
   * If displaying the source-location information of the
   * diagnostic, also include information about source ranges in a
   * machine-parsable format.
   *
   * This option corresponds to the clang flag
   * \c -fdiagnostics-print-source-range-info.
  ]]
  DisplaySourceRanges = 4,
  --[[
   * Display the option name associated with this diagnostic, if any.
   *
   * The option name displayed (e.g., -Wconversion) will be placed in brackets
   * after the diagnostic text. This option corresponds to the clang flag
   * \c -fdiagnostics-show-option.
  ]]
  DisplayOption = 8,
  --[[
   * Display the category number associated with this diagnostic, if any.
   *
   * The category number is displayed within brackets after the diagnostic text.
   * This option corresponds to the clang flag
   * \c -fdiagnostics-show-category=id.
  ]]
  DisplayCategoryId = 16,
  --[[
   * Display the category name associated with this diagnostic, if any.
   *
   * The category name is displayed within brackets after the diagnostic text.
   * This option corresponds to the clang flag
   * \c -fdiagnostics-show-category=name.
  ]]
  DisplayCategoryName = 32,
}

--[[
 * Flags that control the creation of translation units.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * constructing the translation unit.
]]
clang.TranslationUnit_Flags = {
  --[[
   * Used to indicate that no special translation-unit options are
   * needed.
  ]]
  None = 0,
  --[[
   * Used to indicate that the parser should construct a "detailed"
   * preprocessing record, including all macro definitions and instantiations.
   *
   * Constructing a detailed preprocessing record requires more memory
   * and time to parse, since the information contained in the record
   * is usually not retained. However, it can be useful for
   * applications that require more detailed information about the
   * behavior of the preprocessor.
  ]]
  DetailedPreprocessingRecord = 1,
  --[[
   * Used to indicate that the translation unit is incomplete.
   *
   * When a translation unit is considered "incomplete", semantic
   * analysis that is typically performed at the end of the
   * translation unit will be suppressed. For example, this suppresses
   * the completion of tentative declarations in C and of
   * instantiation of implicitly-instantiation function templates in
   * C++. This option is typically used when parsing a header with the
   * intent of producing a precompiled header.
  ]]
  Incomplete = 2,
  --[[
   * Used to indicate that the translation unit should be built with an
   * implicit precompiled header for the preamble.
   *
   * An implicit precompiled header is used as an optimization when a
   * particular translation unit is likely to be reparsed many times
   * when the sources aren't changing that often. In this case, an
   * implicit precompiled header will be built containing all of the
   * initial includes at the top of the main file (what we refer to as
   * the "preamble" of the file). In subsequent parses, if the
   * preamble or the files in it have not changed, \c
   * clang_reparseTranslationUnit() will re-use the implicit
   * precompiled header to improve parsing performance.
  ]]
  PrecompiledPreamble = 4,
  --[[
   * Used to indicate that the translation unit should cache some
   * code-completion results with each reparse of the source file.
   *
   * Caching of code-completion results is a performance optimization that
   * introduces some overhead to reparsing but improves the performance of
   * code-completion operations.
  ]]
  CacheCompletionResults = 8,
  --[[
   * Used to indicate that the translation unit will be serialized with
   * \c clang_saveTranslationUnit.
   *
   * This option is typically used when parsing a header with the intent of
   * producing a precompiled header.
  ]]
  ForSerialization = 16,
  --[[
   * DEPRECATED: Enabled chained precompiled preambles in C++.
   *
   * Note: this is a *temporary* option that is available only while
   * we are testing C++ precompiled preamble support. It is deprecated.
  ]]
  CXXChainedPCH = 32,
  --[[
   * Used to indicate that function/method bodies should be skipped while
   * parsing.
   *
   * This option can be used to search for declarations/definitions while
   * ignoring the usages.
  ]]
  SkipFunctionBodies = 64,
  --[[
   * Used to indicate that brief documentation comments should be
   * included into the set of code completions returned from this translation
   * unit.
  ]]
  IncludeBriefCommentsInCodeCompletion = 128,
  --[[
   * Used to indicate that the precompiled preamble should be created on
   * the first parse. Otherwise it will be created on the first reparse. This
   * trades runtime on the first parse (serializing the preamble takes time) for
   * reduced runtime on the second parse (can now reuse the preamble).
  ]]
  CreatePreambleOnFirstParse = 256,
  --[[
   * Do not stop processing when fatal errors are encountered.
   *
   * When fatal errors are encountered while parsing a translation unit,
   * semantic analysis is typically stopped early when compiling code. A common
   * source for fatal errors are unresolvable include files. For the
   * purposes of an IDE, this is undesirable behavior and as much information
   * as possible should be reported. Use this flag to enable this behavior.
  ]]
  KeepGoing = 512,
  --[[* Sets the preprocessor in a mode for parsing a single file only.]]
  SingleFileParse = 1024,
  --[[
   * Used in combination with CXTranslationUnit_SkipFunctionBodies to
   * constrain the skipping of function bodies to the preamble.
   *
   * The function bodies of the main file are not skipped.
  ]]
  LimitSkipFunctionBodiesToPreamble = 2048,
  --[[* Used to indicate that attributed types should be included in CXType.]]
  IncludeAttributedTypes = 4096,
  --[[* Used to indicate that implicit attributes should be visited.]]
  VisitImplicitAttributes = 8192,
  --[[
   * Used to indicate that non-errors from included files should be ignored.
   *
   * If set, clang_getDiagnosticSetFromTU() will not report e.g. warnings from
   * included files anymore. This speeds up clang_getDiagnosticSetFromTU() for
   * the case where these warnings are not of interest, as for an IDE for
   * example, which typically shows only the diagnostics in the main file.
  ]]
  IgnoreNonErrorsFromIncludedFiles = 16384,
}

--[[
 * Flags that control how translation units are saved.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * saving the translation unit.
]]
clang.SaveTranslationUnit_Flags = {
  --[[* Used to indicate that no special saving options are needed.]]
  None = 0,
}

--[[
 * Describes the kind of error that occurred (if any) in a call to
 * \c clang_saveTranslationUnit().
]]
clang.SaveError = {
  --[[* Indicates that no error occurred while saving a translation unit.]]
  None = 0,
  --[[
   * Indicates that an unknown error occurred while attempting to save
   * the file.
   *
   * This error typically indicates that file I/O failed when attempting to
   * write the file.
  ]]
  Unknown = 1,
  --[[
   * Indicates that errors during translation prevented this attempt
   * to save the translation unit.
   *
   * Errors that prevent the translation unit from being saved can be
   * extracted using \c clang_getNumDiagnostics() and \c clang_getDiagnostic().
  ]]
  TranslationErrors = 2,
  --[[
   * Indicates that the translation unit to be saved was somehow
   * invalid (e.g., NULL).
  ]]
  InvalidTU = 3,
}

--[[
 * Flags that control the reparsing of translation units.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * reparsing the translation unit.
]]
clang.Reparse_Flags = {
  --[[* Used to indicate that no special reparsing options are needed.]]
  None = 0,
}

--[[* Categorizes how memory is being used by a translation unit.]]
clang.TUResourceUsageKind = {
  AST = 1,
  Identifiers = 2,
  Selectors = 3,
  GlobalCompletionResults = 4,
  SourceManagerContentCache = 5,
  AST_SideTables = 6,
  SourceManager_Membuffer_Malloc = 7,
  SourceManager_Membuffer_MMap = 8,
  ExternalASTSource_Membuffer_Malloc = 9,
  ExternalASTSource_Membuffer_MMap = 10,
  Preprocessor = 11,
  PreprocessingRecord = 12,
  SourceManager_DataStructures = 13,
  Preprocessor_HeaderSearch = 14,
  MEMORY_IN_BYTES_BEGIN = 1,
  MEMORY_IN_BYTES_END = 14,
  First = 1,
  Last = 14,
}

--[[* Describes the kind of entity that a cursor refers to.]]
clang.CursorKind = {
  --[[
   * A declaration whose specific kind is not exposed via this
   * interface.
   *
   * Unexposed declarations have the same operations as any other kind
   * of declaration; one can extract their location information,
   * spelling, find their definitions, etc. However, the specific kind
   * of the declaration is not reported.
  ]]
  UnexposedDecl = 1,
  --[[A C or C++ struct.]]
  StructDecl = 2,
  --[[A C or C++ union.]]
  UnionDecl = 3,
  --[[A C++ class.]]
  ClassDecl = 4,
  --[[An enumeration.]]
  EnumDecl = 5,
  --[[
   * A field (in C) or non-static data member (in C++) in a
   * struct, union, or C++ class.
  ]]
  FieldDecl = 6,
  --[[An enumerator constant.]]
  EnumConstantDecl = 7,
  --[[A function.]]
  FunctionDecl = 8,
  --[[A variable.]]
  VarDecl = 9,
  --[[A function or method parameter.]]
  ParmDecl = 10,
  --[[An Objective-C \@interface.]]
  ObjCInterfaceDecl = 11,
  --[[An Objective-C \@interface for a category.]]
  ObjCCategoryDecl = 12,
  --[[An Objective-C \@protocol declaration.]]
  ObjCProtocolDecl = 13,
  --[[An Objective-C \@property declaration.]]
  ObjCPropertyDecl = 14,
  --[[An Objective-C instance variable.]]
  ObjCIvarDecl = 15,
  --[[An Objective-C instance method.]]
  ObjCInstanceMethodDecl = 16,
  --[[An Objective-C class method.]]
  ObjCClassMethodDecl = 17,
  --[[An Objective-C \@implementation.]]
  ObjCImplementationDecl = 18,
  --[[An Objective-C \@implementation for a category.]]
  ObjCCategoryImplDecl = 19,
  --[[A typedef.]]
  TypedefDecl = 20,
  --[[A C++ class method.]]
  CXXMethod = 21,
  --[[A C++ namespace.]]
  Namespace = 22,
  --[[A linkage specification, e.g. 'extern "C"'.]]
  LinkageSpec = 23,
  --[[A C++ constructor.]]
  Constructor = 24,
  --[[A C++ destructor.]]
  Destructor = 25,
  --[[A C++ conversion function.]]
  ConversionFunction = 26,
  --[[A C++ template type parameter.]]
  TemplateTypeParameter = 27,
  --[[A C++ non-type template parameter.]]
  NonTypeTemplateParameter = 28,
  --[[A C++ template template parameter.]]
  TemplateTemplateParameter = 29,
  --[[A C++ function template.]]
  FunctionTemplate = 30,
  --[[A C++ class template.]]
  ClassTemplate = 31,
  --[[A C++ class template partial specialization.]]
  ClassTemplatePartialSpecialization = 32,
  --[[A C++ namespace alias declaration.]]
  NamespaceAlias = 33,
  --[[A C++ using directive.]]
  UsingDirective = 34,
  --[[A C++ using declaration.]]
  UsingDeclaration = 35,
  --[[A C++ alias declaration]]
  TypeAliasDecl = 36,
  --[[An Objective-C \@synthesize definition.]]
  ObjCSynthesizeDecl = 37,
  --[[An Objective-C \@dynamic definition.]]
  ObjCDynamicDecl = 38,
  --[[An access specifier.]]
  CXXAccessSpecifier = 39,
  FirstDecl = 1,
  LastDecl = 39,
  --[[Decl references]]
  FirstRef = 40,
  ObjCSuperClassRef = 40,
  ObjCProtocolRef = 41,
  ObjCClassRef = 42,
  --[[
   * A reference to a type declaration.
   *
   * A type reference occurs anywhere where a type is named but not
   * declared. For example, given:
   *
   * \code
   * typedef unsigned size_type;
   * size_type size;
   * \endcode
   *
   * The typedef is a declaration of size_type (CXCursor_TypedefDecl),
   * while the type of the variable "size" is referenced. The cursor
   * referenced by the type of size is the typedef for size_type.
  ]]
  TypeRef = 43,
  CXXBaseSpecifier = 44,
  --[[
   * A reference to a class template, function template, template
   * template parameter, or class template partial specialization.
  ]]
  TemplateRef = 45,
  --[[* A reference to a namespace or namespace alias.]]
  NamespaceRef = 46,
  --[[
   * A reference to a member of a struct, union, or class that occurs in
   * some non-expression context, e.g., a designated initializer.
  ]]
  MemberRef = 47,
  --[[
   * A reference to a labeled statement.
   *
   * This cursor kind is used to describe the jump to "start_over" in the
   * goto statement in the following example:
   *
   * \code
   *   start_over:
   *     ++counter;
   *
   *     goto start_over;
   * \endcode
   *
   * A label reference cursor refers to a label statement.
  ]]
  LabelRef = 48,
  --[[
   * A reference to a set of overloaded functions or function templates
   * that has not yet been resolved to a specific function or function template.
   *
   * An overloaded declaration reference cursor occurs in C++ templates where
   * a dependent name refers to a function. For example:
   *
   * \code
   * template<typename T> void swap(T&, T&);
   *
   * struct X { ... };
   * void swap(X&, X&);
   *
   * template<typename T>
   * void reverse(T* first, T* last) {
   *   while (first < last - 1) {
   *     swap(*first, *--last);
   *     ++first;
   *   }
   * }
   *
   * struct Y { };
   * void swap(Y&, Y&);
   * \endcode
   *
   * Here, the identifier "swap" is associated with an overloaded declaration
   * reference. In the template definition, "swap" refers to either of the two
   * "swap" functions declared above, so both results will be available. At
   * instantiation time, "swap" may also refer to other functions found via
   * argument-dependent lookup (e.g., the "swap" function at the end of the
   * example).
   *
   * The functions \c clang_getNumOverloadedDecls() and
   * \c clang_getOverloadedDecl() can be used to retrieve the definitions
   * referenced by this cursor.
  ]]
  OverloadedDeclRef = 49,
  --[[
   * A reference to a variable that occurs in some non-expression
   * context, e.g., a C++ lambda capture list.
  ]]
  VariableRef = 50,
  LastRef = 50,
  --[[Error conditions]]
  FirstInvalid = 70,
  InvalidFile = 70,
  NoDeclFound = 71,
  NotImplemented = 72,
  InvalidCode = 73,
  LastInvalid = 73,
  --[[Expressions]]
  FirstExpr = 100,
  --[[
   * An expression whose specific kind is not exposed via this
   * interface.
   *
   * Unexposed expressions have the same operations as any other kind
   * of expression; one can extract their location information,
   * spelling, children, etc. However, the specific kind of the
   * expression is not reported.
  ]]
  UnexposedExpr = 100,
  --[[
   * An expression that refers to some value declaration, such
   * as a function, variable, or enumerator.
  ]]
  DeclRefExpr = 101,
  --[[
   * An expression that refers to a member of a struct, union,
   * class, Objective-C class, etc.
  ]]
  MemberRefExpr = 102,
  --[[An expression that calls a function.]]
  CallExpr = 103,
  --[[
   An expression that sends a message to an Objective-C
   object or class.
  ]]
  ObjCMessageExpr = 104,
  --[[An expression that represents a block literal.]]
  BlockExpr = 105,
  --[[An integer literal.]]
  IntegerLiteral = 106,
  --[[A floating point number literal.]]
  FloatingLiteral = 107,
  --[[An imaginary number literal.]]
  ImaginaryLiteral = 108,
  --[[A string literal.]]
  StringLiteral = 109,
  --[[A character literal.]]
  CharacterLiteral = 110,
  --[[
   A parenthesized expression, e.g. "(1)".
   *
   * This AST node is only formed if full location information is requested.
  ]]
  ParenExpr = 111,
  --[[
   This represents the unary-expression's (except sizeof and
   * alignof).
  ]]
  UnaryOperator = 112,
  --[[C99 6.5.2.1] Array Subscripting.]]
  ArraySubscriptExpr = 113,
  --[[
   A builtin binary operation expression such as "x + y" or
   * "x <= y".
  ]]
  BinaryOperator = 114,
  --[[Compound assignment such as "+=".]]
  CompoundAssignOperator = 115,
  --[[The ?: ternary operator.]]
  ConditionalOperator = 116,
  --[[
   An explicit cast in C (C99 6.5.4) or a C-style cast in C++
   * (C++ [expr.cast]), which uses the syntax (Type)expr.
   *
   * For example: (int)f.
  ]]
  CStyleCastExpr = 117,
  --[[C99 6.5.2.5]]
  CompoundLiteralExpr = 118,
  --[[Describes an C or C++ initializer list.]]
  InitListExpr = 119,
  --[[The GNU address of label extension, representing &&label.]]
  AddrLabelExpr = 120,
  --[[This is the GNU Statement Expression extension: ({int X=4; X;})]]
  StmtExpr = 121,
  --[[Represents a C11 generic selection.]]
  GenericSelectionExpr = 122,
  --[[
   Implements the GNU __null extension, which is a name for a null
   * pointer constant that has integral type (e.g., int or long) and is the same
   * size and alignment as a pointer.
   *
   * The __null extension is typically only used by system headers, which define
   * NULL as __null in C++ rather than using 0 (which is an integer that may not
   * match the size of a pointer).
  ]]
  GNUNullExpr = 123,
  --[[C++'s static_cast<> expression.]]
  CXXStaticCastExpr = 124,
  --[[C++'s dynamic_cast<> expression.]]
  CXXDynamicCastExpr = 125,
  --[[C++'s reinterpret_cast<> expression.]]
  CXXReinterpretCastExpr = 126,
  --[[C++'s const_cast<> expression.]]
  CXXConstCastExpr = 127,
  --[[
   Represents an explicit C++ type conversion that uses "functional"
   * notion (C++ [expr.type.conv]).
   *
   * Example:
   * \code
   *   x = int(0.5);
   * \endcode
  ]]
  CXXFunctionalCastExpr = 128,
  --[[A C++ typeid expression (C++ [expr.typeid]).]]
  CXXTypeidExpr = 129,
  --[[C++ 2.13.5] C++ Boolean Literal.]]
  CXXBoolLiteralExpr = 130,
  --[[C++0x 2.14.7] C++ Pointer Literal.]]
  CXXNullPtrLiteralExpr = 131,
  --[[Represents the "this" expression in C++]]
  CXXThisExpr = 132,
  --[[
   [C++ 15] C++ Throw Expression.
   *
   * This handles 'throw' and 'throw' assignment-expression. When
   * assignment-expression isn't present, Op will be null.
  ]]
  CXXThrowExpr = 133,
  --[[
   A new expression for memory allocation and constructor calls, e.g:
   * "new CXXNewExpr(foo)".
  ]]
  CXXNewExpr = 134,
  --[[
   A delete expression for memory deallocation and destructor calls,
   * e.g. "delete[] pArray".
  ]]
  CXXDeleteExpr = 135,
  --[[A unary expression. (noexcept, sizeof, or other traits)]]
  UnaryExpr = 136,
  --[[An Objective-C string literal i.e. @"foo".]]
  ObjCStringLiteral = 137,
  --[[An Objective-C \@encode expression.]]
  ObjCEncodeExpr = 138,
  --[[An Objective-C \@selector expression.]]
  ObjCSelectorExpr = 139,
  --[[An Objective-C \@protocol expression.]]
  ObjCProtocolExpr = 140,
  --[[
   An Objective-C "bridged" cast expression, which casts between
   * Objective-C pointers and C pointers, transferring ownership in the process.
   *
   * \code
   *   NSString *str = (__bridge_transfer NSString *)CFCreateString();
   * \endcode
  ]]
  ObjCBridgedCastExpr = 141,
  --[[
   Represents a C++0x pack expansion that produces a sequence of
   * expressions.
   *
   * A pack expansion expression contains a pattern (which itself is an
   * expression) followed by an ellipsis. For example:
   *
   * \code
   * template<typename F, typename ...Types>
   * void forward(F f, Types &&...args) {
   *  f(static_cast<Types&&>(args)...);
   * }
   * \endcode
  ]]
  PackExpansionExpr = 142,
  --[[
   Represents an expression that computes the length of a parameter
   * pack.
   *
   * \code
   * template<typename ...Types>
   * struct count {
   *   static const unsigned value = sizeof...(Types);
   * };
   * \endcode
  ]]
  SizeOfPackExpr = 143,
  --[[
   Represents a C++ lambda expression that produces a local function
   * object.
   *
   * \code
   * void abssort(float *x, unsigned N) {
   *   std::sort(x, x + N,
   *             [](float a, float b) {
   *               return std::abs(a) < std::abs(b);
   *             });
   * }
   * \endcode
  ]]
  LambdaExpr = 144,
  --[[Objective-c Boolean Literal.]]
  ObjCBoolLiteralExpr = 145,
  --[[Represents the "self" expression in an Objective-C method.]]
  ObjCSelfExpr = 146,
  --[[OpenMP 4.0 [2.4, Array Section].]]
  OMPArraySectionExpr = 147,
  --[[Represents an @available(...) check.]]
  ObjCAvailabilityCheckExpr = 148,
  --[[* Fixed point literal]]
  FixedPointLiteral = 149,
  LastExpr = 149,
  --[[Statements]]
  FirstStmt = 200,
  --[[
   * A statement whose specific kind is not exposed via this
   * interface.
   *
   * Unexposed statements have the same operations as any other kind of
   * statement; one can extract their location information, spelling,
   * children, etc. However, the specific kind of the statement is not
   * reported.
  ]]
  UnexposedStmt = 200,
  --[[
   A labelled statement in a function.
   *
   * This cursor kind is used to describe the "start_over:" label statement in
   * the following example:
   *
   * \code
   *   start_over:
   *     ++counter;
   * \endcode
   *
  ]]
  LabelStmt = 201,
  --[[
   A group of statements like { stmt stmt }.
   *
   * This cursor kind is used to describe compound statements, e.g. function
   * bodies.
  ]]
  CompoundStmt = 202,
  --[[A case statement.]]
  CaseStmt = 203,
  --[[A default statement.]]
  DefaultStmt = 204,
  --[[An if statement]]
  IfStmt = 205,
  --[[A switch statement.]]
  SwitchStmt = 206,
  --[[A while statement.]]
  WhileStmt = 207,
  --[[A do statement.]]
  DoStmt = 208,
  --[[A for statement.]]
  ForStmt = 209,
  --[[A goto statement.]]
  GotoStmt = 210,
  --[[An indirect goto statement.]]
  IndirectGotoStmt = 211,
  --[[A continue statement.]]
  ContinueStmt = 212,
  --[[A break statement.]]
  BreakStmt = 213,
  --[[A return statement.]]
  ReturnStmt = 214,
  --[[A GCC inline assembly statement extension.]]
  GCCAsmStmt = 215,
  AsmStmt = 215,
  --[[Objective-C's overall \@try-\@catch-\@finally statement.]]
  ObjCAtTryStmt = 216,
  --[[Objective-C's \@catch statement.]]
  ObjCAtCatchStmt = 217,
  --[[Objective-C's \@finally statement.]]
  ObjCAtFinallyStmt = 218,
  --[[Objective-C's \@throw statement.]]
  ObjCAtThrowStmt = 219,
  --[[Objective-C's \@synchronized statement.]]
  ObjCAtSynchronizedStmt = 220,
  --[[Objective-C's autorelease pool statement.]]
  ObjCAutoreleasePoolStmt = 221,
  --[[Objective-C's collection statement.]]
  ObjCForCollectionStmt = 222,
  --[[C++'s catch statement.]]
  CXXCatchStmt = 223,
  --[[C++'s try statement.]]
  CXXTryStmt = 224,
  --[[C++'s for (* : *) statement.]]
  CXXForRangeStmt = 225,
  --[[Windows Structured Exception Handling's try statement.]]
  SEHTryStmt = 226,
  --[[Windows Structured Exception Handling's except statement.]]
  SEHExceptStmt = 227,
  --[[Windows Structured Exception Handling's finally statement.]]
  SEHFinallyStmt = 228,
  --[[A MS inline assembly statement extension.]]
  MSAsmStmt = 229,
  --[[
   The null statement ";": C99 6.8.3p3.
   *
   * This cursor kind is used to describe the null statement.
  ]]
  NullStmt = 230,
  --[[
   Adaptor class for mixing declarations with statements and
   * expressions.
  ]]
  DeclStmt = 231,
  --[[OpenMP parallel directive.]]
  OMPParallelDirective = 232,
  --[[OpenMP SIMD directive.]]
  OMPSimdDirective = 233,
  --[[OpenMP for directive.]]
  OMPForDirective = 234,
  --[[OpenMP sections directive.]]
  OMPSectionsDirective = 235,
  --[[OpenMP section directive.]]
  OMPSectionDirective = 236,
  --[[OpenMP single directive.]]
  OMPSingleDirective = 237,
  --[[OpenMP parallel for directive.]]
  OMPParallelForDirective = 238,
  --[[OpenMP parallel sections directive.]]
  OMPParallelSectionsDirective = 239,
  --[[OpenMP task directive.]]
  OMPTaskDirective = 240,
  --[[OpenMP master directive.]]
  OMPMasterDirective = 241,
  --[[OpenMP critical directive.]]
  OMPCriticalDirective = 242,
  --[[OpenMP taskyield directive.]]
  OMPTaskyieldDirective = 243,
  --[[OpenMP barrier directive.]]
  OMPBarrierDirective = 244,
  --[[OpenMP taskwait directive.]]
  OMPTaskwaitDirective = 245,
  --[[OpenMP flush directive.]]
  OMPFlushDirective = 246,
  --[[Windows Structured Exception Handling's leave statement.]]
  SEHLeaveStmt = 247,
  --[[OpenMP ordered directive.]]
  OMPOrderedDirective = 248,
  --[[OpenMP atomic directive.]]
  OMPAtomicDirective = 249,
  --[[OpenMP for SIMD directive.]]
  OMPForSimdDirective = 250,
  --[[OpenMP parallel for SIMD directive.]]
  OMPParallelForSimdDirective = 251,
  --[[OpenMP target directive.]]
  OMPTargetDirective = 252,
  --[[OpenMP teams directive.]]
  OMPTeamsDirective = 253,
  --[[OpenMP taskgroup directive.]]
  OMPTaskgroupDirective = 254,
  --[[OpenMP cancellation point directive.]]
  OMPCancellationPointDirective = 255,
  --[[OpenMP cancel directive.]]
  OMPCancelDirective = 256,
  --[[OpenMP target data directive.]]
  OMPTargetDataDirective = 257,
  --[[OpenMP taskloop directive.]]
  OMPTaskLoopDirective = 258,
  --[[OpenMP taskloop simd directive.]]
  OMPTaskLoopSimdDirective = 259,
  --[[OpenMP distribute directive.]]
  OMPDistributeDirective = 260,
  --[[OpenMP target enter data directive.]]
  OMPTargetEnterDataDirective = 261,
  --[[OpenMP target exit data directive.]]
  OMPTargetExitDataDirective = 262,
  --[[OpenMP target parallel directive.]]
  OMPTargetParallelDirective = 263,
  --[[OpenMP target parallel for directive.]]
  OMPTargetParallelForDirective = 264,
  --[[OpenMP target update directive.]]
  OMPTargetUpdateDirective = 265,
  --[[OpenMP distribute parallel for directive.]]
  OMPDistributeParallelForDirective = 266,
  --[[OpenMP distribute parallel for simd directive.]]
  OMPDistributeParallelForSimdDirective = 267,
  --[[OpenMP distribute simd directive.]]
  OMPDistributeSimdDirective = 268,
  --[[OpenMP target parallel for simd directive.]]
  OMPTargetParallelForSimdDirective = 269,
  --[[OpenMP target simd directive.]]
  OMPTargetSimdDirective = 270,
  --[[OpenMP teams distribute directive.]]
  OMPTeamsDistributeDirective = 271,
  --[[OpenMP teams distribute simd directive.]]
  OMPTeamsDistributeSimdDirective = 272,
  --[[OpenMP teams distribute parallel for simd directive.]]
  OMPTeamsDistributeParallelForSimdDirective = 273,
  --[[OpenMP teams distribute parallel for directive.]]
  OMPTeamsDistributeParallelForDirective = 274,
  --[[OpenMP target teams directive.]]
  OMPTargetTeamsDirective = 275,
  --[[OpenMP target teams distribute directive.]]
  OMPTargetTeamsDistributeDirective = 276,
  --[[OpenMP target teams distribute parallel for directive.]]
  OMPTargetTeamsDistributeParallelForDirective = 277,
  --[[OpenMP target teams distribute parallel for simd directive.]]
  OMPTargetTeamsDistributeParallelForSimdDirective = 278,
  --[[OpenMP target teams distribute simd directive.]]
  OMPTargetTeamsDistributeSimdDirective = 279,
  --[[C++2a std::bit_cast expression.]]
  BuiltinBitCastExpr = 280,
  LastStmt = 280,
  --[[
   * Cursor that represents the translation unit itself.
   *
   * The translation unit cursor exists primarily to act as the root
   * cursor for traversing the contents of a translation unit.
  ]]
  TranslationUnit = 300,
  --[[Attributes]]
  FirstAttr = 400,
  --[[
   * An attribute whose specific kind is not exposed via this
   * interface.
  ]]
  UnexposedAttr = 400,
  IBActionAttr = 401,
  IBOutletAttr = 402,
  IBOutletCollectionAttr = 403,
  CXXFinalAttr = 404,
  CXXOverrideAttr = 405,
  AnnotateAttr = 406,
  AsmLabelAttr = 407,
  PackedAttr = 408,
  PureAttr = 409,
  ConstAttr = 410,
  NoDuplicateAttr = 411,
  CUDAConstantAttr = 412,
  CUDADeviceAttr = 413,
  CUDAGlobalAttr = 414,
  CUDAHostAttr = 415,
  CUDASharedAttr = 416,
  VisibilityAttr = 417,
  DLLExport = 418,
  DLLImport = 419,
  NSReturnsRetained = 420,
  NSReturnsNotRetained = 421,
  NSReturnsAutoreleased = 422,
  NSConsumesSelf = 423,
  NSConsumed = 424,
  ObjCException = 425,
  ObjCNSObject = 426,
  ObjCIndependentClass = 427,
  ObjCPreciseLifetime = 428,
  ObjCReturnsInnerPointer = 429,
  ObjCRequiresSuper = 430,
  ObjCRootClass = 431,
  ObjCSubclassingRestricted = 432,
  ObjCExplicitProtocolImpl = 433,
  ObjCDesignatedInitializer = 434,
  ObjCRuntimeVisible = 435,
  ObjCBoxable = 436,
  FlagEnum = 437,
  ConvergentAttr = 438,
  WarnUnusedAttr = 439,
  WarnUnusedResultAttr = 440,
  AlignedAttr = 441,
  LastAttr = 441,
  --[[Preprocessing]]
  PreprocessingDirective = 500,
  MacroDefinition = 501,
  MacroExpansion = 502,
  MacroInstantiation = 502,
  InclusionDirective = 503,
  FirstPreprocessing = 500,
  LastPreprocessing = 503,
  --[[* A module import declaration.]]
  ModuleImportDecl = 600,
  TypeAliasTemplateDecl = 601,
  --[[* A static_assert or _Static_assert node]]
  StaticAssert = 602,
  --[[* a friend declaration.]]
  FriendDecl = 603,
  FirstExtraDecl = 600,
  LastExtraDecl = 603,
  --[[* A code completion overload candidate.]]
  OverloadCandidate = 700,
}

--[[* Describe the linkage of the entity referred to by a cursor.]]
clang.LinkageKind = {
  --[[
   This value indicates that no linkage information is available
   * for a provided CXCursor.
  ]]
  Invalid = 0,
  --[[
   * This is the linkage for variables, parameters, and so on that
   *  have automatic storage.  This covers normal (non-extern) local variables.
  ]]
  NoLinkage = 1,
  --[[This is the linkage for static variables and static functions.]]
  Internal = 2,
  --[[
   This is the linkage for entities with external linkage that live
   * in C++ anonymous namespaces.
  ]]
  UniqueExternal = 3,
  --[[This is the linkage for entities with true, external linkage.]]
  External = 4,
}

clang.VisibilityKind = {
  --[[
   This value indicates that no visibility information is available
   * for a provided CXCursor.
  ]]
  Invalid = 0,
  --[[Symbol not seen by the linker.]]
  Hidden = 1,
  --[[Symbol seen by the linker but resolves to a symbol inside this object.]]
  Protected = 2,
  --[[Symbol seen by the linker and acts like a normal symbol.]]
  Default = 3,
}

--[[* Describe the "language" of the entity referred to by a cursor.]]
clang.LanguageKind = {
  Invalid = 0,
  C = 1,
  ObjC = 2,
  CPlusPlus = 3,
}

--[[
 * Describe the "thread-local storage (TLS) kind" of the declaration
 * referred to by a cursor.
]]
clang.TLSKind = {
  None = 0,
  Dynamic = 1,
  Static = 2,
}

--[[* Describes the kind of type]]
clang.TypeKind = {
  --[[* Represents an invalid type (e.g., where no type is available).]]
  Invalid = 0,
  --[[
   * A type whose specific kind is not exposed via this
   * interface.
  ]]
  Unexposed = 1,
  --[[Builtin types]]
  Void = 2,
  Bool = 3,
  Char_U = 4,
  UChar = 5,
  Char16 = 6,
  Char32 = 7,
  UShort = 8,
  UInt = 9,
  ULong = 10,
  ULongLong = 11,
  UInt128 = 12,
  Char_S = 13,
  SChar = 14,
  WChar = 15,
  Short = 16,
  Int = 17,
  Long = 18,
  LongLong = 19,
  Int128 = 20,
  Float = 21,
  Double = 22,
  LongDouble = 23,
  NullPtr = 24,
  Overload = 25,
  Dependent = 26,
  ObjCId = 27,
  ObjCClass = 28,
  ObjCSel = 29,
  Float128 = 30,
  Half = 31,
  Float16 = 32,
  ShortAccum = 33,
  Accum = 34,
  LongAccum = 35,
  UShortAccum = 36,
  UAccum = 37,
  ULongAccum = 38,
  FirstBuiltin = 2,
  LastBuiltin = 38,
  Complex = 100,
  Pointer = 101,
  BlockPointer = 102,
  LValueReference = 103,
  RValueReference = 104,
  Record = 105,
  Enum = 106,
  Typedef = 107,
  ObjCInterface = 108,
  ObjCObjectPointer = 109,
  FunctionNoProto = 110,
  FunctionProto = 111,
  ConstantArray = 112,
  Vector = 113,
  IncompleteArray = 114,
  VariableArray = 115,
  DependentSizedArray = 116,
  MemberPointer = 117,
  Auto = 118,
  --[[
   * Represents a type that was referred to using an elaborated type keyword.
   *
   * E.g., struct S, or via a qualified name, e.g., N::M::type, or both.
  ]]
  Elaborated = 119,
  --[[OpenCL PipeType.]]
  Pipe = 120,
  --[[OpenCL builtin types.]]
  OCLImage1dRO = 121,
  OCLImage1dArrayRO = 122,
  OCLImage1dBufferRO = 123,
  OCLImage2dRO = 124,
  OCLImage2dArrayRO = 125,
  OCLImage2dDepthRO = 126,
  OCLImage2dArrayDepthRO = 127,
  OCLImage2dMSAARO = 128,
  OCLImage2dArrayMSAARO = 129,
  OCLImage2dMSAADepthRO = 130,
  OCLImage2dArrayMSAADepthRO = 131,
  OCLImage3dRO = 132,
  OCLImage1dWO = 133,
  OCLImage1dArrayWO = 134,
  OCLImage1dBufferWO = 135,
  OCLImage2dWO = 136,
  OCLImage2dArrayWO = 137,
  OCLImage2dDepthWO = 138,
  OCLImage2dArrayDepthWO = 139,
  OCLImage2dMSAAWO = 140,
  OCLImage2dArrayMSAAWO = 141,
  OCLImage2dMSAADepthWO = 142,
  OCLImage2dArrayMSAADepthWO = 143,
  OCLImage3dWO = 144,
  OCLImage1dRW = 145,
  OCLImage1dArrayRW = 146,
  OCLImage1dBufferRW = 147,
  OCLImage2dRW = 148,
  OCLImage2dArrayRW = 149,
  OCLImage2dDepthRW = 150,
  OCLImage2dArrayDepthRW = 151,
  OCLImage2dMSAARW = 152,
  OCLImage2dArrayMSAARW = 153,
  OCLImage2dMSAADepthRW = 154,
  OCLImage2dArrayMSAADepthRW = 155,
  OCLImage3dRW = 156,
  OCLSampler = 157,
  OCLEvent = 158,
  OCLQueue = 159,
  OCLReserveID = 160,
  ObjCObject = 161,
  ObjCTypeParam = 162,
  Attributed = 163,
  OCLIntelSubgroupAVCMcePayload = 164,
  OCLIntelSubgroupAVCImePayload = 165,
  OCLIntelSubgroupAVCRefPayload = 166,
  OCLIntelSubgroupAVCSicPayload = 167,
  OCLIntelSubgroupAVCMceResult = 168,
  OCLIntelSubgroupAVCImeResult = 169,
  OCLIntelSubgroupAVCRefResult = 170,
  OCLIntelSubgroupAVCSicResult = 171,
  OCLIntelSubgroupAVCImeResultSingleRefStreamout = 172,
  OCLIntelSubgroupAVCImeResultDualRefStreamout = 173,
  OCLIntelSubgroupAVCImeSingleRefStreamin = 174,
  OCLIntelSubgroupAVCImeDualRefStreamin = 175,
  ExtVector = 176,
}

--[[* Describes the calling convention of a function type]]
clang.CallingConv = {
  Default = 0,
  C = 1,
  X86StdCall = 2,
  X86FastCall = 3,
  X86ThisCall = 4,
  X86Pascal = 5,
  AAPCS = 6,
  AAPCS_VFP = 7,
  X86RegCall = 8,
  IntelOclBicc = 9,
  Win64 = 10,
  --[[Alias for compatibility with older versions of API.]]
  X86_64Win64 = 10,
  X86_64SysV = 11,
  X86VectorCall = 12,
  Swift = 13,
  PreserveMost = 14,
  PreserveAll = 15,
  AArch64VectorCall = 16,
  Invalid = 100,
  Unexposed = 200,
}

--[[
 * Describes the kind of a template argument.
 *
 * See the definition of llvm::clang::TemplateArgument::ArgKind for full
 * element descriptions.
]]
clang.TemplateArgumentKind = {
  Null = 0,
  Type = 1,
  Declaration = 2,
  NullPtr = 3,
  Integral = 4,
  Template = 5,
  TemplateExpansion = 6,
  Expression = 7,
  Pack = 8,
  --[[Indicates an error case, preventing the kind from being deduced.]]
  Invalid = 9,
}

clang.TypeNullabilityKind = {
  --[[* Values of this type can never be null.]]
  NonNull = 0,
  --[[* Values of this type can be null.]]
  Nullable = 1,
  --[[
   * Whether values of this type can be null is (explicitly)
   * unspecified. This captures a (fairly rare) case where we
   * can't conclude anything about the nullability of the type even
   * though it has been considered.
  ]]
  Unspecified = 2,
  --[[* Nullability is not applicable to this type.]]
  Invalid = 3,
}

clang.RefQualifierKind = {
  --[[No ref-qualifier was provided.]]
  None = 0,
  --[[An lvalue ref-qualifier was provided (\c &).]]
  LValue = 1,
  --[[An rvalue ref-qualifier was provided (\c &&).]]
  RValue = 2,
}

--[[
 * Represents the C++ access control level to a base class for a
 * cursor with kind CX_CXXBaseSpecifier.
]]
clang.CXXAccessSpecifier = {
  CXXInvalidAccessSpecifier = 0,
  CXXPublic = 1,
  CXXProtected = 2,
  CXXPrivate = 3,
}

--[[
 * Represents the storage classes as declared in the source. CX_SC_Invalid
 * was added for the case that the passed cursor in not a declaration.
]]
clang.StorageClass = {
  SC_Invalid = 0,
  SC_None = 1,
  SC_Extern = 2,
  SC_Static = 3,
  SC_PrivateExtern = 4,
  SC_OpenCLWorkGroupLocal = 5,
  SC_Auto = 6,
  SC_Register = 7,
}

--[[
 * Describes how the traversal of the children of a particular
 * cursor should proceed after visiting a particular child cursor.
 *
 * A value of this enumeration type should be returned by each
 * \c CXCursorVisitor to indicate how clang_visitChildren() proceed.
]]
clang.ChildVisitResult = {
  --[[* Terminates the cursor traversal.]]
  Break = 0,
  --[[
   * Continues the cursor traversal with the next sibling of
   * the cursor just visited, without visiting its children.
  ]]
  Continue = 1,
  --[[
   * Recursively traverse the children of this cursor, using
   * the same visitor and client data.
  ]]
  Recurse = 2,
}

--[[
 * Properties for the printing policy.
 *
 * See \c clang::PrintingPolicy for more information.
]]
clang.PrintingPolicyProperty = {
  Indentation = 0,
  SuppressSpecifiers = 1,
  SuppressTagKeyword = 2,
  IncludeTagDefinition = 3,
  SuppressScope = 4,
  SuppressUnwrittenScope = 5,
  SuppressInitializers = 6,
  ConstantArraySizeAsWritten = 7,
  AnonymousTagLocations = 8,
  SuppressStrongLifetime = 9,
  SuppressLifetimeQualifiers = 10,
  SuppressTemplateArgsInCXXConstructors = 11,
  Bool = 12,
  Restrict = 13,
  Alignof = 14,
  UnderscoreAlignof = 15,
  UseVoidForZeroParams = 16,
  TerseOutput = 17,
  PolishForDeclaration = 18,
  Half = 19,
  MSWChar = 20,
  IncludeNewlines = 21,
  MSVCFormatting = 22,
  ConstantsAsWritten = 23,
  SuppressImplicitBase = 24,
  FullyQualifiedName = 25,
  LastProperty = 25,
}

clang.ObjCPropertyAttrKind = {
  noattr = 0,
  readonly = 1,
  getter = 2,
  assign = 4,
  readwrite = 8,
  retain = 16,
  copy = 32,
  nonatomic = 64,
  setter = 128,
  atomic = 256,
  weak = 512,
  strong = 1024,
  unsafe_unretained = 2048,
  class = 4096,
}

clang.ObjCDeclQualifierKind = {
  None = 0,
  In = 1,
  Inout = 2,
  Out = 4,
  Bycopy = 8,
  Byref = 16,
  Oneway = 32,
}

clang.NameRefFlags = {
  --[[
   * Include the nested-name-specifier, e.g. Foo:: in x.Foo::y, in the
   * range.
  ]]
  WantQualifier = 1,
  --[[
   * Include the explicit template arguments, e.g. \<int> in x.f<int>,
   * in the range.
  ]]
  WantTemplateArgs = 2,
  --[[
   * If the name is non-contiguous, return the full spanning range.
   *
   * Non-contiguous names occur in Objective-C when a selector with two or more
   * parameters is used, or in C++ when using an operator:
   * \code
   * [object doSomething:here withValue:there]; // Objective-C
   * return some_vector[1]; // C++
   * \endcode
  ]]
  WantSinglePiece = 4,
}

clang.TokenKind = {
  --[[* A token that contains some kind of punctuation.]]
  Punctuation = 0,
  --[[* A language keyword.]]
  Keyword = 1,
  --[[* An identifier (that is not a keyword).]]
  Identifier = 2,
  --[[* A numeric, string, or character literal.]]
  Literal = 3,
  --[[* A comment.]]
  Comment = 4,
}

--[[
 * Describes a single piece of text within a code-completion string.
 *
 * Each "chunk" within a code-completion string (\c CXCompletionString) is
 * either a piece of text with a specific "kind" that describes how that text
 * should be interpreted by the client or is another completion string.
]]
clang.CompletionChunkKind = {
  --[[
   * A code-completion string that describes "optional" text that
   * could be a part of the template (but is not required).
   *
   * The Optional chunk is the only kind of chunk that has a code-completion
   * string for its representation, which is accessible via
   * \c clang_getCompletionChunkCompletionString(). The code-completion string
   * describes an additional part of the template that is completely optional.
   * For example, optional chunks can be used to describe the placeholders for
   * arguments that match up with defaulted function parameters, e.g. given:
   *
   * \code
   * void f(int x, float y = 3.14, double z = 2.71828);
   * \endcode
   *
   * The code-completion string for this function would contain:
   *   - a TypedText chunk for "f".
   *   - a LeftParen chunk for "(".
   *   - a Placeholder chunk for "int x"
   *   - an Optional chunk containing the remaining defaulted arguments, e.g.,
   *       - a Comma chunk for ","
   *       - a Placeholder chunk for "float y"
   *       - an Optional chunk containing the last defaulted argument:
   *           - a Comma chunk for ","
   *           - a Placeholder chunk for "double z"
   *   - a RightParen chunk for ")"
   *
   * There are many ways to handle Optional chunks. Two simple approaches are:
   *   - Completely ignore optional chunks, in which case the template for the
   *     function "f" would only include the first parameter ("int x").
   *   - Fully expand all optional chunks, in which case the template for the
   *     function "f" would have all of the parameters.
  ]]
  Optional = 0,
  --[[
   * Text that a user would be expected to type to get this
   * code-completion result.
   *
   * There will be exactly one "typed text" chunk in a semantic string, which
   * will typically provide the spelling of a keyword or the name of a
   * declaration that could be used at the current code point. Clients are
   * expected to filter the code-completion results based on the text in this
   * chunk.
  ]]
  TypedText = 1,
  --[[
   * Text that should be inserted as part of a code-completion result.
   *
   * A "text" chunk represents text that is part of the template to be
   * inserted into user code should this particular code-completion result
   * be selected.
  ]]
  Text = 2,
  --[[
   * Placeholder text that should be replaced by the user.
   *
   * A "placeholder" chunk marks a place where the user should insert text
   * into the code-completion template. For example, placeholders might mark
   * the function parameters for a function declaration, to indicate that the
   * user should provide arguments for each of those parameters. The actual
   * text in a placeholder is a suggestion for the text to display before
   * the user replaces the placeholder with real code.
  ]]
  Placeholder = 3,
  --[[
   * Informative text that should be displayed but never inserted as
   * part of the template.
   *
   * An "informative" chunk contains annotations that can be displayed to
   * help the user decide whether a particular code-completion result is the
   * right option, but which is not part of the actual template to be inserted
   * by code completion.
  ]]
  Informative = 4,
  --[[
   * Text that describes the current parameter when code-completion is
   * referring to function call, message send, or template specialization.
   *
   * A "current parameter" chunk occurs when code-completion is providing
   * information about a parameter corresponding to the argument at the
   * code-completion point. For example, given a function
   *
   * \code
   * int add(int x, int y);
   * \endcode
   *
   * and the source code \c add(, where the code-completion point is after the
   * "(", the code-completion string will contain a "current parameter" chunk
   * for "int x", indicating that the current argument will initialize that
   * parameter. After typing further, to \c add(17, (where the code-completion
   * point is after the ","), the code-completion string will contain a
   * "current parameter" chunk to "int y".
  ]]
  CurrentParameter = 5,
  --[[
   * A left parenthesis ('('), used to initiate a function call or
   * signal the beginning of a function parameter list.
  ]]
  LeftParen = 6,
  --[[
   * A right parenthesis (')'), used to finish a function call or
   * signal the end of a function parameter list.
  ]]
  RightParen = 7,
  --[[* A left bracket ('[').]]
  LeftBracket = 8,
  --[[* A right bracket (']').]]
  RightBracket = 9,
  --[[* A left brace ('{').]]
  LeftBrace = 10,
  --[[* A right brace ('}').]]
  RightBrace = 11,
  --[[* A left angle bracket ('<').]]
  LeftAngle = 12,
  --[[* A right angle bracket ('>').]]
  RightAngle = 13,
  --[[* A comma separator (',').]]
  Comma = 14,
  --[[
   * Text that specifies the result type of a given result.
   *
   * This special kind of informative chunk is not meant to be inserted into
   * the text buffer. Rather, it is meant to illustrate the type that an
   * expression using the given completion string would have.
  ]]
  ResultType = 15,
  --[[* A colon (':').]]
  Colon = 16,
  --[[* A semicolon (';').]]
  SemiColon = 17,
  --[[* An '=' sign.]]
  Equal = 18,
  --[[* Horizontal space (' ').]]
  HorizontalSpace = 19,
  --[[
   * Vertical space ('\\n'), after which it is generally a good idea to
   * perform indentation.
  ]]
  VerticalSpace = 20,
}

--[[
 * Flags that can be passed to \c clang_codeCompleteAt() to
 * modify its behavior.
 *
 * The enumerators in this enumeration can be bitwise-OR'd together to
 * provide multiple options to \c clang_codeCompleteAt().
]]
clang.CodeComplete_Flags = {
  --[[
   * Whether to include macros within the set of code
   * completions returned.
  ]]
  IncludeMacros = 1,
  --[[
   * Whether to include code patterns for language constructs
   * within the set of code completions, e.g., for loops.
  ]]
  IncludeCodePatterns = 2,
  --[[
   * Whether to include brief documentation within the set of code
   * completions returned.
  ]]
  IncludeBriefComments = 4,
  --[[
   * Whether to speed up completion by omitting top- or namespace-level entities
   * defined in the preamble. There's no guarantee any particular entity is
   * omitted. This may be useful if the headers are indexed externally.
  ]]
  SkipPreamble = 8,
  --[[
   * Whether to include completions with small
   * fix-its, e.g. change '.' to '->' on member access, etc.
  ]]
  IncludeCompletionsWithFixIts = 16,
}

clang.EvalResultKind = {
  Int = 1,
  Float = 2,
  ObjCStrLiteral = 3,
  StrLiteral = 4,
  CFStr = 5,
  Other = 6,
  UnExposed = 0,
}

--[[
 \defgroup CINDEX_HIGH Higher level API functions
 *
 * @{
]]
clang.VisitorResult = {
  Break = 0,
  Continue = 1,
}

clang.Result = {
  --[[* Function returned successfully.]]
  Success = 0,
  --[[* One of the parameters was invalid for the function.]]
  Invalid = 1,
  --[[
   * The function was terminated by a callback (e.g. it returned
   * CXVisit_Break)
  ]]
  VisitBreak = 2,
}

clang.IdxEntityKind = {
  Unexposed = 0,
  Typedef = 1,
  Function = 2,
  Variable = 3,
  Field = 4,
  EnumConstant = 5,
  ObjCClass = 6,
  ObjCProtocol = 7,
  ObjCCategory = 8,
  ObjCInstanceMethod = 9,
  ObjCClassMethod = 10,
  ObjCProperty = 11,
  ObjCIvar = 12,
  Enum = 13,
  Struct = 14,
  Union = 15,
  CXXClass = 16,
  CXXNamespace = 17,
  CXXNamespaceAlias = 18,
  CXXStaticVariable = 19,
  CXXStaticMethod = 20,
  CXXInstanceMethod = 21,
  CXXConstructor = 22,
  CXXDestructor = 23,
  CXXConversionFunction = 24,
  CXXTypeAlias = 25,
  CXXInterface = 26,
}

clang.IdxEntityLanguage = {
  None = 0,
  C = 1,
  ObjC = 2,
  CXX = 3,
  Swift = 4,
}

clang.IdxEntityCXXTemplateKind = {
  NonTemplate = 0,
  Template = 1,
  TemplatePartialSpecialization = 2,
  TemplateSpecialization = 3,
}

clang.IdxAttrKind = {
  Unexposed = 0,
  IBAction = 1,
  IBOutlet = 2,
  IBOutletCollection = 3,
}

clang.IdxDeclInfoFlags = {
  Skipped = 1,
}

clang.IdxObjCContainerKind = {
  ForwardRef = 0,
  Interface = 1,
  Implementation = 2,
}

clang.IdxEntityRefKind = {
  --[[* The entity is referenced directly in user's code.]]
  Direct = 1,
  --[[
   * An implicit reference, e.g. a reference of an Objective-C method
   * via the dot syntax.
  ]]
  Implicit = 2,
}

clang.IndexOptFlags = {
  --[[* Used to indicate that no special indexing options are needed.]]
  None = 0,
  --[[
   * Used to indicate that IndexerCallbacks#indexEntityReference should
   * be invoked for only one reference of an entity per source file that does
   * not also include a declaration/definition of the entity.
  ]]
  SuppressRedundantRefs = 1,
  --[[
   * Function-local symbols should be indexed. If this is not set
   * function-local symbols will be ignored.
  ]]
  IndexFunctionLocalSymbols = 2,
  --[[
   * Implicit function/class template instantiations should be indexed.
   * If this is not set, implicit instantiations will be ignored.
  ]]
  IndexImplicitTemplateInstantiations = 4,
  --[[* Suppress all compiler warnings when parsing for indexing.]]
  SuppressWarnings = 8,
  --[[
   * Skip a function/method body that was already parsed during an
   * indexing session associated with a \c CXIndexAction object.
   * Bodies in system headers are always skipped.
  ]]
  SkipParsedBodiesInSession = 16,
}

return clang
