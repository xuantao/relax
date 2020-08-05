
gcc -c -I F:/tbox/lua-5.3/include -I C:/LLVM/include luaclang-parser.cpp -o luaclang-parser.o
g++ -shared -L F:/tbox/lua-5.3/bin -llua53 -L C:/LLVM/bin -llibclang -o luaclang-parser.dll luaclang-parser.o 

copy /B /Y luaclang-parser.dll F:\tbox\lua-5.3\lib\lua\5.3\luaclang-parser.dll
del luaclang-parser.dll
