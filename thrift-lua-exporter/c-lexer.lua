-- Lua LPeg lexer for C.
-- Note:
--   Does not handle C preprocessing macros.
--   Not well tested.
-- 
-- David Manura, 2007, public domain.  Based on ANSI C Lex
--   specification in http://www.quut.com/c/ANSI-C-grammar-l-1998.html
--   (Jutta Degener, 2006; Tom Stockfisch, 1987, Jeff Lee, 1985)

local lpeg = require 'lpeg'

local P, R, S, C =
  lpeg.P, lpeg.R, lpeg.S, lpeg.C

local whitespace = S' \t\v\n\f'

local digit = R'09'
local letter = R('az', 'AZ') + P'_'
local alphanum = letter + digit
local hex = R('af', 'AF', '09')
local exp = S'eE' * S'+-'^-1 * digit^1
local fs = S'fFlL'
local is = S'uUlL'^0

local hexnum = P'0' * S'xX' * hex^1 * is^-1
local octnum = P'0' * digit^1 * is^-1
local decnum = digit^1 * is^-1
local floatnum = digit^1 * exp * fs^-1 +
                 digit^0 * P'.' * digit^1 * exp^-1 * fs^-1 +
                 digit^1 * P'.' * digit^0 * exp^-1 * fs^-1
local numlit = hexnum + octnum + floatnum + decnum

local charlit =
  P'L'^-1 * P"'" * (P'\\' * P(1) + (1 - S"\\'"))^1 * P"'"

local stringlit =
  P'L'^-1 * P'"' * (P'\\' * P(1) + (1 - S'\\"'))^0 * P'"'

local ccomment = P'/*' * (1 - P'*/')^0 * P'*/'
local newcomment = P'//' * (1 - P'\n')^0
local comment = (ccomment + newcomment)
              / function(...) print('COMMENT', ...) end

local literal = (numlit + charlit + stringlit)
              / function(...) print('LITERAL', ...) end

local keyword = C(
  P"auto" + 
  P"_Bool" +
  P"break" +
  P"case" +
  P"char" +
  P"_Complex" +
  P"const" +
  P"continue" +
  P"default" +
  P"do" +
  P"double" +
  P"else" +
  P"enum" +
  P"extern" +
  P"float" +
  P"for" +
  P"goto" +
  P"if" +
  P"_Imaginary" +
  P"inline" +
  P"int" +
  P"long" +
  P"register" +
  P"restrict" +
  P"return" +
  P"short" +
  P"signed" +
  P"sizeof" +
  P"static" +
  P"struct" +
  P"switch" +
  P"typedef" +
  P"union" +
  P"unsigned" +
  P"void" +
  P"volatile" +
  P"while"
) / function(...) print('KEYWORD', ...) end

local identifier = (letter * alphanum^0 - keyword * (-alphanum))
                 / function(...) print('ID',...) end

local op = C(
  P"..." +
  P">>=" +
  P"<<=" +
  P"+=" +
  P"-=" +
  P"*=" +
  P"/=" +
  P"%=" +
  P"&=" +
  P"^=" +
  P"|=" +
  P">>" +
  P"<<" +
  P"++" +
  P"--" +
  P"->" +
  P"&&" +
  P"||" +
  P"<=" +
  P">=" +
  P"==" +
  P"!=" +
  P";" +
  P"{" + P"<%" +
  P"}" + P"%>" +
  P"," +
  P":" +
  P"=" +
  P"(" +
  P")" +
  P"[" + P"<:" +
  P"]" + P":>" +
  P"." +
  P"&" +
  P"!" +
  P"~" +
  P"-" +
  P"+" +
  P"*" +
  P"/" +
  P"%" +
  P"<" +
  P">" +
  P"^" +
  P"|" +
  P"?"
) / function(...) print('OP', ...) end

local tokens = (comment + identifier + keyword +
                literal + op + whitespace)^0

-- frontend
local filename = arg[1]
local fh = assert(io.open(filename))
local input = fh:read'*a'
fh:close()
print(lpeg.match(tokens, input))
