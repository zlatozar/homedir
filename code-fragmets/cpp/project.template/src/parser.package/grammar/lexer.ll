%{
#include <cstdlib>
#include <errno.h>
#include <limits.h>
#include <string>
#include "configparser.hpp"
#include "parser.hh"

#define yyterminate() return token::END
%}

%{
# define YY_USER_ACTION  yylloc->columns (yyleng);
%}

/* Because there is no #include-like feature we don't need yywrap,
   we don't need unput either, and we parse an actual file,
   this is not an interactive session with the user.
   Finally we enable the scanner tracing features.
 */

%option noyywrap nounput batch debug

DIGIT    [0-9]

INT      [+-]?{DIGIT}+

FLOAT    [+-]?{DIGIT}*\.?{DIGIT}*([eE][+-]?{DIGIT}+)?

BOOLEAN  True|False

STRING   \"[^\n\"]+\"

ID      [a-zA-Z][a-zA-Z0-9_]*

BLANK   [\t ]

PUNCTUATION  [\{\}\[\]=;]

%%

%{
    yylloc->step();
%}


{BLANK}+   yylloc->step();


[\n]+      yylloc->lines(yyleng); yylloc->step();

%{
    typedef yy::Parser::token token;
%}


{INT}  {
    int l;
    l = strtol(yytext, NULL, 10);
    if(errno == ERANGE)
    {
        config.error (*yylloc, "Integer out of range of Int.");
        exit(1);
    }
    yylval->val_int = l;
    return token::VAL_INT;
}

{FLOAT}  {
    double d;
    d = strtod(yytext,NULL);
    if(errno == ERANGE)
    {
        config.error (*yylloc, "Integer out of range of Double.");
        exit(1);
    }
    yylval->val_float = d;
    return token::VAL_FLOAT;
}

{BOOLEAN} {
    if (yytext[0] == 'T')
    {
        yylval->val_bool = true;
    }
    else
    {
        yylval->val_bool = false;
    }
    return token::VAL_BOOL;
}

{ID}  {
    yylval->val_string = new std::string(yytext);
    return token::TK_ID;
}

{STRING}  {
    std::string s(yytext);
    yylval->val_string = new std::string(s.substr(1,yyleng-1));
    return token::VAL_STRING;
}


{PUNCTUATION}  {
    return yy::Parser::token_type(yytext[0]);
}

.  {
    config.error (*yylloc, "invalid character");
}

%%

void LibConfigParser::ConfigParser::scan_begin()
{
    yy_flex_debug = m_trace_scanning;
    if (!(yyin = fopen (m_file.c_str (), "r")))
    {
        error (std::string ("cannot open ") + m_file);
    }
}


void LibConfigParser::ConfigParser::scan_end()
{
    fclose(yyin);
}
