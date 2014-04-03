/**
 * @file   parser.yy
 *
 * @author
 * @date
 *
 * @brief  Bison Grammar file
 *
 *
 */

%skeleton "lalr1.cc"   /*  -*- C++ -*- */
%require "2.1a"
%defines
%define "parser_class_name" "Parser"

%{
#include <string>
namespace LibConfigParser
{
    class ConfigParser;
}
%}

%parse-param {LibConfigParser::ConfigParser& config}
%lex-param   {LibConfigParser::ConfigParser& config}
%locations
%initial-action
{
  // Initialize the initial location.
  @$.begin.filename = @$.end.filename = &config.m_file;
};

//Use the two following directives to enable parser tracing
//and verbose error messages.
%debug
%error-verbose

 // Symbols.
%union
 {
     int val_int;
     double val_float;
     std::string *val_string;
     bool val_bool;
};

%{
#include "configparser.hpp"
#include "record.hpp"
#include "list.hpp"
#include "item.hpp"
using namespace LibConfigParser;
%}

%token END  0 "end of file"
%token <val_int> VAL_INT
%token <val_float> VAL_FLOAT
%token <val_bool> VAL_BOOL
%token <val_string> VAL_STRING

%token <val_string> TK_ID

%destructor { delete $$; } VAL_STRING

%%
 // grammar rules begin
 // start from the configuration section
%start configuration;


configuration: /* empty */{
    // Create the root node
    config.setRoot(new Record("Root", 0));
    config.setCurrentNode(config.root());
} definitions
;


definitions: definitions definition
|
;

definition: item
| named_list
| named_record
;

item: TK_ID '=' {
    Item* it = new Item(*$1);
    if (config.currentType() != Node::Record_Node)
    {
        config.error(yylloc, "Must be a record!");
    }
    config.appendToCurrentNode(it);
    config.setCurrentNode(it);
 }  basic_value ';' {
     config.popCurrentNode();
    }
;

basic_value: VAL_INT {
    if (config.currentType() == Node::Item_Node)
    {
        static_cast<Item*>(config.currentNode())->setValue($1);
    }
    else if (config.currentType() == Node::List_Node)
    {
        static_cast<List*>(config.currentNode())->appendChild($1);
    }
    else
    {
        config.error(yylloc, "Fatal Error!");
    }
 }
| VAL_FLOAT {
    if (config.currentType() == Node::Item_Node)
    {
        static_cast<Item*>(config.currentNode())->setValue($1);
    }
    else if (config.currentType() == Node::List_Node)
    {
        static_cast<List*>(config.currentNode())->appendChild($1);;
    }
    else
    {
        config.error(yylloc, "Fatal Error!");
    }
 }
| VAL_BOOL {
    if (config.currentType() == Node::Item_Node)
    {
        static_cast<Item*>(config.currentNode())->setValue($1);
    }
    else if (config.currentType() == Node::List_Node)
    {
        static_cast<List*>(config.currentNode())->appendChild($1);;
    }
    else
    {
        config.error(yylloc, "Fatal Error!");
    }
 }
| VAL_STRING {
    if (config.currentType() == Node::Item_Node)
    {
        static_cast<Item*>(config.currentNode())->setValue(*$1);
    }
    else if (config.currentType() == Node::List_Node)
    {
        static_cast<List*>(config.currentNode())->appendChild(*$1);;
    }
    else
    {
        config.error(yylloc, "Fatal Error!");
    }
 }
;

named_list: TK_ID '=' '[' {
    List* list = new List(*$1);
    config.appendToCurrentNode(list);
    config.setCurrentNode(list);
 } list_nodes ']' ';'
;

list_nodes: values_list
| lists_list
| records_list
;

values_list: basic_value
| values_list ',' basic_value
;

lists_list: list
| lists_list ',' list
;

// unnamed list
list: '[' {
    if (config.currentType() == Node::List_Node)
    {
        List* list = new List("");
        config.appendToCurrentNode(list);
    }
    else
    {
        config.error(yylloc, "Unamed List must be in a list.");
    }
 } list_nodes ']'{
     config.popCurrentNode();
   }
;

records_list: record
| records_list ',' record
;

// unnamed record
record: '{' {
    if (config.currentType() == Node::List_Node)
    {
        Record* rec = new Record("");
        config.appendToCurrentNode(rec);
        config.setCurrentNode(rec);
    }
    else
    {
        config.error(yylloc, "Unamed List must be in a list.");
    }
 } definitions '}' ';'{
     config.popCurrentNode();
   }
;

named_record: TK_ID '{' {
    Record* rec = new Record(*$1);
    if (config.currentType() == Node::Record_Node)
    {
        config.appendToCurrentNode(rec);
    }
    else
    {
        config.error(yylloc, "Fatal Error!");
    }
    config.setCurrentNode(rec);
 } definitions '}' ';'
;

 // grammar rules end
%%

//      Finally the error member function registers the errors to the parser.
// this function must be supplied by user


void yy::Parser::error(const yy::Parser::location_type& l,
                       const std::string& m)
{
    config.error(l, m);
}
