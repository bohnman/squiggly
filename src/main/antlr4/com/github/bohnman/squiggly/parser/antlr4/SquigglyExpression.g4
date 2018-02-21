grammar SquigglyExpression;

//-----------------------------------------------------------------------------
// Parser Rules
//-----------------------------------------------------------------------------

parse
    : expressionList EOF
    ;

// Expressions

expressionList
    : expression (',' expression)*
    ;

expression
    : negatedExpression
    | fieldList valueFunctionChain? (nestedExpression|emptyNestedExpression)
    | dottedField (nestedExpression|emptyNestedExpression|valueFunctionChain)
    | dottedField
    | field valueFunctionChain?
    | wildcardDeepField
    ;

negatedExpression
    : '-' field
    | '-' dottedField
    ;

nestedExpression
    : LeftSquiggly expressionList RightSquiggly
    | LeftBrace expressionList RightBrace
    ;

emptyNestedExpression
    : LeftSquiggly RightSquiggly
    | LeftBrace RightBrace
    ;


// Fields

fieldList
    : '(' field (('|'|',') field)* ')'
    | field
    ;


dottedField
    : field ('.' field)+
    ;




field
    : Identifier
    | RegexLiteral
    | IntegerLiteral
    | StringLiteral
    | Variable
    | WildcardLiteral
    | wildcardField
    ;

wildcardField
   : Identifier WildcardLiteral
   | Identifier (WildcardLiteral Identifier)+ WildcardLiteral?
   | WildcardLiteral Identifier
   | WildcardLiteral (Identifier WildcardLiteral)+ Identifier?
   ;

wildcardDeepField
    : WildcardDeep
    ;

// Functions

valueFunctionChain
    : ('.' function)+
    ;

function
    : functionName LeftParen functionParameters? RightParen
    ;

functionName
    : Identifier
    ;

functionParameters
    : functionParameter (',' functionParameter)*
    ;

functionParameter
    : BooleanLiteral
    | IntegerLiteral
    | FloatLiteral
    | RegexLiteral
    | StringLiteral
    | Variable
    ;


//-----------------------------------------------------------------------------
// Lexer Tokens
//-----------------------------------------------------------------------------

Identifier
    : IdentifierFirst (IdentifierRest)*
    ;

// Variables

Variable
    : '@' Identifier
    ;

// Literals

BooleanLiteral
    : 'true'
    | 'false'
    ;


IntegerLiteral
    : ('-' | '+')? IntegerNumeral
    ;

FloatLiteral
    : ('-' | '+')? FloatNumeral
    ;

RegexLiteral
    : '/' RegexChar+ '/' RegexFlag*
    | '~' RegexChar+ '~' RegexFlag*
    ;

StringLiteral
    : '"' DoubleQuotedStringCharacters* '"'
    | '\'' SingleQuotedStringCharacters* '\''
    ;

WildcardLiteral
    : WildcardShallow
    | '?'
    ;

// Other Tokesn

LeftParen
    : '('
    ;

RightParen
    : ')'
    ;

LeftBrace
    : '['
    ;

RightBrace
    : ']'
    ;

LeftSquiggly
    : '{'
    ;

RightSquiggly
    : '}'
    ;


WildcardShallow
    : '*'
    ;

WildcardDeep
    : '**'
    ;


// Whitespace and Comments

Whitespace
    : [ \t\n\r]+ -> skip
    ;

//-----------------------------------------------------------------------------
// Lexer Fragments
//-----------------------------------------------------------------------------

fragment IdentifierFirst
    : [a-zA-Z$_]
    ;

fragment IdentifierRest
    : [a-zA-Z$_0-9]
    ;

// Numbers

fragment Digit : [0-9];

fragment IntegerNumeral
    : '0'
    | [1-9] Digit*
    | [1-9] Digit? Digit? (',' Digit Digit Digit)+
    ;

fragment FloatNumeral
    : IntegerNumeral '.' Digit* ExponentPart?
    | '.' Digit+ ExponentPart?
    | IntegerNumeral ExponentPart?
    ;

fragment ExponentPart
    : [eE] [+-]? Digit+
    ;

// Regex

fragment RegexChar
    : ~[/]
    ;

fragment RegexEscape
    : '\\' [/]
    ;

fragment RegexFlag
    : 'i'
    ;


// Strings

fragment DoubleQuotedStringCharacters
    :    ~["\\]
    |    StringEscape
    ;


fragment SingleQuotedStringCharacters
    :    ~['\\]
    |    StringEscape
    ;


fragment StringEscape
    :    '\\' ["'\\]
    ;