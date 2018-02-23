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
    | fieldList fieldFunctionChain? (nestedExpression|emptyNestedExpression)
    | dottedField fieldFunctionChain? (nestedExpression|emptyNestedExpression)?
    | field fieldFunctionChain?
    | wildcardDeepField fieldFunctionChain?
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
    | ('+' | '-')? IntegerLiteral
    | StringLiteral
    | variable
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

fieldFunctionChain
    : keyFunctionChain ':' valueFunctionChain
    | keyFunctionChain
    | functionSeparator valueFunctionChain
    ;

keyFunctionChain
    : ':' functionChain
    ;

valueFunctionChain
    : functionChain
    ;

functionChain
    : function (functionWithSeparator)*
    ;

functionWithSeparator
    : functionSeparator function
    ;

functionSeparator
    : '.'
    | '?.'
    ;

function
    : functionName LeftParen functionParameters? RightParen
    ;

functionName
    : Identifier
    | binaryNamedOperator
    | prefixSymboledOperator
    ;

functionParameters
    : functionParameter (',' functionParameter)*
    ;

functionParameter
    : arg
    ;

intRange
    : '[' intRangeArg ':' intRangeArg? ']'
    ;

intRangeArg
    : ('+' | '-') IntegerLiteral
    | variable
    ;

// Closures

closure
    : variable '->' closureBody
    | '(' variable (',' variable)* ')' '->' closureBody
    ;

closureBody
    : arg
    ;

arg
    : literal
    | closure
    | argChain
    | variable
    | intRange
    | prefixOperator arg
    | arg binaryOperator arg
    | argGroupStart arg argGroupEnd
    ;

literal
    : BooleanLiteral
    | ('+' | '-')? FloatLiteral
    | ('+' | '-')? IntegerLiteral
    | RegexLiteral
    | StringLiteral
    ;

argChain
    : (literal | intRange) (functionSeparator functionChain)?
    | variable (functionSeparator functionChain)?
    | variable propertyChain (functionSeparator functionChain)?
    | propertyChain (functionSeparator functionChain)?
    | directionalPropertyChain
    | functionChain
    ;

argGroupStart
    : '('
    ;

argGroupEnd
    : ')'
    ;


binaryOperator
    : binaryNamedOperator
    | binarySymboledOperator
    ;

// Operators
binaryNamedOperator
    : 'add'
    | 'sub'
    | 'mul'
    | 'div'
    | 'mod'
    | 'eq'
    | 'ne'
    | 'lt'
    | 'lte'
    | 'gt'
    | 'gte'
    | 'match'
    | 'nmatch'
    | 'or'
    | 'and'
    ;

binarySymboledOperator
    : '+'
    | '-'
    | '*'
    | '/'
    | '%'
    | '=='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
    | '=~'
    | '!~'
    | '||'
    | '&&'
    ;



prefixOperator
    : prefixNamedOperator
    | prefixSymboledOperator
    ;

prefixNamedOperator
    : 'not'
    ;

prefixSymboledOperator
    : '!'
    ;



directionalPropertyChain
    : propertySortDirection propertyChain
    ;

propertyChain
    : initialPropertyAccessor propertyAccessor*
    ;

initialPropertyAccessor
    : ('@.')? Identifier
    | '@[' StringLiteral | Variable ']'
    | '@'
    ;

propertySortDirection
    : '+'
    | '-'
    ;

propertyAccessor
    : '.' Identifier
    | '[' StringLiteral ']'
    ;

variable
    : Variable
    ;

//-----------------------------------------------------------------------------
// Lexer Tokens
//-----------------------------------------------------------------------------

// Literals

BooleanLiteral
    : 'true'
    | 'false'
    ;


IntegerLiteral
    : IntegerNumeral
    ;

FloatLiteral
    : FloatNumeral
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

Identifier
    : IdentifierFirst (IdentifierRest)*
    ;

// Variables

Variable
    : '@' Identifier
    | '@' StringLiteral
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
    : IntegerNumeral '.' Digit+
    | '.' Digit+
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