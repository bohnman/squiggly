grammar SquigglyExpression;

//-----------------------------------------------------------------------------
// Parser Rules
//-----------------------------------------------------------------------------

parse
    : expressionList EOF
    ;

// Expressions

expressionList
    : expression (Comma expression)*
    ;

expression
    : negatedExpression
    | fieldList fieldFunctionChain? (nestedExpression|emptyNestedExpression)
    | dottedField fieldFunctionChain? (nestedExpression|emptyNestedExpression)?
    | field fieldFunctionChain?
    | wildcardDeepField fieldFunctionChain?
    ;

negatedExpression
    : Subtract field
    | Subtract dottedField
    ;

nestedExpression
    : SquigglyLeft expressionList SquigglyRight
    | BracketLeft expressionList BracketRight
    ;

emptyNestedExpression
    : SquigglyLeft SquigglyRight
    | BracketLeft BracketRight
    ;


// Fields

fieldList
    : ParenLeft field ((Pipe|Comma) field)* ParentRight
    | field
    ;

dottedField
    : field (Dot field)+
    ;

field
    : Identifier
    | binaryNamedOperator
    | prefixNamedOperator
    | RegexLiteral
    | (Add | Subtract)? IntegerLiteral
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
    : keyFunctionChain Colon valueFunctionChain
    | keyFunctionChain
    | accessOperator valueFunctionChain
    ;

keyFunctionChain
    : Colon functionChain
    ;

valueFunctionChain
    : functionChain
    ;

functionChain
    : function (functionWithSeparator)*
    ;

functionWithSeparator
    : accessOperator function
    ;


function
    : functionName ParenLeft functionParameters? ParentRight
    ;

functionName
    : Identifier
    | binaryNamedOperator
    | prefixSymboledOperator
    ;

functionParameters
    : functionParameter (Comma functionParameter)*
    ;

functionParameter
    : arg
    ;

intRange
    : BracketLeft intRangeArg Colon intRangeArg? BracketRight
    ;

intRangeArg
    : (Add | Subtract) IntegerLiteral
    | variable
    ;

// Closures

lambda
    : lambdaArg Lambda lambdaBody
    | ParenLeft (lambdaArg (Comma lambdaArg)*)? ParentRight Lambda lambdaBody
    ;

lambdaBody
    : arg
    ;

lambdaArg
    : variable
    | Underscore
    ;

arg
    : literal
    | lambda
    | argChain
    | variable
    | intRange
    | prefixOperator arg
    | arg binaryOperator arg
    | argGroupStart arg argGroupEnd
    ;

literal
    : BooleanLiteral
    | (Add | Subtract)? FloatLiteral
    | (Add | Subtract)? IntegerLiteral
    | RegexLiteral
    | StringLiteral
    ;

argChain
    : (literal | intRange) (accessOperator functionChain)?
    | variable (accessOperator functionChain)?
    | variable propertyChain (accessOperator functionChain)?
    | propertyChain (accessOperator functionChain)?
    | directionalPropertyChain
    | functionChain
    ;


argChainLink
    :
    ;

argGroupStart
    : ParenLeft
    ;

argGroupEnd
    : ParentRight
    ;


// Operators
accessOperator
    : Dot
    | SafeNavigation
    ;

binaryOperator
    : binaryNamedOperator
    | binarySymboledOperator
    ;


binaryNamedOperator
    : AddName
    | SubtractName
    | MultiplyName
    | DivideName
    | ModulusName
    | EqualsName
    | EqualsNotName
    | LessThanName
    | LessThanEqualsName
    | GreaterThanName
    | GreaterThanEqualsName
    | MatchName
    | MatchNotName
    | OrName
    | AndName
    ;

binarySymboledOperator
    : Add
    | Subtract
    | WildcardShallow   // multiply
    | SlashForward      // divide
    | Modulus
    | Equals
    | EqualsNot
    | AngleLeft         // less than
    | LessThanEquals
    | AngleRight        // greater than
    | GreaterThanEquals
    | Match
    | MatchNot
    | Or
    | And
    ;



prefixOperator
    : prefixNamedOperator
    | prefixSymboledOperator
    ;

prefixNamedOperator
    : NotName
    ;

prefixSymboledOperator
    : Not
    ;

directionalPropertyChain
    : propertySortDirection propertyChain
    ;

propertyChain
    : initialPropertyAccessor propertyAccessor*
    ;

initialPropertyAccessor
    : AtDot? Identifier
    | AtBrackLeft StringLiteral | Variable BracketRight
    | At
    ;

propertySortDirection
    : Add
    | Subtract
    ;

propertyAccessor
    : Dot Identifier
    | BracketLeft StringLiteral BracketRight
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
    : SlashForward RegexChar+ SlashForward RegexFlag*
    | Tilde RegexChar+ Tilde RegexFlag*
    ;

StringLiteral
    : QuoteDouble DoubleQuotedStringCharacters* QuoteDouble
    | QuoteSingle SingleQuotedStringCharacters* QuoteSingle
    ;

WildcardLiteral
    : WildcardShallow
    | QuestionMark
    ;

Identifier
    : IdentifierFirst (IdentifierRest)*
    ;

// Variables

Variable
    : At Identifier
    | At StringLiteral
    ;

// Keywords

AddName: 'add';
AndName: 'and';
EqualsName: 'eq';
EqualsNotName: 'ne';
DivideName: 'div';
GreaterThanEqualsName: 'gte';
GreaterThanName: 'gt';
LessThanEqualsName: 'lte';
LessThanName: 'lt';
MatchName: 'match';
MatchNotName: 'nmatch';
ModulusName: 'mod';
MultiplyName: 'mul';
NotName: 'not';
OrName: 'or';
SubtractName: 'sub';

// Symbols

Add: '+';
And: '&&';
AngleLeft: '<';
AngleRight: '>';
At: '@';
AtBrackLeft: '@[';
AtDot: '@.';
BracketLeft: '[';
BracketRight: ']';
Colon: ':';
Comma: ',';
Dot: '.';
Equals: '==';
EqualsNot: '!=';
GreaterThanEquals: '>=';
Lambda: '->';
LessThanEquals: '<=';
Match: '=~';
MatchNot: '!~';
Modulus: '%';
Not: '!';
ParenLeft: '(';
ParentRight: ')';
Pipe: '|';
QuestionMark: '?';
QuoteSingle: '\'';
QuoteDouble: '"';
SafeNavigation: '?.';
SlashForward: '/';
Subtract: '-';
SquigglyLeft: '{';
SquigglyRight: '}';
Tilde: '~';
Underscore: '_';
WildcardShallow: '*';
WildcardDeep: '**';
Or: '||';

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