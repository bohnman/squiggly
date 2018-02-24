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
    | namedOperator
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
    : function (argChainLink)*
    ;

functionAccessor
    : accessOperator function
    ;


function
    : functionName ParenLeft functionParameters? ParentRight
    ;

functionName
    : Identifier
    | namedOperator
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
    : argChain
    | lambda
    | (NotName | Not) arg
    | arg (WildcardShallow | MultiplyName | SlashForward | DivideName | Modulus | ModulusName) arg
    | arg (Add | AddName | Subtract | SubtractName) arg
    | arg (AngleLeft | LessThanName | LessThanEquals | LessThanEqualsName | AngleRight | GreaterThanName | GreaterThanEquals | GreaterThanEqualsName) arg
    | arg (Equals | EqualsEquals | EqualsName | EqualsNot | EqualsNotSql | EqualsNotName | Match | MatchName | MatchNot | MatchNotName) arg
    | arg (And | AndName) arg
    | arg (Or | OrName) arg
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
    : (literal | intRange | variable | initialPropertyAccessor | function) argChainLink*
    | propertySortDirection (variable | initialPropertyAccessor | function) argChainLink*
    ;


argChainLink
    : propertyAccessor
    | functionAccessor
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



namedOperator
    : AddName
    | AndName
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
    | NotName
    ;


initialPropertyAccessor
    : (AtDot | AtDotSafe)? Identifier
    | (AtBrackLeft | AtBrackLeftSafe) (StringLiteral | variable) BracketRight
    | At
    ;

propertySortDirection
    : Add
    | Subtract
    ;

propertyAccessor
    : accessOperator Identifier
    | (BracketLeft | BracketLeftSafe) (StringLiteral | variable) BracketRight
    ;

variable
    : Variable
    ;

//-----------------------------------------------------------------------------
// Lexer Tokens
//-----------------------------------------------------------------------------

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
AtBrackLeftSafe: '@?[';
AtDot: '@.';
AtDotSafe: '@?.';
BracketLeft: '[';
BracketLeftSafe: '?[';
BracketRight: ']';
Colon: ':';
Comma: ',';
Dot: '.';
Equals: '=';
EqualsEquals: '==';
EqualsNot: '!=';
EqualsNotSql: '<>';
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