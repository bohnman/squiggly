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
    | fieldGroup nestedExpression
    | fieldGroup keyValueFieldArgChain
    | fieldGroup keyValueFieldArgChain nestedExpression
    | dottedField nestedExpression?
    | dottedField keyValueFieldArgChain (nestedExpression | (Dot dottedField nestedExpression?))?
    | wildcardDeepField keyValueFieldArgChain?
    ;

negatedExpression
    : Subtract field
    | Subtract dottedField
    ;

nestedExpression
    : SquigglyLeft expressionList? SquigglyRight
    | BracketLeft expressionList? BracketRight
    ;

// Fields

fieldGroup
    : ParenLeft field (Comma field)* ParenRight
    ;

dottedField
    : field (Dot field)*
    ;

field
    : Identifier
    | namedOperator
    | RegexLiteral
    | StringLiteral
    | variable
    | wildcard
    | wildcardField
    ;

wildcardField
   : Identifier wildcard
   | Identifier (wildcard Identifier)+ wildcard?
   | wildcard Identifier
   | wildcard (Identifier wildcard)+ Identifier?
   ;

wildcardDeepField
    : WildcardDeep
    ;

keyValueFieldArgChain
    : Colon (fieldArgChain | assignment) Colon (fieldArgChain| assignment)
    | Colon fieldArgChain
    | Colon assignment
    | assignment
    | continuingFieldArgChain
    ;

fieldArgChain
    : (standaloneFieldArg | function) continuingFieldArgChain?
    ;

continuingFieldArgChain
    :  continuingFieldArgChainLink+
    ;

continuingFieldArgChainLink
    : accessOperator function
    | standaloneFieldArg
    ;

standaloneFieldArg
    : intRange
    | arrayAccessor
    ;


// Assignment
assignment
    : Equals arg
    ;


arrayAccessor
    : BracketLeft (Add | Subtract)? IntegerLiteral BracketRight
    ;

// Functions


functionAccessor
    : accessOperator function
    ;


function
    : functionName ParenLeft functionParameters? ParenRight
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
    : inclusiveExclusiveIntRange
    | inclusiveInclusiveIntRange
    ;

inclusiveExclusiveIntRange
    : BracketLeft intRangeArg Colon intRangeArg? BracketRight
    ;

inclusiveInclusiveIntRange
    : BracketLeft intRangeArg DotDot intRangeArg? BracketRight
    ;

intRangeArg
    : (Add | Subtract)? IntegerLiteral
    | variable
    ;

// Closures

lambda
    : lambdaArg Lambda lambdaBody
    | ParenLeft (lambdaArg (Comma lambdaArg)*)? ParenRight Lambda lambdaBody
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
    | arg (EqualsEquals | EqualsName | EqualsNot | EqualsNotSql | EqualsNotName | Match | MatchName | MatchNot | MatchNotName) arg
    | arg (And | AndName) arg
    | arg (Or | OrName) arg
    | argGroupStart arg argGroupEnd argChainLink*
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
    : ParenRight
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
    : (AtDot | AtDotSafe)? (Identifier | function)
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

wildcard
    : WildcardShallow
    | QuestionMark
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
Dollar: '$';
Dot: '.';
DotDot: '..';
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
ParenRight: ')';
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


Identifier
    : IdentifierFirst (IdentifierRest)*
    ;

// Variables

Variable
    : Dollar Identifier
    | Dollar StringLiteral
    ;

// Whitespace and Comments

Whitespace
    : [ \t\n\r]+ -> skip
    ;


//-----------------------------------------------------------------------------
// Lexer Fragments
//-----------------------------------------------------------------------------

fragment IdentifierFirst
    : [a-zA-Z_]
    ;

fragment IdentifierRest
    : [a-zA-Z_0-9]
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