grammar SquigglyGrammar;

//-----------------------------------------------------------------------------
//region Parser Rules
//-----------------------------------------------------------------------------

// region Filter Chains
nodeFilter
    : nodeStatement (statementSeperator nodeStatement)* EOF
    ;

propertyFilter
    : propertyStatement EOF
    ;

statementSeperator
    : Pipe
    | SemiColon
    ;

// endregion

//region Statements
propertyStatement
    : expressionList
    ;

nodeStatement
    : expressionList
    | topLevelExpression
    ;
// endregion

// region Expressions
expressionList
    : expression (Comma expression)*
    ;


expression
    : negatedExpression
    | fieldGroupExpression
    | dottedFieldExpression
    | deepExpression
    | deepInheritExpression
    ;

deepExpression
    : WildcardDeep (ParenLeft deepRange ParenRight)? (ParenLeft deepArg (Comma deepArg)*  ParenRight)?
    ;

deepInheritExpression
    : Inherit
    ;

deepArg
    : (fieldGroup | field) keyValueFieldArgChain?
    | Subtract? field
    ;

deepRange
    : IntegerLiteral? intRangeOp IntegerLiteral?
    ;

dottedFieldExpression
    : dottedField keyValueFieldArgChain? nestedExpression?
    ;

fieldGroupExpression
    : fieldGroup nestedExpression
    | fieldGroup keyValueFieldArgChain nestedExpression?
    ;

negatedExpression
    : Subtract field
    | Subtract dottedField
    ;

nestedExpression
    : ParenLeft expressionList? ParenRight
    | BracketLeft expressionList? BracketRight
    | SquigglyLeft expressionList? SquigglyRight
    ;

topLevelExpression
    : Dollar (assignment | argChainLink+)?
    ;

//endregion

//region Fields
dottedField
    : field (Dot field)*
    ;

exactField
    : Identifier
    | namedSymbol
    | exactField (Subtract | exactField)+
    ;

field
    : exactField
    | RegexLiteral
    | StringLiteral
    | wildcardField
    ;

fieldGroup
    : ParenLeft field (Comma field)* ParenRight
    ;

wildcardField
    : exactField wildcard
    | exactField (wildcard exactField)+ wildcard?
    | wildcard exactField
    | wildcard (exactField wildcard)+ exactField?
    | wildcard
    ;

//endregion

//region Field Arguments

keyValueFieldArgChain
    : Colon keyValueFieldArgChainLink Colon keyValueFieldArgChainLink
    | Colon? keyValueFieldArgChainLink
    ;

keyValueFieldArgChainLink
    : Dot? function argChainLink*
    | assignment
    ;


//endregion

//region Arrays
arrayDeclaration
    : At ParenLeft intRange ParenRight
    | At ParenLeft (arg (Comma arg)*)? ParenRight
    ;
//endregion

//region Assignment
assignment
    : Equals arg?
    ;
//endregion

//region Functions
functionAccessor
    : accessOperator? function
    ;

function
    : functionName ParenLeft (arg (Comma arg)*)? ParenRight
    ;

functionName
    : At Identifier
    | At namedSymbol
    | At
    ;
//endregion

//region Function Arguments
arg
    : argChain
    | Null
    | lambda
    | (NotName | Not) arg
    | arg (WildcardShallow | MultiplyName | SlashForward | DivideName | Modulus | ModulusName) arg
    | arg (Add | AddName | Subtract | SubtractName) arg
    | arg (Elvis) arg
    | arg (AngleLeft | LessThanName | LessThanEquals | LessThanEqualsName | AngleRight | GreaterThanName | GreaterThanEquals | GreaterThanEqualsName) arg
    | arg (EqualsEquals | EqualsName | EqualsNot | EqualsNotSql | EqualsNotName | Match | MatchName | MatchNot | MatchNotName) arg
    | arg (And | AndName) arg
    | arg (Or | OrName) arg
    | ifArg
    | ParenLeft arg ParenRight argChainLink*
    ;

argChain
    : (arrayDeclaration | objectDeclaration | literal | intRange | variable | initialPropertyAccessor | function) argChainLink*
    | propertySortDirection (variable | initialPropertyAccessor | function) argChainLink*
    ;

argChainLink
    : propertyAccessor
    | functionAccessor
    ;


ifArg
    : ifClause elifClause* elseClause? End
    ;

ifClause
    : If arg Then arg
    ;

elifClause
    : Elif arg Then arg
    ;

elseClause
    : Else arg
    ;
//endregion

//region Objects
objectDeclaration
    : '@o' ParenLeft (objectKeyValue (Comma objectKeyValue)*)? ParenRight
    ;

objectKeyValue
    : objectKey Colon objectValue
    ;

objectKey
    : Identifier
    | variable
    | literal
    ;

objectValue
    : arg
    ;
//endregion

//region Ranges
intRange
    : intRangeArg? intRangeOp intRangeArg?
    ;

intRangeArg
    : (Add | Subtract)? IntegerLiteral
    | variable
    ;

intRangeOp
    : DotDot
    | Colon
    ;


//endregion


//region Lambdas

lambda
    : lambdaArg? lambdaOp lambdaBody
    | ParenLeft (lambdaArg (Comma lambdaArg)*)? ParenRight lambdaOp lambdaBody
    ;

lambdaOp
    : Lambda
    | ColonColon
    ;

lambdaBody
    : arg
    ;

lambdaArg
    : variable
    | Underscore
    ;
//endregion

//region Literals

literal
    : BooleanLiteral
    | (Add | Subtract)? FloatLiteral
    | (Add | Subtract)? IntegerLiteral
    | RegexLiteral
    | StringLiteral
    ;

//region Operators

accessOperator
    : Dot
    ;

namedSymbol
    : AddName
    | AndName
    | SubtractName
    | MultiplyName
    | DivideName
    | Elif
    | Else
    | End
    | ModulusName
    | EqualsName
    | EqualsNotName
    | If
    | LessThanName
    | LessThanEqualsName
    | GreaterThanName
    | GreaterThanEqualsName
    | MatchName
    | MatchNotName
    | OrName
    | NotName
    | Then
    ;
//endregion

//region Properties

initialPropertyAccessor
    : (Dollar | DollarDollar | Identifier) function?
    ;

propertySortDirection
    : Add
    | Subtract
    ;

propertyAccessor
    : accessOperator Identifier
    | ParenLeft (StringLiteral | variable | intRange) ParenRight
    ;
//endregion

//region Variables
variable
    : Variable
    ;
//endregion

//region Wildcards
wildcard
    : WildcardShallow
    | QuestionMark
    ;
//endregion

//endregion

//-----------------------------------------------------------------------------
//region Lexer Tokens
//-----------------------------------------------------------------------------

//region Keywords
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
//endregion

//region Symbols
Add: '+';
And: '&&';
AngleLeft: '<';
AngleRight: '>';
At: '@';
Backtick: '`';
BracketLeft: '[';
BracketRight: ']';
Colon: ':';
ColonColon: '::';
Comma: ',';
Dollar: '$';
DollarDollar: '$$';
Dot: '.';
DotDot: '..';
Equals: '=';
EqualsEquals: '==';
EqualsNot: '!=';
EqualsNotSql: '<>';
Elif: 'elif';
Else: 'else';
Elvis: '?:';
End: 'end';
GreaterThanEquals: '>=';
If: 'if';
Inherit: '...';
Lambda: '->';
LessThanEquals: '<=';
Match: '=~';
MatchNot: '!~';
Modulus: '%';
Not: '!';
Null: 'null';
ParenLeft: '(';
ParenRight: ')';
Pound: '#';
Pipe: '|';
QuestionMark: '?';
QuoteSingle: '\'';
QuoteDouble: '"';
SemiColon: ';';
SlashForward: '/';
Subtract: '-';
SquigglyLeft: '{';
SquigglyRight: '}';
Then: 'then';
Tilde: '~';
Underscore: '_';
WildcardShallow: '*';
WildcardDeep: '**';
Or: '||';
//endregion


//region Literals
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
    | Backtick BacktickQuotedStringCharacters* Backtick
    | '@s' ParenLeft ParentRightQuotedStringCharacters* ParenRight
    ;


Identifier
    : IdentifierFirst (IdentifierRest)*
    ;
//endregion

//region Variables
Variable
    : Dollar Identifier
    | Dollar ParenLeft Identifier ParenRight
    | Dollar ParenLeft SquigglyString+ ParenRight
    ;
//endregion

//region Whitespace and Comments
Whitespace
    : [ \t\n\r]+ -> skip
    ;
//endregion

//endregion

//-----------------------------------------------------------------------------
//region Lexer Fragments
//-----------------------------------------------------------------------------

//region Identifiers
fragment IdentifierFirst
    : [a-zA-Z_]
    ;

fragment IdentifierRest
    : [a-zA-Z_0-9]
    ;
//endregion

//region Numbers
fragment Digit : [0-9];


fragment IntegerNumeral
    : '0'
    | [1-9] Digit*
    | [1-9] Digit? Digit? ('_' Digit Digit Digit)+
    ;

fragment FloatNumeral
    : IntegerNumeral '.' Digit+
    | '.' Digit+
    ;
//endregion


//region Regex
fragment RegexChar
    : ~[/]
    ;

fragment RegexEscape
    : '\\' [/]
    ;

fragment RegexFlag
    : [a-zA-Z]
    ;
//endregion


//region Strings
fragment SquigglyString
    : (~[()\\ \t\n\r] | StringEscape)
    | (~[()\\ \t\n\r] | StringEscape) (~[()\\ \t\n\r] | StringEscape)
    | (~[()\\ \t\n\r] | StringEscape) (~[()\\] | StringEscape)+ (~[()\\ \t\n\r] | StringEscape)
    ;

fragment BacktickQuotedStringCharacters
    :    ~[`\\]
    |    StringEscape
    ;

fragment DoubleQuotedStringCharacters
    :    ~["\\]
    |    StringEscape
    ;

fragment ParentRightQuotedStringCharacters
    :    ~[)\\]
    |    StringEscape
    ;

fragment SingleQuotedStringCharacters
    :    ~['\\]
    |    StringEscape
    ;

fragment StringEscape
    :    '\\' ["'\\()]
    ;
//endregion

//endregion