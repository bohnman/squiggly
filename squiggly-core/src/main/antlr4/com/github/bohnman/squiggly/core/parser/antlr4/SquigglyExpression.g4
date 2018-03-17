grammar SquigglyExpression;

//-----------------------------------------------------------------------------
//region Parser Rules
//-----------------------------------------------------------------------------
nodeFilter
    : nodeExpressionList (Pipe nodeExpressionList)* EOF
    ;

propertyFilter
    : expressionList EOF
    ;

//region Expressions
expressionList
    : expression (Comma expression)*
    ;

nodeExpressionList
    : expressionList
    | topLevelExpression
    ;

expression
    : negatedExpression
    | fieldGroupExpression
    | dottedFieldExpression
    | recursiveFieldExpression
    ;

dottedFieldExpression
    : dottedField nestedExpression?
    | dottedField keyValueFieldArgChain (nestedExpression | (Dot dottedField nestedExpression?))?
    ;

fieldGroupExpression
    : fieldGroup nestedExpression
    | fieldGroup keyValueFieldArgChain
    | fieldGroup keyValueFieldArgChain nestedExpression
    ;

negatedExpression
    : Subtract field
    | Subtract dottedField
    ;

nestedExpression
    : SquigglyLeft expressionList? SquigglyRight
    | BracketLeft expressionList? BracketRight
    ;

recursiveFieldExpression
    : recursiveField keyValueFieldArgChain?
    ;

topLevelExpression
    : Dollar topLevelArgChain?
    ;

topLevelArgChain
    : assignment
    | argChainLink+
    ;
//endregion

//region Fields
dottedField
    : field (Dot field)*
    ;

field
    : Identifier
    | namedSymbol
    | RegexLiteral
    | StringLiteral
    | variable
    | wildcard
    | wildcardField
    ;

fieldGroup
    : ParenLeft field (Comma field)* ParenRight
    ;

recursiveField
    : WildcardShallow recursiveDepth
    | WildcardDeep
    ;

recursiveDepth
    : SquigglyLeft recursiveDepthArg SquigglyRight
    | SquigglyLeft recursiveDepthArg Colon recursiveDepth? SquigglyRight
    | QuestionMark
    | WildcardShallow
    ;

recursiveDepthArg
    : IntegerLiteral
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
//endregion

//region Field Arguments
continuingFieldArgChain
    :  continuingFieldArgChainLink+
    ;

continuingFieldArgChainLink
    : accessOperator function
    | standaloneFieldArg
    ;

fieldArgChain
    : (standaloneFieldArg | function) continuingFieldArgChain?
    ;

keyValueFieldArgChain
    : Colon (fieldArgChain | assignment) Colon (fieldArgChain| assignment)
    | Colon fieldArgChain
    | Colon assignment
    | assignment
    | continuingFieldArgChain
    ;

standaloneFieldArg
    : intRange
    | arrayAccessor
    ;
//endregion


//region Arrays
arrayAccessor
    : BracketLeft (Add | Subtract)? IntegerLiteral BracketRight
    ;

arrayDeclaration
    : BracketLeft (arg (Comma arg)*)? BracketRight
    ;
//endregion

//region Assignment
assignment
    : Equals arg
    | AssignSelf arg
    | AddAssign arg
    | SubtractAssign arg
    | MultiplyAssign arg
    | DivideAssign arg
    | ModulusAssign arg
    ;
//endregion

//region Functions
functionAccessor
    : accessOperator function
    ;

function
    : functionName ParenLeft (arg (Comma arg)*)? ParenRight
    ;

functionName
    : Identifier
    | namedSymbol
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
    | argGroupStart arg argGroupEnd argChainLink*
    ;

argChain
    : (arrayDeclaration | objectDeclaration | literal | intRange | variable | initialPropertyAccessor | function) argChainLink*
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
    : SquigglyLeft (objectKeyValue (Comma objectKeyValue)*)? SquigglyRight
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
    : inclusiveExclusiveIntRange
    | inclusiveInclusiveIntRange
    ;

inclusiveExclusiveIntRange
    : BracketLeft intRangeArg? Colon intRangeArg? BracketRight
    ;

inclusiveInclusiveIntRange
    : BracketLeft intRangeArg? DotDot intRangeArg? BracketRight
    ;

intRangeArg
    : (Add | Subtract)? IntegerLiteral
    | variable
    ;

//endregion


//region Lambdas

lambda
    : lambdaArg Lambda lambdaBody
    | ParenLeft (lambdaArg (Comma lambdaArg)*)? ParenRight Lambda lambdaBody
    | lambdaArg? Lambda lambdaBody
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
    | SafeNavigation
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
    : (Dollar QuestionMark? Dot)? Identifier
    | (Dollar QuestionMark? BracketLeft) (StringLiteral | variable) BracketRight
    | Dollar
    ;

propertySortDirection
    : Add
    | Subtract
    ;

propertyAccessor
    : accessOperator Identifier
    | (BracketLeft | BracketLeftSafe) (StringLiteral | variable) BracketRight
    | arrayAccessor
    | intRange
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
AddAssign: '+=';
And: '&&';
AngleLeft: '<';
AngleRight: '>';
AssignSelf: '.=';
At: '@';
Backtick: '`';
BracketLeft: '[';
BracketLeftSafe: '?[';
BracketRight: ']';
Colon: ':';
Comma: ',';
Dollar: '$';
Dot: '.';
DotDot: '..';
DivideAssign: '/=';
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
Lambda: '->';
LessThanEquals: '<=';
Match: '=~';
MatchNot: '!~';
Modulus: '%';
ModulusAssign: '%=';
MultiplyAssign: '*=';
Not: '!';
Null: 'null';
ParenLeft: '(';
ParenRight: ')';
Pipe: '|';
QuestionMark: '?';
QuoteSingle: '\'';
QuoteDouble: '"';
SafeNavigation: '?.';
SlashForward: '/';
Subtract: '-';
SubtractAssign: '-=';
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
    ;


Identifier
    : IdentifierFirst (IdentifierRest)*
    ;
//endregion

//region Variables
Variable
    : Dollar Identifier
    | Dollar SquigglyLeft Identifier SquigglyRight
    | Dollar SquigglyLeft SquigglyString+ SquigglyRight
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
    | [1-9] Digit? Digit? (',' Digit Digit Digit)+
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
    : 'i'
    ;
//endregion


//region Strings
fragment SquigglyString
    : (~[{}\\ \t\n\r] | StringEscape)
    | (~[{}\\ \t\n\r] | StringEscape) (~[{}\\ \t\n\r] | StringEscape)
    | (~[{}\\ \t\n\r] | StringEscape) (~[{}\\] | StringEscape)+ (~[{}\\ \t\n\r] | StringEscape)
    ;

fragment BacktickQuotedStringCharacters
    :    ~[`\\]
    |    StringEscape
    ;

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
//endregion

//endregion