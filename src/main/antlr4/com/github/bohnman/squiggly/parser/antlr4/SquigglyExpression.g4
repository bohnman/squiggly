grammar SquigglyExpression;

// Parser Rules ---------------------------------------------------------------

parse
    : expressionList EOF
    ;

expressionList
    : expression (',' expression)*
    ;

expression
    : negatedExpression
    | fieldList (nestedExpression|emptyNestedExpression)
    | dotPath (nestedExpression|emptyNestedExpression)
    | dotPath
    | field
    | deep
    ;

fieldList
    : '(' field (('|'|',') field)* ')'
    | field
    ;

negatedExpression
    : '-' field
    | '-' dotPath
    ;

nestedExpression
    : LSquiggly expressionList RSquiggly
    | LBrace expressionList RBrace
    ;

emptyNestedExpression
    : LSquiggly RSquiggly
    | LBrace RBrace
    ;

deep
    : WildcardDeep
    ;

dotPath
    : field ('.' field)+
    ;

field
    : exactField
    | regexField
    | wildcardShallowField
    | wildcardField
    | variableField
    ;

exactField
    : Identifier
    ;

regexField
    : '~' regexPattern '~' regexFlag*
    | '/' regexPattern '/' regexFlag*
    ;

regexPattern
    : ('.' | '|' | ',' | LSquiggly | RSquiggly | LBrace | RBrace | '-' | RegexChar  | Identifier | WildcardShallow)+
    ;

regexFlag
    : 'i'
    ;

variableField
    : Variable
    ;

wildcardField
   : Identifier wildcardChar
   | Identifier (wildcardChar Identifier)+ wildcardChar?
   | wildcardChar Identifier
   | wildcardChar (Identifier wildcardChar)+ Identifier?
   ;

wildcardChar
   : WildcardShallow
   | '?'
   ;

wildcardShallowField
    : WildcardShallow
    ;


// Lexer Tokens ---------------------------------------------------------------

fragment Digit
    : [0-9]
    ;

fragment Letter
    : [a-zA-Z]
    ;

fragment ValidFieldChar
    : (Letter | Digit | '$' | '_')
    ;

LBrace
    : '['
    ;

RBrace
    : ']'
    ;

LSquiggly
    : '{'
    ;

RSquiggly
    : '}'
    ;

Identifier
    : ValidFieldChar (ValidFieldChar)*
    ;

Variable
    : ':' Identifier
    ;

WildcardShallow
    : '*'
    ;

WildcardDeep
    : '**'
    ;

RegexChar
    : ~('~' | '/')
    ;
