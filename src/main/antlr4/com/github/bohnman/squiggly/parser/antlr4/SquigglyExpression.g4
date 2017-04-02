grammar SquigglyExpression;

// Parser Rules ---------------------------------------------------------------

parse
    : expression (',' expression)* EOF
    ;

expression
    : negated_expression
    | nested_expression
    | field
    | deep
    ;

negated_expression
    : '-' field
    ;

nested_expression
    : field ('|' field)* LSQUIGGLY expression (',' expression)* RSQUIGGLY
    ;

deep
    : WILDCARD_DEEP
    ;


field
    : IDENTIFIER
    | IDENTIFIER WILDCARD_SHALLOW
    | IDENTIFIER (WILDCARD_SHALLOW IDENTIFIER)+ WILDCARD_SHALLOW?
    | WILDCARD_SHALLOW IDENTIFIER
    | WILDCARD_SHALLOW (IDENTIFIER WILDCARD_SHALLOW)+ IDENTIFIER?
    | WILDCARD_SHALLOW
    ;


// Lexer Tokens ---------------------------------------------------------------

fragment DIGIT
    : [0-9]
    ;

fragment LETTER
    : [a-zA-Z]
    ;

fragment VALID_FIELD_CHAR
    : (LETTER | DIGIT | '$' | '_')
    ;

LSQUIGGLY
    : '{'
    ;

RSQUIGGLY
    : '}'
    ;

IDENTIFIER
    : VALID_FIELD_CHAR (VALID_FIELD_CHAR)*
    ;

WILDCARD_SHALLOW
    : '*'
    ;

WILDCARD_DEEP
    : '**'
    ;

