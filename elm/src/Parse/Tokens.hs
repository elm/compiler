
module Tokens where

data Token =
    NUMBER Int | ID String | TYPE String |
    CHAR Char | STRING String |
    LAMBDA | ARROW | OP String | DOT2 | UNDERSCORE |
    LPAREN | RPAREN |
    LBRACE | RBRACE |
    LBRACKET | RBRACKET |
    COMMA | SEMI |
    TRUE | FALSE |
    IF | THEN | ELSE |
    CASE | OF | LET | IN |
    SPACES | NEWLINE | DATA
           deriving (Show, Eq)

