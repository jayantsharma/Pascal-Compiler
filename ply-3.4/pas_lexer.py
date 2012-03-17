import ply.lex as lex 

reserved = {

#turbo pascal
    'absolute' : 'ABSOLUTE',
    'and' : 'AND',
    'array' : 'ARRAY',
    'asm'   : 'ASM',
    'begin' : 'BEGIN',
    'case'  : 'CASE',
    'char' : 'CHAR',
    'clrscr' : 'CLRSCR',
    'const' : 'CONST',
    'constructor' : 'CONSTRUCTOR',
    'delay' : 'DELAY',
    'destructor' : 'DESTRUCTOR',
    'div' : 'DIV',
    'do' : 'DO',
    'downto' : 'DOWNTO',
    'else' : 'ELSE',
    'end' : 'END',
    'file' : 'FILE',
    'for' : 'FOR',
    'function' : 'FUNCTION',
    'gotoXy' : 'GOTOXY',
    'goto' : 'GOTO',
    'halt' : 'HALT',
    'if' : 'IF',
    'implementation' : 'IMPLEMENTATION',
    'in' : 'IN',
    'inherited' : 'INHERITED',
    'inline' : 'INLINE',
    'interface' : 'INTERFACE',
    'label' : 'LABEL',
    'mod' : 'MOD',
    'nil' : 'NIL',
    'not' : 'NOT',
    'object' : 'OBJECT',
    'of' : 'OF',
    'on' : 'ON',
    'operator' : 'OPERATOR',
    'or' : 'OR',
    'packed' : 'PACKED',
    'procedure' : 'PROCEDURE',
    'program' : 'PROGRAM',
    'record' : 'RECORD',
    'reintroduce' : 'REINTRODUCE',
    'repeat' : 'REPEAT',
    'self' : 'SELF',
    'set' : 'SET',
    'shl' : 'SHL',
    'shr' : 'SHR',
    'string' : 'STRING',
    'then' : 'THEN',
    'to' : 'TO',
    'type' : 'TYPE',
    'unit' : 'UNIT',
    'until' : 'UNTIL',
    'uses' : 'USES',
    'var' : 'VAR',
    'while' : 'WHILE',
    'writeln' : 'WRITELN',
    'write' : 'WRITE',
    'readln' : 'READLN',
    'read' : 'READ',
    'readkey' : 'READKEY',
    'textbackgraound' : 'TEXTBACKGROUND',
    'textcolor' : 'TEXTCOLOR',
    'with' : 'WITH',
    'var' : 'VAR',
    'xor' : 'XOR',

# free pascal
    'dispose' : 'DISPOSE',
    'exit' : 'EXIT',
    'false' : 'FALSE',
    'new' : 'NEW',
    'true' : 'TRUE',


#object pascal
    'as' : 'AS',
    'class' : 'CLASS',
    'dispinterface' : 'DISPINTERFACE',
    'except' : 'EXCEPT',
    'exports' : 'EXPORTS',
    'finalization' : 'FINALIZATION',
    'finally' : 'FINALLY',
    'initialization' : 'INITIALIZATION',
    'inline' : 'INLINE',
    'is' : 'IS',
    'library' : 'LIBRARY',
    'on' : 'ON',
    'out' : 'OUT',
    'packed' : 'PACKED',
    'property' : 'PROPERTY',
    'raise' : 'RAISE',
    'resourcestring' : 'RESOURCESTRING',
    'threadvar' : 'THREADVAR',
    'try' : 'TRY',
}








tokens = list(reserved.values()) + [

#literals
'CHARACTER_STRING','IDENTIFIER','DIGSEQ','REALNUMBER', 

#operators
    'PLUS', 'MINUS', 'STAR', 'SLASH',
    'STARSTAR','GL','NOTEQUAL','HASH','UPARROW','DOLLAR','DVDV',
    'LSHIFT', 'RSHIFT','ATR',
    'LT', 'LE', 'GT', 'GE', 

# Assignment (=, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=)
    'EQUAL','ASSIGNMENT', 'TIMESEQUAL', 'DIVEQUAL', 'PLUSEQUAL', 
    'MINUSEQUAL', 



# Delimeters ( ) [ ] { } , . .. ; :
    'LPAREN', 'RPAREN',
    'LBRAC', 'RBRAC',
    'LBRACE', 'RBRACE',
    'COMMA', 'DOT', 'DOTDOT', 'SEMI', 'COLON',

]
	



# Operators
t_PLUS             = r'\+'
t_MINUS            = r'-'
t_STAR             = r'\*'
t_SLASH            = r'/'
t_LSHIFT           = r'\<\<'
t_RSHIFT           = r'\>\>'
t_ATR              = r'\@'
t_DOLLAR           = r'\$'
t_HASH             = r'\#'
t_UPARROW          = r'\^'
t_LT               = r'<'
t_NOTEQUAL         = r'<>'
t_GL               = r'><'
t_DVDV             = r'//'
t_STARSTAR         = r'\*\*'
t_GT               = r'>'
t_LE               = r'<='
t_GE               = r'>='



# Assignment operators

t_EQUAL            = r'='
t_ASSIGNMENT       = r':='
t_TIMESEQUAL       = r'\*='
t_DIVEQUAL         = r'/='
t_PLUSEQUAL        = r'\+='
t_MINUSEQUAL       = r'-='





# Delimeters
t_LPAREN           = r'\('
t_RPAREN           = r'\)'
t_LBRAC            = r'\['
t_RBRAC            = r'\]'
t_LBRACE           = r'\{'
t_RBRACE           = r'\}'
t_COMMA            = r'\,'
t_DOT   	   = r'\.'
t_DOTDOT	   = r'\.\.'
t_SEMI             = r'\;'
t_COLON            = r'\:'

#string
t_CHARACTER_STRING           = r'\'([^\\\n]|(\\.))*?\''


#integer
t_DIGSEQ = r'\d+ | \$[0-9A-Fa-f]+ | &[0-7]+ | %[0-1]+'

#float
t_REALNUMBER = r'((\d+)(\.\d+)(E(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'



#ignoring comment
def t_COMMENT(t):
    r'//(.*) | \{[^\{\}]*\} | \(\*[^"(*""*)"]*\*\)'
    pass


#checking for identifier
def t_IDENTIFIER(t):
    r'[A-Za-z_][\w_]*'
    t.type = reserved.get(t.value.lower(),"IDENTIFIER")
    return t

#computing line number
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


#computing column number though not necessary
def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

#ignoring blank space
t_ignore = ' \t'

#raising error code
def t_error(t):
    print "Illegal character '%s'" %t.value[0]
    t.lexer.skip(1)

#building lex
lexer = lex.lex()

data = '''
=
:=
#for
FOR
while
a3 + 4 * 10
Library
'bla bla'
Asm
asm
read
set
Readln
Readjunk
$23E
&067
#65
%000111
2.3E-17
//c
{c{ab}}
(*a*) c*)
 + -20 *2
'c'

20.2 + 1

PrograM expressionTest (input, output);
 var a, b : integer;
        c : real;
 BeGIn
   a := 3;
   b := a * 4;
   c := (b + a)/ 2
 End.

'''

lexer.input(data)


#while True:
#    tok = lexer.token()
#    if not tok: break
#    print tok


for tok in lexer:
    print tok



