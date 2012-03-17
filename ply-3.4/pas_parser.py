# Pascal Parser
# Pascal grammer, based originally on BNF given in "Standard Pascal -- User Reference Manual", by Doug Cooper.
# This in turn is the BNF given by the ANSI and ISO Pascal standards,
# and so, is PUBLIC DOMAIN. The grammar is for ISO Level 0 Pascal.
# The grammar has been massaged somewhat to make it LALR, and added
# the following extensions.
#
# constant expressions
# otherwise statement in a case
# productions to correctly match else's with if's
# beginnings of a separate compilation facility

import ply.yacc as yacc

#Get token map from lexer
from pas_lexer import tokens

def p_file (p):
	"""file : program
	| module"""
 

def p_program (p):
	"""program : program_heading semicolon block DOT"""

def p_program_heading (p):
	"""program_heading : PROGRAM identifier
	| PROGRAM identifier LPAREN identifier_list RPAREN"""

def p_identifier_list (p):
	"""identifier_list : identifier_list comma identifier
	| identifier"""

def p_block (p):
	"""block : label_declaration_part constant_definition_part type_definition_part variable_declaration_part procedure_and_function_declaration_part statement_part"""

def p_module (p):
	"""module : constant_definition_part type_definition_part variable_declaration_part procedure_and_function_declaration_part"""

def p_label_declaration_part (p):
	"""label_declaration_part : LABEL label_list semicolon
	|"""

def p_label_list (p):
	"""label_list : label_list comma label
	| label"""

def p_label (p):
	"""label : DIGSEQ"""

def p_constant_definition_part (p):
	"""constant_definition_part : CONST constant_list
	|"""

def p_constant_list (p):
	"""constant_list : constant_list constant_definition
	| constant_definition"""

def p_constant_definition (p):
	"""constant_definition : identifier EQUAL cexpression semicolon"""

#constant : cexpression good stuff! 

def p_cexpression (p):
	"""cexpression : csimple_expression
	| csimple_expression relop csimple_expression"""

def p_csimple_expression (p):
	"""csimple_expression : cterm
	| csimple_expression addop cterm"""

def p_cterm (p):
	"""cterm : cfactor
	| cterm mulop cfactor"""

def p_cfactor (p):
	"""cfactor : sign cfactor
	| cexponentiation"""

def p_cexponentiation (p):
	"""cexponentiation : cprimary
	| cprimary STARSTAR cexponentiation"""

def p_cprimary (p):
	"""cprimary : identifier
	| LPAREN cexpression RPAREN
	| unsigned_constant
	| NOT cprimary"""

def p_constant (p):
	"""constant : non_string
	| sign non_string
	| CHARACTER_STRING"""

def p_sign (p):
	"""sign : PLUS
	| MINUS"""

def p_non_string (p):
	"""non_string : DIGSEQ
	| identifier
	| REALNUMBER"""

def p_type_definition_part (p):
	"""type_definition_part : TYPE type_definition_list
	|"""

def p_type_definition_list (p):
	"""type_definition_list : type_definition_list type_definition
	| type_definition"""

def p_type_definition (p):
	"""type_definition : identifier EQUAL type_denoter semicolon"""

def p_type_denoter (p):
	"""type_denoter : identifier
	| new_type"""

def p_new_type (p):
	"""new_type : new_ordinal_type
	| new_structured_type
	| new_pointer_type"""

def p_new_ordinal_type (p):
	"""new_ordinal_type : enumerated_type
	| subrange_type"""

def p_enumerated_type (p):
	"""enumerated_type : LPAREN identifier_list RPAREN"""

def p_subrange_type (p):
	"""subrange_type : constant DOTDOT constant"""

def p_new_structured_type (p):
	"""new_structured_type : structured_type
	| PACKED structured_type"""

def p_structured_type (p):
	"""structured_type : array_type
	| record_type
	| set_type
	| file_type"""

def p_array_type (p):
	"""array_type : ARRAY LBRAC index_list RBRAC OF component_type"""

def p_index_list (p):
	"""index_list : index_list comma index_type
	| index_type"""

def p_index_type (p):
	"""index_type : ordinal_type"""

def p_ordinal_type (p):
	"""ordinal_type : new_ordinal_type
	| identifier"""

def p_component_type (p):
	"""component_type : type_denoter"""

def p_record_type (p):
	"""record_type : RECORD record_section_list END
	| RECORD record_section_list semicolon variant_part END
	| RECORD variant_part END"""

def p_record_section_list (p):
	"""record_section_list : record_section_list semicolon record_section
	| record_section"""

def p_record_section (p):
	"""record_section : identifier_list COLON type_denoter"""

def p_variant_part (p):
	"""variant_part : CASE variant_selector OF variant_list semicolon
	| CASE variant_selector OF variant_list
	|"""

def p_variant_selector (p):
	"""variant_selector : tag_field COLON tag_type
	| tag_type"""

def p_variant_list (p):
	"""variant_list : variant_list semicolon variant
	| variant"""

def p_variant (p):
	"""variant : case_constant_list COLON LPAREN record_section_list RPAREN
	| case_constant_list COLON LPAREN record_section_list semicolon variant_part RPAREN
	| case_constant_list COLON LPAREN variant_part RPAREN"""

def p_case_constant_list (p):
	"""case_constant_list : case_constant_list comma case_constant
	| case_constant"""

def p_case_constant (p):
	"""case_constant : constant
	| constant DOTDOT constant"""

def p_tag_field (p):
	"""tag_field : identifier"""

def p_tag_type (p):
	"""tag_type : identifier"""

def p_set_type (p):
	"""set_type : SET OF base_type"""

def p_base_type (p):
	"""base_type : ordinal_type"""

def p_file_type (p):
	"""file_type : PFILE OF component_type"""

def p_new_pointer_type (p):
	"""new_pointer_type : UPARROW domain_type"""

def p_domain_type (p):
	"""domain_type : identifier"""

def p_variable_declaration_part (p):
	"""variable_declaration_part : VAR variable_declaration_list semicolon
	|"""

def p_variable_declaration_list (p):
	"""variable_declaration_list : variable_declaration_list semicolon variable_declaration
	| variable_declaration"""

def p_variable_declaration (p):
	"""variable_declaration : identifier_list COLON type_denoter"""

def p_procedure_and_function_declaration_part (p):
	"""procedure_and_function_declaration_part : proc_or_func_declaration_list semicolon
	|"""

def p_proc_or_func_declaration_list (p):
	"""proc_or_func_declaration_list : proc_or_func_declaration_list semicolon proc_or_func_declaration
	| proc_or_func_declaration"""

def p_proc_or_func_declaration (p):
	"""proc_or_func_declaration : procedure_declaration
	| function_declaration"""

def p_procedure_declaration (p):
	"""procedure_declaration : procedure_heading semicolon directive
	| procedure_heading semicolon procedure_block"""

def p_procedure_heading (p):
	"""procedure_heading : procedure_identification
	| procedure_identification formal_parameter_list"""

def p_directive (p):
	"""directive : FORWARD
	| EXTERNAL"""

def p_formal_parameter_list (p):
	"""formal_parameter_list : LPAREN formal_parameter_section_list RPAREN"""

def p_formal_parameter_section_list (p):
	"""formal_parameter_section_list : formal_parameter_section_list semicolon formal_parameter_section
	| formal_parameter_section"""

def p_formal_parameter_section (p):
	"""formal_parameter_section : value_parameter_specification
	| variable_parameter_specification
	| procedural_parameter_specification
	| functional_parameter_specification"""

def p_value_parameter_specification (p):
	"""value_parameter_specification : identifier_list COLON identifier"""

def p_variable_parameter_specification (p):
	"""variable_parameter_specification : VAR identifier_list COLON identifier"""

def p_procedural_parameter_specification (p):
	"""procedural_parameter_specification : procedure_heading"""

def p_functional_parameter_specification (p):
	"""functional_parameter_specification : function_heading"""

def p_procedure_identification (p):
	"""procedure_identification : PROCEDURE identifier"""

def p_procedure_block (p):
	"""procedure_block : block"""

def p_function_declaration (p):
	"""function_declaration : function_heading semicolon directive
	| function_identification semicolon function_block
	| function_heading semicolon function_block"""

def p_function_heading (p):
	"""function_heading : FUNCTION identifier COLON result_type
	| FUNCTION identifier formal_parameter_list COLON result_type"""

def p_result_type (p):
	"""result_type : identifier"""

def p_function_identification (p):
	"""function_identification : FUNCTION identifier"""

def p_function_block (p):
	"""function_block : block"""

def p_statement_part (p):
	"""statement_part : compound_statement"""

def p_compound_statement (p):
	"""compound_statement : PBEGIN statement_sequence END"""

def p_statement_sequence (p):
	"""statement_sequence : statement_sequence semicolon statement
	| statement"""

def p_statement (p):
	"""statement : open_statement
	| closed_statement"""

def p_open_statement (p):
	"""open_statement : label COLON non_labeled_open_statement
	| non_labeled_open_statement"""

def p_closed_statement (p):
	"""closed_statement : label COLON non_labeled_closed_statement
	| non_labeled_closed_statement"""

def p_non_labeled_closed_statement (p):
	"""non_labeled_closed_statement : assignment_statement
	| procedure_statement
	| goto_statement
	| compound_statement
	| case_statement
	| repeat_statement
	| closed_with_statement
	| closed_if_statement
	| closed_while_statement
	| closed_for_statement
	|"""

def p_non_labeled_open_statement (p):
	"""non_labeled_open_statement : open_with_statement
	| open_if_statement
	| open_while_statement
	| open_for_statement"""

def p_repeat_statement (p):
	"""repeat_statement : REPEAT statement_sequence UNTIL boolean_expression"""

def p_open_while_statement (p):
	"""open_while_statement : WHILE boolean_expression DO open_statement"""

def p_closed_while_statement (p):
	"""closed_while_statement : WHILE boolean_expression DO closed_statement"""

def p_open_for_statement (p):
	"""open_for_statement : FOR control_variable ASSIGNMENT initial_value direction final_value DO open_statement"""

def p_closed_for_statement (p):
	"""closed_for_statement : FOR control_variable ASSIGNMENT initial_value direction final_value DO closed_statement"""

def p_open_with_statement (p):
	"""open_with_statement : WITH record_variable_list DO open_statement"""

def p_closed_with_statement (p):
	"""closed_with_statement : WITH record_variable_list DO closed_statement"""

def p_open_if_statement (p):
	"""open_if_statement : IF boolean_expression THEN statement
	| IF boolean_expression THEN closed_statement ELSE open_statement"""

def p_closed_if_statement (p):
	"""closed_if_statement : IF boolean_expression THEN closed_statement ELSE closed_statement"""

def p_assignment_statement (p):
	"""assignment_statement : variable_access ASSIGNMENT expression"""

def p_variable_access (p):
	"""variable_access : identifier
	| indexed_variable
	| field_designator
	| variable_access UPARROW"""

def p_indexed_variable (p):
	"""indexed_variable : variable_access LBRAC index_expression_list RBRAC"""

def p_index_expression_list (p):
	"""index_expression_list : index_expression_list comma index_expression
	| index_expression"""

def p_index_expression (p):
	"""index_expression : expression"""

def p_field_designator (p):
	"""field_designator : variable_access DOT identifier"""

def p_procedure_statement (p):
	"""procedure_statement : identifier params
	| identifier"""

def p_params (p):
	"""params : LPAREN actual_parameter_list RPAREN"""

def p_actual_parameter_list (p):
	"""actual_parameter_list : actual_parameter_list comma actual_parameter
	| actual_parameter"""

  
 # this forces you to check all this to be sure that only write and
 # writeln use the 2nd and 3rd forms, you really can't do it easily in
 # the grammar, especially since write and writeln aren't reserved
 
def p_actual_parameter (p):
	"""actual_parameter : expression
	| expression COLON expression
	| expression COLON expression COLON expression"""

def p_goto_statement (p):
	"""goto_statement : GOTO label"""

def p_case_statement (p):
	"""case_statement : CASE case_index OF case_list_element_list END
	| CASE case_index OF case_list_element_list SEMICOLON END
	| CASE case_index OF case_list_element_list semicolon otherwisepart statement END
	| CASE case_index OF case_list_element_list semicolon otherwisepart statement SEMICOLON END"""

def p_case_index (p):
	"""case_index : expression"""

def p_case_list_element_list (p):
	"""case_list_element_list : case_list_element_list semicolon case_list_element
	| case_list_element"""

def p_case_list_element (p):
	"""case_list_element : case_constant_list COLON statement"""

def p_otherwisepart (p):
	"""otherwisepart : OTHERWISE
	| OTHERWISE COLON"""

def p_control_variable (p):
	"""control_variable : identifier"""

def p_initial_value (p):
	"""initial_value : expression"""

def p_direction (p):
	"""direction : TO
	| DOWNTO"""

def p_final_value (p):
	"""final_value : expression"""

def p_record_variable_list (p):
	"""record_variable_list : record_variable_list comma variable_access
	| variable_access"""

def p_boolean_expression (p):
	"""boolean_expression : expression"""

def p_expression (p):
	"""expression : simple_expression
	| simple_expression relop simple_expression"""

def p_simple_expression (p):
	"""simple_expression : term
	| simple_expression addop term"""

def p_term (p):
	"""term : factor
	| term mulop factor"""

def p_factor (p):
	"""factor : sign factor
	| exponentiation"""

def p_exponentiation (p):
	"""exponentiation : primary
	| primary STARSTAR exponentiation"""

def p_primary (p):
	"""primary : variable_access
	| unsigned_constant
	| function_designator
	| set_constructor
	| LPAREN expression RPAREN
	| NOT primary"""

def p_unsigned_constant (p):
	"""unsigned_constant : unsigned_number
	| CHARACTER_STRING
	| NIL"""

def p_unsigned_number (p):
	"""unsigned_number : unsigned_integer 
	| unsigned_real"""

def p_unsigned_integer (p):
	"""unsigned_integer : DIGSEQ"""

def p_unsigned_real (p):
	"""unsigned_real : REALNUMBER"""

#/* functions with no params will be handled by plain identifier */
def p_function_designator (p):
	"""function_designator : identifier params"""

def p_set_constructor (p):
	"""set_constructor : LBRAC member_designator_list RBRAC
	| LBRAC RBRAC"""

def p_member_designator_list (p):
	"""member_designator_list : member_designator_list comma member_designator
	| member_designator"""

def p_member_designator (p):
	"""member_designator : member_designator DOTDOT expression
	| expression"""

def p_addop(p):
	"""addop : PLUS
	| MINUS
	| OR"""

def p_mulop (p):
	"""mulop : STAR
	| SLASH
	| DIV
	| MOD
	| AND"""

def p_relop (p):
	"""relop : EQUAL
	| NOTEQUAL
	| LT
	| GT
	| LE
	| GE
	| IN"""

def p_identifier (p):
	"""identifier : IDENTIFIER"""

def p_semicolon (p):
	"""semicolon : SEMICOLON"""

def p_comma (p):
	"""comma : COMMA"""

#Error rule for syntax errors
def p_error(p):
	print "Syntax error in input!"

#Build the parser
parser = yacc.yacc(start='otherwisepart')

while True:
	try:
		s = raw_input('pas_input > ')
	except EOFError:
		break
	if not s: continue
	result = parser.parse(s)
	print result
