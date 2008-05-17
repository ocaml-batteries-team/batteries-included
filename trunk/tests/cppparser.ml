open ExtGenlex
module M = Genlex.Make(Genlex.Languages.CPPLanguage)
open M
open ParserCo

let inc_op = operator "++"
let dec_op = operator "--"
let ptr_op = operator "->"

let rec primary_expression = label "primary_expression" (
  either [ identifier     ;
	   constant       ;
	   string_ligeral ;
	   operator "(" >>> expression >>> operator ")"] )

and postfix_expression = label "postfix_expression" ( 
  primary_expression >>>
    zero_plus (either [ 
		 operator "[" >>> expression >>> operator "]" ;
		 operator "(" >>> operator ")" ;
		 operator "(" >>> argument_expression_list >>> operator ")" ;
		 operator "." >>> identifier ;
		 inc_op >>> identifier ;
		 inc_op ;
		 dec_op ]))

and argument_expression_list = label "argument_expression_list" 
    ( one_plus ~sep:(operator ",") assignment_expression )

and unary_expression = label "unary_expression" (
  either [ postfix_expression ;
	   inc_op >>> unary_expression ;
	   dec_op >>> unary_expression ;
	   unary_operator >>> cast_expression ;
	   reserved "sizeof" >>> either [unary_expression ;
					 operator "(" >>> type_name >>> operator ")"] ] )

and unary_operator = label "unary_operator" ( one_of ["&" ;  "*" ; "+" ; "-" ; "~" ; "!" ] )

and cast_expression = label "cast_expression" (
  either [ unary_expression ;
	   operator "(" >>> type_name >>> operator ")" >>> cast_expression ] )

and multiplicative_expression = label "multiplicative_expression" (
  cast_expression >>>
    maybe
    (either [ operator "*" >>> multiplicative_expression ;
	      operator "/" >>> multiplicative_expression ;
	      operator "%" >>> multiplicative_expression ]))

and additive_expression = label "additive_expression" (
  multiplicative_expression >>>
    maybe
    (either [operator "+" >>> additive_expression ;
	     operator "-" >>> additive_expression ] ))

and shift_expression = label "shift_expression" (
  additive_expression >>>
    maybe
    (either [ operator "<<" >>> shift_expression ;
	     operator ">>" >>> shift_expression ] ))

and relational_expression = label "relational_expression" (
  shift_expression >>>
    maybe ([ operator "<"      >>> relational_expression ;
	     operator ">"      >>> relational_expression ;
	     operator "<=" >>> relational_expression ;
	     operator ">=" >>> relational_expression ] ))

and equality_expression = label "equality_expression" (
 relational_expression >>>
   maybe ([ operator "==" >>> equality_expression ;
	    operator "!=" >>> equality_expression ] ) )

and and_expression = label "and_expression" 
    (one_plus ~sep:(operator "&") exclusive_or_expression )

and exclusive_or_expression = label "exclusive_or_expression" 
    (one_plus ~sep:(operator "^") and_expression )

and inclusive_or_expression = label "inclusive_or_expression" 
    (one_plus ~sep:(operator "|") exclusive_or_expression )

and logical_and_expression = label "logical_and_expression" 
    (one_plus ~sep:(operator "&&") logical_or_expression)

and logical_or_expression = label "logical_or_expression" 
    (one_plus ~sep:(operator "||") logical_and_expression)

and conditional_expression = label "conditional_expression" 
    logical_or_expression >>>
    maybe (operator "?" >>> expression >>> operator ":" >>> conditional_expression )

and assignment_expression = label "assignment_expression" 
    ( either [ conditional_expression ;
	       unary_expression >>> assignment_operator >>> assignment_expression ] )

and assignment_operator = label "assignment_operator" 
    (either [ operator "=" ;
	   operator "*=" ;
	   operator "/=" ;
	   operator "%=" ;
	   operator "-=" ;
	   operator "<<=" ;
	   operator ">>=" ;
	   operator "&&=" ;
	   operator "^=" ;
	   operator "||=" ] )


and expression = label "expression" 
    ( either [ assignment_expression ;
	       expression >>> operator "," >>> assignment_expression ] )

and constant_expression = label "constant_expression" 
    ( conditional_expression   )

and declaration = label "declaration" 
    ( declaration_specifiers >>> maybe init_declarator_list >>> operator ";"  )

and declaration_specifiers = label "declaration_specifiers" 
  ( either [ storage_class_specifier >>> maybe declaration_specifiers ;
	     type_specifier >>> maybe declaration_specifiers ;
	     type_qualifier >>> maybe declaration_specifiers ] )

and init_declarator_list = label "init_declarator_list" 
  ( one_plus ~sep:(operator ",") init_declarator )

and init_declarator = label "init_declarator" 
  ( declarator >>> maybe (operator "=" >>> initaliser ) )

and storage_class_specifier = label "storage_class_specifier" 
    ( either [ reserved "typedef" ;
	       reserved "extern"  ;
	       reserved "static"  ;
	       reserved "auto"    ;
	       reserved "register" ] )

and type_specifier = label "type_specifier" 
    ( either [struct_or_union_specifier ;
	      enum_specifier ;
	      identifier ] )

and struct_or_union_specifier = label "struct_or_union_specifier" (
  struct_or_union >>>
    either [ identifier >>> maybe (operator "{" >>> struct_declaration_list >>> operator "}") ;
	     operator "{" >>> struct_declaration_list >>> operator "}" ] )

and struct_or_union = label "struct_or_union" 
    ( either [ reserved "structure" ;
	       reserved "union" ] )

and struct_declaration_list = label "struct_declaration_list" ( one_or_more struct_declaration )

and struct_declaration = label "struct_declaration" 
    (specifier_qualifier_list >>> struct_declarator_list >>> operator ";")

and specifier_qualifier_list = label "specifier_qualifier_list" 
  ( either [ type_specifier >>> maybe specifier_qualifier_list ;
	     type_qualifier >>> maybe specifier_qualifier_list ] )

and struct_declarator_list = label "struct_declarator_list" 
    ( one_plus ?sep:(operator ",") struct_declarator )

and struct_declarator = label "struct_declarator" 
    ( either [ declarator >>> maybe (operator ":" >>> constant_expression) ;
	   operator ":" >>> constant_expression  ] )

and enum_specifier = label "enum_specifier" 
    reserved "enum" >>> 
    ( either [ operator "{" >>> enumerator_list >>> operator "}" ;
	       identifier >>> maybe (operator "{" >>> enumerator_list >>> operator "}") ] )

and enumerator_list = label "enumerator_list" 
    ( one_plus ?sep:(operator ",") enumerator )2

and enumerator = label "enumerator" 
  identifier >>> maybe ( operator "=" >>> constant_expression )

and type_qualifier = label "type_qualifier" 
  ( either [ reserved "const" ;
	     reserved "volatile" ] )

and declarator = label "declarator" 
  ( either [ pointer >>> direct_declarator ;
	     direct_declarator ] )

and direct_declarator = label "direct_declarator" 
    ( either [ identifier ;
	       operator "(" >>> declarator >>> operator ")" ;
	       direct_declarator >>> 
		 either [ operator "[" >>> maybe constant_expression >>> operator "]" ;
			  operator "(" >>> 
			    either [ parameter_type_list >>> operator ")" ;
				     identifier_list >>> operator ")" ;
				     operator ")" ] ] ] )

and pointer = label "pointer" 
    ( operator "*" >>> maybe (
	either [ type_qualifier_list >>> maybe pointer ;
		 pointer ] ) )

and type_qualifier_list = label "type_qualifier_list" 
  ( one_plus type_qualifier )
  
and parameter_type_list = label "parameter_type_list" 
  ( parameter_list >>> maybe (operator "," >>> operator "...") )

and parameter_list = label "parameter_list" 
  ( one_plus ~sep:(operator ",") parameter_declaration )

and parameter_declaration = label "parameter_declaration" 
  ( declaration_specifiers >>> 
    maybe ( either [ declarator ;
		     abstract_declarator ] ) )

and identifier_list = label "identifier_list" 
  ( one_plus ~sep:(operator ",") identifier )

and type_name = label "type_name" 
  ( specifier_qualifier_list >>> maybe abstract_declarator )

and abstract_declarator = label "abstract_declarator" 
  ( either [ pointer >>> maybe direct_abstract_declarator ;
	     direct_abstract_declarator ] )

and direct_abstract_declarator = label "direct_abstract_declarator" 
  ( maybe direct_abstract_declarator ( either [ operator "(" >>> 
	       either [ abstract_declarator >>> operator ")" ;
			operator ")" ;
			parameter_type_list >>> operator ")" ] ;
	     operator "[" >>> 
	       either [
		 constant_expression >>> operator "]" ;
		 operator "]" ] ] ) )

and initaliser = label "initaliser" 
    ( either [ assignment_expression ;
	 >>> operator "{" >>> initaliser_list >>> operator "}" ;
	 >>> operator "{" >>> initaliser_list >>> operator "," >>> operator "}"] )

and initaliser_list = label "initaliser_list" 
    ( either [ initaliser ;
	 initaliser_list >>> operator "," >>> initaliser ] )

and statement = label "statement" 
    ( either [ labeled_statement ;
	 compound_statement ;
	 expression_statement ;
	 selection_statement ;
	 iteration_statement ;
	 jump_statement ] )

and labeled_statement = label "labeled_statement" 
    ( either [ identifier >>> operator ":" >>> statement ;
	 CASE >>> constant_expression >>> operator ":" >>> statement ;
	 DEFAULT >>> operator ":" >>> statement ] )

and compound_statement = label "compound_statement" 
    ( either [ >>> operator "{" >>> operator "}" ;
	 >>> operator "{" >>> statement_list >>> operator "}" ;
	 >>> operator "{" >>> declaration_list >>> operator "}" ;
	 >>> operator "{" >>> declaration_list >>> statement_list >>> operator "}"] )

and declaration_list = label "declaration_list" 
    ( either [ declaration ;
	 declaration_list >>> declaration ] )

and statement_list = label "statement_list" 
    ( either [ statement ;
	 statement_list >>> statement ] )

and expression_statement = label "expression_statement" 
    ( either [ >>> operator ";" ;
	 expression >>> operator ";"] )

and selection_statement = label "selection_statement" 
    ( either [ IF >>> operator "(" >>> expression >>> operator ")" >>> statement ;
	 IF >>> operator "(" >>> expression >>> operator ")" >>> statement >>> ELSE >>> statement ;
	 SWITCH >>> operator "(" >>> expression >>> operator ")" >>> statement ] )

and iteration_statement = label "iteration_statement" 
    ( either [ WHILE >>> operator "(" >>> expression >>> operator ")" >>> statement ;
	 DO >>> statement >>> WHILE >>> operator "(" >>> expression >>> operator ")" >>> operator ";" ;
	 FOR >>> operator "(" >>> expression_statement >>> expression_statement >>> operator ")" >>> statement ;
	 FOR >>> operator "(" >>> expression_statement >>> expression_statement >>> expression >>> operator ")" >>> statement ] )

and jump_statement = label "jump_statement" 
    ( either [ GOTO >>> identifier >>> operator ";" ;
	 CONTINUE >>> operator ";" ;
	 BREAK >>> operator ";" ;
	 RETURN >>> operator ";" ;
	 RETURN >>> expression >>> operator ";"] )

and translation_unit = label "translation_unit" 
    ( either [ external_declaration ;
	 translation_unit >>> external_declaration ] )

and external_declaration = label "external_declaration" 
    ( either [ function_definition ;
	 declaration ] )

and function_definition = label "function_definition" 
    ( either [ declaration_specifiers >>> declarator >>> declaration_list >>> compound_statement ;
	 declaration_specifiers >>> declarator >>> compound_statement ;
	 declarator >>> declaration_list >>> compound_statement ;
	 declarator >>> compound_statement ] )
