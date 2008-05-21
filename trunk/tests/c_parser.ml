open ExtGenlex
module M = Genlex.Languages.Make(Genlex.Languages.Library.C)
open M
open Genlex.Languages.Library.C
open ParserCo
open CharParser

let inc_op = operator (string "++")
let dec_op = operator (string "--")
let ptr_op = operator (string "->")

let schar c = string (ExtString.String.of_char c)

let noop = return ()

let constant = 
    either [ schar '0' >>= fun _ ->
	     either [ one_of ['x'; 'X'] >>= fun _ -> one_plus hex >>= fun _ -> noop ;
		      one_plus digit    >>= fun _ -> noop ] >>= fun _ -> 
	     maybe (one_of ['u'; 'U'; 'l'; 'L']) >>= fun _ -> noop ;
	   one_plus digit >>= fun _ -> maybe (one_of ['u'; 'U'; 'l'; 'L']) >>= fun _ -> noop ;
	   maybe ident_start >>= fun _ -> char_literal >>= fun _ -> noop ;
	   float >>= fun _ -> noop]


let rec primary_expression _ = label "primary_expression ()" (
   any_identifier >>= fun _ -> noop ;
	   constant       >>= fun _ -> noop ;
	   string_literal >>= fun _ -> noop ;
	   operator (schar '(') >>= fun _ -> expression () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ] )

and postfix_expression _ = label "postfix_expression ()" ( 
  primary_expression () >>= fun _ ->
    zero_plus ( 
		 operator (schar '[') >>= fun _ -> expression () >>= fun _ -> operator (schar ']') >>= fun _ -> noop ;
		 operator (schar '(') >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
		 operator (schar '(') >>= fun _ -> argument_expression_list () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
		 operator (schar '.') >>= fun _ -> any_identifier >>= fun _ -> noop ;
		 inc_op >>= fun _ -> any_identifier >>= fun _ -> noop ;
		 inc_op >>= fun _ -> noop ;
		 dec_op>>= fun _ -> noop ]))

and argument_expression_list _ = label "argument_expression_list ()" 
    ( one_plus ~sep:(operator (schar ',')) (assignment_expression () ))

and unary_expression _ = label "unary_expression ()" (
   postfix_expression () >>= fun _ -> noop ;
	   inc_op >>= fun _ -> unary_expression () >>= fun _ -> noop ;
	   dec_op >>= fun _ -> unary_expression () >>= fun _ -> noop ;
	   unary_operator () >>= fun _ -> cast_expression () >>= fun _ -> noop ;
	   reserved (string "sizeof") >>= fun _ ->  unary_expression () >>= fun _ -> noop ;
					  operator (schar '(') >>= fun _ -> type_name () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ]>>= fun _ -> noop ] )

and unary_operator _ = label "unary_operator" ( one_of ['&' ;  '*' ; '+' ; '-' ; '~' ; '!' ] >>= fun _ -> noop )

and cast_expression _ = label "cast_expression" (
   unary_expression () >>= fun _ -> noop ;
	   operator (schar '(') >>= fun _ -> type_name () >>= fun _ -> operator (schar ')') >>= fun _ -> cast_expression () >>= fun _ -> noop ] )

and multiplicative_expression _ = label "multiplicative_expression" (
  cast_expression () >>= fun _ ->
    maybe
    ( operator (schar '*') >>= fun _ -> multiplicative_expression () >>= fun _ -> noop ;
	      operator (schar '/') >>= fun _ -> multiplicative_expression () >>= fun _ -> noop ;
	      operator (schar '%') >>= fun _ -> multiplicative_expression () >>= fun _ -> noop ]))

and additive_expression _ = label "additive_expression" (
  multiplicative_expression () >>= fun _ ->
    maybe
    (operator (schar '+') >>= fun _ -> additive_expression () >>= fun _ -> noop ;
	     operator (schar '-') >>= fun _ -> additive_expression () >>= fun _ -> noop ] >>= fun _ -> return ()))

and shift_expression _ = label "shift_expression" (
  additive_expression () >>= fun _ ->
    maybe
    ( operator (string "<<") >>= fun _ -> shift_expression () >>= fun _ -> noop ;
	      operator (string ">>") >>= fun _ -> shift_expression () >>= fun _ -> noop ] >>= fun _ -> noop))

and relational_expression _ = label "relational_expression" (
  shift_expression () >>= fun _ ->
    maybe ( operator (schar '<')   >>= fun _ -> relational_expression () >>= fun _ -> noop ;
		    operator (schar '>')   >>= fun _ -> relational_expression () >>= fun _ -> noop ;
		    operator (string "<=") >>= fun _ -> relational_expression () >>= fun _ -> noop ;
		    operator (string ">=") >>= fun _ -> relational_expression () >>= fun _ -> noop ] ))

and equality_expression _ = label "equality_expression" (
 relational_expression () >>= fun _ ->
   maybe ( operator (string "==") >>= fun _ -> equality_expression () >>= fun _ -> noop ;
		   operator (string "!=") >>= fun _ -> equality_expression () >>= fun _ -> noop ] >>= fun _ -> noop) )

and and_expression _ = label "and_expression" 
    (one_plus ~sep:(operator (schar '&')) (exclusive_or_expression () ))

and exclusive_or_expression _ = label "exclusive_or_expression" 
    (one_plus ~sep:(operator (schar '^')) (and_expression ()) ) >>= fun _ -> noop

and inclusive_or_expression _ = label "inclusive_or_expression" 
    (one_plus ~sep:(operator (schar '|')) (exclusive_or_expression ()) ) >>= fun _ -> noop

and logical_and_expression _ = label "logical_and_expression" 
    (one_plus ~sep:(operator (string "&&")) (logical_or_expression ()) ) >>= fun _ -> noop

and logical_or_expression _ = label "logical_or_expression" 
    (one_plus ~sep:(operator (string "||")) (logical_and_expression ()) ) >>= fun _ -> noop

and conditional_expression _ = label "conditional_expression" 
  ( logical_or_expression () >>= fun _ ->
      maybe (operator (schar '?') >>= fun _ -> expression () >>= fun _ -> operator (schar ':') >>= fun _ -> conditional_expression ()) >>= fun _ -> noop )

and assignment_expression _ = label "assignment_expression" 
    (  conditional_expression () >>= fun _ -> noop ;
	       unary_expression () >>= assignment_operator >>= assignment_expression >>= fun _ -> noop ] )

and assignment_operator _ = label "assignment_operator" 
    ( operator (schar '=') >>= fun _ -> noop ;
	   operator (string "*=") >>= fun _ -> noop ;
	   operator (string "/=") >>= fun _ -> noop ;
	   operator (string "%=") >>= fun _ -> noop ;
	   operator (string "-=") >>= fun _ -> noop ;
	   operator (string "<<=") >>= fun _ -> noop ;
	   operator (string ">>= fun _ ->") >>= fun _ -> noop ;
	   operator (string "&&=") >>= fun _ -> noop ;
	   operator (string "^=") >>= fun _ -> noop ;
	   operator (string "||=") >>= fun _ -> noop ] )


and expression _ = label "expression" 
    (  assignment_expression () >>= fun _ -> noop ;
	       expression () >>= fun _ -> operator (schar ',') >>= fun _ -> assignment_expression () >>= fun _ -> noop ] )

and constant_expression _ = label "constant_expression" 
    ( conditional_expression () )

and declaration _ = label "declaration" 
    ( declaration_specifiers () >>= fun _ -> maybe (init_declarator_list ()) >>= fun _ -> operator (schar ';')  )

and declaration_specifiers _ = label "declaration_specifiers" 
  (  storage_class_specifier () >>= fun _ -> maybe (declaration_specifiers ()) >>= fun _ -> noop ;
	     type_specifier ()          >>= fun _ -> maybe (declaration_specifiers ()) >>= fun _ -> noop ;
	     type_qualifier ()          >>= fun _ -> maybe (declaration_specifiers ()) >>= fun _ -> noop ] )

and init_declarator_list _ = label "init_declarator_list" 
  ( one_plus ~sep:(operator (schar ',')) (init_declarator () ))

and init_declarator _ = label "init_declarator" 
  ( declarator () >>= fun _ -> maybe (operator (schar '=') >>= fun _ -> initialiser () ) )

and storage_class_specifier _ = label "storage_class_specifier" 
    (  reserved (string "typedef") >>= fun _ -> noop ;
	       reserved (string "extern")  >>= fun _ -> noop ;
	       reserved (string "static")  >>= fun _ -> noop ;
	       reserved (string "auto")    >>= fun _ -> noop ;
	       reserved (string "register") >>= fun _ -> noop ] )

and type_specifier _ = label "type_specifier" 
    ( struct_or_union_specifier () >>= fun _ -> noop ;
	      enum_specifier () >>= fun _ -> noop ;
	      any_identifier    >>= fun _ -> noop ] )

and struct_or_union_specifier _ = label "struct_or_union_specifier" (
  struct_or_union () >>= fun _ ->
     any_identifier >>= fun _ -> maybe (operator (schar '{') >>= fun _ -> struct_declaration_list () >>= fun _ -> operator (schar '}')) >>= fun _ -> noop ;
	     operator (schar '{') >>= fun _ -> struct_declaration_list () >>= fun _ -> operator (schar '}') >>= fun _ -> noop ] )

and struct_or_union _ = label "struct_or_union" 
    (  reserved (string "structure") >>= fun _ -> noop ;
	       reserved (string "union") >>= fun _ -> noop ] )

and struct_declaration_list _ = label "struct_declaration_list" ( one_plus ( struct_declaration () ))

and struct_declaration _ = label "struct_declaration" 
    (specifier_qualifier_list () >>= fun _ -> struct_declarator_list () >>= fun _ -> operator (schar ';'))

and specifier_qualifier_list _ = label "specifier_qualifier_list" 
  (  type_specifier () >>= fun _ -> maybe (specifier_qualifier_list ()) >>= fun _ -> noop ;
	     type_qualifier () >>= fun _ -> maybe (specifier_qualifier_list ()) >>= fun _ -> noop ] )

and struct_declarator_list _ = label "struct_declarator_list" 
  ( one_plus ~sep:(operator (schar ',')) (struct_declarator () ) ) >>= fun _ -> noop

and struct_declarator _ = label "struct_declarator" 
    (  declarator () >>= fun _ -> maybe (operator (schar ':') >>= fun _ -> constant_expression () ) >>= fun _ -> noop ;
	       operator (schar ':') >>= fun _ -> (constant_expression () ) >>= fun _ -> noop ] )

and enum_specifier _ = label "enum_specifier" 
    ( reserved (string "enum") >>= fun _ -> 
    (  operator (schar '{') >>= fun _ -> enumerator_list () >>= fun _ -> operator (schar '}') >>= fun _ -> noop ;
	       any_identifier >>= fun _ -> maybe (operator (schar '{') >>= fun _ -> enumerator_list () >>= fun _ -> operator (schar '}')) >>= fun _ -> noop ] ) )

and enumerator_list _ = label "enumerator_list" 
    ( one_plus ~sep:(operator (schar ',')) (enumerator () )) >>= fun _ -> noop

and enumerator _ = label "enumerator" 
  ( any_identifier >>= fun _ -> maybe ( operator (schar '=') >>= fun _ -> (constant_expression () )) ) >>= fun _ -> noop

and type_qualifier _ = label "type_qualifier" 
  (  reserved (string "const") >>= fun _ -> noop ;
	     reserved (string "volatile") >>= fun _ -> noop ] )

and declarator _ = label "declarator" 
  (  pointer () >>= fun _ -> direct_declarator () >>= fun _ -> noop ;
	     direct_declarator () >>= fun _ -> noop ] )

and direct_declarator _ = label "direct_declarator" 
    (  any_identifier >>= fun _ -> noop ;
	       operator (schar '(') >>= fun _ -> declarator () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
	       direct_declarator () >>= fun _ -> 
		  operator (schar '[') >>= fun _ -> maybe (constant_expression ()) >>= fun _ -> operator (schar ']') >>= fun _ -> noop ;
			  operator (schar '(') >>= fun _ -> 
			     parameter_type_list () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
				     identifier_list () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
				     operator (schar ')') >>= fun _ -> noop ] >>= fun _ -> noop ] >>= fun _ -> noop ] )

and pointer _ = label "pointer" 
    ( operator (schar '*') >>= fun _ -> maybe (
	 type_qualifier_list () >>= fun _ -> maybe (pointer ()) >>= fun _ -> noop ;
		 pointer () >>= fun _ -> noop ] ) )

and type_qualifier_list _ = label "type_qualifier_list" 
  ( one_plus (type_qualifier () ))
  
and parameter_type_list _ = label "parameter_type_list" 
  ( parameter_list () >>= fun _ -> maybe (operator (schar ',') >>= fun _ -> operator (string "...")) )

and parameter_list _ = label "parameter_list" 
  ( one_plus ~sep:(operator (schar ',')) (parameter_declaration ()) )

and parameter_declaration _ = label "parameter_declaration" 
  ( declaration_specifiers () >>= fun _ -> 
    maybe (  declarator () >>= fun _ -> noop ;
		     abstract_declarator () >>= fun _ -> noop ] ) )

and identifier_list _ = label "identifier_list" 
  ( one_plus ~sep:(operator (schar ',')) any_identifier )

and type_name _ = label "type_name" 
  ( specifier_qualifier_list () >>= fun _ -> maybe (abstract_declarator ()) )

and abstract_declarator _ = label "abstract_declarator" 
  (  pointer () >>= fun _ -> maybe (direct_abstract_declarator ()) >>= fun _ -> noop ;
	     direct_abstract_declarator () >>= fun _ -> noop ] )

and direct_abstract_declarator _ = label "direct_abstract_declarator" 
  ( maybe (direct_abstract_declarator ()) >>= fun _ -> (  operator (schar '(') >>= fun _ -> 
	        abstract_declarator () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ;
			operator (schar ')') >>= fun _ -> noop ;
			parameter_type_list () >>= fun _ -> operator (schar ')') >>= fun _ -> noop ] >>= fun _ -> noop ;
	       operator (schar '[') >>= fun _ -> 
	       
		 constant_expression () >>= fun _ -> operator (schar ']') >>= fun _ -> noop ;
		 operator (schar ']') >>= fun _ -> noop ] >>= fun _ -> noop ] ) )

and initialiser _ = label "initaliser" 
  (  assignment_expression () >>= fun _ -> noop ;
	     operator (schar '{')     >>= initaliser_list >>= fun _ -> maybe (operator (schar ',')) >>= fun _ -> operator (schar '}') >>= fun _ -> noop ] )

and initaliser_list _ = label "initaliser_list" 
  ( one_plus ~sep:(operator (schar ',')) (initialiser ())) >>= fun _ -> noop

and statement _ = label "statement" 
  (  labeled_statement () >>= fun _ -> noop ;
	     compound_statement () >>= fun _ -> noop ;
	     expression_statement () >>= fun _ -> noop ;
	     selection_statement () >>= fun _ -> noop ;
	     iteration_statement () >>= fun _ -> noop ;
	     jump_statement () >>= fun _ -> noop ] )

and labeled_statement _ = label "labeled_statement" 
  (  any_identifier         >>= fun _ -> operator (schar ':') >>= fun _ -> statement () >>= fun _ -> noop ;
	     reserved (string "case")    >>= fun _ -> constant_expression () >>= fun _ -> operator (schar ':') >>= fun _ -> statement () >>= fun _ -> noop ;
	     reserved (string "default") >>= fun _ -> operator (schar ':') >>= fun _ -> statement () >>= fun _ -> noop ] )

and compound_statement _ = label "compound_statement" 
  ( operator (schar '{') >>= fun _ ->
      (  operator (schar '}') >>= fun _ -> noop ;
		 statement_list ()   >>= fun _ -> operator (schar '}') >>= fun _ -> noop ;
		 declaration_list () >>= fun _ -> maybe (statement_list ()) >>= fun _ -> operator (schar '}') >>= fun _ -> noop ] ) )

and declaration_list _ = label "declaration_list" 
  ( one_plus (declaration ()))

and statement_list _ = label "statement_list" 
  ( one_plus (statement ()))

and expression_statement _ = label "expression_statement" 
  ( maybe ( expression ()) >>= fun _ -> operator (schar ';') )

and selection_statement _ = label "selection_statement" 
  (  reserved (string "if") >>= fun _ -> operator (schar '(') >>= fun _ -> expression () >>= fun _ -> operator (schar ')') >>= fun _ -> statement () >>= fun _ -> 
	       maybe (reserved (string "else") >>= fun _ -> (statement ()) ) >>= fun _ -> noop ;
	     reserved (string "switch") >>= fun _ -> operator (schar '(') >>= fun _ -> expression () >>= fun _ -> operator (schar ')') >>= fun _ -> statement () >>= fun _ -> noop ] )

and iteration_statement _ = label "iteration_statement" 
  (  reserved (string "while") >>= fun _ -> operator (schar '(') >>= fun _ -> expression () >>= fun _ -> operator (schar ')') >>= fun _ -> statement () >>= fun _ -> noop ;
	     reserved (string "do") >>= fun _ -> statement () >>= fun _ -> reserved (string "while") >>= fun _ -> operator (schar '(') >>= fun _ -> expression () >>= fun _ -> operator (schar ')') >>= fun _ -> operator (schar ';') >>= fun _ -> noop ;
	     reserved (string "for") >>= fun _ -> operator (schar '(') >>= fun _ -> expression_statement () >>= fun _ -> expression_statement () >>= fun _ -> maybe (expression ()) >>= fun _ -> operator (schar ')') >>= fun _ -> statement () >>= fun _ -> noop ] )

and jump_statement _ = label "jump_statement" 
  (  reserved (string "goto") >>= fun _ -> any_identifier >>= fun _ -> operator (schar ';') >>= fun _ -> noop ;
	     reserved (string "continue") >>= fun _ -> operator (schar ';') >>= fun _ -> noop ;
	     reserved (string "break") >>= fun _ -> operator (schar ';') >>= fun _ -> noop ;
	     reserved (string "return") >>= fun _ -> maybe (expression ()) >>= fun _ -> operator (schar ';') >>= fun _ -> noop ] )

and translation_unit _ = label "translation_unit" 
    (  external_declaration () >>= fun _ -> noop ;
	       translation_unit () >>= fun _ -> external_declaration () >>= fun _ -> noop ] )

and external_declaration _ = label "external_declaration" 
  (  ( function_definition () >>= fun _ -> noop ) ;
	     declaration () >>= fun _ -> noop ] )

and function_definition _ = label "function_definition" 
  (  ( declaration_specifiers () >>= fun _ -> declarator () >>= fun _ -> maybe (declaration_list ()) >>= fun _ -> compound_statement () >>= fun _ -> noop ) ;
	     declarator () >>= fun _ -> maybe (declaration_list ()) >>= fun _ -> compound_statement () >>= fun _ -> noop ] )

open IO



let _ = with_file_in (Sys.argv.(0))
(fun x -> run_filter (translation_unit ()) ~newline:'\n' (enum_char x))
