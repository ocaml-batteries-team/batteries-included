%{
open Core;;
%}

%token <string> ID PARAM UID
%token COMMA AS EOF SEMI IN FORALL LBRACKET RBRACKET

%start metaheader_
%type <Core.metaheader> metaheader_

%start modules_
%type <string list> modules_

%%

/* manipulation pragma */

modules_:
| modules EOF  { $1 };

modules:
| UID                 { [$1] }
| UID  COMMA modules  { $1 :: $3 }
| error               { raise Modules_syntax_error };

/* header stuff */

metaheader_ :
| metaheader param EOF { {mhb = $1; mhpar = $2} }
| metaheader /* */ EOF { {mhb = $1; mhpar = ""} };
  
param:
| PARAM param   { $1 ^ " " ^ $2 }
| PARAM         { $1 };

metaheader: /* x,y,z as target; a,b,c as tata ; ... */
| /* gnu */                 { [] }
| multibind                 { [$1] }
| multibind SEMI metaheader { $1 ::  $3 };

multibind : /* x,y,z as target */       
| FORALL ID IN LBRACKET functions RBRACKET  { $5, $2 }
| ID            { [$1], $1 }
| ID AS ID      { [$1], $3 }
| error         { failwith "Parser:Multibind" }

functions: /* x,y,z */
| ID                  { [$1] }
| ID SEMI functions   { $1 :: $3 };

%%