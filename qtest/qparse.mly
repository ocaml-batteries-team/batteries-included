%{
open Core;;
%}

%token <string> ID PARAM UID
%token COMMA AS EOF EOF2 SEMI

%start metaheader_
%type <Core.metaheader> metaheader_

%start modules_
%type <bool * string list> modules_

%%

/* manipulation pragma */

modules_:
| modules EOF2 { true , $1 }
| modules EOF  { false, $1 };

modules:
| UID                 { [$1] }
| UID  COMMA modules  { $1 :: $3 }
| error               { raise Modules_syntax_error };

/* header stuff */

metaheader_ : metaheader param EOF { {mhb = $1; mhpar = $2} };

param:
| PARAM     { $1 }
| /* nil */ { "" };

metaheader: /* x,y,z as target; a,b,c as tata ; ... */
| /* gnu */                 { [] }
| multibind                 { [$1] }
| multibind SEMI metaheader { $1 ::  $3 };

multibind : /* x,y,z as target */       
| functions AS ID { $1, $3 }/* foo -> foo as foo */
| functions       { $1, List.hd $1 };

functions: /* x,y,z */
| ID                  { [$1] }
| ID COMMA functions  { $1 :: $3 };

%%