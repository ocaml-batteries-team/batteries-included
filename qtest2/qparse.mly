%{
open Core;;
%}

%token <string> ID
%token COMMA AS EOF SEMI

%start metaheader_
%type <Core.metaheader> metaheader_

%%

metaheader_ : metaheader EOF { {mhb = $1} };

metaheader: /* x,y,z as target; a,b,c as tata ; ... */
| multibind { [$1] }
| multibind SEMI metaheader { $1 ::  $3 }

multibind : /* x,y,z as target */       
| functions AS ID { $1, $3 }/* foo -> foo as foo */
| functions { $1, List.hd $1 };

functions: /* x,y,z */
| ID { [$1] }
| ID COMMA functions { $1 :: $3 };

%%