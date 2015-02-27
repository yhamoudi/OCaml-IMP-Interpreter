{
open Parser;;        
}

rule token = parse
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les espaces, tabulations et retour à la ligne *)

  | ['A'-'Z']+ as s		{ VAR s }	
  | ['0'-'9']+ as s 	{ INT (int_of_string s) } 
  | '('             	{ LPAREN }
  | ')'            		{ RPAREN }
  
  | '+'             	{ PLUS }
  | '-'					{ MINUS }
  | '*'             	{ TIMES }
  | '/'					{ DIV }

  | "true"				{ BOOL true }
  | "false"				{ BOOL false }  
  | '<'					{ INF }
  | '>'					{ SUP }
  | '='					{ EQU }
  | "and"				{ AND }
  | "or"				{ OR }
  | '~'					{ NOT }
  
  | ';'					{ SEQ }
  | "while"				{ WHILE }
  | "do"				{ DO }
  | "if"				{ IF }
  | "then"				{ THEN }
  | "else"				{ ELSE }
  | ":="				{ AFF }  
  | "print"				{ PRINT }
  | "skip"				{ SKIP }

  | '.'					{ POINT }
  | "function" 			{ FUN }
  | ','					{ COMMA }
  | "return"			{ RETURN }
  | ['a'-'z']+ as s	    { FNAME s}
  
  | eof            		{ EOF } 
  
