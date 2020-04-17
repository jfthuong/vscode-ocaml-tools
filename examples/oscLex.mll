{
open Lexing 
open OscParser

let tab_size = 4

let count_ws str = 
    let pos = ref 0 in
    let char = function 
        ' ' -> incr pos
        |'\t' -> pos := (!pos + tab_size) mod tab_size
        | _ -> ()
    in
    let () = String.iter char str in
    !pos / tab_size (* Check for errors and raise error here if mod <> 4 *)

let trim s = (* remove first and last char from string s. Keeps whole s if s is too small *)
    try 
        String.sub s 1 ((String.length s) - 2 )
    with Invalid_argument _ -> s
}

let digit = [ '0' - '9' ]
let identifier = [ 'a'-'z' 'A'-'Z' '0'-'9' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ] *
let nl = '\n' | "\r\n" 
let comment = '#' [ ^ '\n''\r'] *
let ws = [' ''\t''\x0B''\x0C']
let line_continuation = '\\' ws+ nl ws+
let string = '"' [^ '"' ]* '"'
let int = '-'? digit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let mantiss = digit+ '.' digit* | '.' digit+
let float = '-'? mantiss exp ?

rule token = parse
nl { NL}
| ws+ { SPACES (count_ws (lexeme lexbuf)) }
| ws* comment { token lexbuf }
| line_continuation { token lexbuf }
| string {STRING (trim (lexeme lexbuf)) }
| '.' { DOT }
| int { INT(int_of_string (lexeme lexbuf)) }
(* | float { print_endline (lexeme lexbuf) ; FLOAT(float_of_string (lexeme lexbuf)) } *)
| ':' { COL } 
| '!' { BANG }
| '@' { AT }
| '?' { QUESTION }
| ';' { SCOL }
| ',' { COMA }
| "==" { BCOMPEQ }
| "!=" { BCOMPDIFF }
| '=' { EQ }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRA }
| ']' { RBRA }
| "=>" { IMPLY }
| ">=" { BCOMPGTE }
| '>' { BCOMPGT }
| "<=" { BCOMPLTE }
| '<' { BCOMPLT }
| ".." { RANGE }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT }
| "/" { DIV }
| "%" { MOD }
(* keywords *)
| "else" { ELSE }
| "emit" { EMIT }
| "empty" { EMPTY }
| "event" { EVENT }
| "extend" { EXTEND }
| "external" { EXTERNAL }
| "false" { FALSE }
| "first_of" { FIRSTOF }
| "actor" { ACTOR }
| "and" { AND }
| "any" { ANY }
| "call" { CALL }
| "cover" { COVER }
| "def" { DEF }
| "default" { DEFAULT }
| "do" { DO }
| "if" { IF }
| "import" { IMPORT }
| "in" { IN }
| "is" { IS }
| "is also" { ISALSO }
| "is first" { ISFIRST }
| "is only" { ISONLY }
| "keep" { KEEP }
| "like" { LIKE }
| "match" { MATCH }
| "mix" { MIX }
| "modifier" { MODIFIER }
| "multi_match" { MULTIMATCH }
| "not" { NOT }
| "null" { NULL }
| "on" { ON }
| "one_of" { ONEOF }
| "or" { OR }
| "parallel" { PARALLEL }
| "properties" { PROPERTIES }
| "repeat" { REPEAT }
| "sample" { SAMPLE }
| "scenario" { SCENARIO }
| "serial" { SERIAL }
| "struct" { STRUCT }
| "soft" { SOFT }
| "true" { TRUE }
| "try" { TRY }
| "type" { TYPE }
| "undefined" { UNDEFINED }
| "until" { UNTIL }
| "wait" { WAIT }
| "when" { WHEN }
| "with" { WITH }
(* Types *)
| "list of" { LISTOF }
| identifier { IDENTIFIER (lexeme lexbuf) }
| eof {EOF}

{
let track_ws = ref true
let current_indent_lvl = ref 0
let token_queue = ref []

let indent () =
    incr current_indent_lvl;
    INDENT

let dedent () =
    current_indent_lvl := !current_indent_lvl -1 ;
    DEDENT

let push_token tk =
    token_queue := tk :: !token_queue

let pop_token () = 
    match !token_queue with
    [] -> None
    | h::t -> token_queue := t ; Some h

exception IndentError of int

let dedent_to insert_nl lvl = 
    let rec aux () =
        if !current_indent_lvl <= lvl
        then ()
        else 
            let () = push_token (dedent () ) in
            aux ()
    in
    aux ();
    if insert_nl
    then push_token NL
    else () ;
    match pop_token () with
    Some tk -> tk
    | None -> raise (IndentError 1)

let rec indent_by nb_levels = 
    if nb_levels <=1
    then indent()
    else 
        let () = push_token (indent()) in
        indent_by (nb_levels - 1)



let rec indented_token lexbuf =
    match pop_token () with (* Check if we have some (Dedent or Eof) tokens *)
        Some tk -> tk (* Yes: return int*)
        | None ->
            match token lexbuf with (* No: get a fresh one *)
                NL -> track_ws := true ; NL
                | SPACES lvl -> 
                    if !track_ws 
                    then 
                        let () = track_ws := false in
                        if lvl = !current_indent_lvl 
                        then indented_token lexbuf
                        else 
                            if lvl > !current_indent_lvl
                            then indent_by (lvl - !current_indent_lvl)
                            else 
                                if lvl < !current_indent_lvl 
                                then dedent_to false lvl 
                                else raise (IndentError 0) 
                    else indented_token lexbuf
                | EOF ->
                    push_token EOF;                    
                    dedent_to (not !track_ws) 0; 
                     (* we reached an end of file without NL at the end *)
                    
                
                | any ->
                    if !track_ws then (
                        track_ws := false ;
                        push_token any ;
                        dedent_to false 0 )
                    else any
                    
}