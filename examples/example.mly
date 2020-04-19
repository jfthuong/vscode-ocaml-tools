%{
open Osc

let osc_type_of_ident = function
"int" -> NumericType Int
(* Types *)
| "int64" -> NumericType Int64
| "uint" -> NumericType Uint
| "uint64" -> NumericType Uint64
| "acceleration" -> PhysicalType Acceleration
| "angle" -> PhysicalType Angle
| "angularspeed" -> PhysicalType AngularSpeed
| "distance" -> PhysicalType Distance
| "speed" -> PhysicalType Speed
| "temperature" -> PhysicalType Temperature
| "time" -> PhysicalType Time
| "weight" -> PhysicalType Weight
| "bool" -> BoolType
| "string" -> StringType
| "junction" -> JunctionType
| "segment" -> SegmentType
| n -> NamedType n

 let path_expression_head_of_ident = function
 "it" -> It
 (* | "outer" -> Outer *)
 | id -> Regular (Identifier id)

let a = begin
    print_line "a";
    print_line "b"
end

%}

%token <string> IDENTIFIER
%token COL SCOL COMA DOT BANG AT QUESTION
%token NL EOF
/* Keywords */
/*      Statements */
%token IMPORT ACTOR TYPE STRUCT SCENARIO MODIFIER EXTEND
/*      Scenario expressions */
%token DO FIRSTOF MATCH MIX MULTIMATCH PARALLEL SERIAL REPEAT

%token TRUE

%start statements
%type <Osc.statements> statements


%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main

%%

main:
    expr EOL                { $1 }
;
expr:
    INT                     { $1 }
    | LPAREN expr RPAREN      { $2 }
    | expr PLUS expr          { $1 + $3 }
    | expr MINUS expr         { $1 - $3 }
    | expr TIMES expr         { $1 * $3 }
    | expr DIV expr           { $1 / $3 }
    | MINUS expr %prec UMINUS { - $2 }
;

statements:
    EOF {[]}
    | statement statements {$1::$2}
    | empty_lines statements {$2}
    | statements empty_lines {$1}

statement:
    IMPORT IDENTIFIER NL {Import $2}
    | structure {Structure $1}
    | enumerated_type {EnumeratedType $1}

structure:
    STRUCT IDENTIFIER NL {
        {
            struct_name = Name $2;
            base_struct_type = None;
            struct_members = []
        }
    }
    | STRUCT IDENTIFIER LIKE IDENTIFIER NL {
        {
            struct_name = Name $2;
            base_struct_type = Some (NamedType $4);
            struct_members = []
        }
    }

struct_member: field {StructField $1}



name: IDENTIFIER {Name $1}
field_name: IDENTIFIER {FieldName $1}
event_name: IDENTIFIER {EventName $1}

path_expression_step :
    WHEN LPAR when_params RPAR {WhenExpr $3}
    | IDENTIFIER {Identifier $1}
/*
    | MethodCall (* TODO *)
    | ArrayElementReference (* TODO *)
*/

%%

