%{
open Osc

let osc_type_of_ident = function
"int" -> NumericType Int
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
 | "outer" -> Outer
 | id -> Regular (Identifier id)

exception NotAUnit of string
let unit_of_identifier = function
(*Units *)
| "kphps" -> UAcceleration Kphps
| "mpsps" -> UAcceleration Mpsps
| "deg" -> UAngle Deg
| "rad" -> UAngle Rad
| "degree_per_second" -> UAngularSpeed DegreePerSecond
| "radian_per_second" -> UAngularSpeed RadianPerSecond
| "mm" -> UDistance Mm
| "cm" -> UDistance Cm
| "inch" -> UDistance Inch
| "feet" -> UDistance Feet
| "meter" -> UDistance Meter
| "km" -> UDistance Km
| "mile" -> UDistance Mile
| "kph" -> USpeed Kph
| "mph" -> USpeed Mph
| "mps" -> USpeed Mps
| "celsius" -> UTemperature Celsius
| "fahrenheit" -> UTemperature Fahrenheit
| "ms" -> UTime Ms
| "sec" -> UTime Sec
| "min" -> UTime Min
| "hr" -> UTime Hr
| "kg" -> UWeight Kg
| "ton" -> UWeight Ton
| unkown -> raise (NotAUnit unkown)

let event_path_of_string_list l =
    let rec last l = 
        match List.rev l with
            | h::t -> h, List.rev t
            | [] -> raise Parsing.Parse_error (* supposed to contain at least one element *)
    in
    let evt_name, path = last l in
    FieldPath path, EventName evt_name



%}

%token <string> IDENTIFIER 
%token COL SCOL COMA DOT BANG AT QUESTION
%token NL EOF
%token <int> SPACES
%token INDENT DEDENT
%token BCOMPEQ BCOMPDIFF BCOMPGTE BCOMPGT BCOMPLTE BCOMPLT
%token EQ
%token LPAR RPAR
%token LBRA RBRA
%token RANGE
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token PLUS MINUS MULT DIV MOD
/* Keywords */
/*      Statements */
%token IMPORT ACTOR TYPE STRUCT SCENARIO MODIFIER EXTEND
/*      Scenario expressions */
%token DO FIRSTOF MATCH MIX MULTIMATCH PARALLEL SERIAL REPEAT

%token ELSE
%token EMIT
%token EMPTY
%token EVENT
%token EXTERNAL
%token FALSE
%token AND
%token IMPLY
%token ANY
%token CALL
%token COVER
%token DEF
%token DEFAULT
%token IF
%token IN
%token IS
%token ISALSO
%token ISFIRST
%token ISONLY
%token KEEP
%token LIKE
%token NOT
%token NULL
%token ON
%token ONEOF
%token OR
%token PROPERTIES
%token SAMPLE
%token SOFT
%token TRUE
%token TRY
%token UNDEFINED
%token UNTIL
%token WAIT
%token WHEN
%token WITH
/*      Types */
%token LISTOF

%start statements
%type <Osc.statements> statements
%%
statements: 
    EOF {[]}
    | statement statements {$1::$2}
    | empty_lines statements {$2}
    | statements empty_lines {$1}

statement: 
    IMPORT IDENTIFIER NL {Import $2} 
    | structure {Structure $1}
    | enumerated_type {EnumeratedType $1}
    

enum_member: IDENTIFIER {$1, None}
    | IDENTIFIER EQ exp {$1, Some $3 }

empty_lines: 
    NL empty_lines {$2}
    | INDENT empty_lines {$2}
    | DEDENT empty_lines {$2}
    | NL {()}
    | INDENT{()}
    | DEDENT{()}

enum_members: 
     enum_member COMA enum_members {$1::$3}
    | enum_member empty_lines {[$1]}
    | enum_member {[$1]}
    | empty_lines enum_members {$2}
    | /* Empty */ {[]}


enumerated_type: TYPE IDENTIFIER COL LBRA enum_members RBRA NL{ 
        {enumerated_type_name= Name $2; enumerated_type_members= $5 }
    }


  

osc_type: 
    IDENTIFIER {osc_type_of_ident $1}
    | LISTOF osc_type { ListType $2}


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
    | STRUCT IDENTIFIER COL NL INDENT struct_members {
        {
            struct_name = Name $2;
            base_struct_type = None; 
            struct_members = $6
        }
    }
    | STRUCT IDENTIFIER LIKE IDENTIFIER COL NL INDENT struct_members{
        {
            struct_name = Name $2;
            base_struct_type = Some (NamedType $4);
            struct_members = $8
        }
    }

struct_members: struct_member DEDENT {[$1]}
    | struct_member struct_members {$1::$2}
    | NL struct_members {$2}
    | DEDENT {[]}
                
struct_member: field {StructField $1}

name: IDENTIFIER {Name $1}
field_name: IDENTIFIER {FieldName $1}
event_name: IDENTIFIER {EventName $1}

identifier_path: IDENTIFIER {[$1]}
    | IDENTIFIER DOT identifier_path { $1::$3 }

event_path: identifier_path {event_path_of_string_list $1}

clock: AT event_path {$2, None}
    | AT event_path IMPLY name {$2, Some $4 }
 
bool_exp: exp {BoolExp $1}
qualified_event: 
     /* Empty */ {{ condition= None; clock=None}}
    | bool_exp {{ condition= Some $1; clock=None}}
    | clock {{ condition= None; clock=Some $1}}
    | bool_exp clock {{ condition= Some $1; clock=Some $2}}

sample : SAMPLE LPAR exp COMA qualified_event RPAR { $3, $5}

field_decl: field_name {false, $1}
    | BANG field_name {true, $2}

field_type: osc_type {Some $1, None}
    | osc_type EQ sample {Some $1, Some $3}
    | EQ sample {None, Some $2}

field: field_decl COL field_type NL {
    let dng, fieldname = $1 in
    let field_type, field_sample = $3 in
    {
        do_not_generate= dng;
        field_name= fieldname;
        field_type= field_type;
        sample= field_sample;
        field_with= None;}}  

literal_value: INT {LVInt $1}
    | FLOAT {LVFloat $1}
literal: 
    STRING {String $1}
    | literal_value {LiteralValue $1}
    | literal_value IDENTIFIER { PhysicalValue ($1,unit_of_identifier $2) }
constant: 
    TRUE { True }
    | FALSE { False }
    | NULL { Null }
    | IDENTIFIER {NamedConstant $1}
    | literal {Literal $1}

artihmetics_op: 
    | PLUS { Plus }
    | MINUS { Minus }
    | MULT { Mult }
    | DIV { Div }
    | MOD { Mod }

bool_comparison: 
    BCOMPEQ {Eq}
    | BCOMPDIFF {Diff}
    | BCOMPLT {Lt}
    | BCOMPLTE {Lte}
    | BCOMPLT {Gt}
    | BCOMPLTE {Gte}

bool_compound: 
     AND { And }
    |  OR { Or }
    |  IMPLY { Imply }

function_call_param_list: 
    /* Empty */ {[]}
    | exp {[$1]}
    | exp COMA function_call_param_list {$1::$3}

exp : exp bool_comparison exp { BoolComparison($2, $1, $3) }
    | exp bool_compound exp { BoolCompound ($2,$1,$3) }
    | NOT exp { BoolNeg $2 }
    | BANG exp { BoolNeg $2 }
    | exp LBRA exp RBRA { ListIndexing($1,$3) }
    | LBRA exp RANGE exp RBRA { Range($2, $4) } 
    | exp QUESTION exp COMA exp { IfThenElse ($1, $3, $5) }
    | exp artihmetics_op exp { Arithmetics($2,$1,$3) }
    | MINUS exp {UnaryMinus $2}
    | constant {Constant $1}
    | LPAR exp RPAR { $2 }
    | path_expression {PathExpression $1}
    | IDENTIFIER LPAR function_call_param_list RPAR {FunctionCall ($1, $3)}

path_expression_step :
    WHEN LPAR when_params RPAR {WhenExpr $3}
    | IDENTIFIER {Identifier $1}
/*    
    | MethodCall (* TODO *)
    | ArrayElementReference (* TODO *)
*/

path_expression_steps: 
    path_expression_step {[$1]}
    | path_expression_step DOT path_expression_steps {$1::$3}

path_expression_head : 
    IDENTIFIER {path_expression_head_of_ident $1}
    | path_expression_step {Regular $1}


path_expression: path_expression_head {$1,[]}
    | path_expression_head DOT path_expression_steps {$1, $3}


when_params: 
    /* EMPTY */ {[]} /* Is this legal ? */
    | field_name COL exp { [$1,$3] }
    | field_name COL exp COMA when_params { ($1,$3)::$5}

%%

