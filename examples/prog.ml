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

let a = begin
    print_line "a";
    print_line "b"
end