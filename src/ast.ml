type bop = Add 
          | Sub 
          | Equal 
          | Neq 
          | Less 
          | Lequal
          | And 
          | Or 
          | Not
          | Greater
          | Gequal
          | Mult
          | Div
          | Mod
          | Vbar
          | Dot

type unary_operator =
  | Pos
  | Neg
  | Bang

type expr =
  Literal of int
  | BoolLit of bool
  | StrLit of string
  | ChrLit of char
  | FloatLit of float
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | ListLit of expr list
  | StructId of string
  | Function of bind list * stmt list
  | Call of expr * expr list
  (* | StructCreate of string * stmt list *)
  (* | StructAccess of string * string *)
  | StructAccess of expr
  | StructAssign of expr list (* Used to define an instance of a struct *)
  | Zero
  | UnaryOp of unary_operator * expr
and
func = {
  formals : bind list;
  bdy : stmt list;
}
and
bind = Decl of typ * string 
       | Defn of typ * string * expr
and
typ = 
  Int 
  | Bool 
  | String 
  | Char 
  | Void
  | Float 
  | List of typ
  | StructSig of string (* Used for defining an instance of a struct *)
  | StructMem of string * bind list (* Used for storing the members of astruct *)
  | FunSig of typ list * typ
  | EmptyList
  | Struct
and
stmt =
  | Block of stmt list
  | Expr of expr
  | Bind of bind 
  | If of expr * stmt
  | IfElse of expr * stmt * stmt
  | While of expr * stmt
  | For of bind * expr * expr * stmt
  | Return of expr
  (* | Struct_decl of string * bind list *)
  | StructDecl of {   (* Used for defining a new struct *)
        sname: string;
        members: bind list;
        }
(* and
func = {
  name : string;
  ret : typ;
  num_args : int;
  args : bind list;
  body : stmt list;
} *)
and
program = {
  body: stmt list;
}

(* Pretty-printing functions *)
let string_of_op = function
  Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Lequal -> "<="
  | Greater -> ">"
  | Gequal -> ">="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Vbar -> "|"
  | Dot -> "."

let rec string_of_expr = function
  Zero -> "0"
  | Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> "\"" ^ s ^ "\""
  | ChrLit(c) -> String.make 1 c
  | FloatLit(f) -> string_of_float f
  | ListLit(lst) -> "[" ^ string_of_expr_list ", " lst ^ "]" 
  | StructId(s) -> s 
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Function(args, body) -> "(" ^ string_of_bind_list ", " args ^ ") {\n" ^ string_of_stmt_list "" body ^ "}"
  | Call(x, args) -> begin match x with
      Id id -> id ^ "(" ^ string_of_expr_list ", " args ^ ")"
      | Function(_, _) as y -> string_of_expr y ^ "(" 
        ^ string_of_expr_list ")(" args ^ ")"
      | _ -> raise (Failure "only functions are callable types") end
  | StructAssign(e) -> "{\n" ^ string_of_expr_list ",\n" e ^ ",\n}"
  | StructAccess(str) -> string_of_expr str
  | UnaryOp(op, ex) -> match op with Neg -> "-(" ^ string_of_expr ex ^ ")"
                                    | Pos -> "+(" ^  string_of_expr ex ^ ")"
                                    | Bang -> "!(" ^ string_of_expr ex ^ ")"
and
string_of_expr_list delim = function
  [] -> ""
  | x :: [] -> string_of_expr x
  | x :: rest -> string_of_expr x ^ delim ^ string_of_expr_list delim rest
and
string_of_typ = function
  Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Void -> "void"
  | Float -> "float"
  | Struct -> "struct" (* Used for noting that a variable is in a struct *)
  | List t -> "list<" ^ (string_of_typ t) ^ ">"
  | StructSig (name) -> name      (* Used for defining an instance of a struct *)
  | StructMem (name, members) -> name ^ " " ^ string_of_bind_list "," members    (* Used for storing the members of astruct *)
  | FunSig (args, ret) -> "function<" ^ string_of_typ_list ", " args ^ "> -> " ^ string_of_typ ret 
  | EmptyList -> "[]"
and
string_of_typ_list delim = function
  [] -> ""
  | x :: [] -> string_of_typ x
  | x :: rest -> string_of_typ x ^ delim ^ string_of_typ_list delim rest
and
string_of_bind = function
    Defn(t, id, value) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr value
    | Decl(t, id) -> string_of_typ t ^ " " ^ id
and
string_of_bind_list delim = function
  [] -> ""
  | x :: [] -> string_of_bind x
  | x :: rest -> string_of_bind x ^ delim ^ string_of_bind_list delim rest
and
string_of_stmt = function
  Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s 
  | IfElse(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Bind(bnd) -> string_of_bind bnd ^ ";\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(i, e1, e2, s) -> 
    "for (" ^ string_of_bind i ^ "; " ^ string_of_expr e1 ^ "; " 
    ^ string_of_expr e2 ^ ") {\n" ^ string_of_stmt s ^ "}"
  | Return(expr) -> "\treturn " ^ string_of_expr expr ^ ";\n"
  | StructDecl(s) -> "struct " ^ s.sname ^ " {\n" ^ string_of_bind_list ",\n" s.members ^ ",\n};\n"
  (* | Struct_decl(n, v) -> "struct " ^ n ^ " {\n" ^ string_of_bind_list "," v ^ "\n};" *)
and string_of_stmt_list delim = function
  [] -> ""
  | x :: [] -> string_of_stmt x
  | x :: rest -> delim ^ string_of_stmt x ^ string_of_stmt_list delim rest
let string_of_program (fdecl : program) =
  String.concat "" (List.map string_of_stmt fdecl.body)
