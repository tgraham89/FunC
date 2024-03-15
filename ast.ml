type bop = Add 
          | Sub 
          | Equal 
          | Neq 
          | Less 
          | And 
          | Or 
          | Greater
          | Mult
          | Div


type typ = Int 
          | Bool 
          | String 
          | Char 
          | Void
          | Float 
          | List of typ


type expr =
  | Literal of int
  | BoolLit of bool
  | StrLit of string
  | ChrLit of char
  | FloatLit of float
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | ListLit of expr list


type bind = Decl of typ * string 
            | Defn of typ * string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Bind of bind 
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of bind * expr * stmt * stmt

type program = {
  body: stmt list;
}

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"
  | Mult -> "*"
  | Div -> "/"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> s
  | ChrLit(c) -> String.make 1 c
  | FloatLit(f) -> string_of_float f
  | ListLit(lst) -> let rec help = function 
      [] -> "" 
      | x :: [] -> string_of_expr x
      | x :: xs -> string_of_expr x ^ ", "
      in "[" ^ (help lst) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e


let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Void -> "void"
  | Float -> "float"
  | List t -> "list<" ^ (string_of_typ t) ^ ">"


let string_of_bind = function
    Defn(t, id, value) -> (string_of_typ t) ^ " " ^ id ^ " = " ^ (string_of_expr value)
    | Decl(t, id) -> (string_of_typ t) ^ " " ^ id

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Bind(bnd) -> string_of_bind bnd
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(i, e, s1, s2) -> "for (" ^ string_of_bind i ^ " " ^ (string_of_expr e) ^ " " ^ (string_of_stmt) s1 ^ ")" ^ string_of_stmt s2

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n"
