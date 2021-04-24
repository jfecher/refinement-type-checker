open Expr
open Z3
open Z3.Arithmetic

exception InvalidExprInAssert of expr

let translate_builtin_function = function
    | Identifier("+")  -> Some (fun ctx a b -> Arithmetic.mk_add ctx [a; b])
    | Identifier("-")  -> Some (fun ctx a b -> Arithmetic.mk_sub ctx [a; b])
    | Identifier("*")  -> Some (fun ctx a b -> Arithmetic.mk_mul ctx [a; b])
    | Identifier("/")  -> Some (Arithmetic.mk_div)
    | Identifier("%")  -> None
    | Identifier("==") -> Some (Boolean.mk_eq)
    | Identifier("!=") -> Some (fun ctx a b -> Boolean.mk_distinct ctx [a; b])
    | Identifier("<")  -> Some (Arithmetic.mk_lt)
    | Identifier("<=") -> Some (Arithmetic.mk_le)
    | Identifier(">")  -> Some (Arithmetic.mk_gt)
    | Identifier(">=") -> Some (Arithmetic.mk_ge)
    | _ -> None

let rec to_z3_expr ctx goal = function
    | IntLit(x) -> Integer.mk_numeral_i ctx x
    | BoolLit(b) -> Boolean.mk_val ctx b
    | Identifier(name) -> Expr.mk_const ctx (Symbol.mk_string ctx name) (Integer.mk_sort ctx)
    | Lambda(x, body) -> raise (InvalidExprInAssert(Lambda(x, body)))
    | Let(var, rhs, body) ->
        let x = Expr.mk_const ctx (Symbol.mk_string ctx var) (Integer.mk_sort ctx) in
        let eq = Z3.Boolean.mk_eq ctx x (to_z3_expr ctx goal rhs) in
        Goal.add goal [eq];
        to_z3_expr ctx goal body

    | If(cond, then', else') ->
        let c = to_z3_expr ctx goal cond in
        let then'' = Boolean.mk_implies ctx c (to_z3_expr ctx goal then') in
        let else'' = Boolean.mk_implies ctx (Boolean.mk_not ctx c) (to_z3_expr ctx goal else') in
        Boolean.mk_and ctx [then''; else'']

    | Assert(cond, body) ->
        let cond' = to_z3_expr ctx goal cond in
        let body' = to_z3_expr ctx goal body in
        Boolean.mk_and ctx [cond'; body']

    | FnCall(f, args) ->
        begin match translate_builtin_function f with
        | Some(f) ->
            assert (List.length args = 2);
            let x = List.hd args in
            let y = List.nth args 1 in
            f ctx (to_z3_expr ctx goal x) (to_z3_expr ctx goal y)
        | None ->
            let args_t = List.map (fun _ -> Integer.mk_sort ctx) args in
            let int_t = Integer.mk_sort ctx in
            let f' = Z3.FuncDecl.mk_func_decl_s ctx "foo" args_t int_t in
            let args = List.map (to_z3_expr ctx goal) args in
            Expr.mk_app ctx f' args
        end


let rec check_rec ctx goal = function
    | IntLit(x) -> ()
    | BoolLit(b) -> ()
    | Identifier(name) -> ()
    | Lambda(param, body) -> ()
    | FnCall(f, x) -> ()
    | Let(var, rhs, body) -> ()
    | If(cond, then', else') -> ()
    | Assert(cond, body) ->
        Z3.Goal.add goal [Boolean.mk_not ctx (to_z3_expr ctx goal cond)];
        check_rec ctx goal body

let check expr =
    let ctx = Z3.mk_context [] in
    let goal = Goal.mk_goal ctx true false false in
    check_rec ctx goal expr;
    let solver = Z3.Solver.mk_solver ctx None in

    let exprs = Goal.get_formulas goal in
    (if Solver.check solver exprs != Solver.SATISFIABLE then
        Printf.printf "Program is well-typed!\n"
    else (
        Printf.printf "Can be refuted, counter-example is:\n";
        begin match Solver.get_model solver with
        | Some(model) -> Printf.printf "%s\n" (Model.to_string model)
        | None -> Printf.printf "(Unable to make counter-example!)\n"
        end)
    );
    List.map Expr.to_string exprs
    |> List.fold_left (fun a b -> a ^ "\n" ^ b) ""
