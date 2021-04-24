(******************************************************************************
                       front-end for parsing exprs on
                        the command line and showing
                             their computed types
******************************************************************************)

open Expr
open Refinements

(* The classic read-eval-printline-loop *)
let rec main () =
    (try
        print_string "> ";
        read_line ()
        |> Lexer.parse
        |> Refinements.check
        |> print_endline
    with
       | Not_found -> print_endline "variable not defined"
       | Failure(_) -> print_endline "lexing failure, invalid symbol");
    main ()

let () = main ()


(* Tests

1:    \f.\x. f x  :  (a -> b) -> a -> b
2:    \f.\x. f (f x) : (a -> a) -> a -> a
(+):  \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
succ: \n.\f.\x. f (n f x)  :  ((a -> b) -> c -> a) -> (a -> b) -> c -> b
mult: \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
pred: \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)  :  (((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g

*)

(* Let generalization tests

\x. let y = x in y      : 'a -> 'a
\x. let y = \z.x in y   : 'a -> 'b -> 'a

*)
