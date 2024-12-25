let () = [%ext expr] ; ()

let _ = (match%ext x with () -> ()) [@attr y]

let _ =
  match%ext x with () ->
    let y = [%test let x = y] in
    let%test x = d in
    d

val f : compare:[%compare : 'a] -> sexp_of:[%sexp_of : 'a] -> t

let invariant t =
  Invariant.invariant [%here] t [%sexp_of : t] (fun () ->
      assert (check_t_invariant t) )

;; [%e
     ? ( xxxxxxxxx
       , xxxxxxxxxxxxx
       , xxxxxxxxxxxxxxxx
       , xxxxxxxxxxxxxx
       , xxxxxxxxxxx
       , xxxxxxxxxxxxxxxxxxxx )]

;; [%e
     ? ( xxxxxxxxx
       , xxxxxxxxxxxxx
       , xxxxxxxxxxxxxxxx
       , xxxxxxxxxxxxxx
       , xxxxxxxxxxx
       , xxxxxxxxxxxxxxxxxxxx ) when a < b]


[%ext let f = () and g () = () in e]
(let%ext f = () and g () = () in e)


[%ext let rec f = () and g () = () in e]
(let%ext rec f = () and g () = () in e);;

let _ = ([%ext? (x:x)] : [%ext? (x:x)]);;


[%%ext
11111111111111111111]

[%%ext
11111111111111111111111 22222222222222222222222 33333333333333333333333]

[%%ext
;; 11111111111111111111

;; 22222222222222222222]

[%%ext
;; 11111111111111111111

;; 22222222222222222222

;; 33333333333333333333]

[%%ext
let foooooooooooooooo = foooo

let fooooooooooooooo = foo]

let _ = [%stri
  let [%p xxx] =
    fun (t : [%t tt]) (ut : [%t tt]) ->
      [%e xxx]]


let _ =
  [ x;
    x ---> [%expr [%e x ~loc [%expr x] x] ; iter tail]
    ;x]


let _ =
  [%expr let x = e in f y][@x]


let _ = f (for i = 0 to 1 do () done) (while true do () done)
let _ = f (for%ext i = 0 to 1 do () done) (while%ext true do () done)

let _ = function%ext x -> x

let _ = f (function%ext x -> x)

let _ = f (function%ext x -> x) x

let _ = [%ext function x -> x]

let _ = f [%ext function x -> x]

let _ = f [%ext function x -> x] x

let _ = f ([%ext e] [@attr]) x

let _ = a ;%ext b ; [%ext (a ; b)]

let _ =
  try%lwt Lwt.return 2
  with _ -> assert false

let _ =
  (* foooooooooooo *)
  try%lwt
    (* fooooooooooo *)
    Lwt.return 2
  with _ -> assert false

let _ =
  try%lwt
    let a = 3 in
    Lwt.return a
  with _ -> assert false

let _ =
  (* foooooooooooo *)
  try%lwt
    (* fooooooooooo *)
    let a = 3 in
    Lwt.return a
  with _ -> assert false

let%lwt f = function
  | _ -> ()

type%any_extension t =
  < a: 'a >

let value =
  f
    [%any_extension
      function
      | 0 -> false
      | _ -> true
    ]

let value =
    [%any_extension
      fun x -> y
    ]
      x

let value =
  f
    [%any_extension
      try x with
      | x -> false
      | _ -> true
    ]

let value =
  f
    [%any_extension
      match x with
      | x -> false
      | _ -> true
    ]

let foo =
  [%foooooooooo
    fooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooo]
  [@@foooooooooo
    fooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooo]

;;
[%%foooooooooo:
  fooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooo]

[@@@foooooooooo
  fooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooo]

let _ = [%ext let+ a = b in c]

let _ = (begin%ext "foo"; "bar" end)

let this_function_has_a_long_name plus very many arguments = "and a kind of long body"

[%%expect
  {||}]

;;
[%expect {|
___________________________________________________________
|}]

[%%expect
  {|
___________________________________________________________
|}]

let () =
  (* -1 *) begin (* 0 *) % (* 0.5 *) test (* 1 *) [@foo (* 2 *) "bar"] (* 3 *) end

open%ext M
[%%ext open M]
open! %ext M
[%%ext open! M]
include%ext M
[%%ext include M]

let x =
  let open%ext M in
  x

let x =
  [%ext
    let open M in
    x]

let x =
  let open! %ext M in
  x

let x =
  [%ext
    let open! M in
    x]

exception%ext E
[%%ext exception E]

let _ =
  let exception%ext E in
  x

let _ =
  [%ext
    let exception E in
    x]

module%ext E = P
[%%ext module E = P]

module%ext rec K =A
and L = A

[%%ext module rec K =A
and L = A]

let _ =
  let module%ext E = P in
  x

let _ =
  [%ext
    let module E = P in
    x]

module type%ext E = P
[%%ext module type E = P]

class%ext x = y
[%%ext class x = y]

class%ext x = y
and y = z

[%%ext
  class x = y
  and y = z]

class type%ext x = y
[%%ext class type x = y]

class type%ext x = y
and y = z

[%%ext class type x = y
and y = z]

let _ = (* bar *) [%expr (* comment *) foo (* blabla *)]

let _ = assert%lwt false
let _ = [%lwt assert false]
let _ = f (assert%lwt false)
let _ = f [%lwt assert false]
let _ = (assert%lwt false) [@attr]
let _ = [%lwt assert false] [@attr]
let _ = f ((assert%lwt false) [@attr])
let _ = f ([%lwt assert false] [@attr])

let _ = lazy%ext e
let _ = [%ext lazy e]
let _ = f (lazy%ext e)
let _ = f [%ext lazy e]
let _ = (lazy%ext e) [@attr]
let _ = [%ext lazy e] [@attr]
let _ = f ((lazy%ext e) [@attr])
let _ = f ([%ext lazy e] [@attr])

let _ = object%ext method x = y end
let _ = [%ext object method x = y end]
let _ = f (object%ext method x = y end)
let _ = f [%ext object method x = y end]
let _ = object%ext method x = y end [@attr]
let _ = [%ext object method x = y end] [@attr]
let _ = f ((object%ext method x = y end) [@attr])
let _ = f ([%ext object method x = y end] [@attr])

let _ = if%ext x then y else z
let _ = [%ext if x then y else z]
let _ = f (if%ext x then y else z)
let _ = f [%ext if x then y else z]
let _ = (if%ext x then y else z) [@attr]
let _ = [%ext if x then y else z] [@attr]
let _ = f ((if%ext x then y else z) [@attr])
let _ = f ([%ext if x then y else z] [@attr])

let _ = match%ext x with _ -> x
let _ = [%ext match x with _ -> x]
let _ = f (match%ext x with _ -> x)
let _ = f [%ext match x with _ -> x]
let _ = (match%ext x with _ -> x) [@attr]
let _ = [%ext match x with _ -> x] [@attr]
let _ = f ((match%ext x with _ -> x) [@attr])
let _ = f ([%ext match x with _ -> x] [@attr])

let _ = try%ext x with _ -> x
let _ = [%ext try x with _ -> x]
let _ = f (try%ext x with _ -> x)
let _ = f [%ext try x with _ -> x]
let _ = (try%ext x with _ -> x) [@attr]
let _ = [%ext try x with _ -> x] [@attr]
let _ = f ((try%ext x with _ -> x) [@attr])
let _ = f ([%ext try x with _ -> x] [@attr])

let _ = fun%ext x -> x
let _ = [%ext fun x -> x]
let _ = f (fun%ext x -> x)
let _ = f [%ext fun x -> x]
let _ = (fun%ext x -> x) [@attr]
let _ = [%ext fun x -> x] [@attr]
let _ = f ((fun%ext x -> x) [@attr])
let _ = f ([%ext fun x -> x] [@attr])

let _ = function%ext x -> x
let _ = [%ext function x -> x]
let _ = f (function%ext x -> x)
let _ = f [%ext function x -> x]
let _ = (function%ext x -> x) [@attr]
let _ = [%ext function x -> x] [@attr]
let _ = f ((function%ext x -> x) [@attr])
let _ = f ([%ext function x -> x] [@attr])

let _ = new%ext x
let _ = [%ext new x]
let _ = f (new%ext x)
let _ = f [%ext new x]
let _ = (new%ext x) [@attr]
let _ = [%ext new x] [@attr]
let _ = f ((new%ext x) [@attr])
let _ = f ([%ext new x] [@attr])

let _ = (x ;%ext y)
let _ = [%ext x; y]
let _ = f (x ;%ext y)
let _ = f [%ext x; y]
let _ = (x ;%ext y) [@attr]
let _ = [%ext x; y] [@attr]
let _ = f ((x ;%ext y) [@attr])
let _ = f ([%ext x; y] [@attr])

let formula_base x =
  let open Formula.Infix in
  (Expr.typeof x) #== (Lit (Type IntType))
  #&& (x #<= (Expr.int 4))
  #&& ((Expr.int 0) #< x)

let xxxxxx =
  let%map (* _____________________________
             __________ *)()            = yyyyyyyy in
  { zzzzzzzzzzzzz }
