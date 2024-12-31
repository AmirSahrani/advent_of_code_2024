let s =
  List.fold x ~f:(fun y -> function
    | Aconstructor avalue -> afunction avalue
    | Bconstructor bvalue -> bfunction bvalue )

let f _ = (function x -> x + 1)
let f _ = function x -> x + 1
let f _ = fun _ -> (function x -> x + 1)
let f _ = fun _ -> function x -> x + 1
let f _ = (fun _ -> (function x -> x + 1))
let f _ = (fun _ -> function x -> x + 1)
let f _ = (fun _ -> fun x -> x + 1)
let f _ = fun _ -> fun x -> x + 1
let f _ = (fun _ -> (fun x -> x + 1))
let f _ = fun _ -> (fun x -> x + 1)

let _ = let f _ = (function x -> x + 1) in ()
let _ = let f _ = function x -> x + 1 in ()
let _ = let f _ = fun _ -> (function x -> x + 1) in ()
let _ = let f _ = fun _ -> function x -> x + 1 in ()
let _ = let f _ = (fun _ -> (function x -> x + 1)) in ()
let _ = let f _ = (fun _ -> function x -> x + 1) in ()
let _ = let f _ = (fun _ -> fun x -> x + 1) in ()
let _ = let f _ = fun _ -> fun x -> x + 1 in ()
let _ = let f _ = (fun _ -> (fun x -> x + 1)) in ()
let _ = let f _ = fun _ -> (fun x -> x + 1) in ()

class c = let f _ = (function x -> x + 1) in object end
class c = let f _ = function x -> x + 1 in object end
class c = let f _ = fun _ -> (function x -> x + 1) in object end
class c = let f _ = fun _ -> function x -> x + 1 in object end
class c = let f _ = (fun _ -> (function x -> x + 1)) in object end
class c = let f _ = (fun _ -> function x -> x + 1) in object end
class c = let f _ = (fun _ -> fun x -> x + 1) in object end
class c = let f _ = fun _ -> fun x -> x + 1 in object end
class c = let f _ = (fun _ -> (fun x -> x + 1)) in object end
class c = let f _ = fun _ -> (fun x -> x + 1) in object end

open struct
  [@@@ocamlformat "let-binding-deindent-fun=true"]

  let _ =
    let _ = function
      | Partial _ -> (
          fun {target} ->
            match target with
            | Lazy key -> Val_ref.of_key key
            | Lazy_loaded {v_ref; _} | Dirty {v_ref; _} -> v_ref )
    in
    ()

  let _ = function
    | Partial _ -> (
        fun {target} ->
          match target with
          | Lazy key -> Val_ref.of_key key
          | Lazy_loaded {v_ref; _} | Dirty {v_ref; _} -> v_ref )
end

let _ = call ~f:(fun pair -> (pair : a * b))

;;
f
  (fun _ -> function
    | true ->
        let () = () in
        () | false -> ())
  ()

;;
f
  (fun _ -> function
    | true ->
        let () = () in
        ()
        (* comment *) | false -> ())
  ()

let _ = fun (x : int as 'a) -> (x : int as 'a)

let f = function
  | Foo -> bar
  | EArr l ->
      EArr
        (List.map l ~f:(function
          | ElementHole -> ElementHole
          | Element e -> Element (m#expression e)
          | ElementSpread e -> ElementSpread (m#expression e)))