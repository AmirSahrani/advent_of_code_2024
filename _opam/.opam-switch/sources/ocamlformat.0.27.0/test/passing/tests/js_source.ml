(* Signature items *)
module type S = sig
  class%foo x : t [@@foo]
  class type%foo x = x [@@foo]

end


include struct
  let%test_module "as" =
    (module struct
       let%expect_test "xx xx xxxxxx xxxxxxx xxxxxx xxxxxx xxxxxxxx xx xxxxx xxx xx xxxxx"
         =
         ()
       ;;
    end)
  ;;
end

;;
if fffffffffffffff aaaaa bb
then (if b then aaaaaaaaaaaaaaaa ffff)
else aaaaaaaaaaaa qqqqqqqqqqq

include Base.Fn  (** @open *)

let ssmap
    : (module MapT with type key = string and type data = string and type map = SSMap.map)
  =
  ()
;;

let ssmap
    :  (module MapT with type key = string and type data = string and type map = SSMap.map)
    -> unit
  =
  ()
;;

let _ = match x with | A -> [%expr match y with | e -> e]

let _ = match x with | A -> [%expr match y with | e -> match e with x -> x]

let _ =
  List.map rows ~f:(fun row ->
      Or_error.try_with (fun () -> fffffffffffffffffffffffff row))
;;

module type T = sig

  val find : t -> key -> value option (** @raise if not found. *)

  val f
   :  a_few : params
   -> with_long_names : to_break
   -> the_line : before_the_comment
   -> unit
   (** @param blablabla *)

end

open! Core

(** First documentation comment. *)
exception First_exception

(** Second documentation comment. *)
exception Second_exception

module M = struct
  type t
  [@@immediate]
  (* ______________________________________ *)
  [@@deriving variants, sexp_of]
end

module type Basic3 = sig
  type ('a, 'd, 'e) t

  val return : 'a -> ('a, _, _) t
  val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t

  val map
    : [ `Define_using_apply
      | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      ]
end

let _ =
  aa
    (bbbbbbbbb cccccccccccc dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd)
;;

let _ =
  "_______________________________________________________ _______________________________"
;;

let _ = [ very_long_function_name____________________ very_long_argument_name____________ ]


(* FIX: exceed 90 columns *)
let _ =
  [%str
    let () = very_long_function_name__________________ very_long_argument_name____________]
;;

let _ =
  { long_field_name = 9999999999999999999999999999999999999999999999999999999999999999999 }
;;

(* FIX: exceed 90 columns *)
let _ =
  match () with
  | _ ->
    (match () with
    | _ -> long_function_name long_argument_name__________________________________________)
;;

let _ =
  aaaaaaa
  (* __________________________________________________________________________________ *)
  := bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;;

let g = f ~x (* this is a multiple-line-spanning
                comment *) ~y

let f =
  very_long_function_name
    ~x:very_long_variable_name (* this is a multiple-line-spanning
       comment *)
    ~y
;;

let _ =
  match x with
  | { y =
        (* _____________________________________________________________________ *)
        ( X _ | Y _ )
    } -> ()
;;

let _ =
  match x with
  | { y =
        Z |
        (* _____________________________________________________________________ *)
        ( X _ | Y _ )
    } -> ()
;;

type t = [
  | `XXXX (* __________________________________________________________________________________ *)
  | `XXXX (* __________________________________________________________________ *)
  | `XXXX (* _____________________________________________________ *)
  | `XXXX (* ___________________________________________________ *)
  | `XXXX (* ___________________________________________________ *)
  | `XXXX (* ________________________________________________ *)
  | `XXXX (* __________________________________________ *)
  | `XXXX (* _________________________________________ *)
  | `XXXX (* ______________________________________ *)
  | `XXXX (* ____________________________________ *)
]

type t =
  { field : ty
  (* Here is some verbatim formatted text:
     {v
       starting at column 7
     v}*)
  }

module Intro_sort = struct
  let foo_fooo_foooo fooo ~foooo m1 m2 m3 m4 m5 =
    (* Fooooooooooooooooooooooooooo:
       {v
          1--o-----o-----o--------------1
             |     |     |
          2--o-----|--o--|-----o--o-----2
                   |  |  |     |  |
          3--------o--o--|--o--|--o-----3
                         |  |  |
          4-----o--------o--o--|-----o--4
                |              |     |
          5-----o--------------o-----o--5
        v} *)
    foooooooooo fooooo fooo;
    foooooooooo fooooo fooo;
    foooooooooo fooooo fooo;
  ;;
end

let _ =
  "_ _____________________ ___________ ________ _____________ ________ _____________ _____\n\n\
  \ ___________________"
;;

let nullsafe_optimistic_third_party_params_in_non_strict =
  CLOpt.mk_bool
    ~long:
      "nullsafe-optimistic-third-party-params-in-non-strict"
      (* Turned on for compatibility reasons. Historically this is because
         there was no actionable way to change third party annotations. Now
         that we have such a support, this behavior should be reconsidered,
         provided our tooling and error reporting is friendly enough to be
         smoothly used by developers. *)
    ~default:true
    "Nullsafe: in this mode we treat non annotated third party method \
     params as if they were annotated as nullable."

let foo () =
  if%bind
    (* this is a medium length comment of some sort *)
    this is a medium length expression of_some sort
  then x
  else y

let _ =
  match x with
  | _
    when f ~f:((function
      | _ -> .) [@ocaml.warning (* ....................................... *) "-4"]) -> y
;;

let[@a (* .............................................. ........................... .......................... ...................... *) foo (* ....................... *) (* ................................. *) (* ...................... *)] _ =
  match[@ocaml.warning (* ....................................... *) "-4"] x[@attr (* .......................... .................. *) some_attr] with
  | _
    when f
        ~f:((function
            | _ -> .) [@ocaml.warning (* ....................................... *) "-4"])
        ~f:((function
            | _ -> .) [@ocaml.warning (* ....................................... *)  (* ....................................... *) "foooooooooooooooooooooooooooo fooooooooooooooooooooooooooooooooooooo"])
        ~f:((function
            | _ -> .) [@ocaml.warning (* ....................................... *) let x = a and y = b in x + y])
    -> y[@attr (* ... *) (* ... *) attr (* ... *)]
;;

let x =
  foo (`A b) ~f:(fun thing ->
    something that reaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaally needs wrapping)
;;

let x =
  foo
    (`A `b)
    ~f:(fun thing ->
      something that reaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaally needs wrapping)
;;

let x =
  foo [ A; B ] ~f:(fun thing ->
    something that reaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaally needs wrapping)
;;

let x =
  foo
    [ [ A ]; B ]
    ~f:(fun thing ->
      something that reaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaally needs wrapping)
;;

let x =
  f
    ("A string _____________________"
    ^ "Another string _____________"
    ^ "Yet another string _________")
;;

let x =
  some_fun________________________________
    some_arg______________________________ (fun param ->
    do_something ();
    do_something_else ();
    return_this_value)

let x =
  some_fun________________________________
    some_arg______________________________ ~f:(fun param ->
    do_something ();
    do_something_else ();
    return_this_value)

let x =
  some_value
  |> some_fun (fun x ->
       do_something ();
       do_something_else ();
       return_this_value)

let x =
  some_value
  ^ some_fun (fun x ->
      do_something ();
      do_something_else ();
      return_this_value)

let bind t ~f =
  unfold_step
    ~f:(function
      | Sequence { state = seed; next }, rest ->
        (match next seed with
         | Done ->
           (match rest with
            | Sequence { state = seed; next } ->
              (match next seed with
               | Done -> Done
               | Skip { state = s } -> Skip { state = empty, Sequence { state = s; next } }
               | Yield { value = a; state = s } ->
                 Skip { state = f a, Sequence { state = s; next } }))
         | Skip { state = s } -> Skip { state = Sequence { state = s; next }, rest }
         | Yield { value = a; state = s } ->
           Yield { value = a; state = Sequence { state = s; next }, rest }))
    ~init:(empty, t)

let () =
  very_long_function_name
    ~very_long_argument_label:(fun
                                very_long_argument_name_one
                                very_long_argument_name_two
                                very_long_argument_name_three
                              -> () )

let () = ((one_mississippi, two_mississippi, three_mississippi, four_mississippi) : Mississippi.t * Mississippi.t * Mississippi.t * Mississippi.t)

let _ = ((match foo with | Bar -> bar | Baz -> baz) : string)
let _ = ((match foo with | Bar -> bar | Baz -> baz) :> string)

let _ =
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    ~bbbbbbbbbbbbbbbbbbbbbbbbbbbb:(fun (_ :
                                         (ccccccccccccc * ddddddddddddddddddddddddddddd)
                                         eeee) -> FFFFFFFFF gg)
    ~h
;;

type t
[@@deriving
  some_deriver_name
, another_deriver_name
, another_deriver_name
, another_deriver_name
, yet_another_such_name
, such_that_they_line_wrap]

type t
[@@deriving
  some_deriver_name another_deriver_name another_deriver_name
    another_deriver_name yet_another_such_name such_that_they_line_wrap]

let pat =
  String.Search_pattern.create
    (String.init len ~f:(function
        | 0 -> '\n'
        | n when n < len - 1 -> ' '
        | _ -> '*'))
;;

type t =
  { break_separators: [`Before | `After]
  ; break_sequences: bool
  ; break_string_literals: [`Auto | `Never]
        (** How to potentially break string literals into new lines. *)
  ; break_struct: bool
  ; cases_exp_indent: int
  ; cases_matching_exp_indent: [`Normal | `Compact] }

let rec collect_files ~enable_outside_detected_project ~root ~segs ~ignores
    ~enables ~files =
  match segs with [] | [""] -> (ignores, enables, files, None)

let _ =
  fooooooooooooooooooooooooooooooooooooooo
    fooooooooooooooooooooooooooooooooooooooo
    fooooooooooooooooooooooooooooooooooooooo
    ~f:(fun (type a) foooooooooooooooooooooooooooooooooo : 'a ->
      match fooooooooooooooooooooooooooooooooooooooo with
      | Fooooooooooooooooooooooooooooooooooooooo -> x
      | Fooooooooooooooooooooooooooooooooooooooo -> x )

let _ =
  foo
  |> List.map ~f:(fun x ->
    do_something ();
    do_something ();
    do_something ();
    do_something ();
    do_something_else ())

let _ =
  foo
  |> List.map ~f:(fun x ->
    do_something ();
    do_something ();
    do_something ();
    do_something ();
    do_something_else ())
  |> bar

let _ =
  foo
  |> List.map
    fooooooooooo
    fooooooooooo
    fooooooooooo
    fooooooooooo
    fooooooooooo
    fooooooooooo
    fooooooooooo
    fooooooooooo

let _ =
  foo
  |> List.map (function A -> do_something ())

let _ =
  foo
  |> List.map (function
      | A -> do_something ();
      | A -> do_something ();
      | A -> do_something ();
      | A -> do_something ();
      | A -> do_something_else ())
  |> bar

let _ =
  foo
  |> List.double_map ~f1:(fun x ->
      do_something ();
      do_something ();
      do_something ();
      do_something ();
      do_something_else ())
      ~f2:(fun x ->
          do_something ();
          do_something ();
          do_something ();
          do_something ();
          do_something_else ())
  |> bar

module Stritem_attributes_indent : sig
  val f : int -> int -> int -> int -> int
  [@@cold] [@@inline never] [@@local never] [@@specialise never]

  external unsafe_memset : t -> pos:int -> len:int -> char -> unit
    = "bigstring_memset_stub"
  [@@noalloc]

end = struct
  let raise_length_mismatch name n1 n2 =
    invalid_argf "length mismatch in %s: %d <> %d" name n1 n2 ()
  [@@cold] [@@inline never] [@@local never] [@@specialise never]

  external unsafe_memset : t -> pos:int -> len:int -> char -> unit = "bigstring_memset_stub"
  [@@noalloc]
end

let _ =
  foo
  $$ ( match group with [] -> impossible "previous match"
    | [cmt] -> fmt_cmt t conf cmt ~fmt_code $ maybe_newline ~next cmt )
  $$ bar

let _ =
  foo
  $$ ( try group with [] -> impossible "previous match"
    | [cmt] -> fmt_cmt t conf cmt ~fmt_code $ maybe_newline ~next cmt )
  $$ bar

let _ =
  x == exp
  ||
  match x with
  | {pexp_desc= Pexp_constraint (e, _); _} -> loop e
  | _ -> false

let _ =
  let module M = struct
    include ( val foooooooooooooooooooooooooooooooooooooooo
                : fooooooooooooooooooooooooooooooooooooooooo )
  end in
  ()

type action =
  | In_out of [ `Impl | `Intf ] input * string option
  (** Format input file (or [-] for stdin) of given kind to output file,
      or stdout if None. *)
  (* foo *)
  | Inplace of [ `Impl | `Intf ] input list
  (** Format in-place, overwriting input file(s). *)

let%test_module "semantics" =
  (module (
   struct
     open Core
     open Appendable_list
     module Stable = Stable
   end :
     S))
;;

let _ =
  Error
    (`Foooooooooooooooooo
       (name, Format.sprintf "expecting %S but got %S" Version.version value))
;;

let _ =
  `Foooooooooooooooooo
    (name, Format.sprintf "expecting %S but got %S" Version.version value)
;;

let _ =
  Foooooooooooooooooo
    (name, Format.sprintf "expecting %S but got %S" Version.version value)
;;

let (`Foooooooooooooooooo
      (foooooooooooooo, foooooooooooooo, foooooooooooooo, foooooooooooooo) )
    =
  x

let (Foooooooooooooooooo
      (foooooooooooooo, foooooooooooooo, foooooooooooooo, foooooooooooooo) )
    =
  x

let _ =
  Foooooooooooooooooooo.foooooooooooooooooooo
    foooooooooooooooooooo
    foooooooooooooooooooo
    (fun x ->
       function
       | Foooooooooooooooooooo -> foooooooooooooooooooo
       | Foooooooooooooooooooo -> foooooooooooooooooooo)
;;

let _ =
  Foooooooooooooooooooo.foooooooooooooooooooo
    foooooooooooooooooooo
    foooooooooooooooooooo
    ~x:(fun x ->
      function
      | Foooooooooooooooooooo -> foooooooooooooooooooo
      | Foooooooooooooooooooo -> foooooooooooooooooooo)
;;

let _ =
  Foooooooooooooooooooo.foooooooooooooooooooo
    foooooooooooooooooooo
    foooooooooooooooooooo
    (fun x ->
       match foo with
       | Foooooooooooooooooooo -> foooooooooooooooooooo
       | Foooooooooooooooooooo -> foooooooooooooooooooo)
;;

let _ =
  Foooooooooooooooooooo.foooooooooooooooooooo
    foooooooooooooooooooo
    foooooooooooooooooooo
    ~x:(fun x ->
      match foo with
      | Foooooooooooooooooooo -> foooooooooooooooooooo
      | Foooooooooooooooooooo -> foooooooooooooooooooo)
;;

let _ =
  let x = x in
  fun foooooooooooooooooo foooooooooooooooooo foooooooooooooooooo foooooooooooooooooo
      foooooooooooooooooo foooooooooooooooooo ->
    ()
;;

module type For_let_syntax_local =
  For_let_syntax_gen
    with type ('a, 'b) fn := ('a[@local]) -> 'b
     and type ('a, 'b) f_labeled_fn := f:('a[@local]) -> 'b

type fooooooooooooooooooooooooooooooo =
  ( fooooooooooooooooooooooooooooooo
  , fooooooooooooooooooooooooooooooo )
    fooooooooooooooooooooooooooooooo

val fooooooooooooooooooooooooooooooo
  : ( fooooooooooooooooooooooooooooooo
    , fooooooooooooooooooooooooooooooo )
      fooooooooooooooooooooooooooooooo

(*
   *)

(**
   xxx
*)
include S1
(** @inline *)

type input =
  { name: string
  ; action: [`Format | `Numeric of range] }

let x =
  fun [@foo] x ->
    fun [@foo] y ->
      object
        method x = y
      end

class x =
  fun [@foo] x ->
    fun [@foo] y ->
      object
        method x = y
      end

module M =
  [%demo
    module Foo = Bar

    type t]
;;

let _ =
  Some
    (fun fooooooooooooooooooooooooooooooo
        fooooooooooooooooooooooooooooooo
        fooooooooooooooooooooooooooooooo ->
        foo)
;;

type t =
  { xxxxxx :
      t
        (* _________________________________________________________________________
           ____________________________________________________________________
           ___________ *)
        XXXXXXX.t
  }

module Test_gen
  (For_tests : For_tests_gen)
  (Tested : S_gen
            with type 'a src := 'a For_tests.Src.t
            with type 'a dst := 'a For_tests.Dst.t)
  (Tested : S_gen
            with type 'a src := 'a For_tests.Src.t
            with type 'a dst := 'a For_tests.Dst.t
            and type 'a dst := 'a For_tests.Dst.t
            and type 'a dst := 'a For_tests.Dst.t) =
struct
  open Tested
  open For_tests
end

type t =
  { xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx : YYYYYYYYYYYYYYYYYYYYY.t
    (* ____________________________________ *)
  }

(*{v

      foo

v}*)

(*$
    {|
         f|}
*)

type t =
  { xxxxxxxxxxxxxxxxxxx : yyy
                          [@zzzzzzzzzzzzzzzzzzz
                             (* ________________________________
                                ___ *)
                            _______]
  }

let _ =
  match () with
  (*$
    Printf.(
      printf "\n  | _ -> .\n;;\n")
  *)
  | _ -> .
;;
(*$*)

(*$
  "________________________"

             $*)

(*$
  let open! Core in
  ()
*)
(*$*)

(*$
    [%string
      {| xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
zzzzzzzzzzzzzzzzzzzzzzzzzzzz
    |}]
*)
(*$*)

(*$
    {|
         f|}
*)

let () =
  match () with
  | _ ->
    fun _ : _ ->
    (match () with
     | _ -> ())
  | _ -> ()
;;

(* ocp-indent-compat: Docked fun after apply only if on the same line. *)

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo:(fun foo -> bar)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo:(fun foo -> bar)
       ~fooooooooooooooooooooooooooooooo
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo:(fun foo ->
         match bar with
         | Some _ -> foo
         | None -> baz)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo ~fooooooooooooooooooooooooooooooo (fun foo -> bar)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo ~fooooooooooooooooooooooooooooooo (fun foo ->
    match bar with
    | Some _ -> foo
    | None -> baz)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
       ~fooooooooooooooooooooooooooooooo
       (fun foo ->
          match bar with
          | Some _ -> foo
          | None -> baz)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooofooooooooooooooooooooooooooooooofoooooooooo
       (fun foo ->
          match bar with
          | Some _ -> foo
          | None -> baz)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> foooooooooooooooooooooooooooo ~fooooooooooooooooooooooooooooooo (function foo -> bar)
;;

let _ =
  fooooooooooooooooooooooooooooooo
  |> fooooooooooooooooooooooooooooooo ~fooooooooooooooooooooooooooooooo (function
    | Some _ -> foo
    | None -> baz)
;;

(* *)

(*$
  (* 
     *)
  *)

(** xxxxxxxxxxxxxxxxxxxxxxxxxxx [xxxxxxx
    xxxx] xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx [xxxxxxx] *)

(*    Hand-aligned comment
        .
      . *)

(*    First line is indented more
    .
      . *)


module type M = sig
  val imported_sets_of_closures_table
    : Simple_value_approx.function_declarations option
        Set_of_closures_id.Tbl.fooooooooooooooooooooooooo
end

(*$
  let _ =
    [ x (*
      *)
    ; y
    ]
  ;;
*)

let _ =
  { foo =
      (fun _ -> function
         | _ ->
           let _ = 42 in
           ()
         | () -> ())
  }
;;

let _ =
  match () with
  | _ ->
    f
    >>= (function
     | `Fooooooooooooooooooooooooooooooooooooooo -> 1
     | `Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar -> 2)
;;

let _ =
  match () with
  | _ ->
    f
    >>= (function
     | `Fooooooooooooooooooooooooooooooooooooooo -> 1
     | `Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar -> 2)
    >>= foo
;;

let exists t key =
  S.Tree.kind t.tree (path key)
  >|= function
  | Some `Contents -> Ok (Some `Value)
  | Some `Node -> Ok (Some `Dictionary)
  | None -> Ok None
;;

let _ = if x then 42 (* dummy *) else y
let _ = if x then 42 (* dummy *) else if y then z else w

let _ =
  if x
  then
    fun _ -> true
    (* foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
  else f
;;

let _ =
  match ids_queue with
  | Some q ->
    (* this is more efficient than a linear scan of [ids] *)
    fun id -> not (Ident.HashQueue.mem q id)
  | None -> fun id -> not (List.mem ~equal:Ident.equal ids id)
;;

type callbacks =
  { html_debug_new_node_session_f :
      'a.
      ?kind:[ `ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO ]
      -> pp_name:(Format.formatter -> unit)
      -> Procdesc.Node.t
      -> f:(unit -> 'a)
      -> 'a
  }
