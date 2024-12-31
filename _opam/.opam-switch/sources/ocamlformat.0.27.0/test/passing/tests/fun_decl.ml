let _ = fun (x : int) : int -> some_large_computation

let _ = fun (x : int) : int -> (some_large_computation : int)

let fooo = List.foooo ~f:(fun foooo foooo : bool -> foooooooooooooooooooooo)

let _ =
 fun (x : int) (x : int) (x : int) (x : int) (x : int) :
     fooooooooooooooooooooooooooo foooooooooooooo foooooooooo ->
  some_large_computation

let _ =
 fun (x : int) (x : int) (x : int) (x : int) (x : int) (x : int) (x : int) :
     fooooooooooooooooooooooooooo foooooooooooooo foooooooooo ->
  some_large_computation

let () =
 fun x : int ->
  fun r : int ->
   fun u ->
    foooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooooooooooooooooooooooooo

let to_loc_trace
    ?(desc_of_source =
      fun source ->
        let callsite = Source.call_site source in
        Format.asprintf "return from %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(source_should_nest = fun _ -> true)
    ?(desc_of_sink =
      fun sink ->
        let callsite = Sink.call_site sink in
        Format.asprintf "call to %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(sink_should_nest = fun _ -> true)
    (passthroughs, sources, sinks) =
  ()

let translate_captured
    { Clang_ast_t.lci_captured_var
    ; lci_init_captured_vardecl
    ; lci_capture_this
    ; lci_capture_kind } ((trans_results_acc, captured_vars_acc) as acc) =
  ()

let f ssssssssss =
  String.fold ssssssssss ~init:innnnnnnnnnit
    ~f:(fun accuuuuuuuuuum -> function
    | '0' -> g accuuuuuuuuuum
    | '1' -> h accuuuuuuuuuum
    | _ -> i accuuuuuuuuuum )

let f ssssssssss =
  String.fold ssssssssss ~init:innnnnnnnnnit ~f:(function
    | '0' -> g accuuuuuuuuuum
    | '1' -> h accuuuuuuuuuum
    | _ -> i accuuuuuuuuuum )

let f _ =
  let foooooooooooooooooooooooooooo = foooooooooooooooooooooooooooo in
  fun x ->
    let foooooooooooooooooooooooooooo = foooooooooooooooooooooooooooo in
    x

let f _ =
  let foooooooooooooooooooooooooooo = foooooooooooooooooooooooooooo in
  (* foo *)
  fun x ->
    let foooooooooooooooooooooooooooo = foooooooooooooooooooooooooooo in
    x

let space_break =
  (* a stack is useless here, this would require adding a unit parameter *)
  with_pp (fun fs ->
      Box_debug.space_break fs ;
      Format_.pp_print_space fs () )

let _ =
  (fun k ->
    let _ = 42 in
    () )
  @@ ()

let _ =
  let _ = () in
  fun (context : Context.t)
      ~(local_bins : origin Appendable_list.t Filename.Map.t Memo.Lazy.t) ->
    let _ = () in
    ()

class traverse_labels h =
  object
    method statement =
      function
      | Labelled_statement (L l, (s, _)) ->
          let m = {<ldepth = ldepth + 1>} in
          Hashtbl.add h l ldepth ; m#statement s
      | s -> super#statement s
  end