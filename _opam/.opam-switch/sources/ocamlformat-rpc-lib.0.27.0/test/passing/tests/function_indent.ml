let foooooooo = function
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo

let foooooooo = function
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo

let foo =
  fooooooooo foooooooo ~foooooooo:(function
      | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
      | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo)

let foo =
  fooooooooo foooooooo foooooooo foooooooo foooooooo foooooooo ~foooooooo:(function
      | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
      | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo )

let foooooooo =
  if fooooooooooo then
    function
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
  else
    function
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo
    | fooooooooooooooooooooooo -> foooooooooooooooooooooooooo

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
  | _ -> (
      f
      >>= function
      | `Fooooooooooooooooooooooooooooooooooooooo -> 1
      | `Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar -> 2 )