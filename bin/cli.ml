type args = { from : string; to' : string }

let parse args =
  match args with
  | [ _; from; to' ] -> Ok { from; to' }
  | _ -> Error "Usage: ocaml_ssg <from> <to>"
