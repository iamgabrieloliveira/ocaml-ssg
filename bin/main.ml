let html_template =
  {|
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="style.css">

    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Courier+Prime:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">

    <title><!-- title --></title>
</head>
<body>
    <header>
        <nav>
            <h1>Gabriel Oliveira</h1>
            <ul>
                <li><a href="index.html">Home</a></li>
                <li><a href="posts.html">Posts</a></li>
            </ul>
        </nav>
    </header>
    <main>
        <div class="content">
            <!-- content -->
        </div>
    </main>
    
    <footer>
        <p>© 2025 Gabriel Oliveira</p>
    </footer>
</body>
</html>
|}

type post = { path : string; html : string }

(*
todo:
1. Nested files
*)

(* 1. What's the difference between Array and List in Ocaml? *)

let read_to_string path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let add_variable template key value =
  let pattern = Str.regexp (Printf.sprintf "<!-- %s -->" key) in
  Str.global_replace pattern value template

let md_to_html string = string |> Omd.of_string |> Omd.to_html

(* adding the post html inside the body of the html *)
let apply_template html = add_variable html_template "content" html

let parse_post path =
  let html = read_to_string path |> md_to_html |> apply_template in

  { path; html }

let write_file file content =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let run from to' =
  let files = Sys.readdir from in

  Array.iter
    (fun filename ->
      let file_path = Filename.concat from filename in
      let post = parse_post file_path in

      let input_basename = Filename.basename from in
      let output_basename = Filename.basename to' in

      (*
      1. we replace the input base path with the output base,
      so if we have the input path as: /posts/example.md and output path as /dist/example.md
      it replace "posts" with the "dist".

      2. we change the extension from .md to .html.
      *)
      let output_path =
        Str.replace_first (Str.regexp input_basename) output_basename file_path
      in
      let output_path =
        Str.replace_first (Str.regexp ".md") ".html" output_path
      in

      write_file output_path post.html)
    files

let () =
  match Cli.parse @@ Array.to_list Sys.argv with
  | Ok { from; to' } -> run from to'
  | Error err -> print_endline err
