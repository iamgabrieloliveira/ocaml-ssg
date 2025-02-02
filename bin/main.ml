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
    <link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&family=Merriweather:ital,wght@0,300;0,400;0,700;0,900;1,300;1,400;1,700;1,900&display=swap" rel="stylesheet">
    <title><!-- title --></title>
</head>
<body>
    <header>
        <nav>
            <ul>
                <h1>Gabriel Oliveira</h1>
                <li><a href="index.html">Home</a></li>
                <li><a href="posts.html">Posts</a></li>
            </ul>
        </nav>
    </header>
    <main>
        <hr/>
        
        <div class="content">
            <!-- content -->
        </div>
        
        <hr/>
    </main>
    
    <footer>
        <p>© 2025 Gabriel Oliveira</p>
    </footer>
</body>
</html>
|}

type metadata = (string * string) list

type post = {
  path : string;
  title : string;
  content : string;
  metadata : metadata;
}

let read_file filename =
  let ic = open_in filename in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content

let extract_metadata content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | "---" :: rest ->
      let rec parse_meta acc = function
        | [] -> (List.rev acc, [])
        | "---" :: md -> (List.rev acc, md)
        | line :: xs -> parse_meta (line :: acc) xs
      in
      let meta_lines, markdown_lines = parse_meta [] rest in
      let metadata =
        List.fold_left
          (fun acc line ->
            match String.split_on_char ':' line with
            | key :: value :: _ -> (String.trim key, String.trim value) :: acc
            | _ -> acc)
          [] meta_lines
      in
      (metadata, String.concat "\n" markdown_lines)
  | _ -> ([], content)

let convert_markdown_to_html markdown = markdown |> Omd.of_string |> Omd.to_html

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let get_posts path =
  let files = Sys.readdir path |> Array.to_list in

  List.map
    (fun file ->
      let path = Filename.concat path @@ file in

      let metadata, content = read_file path |> extract_metadata in

      let title = List.assoc_opt "title" metadata in

      match title with
      | Some title -> { path; title; metadata; content }
      | None -> raise (Invalid_argument "Title not found"))
    files

let post_output_path post output_path =
  Filename.basename post.path
  |> Str.replace_first (Str.regexp ".md") ".html"
  |> Filename.concat output_path

let apply_to_template post =
  let open Printf in
  let open Str in
  let title = List.assoc "title" post.metadata in
  let content = convert_markdown_to_html post.content in
  let template =
    Str.global_replace (Str.regexp "<!-- title -->") title html_template
  in
  Str.global_replace (Str.regexp "<!-- content -->") content template

let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input-dir> <output-dir>\n" Sys.argv.(0)
  else
    let input_path = Sys.argv.(1) in
    let output_path = Sys.argv.(2) in

    let posts = get_posts input_path in

    List.iter
      (fun post ->
        let html = apply_to_template post in
        let path = post_output_path post output_path in

        write_file path html)
      posts;

    ()
