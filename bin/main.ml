let html_template =
  {|
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="style.css">
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

let read_file filename =
  let ic = open_in filename in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content

let read_files_in_dir dir =
  Sys.readdir dir |> Array.to_list
  |> List.map (fun filename -> read_file (Filename.concat dir filename))

let convert_markdown_to_html markdown = markdown |> Omd.of_string |> Omd.to_html

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let insert_content_to_template template content =
  Str.global_replace (Str.regexp "<!-- content -->") content template

let insert_metadata template metadata =
  List.fold_left
    (fun temp (key, value) ->
      let placeholder = Printf.sprintf "<!-- %s -->" key in
      Str.global_replace (Str.regexp placeholder) value temp)
    template metadata

type t = { input_file : string; output_path : string }

let process_content state =
  let metadata, input = read_file state.input_file |> extract_metadata in

  let input_html = convert_markdown_to_html input in

  let template = insert_metadata html_template metadata in

  let final_html = insert_content_to_template template input_html in

  let output_to =
    Filename.basename state.input_file
    |> Str.replace_first (Str.regexp ".md") ".html"
    |> Filename.concat state.output_path
  in

  print_endline output_to;

  write_file output_to final_html;
  Printf.printf "Converted %s to %s\n" state.input_file output_to

let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input.md> <output.html>\n" Sys.argv.(0)
  else
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in

    let state = { input_file; output_path = output_file } in

    match Sys.is_directory input_file with
    | false -> process_content state
    | true ->
        let files = read_files_in_dir input_file in

        List.iter
          (fun file -> process_content { state with input_file = file })
          files
