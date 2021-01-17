open Printf


let summary =
  "This is a small markdown compiler project"

let help = 
  "Enter the markdown file to parse as: .\main <my-file.md>"


let rec to_string lines acc =
  match lines with
  | [] -> acc
  | h::t -> to_string t (acc ^ h ^ "\n")


let parse_line line =
  let n = String.length line in
  match n with
  | 0 -> [""]
  | _ -> 
        match String.get line 0 with
        | '#' -> (match n with | 1 -> [] | _ -> ["<h1>" ^ String.sub line 1 (n - 1) ^ "<h1>"])
        | _ -> ["<p>" ^ line ^ "<p>"]


let rec parse_md_file lines =
  match lines with
  | [] -> []
  | h::t -> parse_line h @ parse_md_file t


let read_lines name =
  let ch = open_in name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


let write_to_file lines where =
  let oc = open_out where in
  fprintf oc "%s" (to_string lines "");
  close_out oc


let main file_name =
  print_endline ("Trying to read from: " ^ file_name);
  let lines = read_lines file_name in
  let split = (String.split_on_char '\n' lines) in
  print_endline ("Successfully read from: " ^ file_name);
  print_endline "Trying to parse...";
  let res = parse_md_file split in
  print_endline "Successfully parsed";
  print_endline "Trying to write to file";
  write_to_file res ((List.nth (String.split_on_char '.' file_name) 0) ^ ".html");
  print_endline "Successfully wrote to file"


let () =
  print_endline summary;
  print_endline "-----------------------------------------";
  if Array.length Sys.argv != 2 
  then print_endline help
  else main (Sys.argv.(1))