(** Read an AIGER file, parse it and output it on the standard output. *)

let main =
  if Array.length Sys.argv < 2 
  then print_endline "usage : test <file>";
  let aiger = AigerImperative.read_from_file Sys.argv.(1) in
  AigerImperative.write aiger stdout
