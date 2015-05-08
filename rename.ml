open Aiger

let main =
  if Array.length Sys.argv < 4 then Printf.printf "usage: %s <file.aag> <old-name> <new-name>" Sys.argv.(0)
  else
    write (full_rename (read_from_file Sys.argv.(1)) [Sys.argv.(2),Sys.argv.(3)]) stdout
