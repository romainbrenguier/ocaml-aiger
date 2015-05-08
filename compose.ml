open Aiger

let main = 
  if Array.length Sys.argv < 3 then Printf.printf "usage: %s <file1.aag> <file2.aag>" Sys.argv.(0)
  else
    write (compose (read_from_file Sys.argv.(1)) (read_from_file Sys.argv.(2))) stdout
