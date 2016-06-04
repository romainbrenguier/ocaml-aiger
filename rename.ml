open AigerImperative

let main =
  if Array.length Sys.argv < 4 then Printf.printf "usage: %s <file.aag> <old-name> <new-name>" Sys.argv.(0)
  else
    let aiger = read_from_file Sys.argv.(1) in
    rename aiger (fun x -> if x = Sys.argv.(2) then Sys.argv.(3) else x);
    write aiger stdout
