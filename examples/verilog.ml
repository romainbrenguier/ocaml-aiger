
let lit2string aiger lit = 
  try 
  let string = 
    match Aiger.lit2tag aiger (Aiger.strip lit) with
    | Constant true -> "1" | Constant false -> "0"
    | And (l,_,_) -> "gate"^string_of_int (Aiger.lit2int l)
    | _ -> Aiger.Symbol.to_string (Aiger.lit2symbol aiger (Aiger.strip lit))
  in if Aiger.sign lit then "(!"^string^")" else string
  with Not_found -> "lit "^string_of_int (Aiger.lit2int lit)^" not found"

let aiger2verilog aiger =
  Printf.printf "module FromAiger(\n  input clk,\n";
  
  List.iter
    (fun i -> 
      Printf.printf "  input %s,\n" i
    ) (Aiger.inputs aiger);

  (match Aiger.outputs aiger with
  | hd :: tl ->
    List.iter
      (fun i -> 
	Printf.printf "  output %s,\n" i
      ) tl;
    Printf.printf "  output %s\n" hd
  | _ -> failwith "warning: empty output"
  );
  
  print_endline ");";

  List.iter
    (fun i -> 
      Printf.printf "  reg %s;\n" i
    ) (Aiger.latches aiger);

  List.iter
    (fun (i,_,_) -> 
      Printf.printf "  wire gate%d;\n" (Aiger.lit2int i)
    ) aiger.Aiger.ands;

  List.iter
    (fun (i,a,b) -> 
      let ga = lit2string aiger a in
      let gb = lit2string aiger b in
      Printf.printf "  assign gate%d = %s & %s;\n" (Aiger.lit2int i) ga gb
    ) aiger.Aiger.ands;

  print_endline "\n  always @(posedge clk) begin";

  List.iter
    (fun (l,m) -> 
      let symbol = Aiger.lit2symbol aiger l in
      let gm = lit2string aiger m in
      Printf.printf "    %s <= %s;\n" (Aiger.Symbol.to_string symbol) gm
    ) aiger.Aiger.latches;
  print_endline "  end\n";
  print_endline "endmodule"


let main = 
  if Array.length Sys.argv < 2 then Printf.printf "usage: %s <aiger_file>\n" Sys.argv.(0);
  aiger2verilog (Aiger.read_from_file Sys.argv.(1))
