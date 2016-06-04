(*
 * Copyright 2014 Romain Brenguier
 * Author: Romain Brenguier <romain.brenguier@ulb.ac.be>
 * 
 * This file is part of Ocaml-aiger.
 * 
 * Ocaml-aiger is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details. 
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


type lit = int
let aiger_false = 0
let aiger_true = 1
let neg lit = if lit mod 2 = 0 then lit + 1 else lit - 1

let test = BatDynArray.make 0

type lit_set = (lit,bool) Hashtbl.t

let lit_set_fold f set start = 
  let nb,res = 
    Hashtbl.fold (fun lit b (i,accu) ->
      if not b then (i,accu)
      else
	let res = f i lit accu in (i+1,res)
    ) set (0,start)      
  in res      

type t = {
  mutable maxvar:int;   
  mutable num_inputs:int;
  mutable num_latches:int;
  mutable num_outputs:int;
  mutable num_ands:int;

  inputs: lit_set;
  latches:(lit,lit) Hashtbl.t;
  outputs:lit_set;
  ands: (lit,lit*lit) Hashtbl.t;
  ands_inv: (lit*lit,lit) Hashtbl.t;
  symbols: (lit,string) Hashtbl.t;
  symbols_inv: (string,lit) Hashtbl.t;

  mutable comments:string list;
}


let string2lit_exn aiger string = Hashtbl.find aiger.symbols_inv string 
let lit2string_exn aiger lit = Hashtbl.find aiger.symbols lit 
let string2lit aiger string = try Some (string2lit_exn aiger string) with Not_found -> None
let lit2string aiger lit = try Some (lit2string_exn aiger lit) with Not_found -> None

let add_correspondance aiger lit symbol =
  Hashtbl.add aiger.symbols_inv symbol lit;
  Hashtbl.add aiger.symbols lit symbol


let change_correspondance aiger old_symbol new_symbol =
  try
    let lit = Hashtbl.find aiger.symbols_inv old_symbol in
    Hashtbl.replace aiger.symbols lit new_symbol;
    Hashtbl.remove aiger.symbols_inv old_symbol;
    Hashtbl.add aiger.symbols_inv new_symbol lit 
  with 
    Not_found -> Printf.eprintf "Warning: symbol %s was not previously defined\n" old_symbol


    
let empty () =   
  {maxvar=0; num_inputs=0; num_latches=0;
   num_outputs=0;num_ands=0;
   inputs=Hashtbl.create 100; 
   latches=Hashtbl.create 100; 
   outputs=Hashtbl.create 100; 
   ands=Hashtbl.create 100; 
   ands_inv=Hashtbl.create 100; 
   comments= []; 
   symbols = Hashtbl.create 100;   
   symbols_inv = Hashtbl.create 100;
  }

let parse inch = 
  let line = input_line inch in
  let aiger = empty () in
  Scanf.sscanf line "aag %d %d %d %d %d" 
    (fun v a b c d -> 
      aiger.maxvar <- v; 
      aiger.num_inputs <- a;
      aiger.num_latches <- b;
      aiger.num_outputs <- c;
      aiger.num_ands <- d
    );

  let inputs = Array.make aiger.num_inputs aiger_false in
  let latches = Array.make aiger.num_latches aiger_false in
  let outputs = Array.make aiger.num_outputs aiger_false in

  let add_input index lit = 
    Hashtbl.add aiger.inputs lit true;
    inputs.(index) <- lit
  in
    
  for i = 0 to aiger.num_inputs - 1
  do
    try 
      let line = input_line inch in
      Scanf.sscanf line "%d" (add_input i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading input#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_latch index lit up = 
    Hashtbl.add aiger.latches lit up;
    latches.(index) <- lit
  in

  for i = 0 to aiger.num_latches - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d" (add_latch i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading latch#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_output index lit = 
    Hashtbl.add aiger.outputs lit true;
    outputs.(index) <- lit
  in

  for i = 0 to aiger.num_outputs - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d" (add_output i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading output#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_and lhs rhs0 rhs1 = 
    let rhs = min rhs0 rhs1, max rhs0 rhs1 in
    Hashtbl.add aiger.ands lhs rhs;
    Hashtbl.add aiger.ands_inv rhs lhs
  in

  for i = 0 to aiger.num_ands - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d %d" add_and
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading and#%d/%d\n" (i+1) aiger.num_ands; 
      raise End_of_file
  done;

  let stop = ref false in

  while (not !stop) do
    try 
      let line = input_line inch in
      let id,name = match Str.full_split (Str.regexp "[ ]") line with
	| Str.Text id :: Str.Delim " " :: Str.Text name :: _ -> id,name
	| _ -> failwith ("In Aiger.read_symbols: could not parse symbol "^line)
      in
      let lit = Scanf.sscanf id "%c%d"
	(fun t a -> match t with
	| 'i' -> inputs.(a)
	| 'o' -> outputs.(a)
	| 'l' -> latches.(a)
	| c -> failwith (Printf.sprintf "In Aiger.read_symbols: unknown variable type %c" c))
	  
      in add_correspondance aiger lit name
    with _ -> stop := true
  done;

  stop := false;
  
  while (not !stop) do
    try
      let line = input_line inch in
      aiger.comments <- line :: aiger.comments
    with End_of_file -> stop := true
  done; 
  aiger.comments <- List.rev aiger.comments;
  
  aiger





let read inch = parse inch

let read_from_file file =  
  let inch = open_in file in
  let aiger = read inch in
  close_in inch;
  aiger

let new_var t = t.maxvar <- t.maxvar+1; 2 * t.maxvar

exception AlreadyExists

    
let add_input t name =
  t.num_inputs <- t.num_inputs + 1;
  let lit = new_var t in
  Hashtbl.add t.inputs lit true;
  add_correspondance t lit name;
  lit

let add_latch t name = 
  t.num_latches <- t.num_latches + 1;
  let lit = new_var t in
  Hashtbl.add t.latches lit aiger_false;
  add_correspondance t lit name;
  lit

let set_latch_update t lit upd =
  Hashtbl.replace t.latches lit upd
  
let set_output t name lit = 
  t.num_outputs <- t.num_outputs + 1;
  Hashtbl.add t.outputs lit true;
  add_correspondance t lit name

let conj t rhs0 rhs1 = 
  if rhs0 = aiger_false || rhs1 = aiger_false then aiger_false
  else if rhs0 = aiger_true then rhs1 
  else if rhs1 = aiger_true then rhs0
  else 
    let rhs = min rhs0 rhs1, max rhs1 rhs0 in
    try Hashtbl.find t.ands_inv rhs
    with Not_found ->
      t.num_ands <- t.num_ands + 1;
      let lhs = new_var t in
      Hashtbl.add t.ands lhs rhs;
      Hashtbl.add t.ands_inv rhs lhs;
      lhs


let add_comment t comment = t.comments <- comment :: t.comments


type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

let lit2tag_exn t lit = 
  if lit < 2 then Constant (lit = 1)
  else 
    if Hashtbl.mem t.inputs lit
    then Input lit
    else
      if Hashtbl.mem t.outputs lit
      then Output lit
      else
	try Latch (lit, Hashtbl.find t.latches lit)
	with Not_found ->
	  let rhs0,rhs1 = Hashtbl.find t.ands lit in
	  And (lit,rhs0,rhs1)

let lit2tag t lit = 
  try Some (lit2tag_exn t lit) with Not_found -> None

exception Not_output of tag

let hide t name = 
  let l = string2lit_exn t name in
  try
    if not (Hashtbl.mem t.inputs l || Hashtbl.mem t.latches l)
    then (Hashtbl.remove t.symbols_inv name;
	  Hashtbl.remove t.symbols l);
    Hashtbl.remove t.outputs l;
    t.num_outputs <- t.num_outputs - 1;
  with Not_found -> raise (Not_output (lit2tag_exn t l))


let names aiger =
  Hashtbl.fold (fun lit string accu ->
    string :: accu
  ) aiger.symbols []

let inputs aiger = 
  Hashtbl.fold 
    (fun lit b accu -> 
      if not b then accu else
	let name = lit2string_exn aiger lit in
	name :: accu
    ) aiger.inputs []

let latches aiger = 
  Hashtbl.fold 
    (fun lit _ accu -> 
      let name = lit2string_exn aiger lit in
      name :: accu
    ) aiger.latches []

let outputs aiger = 
  Hashtbl.fold 
    (fun lit b accu -> 
      if not b then accu else
	let name = lit2string_exn aiger lit in
	name :: accu
    ) aiger.outputs []


let write aiger outch =
  Printf.fprintf outch "aag %d %d %d %d %d\n" aiger.maxvar aiger.num_inputs aiger.num_latches aiger.num_outputs aiger.num_ands;
  lit_set_fold (fun i lit () -> Printf.fprintf outch "%d\n" lit) aiger.inputs ();
  Hashtbl.iter (fun a b -> Printf.fprintf outch "%d %d\n" a b) aiger.latches;
  lit_set_fold (fun i lit () -> Printf.fprintf outch "%d\n" lit) aiger.outputs ();
  Hashtbl.iter (fun a (b,c) -> Printf.fprintf outch "%d %d %d\n" a b c) aiger.ands;
  lit_set_fold (fun i a () -> Printf.fprintf outch "i%d %s\n" i (lit2string_exn aiger a)) aiger.inputs ();
  ignore (Hashtbl.fold (fun lhs rhs i -> Printf.fprintf outch "l%d %s\n" i (lit2string_exn aiger lhs); i+1) aiger.latches 0);
  (* Some outputs could have been removed *)
  lit_set_fold (fun i a () -> Printf.fprintf outch "o%d %s\n" i (lit2string_exn aiger a)) aiger.outputs ();
  if aiger.comments <> [] then Printf.fprintf outch "c\n";
  List.iter (fun a -> Printf.fprintf outch "%s\n" a) aiger.comments


let write_to_file aiger file = 
  let outch = open_out file in
  write aiger outch;
  close_out outch

let rename aiger renaming =
  names aiger 
  |>  List.iter (fun name -> if renaming name <> name then change_correspondance aiger name (renaming name))
  

