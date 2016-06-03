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

type t = {
  mutable maxvar:int;   
  mutable num_inputs:int;
  mutable num_latches:int;
  mutable num_outputs:int;
  mutable num_ands:int;

  inputs:(int,lit) Hashtbl.t;
  latches:(int,(lit*lit)) Hashtbl.t;
  latches_inv:(lit,int) Hashtbl.t;
  outputs:(int,lit) Hashtbl.t;

  ands: (lit,lit*lit) Hashtbl.t;
  inverse_ands: (lit*lit,lit) Hashtbl.t;

  mutable comments:string list;

  symbols: (lit,string) Hashtbl.t;
  symbols_inv: (string,lit) Hashtbl.t;
}

let nth_input_exn aiger a = Hashtbl.find aiger.inputs a
let nth_output_exn aiger a = Hashtbl.find aiger.outputs a
let nth_latch_exn aiger a = Hashtbl.find aiger.latches a

let string2lit_exn aiger string = Hashtbl.find string aiger.symbols_inv 
let lit2string_exn aiger lit = Hashtbl.find lit aiger.symbols
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


let read_symbols aiger =
  let comments = aiger.comments in
  aiger.comments <- [];
  List.iter
    (fun line -> 
      try
	let id,name = match Str.full_split (Str.regexp "[ ]") line with
	  | Str.Text id :: Str.Delim " " :: Str.Text name :: _ -> id,name
	  | _ -> failwith ("In Aiger.read_symbols: could not parse symbol "^line)
	in
	let lit = Scanf.sscanf id "%c%d"
	  (fun t a -> match t with
	  | 'i' -> nth_input_exn aiger a 
	  | 'o' -> nth_output_exn aiger a
	  | 'l' -> fst (nth_latch_exn aiger a)
	  | c -> failwith (Printf.sprintf "In Aiger.read_symbols: unknown variable type %c" c))
	  
	in add_correspondance aiger lit name
	
      with _ -> aiger.comments <- line::aiger.comments
    ) comments
    
let empty () =   
  {maxvar=0; num_inputs=0; num_latches=0;
   num_outputs=0;num_ands=0;
   inputs=Hashtbl.create 100; 
   latches=Hashtbl.create 100; 
   latches_inv=Hashtbl.create 100; 
   outputs=Hashtbl.create 100; 
   ands=Hashtbl.create 100; 
   inverse_ands=Hashtbl.create 100; 
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
  
  for i = 0 to aiger.num_inputs - 1
  do
    try 
      let line = input_line inch in
      Scanf.sscanf line "%d" (Hashtbl.add aiger.inputs i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading input#%d\n" (i+1); 
      raise End_of_file
  done;

  for i = 0 to aiger.num_latches - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d" (fun a b -> Hashtbl.add aiger.latches i (a,b))
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading latch#%d\n" (i+1); 
      raise End_of_file
  done;

  for i = 0 to aiger.num_outputs - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d" (Hashtbl.add aiger.outputs i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading output#%d\n" (i+1); 
      raise End_of_file
  done;

  for i = 0 to aiger.num_ands - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d %d" (fun a b c -> 
	Hashtbl.add aiger.ands a (b,c);
	Hashtbl.add aiger.inverse_ands (b,c) a;
      )
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading and#%d/%d\n" (i+1) aiger.num_ands; 
      raise End_of_file
  done;

  let stop = ref false in

  while (not !stop) do
    try 
      let line = input_line inch in
      aiger.comments <- line :: aiger.comments
    with End_of_file -> stop := true
  done;

  aiger.comments <- List.rev aiger.comments;
  read_symbols aiger 


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
  Hashtbl.add t.inputs (t.num_inputs - 1) lit;
  add_correspondance t lit name;
  lit

let add_latch t name = 
  t.num_latches <- t.num_latches + 1;
  let lit = new_var t in
  Hashtbl.add t.latches (t.num_latches - 1) (lit,aiger_false);
  Hashtbl.add t.latches_inv lit (t.num_latches - 1);
  add_correspondance t lit name;
  lit

let set_latch_update t lit upd =
  let index = Hashtbl.find t.latches_inv lit in
  Hashtbl.replace t.latches index (lit,upd)
  
let set_output t name lit = 
  t.num_outputs <- t.num_outputs + 1;
  Hashtbl.add t.outputs (t.num_outputs - 1) lit;
  add_correspondance t lit name

let conj t rhs0 rhs1 = 
  if rhs0 = aiger_false || rhs1 = aiger_false then aiger_false
  else if rhs0 = aiger_true then rhs1 
  else if rhs1 = aiger_true then rhs0
  else 
    let rhs0,rhs1 = (min rhs0 rhs1, max rhs1 rhs0) in
    try Hashtbl.find t.inverse_ands (rhs0,rhs1)
    with Not_found ->
      t.num_ands <- t.num_ands + 1;
      let lhs = new_var t in
      Hashtbl.add t.ands lhs (rhs0,rhs1);
      Hashtbl.add t.inverse_ands (rhs0,rhs1) lhs;
      lhs


let add_comment t comment = t.comments <- comment :: t.comments


type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

let lit2tag t lit = 
  if lit < 2 then Constant (lit = 1)
  else 
    if Hashtbl.mem t.inputs_inv t
    then Input lit
    else
      try Latch (Hashbt.find latches (Hashtbl.find t.latches_inv lit))
      with
	  match List.fold_left (fun accu (l,r,s) -> if l = lit then Some (And (l,r,s)) else accu) None t.ands

	  with Some x -> x
	  | None -> 
	    match List.fold_left (fun accu l -> if l = lit then Some (Output l) else accu) None t.outputs
	    with 
	    | Some x -> x
	    | None ->
	      Printf.eprintf "Warning: in lit2tag, literal %d was not found.\n" (lit2int lit);
	      raise Not_found 

let hide t name = 
  let l = symbol2lit t name in
  if not (List.mem l t.outputs)
  then (Printf.eprintf "Warning : symbol %s is not an output. " (Symbol.to_string name);
	match lit2tag t l with 
	| Input _ -> Printf.eprintf "It is an input.\n"
	| Latch _ -> Printf.eprintf "It is a latch.\n"
	| And _ -> Printf.eprintf "It is an AND gate.\n"
	| Constant _ -> Printf.eprintf "It is a constant.\n"
	| _ -> Printf.eprintf "It is an output.\n"
  );
  let num_outputs = t.num_outputs - 1 in
  let rec my_filter accu = function
    | [] -> Printf.eprintf "Warning: output not found\n"; List.rev accu
    | hd :: tl -> 
      if hd = l then List.rev_append accu tl
      else my_filter (hd :: accu) tl
  in
  let outputs = my_filter [] t.outputs in
(*  if List.length outputs <> num_outputs 
  then Printf.eprintf "Warning : wrong number of outputs when hiding %s : %d instead of %d\n" (Symbol.to_string name) (List.length outputs) num_outputs;*)
  (* warning we should remove the symbol only if it is only used as output *)
  try
    let symbols,abstract = 
      match lit2tag t l with 
      | Input _ | Latch _ -> t.symbols, t.abstract
      | Output _ | And _ | Constant _ -> SymbolMap.remove name t.symbols, LitMap.remove l t.abstract 
    in
    {t with num_outputs = num_outputs; outputs = outputs; symbols=symbols; abstract=abstract }
  with Not_found -> Printf.eprintf "problem hiding %s\n" (Symbol.to_string name); raise Not_found


let remove_latch t name = 
  let l = symbol2lit t name in
  let num_latches = t.num_latches - 1 in
  let latches = List.filter (fun (x,y) -> l <> x) t.latches in
  if List.length latches <> num_latches
  then Printf.eprintf "Warning : wrong number of outputs\n";
  let symbols, abstract = SymbolMap.remove name t.symbols, LitMap.remove l t.abstract in
  ignore (symbols,abstract); failwith "unimplemented: remove_latch"
  

let size_symbol aiger name =
  let (nb,maxi) = SymbolMap.fold (fun (s,io) l (nb,maxi) -> 
    let i = match io with None -> 0 | Some x -> x in
    if s = name then (nb+1,max maxi i) else (nb,maxi)) aiger.symbols (0,-1) in
  if maxi + 1 <> nb
  then 
    Printf.eprintf "warning: problem in input aiger file : there are %d symbols to encode %s but the maximum index used is %d\n" nb name maxi;
  if nb = 0
  then 
    Printf.eprintf "warning: there are no symbols to encode %s\n" name;
  max nb maxi
  

let name_to_symbols aiger name =
  let size = size_symbol aiger name in
  Array.init size (fun i -> (name,Some i))

let name_to_literals aiger name =
  let size = size_symbol aiger name in
  if size = 1 
  then
    try [| symbol2lit aiger (name, None) |]
    with Not_found ->  [| symbol2lit aiger (name, Some 0) |]
  else
    Array.init size (fun i -> symbol2lit aiger (name,Some i))

let full_hide aiger name = 	
  try 
    (*Printf.eprintf "Info: hiding variable %s\n" name;*)
    Array.fold_right (fun s accu -> 
      hide accu s 
    ) (name_to_symbols aiger name) aiger
  with Not_found -> 
    Printf.eprintf "Warning: in full_hide literals not found for variable %s\n" name;
    aiger


module StringSet = Set.Make(String)

let names aiger = 
  let set = 
    SymbolMap.fold (fun sym ind set -> StringSet.add (fst sym) set) aiger.symbols StringSet.empty
  in StringSet.elements set

let inputs aiger = 
  List.filter 
    (fun name -> 
      let lit = name_to_literals aiger name in
      match lit2tag aiger lit.(0) with
      | Input _ -> true
      | _ -> false
    ) (names aiger)

let latches aiger = 
  List.filter 
    (fun name -> 
     let lit = name_to_literals aiger name in
     match lit2tag aiger lit.(0) with
     | Latch _ -> true
     | _ -> false
    ) (names aiger)

let outputs aiger = 
  List.filter 
    (fun name -> 
      let lit = name_to_literals aiger name in
      List.mem lit.(0) aiger.outputs 
    ) (names aiger)



let write aiger outch =
  let maxvar = aiger.maxvar in 

  Printf.fprintf outch "aag %d %d %d %d %d\n" maxvar aiger.num_inputs aiger.num_latches aiger.num_outputs aiger.num_ands;
  let sorted_out = 
    List.fold_left
      (fun accu s -> 
       let lits = name_to_literals aiger s in 
       if Array.length lits = 1
       then 
	 (lits.(0), (s,None)) :: accu
       else
	 let list,nb = Array.fold_left (fun (accu,i) lit -> (lit,(s,Some i)):: accu,i+1) (accu,0) lits in
	 list

      ) [] (outputs aiger)
  in

  List.iter (fun i -> Printf.fprintf outch "%d\n" i) aiger.inputs;
  List.iter (fun (a,b) -> Printf.fprintf outch "%d %d\n" a b) aiger.latches;
  List.iter (fun (lit,_) -> Printf.fprintf outch "%d\n" lit) sorted_out;
  List.iter (fun (a,b,c) -> Printf.fprintf outch "%d %d %d\n" a b c) aiger.ands;
  List.iteri (fun i a -> Printf.fprintf outch "i%d %s\n" i (lit2string aiger a)) aiger.inputs;
  List.iteri (fun i a -> Printf.fprintf outch "l%d %s\n" i (lit2string aiger (fst a))) aiger.latches;
  (* List.iteri (fun i a -> Printf.fprintf outch "o%d %s\n" i (lit2string aiger a)) aiger.outputs;*)
  List.iteri (fun i (lit,sym) -> Printf.fprintf outch "o%d %s\n" i (Symbol.to_string sym)) sorted_out;
  if aiger.comments <> [] then Printf.fprintf outch "c\n";
  List.iter (fun a -> Printf.fprintf outch "%s\n" a) aiger.comments


let write_to_file aiger file = 
  let outch = open_out file in
  write aiger outch;
  close_out outch



let rename aiger renaming =
  List.fold_left (fun a (o,n) -> change_correspondance a o n) aiger renaming
  
let full_rename aiger renaming =
  let aux (name,ind) l (symmap,litmap) =
    try 
      let new_name = List.assoc name renaming in
      SymbolMap.add (new_name,ind) l symmap,
      LitMap.add l (new_name,ind) litmap
    with Not_found -> 
      SymbolMap.add (name,ind) l symmap,
      LitMap.add l (name,ind) litmap
  in
  let symmap,litmap = SymbolMap.fold aux aiger.symbols (SymbolMap.empty,LitMap.empty) in
  {aiger with symbols = symmap; abstract = litmap}


