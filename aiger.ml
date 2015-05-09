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

type var = int

module Lit = 
struct
  type t = int
  let compare a b = a - b

  let of_var i = i * 2
  let to_var i = i / 2
  let to_int i = i
  let of_int i = i
end
  
let aiger_false = 0
let aiger_true = 1
let sign i = i mod 2 = 1
let strip i = if sign i then i - 1 else i 
let aiger_not l = if sign l then l - 1 else l + 1
let var2lit i = i * 2
let var2int i = i * 2
let lit2var i = i / 2
let lit2int i = i
let int2lit i = i
let int2var i = i

module LitMap = Map.Make(Lit)

module Symbol =
struct
  type t = string * (int option) 
  let compare = compare 
  let to_string = function
    | s, None -> s
    | s, Some i -> Printf.sprintf "%s<%d>" s i
      
  let of_string string = 
    match Str.full_split (Str.regexp "[ <>]") string with
      | Str.Text name :: Str.Delim "<" :: Str.Text index :: Str.Delim ">" :: _  -> name,Some (int_of_string index)
      | Str.Text name :: _ -> name, None
      | _ -> failwith ("In Aiger.Symbol.of_string: could not parse symbol "^string)

end

module SymbolMap = Map.Make(Symbol)
type lit = Lit.t

type t = {
  maxvar:int;   
  num_inputs:int;
  num_latches:int;
  num_outputs:int;
  num_ands:int;

  inputs:lit list;
  latches:(lit*lit) list;
  outputs:lit list;

  ands: (lit*lit*lit) list;

  comments:string list;

  symbols: lit SymbolMap.t;
  abstract: Symbol.t LitMap.t
}

let nth_input aiger a = List.nth aiger.inputs a
let nth_output aiger a = List.nth aiger.outputs a
let nth_latch aiger a = List.nth aiger.latches a

let index a = 
  let rec aux i = function 
    | b :: s when b = a -> i
    | b :: s -> aux (i+1) s
    | [] -> raise Not_found
  in aux 0 

let index_input aiger i = index i aiger.inputs
let index_output aiger i = index i aiger.outputs
let index_latch aiger i = index i aiger.latches

let lit2symbol aiger index = LitMap.find index aiger.abstract
let symbol2lit aiger lit = 
  try SymbolMap.find lit aiger.symbols 
  with Not_found -> 
       (* Printf.eprintf "Warning: no literal found for symbol %s\n" (Symbol.to_string lit);*)
    match lit with 
    | (a,None) -> SymbolMap.find (a,Some 0) aiger.symbols 
    | _ -> raise Not_found

let lit2string aiger lit = Symbol.to_string (lit2symbol aiger lit)

let add_correspondance aiger lit symbol =
  let a = LitMap.add lit symbol aiger.abstract in
  let s = SymbolMap.add symbol lit aiger.symbols in
  {aiger with abstract=a; symbols=s}

let change_correspondance aiger old_symbol new_symbol =
  try
    let lit = SymbolMap.find old_symbol aiger.symbols in
    let a = LitMap.add lit new_symbol aiger.abstract in
    let s = SymbolMap.remove old_symbol aiger.symbols in
    let s = SymbolMap.add new_symbol lit s in
    {aiger with abstract=a; symbols=s}
  with 
    Not_found -> Printf.eprintf "Warning: symbol %s was not previously defined\n" (Symbol.to_string old_symbol);
      aiger


let read_symbols aiger =
  List.fold_left
    (fun aig line -> 
     try
       let id,name,index = match Str.full_split (Str.regexp "[ <>]") line with
	 | Str.Text id :: Str.Delim " " :: Str.Text name :: Str.Delim "<" :: Str.Text index :: Str.Delim ">" :: _  -> (id,name,Some (int_of_string index) )
	 | Str.Text id :: Str.Delim " " :: Str.Text name :: _ -> id,name,None
	 | _ -> failwith ("In Aiger.read_symbols: could not parse symbol "^line)
       in
       let lit = Scanf.sscanf id "%c%d"
			      (fun t a -> match t with
					  | 'i' -> nth_input aiger a 
					  | 'o' -> nth_output aiger a
					  | 'l' -> fst (nth_latch aiger a)
					  | c -> failwith (Printf.sprintf "In Aiger.read_symbols: unknown variable type %c" c))
	      
       in add_correspondance aig lit (name,index)
			     
     with _ -> {aig with comments = line::aig.comments}
    ) {aiger with comments = []} aiger.comments
    
let parse inch = 
  let line = input_line inch in
  let (maxvar,num_inputs, num_latches,num_outputs,num_ands) = 
    Scanf.sscanf line "aag %d %d %d %d %d" (fun v a b c d -> (v,a,b,c,d))
  in 

  let inputs = ref [] in 
  let latches = ref [] in
  let outputs = ref [] in
  let ands = ref [] in

  for i = 0 to num_inputs - 1
  do
    try 
      let line = input_line inch in
      Scanf.sscanf line "%d" (fun a -> inputs := a :: !inputs)
    with End_of_file -> Printf.eprintf "Error: end of file reached while reading input#%d\n" (i+1); raise End_of_file
  done;

  for i = 0 to num_latches - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d" (fun a b -> latches := (a,b) :: !latches)
    with End_of_file -> Printf.eprintf "Error: end of file reached while reading latch#%d\n" (i+1); raise End_of_file
  done;

  for i = 0 to num_outputs - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d" (fun a ->  outputs := a :: !outputs)
    with End_of_file -> Printf.eprintf "Error: end of file reached while reading output#%d\n" (i+1); raise End_of_file
  done;

  for i = 0 to num_ands - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d %d" (fun a b c -> ands := (a,b,c) :: !ands)
    with End_of_file -> Printf.eprintf "Error: end of file reached while reading and#%d/%d\n" (i+1) num_ands; raise End_of_file
  done;

  let comments = ref [] in
  let stop = ref false in

  while (not !stop) do
    try 
      let line = input_line inch in
      comments := line :: !comments
    with End_of_file -> stop := true
  done;

  let com = List.rev !comments in
  
  read_symbols 
    {maxvar=maxvar; num_inputs=num_inputs; num_latches=num_latches;
     num_outputs=num_outputs; num_ands=num_ands;
     inputs = List.rev !inputs; latches = List.rev !latches; 
     outputs = List.rev !outputs; ands = List.rev !ands; 
     comments= com;
     symbols = SymbolMap.empty;
     abstract = LitMap.empty;
  }

let read inch = parse inch

let read_from_file file =  
  let inch = open_in file in
  let aiger = read inch in
  close_in inch;
  aiger


let empty =   
  {maxvar=0; num_inputs=0; num_latches=0;
   num_outputs=0;num_ands=0;inputs=[];
   latches=[]; outputs=[]; ands=[]; 
   comments= []; 
   abstract = LitMap.empty;
   symbols = SymbolMap.empty;
  }

let new_var t = {t with maxvar = t.maxvar+1}, (t.maxvar+1)

exception AlreadyExists

let insert x l = 
  let rec aux accu = function
    | [] -> List.rev_append accu [x]
    | a :: s when a < x -> aux (a::accu) s
    | a :: s -> List.rev_append accu (x :: a :: s)
  in aux [] l

(* raise an exception if teh literal is already in the list *)
let insert_unique x l =
  let rec aux accu = function
    | [] -> List.rev_append accu [x]
    | a :: s when a < x -> aux (a::accu) s
    | a :: s when a > x -> List.rev_append accu (x :: a :: s)
    | a :: s -> raise AlreadyExists
  in aux [] l
    
let add_input t lit name =
  let num_inputs = t.num_inputs + 1 in
  let inputs = insert_unique lit t.inputs in
  let nt = {t with num_inputs = num_inputs; inputs = inputs; maxvar = max t.maxvar (lit2var lit)} in
  add_correspondance nt lit name

let add_latch t lit next name = 
  let num_latches = t.num_latches + 1 in
  let latches = insert_unique (lit,next) t.latches in
  let nt = { t with num_latches = num_latches; latches = latches; maxvar = max t.maxvar (lit2var lit);} in
  add_correspondance nt lit name


let add_output t lit name = 
  let num_outputs = t.num_outputs + 1 in
  let outputs = insert lit t.outputs in
  let nt = { t  with num_outputs = num_outputs; outputs = outputs; maxvar = max t.maxvar (lit2var lit)} in
  add_correspondance nt lit name


let add_and t lhs rhs0 rhs1 = 
  let num_ands = t.num_ands + 1 in
  let ands = insert (lhs,rhs0,rhs1) t.ands in
  {t with num_ands = num_ands; ands = ands;maxvar = max t.maxvar (lit2var lhs) }

let add_comment t comment = { t with comments = comment :: t.comments}


type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

let lit2tag t lit = 
  if lit < 2 then Constant (lit = 1)
  else 
    match List.fold_left (fun accu l -> if l = lit then Some (Input l) else accu) None t.inputs with
    | Some x -> x
    | None ->
       match List.fold_left (fun accu (l,n) -> if l = lit then Some (Latch (l,n)) else accu) None t.latches with
       | Some x -> x
       | None ->
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


let compose aiger1 aiger2 =
  let map = Hashtbl.create aiger2.num_ands in
  let find x = 
    try if sign x then aiger_not (Hashtbl.find map (strip x)) else Hashtbl.find map x
    with Not_found ->
      Printf.eprintf "Error: in Aiger.compose: lit %d not found\n" x;
      raise Not_found
  in

  Hashtbl.add map aiger_true aiger_true;
  Hashtbl.add map aiger_false aiger_false;

  SymbolMap.iter
    (fun (name,i) lit -> 
      if SymbolMap.mem (name,i) aiger2.symbols
      then Hashtbl.add map (symbol2lit aiger2 (name,i)) lit
    ) aiger1.symbols;

  (* add entry in the table for latches of the second module *)
  let aiger = List.fold_left
    (fun a (l,n) -> 
      let na,nvar = new_var a in 
      Hashtbl.add map l (var2lit nvar);
      na
    ) aiger1 aiger2.latches
  in

  let aiger = List.fold_left
    (fun a i -> 
      (* if the input is not an output of the first module... *)
      if not (Hashtbl.mem map i)
      then (* ... we need to add it *)
	let na,nvar = new_var a in 
	Hashtbl.add map i (var2lit nvar);
	na
      else a
    ) aiger aiger2.inputs
  in

  (* add entry in the table for gates of the second module *)
  let aiger = List.fold_left
    (fun a (g,l,r) -> 
      let na,nvar = new_var a in 
      Hashtbl.add map g (var2lit nvar);
      na
    ) aiger aiger2.ands
  in
  
  (* add inputs of the second module *)
  let aiger = List.fold_left
    (fun a i -> 
      if SymbolMap.mem (lit2symbol aiger2 i) aiger1.symbols
      then a 
      else add_input a (find i) (lit2symbol aiger2 i)
    ) aiger aiger2.inputs
  in

  (* add gates of the second module *)
  let aiger = List.fold_left 
    (fun a (g,l,r) -> 
      add_and a (find g) (find l) (find r)
    ) aiger aiger2.ands
  in

  (* add latches of the second module *)
  let aiger = List.fold_left 
    (fun a (l,n) -> 
      add_latch a (find l) (find n) (lit2symbol aiger2 l)
    ) aiger aiger2.latches
  in
  
  (* add outputs of the second module *)
  let aiger = List.fold_left
    (fun a o -> 
      add_output a (find o) (lit2symbol aiger2 o)
    ) aiger aiger2.outputs
  in

  aiger


