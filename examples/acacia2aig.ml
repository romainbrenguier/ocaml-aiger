let lexer = Genlex.make_lexer ["State"; "initial"; ":"; "("; ")"; ","; "!"; "&&"; "U"; "||"; "{"; "}"; "to"; "state"; "labeled"]

let nb_var = ref 0 
let new_var () = incr nb_var; 2 * !nb_var 

let parse = 
  let tab_variables = Hashtbl.create 10 in

  
  let is_initial = parser 
    | [< 'Genlex.Kwd ","; 'Genlex.Kwd "initial" >] -> true
    | [< >] -> false
  in
 

  let rec parse_conjunction accu = parser
    | [< 'Genlex.Kwd "!"; e = parse_var_conjunction; f = parse_remainder_conjunction ((false,e)::accu) >] ->  f
    | [< e = parse_var_conjunction; f = parse_remainder_conjunction ((true,e)::accu) >] ->  f
  and parse_var_conjunction = parser
      | [< 'Genlex.Ident v >] ->  
	try Hashtbl.find tab_variables v 
	with Not_found -> 
	  let var = new_var () in
	  Hashtbl.add tab_variables v var;
	  var

  and parse_remainder_conjunction accu = parser
      | [< 'Genlex.Kwd "&&"; e = parse_conjunction accu >] -> e
      | [< >] -> accu
  in

  let parse_implication = parser
    | [< 'Genlex.Kwd "("; e = parse_conjunction [] ; 'Genlex.Kwd ")"; 'Genlex.Kwd "U"; 'Genlex.Kwd "("; f = parse_conjunction []; 'Genlex.Kwd ")" >] -> (e,f)
  in

  let rec parse_label accu = parser
    | [< 'Genlex.Kwd "("; e = parse_implication ; 'Genlex.Kwd ")"; f = parse_remainder_label (e :: accu) >] -> f
  and parse_remainder_label accu = parser
      | [< 'Genlex.Kwd "||"; e = parse_label accu >] -> e
      | [< >] -> accu
  in

      
  let rec parse_transitions accu = parser
    | [< 'Genlex.Kwd "to"; 'Genlex.Kwd "state"; 'Genlex.Int i; 'Genlex.Kwd "labeled"; e = parse_label []; list = parse_transitions ((i,e):: accu) >] -> list
    | [< >] -> accu
  in

  let parse_state = parser
    | [< 'Genlex.Int s; i = is_initial; 'Genlex.Kwd ":"; 'Genlex.Ident outgoing; 'Genlex.Ident transitions; 'Genlex.Kwd ":"; list = parse_transitions [] >] -> s,i,list
  in
  
  let rec parse_transition_system accu = parser
    | [< 'Genlex.Kwd "State"; s = parse_state; f = parse_transition_system (s::accu) >] -> f
    | [< >] -> accu
  in

  parser
  | [< 'Genlex.Ident transition; 'Genlex.Ident system; 'Genlex.Kwd "{"; e=parse_transition_system []; 'Genlex.Kwd "}" >] ->  e

    
let to_aiger transition_system = 
  let nb_states = List.length transition_system in
  let tab = Hashtbl.create nb_states in

  let output_tab = Hashtbl.create nb_states in

  let state = Array.init nb_states (fun i ->  new_var ()) in
  (*Expression.var "state" (Type.int (Common.log nb_states))*) 
  
  List.iter 
    (fun (s,i,trans) ->
      if i && s <> 0 then failwith "initial state is different from 0";
      if s >= nb_states then failwith "index of state is greater than the number of states";
      List.iter
	(fun (target,update_list) ->
	  List.iter (fun (inputs,outputs) ->
	    let expr = List.fold_left (fun e (b,x) -> if b then e $& x else e $& neg x) AigerImperative.aiger_true inputs in
	    let s_and_expr = expr $& (state $= Expression.int s) in
	    List.iter (fun (pos,out) ->
	      if pos 
	      then 
		try 
		  let p = Hashtbl.find output_tab out in
		  Hashtbl.replace output_tab out (p $| s_and_expr)
		with Not_found -> Hashtbl.add output_tab out s_and_expr
	    ) outputs;
	    try 
	      let p = Hashtbl.find tab target in
	      Hashtbl.replace tab target 
		(p $| s_and_expr)
	    with Not_found -> Hashtbl.add tab target s_and_expr
	  ) update_list
	) trans
    ) transition_system;


  let update_state =
    Hashtbl.fold (fun k e accu -> 
      Expression.ite e (Expression.int k) accu) tab (Expression.int 0)
  in
  
  Hashtbl.fold (fun k e accu -> (k,e) :: accu) output_tab  [state, update_state]
    
      

let main = 
  let inch = Sys.argv.(1) |> open_in in
  let stream = inch |> Stream.of_channel |> lexer in
  try
    let spec = parse stream in 
    close_in inch;
    spec |> to_speculog 
  with Stream.Error _ -> Printf.eprintf "%s\n" (Parser.remaining_tokens stream)

  
