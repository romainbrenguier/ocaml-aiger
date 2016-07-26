open AigerImperative

let lexer = Genlex.make_lexer ["State"; "initial"; ":"; "("; ")"; ","; "!"; "&&"; "U"; "||"; "{"; "}"; "to"; "state"; "labeled"]

let aiger = empty ()

type label = {condition:(bool * lit) list; updates:(bool * string) list}
type state = {id:int; initial:bool; transitions:(int * label list) list}

let parse = 
  let is_initial = parser 
    | [< 'Genlex.Kwd ","; 'Genlex.Kwd "initial" >] -> true
    | [< >] -> false
  in

  let rec parse_conjunction accu = parser
    | [< 'Genlex.Kwd "!"; e = parse_var_conjunction; f = parse_remainder_conjunction ((false, e) :: accu) >] ->  f
    | [< e = parse_var_conjunction; f = parse_remainder_conjunction ((true,e):: accu) >] ->  f
  and parse_var_conjunction = parser
      | [< 'Genlex.Ident v >] ->  
	match string2lit aiger v 
	with | Some l -> l | None -> add_input aiger v

  and parse_remainder_conjunction accu = parser
      | [< 'Genlex.Kwd "&&"; e = parse_conjunction accu >] -> e
      | [< >] -> accu
  in

  let rec parse_conjunction_output accu = parser
	     | [< 'Genlex.Kwd "!"; e = parse_var_conjunction_output; f = parse_remainder_conjunction_output ((false, e) :: accu) >] ->  f
	     | [< e = parse_var_conjunction_output; f = parse_remainder_conjunction_output ((true,e):: accu) >] ->  f
  and parse_var_conjunction_output = parser
      | [< 'Genlex.Ident v >] ->  v 

  and parse_remainder_conjunction_output accu = parser
      | [< 'Genlex.Kwd "&&"; e = parse_conjunction_output accu >] -> e
      | [< >] -> accu
  in

  let parse_implication = parser
    | [< 'Genlex.Kwd "("; e = parse_conjunction [] ; 'Genlex.Kwd ")"; 'Genlex.Kwd "U"; 'Genlex.Kwd "("; f = parse_conjunction_output []; 'Genlex.Kwd ")" >] -> {condition=e; updates=f}
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
    | [< 'Genlex.Int s; i = is_initial; 'Genlex.Kwd ":"; 'Genlex.Ident outgoing; 'Genlex.Ident transitions; 'Genlex.Kwd ":"; list = parse_transitions [] >] -> {id=s; initial=i; transitions=list}
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
  let states = Hashtbl.create nb_states in

  for i = 0 to nb_states - 1
  do 
    Hashtbl.add states i (add_latch aiger ("state_"^string_of_int i))
  done;
  
  List.iter 
    (fun state ->
      
      if state.initial && state.id <> 0 then failwith "initial state is different from 0";
      if state.id >= nb_states then failwith "index of state is greater than the number of states";
      List.iter
	(fun (target,label_list) ->
	  List.iter (fun {condition=inputs;updates=outputs} ->
	    let expr = List.fold_left (fun e (b,v) -> conj aiger e (if b then v else neg v)) aiger_true inputs in
	    let s_and_expr = conj aiger expr (Hashtbl.find states state.id) in
	    List.iter (fun (pos,out) ->
	      if pos 
	      then 
		try 
		  let p = Hashtbl.find output_tab out in
		  Hashtbl.replace output_tab out (disj aiger p s_and_expr)
		with Not_found -> Hashtbl.add output_tab out s_and_expr
	    ) outputs;
	    try 
	      let p = Hashtbl.find tab target in
	      Hashtbl.replace tab target (disj aiger p s_and_expr)
	    with Not_found -> Hashtbl.add tab target s_and_expr
	  ) label_list
	) state.transitions
    ) transition_system;


  Hashtbl.iter (fun k e -> 
    set_latch_update aiger (Hashtbl.find states k) e
  ) tab;

  
  Hashtbl.iter (fun k e ->
    set_output aiger k e
  ) output_tab
    
      

let main = 
  let inch = Sys.argv.(1) |> open_in in
  let stream = inch |> Stream.of_channel |> lexer in
  try
    let spec = parse stream in 
    close_in inch;
    spec |> to_aiger;
    write stdout aiger
  with Stream.Error _ -> Printf.eprintf "Stream.Error at position %d\n" (Stream.count stream)

  
