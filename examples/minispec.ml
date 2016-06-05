(* Minimial specification language for AIG *)
open AigerImperative

let lexer = Genlex.make_lexer ["input";"output";"reg";"wire";"<-";"(";")";"&";"|";"~"]

let parse stream = 
  let aiger = empty () in
  let variables = Hashtbl.create 100 in
  let rec parse_lit = parser
    | [< 'Genlex.Kwd "~"; l = parse_lit >] -> neg l
    | [< 'Genlex.Ident name >] -> Hashtbl.find variables name
    | [< 'Genlex.Kwd "("; e = parse_expr; 'Genlex.Kwd ")" >] -> e
  and parse_term = parser
      | [< l = parse_lit >] -> 
	(parser 
	    | [< 'Genlex.Kwd "&" >] -> conj aiger l (parse_term stream)
	    | [< >] -> l
	) stream

  and parse_expr = parser
    | [< t = parse_term >] -> 
      ( parser 
	  | [< 'Genlex.Kwd "|"; e = parse_expr >] -> neg (conj aiger (neg t) (neg e))
	  | [< >] -> t) stream
  in
  let parse_decl = parser
| [< 'Genlex.Kwd "input"; 'Genlex.Ident name >] -> 
    Hashtbl.add variables name (add_input aiger name)
| [< 'Genlex.Kwd "output"; 'Genlex.Ident name; 'Genlex.Kwd "<-"; e=parse_expr >] -> 
    set_output aiger name e
| [< 'Genlex.Kwd "reg"; 'Genlex.Ident name >] -> 
    Hashtbl.add variables name (add_latch aiger name)
| [< 'Genlex.Ident name; 'Genlex.Kwd "<-"; e=parse_expr >] -> 
    set_latch_update aiger (Hashtbl.find variables name) e
  in 

  let rec parse_file = parser
    | [< e=parse_decl; f=parse_file >] -> ()
    | [< >] -> ()
  in
  parse_file stream;
  aiger

let next_token stream =
  (match Stream.next stream with
  | Genlex.Kwd x-> "Kwd "^x
  | Genlex.Ident x -> "Ident "^x
  | Genlex.String x -> "String "^x
  | Genlex.Float f -> "Float "^string_of_float f
  | Genlex.Int i -> "Int "^string_of_int i
  | Genlex.Char c -> "Char "^String.make 1 c)

let main =
  let stream =
  stdin 
  |> Stream.of_channel 
  |> lexer 
  in
  try
    stream
    |> parse
    |> write stdout
  with _ -> failwith (next_token stream)
