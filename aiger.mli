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



(** An OCaml library for AIGER files *)


(** type for literals *)
type lit

(** type for variables *)
type var 

val aiger_false : lit
val aiger_true : lit

val sign : lit -> bool
val strip : lit -> lit
val aiger_not : lit -> lit
val var2lit : var -> lit
val var2int : var -> int
val lit2var : lit -> var
val lit2int : lit -> int
val int2lit : int -> lit
val int2var : int -> var

module Symbol :
sig
  type t = string * (int option) 
  val to_string : t -> string
  val of_string : string -> t
end

module SymbolMap : Map.S with type key = Symbol.t
module LitMap : Map.S with type key = lit

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
  abstract : Symbol.t LitMap.t;
}

val read : in_channel -> t
val read_from_file : string -> t
val write : t -> out_channel -> unit
val write_to_file : t -> string -> unit

val empty : t
val new_var : t -> (t * var)


exception AlreadyExists

val add_input : t -> lit -> Symbol.t -> t
(** [add_latch aiger lit next name] *)
val add_latch : t -> lit -> lit -> Symbol.t -> t
val add_output : t -> lit -> Symbol.t -> t
(** [add_and aiger lhs rhs0 rhs1] *)
val add_and : t -> lit -> lit -> lit -> t

val add_comment : t -> string -> t

(** remove an output *)
val hide : t -> Symbol.t -> t
val full_hide : t -> string -> t

val nth_input : t -> int -> lit 
val nth_output : t -> int -> lit 
val nth_latch : t -> int -> (lit * lit)

(** These function may raise [Not_found] *)
val lit2string : t -> lit -> string
val lit2symbol : t -> lit -> Symbol.t
val symbol2lit : t -> Symbol.t -> lit

(** Give an array containing all the literals encoding the given name.
    For example, for an integer "i" encoded on two bits, [name_to_literals a "i"] will return the literals corresponding to "i<0>" and "i<1>".
*)
val name_to_literals : t -> string -> lit array

(** Gives the number of literal encoding the variable with the given name. *)
val size_symbol : t -> string -> int

(** List of names used as symbols. *)
val names : t -> string list
val inputs : t -> string list
val outputs : t -> string list
val latches : t -> string list

type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

val lit2tag : t -> lit -> tag

(** Rename variables of the aiger file according to the correspondance given in the list *)
val rename : t -> (Symbol.t * Symbol.t) list -> t


val full_rename : t -> (string * string) list -> t


(** Merge 2 aiger files where symbols with the same name are merged.
    They are identified by their name in the comments.
*)
val compose : t -> t -> t

(** Reduce the size of the AIG by merging gates with same definition *)
val reduce : t -> t
