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



(** An Imperative implementation of the OCaml library for AIGER files *)

type lit

val aiger_false : lit
val aiger_true : lit
val neg : lit -> lit

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

val read : in_channel -> t
val read_from_file : string -> t
val write : t -> out_channel -> unit
val write_to_file : t -> string -> unit

val empty : unit -> t

exception AlreadyExists

val add_input : t -> string -> lit
val add_latch : t -> string -> lit
val set_latch_update : t -> lit -> lit -> unit
val set_output : t -> string -> lit -> unit

(** [conj aiger rhs0 rhs1] gives [lhs]*)
val conj : t -> lit -> lit -> lit

val add_comment : t -> string -> unit

(** String corresponding to literals *)
val lit2string : t -> lit -> string option
val string2lit : t -> string -> lit option
(** These function may raise [Not_found] *)
val lit2string_exn : t -> lit -> string
val string2lit_exn : t -> string -> lit

(** remove an output *)
val hide : t -> Aiger.Symbol.t -> unit
val full_hide : t -> string -> unit

(** These functions raise [Not_found] if the index does not correspond to a literal *)
val nth_input_exn : t -> int -> lit 
val nth_output_exn : t -> int -> lit 
val nth_latch_exn : t -> int -> (lit * lit)


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
val rename : t -> (Aiger.Symbol.t * Aiger.Symbol.t) list -> unit

val full_rename : t -> (string * string) list -> unit


