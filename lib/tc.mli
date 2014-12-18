(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Mirage type-classes *)

type 'a equal = 'a -> 'a -> bool
(** Equalities. *)

type 'a compare = 'a -> 'a -> int
(** Comparators. *)

type 'a hash = 'a -> int
(** Hashing. *)

type 'a reader = Mstruct.t -> 'a
(** Mstruct reader. *)

exception Read_error of string
(** [Read_error] can be raised by the [reader] type-classes. *)

type 'a size_of = 'a -> int
(** Compute the size of the written objects. *)

type 'a writer = 'a -> Cstruct.t -> Cstruct.t
(** Write a value to a cstruct buffer. Return a new buffer ready for
    the next write. *)

(** JSON converters. *)
type 'a to_json = 'a -> Ezjsonm.value
type 'a of_json = Ezjsonm.value -> 'a

(** Abstract Identifiers. *)
module type S0 = sig
  type t
  val equal: t equal
  val compare: t compare
  val hash: t hash

  (** The REST inteface. *)
  val to_json: t to_json
  val of_json: t of_json

  (** The serialization format. *)
  val size_of: t size_of
  val write: t writer
  val read: t reader
end

(** Abstract identifiers with one polymorphic parameter. *)
module type S1 = sig
  type 'a t
  val equal: 'a equal -> 'a t equal
  val compare: 'a compare -> 'a t compare
  val hash: 'a hash -> 'a t hash

  (** The REST interface *)
  val to_json: 'a to_json -> 'a t to_json
  val of_json: 'a of_json -> 'a t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'a t size_of
  val write: 'a writer -> 'a t writer
  val read: 'a reader -> 'a t reader
end

(** Abstract identifiers with two polymorphic parameters. *)
module type S2 = sig
  type ('a, 'b) t
  val equal: 'a equal -> 'b equal -> ('a, 'b) t equal
  val compare: 'a compare -> 'b compare -> ('a, 'b) t compare
  val hash: 'a hash -> 'b hash -> ('a, 'b) t hash

  (** The REST interface *)
  val to_json: 'a to_json -> 'b to_json -> ('a, 'b) t to_json
  val of_json: 'a of_json -> 'b of_json -> ('a, 'b) t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'b size_of -> ('a, 'b) t size_of
  val write: 'a writer -> 'b writer -> ('a, 'b) t writer
  val read: 'a reader -> 'b reader -> ('a, 'b) t reader
end

(** Abstract identifiers with two polymorphic parameters. *)
module type S3 = sig
  type ('a, 'b, 'c) t
  val equal: 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
  val compare: 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
  val hash: 'a hash -> 'b hash -> 'c hash -> ('a, 'b, 'c) t hash

  (** The REST interface *)
  val to_json: 'a to_json -> 'b to_json -> 'c to_json -> ('a, 'b, 'c) t to_json
  val of_json: 'a of_json -> 'b of_json -> 'c of_json -> ('a, 'b, 'c) t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'b size_of -> 'c size_of -> ('a, 'b, 'c) t size_of
  val write: 'a writer -> 'b writer -> 'c writer -> ('a, 'b, 'c) t writer
  val read: 'a reader -> 'b reader -> 'c reader -> ('a, 'b, 'c) t reader
end

(** {1 Combinators} *)

type 'a t = (module S0 with type t = 'a)
(** The type-class of values of type ['a]. *)

val list: 'a t -> 'a list t
val option: 'a t -> 'a option t
val pair: 'a t -> 'b t -> ('a * 'b) t
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val unit: unit t
val int: int t
val int32: int32 t
val int64: int64 t
val string: string t
val cstruct: Cstruct.t t
val bool: bool t

val equal: 'a t -> 'a equal
val compare: 'a t -> 'a compare
val hash: 'a t -> 'a hash
val to_json: 'a t -> 'a to_json
val of_json: 'a t -> 'a of_json
val size_of: 'a t -> 'a size_of
val write: 'a t -> 'a writer
val read: 'a t -> 'a reader
val show: 'a t -> 'a -> string
val shows: 'a t -> 'a list -> string
val read_string: 'a t -> string -> 'a
val read_cstruct: 'a t -> Cstruct.t -> 'a
val write_string: 'a t -> 'a -> string
val write_cstruct: 'a t -> 'a -> Cstruct.t

val biject: 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t

(** {1 Builders} *)

(** Build abstract identifiers. *)
module Bin_prot0
    (S: sig
       type t
       val to_json: t to_json
       val of_json: t of_json
       val compare: t compare
       val bin_size_t: t Bin_prot.Size.sizer
       val bin_write_t: t Bin_prot.Write.writer
       val bin_read_t: t Bin_prot.Read.reader
     end):
  S0 with type t = S.t

(** Build abstract identifiers with a polymorphic parameters. *)
module Bin_prot1
    (S: sig
       type 'a t
       val to_json: 'a to_json -> 'a t to_json
       val of_json: 'a of_json -> 'a t of_json
       val compare: 'a compare -> 'a t compare
       val bin_size_t: ('a, 'a t) Bin_prot.Size.sizer1
       val bin_write_t: ('a, 'a t) Bin_prot.Write.writer1
       val bin_read_t: ('a, 'a t) Bin_prot.Read.reader1
     end):
  S1 with type 'a t = 'a S.t

(** Build abstract identfiers with two polymorphic parameters. *)
module Bin_prot2
    (S: sig
       type ('a, 'b) t
       val to_json: 'a to_json -> 'b to_json -> ('a, 'b) t to_json
       val of_json: 'a of_json -> 'b of_json -> ('a, 'b) t of_json
       val compare: 'a compare -> 'b compare -> ('a, 'b) t compare
       val bin_size_t: ('a, 'b, ('a, 'b) t) Bin_prot.Size.sizer2
       val bin_write_t: ('a, 'b, ('a, 'b) t) Bin_prot.Write.writer2
       val bin_read_t: ('a, 'b, ('a, 'b) t) Bin_prot.Read.reader2
     end):
  S2 with type ('a, 'b) t = ('a, 'b) S.t

(** Build abstract identfiers with three polymorphic parameters. *)
module Bin_prot3
    (S: sig
       type ('a, 'b, 'c) t
       val to_json: 'a to_json -> 'b to_json -> 'c to_json -> ('a, 'b, 'c) t to_json
       val of_json: 'a of_json -> 'b of_json -> 'c of_json -> ('a, 'b, 'c) t of_json
       val compare: 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
       val bin_size_t: ('a, 'b, 'c, ('a, 'b, 'c) t) Bin_prot.Size.sizer3
       val bin_write_t: ('a, 'b, 'c, ('a, 'b, 'c) t) Bin_prot.Write.writer3
       val bin_read_t: ('a, 'b, 'c, ('a, 'b, 'c) t) Bin_prot.Read.reader3
     end):
  S3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) S.t

(** Monorphize a type with one parameter. *)
module App1 (F: S1)(X: S0): S0 with type t = X.t F.t

(** Monorphize a type with two parameters. *)
module App2(F: S2)(X: S0)(Y: S0): S0 with type t = (X.t, Y.t) F.t

(** Monorphize a type with three parameters. *)
module App3(F: S3)(X: S0)(Y: S0)(Z: S0): S0 with type t = (X.t, Y.t, Z.t) F.t

(** {1 Useful instances} *)

module Bool: S0 with type t = bool
module String: S0 with type t = string
module Cstruct: S0 with type t = Cstruct.t
module Unit: S0 with type t = unit

module Option (A: S0): S0 with type t = A.t option
module O1: S1 with type 'a t = 'a option

module Pair (A: S0) (B: S0): S0 with type t = A.t * B.t
module P2: S2 with type ('a, 'b) t = 'a * 'b

module Triple (A: S0) (B: S0) (C: S0): S0 with type t = A.t * B.t * C.t
module T3: S3 with type ('a, 'b, 'c) t = 'a * 'b * 'c

module Int: S0 with type t = int
module Int32: S0 with type t = int32
module Int64: S0 with type t = int64

module List (A: S0): S0 with type t = A.t list
module L1: S1 with type 'a t = 'a list

module Set (A: S0): S0 with type t = Set.Make(A).t

module Biject (A: S0)
    (B: sig
       type t
       val to_t: A.t -> t
       val of_t: t -> A.t
     end)
  : S0 with type t = B.t

module As_L0
  (S: sig
     type t
     module K: S0
     val to_list: t -> K.t list
     val of_list: K.t list -> t
   end): S0 with type t := S.t
(** Manorphic list -like. *)

module As_L1
    (S: sig
       type 'a t
       val to_list: 'a t -> 'a list
       val of_list: 'a list -> 'a t
     end): S1 with type 'a t := 'a S.t
(** Polymorphic list -like. *)

module As_AL1
    (S: sig
       type 'a t
       module K: S0
       val to_alist: 'a t -> (K.t * 'a) list
       val of_alist: (K.t * 'a) list -> 'a t
     end): S1 with type 'a t := 'a S.t
(** Association list -like. *)

(** {1 Helpers} *)

module Reader: sig
  val to_bin_prot: 'a reader -> 'a Bin_prot.Read.reader
  val of_bin_prot: 'a Bin_prot.Read.reader -> 'a reader
  val pair: 'a reader -> 'b reader -> ('a * 'b) reader
  val triple: 'a reader -> 'b reader -> 'c reader -> ('a * 'b * 'c) reader
  val list: 'a reader -> 'a list reader
  val option: 'a reader -> 'a option reader
  val error: ('a, unit, string, 'b) format4 -> 'a
end

module Writer: sig
  val to_bin_prot: 'a writer -> 'a Bin_prot.Write.writer
  val of_bin_prot: 'a Bin_prot.Write.writer -> 'a writer
  val pair: 'a writer -> 'b writer -> ('a * 'b) writer
  val triple: 'a writer -> 'b writer -> 'c writer -> ('a * 'b * 'c) writer
  val list: 'a writer -> 'a list writer
  val option: 'a writer -> 'a option writer
end

module Compare: sig
  val pair: 'a compare -> 'b compare -> ('a * 'b) compare
  val triple: 'a compare -> 'b compare -> 'c compare -> ('a * 'b * 'c) compare
  val list: 'a compare -> 'a list compare
  val option: 'a compare -> 'a option compare
end

module Equal: sig
  val pair: 'a equal -> 'b equal -> ('a * 'b) equal
  val triple: 'a equal -> 'b equal -> 'c equal -> ('a * 'b * 'c) equal
  val list: 'a equal -> 'a list equal
  val option: 'a equal -> 'a option equal
end

module Size_of: sig
  val pair: 'a size_of -> 'b size_of -> ('a * 'b) size_of
  val triple: 'a size_of -> 'b size_of -> 'c size_of -> ('a * 'b * 'c) size_of
  val list: 'a size_of -> 'a list size_of
  val option: 'a size_of -> 'a option size_of
end
