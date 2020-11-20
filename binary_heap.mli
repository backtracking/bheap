(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Traditional implementation of priority queues
    using a binary heap encoded in a resizable array *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception Empty

module type H = sig

  (** Type of elements in the heap **)
  type elt

  (** Type of priority queues. *)
  type t

  (** [create ~dummy c] creates a new heap, with initial capacity of [c].
      The value [dummy] is used to fill unused cells of the internal array.
      Note: [dummy] can still be used as a regular value in the queue. *)
  val create : dummy:elt -> int -> t

  (** [length h] returns the number of elements of [h] *)
  val length : t -> int

  (** [is_empty h] checks the emptiness of [h] *)
  val is_empty : t -> bool

  (** [add x h] adds a new element [x] in heap [h]; size of [h] is doubled
      when maximum capacity is reached; complexity $O(log(n))$ *)
  val add : t -> elt -> unit

  (** [minimum h] returns the minimum element of [h]; raises [Empty]
      when [h] is empty; complexity $O(1)$ *)
  val minimum : t -> elt

  (** [remove h] removes the minimum element of [h]; raises [Empty]
      when [h] is empty; complexity $O(log(n))$ *)
  val remove : t -> unit

  (** [pop_minimum h] removes the minimum element of [h] and returns it;
      raises [Empty] when [h] is empty; complexity $O(log(n))$ *)
  val pop_minimum : t -> elt

  (** usual iterators and combinators; elements are presented in
      arbitrary order *)
  val iter : (elt -> unit) -> t -> unit

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

end

module Make(X : Ordered) : H with type elt = X.t
