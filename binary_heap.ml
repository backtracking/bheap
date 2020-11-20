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

(*s Heaps *)

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

module Make(X : Ordered) : H with type elt = X.t = struct

  (** Type of elements in the heap **)
  type elt = X.t

  (* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t = {
      mutable size : int;
      mutable data : elt array;
             dummy : elt;
           min_cap : int; (* minimal capacity, as given initially *)
    }
  (* invariant 0 <= size <= length data *)
  (* invariant data[size..] only contains dummy *)

  let create ~dummy n =
    if n < 0 || n > Sys.max_array_length then invalid_arg "create";
    let n = max 16 n in
    { size = 0; data = Array.make n dummy; dummy = dummy; min_cap = n }

  let length h = h.size

  let is_empty h = h.size = 0

  (* [enlarge] doubles the size of [data] *)
  let enlarge h =
    let n = h.size in
    assert (n > 0 && n = Array.length h.data);
    let n' = min (2 * n) Sys.max_array_length in
    if n' = n then failwith "maximum capacity reached";
    let d = h.data in
    let d' = Array.make n' h.dummy in
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let shrink h =
    let n = Array.length h.data in
    let n' = max h.min_cap (n / 2) in
    assert (h.size <= n' && n' <= n);
    if n' < n then begin
      let d = h.data in
      let d' = Array.make n' h.dummy in
      Array.blit d 0 d' 0 h.size;
      h.data <- d'
    end

  let add h x =
    let n = h.size in
    if n == Array.length h.data then enlarge h;
    let d = h.data in
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x > 0 then begin
	d.(i) <- d.(fi);
	moveup fi
      end else
	d.(i) <- x
    in
    moveup n;
    h.size <- n + 1

  let minimum h =
    if h.size <= 0 then raise Empty;
    h.data.(0)

  let remove h =
    if h.size <= 0 then raise Empty;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    d.(n) <- h.dummy;
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
	let j =
	  let j' = j + 1 in
	  if j' < n && X.compare d.(j') d.(j) < 0 then j' else j
	in
	if X.compare d.(j) x < 0 then begin
	  d.(i) <- d.(j);
	  movedown j
	end else
	  d.(i) <- x
      else
	d.(i) <- x
    in
    movedown 0;
    if 4 * h.size < Array.length h.data then shrink h

  let pop_minimum h = let m = minimum h in remove h; m

  let iter f h =
    let d = h.data in
    for i = 0 to h.size - 1 do f d.(i) done

  let fold f h x0 =
    let n = h.size in
    let d = h.data in
    let rec foldrec x i =
      if i >= n then x else foldrec (f d.(i) x) (succ i)
    in
    foldrec x0 0

end
