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

module Make(X : Ordered) = struct

  (* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t = { mutable size : int; mutable data : X.t array; dummy: X.t }
         (* invariant 0 <= size <= length data *)
         (* invariant data[size..] only contains dummy *)

  let create ~dummy n =
    if n < 0 || n > Sys.max_array_length then invalid_arg "create";
    { size = 0; data = Array.make (max 16 n) dummy; dummy = dummy }

  let length h = h.size

  let is_empty h = h.size = 0

  (* [enlarge] doubles the size of [data] *)
  let enlarge h =
    let n = h.size in
    assert (n > 0);
    let n' = min (2 * n) Sys.max_array_length in
    if n' = n then failwith "maximum capacity reached";
    let d = h.data in
    let d' = Array.make n' h.dummy in
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let add h x =
    let n = h.size in
    if n == Array.length h.data then enlarge h;
    let d = h.data in
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x < 0 then begin
	d.(i) <- d.(fi);
	moveup fi
      end else
	d.(i) <- x
    in
    moveup n;
    h.size <- n + 1

  let maximum h =
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
	  if j' < n && X.compare d.(j') d.(j) > 0 then j' else j
	in
	if X.compare d.(j) x > 0 then begin
	  d.(i) <- d.(j);
	  movedown j
	end else
	  d.(i) <- x
      else
	d.(i) <- x
    in
    movedown 0

  let pop_maximum h = let m = maximum h in remove h; m

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
