
(* quick test of Binary_heap *)

module E = struct
  type t = int
  let compare x y = - Stdlib.compare x y
end
module H = Binary_heap.Make(E)

let minimum = H.maximum
let pop_minimum = H.pop_maximum

let dummy = 1729
let h = H.create ~dummy 0
let () = assert (H.is_empty h)
let () = assert (H.length h = 0)
let () = H.add h 42
let () = assert (not (H.is_empty h))
let () = assert (H.length h = 1)
let () = assert (minimum h = 42)
let x = pop_minimum h
let () = assert (x = 42)
let () = assert (H.is_empty h)

let () = for i = 200 downto -200 do H.add h i done
let () = assert (minimum h = -200)
let () = assert (H.length h = 401)
let () = for i = -200 to 200 do
           assert (minimum h = i);
           let x = pop_minimum h in
           assert (x = i);
           assert (H.length h = 200 - i)
         done
