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

(*s Maps of integers implemented as Patricia trees, following Chris
    Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
    ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
    See the documentation of module [Ptset] which is also based on the
    same data-structure. *)

open Hashcons

type 'a key = 'a hash_consed

type ('a, 'b) t =
  | Empty
  | Leaf of 'a key * 'b
  | Branch of int * int * ('a, 'b) t * ('a, 'b) t

let empty = Empty

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf (j,_) -> k.tag == j.tag
  | Branch (_, m, l, r) -> mem k (if zero_bit k.tag m then l else r)



let rec find k = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if k.tag == j.tag then x else raise Not_found
  | Branch (_, m, l, r) -> find k (if zero_bit k.tag m then l else r)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then
    Branch (mask p0 m, m, t0, t1)
  else
    Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let insert f k x t =
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j, x') as t ->
	if j.tag == k.tag then
	  Leaf (k, f x x')
	else
	  join (k.tag, Leaf (k,x), j.tag, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k.tag p m then
	  if zero_bit k.tag m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k.tag, Leaf (k,x), p, t)
  in
  ins t

let add k x t = insert (fun y _ -> y) k x t 

let replace k x t = insert (fun _ _ -> x) k x t

let rec update k f t =
  let rec ins t = 
    match t with
	Empty -> raise Not_found
      | Leaf(j, x) ->
	if j.tag == k.tag then
	  Leaf (k, f x)
	else
	  raise Not_found
      | Branch(p,m,t0,t1) ->
	  if match_prefix k.tag p m then
	    if zero_bit k.tag m then
	      Branch (p, m, ins t0, t1)
	    else
	      Branch (p, m, t0, ins t1)
	  else
	    raise Not_found
  in
  ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t -> if k.tag == j.tag then Empty else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k.tag p m then
	  if zero_bit k.tag m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let rec iter f = function
  | Empty -> ()
  | Leaf (_,x) -> f x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec iteri f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iteri f t0; iteri f t1

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)

let rec mapi f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f k x)
  | Branch (p,m,t0,t1) -> Branch (p, m, mapi f t0, mapi f t1)

let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

let rec exists p s =
  match s with
      Empty -> false
    | Leaf (_, x) -> p x
    | Branch (_,_,t0,t1) -> exists p t0 || exists p t1
    
let rec for_all p s =
    match s with
      Empty -> true
    | Leaf (_,x) -> p x
    | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

let rec for_all2 p s1 s2 = 
  match s1, s2 with
    Empty, Empty -> true
  | Leaf (k1, x1), Leaf (k2, x2) -> 
    if k1.tag == k2.tag then p x1 x2
    else raise (Failure "Hmap.for_all2")
  | Branch (p0,m0,t00,t01), Branch (p1,m1,t10,t11) -> 
    if p0 = p1 && m0 = m1 then 
      for_all2 p t00 t10 && for_all2 p t01 t11
    else raise (Failure "Hmap.for_all2")
  | _, _ -> raise (Failure "Hmap.for_all2")

let rec map2 f s1 s2 =
  match s1, s2 with
    Empty, Empty -> Empty
  | Leaf (k1, x1), Leaf (k2, x2) -> 
    if k1.tag == k2.tag then Leaf(k1, f x1 x2)
    else raise (Failure "Hmap.map2")
  | Branch (p0,m0,t00,t01), Branch (p1,m1,t10,t11) -> 
    if p0 = p1 && m0 = m1 then
      Branch(p0, m0, map2 f t00 t10, map2 f t01 t11)
    else raise (Failure "Hmap.map2")
  | _, _ -> raise (Failure "Hmap.map2")

let rec mapi2 f s1 s2 =
  match s1, s2 with
    Empty, Empty -> Empty
  | Leaf (k1, x1), Leaf (k2, x2) -> 
    if k1.tag == k2.tag then Leaf(k1, f k1 x1 x2)
    else raise (Failure "Hmap.mapi2")
  | Branch (p0,m0,t00,t01), Branch (p1,m1,t10,t11) -> 
    if p0 = p1 && m0 = m1 then
      Branch(p0, m0, mapi2 f t00 t10, mapi2 f t01 t11)
    else raise (Failure "Hmap.mapi2")
  | _, _ -> raise (Failure "Hmap.mapi2")

let rec iter2 f s1 s2 =
  match s1, s2 with
      Empty, Empty -> ()
    | Leaf (k1, x1), Leaf (k2, x2) -> 
    if k1.tag == k2.tag then f x1 x2
    else raise (Failure "Hmap.iter2")
  | Branch (p0,m0,t00,t01), Branch (p1,m1,t10,t11) -> 
    if p0 = p1 && m0 = m1 then
      begin iter2 f t00 t10; iter2 f t01 t11 end
    else raise (Failure "Hmap.iter2")
  | _, _ -> raise (Failure "Hmap.iter2")


let rec merge f s1 s2 =
   match s1, s2 with
     Empty, s | s, Empty -> s
   | Branch (p1, m1, l1, r1), Branch(p2, m2, l2, r2) 
     when m1 = m2 && p1 = p2 ->
       let l = merge f l1 l2 in
       let r = merge f r1 r2 in
       Branch(p1, m1, l, r)
   
   | Branch (p1, m1, _, _), Branch(p2, m2, l2, r2) 
     when m1 < m2 && match_prefix p1 p2 m2 ->
	 if (zero_bit p1 m2) then
	   Branch(p2, m2, merge f s1 l2, r2)
	 else Branch(p2, m2, l2, merge f s1 r2)


   | Branch (p1, m1, l1, r1), Branch(p2, m2, _, _) 
     when m1 > m2 && match_prefix p2 p1 m1 ->
	 if (zero_bit p2 m1) then
	   Branch (p1, m1, merge f l1 s2, r1)
	 else Branch(p1, m1, l1, merge f r1 s2)


   | Branch (p1, _, _, _), Branch (p2, _, _, _) ->
     join (p1, s1, p2, s2) 


   | Leaf (k, x), t | t, Leaf (k, x) -> insert f k x t

let is_singleton s =
  match s with
    Leaf _ -> true
  | _ -> false

let meet f s1 s2 =
  let rec meet s1 s2 =
    match s1, s2 with
      | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) 
	  when p1 = p2 && m1 = m2 -> 
	  let l = meet l1 l2 in
	  let r = meet r1 r2 in
	    Branch (p1, m1, l, r)

      | Branch (p1, m1, _, _), Branch (p2, m2, l2, r2)
	  when m1 < m2 && (match_prefix p1 p2 m2) ->
	  if (zero_bit p1 m2) then meet s1 l2 else meet s1 r2

      | Branch (p1, m1, l1, r1), Branch (p2, m2, _, _) 
	  when m1 > m2 && (match_prefix p2 p1 m1) ->
	  if (zero_bit p2 m1) then meet l1 s2 else meet r1 s2

      | Branch _, Branch _ -> Empty

      | Leaf (k1, x1), Leaf (k2, x2) when k1.tag == k2.tag -> Leaf (k1, f x1 x2)

      | Leaf (k, x), t | t, Leaf (k, x) -> begin
	  try
	    let y = find k t in
	    let x = f x y in
	      Leaf (k, x)
	  with Not_found | Exit -> Empty
	end
      | Empty, _ | _, Empty -> Empty
  in
    meet s1 s2
