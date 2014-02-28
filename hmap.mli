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

(*s Maps over hash-consed values, implemented as Patricia trees.
    See the module [Hashcons] and [Ptmap]. *)

type (+'a, 'b) t

type 'a key = 'a Hashcons.hash_consed

val empty : ('a, 'b) t

val add : 'a key -> 'b -> ('a, 'b) t -> ('a, 'b) t

val replace : 'a key -> 'b -> ('a, 'b) t -> ('a, 'b) t

(** may raise Not_found if the key is not in the map *)
val update : 'a key -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t

val find : 'a key -> ('a, 'b) t -> 'b

val remove : 'a key -> ('a, 'b) t -> ('a, 'b) t

val mem :  'a key -> ('a, 'b) t -> bool

val iter : ('b -> unit) -> ('a, 'b) t -> unit

val iteri : ('a key -> 'b -> unit) -> ('a, 'b) t -> unit

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val mapi : ('a key -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold : ('a key -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val exists : ('b -> bool) -> ('a, 'b) t -> bool

val for_all : ('b -> bool) -> ('a, 'b) t -> bool

val for_all2 : ('b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool

val map2 : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

val mapi2 : ('a key -> 'b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

val iter2 : ('b -> 'c -> unit) -> ('a, 'b) t -> ('a, 'c) t -> unit
