
open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = fold(fun counter element-> if element = target then counter + 1 else counter) 0 lst ;;

let uniq lst  = fold(fun check element -> if (count_occ check element) == 0 then element::check else check) [] lst;;

let assoc_list lst = let nodups = uniq lst in fold(fun tup element -> (element,count_occ lst element)::tup) [] nodups;;

let ap fns args = (fold (fun acc element -> acc@(map element args))) [] fns;;   