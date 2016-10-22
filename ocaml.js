var code = `module NfaImpl =
struct

type transition = int * char option * int

type nfa = int * int list * transition list

(*method I found online because couldnt get sort_uniq to work*)
let rec remove_dups lst= match lst with 
| [] -> []
| h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t));;

(* starting state, final states,transitions*)
let make_nfa ss fs ts = (ss,fs,ts);;
  
(* returns all states with epsilon transition in trans list*)
let rec e_only l = match l with
  |[] -> []
  |(s,tr,d)::t->if tr = None then (s,tr,d)::e_only t else e_only t;;

(*finds all reachable from one state*)
let rec e_help x y = match y with
  |[] -> []
  |(src,tran,dst)::t -> if src = x then dst::e_help x t else e_help x t;;

(*finds epsilon closure without running back through results*)
let rec e_curr m ss = let (a,b,c) = m in match ss with
  |[] -> []
  |h::t -> h::(e_help h (e_only c))@(e_curr m t);;
  
(*currently doesn't check reachable from reachable from reachable...*)
let rec e_closure m ss = let cur = remove_dups (e_curr m ss) in
                         if cur = remove_dups (e_curr m cur)
                         then List.sort (compare) cur
                         else List.sort (compare)
                                           (remove_dups (cur@e_closure m cur));;
                         (*if ss = remove_dups (e_curr m ss)
                         then ss
                         else remove_dups (ss@e_closure m (e_curr m ss));;*)
(*  
(*returns tuples of (src,dst) that transition on char c*)
let rec c_only l c = match l with
  |[] -> []
  |(src,tran,dst)::t -> if tran = Some c then (src,dst)::c_only t c
                        else c_only t c;;
  
(*returns chain of paths,call with each starting node*)
let rec c_help l curr = match l with
  |[] -> []
  |(s,d)::t -> if curr = s then d::c_help t d else c_help t s;;
  *)
(* works if c_only returns sorted list of tuples*)
let rec move m ss c = let (a,b,tran) = m in
                      let ss = e_closure m ss in
                      let c_only l c =
                        List.fold_left(fun a h -> match h with
                                                  |(src,tran,dst) ->
                                                    if tran = Some c
                                                    then a@[(src,dst)] else a) [] l
                      in
                      let c_help l curr = List.fold_left(fun a h ->
                                                 match h with
                                                 |(src,dst) ->
                                                   if src = curr
                                                   then a@[dst] else a) [] l in
                      match ss with
                      |[] -> []
                      |h::t -> (c_help (c_only tran c)h)
                               @(move m t c);;

(*checks if any l1 and l2 share atleast one membl2*)
let rec member l1 l2 = match l1 with
  |[] -> false
  |h::t -> if List.mem h l2 then true else member t l2;;

let accept (m:nfa) s =
  let (ss,fs,ts) = m in
  let rec a_help states index =
    if index < String.length s then
      let  c = String.get s index
      in a_help (e_closure m (move m states c)) (index+1)
    else member fs states
  in a_help (e_closure m [ss]) 0 ;;
(* takes current state list, the string being checked, and current index*)
(*let rec a_help m states str index = let (ss,fs,ts) = m in
                                    let c = String.get str index in
                                    let moving = move m states c in
                                    let eps_x = e_closure m moving in
                                    if index >= (String.length str)-1 then
                                      if member fs eps_x then true else false
                                    else a_help m eps_x str (index+1);;

let accept (m:nfa) s =
  let (ss,fs,ts) = m in
  let free = e_closure m [ss] in match s with
                                 |"" -> if List.mem ss fs then true else false
                                 |_ -> a_help m free s 0;;
  *)
(*gets rid of transition element in 3 tuple, makes a 2 tuple*)
let rec turn_tup ts = match ts with
  |[] -> []
  |(s,tr,d)::t -> (s,d)::turn_tup t;;

(*returns list with duplicates of all states in nfa*)
let rec find_states (ss,fs,ts)=
  ss::fs@List.fold_left (fun a (s,t,d) -> s::d::a) [] ts;;

let rec frequency l ch (*tuple clist that changes*)
  (*cur_state*) = match ch with
  []-> []
  |h::t -> (fst h,List.fold_left
                    (fun a c -> if fst c = fst h then a+1 else a) 0 l)::
             frequency l t;;
  
let rec count_one one states = match states with
  |[] -> []
  |h::t -> (List.fold_left
             (fun a c -> if c = h then a+1 else a) 0 one,h)::count_one one t;;
(* (0,1) means 1 state with 0 outgoing edges. count all the pairs with the same src node*)
let stats (n:nfa) =
  let (ss,fs,ts) = n in
  (List.length (remove_dups (find_states n)),
   List.length fs,
   let (one,two) = List.split (turn_tup ts) in
   let not_combined = (count_one one (remove_dups (find_states n))) in 
   List.sort (compare)(remove_dups (frequency not_combined not_combined)));;

type regexp =
      Empty_String
    | Char of char
    | Union of regexp * regexp
    | Concat of regexp * regexp
    | Star of regexp

let rec regexp_to_string r = match r with
  |Empty_String -> "E"
  |Char c -> String.make 1 c
  |Union (a,b) -> regexp_to_string a ^ " " ^ regexp_to_string b ^ " |"
  |Concat (a,b) -> regexp_to_string a ^ " " ^ regexp_to_string b ^ " ."
  |Star a -> regexp_to_string a ^ " *"
;;
  
let next = 
  let count = ref 0 in
    function() -> 
      let temp = !count in
        count:= (!count) + 1;
        temp
;;


let rec regexp_to_nfa r = let src = next() in
                      let dst = next() in
                      match r with
  |Empty_String -> make_nfa src [dst] [(src,None,dst)]
  |Char c -> make_nfa src [dst] [(src,Some c,dst)]
  |Union (a,b) -> let (s1,f1,t1) = regexp_to_nfa a in
                  let (s2,f2,t2) = regexp_to_nfa b in
                  make_nfa src [dst] ([(src,None,s1);(src,None,s2);
                                      (List.hd f1,None,dst);
                                      (List.hd f2,None,dst)]@t1@t2)  
  |Concat (a,b) -> (*doesnt use next function (src or dst)*)
    let (s1,f1,t1) = regexp_to_nfa a in
    let (s2,f2,t2) = regexp_to_nfa b in
    make_nfa s1 f2 ((List.hd f1,None,s2)::t1@t2)
  |Star a -> let (s1,f1,t1) = regexp_to_nfa a in
             make_nfa src [dst] ([(src,None,dst);(List.hd f1,None,dst);
                                 (src,None,s1);(List.hd f1,None,s1)]@t1)
;;
 
exception IllegalExpression of string`

var codeArray = code.split(/\n(?!""")/);
var classMatch = /module \w+/g;
var methodMatch = /let (rec)? \w+  \w+/g;
var moc = [];
  for(var i = 0; i < codeArray.length; i++)
  {
  	if(codeArray[i].match(classMatch)){
  		var segment = codeArray[i].match(classMatch);
  		var curClass = segment[0].replace(/class /,"");
  		moc.push({line: i+1, name: curClass, type: "class"})
  	}
  	if(codeArray[i].match(methodMatch)){
  		var segment = codeArray[i].match(methodMatch);
  		var curMethod = segment[0].replace(/def /,"");
  		moc.push({line: i+1, name: curMethod, type: "method"})
  	}
  }
  console.log(codeArray);
  console.log(moc);