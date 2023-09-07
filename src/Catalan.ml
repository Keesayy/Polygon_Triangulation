open Printf 
open Float 
open Sys 

(**********************
    Nombres de Catalan
************************)

(** int -> int 
renvoie le n ème nombre de Catalan à l'aide de 	la formule de récurrence **)
let catalan_recurrence n = 
	(* On somme de k=0 à n les Ck*Cn-k *)
	let rec cn_plus_1 n k acc =
		if n = -1 then acc + 1 else
		match k with
		|_ when k = n+1 -> acc
		|_ ->
			begin (* tmp = Ck * Cn-k *)
				let tmp = (cn_plus_1 (k-1) 0 0) * (cn_plus_1 (n-k-1) 0 0) in
				cn_plus_1 n (k+1) (acc + tmp)
			end
	in cn_plus_1 (n-1) 0 0


(** int -> int 
renvoie le n ème nombre de Catalan à l'aide la formule avec coeff binomiaux **)
let rec catalan_binomial n =
	match n with 
	|0 -> 1
    |_ -> (2*(2*n-1) * catalan_binomial (n-1) ) / (n+1)


(** int -> float 
équivalent asymptotique en utilisant la Formule de Stirling **)
let catalan_asymptotique n =
	(* a = 4^n , b = n^3/2 * sqrt(pi) *)
	let a = (Float.pow 4. (float_of_int n) ) in
	let b = ( (Float.pow (float_of_int n) (3./.2.) ) *. (Float.sqrt (4. *. (Float.atan 1.) ) ) ) in
	int_of_float (a /. b) 


(*  'a -> 'b) -> 'a -> 'b * float 
renvoie le résultat de la fonction appliqué à n et son temps d'éxécution *)
let time f n =
	let t1 = Sys.time() in
    let a = f n in
    let t2 = Sys.time() in   
    	(a, t2 -. t1)


(* int -> unit *)
let affiche n = 
	let affiche_aux f =
		let (a,b) = time f n in
		Printf.printf ": %d               C : %f\n" a b
	in
	print_string "Catalan_rec : " ; affiche_aux catalan_recurrence ;
	print_string "Catalan_bin : " ; affiche_aux catalan_binomial ;
	print_string "Catalan_asy : " ; affiche_aux catalan_asymptotique
