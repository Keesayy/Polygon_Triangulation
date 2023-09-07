open Coloriage
open Val

let time_ear_clipping (lst : polygon) =
	let t1 = Sys.time() in
	try 
		let _ = ear_clipping lst in 
		let t2 = Sys.time() in   
		Printf.printf "%d  %f\n" (List.length lst) (t2 -. t1)
	with
	(* Erreur due à la génération aléatoire polygone simple *)
	|Failure "pas d'oreilles" -> ()

let affiche_ear (l : polygon list) =
	Printf.printf "n temps\n" ;
	List.iter time_ear_clipping l

let nb_cameras lst =
	try
		let n = List.length lst in
		let c = count_colour lst in  
		Printf.printf "%d  %d\n" n (min_color c)
	with
	(* Erreur due à la génération aléatoire polygone simple *)
	|Failure "pas d'oreilles" -> ()

let affiche_camera l =
	Printf.printf "n camera borne\n" ;
	List.iter nb_cameras l