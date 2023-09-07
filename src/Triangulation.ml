open Array 
open Stack 
open List 
open Arbre_BR

type point = int * int 

type polygon = point list 

type triangle = point * point * point 

let (mod) x y = ((x mod y) + y) mod y 

(* 'a -> 'a list -> int *)
let pos_lst e lst =
	let rec aux l count = 
	match l with
	|[] -> failwith "not in list"
	|hd :: tl -> if hd = e then count else aux tl (count+1)
in aux lst 0

(*************************
	Méthode des oreilles
**************************)

(** point -> point -> point -> int
Renvoie (p1p2 ∧ p1m).uz si > 0 m se trouve à gauche du segment, < 0 à droite, = 0 sur le segment **)
let position_to_line (p1 : point) (p2 : point) (m : point) =
	let (a, b) = p1 and (c, d) = p2 and (x, y) = m in
	(c - a)*(y - b) - (d - b)*(x - a) 


(** polygon -> point -> bool **)
let in_triangle (triangle : polygon) (m : point) = 
	(* precedent = point precedent et verifie que position_to_line sur m pour les trois segments du triangle soit > 0*)
	let rec aux lst precedent acc =
		match lst with
		|[] -> acc
		|h :: t -> aux t h (acc && ( (position_to_line precedent h m) > 0) )
	in
	let h = List.hd triangle in
	aux ( (List.tl triangle) @ [h] ) h true 

(* Type pour définir les points *)
type vertex = {
	pos : point ;
	mutable convex : bool ;
	mutable concav : bool ;
	mutable ear : bool
}

(** int -> polygon -> bool * triangle 
Renvoie si le point k est une oreille de "son triangle" **)
let is_ear k (lst : polygon) =
	let n = List.length lst in
	let v_prop = 
		Array.of_list ( List.map ( fun x -> {pos = x; convex = false; concav = false; ear = false} ) lst ) 
	in
	for i = 0 to n-1 do 
		v_prop.(i).convex <- (position_to_line v_prop.((i-1) mod n).pos v_prop.((i+1) mod n).pos v_prop.(i).pos) < 0 ;
		v_prop.(i).concav <- not(v_prop.(i).convex) 
	done;
	if v_prop.(k).convex then
	  begin
	  	let triangle = [v_prop.((k-1) mod n).pos; v_prop.(k).pos; v_prop.((k+1) mod n).pos] in
		v_prop.(k).ear <- not( Array.exists (fun x -> (in_triangle triangle x.pos) ) v_prop )
	  end;
	( v_prop.(k).ear, (v_prop.((k-1) mod n).pos, v_prop.(k).pos, v_prop.((k+1) mod n).pos) ) 


(* A faire : prendre en compte les angles afin d'avoir une triangulation "plus belle" peut etre ? *)

(** polygon -> triangle list **)
let ear_clipping (lst : polygon) =
	let n = List.length lst in
	let rec triangulate (l : polygon) (acc : triangle list) count =
		let h = List.length l in
		if count = n - 2 then acc else
			(* int -> polygon -> (int * triangle) ; parcours la liste l' afin de trouver une oreille *)
			let rec find_ear i (l' : polygon) =
				match i with
				|x when x = h -> failwith "pas d'oreilles"
				|_ -> 
					let (a, b) = is_ear i l' in 
					if a then (i, b) else find_ear (i+1) l'
			in
		let (a, b) = find_ear 0 l in 
		(* On enlève l'oreille et on itère *)
		triangulate (List.filter (fun x -> x <> List.nth l a) l ) (b :: acc) (count +1)
	in
	triangulate lst [] 0 

(*************************
	Polygones Monotones
**************************)

(* Type pour définir les points *)
type vertex_type = 
|Start 
|End 
|Regular 
|Split 
|Merge 
|Ndefined

type axis = X | Y

(* point1, point2, indice de l'arete *)
type edge = point * point * int

type status_line = abr ref

(******************************
	Décomposition Monotone
*******************************)

(** Renvoie l'indice des points (min, max) par rapport à X ou Y **)
let find_indice_min_max (l1 : polygon) (ax : axis) =
	let f = match ax with
		|X -> fst 
		|Y -> snd 
	in
	(* (x,y) : point * point *)
	let rec aux l (x, y) (posx, posy) count =
		match l with
		|[] -> (posx, posy)
		|hd :: tl ->
			begin 
				if f x >= f hd then aux tl (hd , y) (count, posy) (count+1) else
				if f y <= f hd then aux tl (x, hd) (posx, count) (count+1) else
				aux tl (x, y) (posx, posy) (count+1) 
			end
	in
	aux l1 (List.hd l1, List.hd l1) (0, 0) 0

(* *)
let compare p1 p2 =
	match p1, p2 with
	|(_, b), (_, d) when b > d -> -1
	|(_, b), (_, d) when b < d -> 1
	|(a, b), (c, d) when b = d  && a < c -> -1
	|(a, b), (c, d) when b = d  && a > c -> 1
	|_, _ -> 0

(* ('a * 'b) list -> axis -> ('a * 'b) list (tri fusion) *)
let laxis_sorted lst axis = 
	match axis with
	|X -> List.sort (fun (x,y) (x',y') -> compare (y,x) (y',x')) lst
	|Y -> List.sort (fun (x,y) (x',y') -> compare (x,y) (x',y')) lst

let laxis_sorted_w_types lst axis = 
	match axis with
	|X -> List.sort (fun ((x,y), t1) ((x',y'), t2) -> compare (y,x) (y',x')) lst
	|Y -> List.sort (fun ((x,y), t1) ((x',y'), t2) -> compare (x,y) (x',y')) lst

(** Ne Fonctionne pas **)
let make_monotone (lst : polygon) (ax : axis) =

	let n = List.length lst in
	let tree = ref Nil in
	let diag = ref [] in

	(* fonction qui renvoie (first, second) selon X ou Y *)
	let f = match ax with
		|X -> fst 
		|Y -> snd 
	in
	let vertices = 
		Array.of_list ( List.map ( fun x -> {pos = x; convex = false; concav = false; ear = false} ) lst ) 
	in
	let edges = Array.make n ((0,0), (0,0), 0) in
		for i = 0 to n-1 do 
			edges.(i) <- (vertices.(i).pos , vertices.((i+1) mod n).pos, i)
		done;
	(* point list -> list *)
	let set_v_types l =
		for i = 0 to n-1 do 
			vertices.(i).convex <- (position_to_line vertices.((i-1) mod n).pos vertices.((i+1) mod n).pos vertices.(i).pos) < 0 ;
			vertices.(i).concav <- not(vertices.(i).convex) 
		done;
		let rec aux i acc =
			if i = n then acc else 
			let v = vertices.(i).pos and vg = vertices.((i-1) mod n).pos and vd = vertices.((i+1) mod n).pos in
			match vertices.(i).convex with
			|true when (f vg) < (f v) && (f vd) < (f v) -> aux (i+1) ((vertices.(i).pos, Start) :: acc)
			|false when (f vg) < (f v) && (f vd) < (f v) -> aux (i+1) ((vertices.(i).pos, Split) :: acc)
			|true when (f vg) > (f v) && (f vd) > (f v) -> aux (i+1) ((vertices.(i).pos, End) :: acc)
			|false when (f vg) > (f v) && (f vd) > (f v) -> aux (i+1) ((vertices.(i).pos, Merge) :: acc)
			|_ -> aux (i+1) ((vertices.(i).pos, Regular) :: acc)
		in aux 0 []
	in	
	let pqueue = (laxis_sorted_w_types (set_v_types lst) ax) in
	let helper = Array.make n ((0,0), Ndefined) in
	let rec parcours l i =
	Printf.printf "%d\n" i ;
		match l with
		|[] -> !diag
		|(v, t) :: tl ->
				try
					begin
						let (x1, y) = v in
						let x = float_of_int x1 in
						tree := set_y_position !tree y ;
						match t with
						|Start   -> 
								(
									Printf.printf("Start\n");
									(* affiche_tree !tree ; *)
									tree := insert (x_intersection edges.(i) y, edges.(i)) !tree ;
									helper.(i) <- (v, t);
									parcours tl (i+1) 	
								) 		
						|End     ->
								(
									Printf.printf("End\n");
									(* affiche_tree !tree ; *)
									if snd helper.((i-1) mod n) = Merge then(
										diag := (v, fst helper.((i-1) mod n)) :: !diag );
									tree := suppression edges.((i-1) mod n) !tree ;
									parcours tl (i+1) 	
								)
						|Split   ->
								(
									Printf.printf("Split\n");
									(* affiche_tree !tree ; *)
									let (p1, p2, j) = find_left_edge !tree x in
									diag := (v, fst helper.(j)):: !diag ;
									helper.(j) <- (v, t);
									tree := insert (x_intersection edges.(i) y, edges.(i)) !tree ;
									helper.(i) <- (v, t) ;
									parcours tl (i+1) 	
								) 
						|Merge   -> 
								(
									Printf.printf("Merge\n");
									(* affiche_tree !tree ; *)
									if snd helper.((i-1) mod n) = Merge then(
										diag := (v, fst helper.((i-1) mod n)):: !diag );
									tree := suppression edges.((i-1) mod n) !tree ;
									let (p1, p2, j) = find_left_edge !tree x in
									if snd helper.(j) = Merge then(
										diag := (v, fst helper.(j)):: !diag );
									helper.(j) <- (v, t) ;
									parcours tl (i+1) 	
								)
						|Regular -> 
								(
									Printf.printf("Regular\n");
									(* affiche_tree !tree ; *)
									let (pvmax, pvmin) = find_indice_min_max lst ax in
									if not(pvmin < pvmax && i >= pvmin && i <= pvmax) then
									(
										if snd helper.((i-1) mod n) = Merge then
										diag := (v, fst helper.((i-1) mod n)):: !diag ;
										tree := suppression edges.((i-1) mod n) !tree ;
										tree := insert (x_intersection edges.(i) y, edges.(i)) !tree ;
										helper.(i) <- (v, t);
										parcours tl (i+1) 	
									)
									else
									(
										(* affiche_tree !tree ; *)
										let (p1, p2, j) = find_left_edge !tree x in
										if snd helper.(j) = Merge then(
											diag := (v, fst helper.(j)):: !diag );
										helper.(j) <- (v, t);
										parcours tl (i+1) 	
									)
								)
						|Ndefined -> failwith "zz"
					end
				with
				|Failure "arbre vide" -> parcours tl (i+1) 
	in
	parcours pqueue 0

(********************************
	Triangularisation Monotone
*********************************)

(** polygon -> axis -> bool **)
let is_monotone (lst : polygon) (ax : axis) =
	let n = List.length lst in
	let f = match ax with
		|X -> fst 
		|Y -> snd 
	in
	(* c est la fonction de comparaison *)
	let parcours_chaine l1 i j c =
		let rec aux l pos acc precedent =
			match l with
			|_ when pos = j+1 -> acc
			|hd :: tl when pos <= i -> aux tl (pos +1) acc hd 
			|hd :: tl -> (* Printf.printf "%b %d\n" (c precedent hd ) pos ;*) aux tl (pos+1) (acc && (c precedent hd) ) hd
			|_ -> acc
		in aux l1 0 true (List.hd l1)
	in
	let (a, b) = find_indice_min_max lst ax in
	(* Printf.printf "%d %d\n" a b ; *)
	let c1 = (fun x y -> f x <= f y) in
	let c2 = (fun x y -> f x >= f y) in
	if a < b then
		(parcours_chaine lst a b c1 ) && (parcours_chaine (lst@lst) b (n+a) c2 ) 
	else 
		(parcours_chaine lst b a c2 ) && (parcours_chaine (lst@lst) a (n+b) c1 ) 

(* *)
let unstack_all s =
	let rec aux acc =
		if Stack.is_empty s then acc else aux (Stack.pop s :: acc)
	in aux []

(* *)
let test lst p1 p2 ax =	
(
	let (spmin, spmax) = 
		let (posmin, posmax) = find_indice_min_max lst ax in
 		if posmin <= posmax then (posmin, posmax) else (posmax, posmin)
    in
	let pos_vtop = pos_lst p1 lst and pos_uj = pos_lst p2 lst in
	Printf.printf "(%d, %d) (%d, %d)\n" spmin spmax pos_vtop pos_uj ;

	(* si uj et le sommet sur la pile ne sont pas sur la meme chaine *)
	let b1 = (pos_vtop >= spmin && pos_vtop <= spmax) && (pos_uj >= spmin && pos_uj <= spmax) in
	let b2 = not((pos_vtop > spmin && pos_vtop < spmax) || (pos_uj > spmin && pos_uj < spmax)) in
	Printf.printf "%b %b %b\n" b1 b2 (not(b1 || b2)) ;
)

(** polygon -> axis -> triangle list 
Ne Fonctionne pas **)
let triangulate_polygon_monotone (lst : polygon) (ax : axis) =
	if not(is_monotone lst ax) then failwith "polygon is not ax monotone" else

	let u = Array.of_list (laxis_sorted lst ax) in 
	let s = Stack.create() in
		Stack.push u.(0) s ;
		Stack.push u.(1) s ;
	let d = ref [] in (* liste de diagonales *)

	let revf = match ax with
		|Y -> fst 
		|X -> snd 
	in
	(* unit -> lst *)
	let unstack_all st =
		let rec aux acc =
			if Stack.is_empty st then acc else aux (Stack.pop st :: acc)
		in aux []
	in
	(* ajoute les diagonales (u_i , elements de lst_popped) dans d *)
	let rec insert_diagonals l i = 
		match l with
		|[] -> ()
		|hd :: tl -> d := (u.(i), hd) :: !d ; insert_diagonals tl i 
	in
	for j = 2 to (Array.length u)-2 do
	(
		Printf.printf "%d\n" j ;
		(* valeur du sommet sur la pile *)
		let vtop = Stack.pop s in 
				   Stack.push vtop s ; 
		
		(* Pour savoir si uj et le sommet sur la pile ne sont pas sur la meme chaine *)
		let (spmin, spmax) =
			let (posmin, posmax) = find_indice_min_max lst ax in
		 	if posmin <= posmax then (posmin, posmax) else (posmax, posmin)
	    in
		let pos_vtop = pos_lst vtop lst and pos_uj = pos_lst u.(j) lst in
		let b1 = (pos_vtop >= spmin && pos_vtop <= spmax) && (pos_uj >= spmin && pos_uj <= spmax) in
		let b2 = not((pos_vtop > spmin && pos_vtop < spmax) || (pos_uj > spmin && pos_uj < spmax)) in
		
		if not(b1 || b2) then
		 (
		 	Printf.printf "a\n" ;
			let lst_popped = unstack_all s in
			insert_diagonals (List.tl lst_popped) j ;
			Stack.push u.(j-1) s ;
			Stack.push u.(j) s ;
		 )
 		else	
		 (
		 	Printf.printf "b\n" ;
		 	let ul = Stack.pop s in
		 	let vpop = ref ul in
		 	while (revf u.(j)) <= (revf !vpop) do
				d := (u.(j), !vpop) :: !d ;
				vpop := Stack.pop s ;
		 	done;
		 	Stack.push ul s ;
		 	Stack.push u.(j) s 
		 )
	)
	done;
	let l_S = Stack.length s and count = ref (-1) in 
	(* on enlève le 1er et dernier élément *)
	let lst_popped = List.filter (fun x -> incr count ; !count <> 0 && !count <> l_S-1) (unstack_all s) in
		insert_diagonals lst_popped (Array.length u -1) ;
		!d

(* Polygones construit dans le sens trigo *)

let polygone = [(100, 100); (200, 150); (350, 100); (400, 150); (450, 300); (350, 400); (250, 350); (200, 400); (100, 350); (50, 250); (50, 150); (75, 100)]

let polygone_w = [(200,100); (250,200); (300,100); (350,200); (400,100); (450,200); (450,300); (400,400); (300,400); (250,350); (200,400); (100,400); (50,300); (50,200); (100,100)] 

let polygone_xi = [(250, 350); (200, 300); (250, 250); (200, 200); (250, 150); (300, 200); (350, 150); (400, 200); (350, 250); (400, 300); (350, 350)] 

let pentadecagone = [(400, 200); (495, 250); (560, 350); (590, 470); (580, 590); (535, 690); (455, 760); (350, 790); (245, 760); (165, 690); (120, 590); (110, 470); (140, 350); (205, 250); (300, 200)]

let louvre = List.map (fun (x,y) -> (x + 10, (960 - y)-100)) [(0, 565); (0, 655); (828, 631); (824, 512); (856, 514); (856, 532); (1210, 532); (1210, 137); (853, 137); (853, 149); (826, 149); (826, 30); (0, 20); (0, 80); (130, 90); (130, 70); (384, 92); (384, 218); (450, 218); (450, 86); (550, 90); (550, 152); (455, 155); (455, 220); (610, 220); (610, 97); (712, 104); (708, 150); (615, 150); (615, 217); (710, 217); (710, 230); (755, 230); (755, 110); (780, 110); (780, 150); (760, 150); (760, 230); (1130, 230); (1130, 440); (920, 440); (925, 235); (835, 235); (835, 430); (755, 430); (755, 570); (737, 573); (737, 523); (750, 523); (750, 432); (700, 432); (700, 582); (610, 580); (610, 520); (695, 520); (695, 445); (550, 445); (550, 585); (445, 585); (445, 520); (545, 520); (545, 445); (390, 445); (390, 560); (210, 560); (210, 540); (35, 540); (35, 550)]

let mono = [(690, 471); (736, 527); (664, 543); (627, 619); (675, 718); (564, 791); (506, 752); (508, 706); (558, 656); (499, 343); (677, 410)]