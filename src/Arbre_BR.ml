(********************************
	Arbre binaire de recherche
*********************************)

type point = int * int

type edge = point * point * int

type abr = 
|Nil 
|ABR of abr * (float * edge) * abr

type status_line = abr ref

(* *)
let rec affiche_tree a =
	match a with
	|Nil -> Printf.printf "Nil"
	|ABR(g,_ ,d) -> Printf.printf "ABR(" ; affiche_tree g ; Printf.printf ", _ "; affiche_tree d ; Printf.printf ", )"
	

(* *)
let rec insert (x, e) a =
	match a with
	| Nil -> ABR(Nil , (x, e) , Nil)
	| ABR(_, (y, e1) , _) when e1 = e -> a
	| ABR(g, (y, e1), d) -> if x < y then ABR (insert (x, e) g, (y, e1), d)
					  else ABR (g, (y, e1), insert (x, e) d)
(* *)
let rec min a =
	match a with 
	| Nil -> failwith "pas de min"
	| ABR(Nil, x , _) -> x
	| ABR(g, _, _) -> min g


(* *)
let rec find_left_edge a x =
	match a with
	|Nil -> failwith "arbre vide"
	|ABR(g, (y, e1), d) when y <= x ->
			begin
				match d with
				|ABR(dg, (c, e2), dd) when c <= x -> 
							find_left_edge d c 
				|_ -> e1
			end
	|ABR(g, _, _) -> find_left_edge g x
		
(* *)
let x_intersection ( ((a, b) , (c, d), _) : edge) y =
	if b = d then
		if c > a then float_of_int c else float_of_int a
	else
		(float_of_int y -. float_of_int b) /. (float_of_int d -. float_of_int b) *. (float_of_int c -. float_of_int a) +. float_of_int a

(* *)
let rec set_y_position a y =
	match a with
	|Nil -> Nil
	|ABR(g, (_, e), d) -> 
		ABR(set_y_position g y, (x_intersection e y, e), set_y_position d y)


(* *)
let rec suppression e a = 
	match a with
	|Nil -> Nil
	|ABR (g, (y, e1) , d) when e = e1 ->
			begin 
				match g, d with
				| Nil, Nil -> Nil
				| t, Nil | Nil, t -> t
				| _, _ -> let s = min d in ABR (g, s, suppression (snd s) d)
	      	end
  	|ABR (g, (y, e1), d) -> (* if x<y then ABR (suppression (x, e) g, (y, e1), d)
  					   else ABR(g, (y, e1), suppression (x, e) d) *)
  					   ABR (suppression e g, (y, e1), suppression e d)