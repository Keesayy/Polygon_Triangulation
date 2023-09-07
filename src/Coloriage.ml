open Triangulation 

(*************************
	    3-Coloriage
**************************)

type graph = bool array array 

(* polygon -> graph *)
let make_graph_from_triangulation (lst : polygon) (triangulated : triangle list) =
	let n = List.length lst in
	let vertices = Array.of_list lst in
	let graphe = Array.make_matrix n n false in
	let indice_point p =
		let rec loop i =
			if i > n then failwith "error" else
			if p = vertices.(i) then i else loop (i+1)
		in loop 0 
	in
	(* *)
	let rec make_graphe l =
		match l with
		|[] -> ()
		|(a, b, c) :: tl ->
			begin
				let i_a = indice_point a and i_b = indice_point b and i_c = indice_point c in
			    graphe.(i_a).(i_b) <- true ; graphe.(i_b).(i_a) <- true ;
		 	    graphe.(i_a).(i_c) <- true ; graphe.(i_c).(i_a) <- true ;
	 	    	graphe.(i_b).(i_c) <- true ; graphe.(i_c).(i_b) <- true ;
	 	    	make_graphe tl
			end	 	   
	in make_graphe triangulated ;
	graphe 


exception Break 

(* graph -> bool * int array *)
let three_color_graph graphe =
	let n = Array.length graphe.(0) in
	let vertices_color = Array.make n 0 in
	(* *)
	let colorable v color =
		let rec loop i =
			match i with 
			|x when x = n-1 -> true
			|_ when graphe.(v).(i) && color = vertices_color.(i) -> false
			|_ -> loop (i+1)
		in loop 0
	in
	(* *)
	let rec backtrack v =
		if v = n then true else
		try
			for color = 1 to 3 do (* On peut augmenter pour un k-Coloriage *)
				if(colorable v color) then
				begin
					vertices_color.(v) <- color ;
					if(backtrack (v+1)) then raise Break ;
					vertices_color.(v) <- 0 ;
				end
			done;
			false
		with
		|Break -> true
	in ( (backtrack 0) , vertices_color )

(* polygon -> int * int * int *)
let count_colour lst =
	let triangulated = ear_clipping lst in
	let graphe = make_graph_from_triangulation lst triangulated in
	let (colorable, colours) = three_color_graph graphe in
	if not(colorable) then failwith "pas coloriable" else 
	let cred = ref 0 and cgreen = ref 0 and cblue = ref 0 in
	for i = 0 to List.length lst -1 do
		match colours.(i) with
		|1 -> incr cred
		|2 -> incr cgreen
		|3 -> incr cblue
		|_ -> ()
	done;
	(!cred, !cgreen, !cblue)

(* int * int * int -> int *)
let min_color (red, green, blue) = Stdlib.min (Stdlib.min red green) blue



   