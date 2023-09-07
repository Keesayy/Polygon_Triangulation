open Graphics
open Printf 
open Triangulation 
open Coloriage 

let default_color = Graphics.black 
let grey_color = (rgb 211 211 211) 
let light_black_color = (rgb 69 69 69) 
let center = (320, 240) 

(* point -> int -> unit *)
let draw_point ((a, b) : point) color size =
	Graphics.set_color color ;
	Graphics.fill_circle a b size 

let draw_point_coordinates_order ((a, b) : point) color size i =
	draw_point (a,b) color size ;
	Graphics.moveto (a - 15) (b + 15) ;
	Graphics.draw_string (Printf.sprintf "%d,(%d, %d)" i a b ) 

let draw_line (a, b) (c, d) color =
	Graphics.set_color color ;
	Graphics.moveto a b ; Graphics.lineto c d

(** polygon -> int -> unit 
dessine un polygone à partir d'une liste de points **)
let draw_polygon (lst : polygon) color =
	Graphics.set_color color ;
	let rec aux lst (a, b) =
		match lst with
		|[] -> ()
		|(x, y) :: tl -> Graphics.moveto a b ; Graphics.lineto x y ; aux tl (x, y)  
	in
	let h = List.hd lst in 
	aux (lst @ [h]) h 


(** polygon -> int -> unit **)
let draw_triangle (tri : triangle) color =
	let (a, b, c) = tri in
	draw_polygon [a; b; c] color 


(* point list -> unit (même idée que draw_polygon)
Dessine les lignes pour la fonction display_draw() lorsqu'on place les sommets dans la fenêtre graphique *)
let show_drawing_polygon lst color =
	let rec aux lst (a, b) =
		match lst with
		|[] -> ()
		|(x, y) :: tl -> 
			begin
				draw_point (a, b) light_black_color 3 ;
				Graphics.set_color color ;
				Graphics.moveto a b ; 
				Graphics.lineto x y ; aux tl (x, y)
			end  
	in 
	match lst with
	|[] -> ()
	|[x] -> draw_point x light_black_color 3
	|_ -> aux lst (List.hd lst)


(* polygon -> (bool * int array) (= resultat de 3color_graphe ) -> unit 
Dessine les sommets du polygone 3-colorés après triangulation *)
let draw_coloring (lst : polygon) (colorable, colours) =
	let vertices = Array.of_list lst in

	if not(colorable) then failwith "Pas 3 coloriable ?" else
		for i = 0 to List.length lst -1 do
			match colours.(i) with
			|1 -> draw_point vertices.(i) Graphics.red 6
			|2 -> draw_point vertices.(i) Graphics.green 6
			|3 -> draw_point vertices.(i) Graphics.blue 6
			|_ -> ()
		done 

let show_explanations() = ""

(** unit -> unit  
Permet le dessin d'un polygone à trianguler dans la fenêtre puis le trianguler etc ... **)
let display_draw() = 
begin
	let vertices = ref [] in
	let triangulated = ref [] in (* On garde en mémoire la triangulation *)
	let coloriage = ref (false, [||]) in (* Idem on garde en mémoire le coloriage *)

	let draw_fun = ref show_drawing_polygon in
	let tmp_fun = ref show_drawing_polygon in (* Pour affichage des coordonnées des points 'p' *)
	let f() = !draw_fun !vertices default_color in (* Superpose plusieurs affichages *)
	
	let drawn_once = ref 0 in (* Pour 'd' ne pas rev la liste si elle l'a déja été *)
	let c_pressed = ref false in (* pour ne pas superposer un point coloré et un point noir *)
	let key_pressed = ref false in (* Pour ne plus pouvoir dessiner après construction du polygone *)
	let number_pressed = ref false in 
	let monotone = ref (false, false) in
	let cred = ref 0 and cgreen = ref 0 and cblue = ref 0 in 

	let rec display f =
	try
		let e = Graphics.wait_next_event [Graphics.Mouse_motion; Graphics.Button_down; Graphics. Key_pressed] in
		let mouse_description = Printf.sprintf "Mouse position : (%d, %d)" e.mouse_x e.mouse_y in

			clear_graph() ; f() ;

			(* affiche la position du curseur *)
			Graphics.moveto 0 0 ; 
			Graphics.set_color default_color ;
			Graphics.set_font "-*-fixed-medium-r-semicondensed--18-*-*-*-*-*-iso8859-1" ;
			Graphics.draw_string mouse_description ;  

			let is_mx = Printf.sprintf "x-monotone : %b" (fst !monotone) in
			let is_my = Printf.sprintf "y-monotone : %b" (snd !monotone) in
			Graphics.set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1" ;
			Graphics.moveto 1100 900 ;
			Graphics.draw_string is_mx ;
			Graphics.moveto 1100 882 ;
			Graphics.draw_string is_my ;
			Graphics.moveto 1000 864 ;
			Graphics.draw_string (Printf.sprintf "R : %d, G : %d, B : %d" !cred !cgreen !cblue) ;
			(* show_explanations() ; *)

			let pos = (e.mouse_x, e.mouse_y) in

			if e.button && not(!key_pressed) then 
			begin
				draw_point pos Graphics.black 4 ; 
				vertices := pos :: !vertices ;
			end;

		match e.key with
		|'1' -> vertices := louvre ; number_pressed := true ; display f 
		|'2' -> vertices := pentadecagone ; number_pressed := true ; display f
		|'3' -> vertices := List.map (fun (x,y) -> (x*2,y*2)) polygone_xi; number_pressed := true ; display f
		 (* Presets *)
		|'d' -> 
		 (* done/draw, fin du placement des points pour le dessins du polygone *)
		 	begin
		 		if !drawn_once = 0 && not(!number_pressed) then vertices := List.rev !vertices ; 
		 		draw_fun := draw_polygon ; 
		 		tmp_fun := draw_polygon ;
		 		c_pressed := false ;
		 		key_pressed := true ;
		 		incr drawn_once ;
		 		monotone := (is_monotone !vertices X, is_monotone !vertices Y) ;
		 		display f
		 	end
		|'t' ->
		 (* triangulate *)
			begin
				triangulated := ear_clipping !vertices ;
				(* v c ne servent à rien juste une question de type, considérer triangulate(void) *)
				let rec triangulate v c = 
					List.iter (fun x -> draw_triangle x grey_color) !triangulated ;
					draw_polygon !vertices Graphics.black ;
				in
				draw_fun := triangulate ;
				tmp_fun := triangulate ; 
				c_pressed := false ;
		 		key_pressed := true ;
				display f 
			end
		|'c' -> 
		 (* 3-color *)	
			begin
				let graphe = make_graph_from_triangulation !vertices !triangulated in
				coloriage := three_color_graph graphe ;
				(* v c ne servent à rien juste une question de type, considérer triangulate_and_color(void) *)
				let rec triangulate_and_color v c = 
					List.iter (fun x -> draw_triangle x grey_color) !triangulated ;
					draw_coloring !vertices !coloriage ;
					draw_polygon !vertices Graphics.black ;
				in
				let (red, green, blue) = count_colour !vertices in
				cred := red ; cgreen := green ; cblue := blue ;

				draw_fun := triangulate_and_color ; 
				tmp_fun := triangulate_and_color ; 
				c_pressed := true ;
				key_pressed := true ;
				display f 
			end
		|'r' ->
		(* refresh *)
			begin
				List.iter (fun (a, b) -> Printf.printf "(%d, %d); " a b) !vertices ; print_string "\n\n" ;
				vertices := [] ;
				triangulated := [] ;
				draw_fun := show_drawing_polygon ;
				tmp_fun := show_drawing_polygon ; 
				c_pressed := false ;
				key_pressed := false ;
				number_pressed := false ;
				monotone := (false, false) ;
				cred := 0 ; cgreen := 0 ; cred := 0 ;
				display f
			end
		|'p' ->
		(* points coordinates + order *)
			begin
				let rec show_coordinates_order v c =
					!tmp_fun !vertices default_color ;	
					let count = ref 0 in
					Graphics.set_font "-*-fixed-medium-r-semicondensed--11-*-*-*-*-*-iso8859-1" ;
					
					if !c_pressed then 
						List.iter (fun x -> draw_point_coordinates_order x light_black_color 0 !count ; incr count) !vertices 
					else
						List.iter (fun x -> draw_point_coordinates_order x light_black_color 4 !count ; incr count) !vertices 

				in	
				draw_fun := show_coordinates_order ;
				key_pressed := true ;
				display f ;
				c_pressed := false 
			end
		|'q' -> Graphics.close_graph()
		(* quit *)
		|_ -> display f
	with
	|Failure _ -> Printf.printf "------------------ERROR---------------------\n" ;
	in 
display f ;
List.iter (fun (a, b) -> Printf.printf "(%d, %d); " a b) !vertices ; print_string "\n" 
end 


(*** MAIN FUNCTION ***)
let main() = begin

	Graphics.open_graph " 1280x960" ;
	Graphics.set_window_title " Polygon triangulation " ;	
	Graphics.set_line_width 2 ;
	Graphics.set_color default_color ;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--18-*-*-*-*-*-iso8859-1" ;	

	display_draw()
end  
let _ = main()

(* ocamlfind ocamlc -package graphics -linkpkg Triangulation.ml Coloriage.ml Affichage.ml *)