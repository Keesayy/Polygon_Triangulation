from polygenerator import (
    random_polygon,
    random_star_shaped_polygon,
    random_convex_polygon,
)
#source : https://pypi.org/project/polygenerator/

#Renvoie un polygone simple aléatoire avec des coordonnées entières et mise à l'échelle
def rpoly (v, scale) :
    polygon = random_polygon(num_points=v)
    for i in range(v):
        polygon[i] = int(polygon[i][0] * scale), int(polygon[i][1] * scale)
    return polygon
       
def print_ocaml_lst (v, scale) :
    polygon = rpoly(v, scale)
    print("[", end = " ")
    for i in range(v):
        print("(", polygon[i][0], ",", polygon[i][1], ")", end = " ")
        if i != v-1 :
            print(";", end = " ")
    print("]", end = " ")

def val_graph (scale, size):
    print("let g = [", end = " ")
    for i in range(size):
        print_ocaml_lst(i+3, scale)
        if i != size-1 :
            print(";", end = " ")
    print("]", end = " ")

test = val_graph(1000, 200)
# python3 random_poly.py > val.ml