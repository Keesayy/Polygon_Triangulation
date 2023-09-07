# Polygon_Triangulation

This is my first project and it was made for a competitve exam, to enter in "Grandes Ecoles d'Ingénieur" in France after 2 years of "prépa" which can be translated to an intensive foundation degree.

- [Presentation](https://github.com/Keesayy/Polygon_Triangulation/blob/main/TIPE/TIPE_Arthur_P%20.pdf
)
- [MCOT](https://github.com/Keesayy/Polygon_Triangulation/blob/main/TIPE/Mcot_28839_AP_Etape2.pdf)

## Art Gallery Problem

What is the minimum number of guards who together can observe the whole gallery ? \
The surface of the gallery is represented by a simple polygon. Art Gallery Theorem : "To guard a simple polygon with 
n vertices, n/3 guards are always sufficient and sometimes necessary".

### Usage
There is a Graphic Interface where you can draw a simple polygon. It has to be drawn by placing the vertices ordered counterclockwise.
Then you can triangulate it, do the application of the Art Gallery Theorem and know if the polygon is monotone or not.

#### Commands
Place the vertices with your mouse and left click.
Press :
- **d (done drawing)** -> when you have placed the final vertice.
- **1, 2, 3** -> Polygon presets.
- **t (triangulate)** -> to show the triangulation of the polygon.
- **c (3-coloring)** -> You put your cameras on the minimum color (the polygon has to be triangulated).
- **r (refresh)** -> refresh the graphic interface.
- **p (points & coordinates & order)** -> show the vertices with coordinates and the order you drew them.
- **q (quit)** -> quitting the graphic interface

#### Compile
`ocamlfind ocamlc -package graphics -linkpkg Arbre_BR.ml Triangulation.ml Coloriage.ml Affichage.ml`

## Example

### 1st Floor of Le Musée du Louvre

![image](https://github.com/Keesayy/Polygon_Triangulation/blob/main/TIPE/images/plan_louvre.jpg)

### Application

#### Drawing

![output]()

#### Triangulation + Coloring

![output]()

