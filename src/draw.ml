let box_x = 75
let box_y = 25

let draw_line c1 c2 =
  let (x1,y1) = c1 in
  let (x2, y2) = c2 in
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

let draw_rectangle c1 c2 =
  let (x1,y1) = c1 in
  let (x2, y2) = c2 in
  Graphics.moveto x1 y1;
  Graphics.lineto x1 y2;
  Graphics.lineto x2 y2;
  Graphics.lineto x2 y1;
  Graphics.lineto x1 y1

let draw_string_at str coords =
  let (x, y) = coords in
  Graphics.moveto x y;
  Graphics.draw_string str

let leaf_to_string = function
  | Ast.Nbr(n) ->
    begin
      match n with
      | RealInteger(x) -> string_of_int x
      | RealFloat(f) -> string_of_float f
      | ComplexNbr(xi) -> xi#describe
      | Matrix(m) -> raise (Types.Execution_error "Not yet handled")
    end
  | Ast.Func(f) -> f
  | Ast.Variable(v) -> v

let draw_square x y size =
  draw_rectangle (x - size / 2, y - size / 2) (x + size / 2, y + size / 2)

let draw_leaf leaf x y =
  draw_rectangle (x, y) (x + box_x, y - box_y) ;
  draw_string_at (leaf_to_string leaf) (x + box_x / 10, y - box_y / 2 - 5)

let draw_operator op x y =
  draw_rectangle (x, y) (x + box_x, y - box_y) ;
  draw_string_at (Utils.operator_to_string op) (x + box_x / 10, y - box_y / 2 - 5)


let draw_tree tree =
  Graphics.open_graph " 800x600";
  let rec draw_tree_at tree x y n =
    match tree with
    | Ast.Leaf(leaf) -> draw_leaf leaf x y
    | Ast.Node(op, left, right) -> draw_operator op x y ;
      draw_line (x + box_x, y - box_y / 2) (x + box_x * 2, y + 4 * box_y - box_y / 2 - n * box_y) ;
      draw_tree_at left (x + box_x * 2) (y + 4 * box_y - n * box_y) (n + 1);

      draw_line (x + box_x, y - box_y / 2) (x + box_x * 2, y - box_y / 2 - 4 * box_y + n  * box_y ) ;
      draw_tree_at right (x + box_x * 2) (y - 4 * box_y + n * box_y) (n + 1)
  in
  draw_tree_at tree 100 400 0 ;
  print_char (Graphics.read_key ()) ;
  Graphics.close_graph ()


let test_draw () =
  let test_tree = Ast.Node( (* 1 + 2 * x^2 *)
      (Operator.Addition,
       Ast.Leaf(Ast.Nbr(Nbr.RealInteger 1)),
       Ast.Node(
         Operator.Multiplication,
         Ast.Leaf(Ast.Nbr(Nbr.RealInteger(2))),
         Ast.Node(
           Operator.Power,
           Ast.Leaf(Ast.Variable("x")),
           Ast.Leaf(Ast.Nbr(Nbr.RealInteger(2)))
         )
       )))
  in
  draw_tree test_tree
