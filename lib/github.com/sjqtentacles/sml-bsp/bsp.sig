signature BSP =
sig
  type rect = {x: int, y: int, w: int, h: int}
  datatype axis = H | V
  datatype tree
    = Leaf of rect
    | Split of {rect: rect, axis: axis, pos: int, left: tree, right: tree}

  (* split rng minSize maxDepth rootRect *)
  val split : int -> int -> int -> rect -> tree

  (* one room per leaf, slightly inset *)
  val rooms : tree -> rect list

  (* line segments (x1,y1, x2,y2) connecting sibling rooms *)
  val corridors : tree -> (int * int * int * int) list

  (* render to 2D char grid: '#'=wall '.'=floor *)
  val toGrid : tree -> int -> int -> char array array
end
