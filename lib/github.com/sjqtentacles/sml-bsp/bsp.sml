structure Bsp :> BSP =
struct
  type rect = {x: int, y: int, w: int, h: int}
  datatype axis = H | V
  datatype tree
    = Leaf of rect
    | Split of {rect: rect, axis: axis, pos: int, left: tree, right: tree}

  (* LCG using small multiplier to stay within 30-bit int range.
     We keep seed in [0, 46340] so seed*46341 < 2^31-1. *)
  fun lcgNext seed =
    let
      val s = Int.abs (seed mod 46341)
      val next = (s * 46341 + 12345) mod 1073741823
    in
      next
    end

  (* Generate int in [lo, hi] *)
  fun lcgRange seed lo hi =
    let
      val next = lcgNext seed
      val range = hi - lo + 1
    in
      (lo + (Int.abs next mod range), next)
    end

  fun split seed minSize maxDepth r =
    let
      val {x, y, w, h} = r
      val canSplitH = h >= 2 * minSize
      val canSplitV = w >= 2 * minSize
    in
      if maxDepth = 0 orelse (not canSplitH andalso not canSplitV)
      then Leaf r
      else
        let
          val (axisChoice, seed1) = lcgRange seed 0 1
          val axis =
            if canSplitH andalso canSplitV
            then (if axisChoice = 0 then H else V)
            else if canSplitH then H
            else V
          val (leftRect, rightRect, pos, seed2) =
            case axis of
              H =>
                let
                  val (p, s) = lcgRange seed1 (y + minSize) (y + h - minSize)
                in
                  ({x=x, y=y, w=w, h=p-y},
                   {x=x, y=p, w=w, h=y+h-p},
                   p, s)
                end
            | V =>
                let
                  val (p, s) = lcgRange seed1 (x + minSize) (x + w - minSize)
                in
                  ({x=x, y=y, w=p-x, h=h},
                   {x=p, y=y, w=x+w-p, h=h},
                   p, s)
                end
          val leftTree  = split seed2 minSize (maxDepth - 1) leftRect
          val rightTree = split (lcgNext seed2) minSize (maxDepth - 1) rightRect
        in
          Split {rect=r, axis=axis, pos=pos, left=leftTree, right=rightTree}
        end
    end

  fun rooms (Leaf r) =
        let
          val {x, y, w, h} = r
          val rx = x + 1
          val ry = y + 1
          val rw = Int.max (1, w - 2)
          val rh = Int.max (1, h - 2)
        in
          [{x=rx, y=ry, w=rw, h=rh}]
        end
    | rooms (Split {left, right, ...}) = rooms left @ rooms right

  fun centerOf ({x, y, w, h} : rect) = (x + w div 2, y + h div 2)

  fun anyRoom (Leaf r) = r
    | anyRoom (Split {left, ...}) = anyRoom left

  fun corridors (Leaf _) = []
    | corridors (Split {left, right, ...}) =
        let
          val lRoom = anyRoom left
          val rRoom = anyRoom right
          val (lx, ly) = centerOf lRoom
          val (rx, ry) = centerOf rRoom
        in
          (lx, ly, rx, ry) :: corridors left @ corridors right
        end

  fun toGrid tree gridW gridH =
    let
      val grid = Array.tabulate (gridH, fn _ => Array.array (gridW, #"#"))
      fun setCell cx cy ch =
        if cx >= 0 andalso cx < gridW andalso cy >= 0 andalso cy < gridH
        then Array.update (Array.sub (grid, cy), cx, ch)
        else ()
      fun fillRoom ({x, y, w, h} : rect) =
        let
          fun fillRow j =
            if j >= y + h then ()
            else
              let
                fun fillCol i =
                  if i >= x + w then ()
                  else (setCell i j #"."; fillCol (i + 1))
              in
                fillCol x; fillRow (j + 1)
              end
        in
          fillRow y
        end
      fun drawCorridor (x1, y1, x2, y2) =
        let
          fun drawH cx =
            if cx = x2 then setCell cx y1 #"."
            else (setCell cx y1 #"."; drawH (if cx < x2 then cx + 1 else cx - 1))
          fun drawV cy =
            if cy = y2 then setCell x2 cy #"."
            else (setCell x2 cy #"."; drawV (if cy < y2 then cy + 1 else cy - 1))
        in
          drawH x1; drawV y1
        end
      val rms  = rooms tree
      val cors = corridors tree
    in
      List.app fillRoom rms;
      List.app drawCorridor cors;
      grid
    end
end
