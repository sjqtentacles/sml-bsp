(* sml-bsp demo: generates a BSP dungeon (Bsp.split), fills the leaf rooms over
   a solid wall background, carves the corridor center-lines that connect
   sibling rooms, and writes assets/dungeon.png. *)

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

val gridW = 96
val gridH = 64
val cell = 7
val width = gridW * cell
val height = gridH * cell

val root = { x = 0, y = 0, w = gridW, h = gridH }
val tree = Bsp.split 1337 8 6 root

val wall    = rgba (20, 22, 30, 255)
val floor   = rgba (92, 138, 150, 255)
val edge    = rgba (130, 184, 196, 255)
val corrCol = rgba (224, 170, 86, 255)

fun sc v = v * cell + cell div 2

(* Carve a corridor segment with a little thickness. *)
fun carve (img, x0, y0, x1, y1) =
  let
    val a = { x0 = sc x0, y0 = sc y0, x1 = sc x1, y1 = sc y1 }
    val c = Raster.line img a corrCol
    val c = Raster.line c { x0 = #x0 a, y0 = #y0 a + 1, x1 = #x1 a, y1 = #y1 a + 1 } corrCol
  in
    Raster.line c { x0 = #x0 a + 1, y0 = #y0 a, x1 = #x1 a + 1, y1 = #y1 a } corrCol
  end

val img =
  let
    val base = Raster.blank (width, height) wall
    val c =
      List.foldl
        (fn ({ x, y, w, h }, img) =>
            let
              val px = { x = x * cell, y = y * cell, w = w * cell, h = h * cell }
              val img = Raster.fillRect img px floor
            in
              Raster.rect img px edge
            end)
        base (Bsp.rooms tree)
    val c =
      List.foldl (fn ((x0, y0, x1, y1), img) => carve (img, x0, y0, x1, y1))
                 c (Bsp.corridors tree)
  in
    c
  end

val () =
  let
    val os = BinIO.openOut "assets/dungeon.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/dungeon.png\n"
  end
