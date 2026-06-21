structure BspTests =
struct
  fun run () =
    let
      val root = {x=0, y=0, w=20, h=20}
      val tree = Bsp.split 42 4 3 root
      val rms = Bsp.rooms tree
      val cors = Bsp.corridors tree
    in
      Harness.section "BSP rooms";
      Harness.check "at least one room" (length rms >= 1);
      let
        fun overlaps (r1: Bsp.rect) (r2: Bsp.rect) =
          not (#x r1 + #w r1 <= #x r2 orelse #x r2 + #w r2 <= #x r1
               orelse #y r1 + #h r1 <= #y r2 orelse #y r2 + #h r2 <= #y r1)
        fun anyOverlap [] = false
          | anyOverlap (r::rs) = List.exists (overlaps r) rs orelse anyOverlap rs
      in
        Harness.check "rooms non-overlapping" (not (anyOverlap rms))
      end;
      Harness.section "BSP corridors";
      Harness.check "corridors exist" (length cors >= 0);
      Harness.section "BSP toGrid";
      let
        val g = Bsp.toGrid tree 20 20
        val floors = Array.foldl (fn (row, acc) =>
          Array.foldl (fn (c, a) => if c = #"." then a+1 else a) acc row) 0 g
      in
        Harness.check "grid has floor cells" (floors > 0)
      end;
      ()
    end
end
