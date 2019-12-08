with raylib;
with raylib.UI;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure test is
   use raylib;
   use raylib.core;

   package FS renames Ada.Strings.Fixed;
   package TIO renames Ada.Text_IO;

   FPS : constant := 30;

   padding : constant int := 10;
   font_size : constant int := 18;

   type tests is (Lines, Perspective, GUI);
   current_test : tests := Lines;
begin

   raylib.window.init (800, 400, "test");

   raylib.set_target_FPS (FPS);
   --raylib.window.hide_cursor;

   while not window.should_close loop

      if is_key_pressed (raylib.KEY_TAB) then
         current_test := tests'Succ (current_test);
      end if;

      raylib.begin_drawing;
      raylib.clear_background (raylib.RAYWHITE);

      case current_test is
      when Lines =>
         text.draw (
            "raylib.shapes.draw_line",
            padding,
             10,
             font_size,
             raylib.BLACK);

         shapes.draw_line (
            start_posX => padding,
            start_posY => 28,
            end_posx   => 300,
            end_posy   => 28,
            c => raylib.RED);
         --
         text.draw ("raylib.shapes.draw_line_v", padding, 38, font_size, BLACK);
         raylib.shapes.draw_line_v (
            start_pos => (Float (padding), 56.0),
            end_pos   => (300.0, 56.0),
            c => raylib.RED);

         --
         text.draw ("raylib.shapes.draw_line_ex", padding, 66, font_size, BLACK);
         raylib.shapes.draw_line_ex (
            start_pos => (Float (padding), 84.0),
            end_pos   => (300.0, 84.0),
            thick => 2.5,
            c => RED);

      when Perspective =>
         text.draw ("Perspective test not yet implemented", 1, 38, font_size, BLACK);
      when GUI =>
         raylib.UI.panel ((598.0, 48.0, 84.0, 36.0));
         if raylib.UI.button ((600.0, 50.0, 80.0, 32.0), "Next test")
         then current_test := tests'Succ (current_test);
         end if;
      end case;

      raylib.end_drawing;
   end loop;

   raylib.window.close;

end test;
