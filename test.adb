with raylib;
with raylib.UI;
with Ada.strings.fixed;
with Ada.Text_IO;

procedure test is
   use raylib;
   use type raylib.int;
   use type raylib.c_float;

   package FS renames Ada.Strings.Fixed;
   package TIO renames Ada.Text_IO;

   FPS : Int := 30;

   padding : constant Int := 10;
   font_size : constant Int := 18;

   line_test : Boolean := FALSE;
begin

   raylib.window.init (800, 400, "test");

   raylib.set_target_FPS (FPS);
   --raylib.window.hide_cursor;

   while not raylib.window.should_close loop

      exit when raylib.core.is_key_pressed (raylib.KEY_ENTER);

      raylib.begin_drawing;
      raylib.clear_background (raylib.RAYWHITE);

    if raylib.UI.button ((600.0, 50.0, 100.0, 48.0), "Toggle lines") then line_test := not line_test; end if;

    if line_test then
      --
      raylib.text.draw ("raylib.shapes.draw_line", padding, 10, font_size, raylib.BLACK);
      raylib.shapes.draw_line (start_posX => padding,
                               start_posY => 28,
                               end_posX   => 300,
                               end_posY   => 28,
                               c => raylib.RED);
      --
      raylib.text.draw ("raylib.shapes.draw_line_v", padding, 38, font_size, raylib.BLACK);
      raylib.shapes.draw_line_v (start_pos => (C_Float (padding), 56.0),
                                 end_pos   => (300.0, 56.0),
                                 c => raylib.RED);

      --
      raylib.text.draw ("raylib.shapes.draw_line_ex", padding, 66, font_size, raylib.BLACK);
      raylib.shapes.draw_line_ex (start_pos => (C_Float (padding), 84.0),
                                  end_pos   => (300.0, 84.0),
                                  thick => 2.5,
                                  c => raylib.RED);

      end if;

      raylib.end_drawing;
   end loop;

   raylib.window.close;

end test;
