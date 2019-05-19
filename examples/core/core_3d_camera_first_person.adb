with raylib;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with interfaces.C;

procedure core_3d_camera_first_person is
   screen_width  : Integer := 800;
   screen_height : Integer := 450;

   view : aliased raylib.Camera3D;

   MAX_COLUMNS : constant Integer := 20;
   heights : array (1..MAX_COLUMNS) of raylib.c_float;
   positions : array (1..MAX_COLUMNS) of raylib.Vector3;
   colors : array (1..MAX_COLUMNS) of raylib.Color;

   use raylib;
   use type raylib.c_float;
   use type raylib.int;
begin
   raylib.window.init (screen_width, screen_height, "raylib [core] example - 3d camera first person");

   view.position := (4.0, 2.0, 4.0);
   view.target   := (0.0, 1.8, 0.0);
   view.up := (0.0, 1.0, 0.0);
   view.fovy := 60.0;
   view.ctype := CAMERA_PERSPECTIVE;

   for i in 1..MAX_COLUMNS loop
      heights (i) := c_float(raylib.get_random_value (1, 12));
      positions (i) := (
            c_float(raylib.get_random_value (-15, 15)),
            c_float (heights(i)/ 2.0),
            c_float(raylib.get_random_value (-15, 15)));
      colors (i) := (
         r => unsigned_char(raylib.get_random_value(20, 255)),
         g => unsigned_char (raylib.get_random_value(10, 55)),
         b => 30, a => 255 );
   end loop;

   raylib.camera.set_mode (view, CAMERA_FIRST_PERSON);

   raylib.set_target_FPS(60);

   while not raylib.window.should_close loop
      raylib.camera.update (view'unchecked_access);

      raylib.begin_drawing;
      raylib.clear_background (RAYWHITE);

      raylib.drawing.begin_mode3D (view);
         raylib.shapes3D.draw_plane( (0.0, 0.0, 0.0), ( 32.0, 32.0), LIGHTGRAY); -- Draw ground
         raylib.shapes3D.draw_cube_v((-16.0, 2.5, 0.0), (1.0, 5.0, 32.0), BLUE); -- Draw a blue wall
         raylib.shapes3D.draw_cube_v((16.0, 2.5, 0.0), (1.0, 5.0, 32.0), LIME);  -- Draw a green wall
         raylib.shapes3D.draw_cube_v((0.0, 2.5, 16.0), (32.0, 5.0, 1.0), GOLD);  -- Draw a yellow wall

         for col in 1..MAX_COLUMNS loop
            raylib.shapes3D.draw_cube_v (positions (col), (2.0, heights (col), 2.0), colors (col));
            raylib.shapes3D.draw_cube_wires (positions (col), 2.0, heights (col), 2.0, MAROON);
         end loop;
      raylib.drawing.end_mode3D;

      raylib.text.draw_FPS (250, 20);
      raylib.shapes.draw_rectangle (10, 10, 220, 70, raylib.fade (SKYBLUE,0.5));
      raylib.shapes.draw_rectangle_lines (10, 10, 220, 70, BLUE);
      raylib.text.draw ("First person camera default controls:", 20, 20, 10, BLACK);
      raylib.text.draw ("- Move with keys: W, A, S, D", 40, 40, 10, DARKGRAY);
      raylib.text.draw ("- Mouse move to look around", 40, 60, 10, DARKGRAY);
      raylib.end_drawing;
   end loop;

   raylib.window.close;

end core_3d_camera_first_person;
