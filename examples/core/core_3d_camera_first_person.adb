with raylib;

procedure core_3d_camera_first_person is
   screen_width  : Integer := 800;
   screen_height : Integer := 450;

   view : aliased raylib.Camera3D;

   MAX_COLUMNS : constant Integer := 20;
   heights : array (1..MAX_COLUMNS) of Float;
   positions : array (1..MAX_COLUMNS) of raylib.Vector3;
   random_colors : array (1..MAX_COLUMNS) of raylib.Color;

   use raylib;
   use type raylib.int;
begin
   raylib.window.init (screen_width, screen_height, "raylib [core] example - 3d camera first person");

   view.position := (4.0, 2.0, 4.0);
   view.target   := (0.0, 1.8, 0.0);
   view.up := (0.0, 1.0, 0.0);
   view.fovy := 60.0;
   view.ctype := CAMERA_PERSPECTIVE;

   for i in 1..MAX_COLUMNS loop
      heights (i) := Float (raylib.get_random_value (1, 12));
      positions (i) := (
            Float (raylib.get_random_value (-15, 15)),
            Float (heights (i) / 2.0),
            Float (raylib.get_random_value (-15, 15)));
      random_colors (i) := (
         r => unsigned_char (raylib.get_random_value (20, 255)),
         g => unsigned_char (raylib.get_random_value (10, 55)),
         b => 30,
         a => 255);
   end loop;

   raylib.set_target_FPS (60);

   while not raylib.window.should_close loop
      rcamera.update (view'unchecked_access, CAMERA_FIRST_PERSON);

      raylib.begin_drawing;
      raylib.clear_background (RAYWHITE);

      drawing.begin_mode_3D (view);
         --  Draw ground
         shapes3D.draw_plane(
            center => (0.0, 0.0, 0.0),
            size   => (32.0, 32.0),
            tint   => LIGHTGRAY);
         --  Draw a blue wall
         shapes3D.draw_cube_v(
            position => (-16.0, 2.5, 0.0),
            size     => (1.0, 5.0, 32.0),
            tint     => BLUE);
         --  Draw a green wall
         shapes3D.draw_cube_v(
            position => (16.0, 2.5, 0.0),
            size     => (1.0, 5.0, 32.0),
            tint     => LIME);
         --  Draw a yellow wall
         shapes3D.draw_cube_v(
            position => (0.0, 2.5, 16.0),
            size     => (32.0, 5.0, 1.0),
            tint     => GOLD);

         for col in 1..MAX_COLUMNS loop
            raylib.shapes3D.draw_cube_v (
               positions (col),
               size => (2.0, heights (col), 2.0),
               tint => random_colors (col));

            raylib.shapes3D.draw_cube_wires (
               positions (col),
               width  => 2.0,
               height => Float (heights (col)),
               length => 2.0,
               tint   => MAROON);
         end loop;
      drawing.end_mode_3D;

      text.draw_FPS (250, 20);
      shapes.draw_rectangle (10, 10, 220, 70, colors.fade (SKYBLUE, 0.5));
      raylib.shapes.draw_rectangle_lines (10, 10, 220, 70, BLUE);
      text.draw ("First person camera default controls:", 20, 20, 10, BLACK);
      text.draw ("- Move with keys: W, A, S, D", 40, 40, 10, DARKGRAY);
      text.draw ("- Mouse move to look around", 40, 60, 10, DARKGRAY);
      raylib.end_drawing;
   end loop;

   raylib.window.close;

end core_3d_camera_first_person;
