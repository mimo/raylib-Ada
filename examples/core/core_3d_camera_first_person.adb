with raylib;

procedure core_3d_camera_first_person is
   use raylib;
   use raylib.window;
   use type raylib.int;
   use type raylib.bool;

   screen_width  : Integer := 800;
   screen_height : Integer := 450;

   view : aliased raylib.Camera3D;
   cam_mode : raylib.CameraMode := CAMERA_FIRST_PERSON;
   use_camera_pro : Boolean := False;

   MAX_COLUMNS : constant Integer := 20;
   heights       : array (1..MAX_COLUMNS) of Float;
   positions     : array (1..MAX_COLUMNS) of Vector3;
   random_colors : array (1..MAX_COLUMNS) of Color;

   type fp is delta 0.001 digits 9;

begin
   raylib.window.init (screen_width, screen_height, "raylib [core] example - 3d camera first person");

   view.position := (4.0, 2.0, 4.0);
   view.target   := (0.0, 1.8, 0.0);
   view.up := (0.0, 1.0, 0.0);
   view.fovy := 60.0;
   view.projection := CAMERA_PERSPECTIVE;

   for i in 1..MAX_COLUMNS loop
      heights (i) := Float (get_random_value (1, 12));
      positions (i) := (
            Float (get_random_value (-15, 15)),
            Float (heights (i) / 2.0),
            Float (get_random_value (-15, 15)));
      random_colors (i) := (
         r => unsigned_char (get_random_value (20, 255)),
         g => unsigned_char (get_random_value (10, 55)),
         b => 30,
         a => 255);
   end loop;

   window.disable_cursor;
   set_target_FPS (60);

   while not should_close loop

      if input.is_key_pressed (KEY_K) then
         if not use_camera_pro
         then 
            use_camera_pro := True;
            utils.trace_log(LOG_INFO, "Switch to camera pro");
         else
            use_camera_pro := False;
            utils.trace_Log(LOG_INFO, "Stop using camera pro");
         end if;
      end if;

      if input.is_key_pressed (KEY_ONE) then
         cam_mode := CAMERA_FREE;
         view.up := (0.0, 1.0, 0.0);
      end if;

      if input.is_key_pressed (KEY_TWO) then
         cam_mode := CAMERA_FIRST_PERSON;
         view.up := (0.0, 1.0, 0.0);
      end if;

      if input.is_key_pressed (KEY_THREE) then
         cam_mode := CAMERA_THIRD_PERSON;
         view.up := (0.0, 1.0, 0.0);
      end if;

      if input.is_key_pressed (KEY_FOUR) then
         cam_mode := CAMERA_ORBITAL;
         view.up := (0.0, 1.0, 0.0);
      end if;

      if input.is_key_pressed (KEY_P)
      then
         if (view.projection = CAMERA_PERSPECTIVE) then
            cam_mode := CAMERA_THIRD_PERSON;
            view.position := (0.0, 2.0, -100.0);
            view.target := (0.0, 2.0, 0.0);
            view.up := (0.0, 1.0, 0.0);
            view.projection := CAMERA_ORTHOGRAPHIC;
            view.fovy := 20.0;
            rcamera.Camera_Yaw (view'unchecked_access, (-135.0 * PI / 180.0), bool (true));
            rcamera.Camera_Pitch (view'unchecked_access, (-45.0 * PI / 180.0), bool (true), bool (true), bool (false));
         elsif (view.projection = CAMERA_ORTHOGRAPHIC) then
            cam_mode := CAMERA_THIRD_PERSON;
            view.position := (0.0, 2.0, 10.0);
            view.target := (0.0, 2.0, 0.0);
            view.up := (0.0, 1.0, 0.0);
            view.projection := CAMERA_PERSPECTIVE;
            view.fovy := 60.0;
         end if;
      end if;

      if use_camera_pro
      then
         rcamera.update_pro (
            camera => view'unchecked_access,
            movement => Vector3'(
               x => (if    input.is_key_down (KEY_W) or input.is_key_down (KEY_UP)   then  0.1
                     elsif input.is_key_down (KEY_S) or input.is_key_down (KEY_DOWN) then -0.1
                     else  0.0),
               y => (if    input.is_key_down (KEY_D) or input.is_key_down (KEY_RIGHT) then  0.1
                     elsif input.is_key_down (KEY_A) or input.is_key_down (KEY_LEFT)  then -0.1
                     else 0.0),
               z => 0.0
            ),
            rotation => Vector3'(
               x => input.get_mouse_delta.x * 0.05,
               y => input.get_mouse_delta.y * 0.05,
               z => 0.0
            ),
            zoom => input.get_mouse_wheel_move * 2.0
         );
      else
         rcamera.update (view'unchecked_access, cam_mode);
      end if;

      begin_drawing;
      clear_background (RAYWHITE);

      begin_mode3D (view);
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

         if cam_mode = CAMERA_THIRD_PERSON then
            raylib.shapes3D.draw_cube_v (
               position => view.target,
               size     => (0.5, 0.5, 0.5),
               tint     => PURPLE);
            raylib.shapes3D.draw_cube_wires (
               position => view.target,
               width    => 0.5,
               height   => 0.5,
               length   => 0.5,
               tint     => DARKPURPLE);
         end if;
      end_mode3D;

      text.draw_FPS (250, 20);
      shapes.draw_rectangle (5, 5, 330, 115, colors.fade (SKYBLUE, 0.5));
      raylib.shapes.draw_rectangle_lines (5, 5, 330, 115, BLUE);

      text.draw ("Camera controls :", 15, 15, 10, BLACK);
      text.draw ("- Move keys: W, A, S, D, Space, Left-Ctrl", 15, 30, 10, BLACK);
      text.draw ("- Look around: arrow keys or mouse", 15, 45, 10, BLACK);
      text.draw ("- Camera mode keys: 1, 2, 3, 4", 15, 60, 10, BLACK);
      text.draw ("- Zoom keys: num-plus, num-minus or mouse scroll", 15, 75, 10, BLACK);
      text.draw ("- Camera projection key: P", 15, 90, 10, BLACK);
      text.draw ("- Camera update function key: K", 15, 105, 10, BLACK);

      shapes.draw_rectangle (590, 5, 195, 100, colors.fade (SKYBLUE, 0.5));
      shapes.draw_rectangle_lines (590, 5, 195, 100, BLUE);

      text.draw ("Camera status:", 600, 15, 10, BLACK);
      text.draw ("- Mode: " & (if use_camera_pro then "CAMERA_PRO" else cam_mode'Img), 600, 30, 10, BLACK);
      text.draw ("- Projection: " & view.projection'Img, 600, 45, 10, BLACK);
      text.draw ("- Position: (" & fp(view.position.x)'Img & "," & fp(view.position.y)'Img & "," & fp(view.position.z)'Img & ")", 600, 60, 10, BLACK);
      text.draw ("- Target: (" & fp(view.target.x)'Img & "," & fp(view.target.y)'Img & "," & fp(view.target.z)'Img & ")", 600, 75, 10, BLACK);
      text.draw ("- Up: (" & fp(view.up.x)'Img & "," & fp(view.up.y)'Img & "," & fp(view.up.z)'Img & ")", 600, 90, 10, BLACK);

      end_drawing;
   end loop;

   raylib.window.close;

end core_3d_camera_first_person;
