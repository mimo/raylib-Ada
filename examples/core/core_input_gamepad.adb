with raylib;

procedure core_input_gamepad is
   use raylib;
   use raylib.core;
   use type raylib.int;

   screenWidth  : constant := 800;
   screenHeight : constant := 450;

   texPs3Pad, texXboxPad :Texture2D;

begin

   --SetConfigFlags(FLAG_MSAA_4X_HINT);  // Set MSAA 4X hint before windows creation

   window.init (screenWidth, screenHeight, "raylib [core] example - gamepad input");

   texPs3Pad  := textures.load ("resources/ps3.png");
   texXboxPad := textures.load ("resources/xbox.png");

   raylib.set_target_FPS (30);

   while not raylib.window.should_close loop

      begin_drawing;
      clear_background (raylib.RAYWHITE);

      textures.draw_texture (texXboxPad, 0, 0, DARKGRAY);

      -- Draw axis: left joystick
      shapes.draw_circle (259, 152, 39.0, BLACK);
      shapes.draw_circle (259, 152, 34.0, LIGHTGRAY);
      shapes.draw_circle (
         259
         + int (get_gamepad_axis_movement(
               GAMEPAD_PLAYER1,
               GAMEPAD_AXIS_LEFT_X) * 20.0),
         152 + int (get_gamepad_axis_movement (GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_Y) * 20.0), 25.0, BLACK);

         --  Draw axis: right joystick
         raylib.shapes.draw_circle (461, 237, 38.0, BLACK);
         raylib.shapes.draw_circle (461, 237, 33.0, LIGHTGRAY);
         raylib.shapes.draw_circle (
            461 + int(raylib.core.get_gamepad_axis_movement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_X)*20.0),
            237 + int(raylib.core.get_gamepad_axis_movement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_Y)*20.0), 25.0, BLACK);

         -- Draw axis: left-right triggers
         shapes.draw_rectangle (170, 30, 15, 70, GRAY);
         shapes.draw_rectangle (604, 30, 15, 70, GRAY);
         shapes.draw_rectangle (170, 30, 15, int(((1.0 +
            get_gamepad_axis_movement(GAMEPAD_PLAYER1,
               GAMEPAD_AXIS_LEFT_TRIGGER))/2.0)*70.0), RED);
         shapes.draw_rectangle (604, 30, 15, int(((1.0 +
         get_gamepad_axis_movement(GAMEPAD_PLAYER1,
         GAMEPAD_AXIS_RIGHT_TRIGGER))/2.0)*70.0), RED);


      end_drawing;

   end loop;

   textures.unload (texXboxPad);
   textures.unload (texPs3Pad);

   raylib.window.close;

end core_input_gamepad;
