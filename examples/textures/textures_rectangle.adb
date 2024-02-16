with raylib;

procedure textures_rectangle is
   screen_width  : Integer := 800;
   screen_height : Integer := 450;

   MAX_FRAME_SPEED : constant := 15;
   MIN_FRAME_SPEED : constant := 1;

   use raylib;
   use type raylib.int;

   scarfy : Texture2D;

   position : Vector2 := (350.0, 280.0);
   frame_rec : Rectangle;
   current_frame : Integer := 0;
   frames_counter : Integer := 0;
   frames_speed : Integer := 8; -- Number of spritesheet frames shown by second
begin
   raylib.window.init (
      screen_width,
      screen_height,
      "raylib [texture] example - texture rectangle");

   scarfy := textures.load ("textures/resources/scarfy.png");
   frame_rec := (0.0, 0.0, Float(scarfy.width / 6), Float(scarfy.height));

   window.set_target_FPS(60);

   while not raylib.window.should_close loop

      frames_counter := frames_counter + 1;

      if frames_counter >= (60 / frames_speed) then
         frames_counter := 0;
         current_frame := current_frame + 1;

         if current_frame > 5
         then current_frame := 0;
         end if;

         frame_rec.x := Float (current_frame) * Float (scarfy.width) / 6.0;
      end if;

      if input.is_key_pressed (KEY_RIGHT)
      then frames_speed := frames_speed + 1;
      elsif input.is_key_pressed (KEY_LEFT)
      then frames_speed := frames_speed - 1;
      end if;

      if frames_speed > MAX_FRAME_SPEED then frames_speed := MAX_FRAME_SPEED;
      elsif frames_speed < MIN_FRAME_SPEED then frames_speed := MIN_FRAME_SPEED;
      end if;

      window.begin_drawing;
      window.clear_background (RAYWHITE);

      textures.draw_texture (scarfy, 15, 40, WHITE);
      shapes.draw_rectangle_lines (15, 40, scarfy.width, scarfy.height, RED);
      shapes.draw_rectangle_lines (
         15 + int(frame_rec.x),
         40 + int(frame_rec.y),
         int(frame_rec.width),
         int(frame_rec.height), RED);

      text.draw ("FRAME SPEED: ", 165, 210, 10, DARKGRAY);
      text.draw (frames_speed'Img & " FPS", 575, 210, 10, DARKGRAY);
      text.draw ("PRESS RIGHT/LEFT KEYS to change SPEED", 290, 240, 10, DARKGRAY);

      for i in 0..MAX_FRAME_SPEED-1 loop
         if i < frames_speed then
            shapes.draw_rectangle (250 + 21 * int(i), 205, 20, 20, RED);
         end if;

         shapes.draw_rectangle_lines (250 + 21 * int(i), 205, 20, 20, MAROON);
      end loop;

      textures.draw_texture_rec (scarfy, frame_rec, position, WHITE);

      text.draw ("(c) Scarfy sprite by Eiden Marsal", int(screen_width) - 200, int(screen_height) - 20, 10, GRAY);

      window.end_drawing;
   end loop;

   textures.unload (scarfy);

   raylib.window.close;

end textures_rectangle;
