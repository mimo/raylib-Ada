with raylib;
with Interfaces.C;

procedure text_font_loading is
	use raylib;
	package IC renames Interfaces.C;
	use type raylib.int;

    screen_width  : Integer := 800;
    screen_height : Integer := 450;
    window_title : constant String := "raylib [text] example - font loading";

    msg : String := "!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ";
    fontBm, fontTTF : raylib.Font;

    use_ttf : boolean := False;
begin
	--
    --  Initialization
    -------------------------------------
    raylib.window.init (screen_width, screen_height, window_title);

	-- NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

	-- BMFont (AngelCode) : Font data and image atlas have been generated using external program
	fontBm := raylib.text.load_font ("text/resources/pixantiqua.fnt");

	-- TTF font : Font data and atlas are generated directly from TTF
    -- NOTE: We define a font base size of 32 pixels tall and up-to 250 characters
	fontTTF := raylib.text.load_font_ex ("text/resources/pixantiqua.ttf", 32, null, 250);

    window.set_target_FPS(60);

    --
    --  Main game loop
    -------------------------------------
    while not raylib.window.should_close loop

       use_ttf := (if raylib.input.is_key_down (KEY_SPACE) then True else False);

       window.begin_drawing;
       window.clear_background (raylib.RAYWHITE);

       raylib.text.draw ("Hold SPACE to use TTF generated font", 20, 20, 20, LIGHTGRAY);

       if not use_ttf then
          raylib.text.draw_ex (fontBm, msg, Vector2'(20.0, 100.0), Float(fontBm.baseSize), 2.0, MAROON);
          raylib.text.draw ("Using BMFont (Angelcode) imported", 20, raylib.window.get_screen_height - 30, 20, GRAY);
       else
          raylib.text.draw_ex (fontTTF, msg, Vector2'(20.0, 100.0), Float(fontTTF.baseSize), 2.0, LIME);
          raylib.text.draw ("Using TTF font generated", 20, raylib.window.get_screen_height - 30, 20, GRAY);
       end if;

       window.end_drawing;
    end loop;

    raylib.text.unload_font (fontBm);
    raylib.text.unload_font (fontTTF);

    raylib.window.close;

end text_font_loading;
