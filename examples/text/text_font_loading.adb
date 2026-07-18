with Raylib.Window;

procedure text_font_loading is
    use raylib;
    use type raylib.int;

    screen_width  : Integer := 800;
    screen_height : Integer := 450;
    window_title  : constant String := "raylib [text] example - font loading";

    msg             : String :=
       "!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ";
    fontBm, fontTTF : raylib.Font;

    use_ttf : boolean := False;
begin
    --
    --  Initialization
    -------------------------------------
    raylib.Window.Init (screen_width, screen_height, window_title);

    -- NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

    -- BMFont (AngelCode) : Font data and image atlas have been generated using external program
    fontBm := raylib.Text.Load_Font ("text/resources/pixantiqua.fnt");

    -- TTF font : Font data and atlas are generated directly from TTF
    -- NOTE: We define a font base size of 32 pixels tall and up-to 250 characters
    fontTTF :=
       raylib.Text.Load_Font_Ex
          ("text/resources/pixantiqua.ttf", 32, null, 250);

    Window.Set_Target_FPS (60);

    --
    --  Main game loop
    -------------------------------------
    while not raylib.Window.Should_Close loop

        use_ttf :=
           (if raylib.Input.Is_Key_Down (KEY_SPACE) then True else False);

        Window.Begin_Drawing;
        Window.Clear_Background (raylib.RAYWHITE);

        raylib.Text.Draw ("Hold SPACE to use TTF generated font", 20, 20, 20, LIGHTGRAY);

        if not use_ttf then
            raylib.Text.Draw_Ex
               (fontBm,
                msg,
                Vector2'(20.0, 100.0),
                Float (fontBm.baseSize),
                2.0,
                MAROON);
            raylib.Text.Draw ("Using BMFont (Angelcode) imported",
                20,
                Integer (Raylib.Window.Get_Screen_Height) - 30,
                20,
                GRAY);
        else
            raylib.Text.Draw_Ex
               (fontTTF,
                msg,
                Vector2'(20.0, 100.0),
                Float (fontTTF.baseSize),
                2.0,
                LIME);
            raylib.Text.Draw ("Using TTF font generated",
                20,
                Integer (raylib.Window.Get_Screen_Height) - 30,
                20,
                GRAY);
        end if;

        Window.End_Drawing;
    end loop;

    raylib.Text.Unload_Font (fontBm);
    raylib.Text.Unload_Font (fontTTF);

    raylib.Window.Close;

end text_font_loading;
