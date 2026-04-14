--
--   raylib [audio] example - Sound loading and playing
--
--   Example originally created with raylib 1.1, last time updated with raylib 3.5
--
--   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
--   BSD-like license that allows static linking with closed source software
--
--   Copyright (c) 2014-2023 Ramon Santamaria (@raysan5)
--

with Raylib.Window;
with Raylib.Audio;

procedure audio_sound_loading is
    use raylib;

    screenWidth  : constant := 800;
    screenHeight : constant := 450;

    fxWav, fxOgg : raylib.Sound;

begin

    raylib.Window.Init (screenWidth,
        screenHeight,
        "raylib [audio] example - sound loading and playing");

    raylib.Audio.Init_Audio_Device;

    fxWav := raylib.Audio.Load_Sound ("audio/resources/sound.wav");
    fxOgg := raylib.Audio.Load_Sound ("audio/resources/target.ogg");

    raylib.Window.Set_Target_FPS (60);

    while not raylib.Window.Should_Close loop
        if raylib.Input.Is_Key_Pressed (KEY_SPACE) then
            raylib.Audio.Play_Sound (fxWav);
        end if;
        if raylib.Input.Is_Key_Pressed (KEY_ENTER) then
            raylib.Audio.Play_Sound (fxOgg);
        end if;

        raylib.Window.Begin_Drawing;
        raylib.Window.Clear_Background (raylib.RAYWHITE);

        raylib.Text.Draw ("Press SPACE to PLAY the WAV sound!", 200, 180, 20, LIGHTGRAY);
        raylib.Text.Draw ("Press ENTER to PLAY the OGG sound!", 200, 220, 20, LIGHTGRAY);

        raylib.Window.End_Drawing;
    end loop;

    raylib.Audio.Unload_Sound (fxWav);
    raylib.Audio.Unload_Sound (fxOgg);
    raylib.Audio.Close_Audio_Device;
    raylib.Window.Close;

end audio_sound_loading;
