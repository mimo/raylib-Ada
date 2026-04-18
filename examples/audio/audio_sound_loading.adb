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

procedure Audio_Sound_Loading is
    use Raylib;

    Screen_Width  : constant := 800;
    Screen_Height : constant := 450;

    FX_Wav, FX_Ogg : Raylib.Sound;

begin

    Raylib.Window.Init
       (Screen_Width,
        Screen_Height,
        "raylib [audio] example - sound loading and playing");

    Raylib.Audio.Init_Audio_Device;

    FX_Wav := Raylib.Audio.Load_Sound ("audio/resources/sound.wav");
    FX_Ogg := Raylib.Audio.Load_Sound ("audio/resources/target.ogg");

    Raylib.Window.Set_Target_FPS (60);

    while not Raylib.Window.Should_Close loop
        if Raylib.Input.Is_Key_Pressed (KEY_SPACE) then
            Raylib.Audio.Play_Sound (FX_Wav);
        end if;
        if Raylib.Input.Is_Key_Pressed (KEY_ENTER) then
            Raylib.Audio.Play_Sound (FX_Ogg);
        end if;

        Raylib.Window.Begin_Drawing;
        Raylib.Window.Clear_Background (Raylib.RAYWHITE);

        Raylib.Text.Draw
           ("Press SPACE to PLAY the WAV sound!", 200, 180, 20, LIGHTGRAY);
        Raylib.Text.Draw
           ("Press ENTER to PLAY the OGG sound!", 200, 220, 20, LIGHTGRAY);

        Raylib.Window.End_Drawing;
    end loop;

    Raylib.Audio.Unload_Sound (FX_Wav);
    Raylib.Audio.Unload_Sound (FX_Ogg);
    Raylib.Audio.Close_Audio_Device;
    Raylib.Window.Close;

end audio_sound_loading;
