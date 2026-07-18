---
-- Audio Loading and Playing Functions

with Interfaces.C;           use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package body Raylib.Audio is

    --  [[  Audio device management  ]]  --

    function Is_Audio_Device_Ready return Boolean is
        function IsAudioDeviceReady return bool
        with Import, Convention => C, External_Name => "IsAudioDeviceReady";
    begin
        return Boolean (IsAudioDeviceReady);
    end Is_Audio_Device_Ready;

    --  [[  Wave loading  ]]  --

    function Load_Wave (Filename : String) return Wave is
        function C_Load_Wave (Filename : char_array) return Wave
        with Import, Convention => C, External_Name => "LoadWave";

        C_Filename : constant char_array := To_C (Filename);
    begin
        return C_Load_Wave (C_Filename);
    end Load_Wave;

    function Is_Wave_Valid (W : Wave) return Boolean is
        function IsWaveValid (W : Wave) return bool
        with Import, Convention => C, External_Name => "IsWaveValid";
    begin
        return Boolean (IsWaveValid (W));
    end Is_Wave_Valid;

    function Export_Wave (W : Wave; Filename : String) return Boolean is
        function C_Export_Wave (W : Wave; Filename : char_array) return bool
        with Import, Convention => C, External_Name => "ExportWave";

        C_Filename : constant char_array := To_C (Filename);
    begin
        return Boolean (C_Export_Wave (W, C_Filename));
    end Export_Wave;

    function Export_Wave_As_Code (W : Wave; Filename : String) return Boolean is
        function C_Export_Wave_As_Code
           (W : Wave; Filename : char_array) return bool
        with Import, Convention => C, External_Name => "ExportWaveAsCode";

        C_Filename : constant char_array := To_C (Filename);
    begin
        return Boolean (C_Export_Wave_As_Code (W, C_Filename));
    end Export_Wave_As_Code;

    --  [[  Sound loading  ]]  --

    function Load_Sound (Filename : String) return Sound is
        function C_Load_Sound (Filename : char_array) return Sound
        with Import, Convention => C, External_Name => "LoadSound";

        C_Filename : constant char_array := To_C (Filename);
    begin
        return C_Load_Sound (C_Filename);
    end Load_Sound;

    function Is_Sound_Valid (S : Sound) return Boolean is
        function IsSoundValid (S : Sound) return bool
        with Import, Convention => C, External_Name => "IsSoundValid";
    begin
        return Boolean (IsSoundValid (S));
    end Is_Sound_Valid;

    --  [[  Sound management  ]]  --

    function Is_Sound_Playing (S : Sound) return Boolean is
        function IsSoundPlaying (S : Sound) return bool
        with Import, Convention => C, External_Name => "IsSoundPlaying";
    begin
        return Boolean (IsSoundPlaying (S));
    end Is_Sound_Playing;

    --  [[  Music management  ]]  --

    function Load_Music_Stream (Filename : String) return Music is
        function C_Load_Music_Stream (Filename : char_array) return Music
        with Import, Convention => C, External_Name => "LoadMusicStream";

        C_Filename : char_array := To_C (Filename);
    begin
        return C_Load_Music_Stream (C_Filename);
    end Load_Music_Stream;

    function Is_Music_Valid (M : Music) return Boolean is
        function IsMusicValid (M : Music) return bool
        with Import, Convention => C, External_Name => "IsMusicValid";
    begin
        return Boolean (IsMusicValid (M));
    end Is_Music_Valid;

    function Is_Music_Stream_Playing (M : Music) return Boolean is
        function IsMusicStreamPlaying (M : Music) return bool
        with Import, Convention => C, External_Name => "IsMusicStreamPlaying";
    begin
        return Boolean (IsMusicStreamPlaying (M));
    end Is_Music_Stream_Playing;

end Raylib.Audio;
