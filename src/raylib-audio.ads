------------------------------------------------------------------------------------
-- Audio Loading and Playing Functions (Module: audio)
------------------------------------------------------------------------------------

package Raylib.Audio is

    --  [[  Audio device management functions  ]]  --

    --// Initialize audio device and context
    procedure Init_Audio_Device
    with Import, Convention => C, External_Name => "InitAudioDevice";

    --// Close the audio device and context
    procedure Close_Audio_Device
    with Import, Convention => C, External_Name => "CloseAudioDevice";

    --// Check if audio device has been initialized successfully
    function Is_Audio_Device_Ready return Boolean;

    --// Set master volume (listener)
    procedure Set_Master_Volume (Volume : Float)
    with Import, Convention => C, External_Name => "SetMasterVolume";

    --// Get master volume (listener)
    function Get_Master_Volume return Float
    with Import, Convention => C, External_Name => "GetMasterVolume";

    --  [[  Wave loading/unloading functions  ]]  --

    --// Load wave data from file
    function Load_Wave (Filename : String) return Wave;

    --// Checks if wave data is valid (data loaded and parameters)
    function Is_Wave_Valid (W : Wave) return Boolean;

    --// Unload wave data
    procedure Unload_Wave (W : Wave)
    with Import, Convention => C, External_Name => "UnloadWave";

    --// Export wave data to file, returns true on success
    function Export_Wave (W : Wave; Filename : String) return Boolean;

    --// Export wave sample data to code (.h), returns true on success
    function Export_Wave_As_Code (W : Wave; Filename : String) return Boolean;

    --  [[  Wave management functions  ]]  --

    --// Copy a wave to a new wave
    function Wave_Copy (W : Wave) return Wave
    with Import, Convention => C, External_Name => "WaveCopy";

    --// Crop a wave to defined frames range
    procedure Wave_Crop (W : access Wave; Init_Frame, Final_Frame : int)
    with Import, Convention => C, External_Name => "WaveCrop";

    --// Convert wave data to desired format
    procedure Wave_Format
       (W : access Wave; Sample_Rate, Sample_Size, Channels : int)
    with Import, Convention => C, External_Name => "WaveFormat";

    --// Load samples data from wave as a 32bit float data array
    function Load_Wave_Samples (W : Wave) return access Float
    with Import, Convention => C, External_Name => "LoadWaveSamples";

    --// Unload samples data loaded with LoadWaveSamples()
    procedure Unload_Wave_Samples (Samples : access Float)
    with Import, Convention => C, External_Name => "UnloadWaveSamples";

    --  [[  Sound loading/unloading functions  ]]  --

    --// Load sound from file
    function Load_Sound (Filename : String) return Sound;

    --// Load sound from wave data
    function Load_Sound_From_Wave (Source : Wave) return Sound
    with Import, Convention => C, External_Name => "LoadSoundFromWave";

    --// Load sound alias (shares sample data, does not own it)
    function Load_Sound_Alias (Source : Sound) return Sound
    with Import, Convention => C, External_Name => "LoadSoundAlias";

    --// Checks if a sound is valid (data loaded and buffers initialized)
    function Is_Sound_Valid (S : Sound) return Boolean;

    --// Unload sound
    procedure Unload_Sound (S : Sound)
    with Import, Convention => C, External_Name => "UnloadSound";

    --// Unload a sound alias (does not deallocate sample data)
    procedure Unload_Sound_Alias (S : Sound)
    with Import, Convention => C, External_Name => "UnloadSoundAlias";

    --// Update sound buffer with new data
    procedure Update_Sound
       (S : Sound; Data : System.Address; Sample_Count : int)
    with Import, Convention => C, External_Name => "UpdateSound";

    --  [[  Sound management functions  ]]  --

    --// Play a sound
    procedure Play_Sound (S : Sound)
    with Import, Convention => C, External_Name => "PlaySound";

    --// Stop playing a sound
    procedure Stop_Sound (S : Sound)
    with Import, Convention => C, External_Name => "StopSound";

    --// Pause a sound
    procedure Pause_Sound (S : Sound)
    with Import, Convention => C, External_Name => "PauseSound";

    --// Resume a paused sound
    procedure Resume_Sound (S : Sound)
    with Import, Convention => C, External_Name => "ResumeSound";

    --// Check if a sound is currently playing
    function Is_Sound_Playing (S : Sound) return Boolean;

    --// Set volume for a sound (1.0 is max level)
    procedure Set_Sound_Volume (S : Sound; Volume : Float)
    with Import, Convention => C, External_Name => "SetSoundVolume";

    --// Set pitch for a sound (1.0 is base level)
    procedure Set_Sound_Pitch (S : Sound; Pitch : Float)
    with Import, Convention => C, External_Name => "SetSoundPitch";

    --// Set pan for a sound (0.5 is center)
    procedure Set_Sound_Pan (S : Sound; Pan : Float)
    with Import, Convention => C, External_Name => "SetSoundPan";

    --  [[  Music management functions  ]]  --

    --// Load music stream from file
    function Load_Music_Stream (Filename : String) return Music;

    --// Checks if a music stream is valid (context and buffers initialized)
    function Is_Music_Valid (M : Music) return Boolean;

    --// Unload music stream
    procedure Unload_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "UnloadMusicStream";

    --// Start music playing
    procedure Play_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "PlayMusicStream";

    --// Check if music is playing
    function Is_Music_Stream_Playing (M : Music) return Boolean;

    --// Updates buffers for music streaming
    procedure Update_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "UpdateMusicStream";

    --// Stop music playing
    procedure Stop_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "StopMusicStream";

    --// Pause music playing
    procedure Pause_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "PauseMusicStream";

    --// Resume playing paused music
    procedure Resume_Music_Stream (M : Music)
    with Import, Convention => C, External_Name => "ResumeMusicStream";

    --// Seek music to a position (in seconds)
    procedure Seek_Music_Stream (M : Music; Position : Float)
    with Import, Convention => C, External_Name => "SeekMusicStream";

    --// Set volume for music (1.0 is max level)
    procedure Set_Music_Volume (M : Music; Volume : Float)
    with Import, Convention => C, External_Name => "SetMusicVolume";

    --// Set pitch for a music (1.0 is base level)
    procedure Set_Music_Pitch (M : Music; Pitch : Float)
    with Import, Convention => C, External_Name => "SetMusicPitch";

    --// Set pan for a music (0.5 is center)
    procedure Set_Music_Pan (M : Music; Pan : Float)
    with Import, Convention => C, External_Name => "SetMusicPan";

    --// Get music time length (in seconds)
    function Get_Music_Time_Length (M : Music) return Float
    with Import, Convention => C, External_Name => "GetMusicTimeLength";

    --// Get current music time played (in seconds)
    function Get_Music_Time_Played (M : Music) return Float
    with Import, Convention => C, External_Name => "GetMusicTimePlayed";

end Raylib.Audio;
