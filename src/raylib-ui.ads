--  https://github.com/raysan5/raygui/blob/master/src/raygui.h

package raylib.UI is

   ---
   --  Widgets and their style properties
   --
   type Controls is (
      DEFAULT,
      LABEL,
      BUTTON,
      TOGGLE,
      SLIDER,
      PROGRESSBAR,
      CHECKBOX,
      COMBOBOX,
      DROPDOWNBOX,
      TEXTBOX,
      VALUEBOX,
      SPINNER,
      LISTVIEW,
      COLORPICKER,
      SCROLLBAR,
      STATUSBAR);

   type Properties is (
      BORDER_COLOR_NORMAL,
      BASE_COLOR_NORMAL,
      TEXT_COLOR_NORMAL,
      BORDER_COLOR_FOCUSED,
      BASE_COLOR_FOCUSED,
      TEXT_COLOR_FOCUSED,
      BORDER_COLOR_PRESSED,
      BASE_COLOR_PRESSED,
      TEXT_COLOR_PRESSED,
      BORDER_COLOR_DISABLED,
      BASE_COLOR_DISABLED,
      TEXT_COLOR_DISABLED,
      BORDER_WIDTH,
      TEXT_PADDING,
      TEXT_ALIGNMENT,
      RESERVED,
      --  Default Properties
      TEXT_SIZE,
      TEXT_SPACING,
      LINE_COLOR,
      BACKGROUND_COLOR,
      --  Widget specific properties
      --  Toggle
      GROUP_PADDING,
      --  Slider / SliderBar
      SLIDER_WIDTH,
      SLIDER_PADDING,
      --  ProgressBar
      PROGRESS_PADDING,
      --  CheckBox
      CHECK_PADDING,
      --  ComboBox
      COMBO_BUTTON_WIDTH,
      COMBO_BUTTON_PADDING,
      --  TextBox
      TEXT_INNER_PADDING,
      TEXT_LINES_PADDING,
      COLOR_SELECTED_FG,
      COLOR_SELECTED_BG);

   function  Get_Property_Index (P : Properties) return Integer;

   type Property_Element is
     (BORDER,
      BASE,
      TEXT,
      OTHER);

   ---------------------------------------------------------------------------
   --  État global des contrôles
   ---------------------------------------------------------------------------

   type Control_State is
     (NORMAL,
      FOCUSED,
      PRESSED,
      DISABLED);

   function  Get_State return Control_State;
   procedure Set_State (State : Control_State);

   subtype Transparency is Float range 0.0 .. 1.0;

   function  Get_Alpha return Transparency;
   procedure Set_Alpha (Alpha : Transparency);

   procedure Lock;
   procedure Unlock;

   procedure Set_Style
     (Control  : Controls;
      Property : Properties;
      Value    : unsigned);

   function Get_Style
     (Control  : Controls;
      Property : Properties)
      return unsigned;

   function Get_Style
     (Control : Controls;
      Element : Property_Element;
      State   : Control_State)
      return unsigned;

   ---------------------------------------------------------------------------
   --  Contrôles UI
   ---------------------------------------------------------------------------

   procedure Label
     (Bounds : Rectangle;
      Text   : String);

   function Button
     (Bounds : Rectangle;
      Label  : String)
      return Boolean;

   procedure Checkbox
     (Bounds  : in     Rectangle;
      Text    : in     String;
      Checked : in out Boolean);

   procedure Panel (Bounds : Rectangle);

   procedure Statusbar
     (Bounds : Rectangle;
      Text   : String);

   procedure Toggle
     (Bounds : in     Rectangle;
      Text   : in     String;
      Active : in out Boolean);

   function Textbox
     (Bounds    : in     Rectangle;
      Text      : in out String;
      Edit_Mode : in out Boolean)
      return Boolean;

   function Textbox_Multi
     (Bounds    :        Rectangle;
      Text      : in out String;
      Edit_Mode : in out Boolean)
      return Vector2;
   ---------------------------------------------------------------------------
   --  Utilitaires texte
   ---------------------------------------------------------------------------

   type Text_Alignment_Type is
     (TEXT_ALIGN_LEFT,
      TEXT_ALIGN_CENTER,
      TEXT_ALIGN_RIGHT);
   for Text_Alignment_Type'Size use unsigned'Size;

   function  To_Text_Alignment (Position : unsigned) return Text_Alignment_Type;
   function  To_Unsigned (Value : Text_Alignment_Type) return unsigned;

   function  Get_Text_Width (Text : String) return int;

   function  Get_Text_Bounds
     (Control : Controls;
      Bounds  : Rectangle)
      return Rectangle;

private

   function Get_Property_By_State
     (C     : Property_Element;
      State : Control_State)
      return Properties;

   procedure Load_Style_Default;
   procedure Apply_Theme_To_Style;

   procedure Draw_Border
     (Rec          : Rectangle;
      Border_Width : int;
      Border_Color : Color);

   procedure Draw_Rectangle
     (Rec          : Rectangle;
      Border_Width : int;
      Border_Color : Color;
      Bg           : Color);

   procedure Draw_Text
     (Text      : String;
      Bounds    : Rectangle;
      Alignment : Text_Alignment_Type;
      Tint      : Color);

   ---------------------------------------------------------------------------
   --  Theme system
   ---------------------------------------------------------------------------

   type Color_Role is
     (Primary,
      Secondary,
      Text_Primary,
      Text_Secondary,
      Background,
      Border);

   type Font_Role is
     (Default,
      Title,
      Small);

   type Size_Role is
     (Default_Text_Size,
      Title_Size,
      Small_Size);

   type Theme is record
      Colors : array (Color_Role) of Color;
      Fonts  : array (Font_Role) of String (1 .. 256);
      Sizes  : array (Size_Role) of Positive;
   end record;

   --  Default theme based on raygui default style
   Default_Theme : constant Theme :=
     (Colors =>
        (Primary        => (16#c9#, 16#c9#, 16#c9#, 16#ff#),  -- BASE_COLOR_NORMAL
         Secondary      => (16#97#, 16#e8#, 16#ff#, 16#ff#),  -- BASE_COLOR_PRESSED
         Text_Primary   => (16#68#, 16#68#, 16#68#, 16#ff#),  -- TEXT_COLOR_NORMAL
         Text_Secondary => (16#36#, 16#8b#, 16#af#, 16#ff#),  -- TEXT_COLOR_PRESSED
         Background     => (16#f5#, 16#f5#, 16#f5#, 16#ff#),  -- BACKGROUND_COLOR
         Border         => (16#83#, 16#83#, 16#83#, 16#ff#)), -- BORDER_COLOR_NORMAL
      Fonts =>
        (Default => (others => ' '),
         Title  => (others => ' '),
         Small  => (others => ' ')),
      Sizes =>
        (Default_Text_Size => 14,
         Title_Size        => 20,
         Small_Size        => 10));

   procedure Load (T : Theme := Default_Theme);
   procedure Unload;

   procedure Use_Color (C : Color_Role);
   procedure Use_Font (F : Font_Role);
   procedure Stop_Override;

   function Color_Of (C : Color_Role) return Color;
   function Font_Of (F : Font_Role) return Font;
   function Size_Of (S : Size_Role) return Positive;

end raylib.UI;
