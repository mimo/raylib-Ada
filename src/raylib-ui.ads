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

   function  get_property_index (P : Properties) return Integer;

   type Property_Element is (
      BORDER,
      BASE,
      TEXT,
      OTHER);

   ---
   --  State control
   --
   type Control_State is (
      NORMAL,
      FOCUSED,
      PRESSED,
      DISABLED);

   function  get_state
      return Control_State;

   procedure set_state (state : Control_State);

   subtype Transparency is Float range 0.0 .. 1.0;

   function  get_alpha
      return Transparency;

   procedure set_alpha (alpha : Transparency);

   procedure lock;
   procedure unlock;

   procedure set_style (
      control  : in Controls;
      property : in Properties;
      value    : in unsigned);

   function  get_style (
      control  : Controls;
      property : Properties)
      return unsigned;

   function  get_style (
      control : in Controls;
      element : in Property_Element;
      state   : in Control_State)
      return unsigned;

   ---
   --  Controls
   --
   procedure label (
      bounds : Rectangle;
      text   : String);
   
   function  button (
      bounds : Rectangle;
      label  : String)
      return Boolean;

   procedure checkbox (
      bounds  : in Rectangle;
      text    : in String;
      checked : in out Boolean);

   procedure panel (bounds : Rectangle);

   procedure statusbar (
      bounds : Rectangle;
      text : String);

   procedure toggle (
      bounds : Rectangle;
      text : String;
      active : in out Boolean);

   function  textbox (
      bounds    : in Rectangle;
      text      : in out String;
      edit_mode : in out Boolean) return Boolean;

   function textbox_multi (
      bounds : Rectangle;
      text : in out String;
      edit_mode : in out Boolean)
      return Vector2;
   ---
   --
   --
   type Text_Alignment_Type is (
      TEXT_ALIGN_LEFT,   -- 0
      TEXT_ALIGN_CENTER,
      TEXT_ALIGN_RIGHT);
   for Text_Alignment_Type'Size use unsigned'Size;

   function  to_text_alignment (position : unsigned)
      return Text_Alignment_Type;

   function  to_unsigned (value : Text_Alignment_Type)
      return unsigned;

   function  get_text_width (text : String)
      return int;

   function  get_text_bounds (
      control : Controls;
      bounds : Rectangle)
      return Rectangle;

private

   function get_property_by_state (
      C : Property_Element;
      state : Control_State)
      return Properties;

   procedure load_style_default;

   procedure draw_border (
      rec : Rectangle;
      border_width : int;
      border_color : Color);

   procedure draw_rectangle (
      rec : Rectangle;
      border_width : int;
      border_color : Color;
      bg : Color);

   procedure draw_text (
      text : String;
      bounds : Rectangle;
      alignment : Text_Alignment_Type;
      tint : Color);

end raylib.UI;
