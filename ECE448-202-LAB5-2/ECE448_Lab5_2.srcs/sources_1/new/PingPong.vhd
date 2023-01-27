
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- simulation library
--library UNISIM;
--use UNISIM.VComponents.all;

-- the mouse_displayer entity declaration
-- read above for behavioral description and port definitions.
entity PingPong is
port (
   pixel_clk: in std_logic;
   xpos     : in std_logic_vector(11 downto 0);
   ypos     : in std_logic_vector(11 downto 0);
   hcount   : in std_logic_vector(11 downto 0);
   vcount   : in std_logic_vector(11 downto 0);
   enable_ball_display_out : out std_logic;
   red_out  : out std_logic_vector(3 downto 0);
   green_out: out std_logic_vector(3 downto 0);
   blue_out : out std_logic_vector(3 downto 0)
);

-- force synthesizer to extract distributed ram for the
-- displayrom signal, and not a block ram, to save BRAM resources.
attribute rom_extract : string;
attribute rom_extract of pingpong: entity is "yes";
attribute rom_style : string;
attribute rom_style of pingpong: entity is "distributed";

end pingpong;

architecture Behavioral of pingpong is

------------------------------------------------------------------------
-- CONSTANTS
------------------------------------------------------------------------

type displayrom is array(0 to 255) of std_logic_vector(1 downto 0);
-- the memory that holds the cursor.
-- 00 - black
-- 01 - white
-- 1x - transparent

constant ballrom: displayrom := (
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01",
"01","01","01","01","01","01","01","01","01","01","01","01","01","01","01","01"
);

-- width and height of cursor.
constant OFFSET: std_logic_vector(4 downto 0) := "10000";   -- 16

------------------------------------------------------------------------
-- SIGNALS
------------------------------------------------------------------------

-- pixel from the display memory, representing currently displayed
-- pixel of the cursor, if the cursor is being display at this point
signal mousepixel: std_logic_vector(1 downto 0) := (others => '0');
-- when high, enables displaying of the cursor, and reading the
-- cursor memory.
signal enable_ball_display: std_logic := '0';

-- difference in range 0-15 between the vga counters and mouse position
signal xdiff: std_logic_vector(3 downto 0) := (others => '0');
signal ydiff: std_logic_vector(3 downto 0) := (others => '0');

signal red_int  : std_logic_vector(3 downto 0);
signal green_int: std_logic_vector(3 downto 0);
signal blue_int : std_logic_vector(3 downto 0);

signal red_int1  : std_logic_vector(3 downto 0);
signal green_int1: std_logic_vector(3 downto 0);
signal blue_int1 : std_logic_vector(3 downto 0);

begin

   -- compute xdiff
   x_diff: process(hcount, xpos)
   variable temp_diff: std_logic_vector(11 downto 0) := (others => '0');
   begin
         temp_diff := hcount - xpos;
         xdiff <= temp_diff(3 downto 0);
   end process x_diff;

   -- compute ydiff
   y_diff: process(vcount, xpos)
   variable temp_diff: std_logic_vector(11 downto 0) := (others => '0');
   begin
         temp_diff := vcount - ypos;
         ydiff <= temp_diff(3 downto 0);
   end process y_diff;

 -- read pixel from memory at address obtained by concatenation of
   -- ydiff and xdiff
   mousepixel <= ballrom(conv_integer(ydiff & xdiff))
                 when rising_edge(pixel_clk);

   -- set enable_mouse_display high if vga counters inside cursor block
   enable_mouse: process(pixel_clk, hcount, vcount, xpos, ypos)
   begin
      if(rising_edge(pixel_clk)) then
         if(hcount >= xpos +X"001" and hcount < (xpos + OFFSET - X"001") and
            vcount >= ypos and vcount < (ypos + OFFSET)) and
            (mousepixel = "00" or mousepixel = "01")
         then
            enable_ball_display <= '1';
         else
            enable_ball_display <= '0';
         end if;
      end if;
   end process enable_mouse;
   
enable_ball_display_out <= enable_ball_display;

   -- if cursor display is enabled, then, according to pixel
   -- value, set the output color channels.
 process(pixel_clk)
   begin
      if(rising_edge(pixel_clk)) then
            if(enable_ball_display = '1') then
               if(mousepixel = "01") then
                  red_out <= (others => '1');
                  green_out <= (others => '1');
                  blue_out <= (others => '1');
               end if;
            end if;
      end if;
   end process;


end Behavioral;
