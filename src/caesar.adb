-- caesar.adb
--     An improved algorithm that uses bit magic to transform ASCII
--     characters instead of looping through a lookup table.
--
-- Written by: A Dikdik <cutedikdik@yandex.com>
--     Copyright (c) 2017 A Dikdik
--     All rights reserved.
--    
--     Licensed under the BSD clause 3 license.
--
-- About:
--     The goal of this program is to function as a bi-directional way of
--     ciphering and deciphering text using the classical Caesar cipher (ROT-1)
--
--     In order to do this without using a look-up table that contains pairs of
--     both upper and lower-case ASCII letters this program uses tricks in order
--     shift and wrap around ASCII values. In some ways this is more efficient
--     and requires less code while still performing the same basic job as a
--     program that uses a look-up table.
--
-- to compile: ~$ make
--
-- to encipher: ~$ ./bin/caesar --encipher 3 'some text to encipher'
-- to decipher: ~$ ./bin/caesar --decipher 3 'some text to decipher'
with Ada.Text_IO;
with Ada.Command_Line;

procedure Caesar
is
   -- to be brief and safe we rename instead of use
   package IO renames Ada.Text_IO;
   package CL renames Ada.Command_Line;
   
   -- create some variables to hold our argument parsing state
   Key_Argument  : Integer;  -- stores the key argument
   Mode          : Boolean;  -- stores whether or not this is a cipher operation
   
   -- ASCII value constants
   ASCII_la : constant Integer := 97;
   ASCII_lz : constant Integer := 122;
   ASCII_UA : constant Integer := 65;
   ASCII_UZ : constant Integer := 90;
   
   -- Shift_Character
   --     Takes a valid ASCII letter and either shifts it up in value by one
   --     or, in the case of 'z' and 'Z', assigns it the value of 'a' or 'A'
   --     or vice versa depending on the mode.
   --
   -- Parameter: (Character) In_Character  - Valid ASCII character to shift
   -- Parameter: (Integer)   Key           - Places to shift
   -- Parameter: (Boolean)   Mode          - Whether or not this is a deciphering
   -- Return:    (Character)               - Valid ASCII character result
   function Shift_Character (In_Character : Character;
			     Key          : Integer;
			     Mode         : Boolean)
			    return Character
   is
      Temp_Character  : Integer;
      Key_Text_Sum    : Integer;
      Key_Text_Sub    : Integer;
   begin
      Temp_Character  := Character'Pos (In_Character);
      Key_Text_Sum    := Temp_Character + Key;
      Key_Text_Sub    := Key_Text_Sum - (Key * 2);
      
      if Mode = TRUE then
	 if Key_Text_Sum > ASCII_lz then
	    Temp_Character := ((Key_Text_Sum - ASCII_lz) + ASCII_la);
	 elsif Key_Text_Sum > ASCII_UZ and Key_Text_Sum < ASCII_la then
	    Temp_Character := ((Key_Text_Sum - ASCII_UZ) + ASCII_UA);
	 else
	    Temp_Character := Key_Text_Sum;
	 end if;
      elsif Mode = FALSE then
	 if Key_Text_Sub < ASCII_la and Key_Text_Sub > ASCII_UZ then
	    Temp_Character := (ASCII_lz - (ASCII_la - Key_Text_Sub));
	 elsif Key_Text_Sub < ASCII_UA then
	    Temp_Character := (ASCII_UZ - (ASCII_UA - Key_Text_Sub));
	 else
	    Temp_Character := Key_Text_Sub;
	 end if;
      end if;
      
      return Character'Val (Temp_Character);
   end Shift_Character;
   
   -- Is_Valid_Character
   --     Takes a character and returns whether or not it is considered to be
   --     a valid ASCII character that can be shifted.
   --
   -- Memo:
   --     If the character is invalid then it should be readded to the string
   --     as is, effectively ignoring it.
   --
   -- Parameter: (Character) In_Character - Character to be evaluated
   -- Return:    (Boolean)                - Result of whether or not the input is valid
   function Is_Valid_Character (In_Character : Character)
			       return Boolean
   is
      Character_Code : Integer;
   begin
      Character_Code := Character'Pos (In_Character);
      
      if Character_Code >= ASCII_UA and Character_Code <= ASCII_UZ then
	 return TRUE;
      elsif Character_Code >= ASCII_la and Character_Code <= ASCII_lz then
	 return TRUE;
      end if;
      
      return FALSE;
   end Is_Valid_Character;
   
   -- Cipher
   --     Takes a string and applies the cipher algorithm to it
   --
   -- Parameter (String)  Text    - Text to be ciphered
   -- Parameter (Integer) Key     - Places to shift
   -- Parameter (Boolean) Mode    - Whether to encipher or decipher
   procedure Cipher (Text : in String;
		     Key  : in Integer;
		     Mode : in Boolean)
   is
      I : Integer := 1; -- loop iterator
   begin
      for I in 1 .. Text'Length loop
	 if Is_Valid_Character (Text (I)) = TRUE then
	    IO.Put (Shift_Character (Text (I), Key, Mode));
	 else
	    IO.Put (Text (I));
	 end if;
      end loop;
      
      IO.New_Line (1);
   end Cipher;
   
begin
   -- check number of arguments (expects 3)
   if CL.Argument_Count /= 3 then
      IO.Put_Line ("Unexpected amount of arguments!");
      return;
   end if;
   
   -- check if it is a mode flag
   -- memo: argument 1 must be a mode flag!
   if CL.Argument (1) = "--decipher" then
      Mode := FALSE;
   elsif CL.Argument (1) = "--encipher" then
      Mode := TRUE;
   else
      IO.Put_Line ("Could not find mode flag at position one.");
      return;
   end if;
   
   -- grab the text and the key
   Key_Argument := Integer'Value (CL.Argument (2));
   
   -- generate our cipher-text
   Cipher (CL.Argument (3), Key_Argument, Mode);
end Caesar;
