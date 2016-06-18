Ultibo Custom Font Creator tool

This tool can be used to create a custom fixed width and height bitmap font from any Windows supported font. In combination with the UFnt2Unit tool these can be converted to a pascal unit for use in Ultibo projects. See below for usage.

Please respect copyright restrictions when creating fonts with this tool, not all fonts are free and not all allow embedding in applications.

Contributed by Kerry Shipman

Compiled with Lazarus

--------------

Using UFontCreator and UFnt2Unit

Run UFontCreator to export any Windows font to a <font>.ufnt text file. You can select any font registered in Windows, change the size, and even preview the raw character patterns before exporting the font data.  Fonts are saved in the same directory that the UFontCreator program is located.

The individual characters in the .ufnt file can be edited as desired with any text editor. You can make adjustments to char looks or create custom characters or icons in unused char code slots.

The maximum character size allowed by Ultibo is 32 wide by 64 high. UFontCreator will allow you to set sizes larger than that in case that Ultibo limit is ever changed.

Currently the fonts can only be exported as fixed width and 1 bit per pixel as that is what is supported by Ultibo.  

NOTE: If the windows font has a space in the name rename it without the space and also edit the FontName= field to remove the spaces. This will make sure that the font can be properly converted into a .pas unit by the UFnt2Unit program.

--------------

Once the .ufnt file is done run the UFnt2Unit program, select the <font>.ufnt file to convert and click the Export Font Unit button. It will create the <font>.pas file you will need to include in your project to use the font. Font units are saved in the same directory as the UFnt2Unit program.

--------------

Add the font to your uses clause.

In your Ultibo program you can get the font handle using:

FontHandle:TFontHandle;

FontHandle := FontFindByName('fontname');

Activate the font by using:

ConsoleWindowSetFont(WindowHandle, FontFindByName('fontname'));
