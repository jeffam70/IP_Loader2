Introduction
============

This is the code and resources used during R&D of the protocol to download to the Propeller over Wi-Fi using the XBee Wi-Fi S6B modules.

This project is based in Delphi XE6, compiled to run on Windows 32-bit or 64-bit.  It "may" also be compiled to Mac OS X; though that has not
been tested.

Project/Folder Description
--------------------------

  * IP_Loader/ - contains the IP Loader project.  The Main.pas and XBeeWiFi.pas units are the most important for study. 
    * Loader - contains the IP_Loader.spin source code and binary image for the micro boot loader used by the start of the downloading process.
  * PropellerImages/ - contains the Spin and Binary example Propeller Applications used during testing of the download process.
  * PropellerStream - contains a related project meant to extract the micro boot loader components from the compiled binary image and provide
                      a Delphi array-based representation of those components for pasting into the IP_Loader project's GenerateLoaderPacket
                      method.
    * Parser - contains "StreamParser," a subproject that takes a Delphi-code compatible download stream and parses it into a Propeller Image.
