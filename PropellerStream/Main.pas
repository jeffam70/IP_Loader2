unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.StrUtils, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Edit;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..8191] of Integer;

  TPropellerStreamForm = class(TForm)
    OpenDialog: TOpenDialog;
    LoadPropellerAppButton: TButton;
    SaveDialog: TSaveDialog;
    Description: TLabel;
    StreamMemo: TMemo;
    ButtonBarLayout: TLayout;
    MemoLayout: TLayout;
    DescriptionLabel: TLayout;
    AppNameEdit: TEdit;
    AppNameLabel: TLabel;
    ButtonLayout: TLayout;
    MiniLoaderCheckbox: TCheckBox;
    procedure LoadPropellerAppButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure ProcessMiniLoader;
    procedure GenerateStream(InImage: PByteArray; InSize: Integer; OutImage: PByteArray; var OutSize: Integer);
    procedure GenerateDelphiCode(Image: PByteArray; AppSizeLongs, ImageSizeBytes: Integer; Header: String);
  public
    { Public declarations }
  end;

  EFileCorrupt = class(Exception); {File Corrupt exception}

  procedure FreeFPktImage;

var
  PropellerStreamForm: TPropellerStreamForm;
  FBinImage  : PByteArray;               {Loaded Propeller Application's binary image}
  FBinSize   : Integer;                  {The size of FBinImage (in longs)}
  FLdrImage  : PByteArray;               {Mini-Loader's image}
  FLdrSize   : Integer;                  {The size of the mini-loader (in longs)}
  FPktImage  : array of PByteArray;      {Mini-Loader's executable packets}
  FPktSize   : array of Integer;         {The sizes of mini-loader's executable packets}
  FStrmImage : PByteArray;               {The generated download stream)}
  FStrmSize  : Integer;                  {The size of FStrmImage (in longs)}

const
  ImageLimit     = 32768 + 1;            {Max size of Propeller Application image file (plus 1 to support overlaping-element bit extraction for translation)}
  {Call frame}
  InitCallFrame     : array [0..7] of byte = ($FF, $FF, $F9, $FF, $FF, $FF, $F9, $FF); {See ValidateImageDataIntegrity for info on InitCallFrame}


implementation

{$R *.fmx}

procedure TPropellerStreamForm.LoadPropellerAppButtonClick(Sender: TObject);
var
  Idx         : Integer;
  FStream     : TFileStream;
  ImageSize   : Integer;
  FName       : String;

    {----------------}

    procedure ValidateImageDataIntegrity(Buffer: PByteArray; ImageSize: Integer; Filename: String);
    {Validate Propeller application image data integrity in Buffer.  This is done through a series of tests that verify that the file is not too small.
     Also installs initial call frame.

     PROPELLER APPLICATION FORMAT:
       The Propeller Application image consists of data blocks for initialization, program, variables, and data/stack space.  The first block, initialization, describes the application's
       startup paramemters, including the position of the other blocks within the image, as shown below.

         long 0 (bytes 0:3) - Clock Frequency
         byte 4             - Clock Mode
         byte 5             - Checksum (this value causes additive checksum of bytes 0 to ImageLimit-1 to equal 0)
         word 3             - Start of Code pointer (must always be $0010)
         word 4             - Start of Variables pointer
         word 5             - Start of Stack Space pointer
         word 6             - Current Program pointer (points to first public method of object)
         word 7             - Current Stack Space pointer (points to first run-time usable space of stack)

     WHAT GETS DOWNLOADED:
       To save time, the Propeller Tool does not download the entire Propeller Application Image.  Instead, it downloads only the parts of the image from long 0 through the end of code (up to the
       start of variables) and then the Propeller chip itself writes zeros (0) to the rest of the RAM/EEPROM, after the end of code (up to 32 Kbytes), and inserts the initial call frame in the
       proper location.  This effectively clears (initializes) all global variables to zero (0) and sets all available stack and free space to zero (0) as well.

     INITIAL CALL FRAME:
       The Initial Call Frame is stuffed into the Propeller Application's image at location DBase-8 (eight bytes (2 longs) before the start of stack space).  The Propeller Chip itself stores the
       Initial Call Frame into those locations at the end of the download process.  The initial call frame is exactly like standard run-time call frames in their format, but different in value.

             Call Frame Format:  PBase VBase DBase PCurr Return Extra... (each are words arranged in this order from Word0 to Word3; 4 words = two longs)
       Initial Call Frame Data:  $FFFF $FFF9 $FFFF $FFF9  n/a    n/a

       Note: PBase is Start of Object Program, VBase is Start of Variables, DBase is Start of Data/Stack, PCurr is current program location (PC).

       The Initial Call Frame is stuffed prior to DBase so that if one-too-many returns are executed by the Spin-based Propeller Application, the Initial Call Frame is popped off the stack next
       which instructs the Spin Interpreter to jump to location $FFF9 (PCurr) and execute the byte code there (which is two instructions to perform a "Who am I?" followed by a cog stop, "COGID ID"
       and "COGSTOP ID", to halt the cog).  NOTE: The two duplicate longs $FFFFFFF9 are used for simplicity and the first long just happens to set the right bits to indicate to an ABORT command
       that it has reached the point where it should stop popping the stack and actually execute code.

       Note that a Call Frame is followed by one or more longs of data.  Every call, whether it be to an inter-object method or an intra-object method, results in a call frame that consists
       of the following:

             Return Information     : 2 longs (first two longs shown in Call Frame Format, above, but with different data)
             Return Result          : 1 long
             Method Parameters      : x longs (in left-to-right order)
             Local Variables        : y longs (in left-to-right order)
             Intermediate Workspace : z longs

       The first four items in the call stack are easy to determine from the code.  The last, Intermediate Workspace, is much more difficult because it relates directly to how and when the
       interpreter pushes and pops items on the stack during expression evaluations.}

    var
      Idx      : Integer;
      CheckSum : Byte;
    begin
      {Raise exception if file truncated}
      if (ImageSize < 16) or (ImageSize < PWordArray(Buffer)[4]) then
//        raise EFileCorrupt.Create(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is truncated or is not a Propeller Application' + ifthen(Filename <> '',' file', '') + '!'+#$D#$A#$D#$A+ifthen(Filename <> '', 'File', 'Image') + ' size is less than 16 bytes or is less than word 4 (VarBase) indicates.');
        raise Exception.Create(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is truncated or is not a Propeller Application' + ifthen(Filename <> '',' file', '') + '!'+#$D#$A#$D#$A+ifthen(Filename <> '', 'File', 'Image') + ' size is less than 16 bytes or is less than word 4 (VarBase) indicates.');
      if (PWordArray(Buffer)[3] <> $0010) then
//        raise EFileCorrupt.Create('Initialization code invalid!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Word 3 (CodeBase) must be $0010.');
        raise Exception.Create('Initialization code invalid!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Word 3 (CodeBase) must be $0010.');
      {Write initial call frame}
//      copymemory(@Buffer[min($7FF8, max($0010, PWordArray(Buffer)[5] - 8))], @InitCallFrame[0], 8);
      move(InitCallFrame[0], Buffer[min($7FF8, max($0010, PWordArray(Buffer)[5] - 8))], 8);
      {Raise exception if file's checksum incorrect}
      CheckSum := 0;
      for Idx := 0 to ImageLimit-1 do CheckSum := CheckSum + Buffer[Idx];
      if CheckSum <> 0 then
//        raise EFileCorrupt.Create('Checksum Error!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Byte 5 (Checksum) is incorrect.');
        raise Exception.Create('Checksum Error!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Byte 5 (Checksum) is incorrect.');
      {Check for extra data beyond code}
      Idx := PWordArray(Buffer)[4];
      while (Idx < ImageSize) and ((Buffer[Idx] = 0) or ((Idx >= PWordArray(Buffer)[5] - 8) and (Idx < PWordArray(Buffer)[5]) and (Buffer[Idx] = InitCallFrame[Idx-(PWordArray(Buffer)[5]-8)]))) do inc(Idx);
      if Idx < ImageSize then
        begin
//        {$IFDEF ISLIB}
//        if (CPrefs[GUIDisplay].IValue in [0, 2]) then  {Dialog display needed?  Show error.}
//        {$ENDIF}
//          begin
//          MessageBeep(MB_ICONWARNING);
          raise Exception.Create(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' contains data after code space' + {$IFNDEF ISLIB}' that was not generated by the Propeller Tool' +{$ENDIF} '.  This data will not be displayed or downloaded.');
//          messagedlg(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' contains data after code space' + {$IFNDEF ISLIB}' that was not generated by the Propeller Tool'{$ENDIF}+ '.  This data will not be displayed or downloaded.', mtWarning, [mbOK], 0);
//          end;
        end;
    end;

    {----------------}

begin
  StreamMemo.Lines.Clear;
//  if not OpenDialog.Execute then exit;
  {Initialize}
  ImageSize := 0;
//  fillmemory(Buffer, ImageLimit, 0);
  fillchar(FBinImage[0], ImageLimit, 0);
//  FName := ifthen(Filename <> nil, strpas(Filename), '');
  {Process file/image}
//  if FName <> '' then
//    begin {Path and possibly filename specified}
//    if not fileexists(FName) then
//      begin {Only path specified, or non-existant file; open dialog}
      {Set open dialog parameters}
      OpenDialog.Title := 'Download Propeller Application Image';
      OpenDialog.Filter := 'Propeller Applications (*.binary, *.eeprom)|*.binary;*.eeprom|All Files (*.*)|*.*';
      OpenDialog.FilterIndex := 0;
      OpenDialog.FileName := '';
//      OpenDialog.InitialDir := extractfiledir(FName);
      {Show Open Dialog}
      if not OpenDialog.Execute then exit;
      FName := OpenDialog.Filename;
//      end;
    if fileexists(FName) then
      begin {File found, load it up}
      FStream := TFileStream.Create(FName, fmOpenRead+fmShareDenyWrite);
      try
        ImageSize := FStream.Read(FBinImage^, ImageLimit);
      finally
        FStream.Free;
      end; {finally}
      end;
//    end
//  else
//    begin {Path not specified, use last successfully compiled image}
//    if BinImage = nil then exit;
//    copymemory(@Buffer[0], @BinImage[0], PWordArray(BinImage)[4]);
//    ImageSize := PWordArray(BinImage)[4];
//    end;
  try
    {Validate application image (and install initial call frame)}
    ValidateImageDataIntegrity(FBinImage, min(ImageLimit, ImageSize), FName);
    FBinSize := ImageSize div 4;
    {Download image to Propeller chip (use VBase (word 4) value as the 'image long-count size')}
//    Propeller.Download(Buffer, PWordArray(Buffer)[4] div 4, DownloadCmd);
    if MiniLoaderCheckbox.IsChecked then
      begin
      ProcessMiniLoader;
//      GenerateStream(FLdrImage, FLdrSize, FStrmImage, FStrmSize);
      GenerateDelphiCode(FLdrImage, FLdrSize, FLdrSize*4, '//Raw Application Image');
//      GenerateDelphiCode(FStrmImage, FLdrSize, FStrmSize, '//Optimized Download Stream Image');
      for Idx := high(FPktImage) downto 0 do
        GenerateDelphiCode(FPktImage[Idx], FPktSize[Idx], FPktSize[Idx]*4, '//Raw Executable Packet Code ' + (high(FPktImage)-Idx+1).ToString);
      end
    else
      begin
      GenerateStream(FBinImage, FBinSize, FStrmImage, FStrmSize);
      GenerateDelphiCode(FBinImage, FBinSize, FBinSize*4, '//Raw Application Image');
      GenerateDelphiCode(FStrmImage, FBinSize, FStrmSize, '//Optimized Download Stream Image');
      end;
  except
    on E: EFileCorrupt do
      begin {Image corrupt, show error and exit}
//      if (CPrefs[GUIDisplay].IValue in [0, 2]) then
//        begin
          raise Exception.Create(E.Message);
//        ErrorMsg('052-'+E.Message);                     {Dialog display needed?  Show error.}
//        end
//      else
//        begin
//        {$IFDEF ISLIBWRAP}
//        StdOutMsg(pmtError, '052-'+E.Message);          {Else only write message to standard out}
//        {$ENDIF}
//        end;
      exit;
      end;
  end;
end;

{--------------------------------------------------------------------------------}

procedure TPropellerStreamForm.ProcessMiniLoader;
{Process the Propeller Application as a Mini-Loader application that contains a core and executable packets.}
var
  SIdx, EIdx : Integer;
  RCount     : Integer;  {Removed long count}
  Checksum   : Byte;

begin
  {Reset packet image array}
  FreeFPktImage;
  setlength(FPktSize, 0);
  {Set end and start indexes to the start of Spin code (after data; PASM)}
  EIdx := PWordArray(FBinImage)[6] div 4;
  SIdx := EIdx-1;
  {Prime removed long count to end index}
  RCount := EIdx;
  {Find and extract executable packet code}
  repeat
    {Find previous packet code marker, if any}
    while (SIdx > 0) and (PIntArray(FBinImage)[SIdx] <> $11111111) do dec(SIdx);
    if SIdx > 0 then
      begin {Found a packet code marker}
      {Make room in Packet Image array}
      setlength(FPktImage, high(FPktImage)+2);
      setlength(FPktSize, high(FPktSize)+2);
      getmem(FPktImage[high(FPktImage)], ImageLimit);
      {Calculate size of packet code}
      FPktSize[high(FPktSize)] := EIdx-SIdx-1;
      {Copy packet code into array}
      move(FBinImage[SIdx*4+4], FPktImage[high(FPktImage)][0], FPktSize[high(FPktSize)]*4);
      end;
    {Ready to find previous packet code, if any}
    if SIdx > 0 then EIdx := SIdx;
    dec(SIdx);
  until SIdx < 0;
  {Calculate total longs to remove from application image}
  dec(RCount, EIdx);
  {Copy core data (PASM) and Spin code to loader image array, leaving out executable packet code}
  move(FBinImage[0], FLdrImage[0], EIdx*4);
  FLdrSize := EIdx;
  move(FBinImage[PWordArray(FBinImage)[6]], FLdrImage[FLdrSize*4], PWordArray(FBinImage)[7]-PWordArray(FBinImage)[6]);
  inc(FLdrSize, (PWordArray(FBinImage)[4]-PWordArray(FBinImage)[6]) div 4);
  {Adjust pointers (start of variables, start of stack space, first public method pointer, current stack pointer, and Spin method pointers}
  for SIdx := 4 to 7 do PWordArray(FLdrImage)[SIdx] := PWordArray(FLdrImage)[SIdx] - RCount*4;
  for SIdx := 0 to PWordArray(FLdrImage)[9]-1 do PWordArray(FLdrImage)[8+SIdx*2] := PWordArray(FLdrImage)[8+SIdx*2] - RCount*4;
  {Recalculate and adjust checksum}
  FLdrImage[5] := 0;
  Checksum := 0;
  for SIdx := 0 to ImageLimit-1 do CheckSum := CheckSum + FLdrImage[SIdx];
  FLdrImage[5] := $100-Checksum;
end;

{--------------------------------------------------------------------------------}

procedure TPropellerStreamForm.GenerateStream(InImage: PByteArray; InSize: Integer; OutImage: PByteArray; var OutSize: Integer);
{Take Propeller Application image (InImage) and generate Propeller Download stream (OutImage) in an optimized format (3, 4, or 5 bits per byte; 7 to 11
 bytes per long).  Note: for every 5 contiguous bits in Propeller Application Image (LSB first) 3, 4, or 5 bits can be translated to a byte.  The process
 requires 5 bits input (ie: indexed into the array) and gets a byte out that contains the first 3, 4, or 5 bits encoded in the Propeller Download stream
 format. If less than 5 bits were translated, the remaining bits leads the next 5 bit translation unit input to the translation process.}
var
  BValue : Byte;     {Binary Value to translate}
  BitsIn : Byte;     {Number of bits ready for translation}
  BCount : Integer;  {Total number of bits translated}
const
  dtTx = 0;          {Data type: Translation pattern}
  dtBits = 1;        {Date type: Bits translated}

  {Power of 2 - 1 array.  Index into this array with the desired power of 2 (1 through 5) and element value is mask equal to power of 2 minus 1}
  Pwr2m1 : array[1..5] of byte = ($01, $03, $07, $0F, $1F);

  {Propeller Download Stream Translator array.  Index into this array using the "Binary Value" (usually 5 bits) to translate,
   the incoming bit size (again, usually 5), and the desired data element to retrieve (dtTx = translation, dtBits = bit count
   actually translated.}

               {Binary    Incoming    Translation }
               {Value,    Bit Size,   or Bit Count}
  PDSTx : array[0..31,      1..5,     dtTx..dtBits]   of byte =

              {***  1-BIT  ***}   {***  2-BIT  ***}   {***  3-BIT  ***}   {***  4-BIT  ***}   {***  5-BIT  ***}
          ( ( {%00000} ($FE, 1),  {%00000} ($F2, 2),  {%00000} ($92, 3),  {%00000} ($92, 3),  {%00000} ($92, 3) ),
            ( {%00001} ($FF, 1),  {%00001} ($F9, 2),  {%00001} ($C9, 3),  {%00001} ($C9, 3),  {%00001} ($C9, 3) ),
            (          (0,   0),  {%00010} ($FA, 2),  {%00010} ($CA, 3),  {%00010} ($CA, 3),  {%00010} ($CA, 3) ),
            (          (0,   0),  {%00011} ($FD, 2),  {%00011} ($E5, 3),  {%00011} ($25, 4),  {%00011} ($25, 4) ),
            (          (0,   0),           (0,   0),  {%00100} ($D2, 3),  {%00100} ($D2, 3),  {%00100} ($D2, 3) ),
            (          (0,   0),           (0,   0),  {%00101} ($E9, 3),  {%00101} ($29, 4),  {%00101} ($29, 4) ),
            (          (0,   0),           (0,   0),  {%00110} ($EA, 3),  {%00110} ($2A, 4),  {%00110} ($2A, 4) ),
            (          (0,   0),           (0,   0),  {%00111} ($FA, 3),  {%00111} ($95, 4),  {%00111} ($95, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01000} ($92, 3),  {%01000} ($92, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01001} ($49, 4),  {%01001} ($49, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01010} ($4A, 4),  {%01010} ($4A, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01011} ($A5, 4),  {%01011} ($A5, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01100} ($52, 4),  {%01100} ($52, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01101} ($A9, 4),  {%01101} ($A9, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01110} ($AA, 4),  {%01110} ($AA, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),  {%01111} ($D5, 4),  {%01111} ($D5, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10000} ($92, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10001} ($C9, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10010} ($CA, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10011} ($25, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10100} ($D2, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10101} ($29, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10110} ($2A, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%10111} ($95, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11000} ($92, 3) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11001} ($49, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11010} ($4A, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11011} ($A5, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11100} ($52, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11101} ($A9, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11110} ($AA, 4) ),
            (          (0,   0),           (0,   0),           (0,   0),           (0,   0),  {%11111} ($55, 5) )
          );
begin
  BCount := 0;
  OutSize := 0;
  while BCount < (InSize*4) * 8 do                                                         {For all bits in data stream...}
    begin
      BitsIn := Min(5, (InSize*4) * 8 - BCount);                                           {  Determine number of bits in current unit to translate; usually 5 bits}
      BValue := ( (InImage[BCount div 8] shr (BCount mod 8)) +                             {  Extract next translation unit (contiguous bits, LSB first; usually 5 bits)}
        (InImage[(BCount div 8) + 1] shl (8 - (BCount mod 8))) ) and Pwr2m1[BitsIn];
      OutImage[OutSize] := PDSTx[BValue, BitsIn, dtTx];                                    {  Translate unit to encoded byte}
      inc(OutSize);                                                                        {  Increment byte index}
      inc(BCount, PDSTx[BValue, BitsIn, dtBits]);                                          {  Increment bit index (usually 3, 4, or 5 bits, but can be 1 or 2 at end of stream)}
    end;
end;

{--------------------------------------------------------------------------------}

procedure TPropellerStreamForm.GenerateDelphiCode(Image: PByteArray; AppSizeLongs, ImageSizeBytes: Integer; Header: String);
var
  StrList : TStringList;
  Str     : String;   {Temporary String}
  HLen    : Integer;  {Length of header (for string creation)}
  BIdx    : Integer;  {Byte index (for string creation)}
begin
  {Generate Delphi code}
  StrList := TStringList.Create;
  StrList.Text := StreamMemo.Text;
  try
    StrList.Add(Header);
    StrList.Add('');
    StrList.Add('  ' + AppNameEdit.Text + 'AppSize = ' + inttostr(AppSizeLongs) + ';');
    Str := '  ' + AppNameEdit.Text + 'Image : array[0..' + inttostr(ImageSizeBytes-1) + '] of byte = (';
    HLen := Length(Str);
    for BIdx := 0 to ImageSizeBytes-1 do
      begin
      if (BIdx > 0) and (BIdx mod 16 = 0) then
        begin
        StrList.Add(Str);
        Str := System.StringOfChar(' ', HLen);
        end;
      Str := Str + '$' + inttohex(Image[BIdx],2) + ifthen(BIdx < ImageSizeBytes-1, ',', '');
      end;
    Str := Str + ');';
    StrList.Add(Str);
    StrList.Add('');
    StrList.Add('');
    StreamMemo.Text := StrList.Text;
  finally
    StrList.Free;
  end;
end;

{--------------------------------------------------------------------------------}

procedure FreeFPktImage;
begin
  while high(FPktImage) > -1 do
    begin
    freemem(FPktImage[high(FPktImage)]);
    setlength(FPktImage, high(FPktImage));
    end;
end;

{--------------------------------------------------------------------------------}

initialization
  getmem(FBinImage, ImageLimit);
  getmem(FLdrImage, ImageLimit);
  getmem(FStrmImage, ImageLimit*11);

{--------------------------------------------------------------------------------}

finalization
  freemem(FBinImage);
  freemem(FLdrImage);
  FreeFPktImage;
  setlength(FPktSize, 0);
  freemem(FStrmImage);

end.

