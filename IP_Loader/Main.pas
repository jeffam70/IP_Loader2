unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  XBeeWiFi, IdGlobal, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient;

type
  {Define XBee Info record}
  PXBee = ^ TXBee;
  TXBee = record
    PCPort      : String;                     {Pseudo-Communication Port (derived from MacAddr}
    IPAddr      : String;                     {IP Address}
    IPPort      : Cardinal;                   {IP Port}
    MacAddrHigh : Cardinal;                   {Upper 16 bits of MAC address}
    MacAddrLow  : Cardinal;                   {Lower 32 bits of MAC address}
    NodeID      : String;                     {Friendly Node ID}
    CfgChecksum : Cardinal;                   {Configuration checksum}
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PCPortLabel: TLabel;
    IPPort: TEdit;
    PortLabel: TLabel;
    IdentifyButton: TButton;
    ResetPulseButton: TButton;
    NodeID: TEdit;
    PCPortCombo: TComboBox;
    IPAddr: TEdit;
    MACAddr: TEdit;
    TransmitButton: TButton;
    OpenDialog: TOpenDialog;
    LoadButton: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PCPortComboChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TransmitButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure GenerateResetSignal;
    function EnforceXBeeConfiguration: Boolean;
  public
    { Public declarations }
  end;

  EFileCorrupt = class(Exception); {File Corrupt exception}


var
  Form1     : TForm1;
  XBee      : TXBeeWiFi;
  XBeeList  : array of TXBee;           {Dynamic array of XBee Info records}
  TxBuf     : TIdBytes;                 {Transmit packet (resized per packet)}
  RxBuf     : TIdBytes;                 {Receive packet (resized on receive)}
  Buffer    : PByteArray;               {Holds Propeller Application image loaded from disk or copied from BinImage}
  FBinImage : PByteArray;               {A copy of the Propeller Application's binary image (used to generate the download stream)}
  FBinSize  : Integer;                  {The size of FBinImage (in longs)}


const
  SerTimeout     = 1000;
  AppTimeout     = 200;
  CSumUnknown    = $FFFFFFFF;          {Unknown checksum value}
  ImageLimit     = 32768;              {Max size of Propeller Application image file}

  {Call frame}
  InitCallFrame     : array [0..7] of byte = ($FF, $FF, $F9, $FF, $FF, $FF, $F9, $FF); {See ValidateImageDataIntegrity for info on InitCallFrame}


implementation

{$R *.fmx}

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------- Event Methods ------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
  XBee := TXBeeWiFi.Create;
  XBee.SerialTimeout := SerTimeout;
  XBee.ApplicationTimeout := AppTimeout;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  XBee.Destroy;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
//var
//  IP : TIDStack;
begin
  XBee.SetItem(xbIO2State, pinOutHigh);
//  IP := TIDStack.Create;
//  IP.NewInstance;
//  IPAddr.Text := IP.LocalAddress;
//  IP.Free;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  XBee.SetItem(xbIO2State, pinOutLow);
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.IdentifyButtonClick(Sender: TObject);
var
  Idx  : Cardinal;
  Nums : TSimpleNumberList;
  PXB   : PXBee;
begin
  PCPortCombo.Clear;
  SetLength(XBeeList, 0);
  MacAddr.Text := '';
  IPAddr.Text := '';
  IPPort.Text := '';
  NodeID.Text := '';
{ TODO : Harden IdentifyButtonClick for interim errors.  Handle gracefully. }
  XBee.RemoteIPAddr := '192.168.1.255';
  if XBee.GetItem(xbIPAddr, Nums) then
    begin
    for Idx := 0 to High(Nums) do
      begin
      SetLength(XBeeList, Length(XBeeList)+1);
      PXB := @XBeeList[High(XBeeList)];
      PXB.CfgChecksum := CSumUnknown;
      PXB.IPAddr := FormatIPAddr(Nums[Idx]);
      XBee.RemoteIPAddr := PXB.IPAddr;
      if XBee.GetItem(xbIPPort, PXB.IPPort) then
        if XBee.GetItem(xbMacHigh, PXB.MacAddrHigh) then
          if XBee.GetItem(xbMacLow, PXB.MacAddrLow) then
            if XBee.GetItem(xbNodeID, PXB.NodeID) then
              begin
              PXB.PCPort := 'XB' + rightstr(inttostr(PXB.MacAddrLow), 2);
              PCPortCombo.Items.AddObject(PXB.PCPort, TObject(PXB));
              end;
      end;
    if PCPortCombo.Count > 0 then PCPortCombo.DropDown;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.LoadButtonClick(Sender: TObject);
var
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
  if not OpenDialog.Execute then exit;
  {Initialize}
  ImageSize := 0;
//  fillmemory(Buffer, ImageLimit, 0);
  fillchar(Buffer, ImageLimit, 0);
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
      OpenDialog.InitialDir := extractfiledir(FName);
      {Show Open Dialog}
      if not OpenDialog.Execute then exit;
      FName := OpenDialog.Filename;
//      end;
    if fileexists(FName) then
      begin {File found, load it up}
      FStream := TFileStream.Create(FName, fmOpenRead+fmShareDenyWrite);
      try
        ImageSize := FStream.Read(Buffer^, ImageLimit);
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
    ValidateImageDataIntegrity(Buffer, min(ImageLimit, ImageSize), FName);
    {Download image to Propeller chip (use VBase (word 4) value as the 'image long-count size')}
//    Propeller.Download(Buffer, PWordArray(Buffer)[4] div 4, DownloadCmd);
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

{----------------------------------------------------------------------------------------------------}

procedure TForm1.ResetPulseButtonClick(Sender: TObject);
begin
  GenerateResetSignal;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.PCPortComboChange(Sender: TObject);
begin
  IPAddr.Text := XBeeList[PCPortCombo.Selected.Index].IPAddr;
  XBee.RemoteIPAddr := IPAddr.Text;
  IPPort.Text := inttostr(XBeeList[PCPortCombo.Selected.Index].IPPort);
  XBee.RemoteSerialIPPort := XBeeList[PCPortCombo.Selected.Index].IPPort;
  NodeID.Text := XBeeList[PCPortCombo.Selected.Index].NodeID;
  MacAddr.Text := FormatMACAddr(XBeeList[PCPortCombo.Selected.Index].MacAddrHigh, XBeeList[PCPortCombo.Selected.Index].MacAddrLow);
end;


{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.GenerateResetSignal;
{Generate Reset Pulse}
begin
{ TODO : Enhance GenerateResetSignal for errors. }
  if EnforceXBeeConfiguration then XBee.SetItem(xbOutputState, $0000);            {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.EnforceXBeeConfiguration: Boolean;
{Validate necessary XBee configuration; set attributes if needed.
 Returns True if XBee properly configured; false otherwise.}

    {----------------}

    function Validate(Attribute: xbCommand; Value: Cardinal; ReadOnly: Boolean = False): Boolean;
    {Check if XBee Attribute is equal to Value; if not, set it as such.
     Set ReadOnly if attribute should be read and compared, but not written.
     Returns True upon exit if Attribute = Value.}
    var
      Setting : Cardinal;
    begin
      if not XBee.GetItem(Attribute, Setting) then raise Exception.Create('Can not read XBee attribute.');
      Result := Setting = Value;
      if not Result and not ReadOnly then
        begin
          if not XBee.SetItem(Attribute, Value) then raise Exception.Create('Can not set XBee attribute.');
          Result := True;
        end;
    end;

    {----------------}

begin
{ TODO : Enhance Enforce... to log any error }
  Result := (XBeeList[PCPortCombo.Selected.Index].CfgChecksum <> CSumUnknown) and            {Is the configuration known and valid?}
            (Validate(xbChecksum, XBeeList[PCPortCombo.Selected.Index].CfgChecksum, True));
  if not Result then                                                                         {  If not...}
    begin
    Validate(xbSerialIP, SerialUDP, False);                                                  {    Ensure XBee's Serial Service uses UDP packets}
    Validate(xbIO2State, pinOutHigh);                                                        {    Ensure I/O is set to output high}
    Validate(xbOutputMask, $7FFF);                                                           {    Ensure output mask is proper (default, in this case)}
    Validate(xbIO2Timer, 1);                                                                 {    Ensure DIO2's timer is set to 100 ms}
    Validate(xbSerialMode, TransparentMode);                                                 {    Ensure Serial Mode is transparent}
    Validate(xbSerialBaud, Baud115200);                                                      {    Ensure baud rate is 115,200 bps}
    Validate(xbSerialParity, ParityNone);                                                    {    Ensure parity is none}
    Validate(xbSerialStopBits, StopBits1);                                                   {    Ensure stop bits is 1}
    XBee.GetItem(xbChecksum, XBeeList[PCPortCombo.Selected.Index].CfgChecksum);              {    Record new configuration checksum}
    Result := True;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.TransmitButtonClick(Sender: TObject);
var
  i             : Integer;
  r             : Byte;
  TxBuffLength  : Integer;

  RxCount       : Integer;
  FRxBuffStart  : Cardinal;
  RxBuffSize    : Cardinal;
  FRxBuffEnd    : Cardinal;
  FVersion      : Byte;
  FVersionMode  : Boolean;
  FDownloadMode : Byte;

const
  {The TxHandshake array consists of 250 bytes representing the bit 0 values (0 or 1) of each of 250 iterations of the LFSR (seeded with ASCII 'P') described above.}
  TxHandshake : array[1..250] of byte = (0,1,0,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,1,0,0,0,1,0,0,0,
                                         1,1,0,1,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,1,0,1,0,1,1,0,1,0,0,1,1,1,1,1,
                                         1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,1,0,1,0,1,0,0,1,
                                         0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,1,1,1,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,0,0,1,0,1,0,0,1,1,0,1);

  {The RxHandshake array consists of 250 bytes representing the bit 0 values (0 or 1) of each of 250 successive iterations of the LFSR of TxHandshake, above.}
  RxHandshake : array[1..250] of byte = (0,1,0,0,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,1,0,0,
                                         0,1,0,0,0,1,1,0,1,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,1,0,1,0,1,1,0,1,0,0,
                                         1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,1,0,1,
                                         0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,1,1,1,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,0,0,1,0,1,0);

    {----------------}

    procedure AppendByte(x: Byte);
    {Add byte to comm buffer}
    begin
      TxBuf[TxBuffLength] := x;
      Inc(TxBuffLength);
    end;

    {----------------}

    procedure AppendLong(x: Cardinal);
    {Add encoded long (11 bytes) to comm buffer}
    var
      i: Cardinal;
    begin
      for i := 0 to 10 do
        begin
        AppendByte($92 or -Ord(i=10) and $60 or x and 1 or x and 2 shl 2 or x and 4 shl 4);
        x := x shr 3;
        end;
    end;

    {----------------}

    function ReceiveBit(Template: Boolean; Timeout: Int64; Connected: Boolean = True): Byte;
    {Receive bit-sized response (0 or 1) from Propeller, optionally transmitting timing template if necessary.
     Template:  True: if no response, transmit timing template.
                False: if no response, fail.
     Connected: True: established communication already.
                False: haven't established communication yet.}
    var
      StartTime  : Int64;
      Read       : Boolean; {True = data already read before ReadFile returned; False = data read in progress}
      WaitResult : Cardinal;

        {----------------}

        procedure ReadError;
        {Notify of read error.  This is a non-fatal error if FAbortMode = False, or is a fatal error if FAbortMode = True}
        begin
          raise exception.Create('Some kind of Read Error occurred');
//          Error(ord(mtPortUnreadable)*ord(not FAbortMode) + ord(mtNoRead)*ord(FAbortMode) + (strtoint(FComPort) shl 16));
        end;

        {----------------}

        function GetTime: Int64;
        {Return millisecond clock count.  This will be computed from either the high-performance system counter, if one exists,
        or the standard clock, if the high-performance system counter doesn't exist.}
        begin
//          if FHPCFreq > 0 then
//            begin {High performance counter exists, use it}
//            QueryPerformanceCounter(Result);
//            Result := Result div FHPCFreq;
//            end
//          else    {High performance counter does not exist, use standard}
{ TODO : Replace GetTime with a multi-platform solution. }
            Result := System.Classes.TThread.GetTickCount;
        end;

        {----------------}

    begin
//      FCommOverlap.Offset := 0;
//      FCommOverlap.OffsetHigh := 0;
      StartTime := GetTime;
      repeat                                                                                        {Loop...}
        if FRxBuffStart = FRxBuffEnd then                                                             {Buffer empty, check Rx}
          begin
          FRxBuffStart := 0;                                                                            {Reset start}
//          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtProgress));                            {Update GUI - Progressing (receiving bit)}
          if Template then                                                                              {Need echo byte?}
            begin
            AppendByte($F9);
            XBee.SendUDP(TxBuf);                                                                                     {Transmit template byte}
            sleep(100);                                                                                   {Delay to give plenty of round-trip time}
            end;
          Read := XBee.ReceiveUDP(RxBuf, 2000);
//          Read := XBee.ReceiveTCP(RxBuf, 2000);
          RxBuffSize := length(RxBuf);
          FRxBuffEnd := RxBuffSize;
          if not Read then                                                                              {Data not entirely read yet?}
            begin
//            if GetLastError <> ERROR_IO_PENDING then ReadError;                                           {Error, unable to read}
//            WaitResult := waitforsingleobject(FCommIOEvent, 1000);                                        {Wait for completion, or 1 second, whichever comes first}
//            if WaitResult = WAIT_FAILED then Error(ord(mtWaitFailed));
//            if WaitResult = WAIT_TIMEOUT then ReadError;                                                  {Error, timed-out on read of PC hardware}
              raise Exception.Create('Error: Timed-out on read.');
            end;
//          if not GetOverlappedResult(FCommHandle, FCommOverlap, FRxBuffEnd, True) then ReadError;       {Get count of received bytes; error if necessary}
          end;
        if FRxBuffStart <> FRxBuffEnd then                                                            {Buffer has data, parse it}
          begin
          Result := RxBuf[FRxBuffStart] - $FE;                                                        {Translate properly-formed data to 0 or 1; improper data will be > 1}
          Inc(FRxBuffStart);
          if (Result and $FE = 0) or (not Connected) then Exit;                                         {Result properly-formed? (or ill-formed but not yet connected); exit, returning Result}
          raise Exception.Create('Hardware Lost');                                                     {Otherwise, error; lost communication}
          end;
      until GetTime - StartTime > Timeout;                                                          {Loop back until time-out}
      raise Exception.Create('Hardware Lost');                                                      {Timed-out? Error; lost communication}
    end;

   {----------------}

begin

  FVersionMode := True;
  FDownloadMode := 1; {The download command; 1 = write to RAM and run, 2 = write to EEPROM and stop, 3 = write to EEPROM and run}

  FRxBuffStart := 0;
  FRxBuffEnd := 0;

//  if not XBee.ConnectTCP then
  if not XBee.ConnectSerialUDP then
    begin
    showmessage('Cannot connect');
    exit;
    end;

  try
    GenerateResetSignal;         {(Enforce XBee Configuration and...) Generate reset signal}
    IndySleep(190);
//    if XBee.ConnectTCP then
//      begin
//      try
        SetLength(TxBuf, 1+250+250+8);
        TxBuffLength := 0;
        AppendByte($F9);
        for i := 1 to 250 do AppendByte(TxHandshake[i] or $FE);                     {Note "or $FE" encodes 1 handshake bit per transmitted byte}
        for i := 1 to 250 + 8 do AppendByte($F9);

        XBee.SendUDP(TxBuf);
//        XBee.ReceiveUDP(RxBuf, 0, 1000);

//        GenerateResetSignal;
//        XBee.SetItem(xbData, $0000);
//        XBee.Send(TxBuf);
//      finally
//        XBee.DisconnectTCP;
//      end;
//      end;

      {Receive connect string}
      RxCount := 0;
      i := 1;
      repeat                                                                      {Loop}
        r := ReceiveBit(False, 100, False);                                       {  Receive encoded bit}
        inc(RxCount);
        if r = RxHandshake[i] then                                                {  Bits match?}
          inc(i)                                                                  {    Ready to match next RxHandshake bit}
        else
          begin                                                                   {  Else (bits don't match)}
          dec(FRxBuffStart, (i-1)*ord(r < 2));                                    {    Proper encoding (r < 2)?; start with 2nd bit checked and try again. Improper encoding (r > 1)?; may be junk prior to RxHandshake stream, ignore junk}
          i := 1;                                                                 {    Prep to match first RxHandshake bit}
          if RxCount > RxBuffSize then showmessage('Hardware Lost');              {    No RxHandshake in stream?  Time out; error}
          end;
      until (i > 250);                                                            {Loop until all RxHandshake bits received}
      {Receive version}
      for i := 1 to 8 do FVersion := FVersion shr 1 and $7F or ReceiveBit(False, 50) shl 7;
      if FVersionMode then
        begin {If version mode, send shutdown command and reset hardware to reboot}
        SetLength(TxBuf, 11);
        TxBuffLength := 0;
        AppendLong(0);
        XBee.SendUDP(TxBuf);
        GenerateResetSignal;
//        CloseComm;
        end;
        {Send download command immediately}
      SetLength(TxBuf, 11);
      TxBuffLength := 0;
      AppendLong(FDownloadMode);
      XBee.SendUDP(TxBuf);
      {If download command 1-3, do the following}
      if FDownloadMode > 0 then
      begin
//        {Update GUI - Loading RAM}
///        QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtLoadingRAM));
        {Send count and longs}
        SetLength(TxBuf, (1+FBinSize)*11);
        TxBuffLength := 0;
        AppendLong(FBinSize);
        for i := 0 to FBinSize-1 do
          begin
//          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtProgress));
          AppendLong(PIntegerArray(FBinImage)[i]);
          end;
        XBee.SendUDP(TxBuf);
//        {Update GUI - Verifying RAM}
//        QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtVerifyingRAM));
        {Receive ram checksum pass/fail}
        if ReceiveBit(True, 2500) = 1 then raise Exception.Create('RAM Checksum Error');//Error(ord(mtRAMChecksumError) + (strtoint(FComPort) shl 16));
        {If download command 2-3, do the following}
        if FDownloadMode > 1 then
        begin
//          {Update GUI - Programming EEPROM}
//          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtProgrammingEEPROM));
          {Receive eeprom program pass/fail}
          if ReceiveBit(True, 5000) = 1 then raise Exception.Create('EEPROM Programming Error');//Error(ord(mtEEPROMProgrammingError) + (strtoint(FComPort) shl 16));
//          {Update GUI - Verifying EEPROM}
//          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtVerifyingEEPROM));
          {Receive eeprom verify pass/fail}
          if ReceiveBit(True, 2500) = 1 then raise Exception.Create('EEPROM Verify Error');//Error(ord(mtEEPROMVerifyError) + (strtoint(FComPort) shl 16));
        end;
      end;
//      CloseComm;

  finally
    XBee.DisconnectSerialUDP;
//    XBee.DisconnectTCP;
  end;
end;

Initialization
  getmem(Buffer, ImageLimit);

end.
