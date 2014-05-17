unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  XBeeWiFi,
  IdGlobal, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient, IdStack, FMX.Layouts, FMX.Memo,
  Time,
  Debug;

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
    PCPortLabel: TLabel;
    IPPort: TEdit;
    PortLabel: TLabel;
    IdentifyButton: TButton;
    NodeID: TEdit;
    PCPortCombo: TComboBox;
    IPAddr: TEdit;
    MACAddr: TEdit;
    OpenDialog: TOpenDialog;
    BroadcastAddress: TEdit;
    Label1: TLabel;
    Progress: TProgressBar;
    ProgressLabel: TLabel;
    StatusLabel: TLabel;
    ButtonLayout: TLayout;
    Button1: TButton;
    Button2: TButton;
    ResetPulseButton: TButton;
    LoadButton: TButton;
    TransmitButton: TButton;
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
    procedure GenerateLoaderPacket(PacketCount: Integer);
  public
    { Public declarations }
  end;

  EFileCorrupt  = class(Exception);     {File Corrupt exception}
  EDownload = class(Exception);         {Download protocol base exception class}
  ESoftDownload = class(EDownload);     {Soft download protocol error}
  EHardDownload = class(EDownload);     {Hard download protocol error; fatal}

var
  Form1             : TForm1;
  Time              : TTime;
  XBee              : TXBeeWiFi;
  XBeeList          : array of TXBee;   {Dynamic array of XBee Info records}
  TxBuf             : TIdBytes;         {Transmit packet (resized per packet)}
  RxBuf             : TIdBytes;         {Receive packet (resized on receive)}
  FBinImage         : PByteArray;       {A copy of the Propeller Application's binary image (used to generate the download stream)}
  FBinSize          : Integer;          {The size of FBinImage (in longs)}

const
  MinSerTimeout  = 100;
  SerTimeout     = 1000;
  AppTimeout     = 200;
  CSumUnknown    = $FFFFFFFF;          {Unknown checksum value}
  ImageLimit     = 32768;              {Max size of Propeller Application image file}

  InitialBaud    = 115200;             {Initial XBee-to-Propeller baud rate}
  FinalBaud      = 230400;             {Final XBee-to-Propeller baud rate}

  {The RxHandshake array consists of 125 bytes encoded to represent the expected 250-bit (125-byte @ 2 bits/byte) response
  of continuing-LFSR stream bits from the Propeller, prompted by the timing templates following the TxHandshake stream.}
  RxHandshake : array[0..124] of byte = ($EE,$CE,$CE,$CF,$EF,$CF,$EE,$EF,$CF,$CF,$EF,$EF,$CF,$CE,$EF,$CF,
                                         $EE,$EE,$CE,$EE,$EF,$CF,$CE,$EE,$CE,$CF,$EE,$EE,$EF,$CF,$EE,$CE,
                                         $EE,$CE,$EE,$CF,$EF,$EE,$EF,$CE,$EE,$EE,$CF,$EE,$CF,$EE,$EE,$CF,
                                         $EF,$CE,$CF,$EE,$EF,$EE,$EE,$EE,$EE,$EF,$EE,$CF,$CF,$EF,$EE,$CE,
                                         $EF,$EF,$EF,$EF,$CE,$EF,$EE,$EF,$CF,$EF,$CF,$CF,$CE,$CE,$CE,$CF,
                                         $CF,$EF,$CE,$EE,$CF,$EE,$EF,$CE,$CE,$CE,$EF,$EF,$CF,$CF,$EE,$EE,
                                         $EE,$CE,$CF,$CE,$CE,$CF,$CE,$EE,$EF,$EE,$EF,$EF,$CF,$EF,$CE,$CE,
                                         $EF,$CE,$EE,$CE,$EF,$CE,$CE,$EE,$CF,$CF,$CE,$CF,$CF);

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
  Time := TTime.Create;
  XBee := TXBeeWiFi.Create;
  XBee.SerialTimeout := SerTimeout;
  XBee.ApplicationTimeout := AppTimeout;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  XBee.Destroy;
  Time.Destroy;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
//var
//  Idx : Integer;
//  IP : TIDStack;
begin
//  for Idx := 0 to GStack.LocalAddresses.Count-1 do showmessage(GStack.LocalAddresses[Idx]);
  XBee.SetItem(xbIO2State, pinOutHigh);
//  caption := inttostr(XBee.UDPRoundTrip);
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
  XBee.RemoteIPAddr := ifthen(BroadcastAddress.Text <> '', BroadcastAddress.Text, '192.168.1.255');
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
    if PCPortCombo.Count > 0 then
      begin
      PCPortCombo.Enabled := True;
      PCPortCombo.DropDown;
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.TransmitButtonClick(Sender: TObject);
var
  i                : Integer;
  r                : Byte;
  TxBuffLength     : Integer;

  RxCount          : Integer;
  FVersion         : Byte;
  FVersionMode     : Boolean;
  FDownloadMode    : Byte;

  Checksum         : Integer;                            {Target Propeller Application's checksum (low byte = 0)}

  TotalPackets     : Integer;                            {Total number of image packets}
  PacketID         : Integer;                            {ID of packet transmitted}
  Retry            : Integer;                            {Retry counter}
  RemainingTxTime  : Cardinal;                           {Holds remaining time to transmit packet}
  Acknowledged     : Boolean;                            {True = positive/negative acknowledgement received from loader, False = no response from loader}

  STime            : Int64;

const
  pReset = Integer.MinValue;

   {----------------}

   function Long(Addr: TIdBytes): Cardinal;
   {Returns four bytes starting at Addr as a single cardinal value (Long)}
   begin
     Result := (Addr[3] shl 24) + (Addr[2] shl 16) or (Addr[1] shl 8) or Addr[0];
   end;

   {----------------}

   function DynamicSerTimeout: Integer;
   {Returns serial timeout adjusted for recent communication delays; minimum MinSerTimeout ms, maximum SerTimeout ms}
   begin
     Result := Max(MinSerTimeout, Min(XBee.UDPMaxRoundTrip*3, SerTimeout));
     SendDebugMessage('          - MaxRoundTrip: ' + XBee.UDPMaxRoundTrip.ToString+ ' DynamicSerTimeout: ' + Result.ToString, True);
   end;

   {----------------}

   procedure UpdateProgress(Offset: Integer; Status: String = ''; Show: Boolean = True);
   {Update progress bar.}
   begin
     if Offset > 0 then
       begin
       if Progress.Tag = 0 then
         begin
         Progress.Opacity := 1;
         Progress.Value := Progress.Value + Offset;
         end
       else
         Progress.Tag := Min(0, Progress.Tag + Offset);
       end
     else
       begin
       if Offset = Integer.MinValue then Offset := -Trunc(Progress.Value);
       if Offset < 0 then
         begin
         Progress.Tag := Offset;
         Progress.Opacity := 0.5;
         end;
       end;
     if Status <> '' then StatusLabel.Text := Status;
     Progress.Visible := Show;
     Application.ProcessMessages;
//     SendDebugMessage('Progress Updated: ' + Trunc(Progress.Value).ToString + ' of ' + Trunc(Progress.Max).ToString, True);
   end;

   {----------------}

   procedure InitializeProgress(MaxIndex: Cardinal);
   {Initialize Progress Bar}
   begin
     Progress.Value := 0;
     Progress.Max := MaxIndex;
     Progress.Tag := 0;
     StatusLabel.Text := '';
     UpdateProgress(0);
   end;

   {----------------}

   function TransmitPacket: Integer;
   {Transmit (and retransmit if necessary) the packet in TxBuf, waiting for non-retransmit response or timeout.
    Returns response value (if any), raises exception otherwise.}
   var
     Retry : Integer;
   begin
     Retry := 3;
     repeat {(Re)Transmit packet}                                                           {  Send application image packet, get acknowledgement, retransmit as necessary}
       if Retry < 3 then UpdateProgress(-1);

       UpdateProgress(+1, 'Sending packet: ' + (TotalPackets-PacketID+1).ToString + ' of ' + TotalPackets.ToString);

       SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' Transmitting packet ' + PacketID.ToString, True);

       Time.Left(Trunc((TxBuffLength*4*10/FinalBaud)*1000));                                {    Mark required Tx time}
       if not XBee.SendUDP(TxBuf, True, False) then
         raise EHardDownload.Create('Error: Can not transmit packet!');

       SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for packet acknowledgement', True);

       Acknowledged := XBee.ReceiveUDP(RxBuf, DynamicSerTimeout) and (Length(RxBuf) = 4);   {    Wait for positive/negative acknowledgement, or timeout}
       RemainingTxTime := Time.Left;                                                        {    Check remaining time to transmit (should be 0 ms)}
       if RemainingTxTime > 0 then SendDebugMessage('          - ERROR: Remaining Tx Time: ' + RemainingTxTime.ToString, True);
       dec(Retry);                                                                          {  Loop and retransmit until timely non-retransmit acknowledgement received, or retry count exhausted}
     {Repeat - (Re)Transmit packet...}
     { TODO : Revisit phase variance timing trap }
     until ( Acknowledged and (RemainingTxTime = 0) and (Long(@RxBuf[0]) <> Long(@TxBuf[4])) ) or (Retry = 0);
     if not ( Acknowledged and (RemainingTxTime = 0) and (Long(@RxBuf[0]) <> Long(@TxBuf[4])) ) then
       raise EHardDownload.Create('Error: connection lost!');                               {  No acknowledgement received? Error}
     Result := Long(@RxBuf[0]);
   end;

   {----------------}
begin
  try {Handle download errors}
    if FBinSize = 0 then exit;

    TransmitButton.Enabled := False;

    FVersionMode := False;
    FDownloadMode := 1; {The download command; 1 = write to RAM and run, 2 = write to EEPROM and stop, 3 = write to EEPROM and run}

    try {Reserved Memory}

      STime := Ticks;
  
      {Determine number of required packets for target application image; value becomes first Packet ID}
      SetRoundMode(rmUp);
      TotalPackets := Round(FBinSize*4 / (XBee.MaxDataSize-4*2));                                         {Calculate required number of packets for target image; binary image size (in bytes) / (max packet size - packet header)}
      PacketID := TotalPackets;
      {Calculate target application checksum (used for RAM Checksum confirmation)}
      Checksum := 0;
      for i := 0 to FBinSize*4-1 do inc(Checksum, FBinImage[i]);
      for i := 0 to high(InitCallFrame) do inc(Checksum, InitCallFrame[i]);

      {Initialize Progress Bar to proper size}
      InitializeProgress(9 + TotalPackets);

      {Begin download process}
      if not XBee.ConnectSerialUDP then
        begin
        showmessage('Cannot connect');
        exit;
        end;

      try {UDP Connected}
        Retry := 3;
        repeat {Connecting Propeller}                                                                     {Try connecting up to 3 times}
          UpdateProgress(pReset);

          {Generate initial packet (handshake, timing templates, and Propeller Loader's Download Stream) all stored in TxBuf}
          GenerateLoaderPacket(TotalPackets);

          SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Connecting...', True);

          try {Connecting...}
            {(Enforce XBee Configuration and...) Generate reset signal, then wait for serial transfer window}
            UpdateProgress(0, 'Generating reset signal');
            GenerateResetSignal;
            IndySleep(190);

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Sending handshake and loader image', True);

            {Send initial packet and wait for serial transfer time + 120 ms}
            UpdateProgress(+1, 'Connecting');
            if not XBee.SendUDP(TxBuf, True, False) then                                                  {Send Connect and Loader packet}
              raise EHardDownload.Create('Error: Can not send connection request!');
            IndySleep(Length(TxBuf)*10 div InitialBaud + 120);

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Sending timing templates', True);

            {Prep and send timing templates, then wait for serial transfer time}
            UpdateProgress(+1);
            SetLength(TxBuf, XBee.MaxDataSize);
            FillChar(TxBuf[0], XBee.MaxDataSize, $F9);
            if not XBee.SendUDP(TxBuf, True, False) then                                                  {Send timing template packet}
              raise EHardDownload.Create('Error: Can not request connection response!');
            IndySleep(Length(TxBuf)*10 div InitialBaud);

            { TODO : Revisit handshake receive loop to check for all possibilities and how they are handled. }
            repeat {Flush receive buffer and get handshake response}

              SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for handshake', True);

              if not XBee.ReceiveUDP(RxBuf, SerTimeout) then                                              {Receive response}
                raise ESoftDownload.Create('Error: No connection response from Propeller!');
              if Length(RxBuf) = 129 then                                                                 {Validate response}
                begin
                for i := 0 to 124 do if RxBuf[i] <> RxHandshake[i] then
                  raise EHardDownload.Create('Error: Unrecognized response - not a Propeller?');          {Validate handshake response}
                for i := 125 to 128 do FVersion := (FVersion shr 2 and $3F) or ((RxBuf[i] and $1) shl 6) or ((RxBuf[i] and $20) shl 2); {Parse hardware version}
                end;
            {Repeat - Flush receive buffer and get handshake response...}
            until Length(RxBuf) = 129;                                                                    {Loop if not correct (to flush receive buffer of previous data)}

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for RAM Checksum acknowledgement', True);

            {Receive RAM checksum response}
            UpdateProgress(+1);
            if not XBee.ReceiveUDP(RxBuf, DynamicSerTimeout) or (Length(RxBuf) <> 1) then                 {Receive Loader RAM Checksum Response}
              raise ESoftDownload.Create('Error: No loader checksum response!');
            if RxBuf[0] <> $FE then
              raise EHardDownload.Create('Error: Loader failed checksum test');

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for "Ready" signal', True);

            {Now loader starts up in the Propeller; wait for loader's "ready" signal}
            UpdateProgress(+1);
            Acknowledged := XBee.ReceiveUDP(RxBuf, DynamicSerTimeout);                                    {Receive loader's response}
            if not Acknowledged or (Length(RxBuf) <> 4) then                                              {Verify ready signal format}
              raise ESoftDownload.Create('Error: No "Ready" signal from loader!');
            if Cardinal(RxBuf[0]) <> PacketID then                                                        {Verify ready signal}
              raise EHardDownload.Create('Error: Loader''s "Ready" signal unrecognized!');
          except {on - Connecting...}
            {Error?  Repeat if possible on Soft error, else re-raise the exeption to exit}
            on E:ESoftDownload do
              begin
              Acknowledged := False;
              dec(Retry);
              if Retry = 0 then raise EHardDownload.Create(E.Message);
              end
            else
              raise;
          end;
        {repeat - Connecting Propeller...}
        until Acknowledged;

        SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Switching to final baud rate', True);

        {Switch to final baud rate}
        UpdateProgress(+1, 'Increasing connection speed');
        if not XBee.SetItem(xbSerialBaud, FinalBaud) then
          raise EHardDownload.Create('Error: Unable to increase connection speed!');

        {Transmit packetized target application}
        i := 0;
        repeat {Transmit target application packets}                                             {Transmit application image}
          TxBuffLength := 1 + Min((XBee.MaxDataSize div 4)-1, FBinSize - i);                     {  Determine packet length (in longs); header + packet limit or remaining data length}
          SetLength(TxBuf, TxBuffLength*4);                                                      {  Set buffer length (Packet Length) (in longs)}
          Move(PacketID, TxBuf[0], 4);                                                           {  Store Packet ID}
          Move(FBinImage[i*4], TxBuf[4], (TxBuffLength-1)*4);                                    {  Store section of data}
          if TransmitPacket <> PacketID-1 then                                                   {  Transmit packet (retransmit as necessary)}
            raise EHardDownload.Create('Error: communication failed!');                          {    Error if unexpected response}
          inc(i, TxBuffLength-1);                                                                {  Increment image index}
          dec(PacketID);                                                                         {  Decrement Packet ID (to next packet)}
        {repeat - Transmit target application packets...}
        until PacketID = 0;                                                                      {Loop until done}

        SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for RAM checksum', True);
        UpdateProgress(+1, 'Verifying RAM');

        {Send verify RAM command}                                                                {Verify RAM Checksum}
        TxBuffLength := 1;                                                                       {Set length for empty payload}
        SetLength(TxBuf, TxBuffLength*4);                                                        {Set buffer length (Packet Length) (in longs)}
        Move(PacketID, TxBuf[0], 4);                                                             {Store Packet ID}
        if TransmitPacket <> -Checksum then                                                      {Transmit packet (retransmit as necessary)}
          raise EHardDownload.Create('Error: RAM Checksum Failure!');                            {  Error if RAM Checksum differs}
        PacketID := -Checksum;                                                                   {Ready next packet; ID's by checksum now }

        SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Launching application (step 1)', True);
        UpdateProgress(+1, 'Launching application (step 1)');

        {Send verified/launch command}                                                           {Verified/Launch}
        TxBuffLength := 1;                                                                       {Set length for empty payload}
        SetLength(TxBuf, TxBuffLength*4);                                                        {Set buffer length (Packet Length) (in longs)}
        Move(PacketID, TxBuf[0], 4);                                                             {Store Packet ID}
        if TransmitPacket <> PacketID-1 then                                                     {Transmit packet (retransmit as necessary)}
          raise EHardDownload.Create('Error: communication failed!');                            {  Error if unexpected response}
        dec(PacketID);                                                                           {Ready next packet}

        SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Launching application (step 2)', True);
        UpdateProgress(+1, 'Launching application (step 2)');

        {Send launch command}                                                                    {Verified}
        TxBuffLength := 1;                                                                       {Set length for empty payload}
        SetLength(TxBuf, TxBuffLength*4);                                                        {Set buffer length (Packet Length) (in longs)}
        Move(PacketID, TxBuf[0], 4);                                                             {Store Packet ID}
        XBee.SendUDP(TxBuf, True, False);                                                        {Transmit last packet (Launch step 2); no retransmission}
        UpdateProgress(+1, 'Success');

      finally {UDP Connected}
        XBee.DisconnectSerialUDP;
      end;
    finally {Reserved Memory}

      SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Exiting', True);
      IndySleep(500);
      UpdateProgress(0, '', False);

      TransmitButton.Enabled := True;
    end;
  except {on - Handle download errors}
    on E:EDownload do ShowMessage(E.Message);
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
        raise EFileCorrupt.Create(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is truncated or is not a Propeller Application' + ifthen(Filename <> '',' file', '') + '!'+#$D#$A#$D#$A+ifthen(Filename <> '', 'File', 'Image') + ' size is less than 16 bytes or is less than word 4 (VarBase) indicates.');
      if (PWordArray(Buffer)[3] <> $0010) then
        raise EFileCorrupt.Create('Initialization code invalid!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Word 3 (CodeBase) must be $0010.');
      {Write initial call frame}
//      copymemory(@Buffer[min($7FF8, max($0010, PWordArray(Buffer)[5] - 8))], @InitCallFrame[0], 8);
      move(InitCallFrame[0], Buffer[min($7FF8, max($0010, PWordArray(Buffer)[5] - 8))], 8);
      {Raise exception if file's checksum incorrect}
      CheckSum := 0;
      for Idx := 0 to ImageLimit-1 do CheckSum := CheckSum + Buffer[Idx];
      if CheckSum <> 0 then
        raise EFileCorrupt.Create('Checksum Error!  ' + ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' is corrupt or is not a Propeller Application' + ifthen(Filename <> '',' file', '') +'!'+#$D#$A#$D#$A+'Byte 5 (Checksum) is incorrect.');
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
          raise EFileCorrupt.Create(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' contains data after code space' + {$IFNDEF ISLIB}' that was not generated by the Propeller Tool' +{$ENDIF} '.  This data will not be displayed or downloaded.');
//          messagedlg(ifthen(Filename <> '', 'File '''+Filename+'''', 'Image') + ' contains data after code space' + {$IFNDEF ISLIB}' that was not generated by the Propeller Tool'{$ENDIF}+ '.  This data will not be displayed or downloaded.', mtWarning, [mbOK], 0);
//          end;
        end;
    end;

    {----------------}

begin
  {Process file/image}
  {Set open dialog parameters}
  OpenDialog.Title := 'Download Propeller Application Image';
  OpenDialog.Filter := 'Propeller Applications (*.binary, *.eeprom)|*.binary;*.eeprom|All Files (*.*)|*.*';
  OpenDialog.FilterIndex := 0;
  OpenDialog.FileName := '';
  {Show Open Dialog}
  if not OpenDialog.Execute then exit;
  FName := OpenDialog.Filename;
  if fileexists(FName) then
    begin {File found, load it up}
    {Initialize}
    ImageSize := 0;
    fillchar(FBinImage[0], ImageLimit, 0);
    {Load the file}
    FStream := TFileStream.Create(FName, fmOpenRead+fmShareDenyWrite);
    try
      ImageSize := FStream.Read(FBinImage^, ImageLimit);
    finally
      FStream.Free;
    end;
    end;
  try
    {Validate application image (and install initial call frame)}
    ValidateImageDataIntegrity(FBinImage, min(ImageLimit, ImageSize), FName);
    FBinSize := ImageSize div 4;
    {Download image to Propeller chip (use VBase (word 4) value as the 'image long-count size')}
//    Propeller.Download(Buffer, PWordArray(Buffer)[4] div 4, DownloadCmd);
    TransmitButton.Enabled := True;
  except
    on E: EFileCorrupt do
      begin {Image corrupt, show error and exit}
//      if (CPrefs[GUIDisplay].IValue in [0, 2]) then
//        begin
          TransmitButton.Enabled := False;
          ShowMessage(E.Message);
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
  ButtonLayout.Enabled := True;
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
  if EnforceXBeeConfiguration then
    if not XBee.SetItem(xbOutputState, $0000) then            {Start reset pulse}
      raise Exception.Create('Error Generating Reset Signal');
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
    Validate(xbSerialIP, SerialUDP, False);                                                  {    Ensure XBee's Serial Service uses UDP packets [WRITE DISABLED DUE TO FIRMWARE BUG]}
    { TODO : Figure out how to set xbIPDestination. }
    Validate(xbIPDestination, $C0A80188);                                                    {    Ensure Serial-to-IP destination is us (our IP)}
    Validate(xbIO2State, pinOutHigh);                                                        {    Ensure I/O is set to output high}
    Validate(xbOutputMask, $7FFF);                                                           {    Ensure output mask is proper (default, in this case)}
    Validate(xbIO2Timer, 1);                                                                 {    Ensure DIO2's timer is set to 100 ms}
    Validate(xbSerialMode, TransparentMode {APIwoEscapeMode} {APIwEscapeMode}, False);       {    Ensure Serial Mode is transparent [WRITE DISABLED DUE TO FIRMWARE BUG]}
    Validate(xbSerialBaud, Baud115200);                                                      {    Ensure baud rate is 115,200 bps}
    Validate(xbSerialParity, ParityNone);                                                    {    Ensure parity is none}
    Validate(xbSerialStopBits, StopBits1);                                                   {    Ensure stop bits is 1}
    XBee.GetItem(xbChecksum, XBeeList[PCPortCombo.Selected.Index].CfgChecksum);              {    Record new configuration checksum}
    Result := True;
    end;
end;

{--------------------------------------------------------------------------------}

procedure TForm1.GenerateLoaderPacket(PacketCount: Integer);
{Generate a single packet that contains the standard Propeller handshake, timing templates, and Propeller Loader Image stream, encoded in an optimized
 format (3, 4, or 5 bits per byte; 7 to 11 bytes per long).

 Note: for optimal encoding, every 5 contiguous bits in Propeller Application Image
 (LSB first) 3, 4, or 5 bits can be translated to a byte.  The process requires 5 bits input (ie: indexed into the PDSTx array) and gets a byte out that
 contains the first 3, 4, or 5 bits encoded in the Propeller Download stream format. If less than 5 bits were translated, the remaining bits lead the
 next 5-bit translation unit input to the translation process.}
var
  Idx              : Integer;      {General index value}
  BValue           : Byte;         {Binary Value to translate}
  BitsIn           : Byte;         {Number of bits ready for translation}
  BCount           : Integer;      {Total number of bits translated}
  LoaderImage      : PByteArray;   {Adjusted Loader Memory image}
  LoaderStream     : PByteArray;   {Loader Download Stream (Generated from Loader Image inside GenerateStream())}
  LoaderStreamSize : Integer;      {Size of Loader Download Stream}
  Checksum         : Integer;      {Loader application's checksum (low byte = 0)}

  TxBuffLength     : Integer;
  RawSize          : Integer;

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

  {After reset, the Propeller's exact clock rate is not known by either the host or the Propeller itself, so communication with the Propeller takes place based on
   a host-transmitted timing template that the Propeller uses to read the stream and generate the responses.  The host first transmits the 2-bit timing template,
   then transmits a 250-bit Tx handshake, followed by 250 timing templates (one for each Rx handshake bit expected) which the Propeller uses to properly transmit
   the Rx handshake sequence.  Finally, the host transmits another eight timing templates (one for each bit of the Propeller's version number expected) which the
   Propeller uses to properly transmit it's 8-bit hardware/firmware version number.

   After the Tx Handshake and Rx Handshake are properly exchanged, the host and Propeller are considered "connected," at which point the host can send a download
   command followed by image size and image data, or simply end the communication.

   PROPELLER HANDSHAKE SEQUENCE: The handshake (both Tx and Rx) are based on a Linear Feedback Shift Register (LFSR) tap sequence that repeats only after 255
   iterations.  The generating LFSR can be created in Pascal code as the following function (assuming FLFSR is pre-defined Byte variable that is set to ord('P')
   prior to the first call of IterateLFSR).  This is the exact function that was used in previous versions of the Propeller Tool and Propellent software.

    function IterateLFSR: Byte;
    begin //Iterate LFSR, return previous bit 0
      Result := FLFSR and $01;
      FLFSR := FLFSR shl 1 and $FE or (FLFSR shr 7 xor FLFSR shr 5 xor FLFSR shr 4 xor FLFSR shr 1) and 1;
    end;

   The handshake bit stream consists of the lowest bit value of each 8-bit result of the LFSR described above.  This LFSR has a domain of 255 combinations, but
   the host only transmits the first 250 bits of the pattern, afterwards, the Propeller generates and transmits the next 250-bits based on continuing with the same
   LFSR sequence.  In this way, the host-transmitted (host-generated) stream ends 5 bits before the LFSR starts repeating the initial sequence, and the host-received
   (Propeller generated) stream that follows begins with those remaining 5 bits and ends with the leading 245 bits of the host-transmitted stream.

   For speed and compression reasons, this handshake stream has been encoded as tightly as possible into the pattern described below.

   The TxHandshake array consists of 209 bytes that are encoded to represent the required '1' and '0' timing template bits, 250 bits representing the
   lowest bit values of 250 iterations of the Propeller LFSR (seeded with ASCII 'P').}
  TxHandshake : array[0..208] of byte = ($49,                                                              {First timing template ('1' and '0') plus first two bits of handshake ('0' and '1')}
                                         $AA,$52,$A5,$AA,$25,$AA,$D2,$CA,$52,$25,$D2,$D2,$D2,$AA,$49,$92,  {Remaining 248 bits of handshake...}
                                         $C9,$2A,$A5,$25,$4A,$49,$49,$2A,$25,$49,$A5,$4A,$AA,$2A,$A9,$CA,
                                         $AA,$55,$52,$AA,$A9,$29,$92,$92,$29,$25,$2A,$AA,$92,$92,$55,$CA,
                                         $4A,$CA,$CA,$92,$CA,$92,$95,$55,$A9,$92,$2A,$D2,$52,$92,$52,$CA,
                                         $D2,$CA,$2A,$FF,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,  {250 timing templates ('1' and '0') to receive 250-bit handshake from Propeller.}
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,  {This is encoded as two pairs per byte; 125 bytes}
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,
                                         $29,$29,$29,$29,                                                  {8 timing templates ('1' and '0') to receive 8-bit Propeller version; two pairs per byte; 4 bytes}
                                         $93,$92,$92,$92,$92,$92,$92,$92,$92,$92,$F2);                     {Download command (1; program RAM and run); 11 bytes}

  {Raw loader image.  This is a memory image of a Propeller Application written in PASM that fits into our initial download packet.  Once started,
  it assists with the remainder of the download (at a faster speed and with more relaxed interstitial timing conducive of Internet Protocol delivery.
  This memory image isn't used as-is; before download, it is first adjusted to contain special values assigned by this host (communication timing and
  synchronization values) and then is translated into an optimized Propeller Download Stream understandable by the Propeller ROM-based boot loader.}
  RawLoaderAppSize = 116;
  RawLoaderImage : array[0..463] of byte = ($00,$B4,$C4,$04,$6F,$06,$10,$00,$D0,$01,$D8,$01,$C8,$01,$DC,$01,
                                            $C0,$01,$02,$00,$B8,$01,$00,$00,$65,$E8,$BF,$A0,$65,$EC,$BF,$A0,
                                            $01,$E8,$FF,$68,$01,$EC,$FF,$68,$66,$DA,$BC,$A1,$01,$DA,$FC,$28,
                                            $F1,$DB,$BC,$80,$A0,$D8,$CC,$A0,$66,$DA,$BC,$F8,$F2,$C9,$3C,$61,
                                            $07,$D8,$FC,$E4,$04,$DE,$FC,$A0,$6B,$DC,$BC,$A0,$08,$D6,$FC,$20,
                                            $FF,$DC,$FC,$60,$00,$DD,$FC,$68,$01,$DC,$FC,$2C,$66,$DA,$BC,$A0,
                                            $F1,$DB,$BC,$80,$01,$DC,$FC,$29,$66,$DA,$BC,$F8,$65,$E8,$BF,$70,
                                            $13,$DC,$7C,$E8,$0C,$DE,$FC,$E4,$67,$CC,$BC,$A0,$69,$40,$FC,$50,
                                            $71,$E0,$FC,$A0,$70,$3E,$BC,$54,$70,$5C,$BC,$54,$70,$5E,$BC,$54,
                                            $04,$DE,$FC,$A0,$00,$E2,$FC,$A0,$69,$D8,$BC,$A0,$68,$DA,$BC,$A1,
                                            $00,$DC,$FC,$A0,$80,$DC,$FC,$72,$F2,$C9,$3C,$61,$23,$D8,$F8,$E4,
                                            $34,$00,$78,$5C,$F1,$DB,$BC,$80,$66,$DA,$BC,$F8,$01,$E8,$FF,$6C,
                                            $F2,$C9,$3C,$61,$00,$DD,$FC,$70,$01,$DC,$FC,$29,$28,$00,$4C,$5C,
                                            $6E,$E2,$BC,$68,$08,$E2,$FC,$20,$6A,$40,$FC,$50,$20,$DE,$FC,$E4,
                                            $01,$E0,$FC,$80,$1B,$00,$7C,$5C,$20,$D8,$BC,$A0,$FF,$D9,$FC,$60,
                                            $69,$D8,$7C,$87,$00,$BE,$68,$0C,$6B,$E2,$3C,$C2,$01,$D6,$E8,$C1,
                                            $72,$E0,$C8,$84,$5E,$E4,$08,$08,$04,$BC,$C8,$80,$60,$76,$88,$80,
                                            $3B,$E0,$C8,$E4,$72,$76,$C8,$54,$0B,$00,$4C,$5C,$01,$D6,$FC,$82,
                                            $54,$00,$54,$5C,$61,$BA,$BC,$A0,$5E,$BA,$BC,$84,$02,$BA,$FC,$2A,
                                            $5E,$DE,$14,$08,$04,$BC,$D4,$80,$46,$BA,$D4,$E4,$0A,$BA,$FC,$04,
                                            $04,$BA,$FC,$84,$5D,$C4,$3C,$08,$04,$BA,$FC,$84,$5D,$C4,$3C,$08,
                                            $01,$BC,$FC,$84,$5E,$DE,$BC,$00,$6F,$D6,$BC,$80,$4E,$BC,$7C,$E8,
                                            $6B,$D6,$BC,$A4,$0B,$00,$7C,$5C,$01,$D6,$FC,$84,$B8,$6E,$FC,$58,
                                            $54,$6E,$FC,$50,$C0,$B0,$FC,$58,$0B,$00,$7C,$5C,$06,$BC,$FC,$04,
                                            $10,$BC,$7C,$86,$00,$BE,$54,$0C,$02,$C6,$7C,$0C,$00,$00,$00,$00,
                                            $00,$00,$00,$00,$80,$00,$00,$00,$00,$02,$00,$00,$00,$80,$00,$00,
                                            $FF,$FF,$F9,$FF,$10,$C0,$07,$00,$00,$00,$00,$80,$00,$00,$00,$40,
                                            $B6,$02,$00,$00,$5B,$01,$00,$00,$08,$02,$00,$00,$55,$73,$CB,$00,
                                            $50,$45,$01,$00,$00,$00,$00,$00,$35,$C7,$08,$35,$2C,$32,$00,$00);

  RawLoaderInitOffset = -8*4;             {Offset (in bytes) from end of Loader Image pointing to where most host-initialized values exist.
                                          Host-Initialized values are: Initial Bit Time, Final Bit Time, 1.5x Bit Time, Failsafe timeout,
                                          End of Packet timeout, and ExpectedID.  In addition, the image checksum at word 5 needs to be
                                          updated.  All these values need to be updated before the download stream is generated.}

  InitCallFrame : array [0..7] of byte = ($FF, $FF, $F9, $FF, $FF, $FF, $F9, $FF);

//  {The TxHandshake array consists of 250 bytes representing the bit 0 values (0 or 1) of each of 250 iterations of the LFSR (seeded with ASCII 'P') described above.}
{  TxHandshake : array[1..250] of byte = (0,1,0,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,1,0,0,0,1,0,0,0,
                                         1,1,0,1,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,1,0,1,0,1,1,0,1,0,0,1,1,1,1,1,
                                         1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,1,0,1,0,1,0,0,1,
                                         0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,1,1,1,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,0,0,1,0,1,0,0,1,1,0,1);
}
//  {The RxHandshake array consists of 250 bytes representing the bit 0 values (0 or 1) of each of 250 successive iterations of the LFSR of TxHandshake, above.}
{  RxHandshake : array[1..250] of byte = (0,1,0,0,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,1,0,0,
                                         0,1,0,0,0,1,1,0,1,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,1,0,1,0,1,1,0,1,0,0,
                                         1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,1,0,1,
                                         0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,1,1,1,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,0,0,1,0,1,0);
}

    {----------------}

    procedure SetHostInitializedValue(Addr: Integer; Value: Integer);
    {Adjust LoaderImage to contain Value (long) at Addr}
    var
      Idx : Integer;
    begin
      for Idx := 0 to 3 do LoaderImage[Addr+Idx] := Value shr (Idx*8) and $FF;
    end;

    {----------------}

begin
  {Reserve memory for Raw Loader Image}
  getmem(LoaderImage, RawLoaderAppSize*4+1);                                                                      {Reserve LoaderImage space for RawLoaderImage data plus 1 extra byte to accommodate generation routine}
  getmem(LoaderStream, ImageLimit div 4 * 11);                                                                    {Reserve LoaderStream space for maximum-sized download stream}
  try {Reserved memory}
    {Prepare Loader Image}
    Move(RawLoaderImage, LoaderImage[0], RawLoaderAppSize*4);                                                     {Copy raw loader image to LoaderImage (for adjustments and processing)}
    {Clear checksum and set host-initialized values}
    LoaderImage[5] := 0;
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset, 80000000 div InitialBaud);                    {Initial Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 4, 80000000 div FinalBaud);                  {Final Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 8, trunc(1.5 * 80000000) div FinalBaud);     {1.5x Final Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 12, 2 * 80000000 div (3 * 4));               {Failsafe Timeout (seconds-worth of Loader's Receive loop iterations)}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 16, trunc(2 * 80000000 / 230400 * 10 / 12)); {EndOfPacket Timeout (2 bytes worth of Loader's Receive loop iterations)}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 20, PacketCount);                            {First Expected Packet ID; total packet count}
    {Recalculate and update checksum}
    Checksum := 0;
    for Idx := 0 to RawLoaderAppSize*4-1 do inc(Checksum, LoaderImage[Idx]);
    for Idx := 0 to high(InitCallFrame) do inc(Checksum, InitCallFrame[Idx]);
    LoaderImage[5] := 256-(CheckSum and $FF);                                                                     {Update loader image so low byte of checksum calculates to 0}
    {Generate Propeller Loader Download Stream from adjusted LoaderImage (above); Output delivered to LoaderStream and LoaderStreamSize}
    BCount := 0;
    LoaderStreamSize := 0;
    while BCount < (RawLoaderAppSize*4) * 8 do                                                                    {For all bits in data stream...}
      begin
        BitsIn := Min(5, (RawLoaderAppSize*4) * 8 - BCount);                                                      {  Determine number of bits in current unit to translate; usually 5 bits}
        BValue := ( (LoaderImage[BCount div 8] shr (BCount mod 8)) +                                              {  Extract next translation unit (contiguous bits, LSB first; usually 5 bits)}
          (LoaderImage[(BCount div 8) + 1] shl (8 - (BCount mod 8))) ) and Pwr2m1[BitsIn];
        LoaderStream[LoaderStreamSize] := PDSTx[BValue, BitsIn, dtTx];                                            {  Translate unit to encoded byte}
        inc(LoaderStreamSize);                                                                                    {  Increment byte index}
        inc(BCount, PDSTx[BValue, BitsIn, dtBits]);                                                               {  Increment bit index (usually 3, 4, or 5 bits, but can be 1 or 2 at end of stream)}
      end;
    {Prepare loader packet; contains handshake and Loader Stream.}
    SetLength(TxBuf, Length(TxHandshake)+11+LoaderStreamSize);                                                    {Set packet size}

    SendDebugMessage('**** INITIAL PACKET SIZE : ' + Length(TxBuf).ToString + ' BYTES ****', True);

    if Length(TxBuf) > XBee.MaxDataSize then
      raise EHardDownload.Create('Developer Error: Initial packet is too large (' + Length(TxBuf).ToString + ' bytes)!');
    Move(TxHandshake, TxBuf[0], Length(TxHandshake));                                                             {Fill packet with handshake stream (timing template, handshake, and download command (RAM+Run))}

    TxBuffLength := Length(TxHandshake);                                                                          {followed by Raw Loader Images's App size (in longs)}
    RawSize := RawLoaderAppSize;
    for Idx := 0 to 10 do
      begin
      TxBuf[TxBuffLength] := $92 or -Ord(Idx=10) and $60 or RawSize and 1 or RawSize and 2 shl 2 or RawSize and 4 shl 4;
      Inc(TxBuffLength);
      RawSize := RawSize shr 3;
      end;

    Move(LoaderStream[0], TxBuf[TxBuffLength], LoaderStreamSize);                                                 {and the Loader Stream image itself}

  finally {Reserved memory}
    freemem(LoaderImage);
    freemem(LoaderStream);
  end;
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  getmem(FBinImage, ImageLimit);
  FBinSize := 0;

Finalization
  freemem(FBinImage);

end.
