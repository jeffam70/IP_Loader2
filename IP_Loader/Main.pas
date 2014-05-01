unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  XBeeWiFi,
  IdGlobal, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient, IdStack;

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
    BroadcastAddress: TEdit;
    Label1: TLabel;
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
    procedure GenerateStream(InImage: PByteArray; InSize: Integer; OutImage: PByteArray; var OutSize: Integer);
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
  caption := inttostr(XBee.UDPRoundTrip);
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
    if PCPortCombo.Count > 0 then PCPortCombo.DropDown;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.TransmitButtonClick(Sender: TObject);
var
  i                : Integer;
  r                : Byte;
  TxBuffLength     : Integer;

  RxCount          : Integer;
  FRxBuffStart     : Cardinal;
  RxBuffSize       : Cardinal;
  FRxBuffEnd       : Cardinal;
  FVersion         : Byte;
  FVersionMode     : Boolean;
  FDownloadMode    : Byte;

  Checksum         : Byte;
  LoaderImage      : PByteArray;                         {Adjusted Loader Memory image}
  LoaderStream     : PByteArray;                         {Loader Download Stream (Generated from Loader Image)}
  LoaderStreamSize : Integer;                            {Size of Loader Download Stream}

  PacketID         : Integer;                            {ID of packet transmitted}

const
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

  {The RxHandshake array consists of 125 bytes that are encoded to represent the expected 250 bit continuing LFSR stream started by the TxHandshake; 2-bits per byte.}
  RxHandshake : array[0..124] of byte = ($EE,$CE,$CE,$CF,$EF,$CF,$EE,$EF,$CF,$CF,$EF,$EF,$CF,$CE,$EF,$CF,
                                         $EE,$EE,$CE,$EE,$EF,$CF,$CE,$EE,$CE,$CF,$EE,$EE,$EF,$CF,$EE,$CE,
                                         $EE,$CE,$EE,$CF,$EF,$EE,$EF,$CE,$EE,$EE,$CF,$EE,$CF,$EE,$EE,$CF,
                                         $EF,$CE,$CF,$EE,$EF,$EE,$EE,$EE,$EE,$EF,$EE,$CF,$CF,$EF,$EE,$CE,
                                         $EF,$EF,$EF,$EF,$CE,$EF,$EE,$EF,$CF,$EF,$CF,$CF,$CE,$CE,$CE,$CF,
                                         $CF,$EF,$CE,$EE,$CF,$EE,$EF,$CE,$CE,$CE,$EF,$EF,$CF,$CF,$EE,$EE,
                                         $EE,$CE,$CF,$CE,$CE,$CF,$CE,$EE,$EF,$EE,$EF,$EF,$CF,$EF,$CE,$CE,
                                         $EF,$CE,$EE,$CE,$EF,$CE,$CE,$EE,$CF,$CF,$CE,$CF,$CF);

  {Raw loader image.  This is a memory image of a Propeller Application written in PASM that fits into our initial download packet.  Once started,
  it assists with the remainder of the download (at a faster speed and with more relaxed interstitial timing conducive of Internet Protocol delivery.
  This memory image isn't used as-is; before download, it is first adjusted to contain special values assigned by this host (communication timing and
  synchronization values) and then is translated into an optimized Propeller Download Stream understandable by the Propeller ROM-based boot loader.}
  RawLoaderAppSize = 74;
  RawLoaderImage : array[0..295] of byte = ($00,$B4,$C4,$04,$6F,$BD,$10,$00,$28,$01,$30,$01,$20,$01,$34,$01,
                                            $18,$01,$02,$00,$10,$01,$00,$00,$3C,$E8,$BF,$A0,$3C,$EC,$BF,$A0,
                                            $15,$00,$7C,$5C,$48,$8E,$FC,$A0,$47,$10,$BC,$54,$47,$12,$BC,$54,
                                            $04,$8C,$FC,$A0,$27,$6C,$FC,$5C,$44,$90,$BC,$68,$08,$90,$FC,$20,
                                            $07,$8C,$FC,$E4,$01,$8E,$FC,$80,$01,$6E,$FC,$80,$04,$90,$FC,$E4,
                                            $41,$92,$3C,$86,$01,$82,$E8,$84,$38,$94,$28,$08,$04,$70,$E8,$80,
                                            $3A,$20,$A8,$80,$10,$6E,$E8,$E4,$4A,$20,$E8,$54,$04,$8C,$FC,$A0,
                                            $41,$88,$BC,$A0,$08,$82,$FC,$20,$1D,$4C,$FC,$5C,$16,$8C,$FC,$E4,
                                            $3E,$7A,$BC,$A0,$03,$82,$7C,$E8,$1C,$00,$7C,$5C,$00,$89,$FC,$68,
                                            $01,$88,$FC,$2C,$0A,$8A,$FC,$A0,$3D,$86,$BC,$A0,$F1,$87,$BC,$80,
                                            $01,$88,$FC,$29,$3D,$86,$BC,$F8,$3C,$E8,$BF,$70,$22,$8A,$FC,$E4,
                                            $00,$00,$7C,$5C,$40,$84,$BC,$A0,$3F,$86,$BC,$A0,$08,$8A,$FC,$A0,
                                            $F2,$77,$3C,$61,$2A,$84,$CC,$E4,$00,$72,$4C,$0C,$F2,$77,$3C,$61,
                                            $2D,$84,$F0,$E4,$00,$72,$70,$0C,$F1,$87,$BC,$80,$3D,$86,$BC,$F8,
                                            $F2,$77,$3C,$61,$01,$88,$FC,$28,$80,$88,$FC,$70,$31,$8A,$FC,$E4,
                                            $00,$00,$7C,$5C,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,
                                            $00,$02,$00,$00,$00,$00,$00,$80,$00,$00,$00,$40,$B6,$02,$00,$00,
                                            $5B,$01,$00,$00,$08,$02,$00,$00,$00,$2D,$31,$01,$00,$00,$00,$00,
                                            $35,$C7,$08,$35,$2C,$32,$00,$00);

  RawLoaderInitOffset = -(7*4);          {Offset (in bytes) from end of Loader Image pointing to where host-initialized values exist.
                                          Host-Initialized values are: Initial Bit Time, Final Bit Time, 1.5x Bit Time, Timeout, and
                                          ExpectedID (as well as image checksum).  They need to be updated before the download stream
                                          is generated.}

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
            SetLength(TxBuf, 1);
            TxBuffLength := 0;
            AppendByte($F9);
            XBee.SendUDP(TxBuf);                                                                                     {Transmit template byte}
            sleep(100);                                                                                   {Delay to give plenty of round-trip time}
            end;
          Read := XBee.ReceiveUDP(RxBuf, 1);
//          Read := XBee.ReceiveTCP(RxBuf, 2000);
          RxBuffSize := length(RxBuf);
          FRxBuffEnd := RxBuffSize;
          if not Read then                                                                              {Data not entirely read yet?}
            begin
//            if GetLastError <> ERROR_IO_PENDING then ReadError;                                           {Error, unable to read}
//            WaitResult := waitforsingleobject(FCommIOEvent, 1000);                                        {Wait for completion, or 1 second, whichever comes first}
//            if WaitResult = WAIT_FAILED then Error(ord(mtWaitFailed));
//            if WaitResult = WAIT_TIMEOUT then ReadError;                                                  {Error, timed-out on read of PC hardware}
//              raise Exception.Create('Error: Timed-out on read.');
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
  if FBinSize = 0 then exit;

  FVersionMode := False;
  FDownloadMode := 1; {The download command; 1 = write to RAM and run, 2 = write to EEPROM and stop, 3 = write to EEPROM and run}

  FRxBuffStart := 0;
  FRxBuffEnd := 0;

  {Determine number of required packets for target application image; value becomes first Packet ID}
  SetRoundMode(rmUp);
  PacketID := Round(FBinSize*4 / (XBee.MaxDataSize-4*2));                                           {Calculate required number of packets for target image; binary image size (in bytes) / (max packet size - packet header)}

  {Prepare Loader Image}
  getmem(LoaderImage, RawLoaderAppSize*4+1);                                                        {Reserve LoaderImage space for RawLoaderImage data plus 1 extra byte to accommodate generation routine}
  getmem(LoaderStream, ImageLimit div 4 * 11);                                                      {Reserve LoaderStream space for maximum-sized download stream}
  try
    Move(RawLoaderImage, LoaderImage[0], RawLoaderAppSize*4);                                       {Copy raw loader image to LoaderImage (for adjustments and processing)}
    {Clear checksum}
    LoaderImage[5] := 0;
    {Set host-initialized values}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset, 80000000 div 115200);                   {Initial Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 4, 80000000 div 230400);               {Final Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 8, trunc(1.5 * 80000000) div 230400);  {1.5x Final Bit Time}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 12, 80000000 * 4 div (2*8));           {Timeout (seconds-worth of Loader's Receive loop iterations}
    SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 16, PacketID);                         {First Expected Packet ID}
    {Recalculate and update checksum}
    CheckSum := 0;
    for i := 0 to RawLoaderAppSize*4-1 do CheckSum := CheckSum + LoaderImage[i];
    for i := 0 to high(InitCallFrame) do CheckSum := CheckSum + InitCallFrame[i];
    LoaderImage[5] := 256-CheckSum;
    {Generate Propeller Download Stream from adjusted LoaderImage; Output delivered to LoaderStream and LoaderStreamSize}
    GenerateStream(LoaderImage, RawLoaderAppSize, LoaderStream, LoaderStreamSize);

    {Prepare initial packet: contains handshake and Loader Stream.}
    SetLength(TxBuf, Length(TxHandshake)+11+LoaderStreamSize);                                      {Set initial packet size}
    Move(TxHandshake, TxBuf[0], Length(TxHandshake));                                               {Fill packet with handshake stream (timing template, handshake, and download command (RAM+Run))}
    TxBuffLength := Length(TxHandshake);                                                            {followed by Raw Loader Images's App size (in longs)}
    AppendLong(RawLoaderAppSize);
    Move(LoaderStream[0], TxBuf[TxBuffLength], LoaderStreamSize);                                   {and the Loader Stream image itself}

    {Begin download process}
    if not XBee.ConnectSerialUDP then
      begin
      showmessage('Cannot connect');
      exit;
      end;

    try
      GenerateResetSignal;         {(Enforce XBee Configuration and...) Generate reset signal}
      IndySleep(190);

//    for i := 0 to FBinSize-1 do AppendLong(PIntegerArray(FBinImage)[i]);                                       {and the Loader image itself}
      if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Connecting and Transmitting Loader Image');  {Send connect and Loader packet}
      { TODO : Revisit handshake receive loop to check for all possibilities and how they are handled. }
      repeat
        if not XBee.ReceiveUDP(RxBuf, AppTimeout) then raise Exception.Create('Error, trouble connecting');      {Receive response}
        if Length(RxBuf) = 0 then
          raise Exception.Create('Error no response from Propeller')                                             {No response?}
        else
          if Length(RxBuf) = 129 then
            begin
            for i := 0 to 124 do if RxBuf[i] <> RxHandshake[i] then raise Exception.Create('Error Failed connection'); {Validate handshake response}
            for i := 125 to 128 do FVersion := (FVersion shr 2 and $3F) or ((RxBuf[i] and $1) shl 6) or ((RxBuf[i] and $20) shl 2); {Parse hardware version}
            end;
      until Length(RxBuf) = 129;                                                                                 {Loop if not correct (to flush receive buffer of previous data)}

      {Receive ram checksum pass/fail}
      if ReceiveBit(True, 2500) = 1 then raise Exception.Create('RAM Checksum Error');//Error(ord(mtRAMChecksumError) + (strtoint(FComPort) shl 16));

      {Now loader starts up in the Propeller; wait for loader's "ready" signal}
      if not XBee.ReceiveUDP(RxBuf, SerTimeout) or (Length(RxBuf) <> 4) then raise Exception.Create('Error, No Ready Signal from loader');   {Receive loader's ready signal}
      if Cardinal(RxBuf[0]) <> PacketID then raise Exception.Create('Error, Loader communication failure');   {Check loader's ready signal}

      {Switch to final baud rate}
      if not XBee.SetItem(xbSerialBaud, Baud230400) then raise Exception.Create('Can not switch to final baud rate');

      {Transmit packetized target application}
      i := 0;
      repeat                                                                         {Transmit application image}
        TxBuffLength := 2 + Min((XBee.MaxDataSize div 4)-2, FBinSize - i);           {  Determine packet length (in longs); header + packet limit or remaining data length}
        SetLength(TxBuf, TxBuffLength*4);                                            {  Set buffer length (Packet Length) (in longs)}
        Move(TxBuffLength, TxBuf[0], 4);                                             {  Store Packet Size (longs)}
        Move(PacketID, TxBuf[4], 4);                                                 {  Store Packet ID}
        Move(FBinImage[i*4], TxBuf[2*4], (TxBuffLength-2)*4);                        {  Store section of data}
        repeat                                                                       {  Set application image packet, get acknowledgement, repeat as necessary}
          { TODO : Think about limiting number of retransmissions }
          if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Transmitting Application Image');
          if not XBee.ReceiveUDP(RxBuf, SerTimeout) or (Length(RxBuf) <> 4) then raise Exception.Create('Error, Loader communication failure');
        until Cardinal(RxBuf[0]) = PacketID-1;
        inc(i, TxBuffLength);                                                        {  Increment image index}
        dec(PacketID);                                                               {  Decrement Packet ID (to next packet)}
      until PacketID = 0;                                                            {Loop until done}



//    if XBee.ConnectTCP then
//      begin
//      try

//!!        SetLength(TxBuf, 1+250+250+8);
//!!        TxBuffLength := 0;
//!!        AppendByte($F9);
//!!        for i := 1 to 250 do AppendByte(TxHandshake[i] or $FE);                     {Note "or $FE" encodes 1 handshake bit per transmitted byte}
//!!        for i := 1 to 250 + 8 do AppendByte($F9);

//!!        if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Connecting to Propeller');

//        XBee.ReceiveUDP(RxBuf, 0, 1000);

//        GenerateResetSignal;
//        XBee.SetItem(xbData, $0000);
//        XBee.Send(TxBuf);
//      finally
//        XBee.DisconnectTCP;
//      end;
//      end;

//!!      {Receive connect string}
//!!      RxCount := 0;
//!!      i := 1;
//!!      repeat                                                                      {Loop}
//!!        r := ReceiveBit(False, 100, False);                                       {  Receive encoded bit}
//!!        inc(RxCount);
//!!        if r = RxHandshake[i] then                                                {  Bits match?}
//!!          inc(i)                                                                  {    Ready to match next RxHandshake bit}
//!!        else
//!!          begin                                                                   {  Else (bits don't match)}
//!!          dec(FRxBuffStart, (i-1)*ord(r < 2));                                    {    Proper encoding (r < 2)?; start with 2nd bit checked and try again. Improper encoding (r > 1)?; may be junk prior to RxHandshake stream, ignore junk}
//!!          i := 1;                                                                 {    Prep to match first RxHandshake bit}
//!!          if RxCount > RxBuffSize then showmessage('Hardware Lost');              {    No RxHandshake in stream?  Time out; error}
//!!          end;
//!!      until (i > 250);                                                            {Loop until all RxHandshake bits received}
//!!      {Receive version}
//!!      for i := 1 to 8 do FVersion := FVersion shr 1 and $7F or ReceiveBit(False, 50) shl 7;
//!!      if FVersionMode then
//!!        begin {If version mode, send shutdown command and reset hardware to reboot}
//!!        SetLength(TxBuf, 11);
//!!        TxBuffLength := 0;
//!!        AppendLong(0);
//!!        if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Sending shutdown command');
//!!        GenerateResetSignal;
//!!//        CloseComm;
//!!        end
//!!      else
//!!        begin
//!!        {Send download command immediately}
//!!        SetLength(TxBuf, 11);
//!!        TxBuffLength := 0;
//!!        AppendLong(FDownloadMode);
//!!        if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Sending Download Command');
//!!        {If download command 1-3, do the following}
//!!        if FDownloadMode > 0 then
//!!          begin
//!!  //        {Update GUI - Loading RAM}
//!!  ///        QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtLoadingRAM));
//!!          {Send count and longs}
//!!          SetLength(TxBuf, (1+FBinSize)*11);
//!!          TxBuffLength := 0;
//!!          AppendLong(FBinSize);
//!!          for i := 0 to FBinSize-1 do
//!!            begin
//!!  //          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtProgress));
//!!             AppendLong(PIntegerArray(FBinImage)[i]);
//!!             end;
//!!          if not XBee.SendUDP(TxBuf) then raise Exception.Create('Error Sending Application Image');
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
//!!          end;
  //      CloseComm;
//!!        end;
    finally
      XBee.DisconnectSerialUDP;
    end;
  finally
    freemem(LoaderImage);
    freemem(LoaderStream);
//    XBee.DisconnectTCP;
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

procedure TForm1.GenerateStream(InImage: PByteArray; InSize: Integer; OutImage: PByteArray; var OutSize: Integer);
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

{----------------------------------------------------------------------------------------------------}

Initialization
  getmem(FBinImage, ImageLimit);
  FBinSize := 0;

Finalization
  freemem(FBinImage);

end.
