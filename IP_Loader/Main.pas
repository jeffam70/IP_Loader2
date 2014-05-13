unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  XBeeWiFi,
  IdGlobal, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient, IdStack, FMX.Layouts, FMX.Memo,
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
    procedure GenerateStream(InImage: PByteArray; InSize: Integer; OutImage: PByteArray; var OutSize: Integer);
  public
    { Public declarations }
  end;

  EFileCorrupt  = class(Exception);     {File Corrupt exception}
  EDownload = class(Exception);         {Download protocol base exception class}
  ESoftDownload = class(EDownload);     {Soft download protocol error}
  EHardDownload = class(EDownload);     {Hard download protocol error; fatal}

var
  Form1     : TForm1;
  XBee      : TXBeeWiFi;
  XBeeList  : array of TXBee;           {Dynamic array of XBee Info records}
  TxBuf     : TIdBytes;                 {Transmit packet (resized per packet)}
  RxBuf     : TIdBytes;                 {Receive packet (resized on receive)}
  FBinImage : PByteArray;               {A copy of the Propeller Application's binary image (used to generate the download stream)}
  FBinSize  : Integer;                  {The size of FBinImage (in longs)}


const
  MinSerTimeout  = 100;
  SerTimeout     = 1000;
  AppTimeout     = 200;
  CSumUnknown    = $FFFFFFFF;          {Unknown checksum value}
  ImageLimit     = 32768;              {Max size of Propeller Application image file}

  InitialBaud    = 115200;             {Initial XBee-to-Propeller baud rate}
  FinalBaud      = 230400;             {Final XBee-to-Propeller baud rate}

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

  TotalPackets     : Integer;                            {Total number of image packets}
  PacketID         : Integer;                            {ID of packet transmitted}
  Retry            : Integer;                            {Retry counter}
  Time             : Int64;
  Acknowledged     : Boolean;                            {True = positive/negative acknowledgement received from loader, False = no response from loader}

  STime            : Int64;

const
  pReset = Integer.MinValue;

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
{  RawLoaderAppSize = 74;
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
}
  RawLoaderAppSize = 109;
  RawLoaderImage : array[0..435] of byte = ($00,$B4,$C4,$04,$6F,$70,$10,$00,$B4,$01,$BC,$01,$AC,$01,$C0,$01,
                                            $A4,$01,$02,$00,$9C,$01,$00,$00,$5F,$E8,$BF,$A0,$5F,$EC,$BF,$A0,
                                            $01,$E8,$FF,$68,$01,$EC,$FF,$68,$60,$CC,$BC,$A1,$01,$CC,$FC,$28,
                                            $F1,$CD,$BC,$80,$A0,$CA,$CC,$A0,$60,$CC,$BC,$F8,$F2,$BD,$3C,$61,
                                            $07,$CA,$FC,$E4,$22,$00,$7C,$5C,$6A,$D2,$FC,$A0,$00,$AE,$FC,$A0,
                                            $69,$24,$BC,$54,$69,$28,$BC,$54,$69,$2A,$BC,$54,$04,$D0,$FC,$A0,
                                            $00,$D4,$FC,$A0,$48,$AC,$FC,$5C,$67,$D4,$BC,$68,$08,$D4,$FC,$20,
                                            $13,$D0,$FC,$E4,$01,$D2,$FC,$80,$01,$AE,$FC,$80,$0E,$D4,$FC,$E4,
                                            $64,$D6,$3C,$86,$01,$C8,$E8,$84,$02,$AE,$FC,$84,$58,$D8,$28,$08,
                                            $04,$B0,$E8,$80,$5A,$3A,$A8,$80,$1D,$AE,$E8,$E4,$6C,$3A,$E8,$54,
                                            $04,$D0,$FC,$A0,$64,$CE,$BC,$A0,$08,$C8,$FC,$20,$3E,$8E,$FC,$5C,
                                            $23,$D0,$FC,$E4,$61,$C0,$BC,$A0,$0C,$C8,$7C,$E8,$5B,$AE,$BC,$A0,
                                            $58,$AE,$BC,$84,$02,$AE,$FC,$2A,$58,$D0,$14,$08,$04,$B0,$D4,$80,
                                            $2C,$AE,$D4,$E4,$0A,$AE,$FC,$04,$04,$AE,$FC,$84,$57,$B8,$3C,$08,
                                            $04,$AE,$FC,$84,$57,$B8,$3C,$08,$01,$B0,$FC,$84,$58,$D0,$BC,$00,
                                            $68,$CE,$BC,$80,$34,$B0,$7C,$E8,$FF,$CE,$FC,$62,$3E,$8E,$FC,$5C,
                                            $06,$B0,$FC,$04,$10,$B0,$7C,$86,$00,$B2,$54,$0C,$02,$BA,$7C,$0C,
                                            $FF,$CE,$FC,$60,$00,$CF,$FC,$68,$01,$CE,$FC,$2C,$60,$CC,$BC,$A0,
                                            $F1,$CD,$BC,$80,$01,$CE,$FC,$29,$60,$CC,$BC,$F8,$5F,$E8,$BF,$70,
                                            $43,$CE,$7C,$E8,$00,$00,$7C,$5C,$63,$CA,$BC,$A0,$62,$CC,$BC,$A1,
                                            $00,$CE,$FC,$A0,$80,$CF,$FC,$72,$F2,$BD,$3C,$61,$4B,$CA,$F8,$E4,
                                            $00,$B2,$78,$0C,$F1,$CD,$BC,$80,$60,$CC,$BC,$F8,$01,$E8,$FF,$6C,
                                            $F2,$BD,$3C,$61,$00,$CF,$FC,$70,$01,$CE,$FC,$29,$50,$00,$4C,$5C,
                                            $00,$00,$7C,$5C,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,
                                            $00,$02,$00,$00,$00,$80,$00,$00,$FF,$FF,$F9,$FF,$10,$C0,$07,$00,
                                            $00,$00,$00,$80,$00,$00,$00,$40,$B6,$02,$00,$00,$5B,$01,$00,$00,
                                            $08,$02,$00,$00,$00,$2D,$31,$01,$00,$00,$00,$00,$35,$C7,$08,$35,
                                            $2C,$32,$00,$00);

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

begin
  try {Handle download errors}
    if FBinSize = 0 then exit;

    TransmitButton.Enabled := False;

    FVersionMode := False;
    FDownloadMode := 1; {The download command; 1 = write to RAM and run, 2 = write to EEPROM and stop, 3 = write to EEPROM and run}

    FRxBuffStart := 0;
    FRxBuffEnd := 0;

    {Reserve memory for Loader Image}
    getmem(LoaderImage, RawLoaderAppSize*4+1);                                                        {Reserve LoaderImage space for RawLoaderImage data plus 1 extra byte to accommodate generation routine}
    getmem(LoaderStream, ImageLimit div 4 * 11);                                                      {Reserve LoaderStream space for maximum-sized download stream}

    try {Reserved Memory}

      STime := Ticks;
  
      {Determine number of required packets for target application image; value becomes first Packet ID}
      SetRoundMode(rmUp);
      TotalPackets := Round(FBinSize*4 / (XBee.MaxDataSize-4*2));                                               {Calculate required number of packets for target image; binary image size (in bytes) / (max packet size - packet header)}
      PacketID := TotalPackets;
      {Initialize Progress Bar to proper size}
      InitializeProgress(7 + TotalPackets);
      {Prepare Loader Image}
      Move(RawLoaderImage, LoaderImage[0], RawLoaderAppSize*4);                                                 {Copy raw loader image to LoaderImage (for adjustments and processing)}
      {Clear checksum}
      LoaderImage[5] := 0;
      {Set host-initialized values}
      SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset, 80000000 div InitialBaud);                {Initial Bit Time}
      SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 4, 80000000 div FinalBaud);              {Final Bit Time}
      SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 8, trunc(1.5 * 80000000) div FinalBaud); {1.5x Final Bit Time}
      SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 12, 80000000 * 4 div (2*8));             {Timeout (seconds-worth of Loader's Receive loop iterations}
      SetHostInitializedValue(RawLoaderAppSize*4+RawLoaderInitOffset + 16, PacketID);                           {First Expected Packet ID}
      {Recalculate and update checksum}
      CheckSum := 0;
      for i := 0 to RawLoaderAppSize*4-1 do CheckSum := CheckSum + LoaderImage[i];
      for i := 0 to high(InitCallFrame) do CheckSum := CheckSum + InitCallFrame[i];
      LoaderImage[5] := 256-CheckSum;
      {Generate Propeller Download Stream from adjusted LoaderImage; Output delivered to LoaderStream and LoaderStreamSize}
      GenerateStream(LoaderImage, RawLoaderAppSize, LoaderStream, LoaderStreamSize);

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

          SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - **** CONNECTING ***', True);
      
          {Prepare initial packet; contains handshake and Loader Stream.}
          SetLength(TxBuf, Length(TxHandshake)+11+LoaderStreamSize);                                      {Set initial packet size}
          if Length(TxBuf) > XBee.MaxDataSize then
            raise EHardDownload.Create('Developer Error: Initial packet is too large (' + Length(TxBuf).ToString + ' bytes)!');

          SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Initial packet size: ' + Length(TxBuf).ToString + ' bytes', True);

          Move(TxHandshake, TxBuf[0], Length(TxHandshake));                                               {Fill packet with handshake stream (timing template, handshake, and download command (RAM+Run))}
          TxBuffLength := Length(TxHandshake);                                                            {followed by Raw Loader Images's App size (in longs)}
          AppendLong(RawLoaderAppSize);
          Move(LoaderStream[0], TxBuf[TxBuffLength], LoaderStreamSize);                                   {and the Loader Stream image itself}

          SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Generating reset signal', True);

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
          TxBuffLength := 2 + Min((XBee.MaxDataSize div 4)-2, FBinSize - i);                     {  Determine packet length (in longs); header + packet limit or remaining data length}
          SetLength(TxBuf, TxBuffLength*4);                                                      {  Set buffer length (Packet Length) (in longs)}
          Move(TxBuffLength, TxBuf[0], 4);                                                       {  Store Packet Size (longs)}
          Move(PacketID, TxBuf[4], 4);                                                           {  Store Packet ID}
          Move(FBinImage[i*4], TxBuf[2*4], (TxBuffLength-2)*4);                                  {  Store section of data}
          Retry := 4;
          repeat {(Re)Transmit packet}                                                           {  Send application image packet, get acknowledgement, retransmit as necessary}
            UpdateProgress(+1, 'Sending packet: ' + (TotalPackets-PacketID+1).ToString + ' of ' + TotalPackets.ToString);

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Time: ' + Time.ToString + ' Transmitting packet ' + PacketID.ToString, True);

            Time := Ticks;                                                                       {    Note transmit time}
            if not XBee.SendUDP(TxBuf, True, False) then
              raise EHardDownload.Create('Error: Can not transmit application image!');

            SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for packet acknowledgement', True);

            Acknowledged := XBee.ReceiveUDP(RxBuf, DynamicSerTimeout) and (Length(RxBuf) = 4);   {    Wait for positive/negative acknowledgement, or timeout}
            Time := GetTickDiff(Time, Ticks);                                                    {    Calculate acknowledgement/timeout time}
            SendDebugMessage('          - Elapsed: ' + Time.ToString + ' Minimum: ' + Trunc((TxBuffLength*4*10/FinalBaud)*1000).ToString, True);
            dec(Retry);                                                                          {  Loop and retransmit until timely positive acknowledgement received, or retry count exhausted}
          {Repeat - (Re)Transmit packet...}
          { TODO : Revisit phase variance timing trap }
            if not (Acknowledged and (Integer(RxBuf[0]) = PacketID-1)) then UpdateProgress(-1);
          until (Acknowledged and {(Time > Trunc((TxBuffLength*4*10/FinalBaud)*1000) and} (Integer(RxBuf[0]) = PacketID-1)) or (Retry = 0);
          if Retry = 0 then
            raise EHardDownload.Create('Error: connection lost!');                               {  No acknowledgement received? Error}
          inc(i, TxBuffLength-2);                                                                {  Increment image index}
          dec(PacketID);                                                                         {  Decrement Packet ID (to next packet)}
        {repeat - Transmit target application packets...}  
        until PacketID = 0;                                                                      {Loop until done}

        {Receive RAM checksum pass/fail}

        SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Waiting for RAM checksum', True);

        { TODO : Think about possible need for retransmission of RAM checksum here. }
        UpdateProgress(+1, 'Verifying RAM');
        if not XBee.ReceiveUDP(RxBuf, SerTimeout) or (Length(RxBuf) <> 1) then
          raise EHardDownload.Create('Error: No final response!');
        if Cardinal(RxBuf[0]) <> 0 then
          raise EHardDownload.Create('Error: RAM Checksum Error!');

        UpdateProgress(+1, 'Success');

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
  //!!          if ReceiveBit(True, 2500) = 1 then raise Exception.Create('RAM Checksum Error');//Error(ord(mtRAMChecksumError) + (strtoint(FComPort) shl 16));
            {If download command 2-3, do the following}
  //!!          if FDownloadMode > 1 then
  //!!            begin
    //          {Update GUI - Programming EEPROM}
    //          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtProgrammingEEPROM));
              {Receive eeprom program pass/fail}
  //!!            if ReceiveBit(True, 5000) = 1 then raise Exception.Create('EEPROM Programming Error');//Error(ord(mtEEPROMProgrammingError) + (strtoint(FComPort) shl 16));
    //          {Update GUI - Verifying EEPROM}
    //          QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtVerifyingEEPROM));
              {Receive eeprom verify pass/fail}
  //!!            if ReceiveBit(True, 2500) = 1 then raise Exception.Create('EEPROM Verify Error');//Error(ord(mtEEPROMVerifyError) + (strtoint(FComPort) shl 16));
  //!!            end;
  //!!          end;
    //      CloseComm;
  //!!        end;
      finally {UDP Connected}
        XBee.DisconnectSerialUDP;
      end;
    finally {Reserved Memory}
      freemem(LoaderImage);
      freemem(LoaderStream);

      SendDebugMessage('+' + GetTickDiff(STime, Ticks).ToString + ' - Exiting', True);
      IndySleep(500);
      UpdateProgress(0, '', False);

      TransmitButton.Enabled := True;

  //    XBee.DisconnectTCP;
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
