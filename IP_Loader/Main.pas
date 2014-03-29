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
    PingClient: TIdIcmpClient;
    TransmitButton: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PCPortComboChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TransmitButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure GenerateResetSignal;
    function EnforceXBeeConfiguration: Boolean;
  public
    { Public declarations }
  end;

var
  Form1    : TForm1;
  XBee     : TXBeeWiFi;
  XBeeList : array of TXBee;           {Dynamic array of XBee Info records}
  TxBuf    : TIdBytes;                 {Transmit packet (resized per packet)}
  RxBuf    : TIdBytes;                 {Receive packet (resized on receive)}

const
  Timeout        = 1000;
  CSumUnknown    = $FFFFFFFF;          {Unknown checksum value}

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
  XBee.Timeout := Timeout;
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
    Validate(xbListenIP, TCPListen);                                                         {    Ensure XBee's Serial Service listens to TCP packets}
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
  i            : Integer;
  TxBuffLength : Integer;
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

begin
  GenerateResetSignal;         {(Enforce XBee Configuration and...) Generate reset signal}
  IndySleep(190);
//  if XBee.ConnectTCP then
//    begin
//    try
      SetLength(TxBuf, 1+250+250+8);
      TxBuffLength := 0;
      AppendByte($F9);
      for i := 1 to 250 do AppendByte(TxHandshake[i] or $FE);                     {Note "or $FE" encodes 1 handshake bit per transmitted byte}
      for i := 1 to 250 + 8 do AppendByte($F9);

      XBee.SendUDP(TxBuf);

//      GenerateResetSignal;
//      XBee.SetItem(xbData, $0000);
//      XBee.Send(TxBuf);
//    finally
//      XBee.DisconnectTCP;
//    end;
//    end;
end;

end.
