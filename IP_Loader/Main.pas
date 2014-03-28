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
  {Upon start up, bind UDP to 0xBEE port ($BEE) to receive all responses from XBee Wi-Fi modules
   because some features have been seen to respond only to a source port of $BEE.}
  XBee := TXBeeWiFi.Create;
  XBee.LocalUDPPort := $BEE;
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
  XBee.SetItem(udpIO2State, pinOutHigh);
//  IP := TIDStack.Create;
//  IP.NewInstance;
//  IPAddr.Text := IP.LocalAddress;
//  IP.Free;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  XBee.SetItem(udpIO2State, pinOutLow);
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
  if XBee.GetItem(udpIPAddr, Nums, '192.168.1.255') then
    begin
    for Idx := 0 to High(Nums) do
      begin
      SetLength(XBeeList, Length(XBeeList)+1);
      PXB := @XBeeList[High(XBeeList)];
      PXB.CfgChecksum := CSumUnknown;
      PXB.IPAddr := FormatIPAddr(Nums[Idx]);
      if XBee.GetItem(udpIPPort, PXB.IPPort, PXB.IPAddr) then
        if XBee.GetItem(udpMacHigh, PXB.MacAddrHigh, PXB.IPAddr) then
          if XBee.GetItem(udpMacLow, PXB.MacAddrLow, PXB.IPAddr) then
            if XBee.GetItem(udpNodeID, PXB.NodeID, PXB.IPAddr) then
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
  XBee.IPAddr := IPAddr.Text;
  IPPort.Text := inttostr(XBeeList[PCPortCombo.Selected.Index].IPPort);
  XBee.IPPort := strtoint(IPPort.Text);
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
  if EnforceXBeeConfiguration then XBee.SetItem(udpOutputState, $0000);            {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.EnforceXBeeConfiguration: Boolean;
{Validate necessary XBee configuration; set attributes if needed.
 Returns True if XBee properly configured; false otherwise.}

    {----------------}

    function Validate(Attribute: udpCommand; Value: Cardinal; ReadOnly: Boolean = False): Boolean;
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
            (Validate(udpChecksum, XBeeList[PCPortCombo.Selected.Index].CfgChecksum, True));
  if not Result then                                                                         {  If not...}
    begin
    Validate(udpIO2State, pinOutHigh);                                                       {    Ensure I/O is set to output high}
    Validate(udpOutputMask, $7FFF);                                                          {    Ensure output mask is proper (default, in this case)}
    Validate(udpIO2Timer, 1);                                                                {    Ensure DIO2's timer is set to 100 ms}
//    Validate(udpSerialMode, TransparentMode);                                                {    Ensure Serial Mode is transparent}
    Validate(udpSerialBaud, Baud115200);                                                     {    Ensure baud rate is 115,200 bps}
    Validate(udpSerialParity, ParityNone);                                                   {    Ensure parity is none}
    Validate(udpSerialStopBits, StopBits1);                                                  {    Ensure stop bits is 1}
    XBee.GetItem(udpChecksum, XBeeList[PCPortCombo.Selected.Index].CfgChecksum);             {    Record new configuration checksum}
    Result := True;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.TransmitButtonClick(Sender: TObject);
begin
  GenerateResetSignal;         {(Enforce XBee Configuration and...) Generate reset signal}
  IndySleep(190);
//  if XBee.ConnectTCP then
//    begin
//    try
      SetLength(TxBuf, 5);
      TxBuf[0] := $1;
      TxBuf[1] := $2;
      TxBuf[2] := $3;
      TxBuf[3] := $4;
      TxBuf[4] := $5;
      XBee.SendUDP(TxBuf);
//      GenerateResetSignal;
//      XBee.SetItem(udpData, $0000);
//      XBee.Send(TxBuf);
//    finally
//      XBee.DisconnectTCP;
//    end;
//    end;
end;

end.
