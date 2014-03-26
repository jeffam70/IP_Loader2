unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  XBeeWiFi, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient;

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PCPortComboChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure GenerateResetSignal;
    procedure SetSerialConfiguration;
  public
    { Public declarations }
  end;

var
  Form1    : TForm1;
  XBee     : TXBeeWiFi;
  XBeeList : array of TXBee;           {Dynamic array of XBee Info records}


const
  Timeout        = 1000;

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
var
  Value : Cardinal;
begin
  if XBee.GetItem(udpIO2State, Value) then
    if (Value = pinOutHigh) or XBee.SetItem(udpIO2State, pinOutHigh) then                     {Ensure I/O is set to output high}
      if XBee.GetItem(udpOutputMask, Value) then
        if (Value = $7FFF) or XBee.SetItem(udpOutputMask, $7FFF) then                         {Ensure output mask is proper (default, in this case)}
          if XBee.GetItem(udpIO2Timer, Value) then
            if (Value = 1) or XBee.SetItem(udpIO2Timer, 1) then                               {Ensure DIO2's timer is set to 100 ms}
              XBee.SetItem(udpOutputState, $0000);                                            {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.SetSerialConfiguration;
var
  Value : Cardinal;
begin
  if XBee.GetItem(udpSerialBaud, Value) then
    if (Value = Baud115200) or XBee.SetItem(udpSerialBaud, Baud115200) then                   {Ensure baud rate is 115,200 bps}
      if XBee.GetItem(udpSerialParity, Value) then
        if (Value = ParityNone) or XBee.SetItem(udpSerialParity, ParityNone) then             {Ensure parity is none}
          if XBee.GetItem(udpSerialStopBits, Value) then
            if (Value = StopBits1) or XBee.SetItem(udpSerialStopBits, StopBits1) then;         {Ensure stop bits is 1}
end;


end.
