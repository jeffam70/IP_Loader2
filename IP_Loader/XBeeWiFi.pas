unit XBeeWiFi;

interface

uses
  System.SysUtils, System.StrUtils, System.Math,
  FMX.Dialogs,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack;

type
  {Define XBee WiFi's udp commands}
  {IMPORTANT: Do not rearrange, append, or delete from this list without similarly modifying the ATCmd constant array}
  udpCommand = (udpMacHigh, udpMacLow, udpSSID, udpIPAddr, udpIPMask, udpIPGateway, udpIPPort, udpNodeID, udpMaxRFPayload, udpIO2State,
                udpOutputMask, udpOutputState, udpIO2Timer, udpSerialBaud, udpSerialParity, udpSerialStopBits);

const
  {Define XBee WiFi's AT commands}
  ATCmd : array[low(udpCommand)..high(udpCommand)] of Word =
    ({NOTES: [R] - read only, [R/W] = read/write, [s] - string, [b] - binary number, [sb] - string or binary number}
    {udpMacHigh}         Byte('S') + Byte('H') shl 8,  {[Rb] XBee's Mac Address (highest 16-bits)}
    {udpMacLow}          Byte('S') + Byte('L') shl 8,  {[Rb] XBee's Mac Address (lowest 32-bits)}
    {udpSSID}            Byte('I') + Byte('D') shl 8,  {[Rs/Ws] SSID (0 to 31 printable ASCII characters)}
    {udpIPAddr}          Byte('M') + Byte('Y') shl 8,  {[Rb*/Wsb] XBee's IP Address (32-bits); *Read-only in DHCP mode}
    {udpIPMask}          Byte('M') + Byte('K') shl 8,  {[Rb*/Wsb] XBee's IP Mask (32-bits); *Read-only in DHCP mode}
    {udpIPGateway}       Byte('G') + Byte('W') shl 8,  {[Rb*/Wsb] XBee's IP Gateway (32-bits); *Read-only in DHCP mode}
    {udpIPPort}          Byte('C') + Byte('0') shl 8,  {[Rb/Wb] Xbee's UDP/IP Port (16-bits)}
    {udpNodeID}          Byte('N') + Byte('I') shl 8,  {[Rs/Ws] Friendly node identifier string (20 printable ASCII characters)}
    {udpMaxRFPayload}    Byte('N') + Byte('P') shl 8,  {[Rb] Maximum RF Payload (16-bits; in bytes)}
    {udpIO2State}        Byte('D') + Byte('2') shl 8,  {[Rb/Wb] Designated reset pin (3-bits; 0=Disabled, 1=SPI_CLK, 2=Analog input, 3=Digital input, 4=Digital output, 5=Digital output)}
    {udpOutputMask}      Byte('O') + Byte('M') shl 8,  {[Rb/Wb] Output mask for all I/O pins (each 1=output pin, each 0=input pin) (15-bits on TH, 20-bits on SMT)}
    {udpOutputState}     Byte('I') + Byte('O') shl 8,  {[Rb/Wb] Output state for all I/O pins (each 1=high, each 0=low) (15-bits on TH, 20-bits on SMT).  Period affected by updIO2Timer}
    {udpIO2Timer}        Byte('T') + Byte('2') shl 8,  {[Rb/Wb] I/O 2 state timer (100 ms units; $0..$1770)}
    {udpSerialBaud}      Byte('B') + Byte('D') shl 8,  {[Rb/Wb] serial baud rate ($1=2400, $2=4800, $3=9600, $4=19200, $5=38400, $6=57600, $7=115200, $8=230400, $9=460800, $A=921600)}
    {udpSerialParity}    Byte('N') + Byte('B') shl 8,  {[Rb/Wb] serial parity ($0=none, $1=even, $2=odd)}
    {udpSerialStopBits}  Byte('S') + Byte('B') shl 8   {[Rb/Wb] serial stop bits ($0=one stop bit, $1=two stop bits)}
    );

  {Set of UDP Commands that take string parameters (instead of numeric parameters)}
  udpStrCommands : set of udpCommand = [udpNodeID];

type
  {Define Parameter Value}
  TParamValue = array[0..63+1] of Byte;

  {Define XBee Transmission Packet - the format of packets transmitted from networked host to XBee}
  PxbTxPacket = ^TxbTxPacket;
  TxbTxPacket = packed record
    {Application Header}
    Number1        : Word;         //Can be any random number
    Number2        : Word;         //Must be Number1 ^ $4242
    PacketID       : Byte;         //Reserved: $00 for now
    EncryptionPad  : Byte;         //Reserved: $00 for now
    CommandID      : Byte;         //$00 = Data, $02 = Remote Command, $03 = General Purpose Memory Command, $04 = I/O Sample
    CommandOptions : Byte;         //Bit 0 : Encrypt (Reserved), Bit 1 : Request Packet ACK, Bits 2..7 : (Reserved)
    {Command Data}
    FrameID        : Byte;         //1
    ConfigOptions  : Byte;         //0 = Queue command only; must follow with AC command to apply changes, 2 = Apply Changes immediately
    ATCommand      : Word;         //2-ASCII_character AT command
    ParameterValue : TParamValue;  //[Array] (if present) is value to set in the given command, otherwise, command is queried.
  end;

  {Define XBee Reception Packet - the format of packets transmitted from XBee to networked host}
  PxbRxPacket = ^TxbRxPacket;
  TxbRxPacket = packed record
    {Application Header}
    Number1        : Word;         //Can be any random number (copied from command packet if this is a response)
    Number2        : Word;         //Must be Number1 ^ $4242 (copied from command packet if this is a response)
    PacketID       : Byte;         //Reserved: $00 for now
    EncryptionPad  : Byte;         //Reserved: $00 for now
    CommandID      : Byte;         //$80 = Response to Data, $82 = Response to Remote Command, $83 = Response to General Purpose Memory Command
    CommandOptions : Byte;         //$00 (always)
    {Command Data}
    FrameID        : Byte;         //1 (copied from command packet)
    ATCommand      : Word;         //2-ASCII_character AT command (copied from command packet)
    Status         : Byte;         //0 = OK, 1 = ERROR, 2 = Invalid Command, 3 = Invalid Parameter
    ParameterValue : TParamValue;  //[Array] If present, contains the binary or ASCII-format data requested by the command packet
  end;

  {Define response; used to record or more responses (ParameterValues) from incoming response packets}
  TResponse = record
    Length : Cardinal;
    Status : Byte;
    Data   : TParamValue;
  end;

  {Define simple string list}
  TSimpleStringList = array of String;

  {Define simple number list}
  TSimpleNumberList = array of Cardinal;

  TXBeeWiFi = class(TObject)
  protected
    FTCPClient: TIdTCPClient;
    FUDPClient: TIdUDPClient;
    {IP Buffer}
    FTxBuf        : TIdBytes;                 {Raw transmit packet (resized per packet)}
    FRxBuf        : TIdBytes;                 {Raw receive packet stream (fixed to MaxPacketSize)}
    FRxPacketSize : Integer;                  {Size of last actual received packet (inside of RxBuf)}
    FPTxBuf       : PxbTxPacket;              {Pointer to structured transmit packet (shares memory space with TxBuf)}
    FPRxBuf       : PxbRxPacket;              {Pointer to structured receive packet (shares memory space with RxBuf)}
    FResponseList : array of TResponse;       {Dynamic array of received responses (from packet's parameter field); multiple XBee may respond to broadcast message}
    {XBeeWiFi Metrics}
    FIPAddr       : String;                   {IP address to contact (if TargetIP not provided in method calls)}
    FIPPort       : Cardinal;                 {IP port to contact (if TargetIP not provided in method calls)}
    FTimeout      : Cardinal;                 {Timeout for UDP responses}
    {Getters and Setters}
    function  GetLocalUDPPort: Cardinal;
    procedure SetLocalUDPPort(Value: Cardinal);
    function  GetLocalTCPPort: Cardinal;
    procedure SetLocalTCPPort(Value: Cardinal);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy;
    {XBee communication methods}
    { TODO : Determine best way to deal with IPs and Ports. }
    function  GetItem(Command: udpCommand; var Str: String; TargetIP: String = ''): Boolean; overload;
    function  GetItem(Command: udpCommand; var StrList: TSimpleStringList; TargetIP: String = ''): Boolean; overload;
    function  GetItem(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    function  GetItem(Command: udpCommand; var NumList: TSimpleNumberList; TargetIP: String = ''): Boolean; overload;
    function  SetItem(Command: udpCommand; Str: String; TargetIP: String = ''; ExpectMultiple: Boolean = False): Boolean; overload;
    function  SetItem(Command: udpCommand; Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    function  ConnectStream(TargetPort: Cardinal = 0; TargetIP: String = ''): Boolean;
    function  DisconnectStream: Boolean;
    function  Send(Data: TIDBytes): Boolean;
    function  Receive(var Data: TIDBytes; Count: Integer): Boolean;
    {Class properties}
    { TODO : Make setter for IPAddr property }
    property IPAddr : String read FIPAddr write FIPAddr;
    property IPPort : Cardinal read FIPPort write FIPPort;
    property LocalUDPPort : Cardinal read GetLocalUDPPort write SetLocalUDPPort;
    property LocalTCPPort : Cardinal read GetLocalTCPPort write SetLocalTCPPort;
    property Timeout : Cardinal read FTimeout write FTimeout;
  private
    { Private declarations }
    procedure PrepareBuffer(Command: udpCommand; Parameter: String; RequestPacketAck: Boolean);
  end;

  {Non-object functions and procedures}
  function FormatIPAddr(Addr: Cardinal): String;
  function FormatMACAddr(AddrHigh, AddrLow: Cardinal): String;

const
  {Network Header Metrics}
  RemoteCommand  = $02;
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  {--I/O--}
  pinOutLow      = $04;
  pinOutHigh     = $05;
  {--Serial--}
  Baud2400       = $01;
  Baud4800       = $02;
  Baud9600       = $03;
  Baud19200      = $04;
  Baud38400      = $05;
  Baud57600      = $06;
  Baud115200     = $07;
  Baud230400     = $08;
  Baud460800     = $09;
  Baud921600     = $0A;
  ParityNone     = $00;
  ParityEven     = $01;
  ParityOdd      = $02;
  StopBits1      = $00;
  StopBits2      = $01;
  {Misc}
  DefaultTimeout = 1000;

implementation

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------- Protected Methods ----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetLocalUDPPort: Cardinal;
begin
  Result := FUDPClient.BoundPort;
end;

{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.SetLocalUDPPort(Value: Cardinal);
begin
  if (Value <= $10000) and (Value <> FUDPClient.BoundPort) then
    FUDPClient.BoundPort := Value;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetLocalTCPPort: Cardinal;
begin
  Result := FTCPClient.BoundPort;
end;

{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.SetLocalTCPPort(Value: Cardinal);
begin
  if (Value <= $10000) and (Value <> FTCPClient.BoundPort) then
    FTCPClient.BoundPort := Value;
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------- Public Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

constructor TXBeeWiFi.Create;
begin
  FTCPClient := TIdTCPClient.Create;             {Create TCP Client}
  FUDPClient := TIdUDPClient.Create;             {Create UDP Client}
  SetLength(FRxBuf, 1500);                       {Set receive buffer length}
  FPRxBuf := PxbRxPacket(@FRxBuf[0]);            {Point PRxBuf at RxBuf}
  FTimeout := DefaultTimeout;                    {Set default timeout}
end;

{----------------------------------------------------------------------------------------------------}
destructor TXBeeWiFi.Destroy;
begin
  FTCPClient.Destroy;                            {Destroy TCP Client}
  FUDPClient.Destroy;                            {Destroy UDP Client}
  SetLength(FRxBuf, 0);                          {Free RxBuf memory}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: udpCommand; var Str: String; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute string (Str) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
begin
  Str := '';
  Result := SetItem(Command, '', TargetIP);                                                {Send packet}
  if Result then Str := StrPas(PAnsiChar(@FResponseList[0].Data));                         {Copy data to Str}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: udpCommand; var StrList: TSimpleStringList; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute strings (StrList) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
{ TODO : Test GetXBee StrList }
  SetLength(StrList, 0);
  Result := SetItem(Command, '', TargetIP, True);                                         {Send packet}
  if Result then                                                                          {If response received}
    begin                                                                                 {Copy data to StrList}
    for Idx := 0 to High(FResponseList) do
      begin
      SetLength(StrList, Length(StrList)+1);
      StrList[High(StrList)] := StrPas(PAnsiChar(@FResponseList[0].Data));
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute value (Num) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
  Num := 0;
  Result := SetItem(Command, '', TargetIP);
  if Result then for Idx := 0 to FResponseList[0].Length-1 do Num := Num shl 8 + FResponseList[0].Data[Idx];
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: udpCommand; var NumList: TSimpleNumberList; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute values (NumList) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  RIdx : Cardinal;
  NIdx : Cardinal;
begin
  SetLength(NumList, 0);
  Result := SetItem(Command, '', TargetIP, True);                                         {Send packet}
  if Result then                                                                          {If response received}
    begin                                                                                 {Copy data to NumList}
    for RIdx := 0 to High(FResponseList) do
      begin
      SetLength(NumList, Length(NumList)+1);
      NumList[High(NumList)] := 0;
      for NIdx := 0 to FResponseList[RIdx].Length-1 do
        NumList[High(NumList)] := NumList[High(NumList)] shl 8 + FResponseList[RIdx].Data[NIdx];
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SetItem(Command: udpCommand; Str: String; TargetIP: String = ''; ExpectMultiple: Boolean = False): Boolean;
{Set XBee attribute to string (Str) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.  Set ExpectMultiple true if multiple responses possible, such as
 when a broadcast packet is being transmitted.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
var
  RLIdx         : Integer;          // response list index
  IP            : String;
  RequiredRx    : Cardinal;         // bit 0 = Packet ACK received, bit 1 = Command response received
  Status        : Cardinal;         // $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter
const
  {Required Rx codes}
  PacketAck  = $01;                     {RequiredRx's code for packet ACK received}
  CommandRsp = $02;                     {RequiredRx's code for command response received}
  GotItAll   = PacketAck + CommandRsp;  {Code for all required receipts}
  MuliRsp    = $04;                     {Code for multiple responses possible}

    {----------------}

    function UDPResponse: Boolean;
    {Check for UDP packet for up to FTimeout ms.  Return True/False (packet received) and store packet size in Count.}
    begin
      FRxPacketSize := FUDPClient.ReceiveBuffer(FRxBuf, FTimeout);
      Result := FRxPacketSize > 0;
    end;

    {----------------}

begin
{ TODO : Remove debugging Info }
//  self.Caption := 'Transmitting...';
  Result := False;                                                                                           {Initialize result to false}
  RequiredRx := 0;                                                                                           {Initialize required reception to none}
  Status := 0;                                                                                               {Initialize status}
  RLIdx := -1;                                                                                               {Clear response list index}
  SetLength(FResponseList, 0);                                                                               {Clear the response list}
  PrepareBuffer(Command, Str, True);                                                                         {Prepare command packet}
  try
    {Try to transmit; IP exceptions handled}
    FUDPClient.SendBuffer(ifthen(TargetIP <> '', TargetIP, FIPAddr), $BEE, FTxBuf);                          {Send to TargetIP or GUI-displayed IP}
    {Transmitted fine, retrieve}
    while (RequiredRx < GotItAll+ord(ExpectMultiple)*MuliRsp) and UDPResponse do                             {For every UDP Packet received (until we've received the expected packets)}
      begin {Process each UDP packet received}
      if FPRxBuf.Number1 xor $4242 = FPRxBuf.Number2 then                                                    {if packet is an XBee Wi-Fi response packet (Number2 is Number1 ^ $4242)}
        begin {It's an XBee Response packet}
        if (FPRxBuf.CommandID = $80) then RequiredRx := RequiredRx or PacketAck;                             {Note when we received XBee Wi-Fi UDP ACK packet}
        if (FPRxBuf.CommandID = (FPTxBuf.CommandID or $80)) and (FPRxBuf.ATCommand = FPTxBuf.ATCommand) then
          begin
          RequiredRx := RequiredRx or CommandRsp or (FPRxBuf.Status shl 2);                                  {Note when we received XBee Wi-Fi UDP command response packet}
          Status := Status or FPRxBuf.Status;
          inc(RLIdx);
          SetLength(FResponseList, RLIdx+1);                                                                 {Make room for response}
          FResponseList[RLIdx].Length := FRxPacketSize-(sizeof(TxbRxPacket)-sizeof(FPRxBuf.ParameterValue)); {Note response length}
          FResponseList[RLIdx].Status := FPRxBuf.Status;                                                     {Note response status}
          Move(FPRxBuf.ParameterValue, FResponseList[RLIdx].Data, FResponseList[RLIdx].Length);              {Save response data}
          FResponseList[RLIdx].Data[FResponseList[RLIdx].Length] := 0;                                       {Null-terminate (in case it's a string)}
          end;
        end;
      end;
//    case RequiredRx of
//      0 : self.Caption := 'No Response';
//      1 : self.Caption := 'Only Ack received';
//      2 : self.Caption := 'Only Response received';
//      3 : self.Caption := 'Both Ack and Response received';
//    end;
//    case Status of
//      0 : self.Caption := self.Caption + ' - Okay';
//      1 : self.Caption := self.Caption + ' - Error';
//      2 : self.Caption := self.Caption + ' - Invalid Command';
//      3 : self.Caption := self.Caption + ' - Invalid Parameter';
//    end;
    Result := (RequiredRx = PacketAck + CommandRsp) and (Status = 0);
  except
    {Handle known exceptions}
    on E: EIdSocketError do
      begin
      if E.LastError = 10013 then
        showmessage('Communication Error: Invalid IP Address')
      else
        showmessage('Communication Error: ' + #$D#$A +E.Message);
      end;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SetItem(Command: udpCommand; Num: Cardinal; TargetIP: String = ''): Boolean;
{Set XBee attribute to value (Num) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
begin
  Result := SetItem(Command, inttostr(Num), TargetIP);
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ConnectStream(TargetPort: Cardinal = 0; TargetIP: String = ''): Boolean;
begin
  Result := False;
  try
    try
      if FTCPClient.Connected then raise Exception.Create('Already connected.');
      FTCPClient.Host := ifthen(TargetIP <> '', TargetIP, FIPAddr);
      FTCPClient.Port := ifthen(TargetPort > 0, TargetPort, FIPPort);
      FTCPClient.Connect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.DisconnectStream: Boolean;
begin
  Result := False;
  try
    try
      if FTCPClient.Connected then FTCPClient.Disconnect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.Send(Data: TIDBytes): Boolean;
{Send data with TCP}
begin
  Result := False;
  if FTCPClient.Connected then
    begin
    try
      FTCPClient.IOHandler.WriteDirect(Data);
    finally
      Result := True;
    end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.Receive(var Data: TIDBytes; Count: Integer): Boolean;
{Receive data with TCP}
begin
  Result := False;
  if FTCPClient.IOHandler.Connected then
    begin
    try
      FTCPClient.IOHandler.ReadBytes(Data, Count);
    finally
      Result := True;
    end;
    end;
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.PrepareBuffer(Command: udpCommand; Parameter: String; RequestPacketAck: Boolean);
{Resize and fill TxBuf (pointed to by PTxBuf) with Command, Parameter, and RequestPacketAck attributes.}
var
  ParamLength : Cardinal;
  ParamValue  : Cardinal;
  Temp        : Cardinal;
begin
  ParamLength := 0;
  if Command in udpStrCommands then                                                    {If command is a string-type command}
    ParamLength := Length(Parameter)                                                   {  note length in bytes/characters}
  else
    begin                                                                              {Else}
    if Parameter <> '' then                                                            {  if parameter not empty}
      begin
      Temp := strtoint(Parameter);                                                     {    convert Parameter string to value, and...}
      ParamValue := 0;
      repeat
        Inc(ParamLength);                                                              {    note length in bytes of numeric value (1..4) and }
        ParamValue := (ParamValue shl 8) + (Temp and $FF);                             {    rearrange for big-endian storage}
        Temp := Temp shr 8;
      until Temp = 0;
      end;
    end;
  SetLength(FTxBuf, 8+1+1+2+ParamLength);                                              {Size TxBuf for Application Header (8 bytes) plus FrameID (1) plus ConfigOptions (1) + ATCommand (2) + ParameterValue (0..19)}
  FPTxBuf := PxbTxPacket(@FTxBuf[0]);                                                  {Point PTxBuf at TxBuf}
  FPTxBuf.Number1 := Random($FFFF);                                                    {Randomize Number1 ID}
  FPTxBuf.Number2 := FPTxBuf.Number1 xor $4242;                                        {Prepare Number2 to match requirements}
  FPTxBuf.PacketID := 0;                                                               {PacketID (always 0)}
  FPTxBuf.EncryptionPad := 0;                                                          {EncryptionPad (always 0)}
  FPTxBuf.CommandID := RemoteCommand;                                                  {Set to be remote command}
  FPTxBuf.CommandOptions := ord(RequestPacketAck)*$02;                                 {Set to request (or not request) acknowledgement}
  FPTxBuf.FrameID := FrameID;                                                          {Set Frame ID}
  FPTxBuf.ConfigOptions := ApplyCommand;                                               {Set to apply the command}
  FPTxBuf.ATCommand := ATCmd[Command];                                                 {Set AT Command}
  if True then
  if Parameter <> '' then                                                              {If Parameter not empty}
    if Command in udpStrCommands then
      Move(Parameter, FPTxBuf.ParameterValue[0], ParamLength)                          {  append Parameter as a string}
    else
      Move(ParamValue, FPTxBuf.ParameterValue[0], ParamLength);                        {  or as a numeric value}
end;


{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{-------------------------------- Non-Object Functions and Procedure --------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

function FormatIPAddr(Addr: Cardinal): String;
{Return IP Address in standard string format}
begin
  Result := Format('%d.%d.%d.%d', [Addr shr 24 and $FF, Addr shr 16 and $FF, Addr shr 8 and $FF, Addr and $FF]);
end;

{----------------------------------------------------------------------------------------------------}

function FormatMACAddr(AddrHigh, AddrLow: Cardinal): String;
{Return MAC Address (48-bit number in MacAddrHigh:MaccAddrLow) in standard string format}
var
  Idx : Cardinal;
begin
  Result := Format('%.2x:%.2x:%.2x:%.2x:%.2x:%.2x', [AddrHigh shr 8 and $FF, AddrHigh and $FF, AddrLow shr 24 and $FF, AddrLow shr 16 and $FF, AddrLow shr 8 and $FF, AddrLow and $FF]);
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  Randomize;                                     {Initialize the random seed}

end.
