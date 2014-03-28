unit XBeeWiFi;

interface

uses
  System.SysUtils, System.StrUtils, System.Math,
  FMX.Dialogs,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack;

type
  {Define XBee WiFi's udp commands}
  {IMPORTANT: Do not rearrange, append, or delete from this list without similarly modifying the ATCmd constant array}
  xbCommand = (xbMacHigh, xbMacLow, xbSSID, xbIPAddr, xbIPMask, xbIPGateway, xbIPPort, xbNodeID, xbMaxRFPayload, xbIO2State,
                xbOutputMask, xbOutputState, xbIO2Timer, xbSerialMode, xbSerialBaud, xbSerialParity, xbSerialStopBits, xbChecksum,
                xbData);

const
  {Define XBee WiFi's AT commands}
  ATCmd : array[low(xbCommand)..high(xbCommand)] of Word =
    ({NOTES: [R] - read only, [R/W] = read/write, [s] - string, [b] - binary number, [sb] - string or binary number}
    {xbMacHigh}         Byte('S') + Byte('H') shl 8,  {[Rb] XBee's Mac Address (highest 16-bits)}
    {xbMacLow}          Byte('S') + Byte('L') shl 8,  {[Rb] XBee's Mac Address (lowest 32-bits)}
    {xbSSID}            Byte('I') + Byte('D') shl 8,  {[Rs/Ws] SSID (0 to 31 printable ASCII characters)}
    {xbIPAddr}          Byte('M') + Byte('Y') shl 8,  {[Rb*/Wsb] XBee's IP Address (32-bits); *Read-only in DHCP mode}
    {xbIPMask}          Byte('M') + Byte('K') shl 8,  {[Rb*/Wsb] XBee's IP Mask (32-bits); *Read-only in DHCP mode}
    {xbIPGateway}       Byte('G') + Byte('W') shl 8,  {[Rb*/Wsb] XBee's IP Gateway (32-bits); *Read-only in DHCP mode}
    {xbIPPort}          Byte('C') + Byte('0') shl 8,  {[Rb/Wb] Xbee's UDP/IP Port (16-bits)}
    {xbNodeID}          Byte('N') + Byte('I') shl 8,  {[Rs/Ws] Friendly node identifier string (20 printable ASCII characters)}
    {xbMaxRFPayload}    Byte('N') + Byte('P') shl 8,  {[Rb] Maximum RF Payload (16-bits; in bytes)}
    {xbIO2State}        Byte('D') + Byte('2') shl 8,  {[Rb/Wb] Designated reset pin (3-bits; 0=Disabled, 1=SPI_CLK, 2=Analog input, 3=Digital input, 4=Digital output, 5=Digital output)}
    {xbOutputMask}      Byte('O') + Byte('M') shl 8,  {[Rb/Wb] Output mask for all I/O pins (each 1=output pin, each 0=input pin) (15-bits on TH, 20-bits on SMT)}
    {xbOutputState}     Byte('I') + Byte('O') shl 8,  {[Rb/Wb] Output state for all I/O pins (each 1=high, each 0=low) (15-bits on TH, 20-bits on SMT).  Period affected by updIO2Timer}
    {xbIO2Timer}        Byte('T') + Byte('2') shl 8,  {[Rb/Wb] I/O 2 state timer (100 ms units; $0..$1770)}
    {xbSerialMode}      Byte('A') + Byte('P') shl 8,  {[Rb/Wb] Serial mode (0=Transparent, 1=API wo/Escapes, 2=API w/Escapes)}
    {xbSerialBaud}      Byte('B') + Byte('D') shl 8,  {[Rb/Wb] serial baud rate ($1=2400, $2=4800, $3=9600, $4=19200, $5=38400, $6=57600, $7=115200, $8=230400, $9=460800, $A=921600)}
    {xbSerialParity}    Byte('N') + Byte('B') shl 8,  {[Rb/Wb] serial parity ($0=none, $1=even, $2=odd)}
    {xbSerialStopBits}  Byte('S') + Byte('B') shl 8,  {[Rb/Wb] serial stop bits ($0=one stop bit, $1=two stop bits)}
    {xbChecksum}        Byte('C') + Byte('K') shl 8,  {[Rb] current configuration checksum (16-bits)}
    {xbData}            $0000                         {[Rb/Wb] read/write data stream}
    );

  {Set of XBee Commands that take string parameters (instead of numeric parameters)}
  xbStrCommands : set of xbCommand = [xbNodeID];

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
    FTargetIPAddr : String;                   {IP address to contact}
    FTargetIPPort : Cardinal;                 {IP port to contact}
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
    {XBee configuration methods}
    function  GetItem(Command: xbCommand; var Str: String): Boolean; overload;
    function  GetItem(Command: xbCommand; var StrList: TSimpleStringList): Boolean; overload;
    function  GetItem(Command: xbCommand; var Num: Cardinal): Boolean; overload;
    function  GetItem(Command: xbCommand; var NumList: TSimpleNumberList): Boolean; overload;
    function  SetItem(Command: xbCommand; Str: String): Boolean; overload;
    function  SetItem(Command: xbCommand; Num: Cardinal): Boolean; overload;
    {XBee UDP data methods}
    function  SendUDP(Data: TIDBytes): Boolean;
    function  ReceiveUDP(var Data: TIDBytes; Timeout: Cardinal): Boolean;
    {XBee TCP data methods}
    function  ConnectTCP: Boolean;
    function  DisconnectTCP: Boolean;
    { TODO : Make TCP methods consistent }
    function  SendTCP(Data: TIDBytes): Boolean;
    function  ReceiveTCP(var Data: TIDBytes; Count: Integer): Boolean;
    {Class properties}
    { TODO : Make setter for IPAddr property }
    property TargetIPAddr : String read FTargetIPAddr write FTargetIPAddr;
    property TargetIPPort : Cardinal read FTargetIPPort write FTargetIPPort;
    property LocalUDPPort : Cardinal read GetLocalUDPPort write SetLocalUDPPort;
    property LocalTCPPort : Cardinal read GetLocalTCPPort write SetLocalTCPPort;
    property Timeout : Cardinal read FTimeout write FTimeout;
  private
    { Private declarations }
    procedure PrepareBuffer(Command: xbCommand; ParamStr: String = ''; ParamNum: Integer = -1; ParamData: TIdBytes = nil; RequestPacketAck: Boolean = True);
    function  TransmitUDP(ExpectMultiple: Boolean = False): Boolean;
  end;

  {Non-object functions and procedures}
  function FormatIPAddr(Addr: Cardinal): String;
  function FormatMACAddr(AddrHigh, AddrLow: Cardinal): String;

const
  {Network Header Metrics}
  DataCommand     = $00;
  RemoteCommand   = $02;
  {Command Metrics}
  FrameID         = $01;
  QueueCommand    = $00;
  ApplyCommand    = $02;
  {Serial Modes}
  TransparentMode = $00;
  APIwoEscapeMode = $01;
  APIwEscapeMode  = $02;
  {--I/O--}
  pinOutLow       = $04;
  pinOutHigh      = $05;
  {--Serial--}
  Baud2400        = $01;
  Baud4800        = $02;
  Baud9600        = $03;
  Baud19200       = $04;
  Baud38400       = $05;
  Baud57600       = $06;
  Baud115200      = $07;
  Baud230400      = $08;
  Baud460800      = $09;
  Baud921600      = $0A;
  ParityNone      = $00;
  ParityEven      = $01;
  ParityOdd       = $02;
  StopBits1       = $00;
  StopBits2       = $01;
  {Misc}
  DefaultTimeout  = 1000;

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
  FTCPClient.UseNagle := False;                  {Disable Nagel Timer}
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

function TXBeeWiFi.GetItem(Command: xbCommand; var Str: String): Boolean;
{Retrieve XBee attribute string (Str) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
begin
  Str := '';
  PrepareBuffer(Command);                                                                                    {Prepare command packet}
  Result := TransmitUDP;                                                                                     {and transmit it}
  if Result then Str := StrPas(PAnsiChar(@FResponseList[0].Data));                                           {If response received, copy data to Str}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var StrList: TSimpleStringList): Boolean;
{Retrieve XBee attribute strings (StrList) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
{ TODO : Test GetXBee StrList }
{ TODO : Check all comments }
  SetLength(StrList, 0);
  PrepareBuffer(Command);                                                                                    {Prepare command packet}
  Result := TransmitUDP(True);                                                                               {and transmit it}
  if Result then                                                                                             {If response received}
    begin                                                                                                    {  Copy data to StrList}
    for Idx := 0 to High(FResponseList) do
      begin
      SetLength(StrList, Length(StrList)+1);
      StrList[High(StrList)] := StrPas(PAnsiChar(@FResponseList[0].Data));
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var Num: Cardinal): Boolean;
{Retrieve XBee attribute value (Num) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
  Num := 0;
  PrepareBuffer(Command);                                                                                    {Prepare command packet}
  Result := TransmitUDP;                                                                                     {and transmit it}
  if Result then for Idx := 0 to FResponseList[0].Length-1 do Num := Num shl 8 + FResponseList[0].Data[Idx]; {If response received, convert from big-endian}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var NumList: TSimpleNumberList): Boolean;
{Retrieve XBee attribute values (NumList) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  RIdx : Cardinal;
  NIdx : Cardinal;
begin
  SetLength(NumList, 0);
  PrepareBuffer(Command);                                                                                    {Prepare command packet}
  Result := TransmitUDP(True);                                                                               {and transmit it}
  if Result then                                                                                             {If response received}
    begin                                                                                                    {  Copy data to NumList}
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

function TXBeeWiFi.SetItem(Command: xbCommand; Str: String): Boolean;
{Set XBee attribute to string (Str) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
begin
  PrepareBuffer(Command, Str);                                                                               {Prepare command packet}
  Result := TransmitUDP;                                                                                     {and transmit it}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SetItem(Command: xbCommand; Num: Cardinal): Boolean;
{Set XBee attribute to value (Num) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).}
begin
  PrepareBuffer(Command, '', Num);                                                                           {Prepare command packet}
  Result := TransmitUDP;                                                                                     {and transmit it}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SendUDP(Data: TIDBytes): Boolean;
{Send UDP data packet.  Data must be sized to exactly the number of bytes to transmit.
 Returns True if successful, False if not.}
begin
  PrepareBuffer(xbData, '', -1, Data);                                                                      {Prepare data packet}
  Result := TransmitUDP;                                                                                     {and transmit it}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ReceiveUDP(var Data: TIDBytes; Timeout: Cardinal): Boolean;
{Receive UDP data packet.  Data will be resized to exactly the number of bytes recieved.
 Returns True if successful, False if not.}
begin
{ TODO : Make ReceivedUDP }
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ConnectTCP: Boolean;
begin
  Result := False;
  try
    try
      if FTCPClient.Connected then raise Exception.Create('Already connected.');
      FTCPClient.Host := FTargetIPAddr;
      FTCPClient.Port := FTargetIPPort;
      FTCPClient.Connect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.DisconnectTCP: Boolean;
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

function TXBeeWiFi.SendTCP(Data: TIDBytes): Boolean;
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

function TXBeeWiFi.ReceiveTCP(var Data: TIDBytes; Count: Integer): Boolean;
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

procedure TXBeeWiFi.PrepareBuffer(Command: xbCommand; ParamStr: String = ''; ParamNum: Integer = -1; ParamData: TIdBytes = nil;  RequestPacketAck: Boolean = True);
{Resize and fill TxBuf (pointed to by PTxBuf) with Command, Parameter (Str, Num, or Data), and RequestPacketAck attributes.}
var
  ParamLength : Cardinal;
  ParamValue  : Cardinal;
begin
{ TODO : Make PrepareBuffer error out if Param data doesn't match command type }
  if Command in xbStrCommands then                                                    {If command is a string-type command}
    ParamLength := Length(ParamStr)                                                    {  note length in bytes/characters}
  else                                                                                 {Else}
    if Command = xbData then                                                          {  If command is a data command}
      ParamLength := ifthen(assigned(ParamData), Length(ParamData), 0)                 {    note length of data buffer}
    else                                                                               {  Else}
      begin
      ParamLength := 0;
      if ParamNum > -1 then                                                            {    If number in range}
        begin                                                                          {      note length in bytes of numeric value (1..4) and }
        ParamValue := 0;                                                               {      rearrange for big-endian storage}
        repeat
          Inc(ParamLength);
          ParamValue := (ParamValue shl 8) + (ParamNum and $FF);
          ParamNum := ParamNum shr 8;
        until ParamNum = 0;
        end;
      end;
{ TODO : Protect against too big a data buffer (ParamData) }
  SetLength(FTxBuf, 8 + ord(Command <> xbData)*(1+1+2) + ParamLength);                {Size TxBuf for Application Header (8 bytes) plus (if not Data command; FrameID (1) and ConfigOptions (1) and ATCommand (2)),  plus ParameterValue (0+)}
  FPTxBuf := PxbTxPacket(@FTxBuf[0]);                                                  {Point PTxBuf at TxBuf}
  FPTxBuf.Number1 := Random($FFFF);                                                    {Randomize Number1 ID}
  FPTxBuf.Number2 := FPTxBuf.Number1 xor $4242;                                        {Prepare Number2 to match requirements}
  FPTxBuf.PacketID := 0;                                                               {PacketID (always 0)}
  FPTxBuf.EncryptionPad := 0;                                                          {EncryptionPad (always 0)}
  FPTxBuf.CommandID := ifthen(Command <> xbData, RemoteCommand, DataCommand);         {Set to be remote command or data command}
  FPTxBuf.CommandOptions := ord(RequestPacketAck)*$02;                                 {Set to request (or not request) acknowledgement}
  if Command <> xbData then                                                           {If not a data stream}
    begin
    FPTxBuf.FrameID := FrameID;                                                        {  Set Frame ID}
    FPTxBuf.ConfigOptions := ApplyCommand;                                             {  Set to apply the command}
    FPTxBuf.ATCommand := ATCmd[Command];                                               {  Set AT Command}
    end;
  if ParamLength > 0 then                                                              {If Parameter not empty}
    if Command in xbStrCommands then
      Move(ParamStr, FPTxBuf.ParameterValue[0], ParamLength)                           {  append Parameter as a string}
    else
      if Command = xbData then
        Move(ParamData[0], FPTxBuf.FrameID, ParamLength)                               {  or as a data stream}
      else
        Move(ParamValue, FPTxBuf.ParameterValue[0], ParamLength);                      {  or as a numeric value}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.TransmitUDP(ExpectMultiple: Boolean = False): Boolean;
{Transmit UDP packet already prepared by a call to PrepareBuffer.
 Returns True if successful, False otherwise.
 Set ExpectMultiple true if multiple responses possible, such as when a broadcast packet is being transmitted.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
var
  RLIdx         : Integer;              // response list index
  IP            : String;
  RequiredRx    : Cardinal;             // bit 0 = Packet ACK received, bit 1 = Command response received
  Status        : Cardinal;             // $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter
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
{ TODO : Enhance SetItem log errors }
//  self.Caption := 'Transmitting...';
  Result := False;                                                                                           {Initialize result to false}
  RequiredRx := 0;                                                                                           {Initialize required reception to none}
  Status := 0;                                                                                               {Initialize status}
  RLIdx := -1;                                                                                               {Clear response list index}
  SetLength(FResponseList, 0);                                                                               {Clear the response list}
  try
    {Try to transmit; IP exceptions handled}
    FUDPClient.SendBuffer(FTargetIPAddr, $BEE, FTxBuf);                                                      {Send to TargetIP or GUI-displayed IP}
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
