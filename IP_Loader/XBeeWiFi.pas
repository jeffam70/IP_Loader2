unit XBeeWiFi;

{XBee Wi-Fi communication unit.  This unit, and the TXBeeWiFi class within it, facilitates UDP and TCP communication with an XBee Wi-Fi module
for purposes of configuring the module and transceiving data through its UART port.

A combination of the XBee's "XBee Application Service" and its "Serial Communication Service" is used.
}

{ TODO : Check all comments }
{ TODO : Resolve all warnings }

interface

uses
  System.SysUtils, System.StrUtils, System.Math,
{  mmsystem,}
  FMX.Dialogs,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack,
  Time,
  Debug;

type
  {Define XBee WiFi's udp commands}
  {IMPORTANT: Do not rearrange, append, or delete from this list without similarly modifying the ATCmd constant array}
  xbCommand = (xbData, xbMacHigh, xbMacLow, xbSSID, xbIPAddr, xbIPMask, xbIPGateway, xbIPPort, xbIPDestination, xbNodeID, xbMaxRFPayload, xbIO2State,
               xbOutputMask, xbOutputState, xbIO2Timer, xbSerialMode, xbSerialBaud, xbSerialParity, xbSerialStopBits, xbSerialIP, xbChecksum);

const
  {Define XBee WiFi's AT commands}
  ATCmd : array[low(xbCommand)..high(xbCommand)] of Word =
    ({NOTES: [R] - read only, [R/W] = read/write, [s] - string, [b] - binary number, [sb] - string or binary number}
    {xbData}            $0000,                        {[Wb] write data stream}
    {xbMacHigh}         Byte('S') + Byte('H') shl 8,  {[Rb] XBee's Mac Address (highest 16-bits)}
    {xbMacLow}          Byte('S') + Byte('L') shl 8,  {[Rb] XBee's Mac Address (lowest 32-bits)}
    {xbSSID}            Byte('I') + Byte('D') shl 8,  {[Rs/Ws] SSID (0 to 31 printable ASCII characters)}
    {xbIPAddr}          Byte('M') + Byte('Y') shl 8,  {[Rb*/Wsb] XBee's IP Address (32-bits; IPv4); *Read-only in DHCP mode}
    {xbIPMask}          Byte('M') + Byte('K') shl 8,  {[Rb*/Wsb] XBee's IP Mask (32-bits); *Read-only in DHCP mode}
    {xbIPGateway}       Byte('G') + Byte('W') shl 8,  {[Rb*/Wsb] XBee's IP Gateway (32-bits); *Read-only in DHCP mode}
    {xbIPPort}          Byte('C') + Byte('0') shl 8,  {[Rb/Wb] Xbee's UDP/IP Port (16-bits)}
    {xbIPDestination}   Byte('D') + Byte('L') shl 8,  {[Rb/Wsb] Xbee's serial-to-IP destination address (32-bits; IPv4)}
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
    {xbSerialIP}        Byte('I') + Byte('P') shl 8,  {[Rb/Wb] Protocol for serial service (0=UDP, 1=TCP)}
    {xbChecksum}        Byte('C') + Byte('K') shl 8   {[Rb] current configuration checksum (16-bits)}
    );

  {Set of XBee Commands that take string parameters only (instead of numeric parameters)}
  xbStrCommands : set of xbCommand = [xbSSID, xbNodeID];

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
    FSerTCPClient    : TIdTCPClient;             {TCP Client for XBee's Serial Communication Service (usually port $2616 (9750))}
    FSerUDPClient    : TIdUDPClient;             {UDP Client for XBee's Serial Communication Service (usually port $2616 (9750))}
    FAppUDPClient    : TIdUDPClient;             {UDP Client for XBee's Application Service (always port $BEE (3054))}
    FTime            : TTime;                    {Time object}
    {IP Buffer}
    FTxBuf           : TIdBytes;                 {Raw transmit packet (resized per packet)}
    FRxBuf           : TIdBytes;                 {Raw receive packet stream (fixed to MaxPacketSize)}
    FPTxBuf          : PxbTxPacket;              {Pointer to structured transmit packet (shares memory space with TxBuf)}
    FPRxBuf          : PxbRxPacket;              {Pointer to structured receive packet (shares memory space with RxBuf)}
    FResponseList    : array of TResponse;       {Dynamic array of received responses (from packet's parameter field); multiple XBee may respond to broadcast message}
    FMaxDataSize     : Cardinal;                 {Maximum size allowed for data (payload) of packet}
    FUDPRoundTrip    : Integer;                  {Measured round-trip time for UDP Application Packets; -1 means never received expected response}
    FMaxUDPRoundTrip : Integer;                  {The maximum UDP round-trip time ever measured for the device}
    {Getters and Setters}
    function  GetRemoteIPAddr: String;
    procedure SetRemoteIPAddr(Value: String);
    function  GetRemoteSerIPPort: Cardinal;
    procedure SetRemoteSerIPPort(Value: Cardinal);
    function  GetTimeout(Index: Integer): Integer;
    procedure SetTimeout(Index, Value: Integer);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy;
    {XBee Application Service configuration methods}
    function  GetItem(Command: xbCommand; var Str: String): Boolean; overload;                             {Get string value}
    function  GetItem(Command: xbCommand; var StrList: TSimpleStringList): Boolean; overload;              {Get list of string values}
    function  GetItem(Command: xbCommand; var Num: Cardinal): Boolean; overload;                           {Get numeric value}
    function  GetItem(Command: xbCommand; var NumList: TSimpleNumberList): Boolean; overload;              {Get list of numeric values}
    function  SetItem(Command: xbCommand; Str: String): Boolean; overload;                                 {Set string value}
    function  SetItem(Command: xbCommand; Num: Cardinal): Boolean; overload;                               {Set numeric value}
    {XBee UDP data methods}
    { TODO : Resolve TCP and UDP serial vs application names from interface perspective. }
    function  ConnectSerialUDP: Boolean;                                                                   {Connect UDP channel to Serial Service}
    function  DisconnectSerialUDP: Boolean;                                                                {Disconnect UDP channel from Serial Service}
    function  SendUDP(Data: TIDBytes; UseAppService: Boolean = True; AutoRetry: Boolean = True): Boolean;  {Send data with UDP; over Application Service (normally) or Serial Serivce}
    function  ReceiveUDP(var Data: TIDBytes; Timeout: Cardinal): Boolean;                                  {Receive data with UDP; over Serial Service only}
    {XBee TCP data methods}
    function  ConnectTCP: Boolean;                                                                         {Connect TCP channel to Serial Service}
    function  DisconnectTCP: Boolean;                                                                      {Disconnect TCP channel from Serial Service}
    function  SendTCP(Data: TIDBytes): Boolean;                                                            {Send data with TCP; over Serial Service}
    function  ReceiveTCP(var Data: TIDBytes; Timeout: Cardinal): Boolean;                                  {Receive data with TCP; over Serial Service}
    {Class properties}
    property RemoteIPAddr : String read GetRemoteIPAddr write SetRemoteIPAddr;                             {IP address of XBee to contact (for both Serial and Application services)}
    property RemoteSerialIPPort : Cardinal read GetRemoteSerIPPort write SetRemoteSerIPPort;               {IP port of XBee to contact (for Serial service only; Application service's port is fixed)}
//    property LocalUDPPort : Cardinal read GetLocalUDPPort write SetLocalUDPPort;
//    property LocalTCPPort : Cardinal read GetLocalTCPPort write SetLocalTCPPort;
    property SerialTimeout : Integer index 0 read GetTimeout write SetTimeout;                             {Read-timeout for serial service}
    property ApplicationTimeout : Integer index 1 read GetTimeout write SetTimeout;                        {Read-timeout for application service}
    property UDPRoundTrip : Integer read FUDPRoundTrip;                                                    {Get last round-trip time for UDP App Packets for this device}
{ TODO : Ensure UDPRoundTrip and MaxUDPRoundTrip is reset with device changes. }
    property UDPMaxRoundTrip : Integer read FMaxUDPRoundTrip;                                              {Get maximum rount-trip time for UDP App Packets for this device}
    { TODO : Determin if MaxDataSize property should retrieve the actual value when called. }
    property MaxDataSize : Cardinal read FMaxDataSize;                                                     {Get maximum packet payload size}
  private
    { Private declarations }
    {XBee Application Service buffer transmit methods}
    procedure PrepareAppBuffer(Command: xbCommand; ParamStr: String = ''; ParamNum: Int64 = -1; ParamData: TIdBytes = nil);
    function  TransmitAppUDP(ExpectMultiple: Boolean = False; AutoRetry: Boolean = True): Boolean;
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
  {IP Modes}
  SerialUDP       = $00;
  SerialTCP       = $01;
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
  DefaultSerialTimeout      = 2000;
  DefaultApplicationTimeout = 1000;
  DefaultBufferSize         = 1500;              {Default size for receive buffer(s)}
  DefaultMaxDataSize        = 1392;

var
  ctTime  : Int64;                               {Holds start time for CheckTime function}
  ctDelay : Cardinal;                            {Holds desired delay (in milliseconds) for CheckTime function}

implementation

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------- Protected Methods ----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetRemoteIPAddr: String;
{Get IP address of XBee to contact (for both Serial and Application services)}
begin
  Result := FSerTCPClient.Host;
end;

{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.SetRemoteIPAddr(Value: String);
{Set IP address of XBee to contact (for both Serial and Application services)}
begin
  if (Value <> FSerTCPClient.Host) then
    begin
    FSerTCPClient.Host := Value;                     {Set Serial Services' IP address}
    FSerUDPClient.Host := Value;
    FAppUDPClient.Host := Value;                     {Set Application Service's IP address}
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetRemoteSerIPPort: Cardinal;
{Get IP port of XBee to contact (for Serial service only; Application service's port is fixed)}
begin
  Result := FSerTCPClient.Port;
end;

{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.SetRemoteSerIPPort(Value: Cardinal);
{Set IP port of XBee to contact (for Serial service only; Application service's port is fixed)}
begin
  if (Value <= $10000) and (Value <> FSerTCPClient.Port) then
    begin
    FSerTCPClient.Port := Value;                     {Set Serial Services' IP address}
    FSerUDPClient.Port := Value;
    FSerUDPClient.BoundPort := Value;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetTimeout(Index: Integer): Integer;
{Get read-timeout of Serial or Application service}
begin
  case Index of
    0: Result := FSerTCPClient.ReadTimeout;          {Get Serial Services' read timeout}
    1: Result := FAppUDPClient.ReceiveTimeout;       {Get Serial Service's read timeout}
  end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TXBeeWiFi.SetTimeout(Index, Value: Integer);
{Set read-timeout for Serial or Application service}
begin
  case Index of
    0: if (Value <> FSerTCPClient.ReadTimeout) then
         begin
         FSerTCPClient.ReadTimeout := Value;                                                     {Set Serial Services' read timeout}
         FSerUDPClient.ReceiveTimeout := Value;
         end;
    1: if (Value <> FAppUDPClient.ReceiveTimeout) then FAppUDPClient.ReceiveTimeout := Value;    {Set Application Service's read timeout}
  end;
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------- Public Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

constructor TXBeeWiFi.Create;
begin
  FSerTCPClient := TIdTCPClient.Create;                      {Create Serial TCP Client}
  FSerTCPClient.UseNagle := False;                           {Disable its Nagel Timer}
  FSerTCPClient.ReadTimeout := DefaultSerialTimeout;         {Set its read-timeout}
  FSerUDPClient := TIdUDPClient.Create;                      {Create Serial UDP Client}
  FSerUDPClient.ReceiveTimeout := DefaultSerialTimeout;      {Set its read-timeout}
  FAppUDPClient := TIdUDPClient.Create;                      {Create Application UDP Client}
  FAppUDPClient.BoundPort := $BEE;                           {Bind Application UDP Client to port $BEE (local and remote side)}
  FAppUDPClient.Port := $BEE;
  FAppUDPClient.ReceiveTimeout := DefaultApplicationTimeout; {Set it's read-timeout}
  FTime := TTime.Create;                                     {Create Time object}
  SetLength(FRxBuf, DefaultBufferSize);                      {Set receive buffer length}
  FPRxBuf := PxbRxPacket(@FRxBuf[0]);                        {Point PRxBuf at RxBuf}
 { TODO : Consider determining real max data packet size from module. }
  FMaxDataSize := DefaultMaxDataSize;                        {Set data packet size to default}
{ TODO : Reset different items when XBee target changed. }
  FUDPRoundTrip := -1;                                        {Initialize round-trip measurement};
  FMaxUDPRoundTrip := 0;
end;

{----------------------------------------------------------------------------------------------------}

destructor TXBeeWiFi.Destroy;
begin
  FSerTCPClient.Destroy;                         {Destroy Serial TCP Client}
  FSerUDPClient.Destroy;                         {Destroy Serial UDP Client}
  FAppUDPClient.Destroy;                         {Destroy Application UDP Client}
  FTime.Destroy;                                 {Destroy Time object}
  SetLength(FRxBuf, 0);                          {Free RxBuf memory}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var Str: String): Boolean;
{Retrieve XBee attribute string (Str) and return True if successful; False otherwise.
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
begin
  Str := '';
  PrepareAppBuffer(Command);                                                                                 {Prepare command packet}
  Result := TransmitAppUDP;                                                                                  {and transmit it}
  if Result then Str := StrPas(PAnsiChar(@FResponseList[0].Data));                                           {If response received, copy data to Str}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var StrList: TSimpleStringList): Boolean;
{Retrieve XBee attribute strings (StrList) and return True if successful; False otherwise.
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
var
  Idx : Cardinal;
begin
{ TODO : Test GetXBee StrList }
  SetLength(StrList, 0);
  PrepareAppBuffer(Command);                                                                                 {Prepare command packet}
  Result := TransmitAppUDP(True);                                                                            {and transmit it}
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
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
var
  Idx : Cardinal;
begin
  Num := 0;
  PrepareAppBuffer(Command);                                                                                 {Prepare command packet}
  Result := TransmitAppUDP;                                                                                  {and transmit it}
  if Result then for Idx := 0 to FResponseList[0].Length-1 do Num := Num shl 8 + FResponseList[0].Data[Idx]; {If response received, convert from big-endian}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.GetItem(Command: xbCommand; var NumList: TSimpleNumberList): Boolean;
{Retrieve XBee attribute values (NumList) and return True if successful; False otherwise.
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
var
  RIdx : Cardinal;
  NIdx : Cardinal;
begin
  SetLength(NumList, 0);
  PrepareAppBuffer(Command);                                                                                 {Prepare command packet}
  Result := TransmitAppUDP(True);                                                                            {and transmit it}
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
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
begin
  PrepareAppBuffer(Command, Str);                                                                            {Prepare command packet}
  Result := TransmitAppUDP;                                                                                  {and transmit it}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SetItem(Command: xbCommand; Num: Cardinal): Boolean;
{Set XBee attribute to value (Num) and return True if successful; False otherwise.
 This method sends a UDP packet over the XBee's Application Service and checks and validates the response (if any).}
begin
  PrepareAppBuffer(Command, '', Num);                                                                        {Prepare command packet}
  Result := TransmitAppUDP;                                                                                  {and transmit it}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ConnectSerialUDP: Boolean;
{Connect serial service UDP channel to already-set RemoteIP/RemoteIPPort metrics}
begin
  Result := False;
  try
    try
      if not FSerUDPClient.Connected then FSerUDPClient.Connect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.DisconnectSerialUDP: Boolean;
{Disconnect serial service UDP channel}
begin
  Result := False;
  try
    try
      if FSerUDPClient.Connected then FSerUDPClient.Disconnect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.SendUDP(Data: TIDBytes; UseAppService: Boolean = True; AutoRetry: Boolean = True): Boolean;
{Send UDP data packet to XBee's UART.  Data must be sized to exactly the number of bytes to transmit.
 This normally uses the Application Service to verify the data packet was received (packet acknowlegement) and retransmits if needed.
 Set UseAppService to False to use Serial Service instead (no verified receipt) and no automatic retransmission.
 Set AutoRetry to false to optionally prevent automatic retries (when UseAppService is active).
 Returns True if successful, False if not.}
var
  TempTxBuff     : TIdBytes;  {Temporary transmission buffer used only when Data is too big}
  Idx            : Integer;   {Holds index of next byte to transmit}
  PacketSize     : Cardinal;  {Holds size of next packet being transmitted}
//  MaxSize        : Cardinal;

    {----------------}

    function Send(Buffer: TIdBytes): Boolean;
    begin
      Result := False;
      if UseAppService then                                                                      {Use Application Service?}
        begin {Prep and send data using Application Service}
        PrepareAppBuffer(xbData, '', -1, Buffer);                                                {  Prepare data packet}
        Result := TransmitAppUDP(False, AutoRetry);                                              {  and transmit it using Application Service}
        end
      else
        begin                                                                                    {Else}
        FSerUDPClient.SendBuffer(Buffer);                                                        {  Transmit it using Serial Service}
        Result := True;
        end;
    end;

    {----------------}

begin
  Result := False;
  if not Assigned(Data) then raise Exception.Create('Error: Need pointer to data.');
  if Length(Data) = 0 then raise Exception.Create('Error: Data is empty.');
  if Length(Data) <= FMaxDataSize then                                                           {Data small enough for a single packet?}
    Result := Send(Data)                                                                         {  Send data in full}
  else                                                                                           {Else, break up into multiple timed packets}
    begin
    try

//      PrevPacketSize := 1;                                                                       {  Prep for first packet}
//      NextPacketSize := FMaxDataSize;
//      Idx := -PrevPacketSize;
//      FUDPRoundTrip := -1;
//      while (Idx < 0) and (PrevPacketSize > 0) and Send(TempTxBuff) do                           {  Loop for all necessary packets}
//        begin {While first packet not sent and latest packet sent and more to go}
//        if Idx > -1 then sleep(Trunc(FMaxDataSize*0.30*10/115200*1000-FUDPRoundTrip));           {    Wait for 30% of "full data" to transmit out UART}
//        inc(Idx, PrevPacketSize);                                                                {    Position for start of next packet}
//        SetLength(TempTxBuff, NextPacketSize);                                                   {    Set proper packet size}
//        Move(Data[Idx], TempTxBuff[0], NextPacketSize);                                          {    Prepare packet}
//        PrevPacketSize := NextPacketSize;
//        NextPacketSize := Min(Trunc(FMaxDataSize*0.30), Length(Data)-Idx-PrevPacketSize);        {    Ensure proper packet size for next time}
//        end;
//      Result := Idx+PrevPacketSize = Length(Data);                                               {  Return appropriate result}

      Idx := 0;                                                                                    {  Prep for first packet}
      PacketSize := FMaxDataSize;
      repeat                                                                                       {  Loop for all necessary packets}
//        SendDebugMessage('Iterate', True);
        SetLength(TempTxBuff, PacketSize);                                                         {    Else, set proper packet size}
        Move(Data[Idx], TempTxBuff[0], PacketSize);                                                {    Prepare packet}
{ TODO : Make SendUDP adjust to baud rate }
//        SendDebugMessage('Pausing: ' + inttostr(Max(Trunc(FMaxDataSize*0.30*11/115200*1000-FUDPRoundTrip), 0)) + ' ms.  FUDPRoundTrip: ' + inttostr(FUDPRoundTrip), True);
        if Idx > 0 then IndySleep(Max(Trunc(FMaxDataSize*0.30*11/115200*1000-FUDPRoundTrip), 0));  {    If not first packet, wait for 30% of "full data" to transmit out UART}
        if not Send(TempTxBuff) then break;                                                        {    Send packet; exit if failure}
        inc(Idx, PacketSize);                                                                      {    Prep for start of next packet}
        PacketSize := Min(Trunc(FMaxDataSize*0.30), Length(Data)-Idx);                             {    Next packet will be maximum of 30% of "full data," so as not to overflow XBee UART buffer}
        Result := PacketSize = 0;                                                                  {    Total success? (all packets successfully sent)}
      until Result;                                                                                {    Loop until done}
    finally
      SetLength(TempTxBuff, 0);
//      SendDebugMessage('Exiting', True);
    end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ReceiveUDP(var Data: TIDBytes; Timeout: Cardinal): Boolean;
{Receive UDP data packet.  Data will be resized to exactly fit the received byte stream.
 This always uses the Serial Service.
 Returns True if successful, False if not.}
var
  Count : Integer;
begin
  if not FSerUDPClient.Connected then raise Exception.Create('Error: Serial UDP socket must first be connected.');
  SetLength(Data, DefaultBufferSize);                                                                        {Resize buffer to standard max packet size}
  FTime.Left(Timeout);
  repeat
    Count := FSerUDPClient.ReceiveBuffer(Data, Timeout);
  until (Count > 0) or (FTime.Left = 0);
  SetLength(Data, Count);                                                                                    {Receive data and resize buffer to exactly fit it}
  Result := Count > 0;
  SendDebugMessage('          - ' + ifthen(Result, 'Received ' + Count.ToString + ' byte(s)', 'No response!'), True);
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ConnectTCP: Boolean;
{Connect serial service TCP channel to already-set RemoteIP/RemoteIPPort metrics}
begin
  Result := False;
  try
    try
      if not FSerTCPClient.Connected then FSerTCPClient.Connect;
    except
      Result := False;
    end;
  finally
    Result := True;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.DisconnectTCP: Boolean;
{Disconnect serial service TCP channel}
begin
  Result := False;
  try
    try
      if FSerTCPClient.Connected then FSerTCPClient.Disconnect;
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
  if FSerTCPClient.Connected then
    begin
    try
      FSerTCPClient.IOHandler.WriteDirect(Data);
    finally
      Result := True;
    end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.ReceiveTCP(var Data: TIDBytes; Timeout: Cardinal): Boolean;
{Receive data with TCP}
begin
  Result := False;
  if FSerTCPClient.IOHandler.Connected then
    begin
    try
      FSerTCPClient.IOHandler.ReadBytes(Data, -1);
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

procedure TXBeeWiFi.PrepareAppBuffer(Command: xbCommand; ParamStr: String = ''; ParamNum: Int64 = -1; ParamData: TIdBytes = nil);
{Resize and fill TxBuf (pointed to by PTxBuf) with Command and Parameter (Str, Num, or Data).}
var
  ParamLength : Cardinal;
  ParamValue  : Cardinal;
begin
{ TODO : Make PrepareBuffer error out if Param data doesn't match command type }
  if Command in xbStrCommands then                                                    {If command is a string-type command}
    ParamLength := Length(ParamStr)                                                   {  note length in bytes/characters}
  else                                                                                {Else}
    if Command = xbData then                                                          {  If command is a data command}
      ParamLength := ifthen(assigned(ParamData), Length(ParamData), 0)                {    note length of data buffer}
    else                                                                              {  Else}
      begin
      ParamLength := 0;
      if ParamNum > -1 then                                                           {    If number in range}
        begin                                                                         {      note length in bytes of numeric value (1..4) and }
        ParamValue := 0;                                                              {      rearrange for big-endian storage}
        repeat
          Inc(ParamLength);
//          ParamLength := 2;
          ParamValue := (ParamValue shl 8) + (ParamNum and $FF);
          ParamNum := ParamNum shr 8;
        until ParamNum = 0;
        end;
      end;
{ TODO : Protect against too big a data buffer (ParamData) }
  SetLength(FTxBuf, 8 + ord(Command <> xbData)*(1+1+2) + ParamLength);                {Size TxBuf for Application Header (8 bytes) plus (if not Data command; FrameID (1) and ConfigOptions (1) and ATCommand (2)),  plus ParameterValue (0+)}
  FPTxBuf := PxbTxPacket(@FTxBuf[0]);                                                 {Point PTxBuf at TxBuf}
  FPTxBuf.Number1 := Random($FFFF);                                                   {Randomize Number1 ID}
  FPTxBuf.Number2 := FPTxBuf.Number1 xor $4242;                                       {Prepare Number2 to match requirements}
  FPTxBuf.PacketID := 0;                                                              {PacketID (always 0)}
  FPTxBuf.EncryptionPad := 0;                                                         {EncryptionPad (always 0)}
  FPTxBuf.CommandID := ifthen(Command <> xbData, RemoteCommand, DataCommand);         {Set to be remote command or data command}
  FPTxBuf.CommandOptions := $02;                                                      {Set to request packet acknowledgement}
  if Command <> xbData then                                                           {If not a data stream}
    begin
    FPTxBuf.FrameID := FrameID;                                                       {  Set Frame ID}
    FPTxBuf.ConfigOptions := ApplyCommand;                                            {  Set to apply the command}
    FPTxBuf.ATCommand := ATCmd[Command];                                              {  Set AT Command}
    end;
  if ParamLength > 0 then                                                             {If Parameter not empty}
    if Command in xbStrCommands then
      Move(ParamStr, FPTxBuf.ParameterValue[0], ParamLength)                          {  append Parameter as a string}
    else
      if Command = xbData then
        Move(ParamData[0], FPTxBuf.FrameID, ParamLength)                              {  or as a data stream}
      else
        Move(ParamValue, FPTxBuf.ParameterValue[0], ParamLength);                     {  or as a numeric value}
end;

{----------------------------------------------------------------------------------------------------}

function TXBeeWiFi.TransmitAppUDP(ExpectMultiple: Boolean = False; AutoRetry: Boolean = True): Boolean;
{Transmit UDP packet (which should have already been prepared by a call to PrepareBuffer).
 Returns True if successful, False otherwise.  NOTE: Returns False only if AutoRetry is True and expected responses not received.
 Set ExpectMultiple true if multiple responses possible, such as when a packet is being broadcast to multiple XBee modules.
 This method creates and sends a UDP packet over the XBee's Application Service, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.
 If AutoRetry is true, the packet is automatically retransmitted if expected response(s) not received.}
var
  RLIdx         : Integer;              // response list index
//  IP            : String;
  RequiredRx    : Cardinal;             // bit 0 = Packet ACK received, bit 1 = Command response received
  Count         : Cardinal;             // Number of bytes received in response
  Status        : Cardinal;             // $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter
  TxCount       : Cardinal;             // Number of transmissions to try
const
  {Required Rx codes}
  PacketAck  = $01;                     {RequiredRx's code for packet ACK received}
  CommandRsp = $02;                     {RequiredRx's code for command response received}
  MuliRsp    = $04;                     {Code for multiple responses possible}

    {----------------}

    function AppUDPResponse: Boolean;
    {Unless we've received all the expected packets, check for another UDP packet for up to FTimeout milliseconds.
    Return True/False (packet received) and store packet size in Count.}
    begin
      Result := RequiredRx < PacketAck + ord(FPTxBuf.CommandID <> DataCommand)*CommandRsp + ord(ExpectMultiple)*MuliRsp;
      if Result then
        begin
        Count := FAppUDPClient.ReceiveBuffer(FRxBuf);
        Result := Count > 0;
        end;
    end;

    {----------------}

begin
{ TODO : Remove debugging Info }
{ TODO : Enhance SetItem log errors }
//  self.Caption := 'Transmitting...';
  try
    TxCount := 1 + 2*ord(AutoRetry);                                                                           {Set max number of transmissions; just once if AutoRetry False}
    repeat                                                                                                     {Loop (in case of retransmission)}
      Result := False;                                                                                         {  Initialize result to false}
      RequiredRx := 0;                                                                                         {  Initialize required reception to none}
      Status := 0;                                                                                             {  Initialize status}
      RLIdx := -1;                                                                                             {  Clear response list index}
      SetLength(FResponseList, 0);                                                                             {  Clear the response list}
      {Try to transmit; IP exceptions handled}
      FUDPRoundTrip := Ticks;                                                                                  {  Note start time for round-trip measurement}
      FAppUDPClient.SendBuffer(FTxBuf);                                                                        {  Send to Remote IP}
      {Transmitted fine, retrieve}
      while AppUDPResponse do                                                                                  {  For every App Service UDP Packet received}
        begin {Process each UDP packet received}
        if FPRxBuf.Number1 xor $4242 = FPRxBuf.Number2 then                                                    {  if packet is an XBee Wi-Fi response packet (Number2 is Number1 ^ $4242)}
          begin {It's an XBee Response packet}
          if (FPRxBuf.CommandID = $80) then RequiredRx := RequiredRx or PacketAck;                             {    Note when we received XBee Wi-Fi UDP ACK packet}
          if (FPRxBuf.CommandID = (FPTxBuf.CommandID or $80)) and (FPRxBuf.ATCommand = FPTxBuf.ATCommand) then
            begin
            RequiredRx := RequiredRx or CommandRsp or (FPRxBuf.Status shl 2);                                  {      Note when we received XBee Wi-Fi UDP command response packet}
            Status := Status or FPRxBuf.Status;
            inc(RLIdx);
            SetLength(FResponseList, RLIdx+1);                                                                 {      Make room for response}
            FResponseList[RLIdx].Length := Count-(sizeof(TxbRxPacket)-sizeof(FPRxBuf.ParameterValue));         {      Note response length}
            FResponseList[RLIdx].Status := FPRxBuf.Status;                                                     {      Note response status}
            Move(FPRxBuf.ParameterValue, FResponseList[RLIdx].Data, FResponseList[RLIdx].Length);              {      Save response data}
            FResponseList[RLIdx].Data[FResponseList[RLIdx].Length] := 0;                                       {      Null-terminate (in case it's a string)}
            end;
          end;
        end;
      dec(TxCount);
      Result := ((RequiredRx >= PacketAck + ord(FPTxBuf.CommandID <> DataCommand)*CommandRsp) and (Status = 0));
    until Result or (TxCount = 0);
    {Adjust Result if AutoRetry is False}
    Result := Result or not AutoRetry;
//    if TxCount < 2 then SendDebugMessage('Retried: ' + inttostr(2-TxCount), True);
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
    FUDPRoundTrip := ifthen(Result, GetTickDiff(FUDPRoundTrip, Ticks), -1);                                  {Calculate round-trip time (milliseconds)}
{ TODO : Make FUDPRoundTrip and FMaxUDPRoundTrip reset when remote target is changed. }
    FMaxUDPRoundTrip := Max(FMaxUDPRoundTrip, FUDPRoundTrip);                                                {Record largest round-trip time (milliseconds)}
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
begin
  Result := Format('%.2x:%.2x:%.2x:%.2x:%.2x:%.2x', [AddrHigh shr 8 and $FF, AddrHigh and $FF, AddrLow shr 24 and $FF, AddrLow shr 16 and $FF, AddrLow shr 8 and $FF, AddrLow and $FF]);
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  Randomize;                                     {Initialize the random seed}

end.
