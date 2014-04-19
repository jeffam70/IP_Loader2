unit Debug;

{Version 1.2}

{NOTE: This code is written to work with Delphi XE or above.  Use Version 1.1 for Delphi 6.}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Math, System.SysUtils,
  Vcl.Dialogs;

  procedure SendDebugMessage(Msg: ANSIString = 'xxxxxxxx'; IsText: Boolean = False); overload;
  procedure SendDebugMessage(NumMsg, AlphaMsg: ANSIString); overload;
  procedure SetDebugNumber(NumPattern: ANSIString = 'xxxxxxxx');
  function  GetDebugNumber: Integer;
  procedure GetDebugNumberComponents(var B : array of Byte);
  function  MailSlotOpen: Boolean;

var
  MailSlot : THandle;
  DNum     : Integer;  {Current Debug Number}

const
  MaxSize = 424;
  MailSlotPath = '\\.\mailslot\ParallaxDebugger';                {Parallax Debugger mail slot path and name}


implementation

procedure SendDebugMessage(Msg: ANSIString = 'xxxxxxxx'; IsText: Boolean = False); overload;
{Send a numeric message (IsText = False) or a text message (IsText = True)}
var
  BytesWritten : Cardinal;
begin
  if not MailSlotOpen then exit;
  if IsText then
    begin
    if not writefile(MailSlot, Msg[1], min(length(Msg), MaxSize), BytesWritten, nil) then
      begin  {If failed, maybe debugger was closed and reopened, invalidate MailSlot and try once more}
      MailSlot := INVALID_HANDLE_VALUE;
      SendDebugMessage(Msg, True);
      end;
    end
  else
    begin
    SetDebugNumber(Msg);
    SendDebugMessage(ANSIChar(0) + ANSIChar(DNum and $FF) + ANSIChar((DNum shr 8) and $FF) + ANSIChar((DNum shr 16) and $FF) + ANSIChar((DNum shr 24) and $FF), True);
    end;
end;

{------------------------------------------------------------------------------}

procedure SendDebugMessage(NumMsg, AlphaMsg: ANSIString); overload;
{Send a combined numeric message and text message.}
begin
  SetDebugNumber(NumMsg);
  if not MailSlotOpen then exit;
  SendDebugMessage(ANSIChar(1) + ANSIChar(DNum and $FF) + ANSIChar((DNum shr 8) and $FF) + ANSIChar((DNum shr 16) and $FF) + ANSIChar((DNum shr 24) and $FF) + AlphaMsg, True);
end;

{------------------------------------------------------------------------------}

procedure SetDebugNumber(NumPattern: ANSIString = 'xxxxxxxx');
{Set the debug number to a value consisting of up to four byte elements described by NumPattern.
 NumPattern is parsed from right to left in character pairs.  Any leading pairs left out of the pattern are considered
 to be xx's so that the upper byte components will be left unaffected.  You may use separators in-between pairs, such
 as periods, spaces, commas, etc.  If delimited in this way, single characters would work just as well, but pairs are
 still suggested to keep things aligned well.
 Each character pair is one of the following (case insensitive, all other characters ignored):
 00 - FF = Change corresponding byte to the hex value indicated
 ++      = Increment corresponding byte by 1.
 --      = Decrement corresponding byte by 1.
 xx      = Ignore; leave corresponding byte unchanged.}
var
  Idx : Integer;

    {------------------------------}

    function ParsePattern(Idx: Integer): Integer;
    {Parse NumPattern for item at Idx and return fixed value or $100, $101, or $102 for Inc, Dec or Ignore, respectively}
    var
      Pos  : Integer;
      Chrs : ANSIString;
    const
      AdjChars      : set of ANSIChar = ['+', '-', 'X'];                            {Adjustment characters}
      AlphaNumChars : set of ANSIChar = ['0'..'9', 'A'..'F'];                       {Alpha or numeric characters}
      ValidChars    : set of ANSIChar = ['0'..'9', 'A'..'F', '+', '-', 'X'];        {Both adjustment and alphanumeric character sets}

        {--------------}

        function Hex(S: ANSIString): Integer;
        {Return decimal value of hexadecimal string S.}
        var
          Idx : Integer;
        const
          N = ord('0');
          A = ord('A');
        begin
          Result := 0;                                                                {Assume zero}
          S := ANSIUpperCase(S);                                                      {Uppercase string}
          for Idx := 1 to length(S) do
            begin
            if not (S[Idx] in AlphaNumChars) then break;                              {Exit if invalid}
            Result := Result * 16 + ord(S[Idx]) - N - (A-N-10)*ord(ord(S[Idx]) >= A); {Accumulate}
            end;
        end;

        {--------------}

    begin
      Result := $102;                                                                 {Assume ignore}
      Pos := length(NumPattern);
      NumPattern := ANSIUpperCase(NumPattern);
      try
        repeat
          dec(Idx);                                                                   {Decrement index}
          while (Pos > 0) and not (NumPattern[Pos] in ValidChars) do dec(Pos);        {Skip invalid characters}
          if not(Pos > 0) then abort;                                                 {More chars?}
          Chrs := NumPattern[Pos];                                                    {Get second char in pair}
          dec(Pos);
          if Chrs[1] in AdjChars then                                                 {  If second char in pair was adjustment char}
            inc(Pos, ord(NumPattern[Pos] <> Chrs[1]))                                 {    Save 1st char for next time if <> 2nd char}
          else
            if NumPattern[Pos] in AlphaNumChars then Chrs := NumPattern[Pos] + Chrs;  {  Else complete the pair}
          dec(Pos);
        until Idx < 0;
        if not (Chrs[1] in AdjChars) then                                             {  If not adjustment pattern}
          Result := Hex(Chrs)                                                         {    get fixed value}
        else
          Result := ifthen(Chrs[1] = '+', $100, ifthen(Chrs[1] = '-', $101, $102));   {  Else, set Result to $100, $101, or $102 for +, - or x}
      except
      end;
    end;

    {------------------------------}

begin
  for Idx := 0 to 3 do
    case ParsePattern(Idx) of
      $00..$FF : DNum := (DNum and (($FF shl (Idx * 8)) xor $FFFFFFFF)) + (ParsePattern(Idx) shl (Idx * 8));           {Set value}
      $100     : DNum := (DNum and (($FF shl (Idx * 8)) xor $FFFFFFFF)) + (((((DNum shr (Idx * 8)) and $FF) + 1) and $FF) shl (Idx * 8));  {Inc value}
      $101     : DNum := (DNum and (($FF shl (Idx * 8)) xor $FFFFFFFF)) + (((((DNum shr (Idx * 8)) and $FF) - 1) and $FF) shl (Idx * 8));  {Dec value}
    end;                                                                                                               {Or leave value}
end;

{------------------------------------------------------------------------------}

function GetDebugNumber: Integer;
{Return current debug number}
begin
  Result := DNum;
end;

{------------------------------------------------------------------------------}

procedure GetDebugNumberComponents(var B : array of Byte);
{Returns byte components of DNum in PByte array.}
var
  Idx : Integer;
  Num : Integer;
begin
  Num  := DNum;
  for Idx := 0 to 3 do
    begin
    B[Idx] := Num and $FF;
    Num := Num shr 8;
    end;
end;

{------------------------------------------------------------------------------}

function MailSlotOpen: Boolean;
begin
  if MailSlot = INVALID_HANDLE_VALUE then
    MailSlot := createfile(pchar(MailSlotPath), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := MailSlot <> INVALID_HANDLE_VALUE;
end;

{------------------------------------------------------------------------------}

initialization
  DNum := 0;
  MailSlot := INVALID_HANDLE_VALUE;

finalization
  closehandle(MailSlot);  {Close mail slot}

end.
