unit Timer;

interface

  uses System.Math,
       Windows;

  function CheckTime(Duration: Cardinal = 0): Cardinal;
  function GetMSCount: Cardinal;

var
  ctTime         : Cardinal;                            {Holds start time for CheckTime function}
  ctDelay        : Cardinal;                            {Holds desired delay (in milliseconds) for CheckTime function}
  HPCFreq        : Int64;                               {Frequency of high-performance system clock, if one exists}

implementation

function CheckTime(Duration: Cardinal = 0): Cardinal;
{Mark time (if Duration > 0) and note if last time Duration has passed (if Duration = 0).
 Returns time left to complete previous Duration, or 0 if time Duration has already passed.}
begin
  if Duration > 0 then
    begin {New delay; note time and duration value}
    ctTime := GetMSCount;
    ctDelay := Duration;
    end;
  {Return difference between current time and last duration (minimum 0)}
  {NOTE: cast to Integer to allow roll-under calculation, then to Cardinal again; this accomidates for tick counter rollover}
  Result := Cardinal(Max(0, Integer(ctDelay - (Integer(GetMSCount) - Integer(ctTime)))));
end;

{----------------------------------------------------------------------------------------------------}

function GetMSCount: Cardinal;
{Return millisecond clock count.  This is computed from the high-performance system counter which exists on Win XP and later.}
var
  HPCount : Int64;
begin
  QueryPerformanceCounter(HPCount);
  Result := HPCount div HPCFreq;
end;

{----------------------------------------------------------------------------------------------------}

initialization
  {Get high performance counter frequency and convert it to milliseconds (works on WinXP and later.}
  QueryPerformanceFrequency(HPCFreq);
  HPCFreq := HPCFreq div 1000;

end.
