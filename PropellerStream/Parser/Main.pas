unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.StrUtils,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    StreamMemo: TMemo;
    Layout1: TLayout;
    ParseAndTranslate: TButton;
    ImageMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    procedure ParseAndTranslateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  RawLoaderAppSize = 91;
  RawLoaderImage : array[0..840] of byte = ($92,$92,$92,$4A,$29,$CA,$52,$D2,$92,$52,$A5,$C9,$2A,$95,$92,$C9,
                                            $92,$92,$92,$52,$2A,$C9,$92,$92,$4A,$A5,$92,$92,$92,$49,$29,$92,
                                            $92,$92,$D5,$CA,$92,$92,$52,$29,$C9,$92,$92,$C9,$92,$92,$92,$92,
                                            $D2,$4A,$CA,$92,$92,$92,$92,$92,$92,$92,$4A,$49,$92,$4A,$55,$95,
                                            $C9,$92,$A9,$2A,$CA,$52,$AA,$55,$29,$92,$4A,$C9,$92,$92,$D2,$AA,
                                            $55,$95,$92,$4A,$2A,$C9,$92,$92,$92,$52,$95,$52,$29,$92,$A5,$52,
                                            $29,$49,$55,$92,$D2,$AA,$29,$49,$D2,$92,$52,$A5,$D2,$4A,$AA,$4A,
                                            $92,$29,$92,$AA,$29,$4A,$CA,$D2,$92,$92,$2A,$29,$AA,$95,$92,$4A,
                                            $49,$25,$4A,$92,$49,$55,$52,$29,$29,$29,$92,$2A,$49,$95,$C9,$4A,
                                            $C9,$D2,$92,$92,$52,$CA,$AA,$95,$92,$CA,$92,$CA,$92,$52,$4A,$52,
                                            $D5,$D2,$52,$25,$92,$92,$95,$29,$AA,$95,$92,$92,$25,$92,$92,$2A,
                                            $D2,$52,$D5,$92,$92,$2A,$C9,$92,$92,$2A,$49,$55,$D2,$52,$29,$4A,
                                            $D2,$52,$CA,$AA,$C9,$2A,$92,$25,$92,$92,$CA,$4A,$92,$A9,$49,$92,
                                            $AA,$C9,$CA,$D2,$2A,$C9,$4A,$92,$D2,$92,$92,$C9,$92,$AA,$92,$C9,
                                            $4A,$25,$92,$52,$D2,$D2,$D2,$92,$C9,$92,$29,$C9,$92,$52,$92,$C9,
                                            $92,$25,$CA,$D2,$AA,$D2,$52,$29,$A5,$D2,$92,$C9,$92,$A9,$49,$4A,
                                            $92,$C9,$92,$52,$4A,$52,$D5,$92,$D2,$4A,$D2,$CA,$4A,$4A,$52,$A5,
                                            $92,$D2,$CA,$D2,$92,$92,$49,$CA,$AA,$95,$92,$CA,$AA,$29,$92,$52,
                                            $C9,$AA,$95,$AA,$4A,$A5,$92,$52,$4A,$52,$D5,$D2,$52,$55,$D2,$92,
                                            $95,$CA,$AA,$29,$92,$4A,$D2,$92,$92,$CA,$4A,$52,$95,$92,$A9,$29,
                                            $49,$92,$25,$CA,$AA,$29,$92,$4A,$95,$D2,$92,$25,$CA,$AA,$29,$CA,
                                            $92,$29,$92,$92,$2A,$D2,$52,$D5,$4A,$CA,$AA,$92,$C9,$2A,$29,$4A,
                                            $92,$92,$C9,$92,$CA,$92,$52,$C9,$CA,$4A,$2A,$92,$92,$C9,$92,$C9,
                                            $52,$92,$49,$4A,$49,$52,$A5,$92,$92,$2A,$D2,$52,$D5,$D2,$92,$92,
                                            $CA,$92,$92,$25,$CA,$AA,$95,$CA,$92,$A9,$92,$49,$29,$CA,$AA,$C9,
                                            $92,$C9,$92,$CA,$92,$92,$25,$CA,$AA,$95,$CA,$92,$A9,$92,$49,$29,
                                            $CA,$AA,$C9,$92,$C9,$D2,$92,$92,$52,$C9,$CA,$AA,$95,$CA,$92,$D5,
                                            $92,$C9,$2A,$29,$AA,$29,$92,$92,$52,$4A,$D2,$4A,$CA,$AA,$29,$92,
                                            $92,$C9,$4A,$92,$95,$D2,$52,$95,$92,$A9,$55,$D5,$4A,$4A,$52,$D5,
                                            $CA,$52,$AA,$29,$92,$52,$C9,$AA,$95,$AA,$CA,$AA,$CA,$92,$92,$92,
                                            $52,$95,$52,$29,$55,$95,$29,$29,$AA,$95,$92,$2A,$92,$92,$52,$4A,
                                            $CA,$AA,$95,$D2,$2A,$C9,$92,$92,$29,$29,$AA,$95,$2A,$C9,$AA,$D2,
                                            $92,$4A,$CA,$AA,$29,$92,$4A,$C9,$AA,$25,$29,$49,$95,$C9,$92,$52,
                                            $92,$92,$D2,$4A,$CA,$AA,$D5,$D2,$CA,$52,$49,$92,$4A,$CA,$AA,$29,
                                            $52,$D5,$2A,$CA,$92,$A9,$55,$A5,$92,$AA,$92,$A9,$92,$29,$29,$AA,
                                            $25,$D2,$AA,$92,$92,$92,$92,$92,$92,$55,$92,$95,$29,$D2,$CA,$2A,
                                            $4A,$52,$A5,$92,$D2,$CA,$92,$29,$92,$29,$49,$95,$25,$D2,$CA,$92,
                                            $92,$95,$29,$49,$55,$CA,$AA,$D2,$52,$95,$2A,$CA,$AA,$49,$92,$2A,
                                            $A5,$25,$2A,$4A,$92,$55,$D2,$52,$C9,$92,$92,$92,$CA,$CA,$52,$25,
                                            $2A,$92,$C9,$AA,$25,$29,$49,$95,$C9,$92,$D2,$AA,$D2,$92,$4A,$CA,
                                            $AA,$29,$52,$95,$49,$D5,$52,$D2,$52,$25,$C9,$52,$92,$92,$92,$A5,
                                            $4A,$52,$D5,$92,$AA,$CA,$92,$92,$4A,$4A,$52,$55,$D2,$CA,$92,$92,
                                            $CA,$92,$92,$92,$2A,$CA,$52,$29,$92,$92,$92,$92,$92,$92,$55,$92,
                                            $95,$C9,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,
                                            $92,$92,$92,$92,$92,$92,$92,$92,$92,$CA,$92,$92,$92,$92,$92,$92,
                                            $92,$92,$92,$92,$D2,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,
                                            $92,$CA,$92,$92,$92,$92,$92,$55,$55,$55,$25,$55,$55,$95,$92,$92,
                                            $92,$92,$92,$92,$92,$92,$92,$92,$C9,$92,$92,$92,$92,$92,$92,$92,
                                            $92,$92,$CA,$2A,$A5,$CA,$92,$92,$92,$92,$92,$92,$92,$A5,$29,$C9,
                                            $92,$92,$92,$92,$92,$92,$92,$92,$C9,$92,$C9,$92,$92,$92,$92,$92,
                                            $92,$92,$92,$92,$4A,$29,$CA,$52,$D2,$92,$92,$92,$92,$92,$92,$92,
                                            $92,$92,$92,$92,$92,$92,$29,$25,$95,$52,$92,$C9,$D2,$4A,$C9,$52,
                                            $CA,$D2,$52,$92,$92,$92,$92,$92,$92);

implementation

{$R *.fmx}

procedure TForm1.ParseAndTranslateClick(Sender: TObject);
var
  InStr   : TStringList;
  OutStr  : TStringList;
  SIdx    : Integer;
  VIdx    : Integer;
  CIdx    : Integer;
  Values  : TArray<String>;
  Value   : Integer;
  IBCount : Integer;
  BValue  : Integer;
  OValue  : Integer;
  OBCount : Integer;
begin
  ImageMemo.Lines.Clear;
  InStr := TStringList.Create;
  OutStr := TStringList.Create;
  try
    {Input and clean stream}
    InStr.Text := StreamMemo.Text;
    for SIdx := 0 to InStr.Count-1 do
      begin
      {Trim Strings}
      if InStr[SIdx].IndexOf('(') > -1 then
        InStr[SIdx] := InStr[SIdx].Substring(InStr[SIdx].IndexOf('(')+1)
      else
        begin
        InStr[SIdx] := InStr[SIdx].TrimLeft;
        if InStr[SIdx].IndexOf(')') > -1 then
          InStr[SIdx] := InStr[SIdx].Remove(InStr[SIdx].IndexOf(')'));
        end;
      end;
    {Remove leading lines that appear invalid}
    while (InStr.Count > 0) and not (InStr[0].StartsWith('$')) do InStr.Delete(0);
    {Get values as one long comma-delimited string}
    Values := InStr.Text.Split([',']);
    {Parse values and generate Propeller Application Image}
    SIdx := 0;
    OValue := 0;
    for VIdx := 0 to high(Values) do
      begin
      Value := (Values[VIdx].Trim.ToInteger or $100) shl 1;
      IBCount := 0;
      BValue := 2;
      repeat
        dec(BValue, ord((Value and $1) = 0));
        Value := Value shr 1;
        inc(IBCount);
        if BValue < 0 then
          raise Exception.Create('Encoding Error at ' + Value.ToString)
        else
          if (BValue < 2) and ((Value and $1) = 1) then
            begin
            OValue := (OValue shr 1) or (BValue shl 7);
            BValue := 2;
            inc(OBCount);
            if OBCount = 8 then
              begin
              OutStr.Text := OutStr.Text.TrimRight + ifthen(CIdx mod 16 = 0, ',' + #$D#$A, ',') + '$' + OValue.ToHexString(2);
              OBCount := 0;
              OValue := 0;
              inc(CIdx);
              end;
            end;
      until (IBCount = 10);
      end;
    OutStr.Delete(0);
    ImageMemo.Text := OutStr.Text;
  finally
    InStr.Free;
    OutStr.Free;
  end;
end;

end.
