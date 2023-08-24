program TestEnvBlock;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.StrUtils,
  Winapi.Windows;

procedure CheckEnvBlock;
begin
  var FEnvBlock := GetEnvironmentStringsA;
  try
    var LSplitString := SplitString(FEnvBlock, '=');
    var P: PAnsiChar := FEnvBlock;
    if (Length(LSplitString)>0) and (Length(LSplitString[0])>0) then
      begin
        WriteLn('Value: ', LSplitString[0]);
        WriteLn('Working!');
      end else
      begin
        Writeln('Possible corruption!');
        repeat
          P := StrEnd(P) + 1;
        until P^=#0;
      end;
  finally
    FreeEnvironmentStringsA(FEnvBlock);
  end;
end;

begin
  CheckEnvBlock;
  Readln;
end.
