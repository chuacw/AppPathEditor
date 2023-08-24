unit AppPathEditor.Types;

interface
uses
  System.Generics.Defaults, System.Generics.Collections;

type
  TProcAddPath = reference to procedure (const APath: string);
  TFuncFixPath = reference to function (var VPathName: string): Boolean;

  TLengthComparer = class(TInterfacedObject, IComparer<string>)
  private
    FDictionary: TDictionary<string, string>;
  public
    constructor Create(const ADictionary: TDictionary<string, string>);
    function Compare(const Left, Right: string): Integer;
  end;

implementation

constructor TLengthComparer.Create(const ADictionary: TDictionary<string, string>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TLengthComparer.Compare(const Left: string; const Right: string): Integer;
var
  LValLeft, LValRight: string;
begin
  LValLeft := FDictionary[Left];
  LValRight := FDictionary[Right];
  Result := Length(Left) + Length(LValLeft) - Length(Right) - Length(LValRight);
end;

end.
