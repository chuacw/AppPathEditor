unit AppPathEditor.Messages;

interface
uses
  System.Messaging, System.Generics.Collections;

type
  TMessageIdle = class(TMessage) end;
  TMessageEnableDelete = class(TMessage) end;
  TMessageEnableDeleteIfAppPathExist = class(TMessage) end;

  TStringMessage = class(TMessage<string>) end;
  TMessageLog = class(TStringMessage)
  public
    property Msg: string read FValue;
  end;
  TShrinkPathMsg = class(TStringMessage)
  public
    property Path: string read FValue write FValue;
  end;
  TExpandPathMsg = class(TShrinkPathMsg) end;
  TDeleteRegKeyMsg = class(TStringMessage)
  public
    property Key: string read FValue;
  end;

  TMessageLoadKeyComplete = class(TMessage) end;

implementation

end.
