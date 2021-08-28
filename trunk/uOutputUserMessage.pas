unit uOutputUserMessage;

interface

type
  tOUS_Context = (ouscINFORMATION, ouscERROR);
  tOUS_OutputUserMessage = procedure(const sMsgToShow: string; const iContext: tOUS_Context);

var
  OUM_OutputUserMessage: tOUS_OutputUserMessage = nil;

procedure WriteUserMessage(const sMsgToShow: string; const iContext: tOUS_Context = ouscINFORMATION);

implementation

procedure WriteUserMessage(const sMsgToShow: string; const iContext: tOUS_Context = ouscINFORMATION);
begin
  if Assigned(OUM_OutputUserMessage) then
    OUM_OutputUserMessage(sMsgToShow, iContext);
end;

end.

