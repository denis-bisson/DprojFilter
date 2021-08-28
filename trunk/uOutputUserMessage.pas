//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* Unit to hold the function to output messages to user.                        *
//* May be used in console or/and visual applications.                           *
//* We need to assigned "OUM_OutputUserMessage" procedure to the function to use.*
//* Written by Denis Bisson, Drummondville, Québec, 2021-08-27.                  *
//* -----------------------------------------------------------------------------*
//* Used in the project DprojFilter                                              *
//* Originally and mainly written by Thomas Mueller                              *
//*   https://osdn.net/projects/dprojfilter                                      *
//* This little adaptation written by Denis Bisson, Drummondville, Québec, Canada*
//*   https://github.com/denis-bisson/DprojFilter                                *
//*   2021-08-27                                                                 *
//* -----------------------------------------------------------------------------*
//* See following notes for complement information.                              *
//* You should not remove these comments.                                        *
//********************************************************************************
//

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
