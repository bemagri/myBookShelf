unit unitAppEvents;

{$mode objfpc}{$H+}

interface

type
  TNotifyProc = procedure;

var
  OnBooksChanged: TNotifyProc = nil;

procedure NotifyBooksChanged;

implementation

procedure NotifyBooksChanged;
begin
  if Assigned(OnBooksChanged) then
    OnBooksChanged();
end;

end.

