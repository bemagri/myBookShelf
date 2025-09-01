unit unitAppEvents;

{$mode objfpc}{$H+}

interface

type
  // Use "of object" so methods (e.g., TForm1.SaveBooksNow) can be assigned
  TNotifyProc = procedure of object;

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
