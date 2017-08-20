program myBookShelf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  Cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, book, bookCollection
  { you can add units after this };

{$R *.res}

begin
  Requirederivedformresource:=True;
  Application.Initialize;
  Application.Createform(Tform1, Form1);
  Application.Run;
end.

