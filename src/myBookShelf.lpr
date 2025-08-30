program myBookShelf;

{$mode objfpc}{$H+}

// Enable pthread support on Unix before any other unit uses threads.
{$IFDEF UNIX}{$DEFINE UseCThreads}{$ENDIF}

uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  Interfaces,           // LCL widgetset (gtk2/qt5 decided by your env/flags)
  Forms,
  main,                 // your main form unit (TForm1, Form1)
  unitSettingsDialog,   // settings dialog unit
  unitCoverWorker;      // background PDF cover worker

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
