unit unitSettingsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, IniFiles;

type
  TSettingsDialog = class(TForm)
  private
    lblDataDir: TLabel;
    edtDataDir: TEdit;
    btnBrowse: TButton;
    chkPdfCovers: TCheckBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    function  ConfigDir: String;
    function  ConfigPath: String;
    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;

var
  SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin
  Caption := 'Settings';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 520; ClientHeight := 180;

  lblDataDir := TLabel.Create(Self); lblDataDir.Parent := Self;
  lblDataDir.Caption := 'Data folder (where data.dat is saved):';
  lblDataDir.Left := 16; lblDataDir.Top := 16;

  edtDataDir := TEdit.Create(Self); edtDataDir.Parent := Self;
  edtDataDir.Left := 16; edtDataDir.Top := 40; edtDataDir.Width := 410;

  btnBrowse := TButton.Create(Self); btnBrowse.Parent := Self;
  btnBrowse.Left := 436; btnBrowse.Top := 38; btnBrowse.Caption := 'Browse...';
  btnBrowse.OnClick := @BtnBrowseClick;

  chkPdfCovers := TCheckBox.Create(Self); chkPdfCovers.Parent := Self;
  chkPdfCovers.Caption := 'Auto-extract PDF cover on import (requires pdftoppm)';
  chkPdfCovers.Left := 16; chkPdfCovers.Top := 80;

  btnOK := TBitBtn.Create(Self); btnOK.Parent := Self;
  btnOK.Kind := bkOK; btnOK.Left := ClientWidth - 180; btnOK.Top := 130;
  btnOK.OnClick := @BtnOKClick;

  btnCancel := TBitBtn.Create(Self); btnCancel.Parent := Self;
  btnCancel.Kind := bkCancel; btnCancel.Left := ClientWidth - 92; btnCancel.Top := 130;

  LoadSettings;
end;

procedure TSettingsDialog.BtnBrowseClick(Sender: TObject);
var dir: String;
begin
  dir := edtDataDir.Text;
  if SelectDirectory('Choose data folder', '', dir) then
    edtDataDir.Text := dir;
end;

procedure TSettingsDialog.BtnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

function TSettingsDialog.ConfigDir: String;
begin
  {$IFDEF MSWINDOWS}
  Result := GetEnvironmentVariableUTF8('APPDATA') + DirectorySeparator + 'mybookshelf' + DirectorySeparator;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariableUTF8('HOME') + DirectorySeparator + '.mybookshelf' + DirectorySeparator;
  {$ENDIF}
  if not DirectoryExistsUTF8(Result) then CreateDirUTF8(Result);
end;

function TSettingsDialog.ConfigPath: String;
begin
  Result := ConfigDir + 'config.ini';
end;

procedure TSettingsDialog.LoadSettings;
var ini: TIniFile;
begin
  ini := TIniFile.Create(ConfigPath);
  try
    edtDataDir.Text := ini.ReadString('general', 'data_dir', '');
    chkPdfCovers.Checked := ini.ReadBool('general', 'auto_pdf_cover', True);
  finally
    ini.Free;
  end;
end;

procedure TSettingsDialog.SaveSettings;
var ini: TIniFile;
begin
  ini := TIniFile.Create(ConfigPath);
  try
    ini.WriteString('general', 'data_dir', edtDataDir.Text);
    ini.WriteBool('general', 'auto_pdf_cover', chkPdfCovers.Checked);
  finally
    ini.Free;
  end;
end;

end.
