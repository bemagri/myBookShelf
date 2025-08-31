unit unitSettingsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, IniFiles, LazFileUtils;

type
  TSettingsDialog = class(TForm)
  private
    lblDataDir: TLabel;
    edtDataDir: TEdit;
    btnBrowse: TButton;
    lblBooksDir: TLabel;
    edtBooksDir: TEdit;
    btnBrowseBooks: TButton;
    chkCopyBooks: TCheckBox;
    chkRenameBooks: TCheckBox;
    chkMeta: TCheckBox;
    chkPdfCovers: TCheckBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate({%H-}Sender: TObject);
    procedure BtnBrowseClick({%H-}Sender: TObject);
    procedure BtnBrowseBooksClick({%H-}Sender: TObject);
    procedure BtnOKClick({%H-}Sender: TObject);
    function  ConfigDir: String;
    function  ConfigPath: String;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

constructor TSettingsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FormCreate(Self);
end;

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin
  Caption := 'Settings';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 520; ClientHeight := 300;

  lblDataDir := TLabel.Create(Self); lblDataDir.Parent := Self;
  lblDataDir.Caption := 'Data folder (where books.xml is saved):';
  lblDataDir.Left := 16; lblDataDir.Top := 16;

  edtDataDir := TEdit.Create(Self); edtDataDir.Parent := Self;
  edtDataDir.Left := 16; edtDataDir.Top := 40; edtDataDir.Width := 410;

  btnBrowse := TButton.Create(Self); btnBrowse.Parent := Self;
  btnBrowse.Left := 436; btnBrowse.Top := 38; btnBrowse.Caption := 'Browse...';
  btnBrowse.OnClick := @BtnBrowseClick;

  lblBooksDir := TLabel.Create(Self); lblBooksDir.Parent := Self;
  lblBooksDir.Caption := 'Managed books folder:';
  lblBooksDir.Left := 16; lblBooksDir.Top := 80;

  edtBooksDir := TEdit.Create(Self); edtBooksDir.Parent := Self;
  edtBooksDir.Left := 16; edtBooksDir.Top := 104; edtBooksDir.Width := 410;

  btnBrowseBooks := TButton.Create(Self); btnBrowseBooks.Parent := Self;
  btnBrowseBooks.Left := 436; btnBrowseBooks.Top := 102; btnBrowseBooks.Caption := 'Browse...';
  btnBrowseBooks.OnClick := @BtnBrowseBooksClick;

  chkCopyBooks := TCheckBox.Create(Self); chkCopyBooks.Parent := Self;
  chkCopyBooks.Caption := 'Copy books to managed folder on import';
  chkCopyBooks.Left := 16; chkCopyBooks.Top := 144;

  chkRenameBooks := TCheckBox.Create(Self); chkRenameBooks.Parent := Self;
  chkRenameBooks.Caption := 'Rename books based on metadata';
  chkRenameBooks.Left := 16; chkRenameBooks.Top := 168;

  chkMeta := TCheckBox.Create(Self); chkMeta.Parent := Self;
  chkMeta.Caption := 'Extract metadata from book files';
  chkMeta.Left := 16; chkMeta.Top := 192;

  chkPdfCovers := TCheckBox.Create(Self); chkPdfCovers.Parent := Self;
  chkPdfCovers.Caption := 'Auto-extract PDF cover on import (requires pdftoppm)';
  chkPdfCovers.Left := 16; chkPdfCovers.Top := 216;

  btnOK := TBitBtn.Create(Self); btnOK.Parent := Self;
  btnOK.Kind := bkOK; btnOK.Left := ClientWidth - 180; btnOK.Top := 248;
  btnOK.OnClick := @BtnOKClick;

  btnCancel := TBitBtn.Create(Self); btnCancel.Parent := Self;
  btnCancel.Kind := bkCancel; btnCancel.Left := ClientWidth - 92; btnCancel.Top := 248;

  LoadSettings;
end;

procedure TSettingsDialog.BtnBrowseClick(Sender: TObject);
var dir: String;
begin
  dir := edtDataDir.Text;
  if SelectDirectory('Choose data folder', '', dir) then
    edtDataDir.Text := dir;
end;

procedure TSettingsDialog.BtnBrowseBooksClick(Sender: TObject);
var dir: String;
begin
  dir := edtBooksDir.Text;
  if SelectDirectory('Choose books folder', '', dir) then
    edtBooksDir.Text := dir;
end;

procedure TSettingsDialog.BtnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

function TSettingsDialog.ConfigDir: String;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False));
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
    edtBooksDir.Text := ini.ReadString('general', 'books_dir', edtDataDir.Text);
    chkCopyBooks.Checked := ini.ReadBool('general', 'copy_books', True);
    chkRenameBooks.Checked := ini.ReadBool('general', 'rename_books', True);
    chkMeta.Checked := ini.ReadBool('general', 'extract_metadata', True);
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
    ini.WriteString('general', 'books_dir', edtBooksDir.Text);
    ini.WriteBool('general', 'copy_books', chkCopyBooks.Checked);
    ini.WriteBool('general', 'rename_books', chkRenameBooks.Checked);
    ini.WriteBool('general', 'extract_metadata', chkMeta.Checked);
    ini.WriteBool('general', 'auto_pdf_cover', chkPdfCovers.Checked);
  finally
    ini.Free;
  end;
end;

end.
