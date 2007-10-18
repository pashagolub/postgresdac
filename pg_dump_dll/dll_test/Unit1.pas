unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses pg_dump_dll;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  a : integer;
  s : string;
  v : integer;//mi:2007-01-15
begin
  DeleteFile('a.dump');
  a := pg_dump('zymotic', 'mi_backup', 'a.dump', 'postgres', 5432, s, v);
  showmessage('result: ' + IntTOStr(a) + '  str: ' + s);
  Label1.Caption := Format('Version: %d', [v]);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  a : integer;
begin
  a := pg_restore();
  showmessage('result: ' + IntTOStr(a));
end;

end.
