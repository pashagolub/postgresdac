{$I PSQLdac.inc}
unit PSQLStoredProcFrm;

{SVN revision: $Id$}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, PSQLDBTables;

type
  TPSQLStoredProcProp = class(TForm)
    Panel1: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    ListView1: TListView;
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FColumnToSort: integer;
    FColumnsSortDir: array[0..2] of boolean;
  public
    FStoredProc: TPSQLStoredProc;
    function Edit: Boolean;
    procedure SetStoredProcProperty;
    procedure GetStoredProcList;
  end;

var
  PSQLStoredProcProp: TPSQLStoredProcProp;
  PB: TProgressBar;
function EditStoredProc(AStoredProc: TPSQLStoredProc): Boolean;

implementation

uses PSQLTypes;

{$R *.dfm}

function GetSQLForParams(const anIntVer: integer): string;
 const ArgNames: array[boolean] of string = ('NULL','proargnames[g.s]');

       sqlShowParameters810  = 'SELECT proargnames[g.s],   '+
                          '       COALESCE(proargtypes[g.s-1], proallargtypes[g.s]),'+
                          '       proargmodes[g.s],   '+
                          '       format_type(t.oid,-1)    '+
                          ' FROM                      '+
                          '     			pg_proc p,      '+
                          '           pg_type t ,   '+
                          '     			generate_series(1,32) as g(s)'+
                          ' WHERE                     '+
                          '   COALESCE(p.proargtypes[g.s-1],proallargtypes[g.s]) = t.oid AND ';


      sqlShowParameters     = 'SELECT %s,                 '+ //proargnames[g.s] > 8.0.0
                          '       proargtypes[g.s],   '+
                          '       ''i''::varchar,     '+
                          '       format_type(t.oid,-1) '+
                          ' FROM                      '+
                          '     			pg_proc p,      '+
                          '           pg_type t ,   '+
                          '     			%s as g(s)      '+ //!!! generate_series(0,current_setting(''max_function_args'')::int)
                          ' WHERE                     '+
                          '   p.proargtypes[g.s] = t.oid AND ';

      sqlTail =           ' p.oid = %d '+
                          ' ORDER BY g.s ';
        function GetGenSeries(const StartPos,EndPos: string): string;
        const  sqlGenerateSeries =
                '(select i*10+j as n'+
                ' from (select 0 union all select 1 union all select 2 union all'+
                '       select 3 union all select 4 union all select 5 union all'+
                '       select 6 union all select 7 union all select 8 union all'+
                '       select 9) s1(i),'+
                '      (select 0 union all select 1 union all select 2 union all'+
                '       select 3 union all select 4 union all select 5 union all'+
                '       select 6 union all select 7 union all select 8 union all'+
                '       select 9) s2(j)'+
                ' where (i*10+j >= %s) AND (i*10+j <= %s))';
        begin
          if anIntVer >= 080000 then
           Result := Format('generate_series(%s,%s)',[StartPos, EndPos])
          else
           Result := Format(sqlGenerateSeries,[StartPos, EndPos]);
        end;
begin
 If anIntVer >= 080100 then
   Result := sqlShowParameters810 + sqlTail
 else
   Result := Format(sqlShowParameters,[ArgNames[anIntVer >= 080000], GetGenSeries('0','32')]) +
             sqlTail;
end;

function EditStoredProc(AStoredProc: TPSQLStoredProc): Boolean;
begin
  If not Assigned(AStoredProc)
     or not Assigned(AStoredProc.Database)
     or not AStoredProc.Database.Connected
  then
    raise EPSQLDatabaseError.CreateFmt('Can''t open stored procedure editor due '+
        'one of possible errors:'#13#10+
        '1. Stored procedure object not assigned'#13#10+
        '2. Database property of stored procedure object not assigned'#13#10+
        '3. Database object specified in Database property of stored procedure object'+
        ' is not Connected',[]);
  with TPSQLStoredProcProp.Create(Application) do
  try
    FStoredProc := AStoredProc;
    Result := Edit;
  finally
    Free;
  end;
end;

function TPSQLStoredProcProp.Edit: Boolean;
var
    Splash: TForm;
    Pan: Tpanel;
begin
  Splash := TForm.Create(Application);
try
  Splash.Width := 300;
  Splash.Height := 75;
  Splash.Position := poDesktopCenter;
  Splash.BorderStyle := bsNone;
  Splash.Font.Style := [fsBold];
  Pan := TPanel.Create(Splash);
  Pan.Parent := Splash;
  Pan.Align := alClient;
  Pan.BevelOuter := bvRaised;
  PAn.BevelWidth := 2;
  Pan.BevelInner := bvLowered;
  with TLabel.Create(Pan) do
   begin
    Parent := Pan;
    Caption := 'Loading Procedures List...';
    AutoSize := true;
    Left := (Splash.Width - Width) div 2;
    Top := Height;
   end;
  PB := TProgressBar.Create(Pan);
  PB.Parent := Pan;
  PB.Left := 10;
  PB.Top := (Splash.Height - PB.Height) div 2;
  PB.Width := Splash.Width-PB.Left-10;
  PB.Smooth := True;
  Splash.Show;
  Application.ProcessMessages;
  GetStoredProcList;
finally
  Splash.Free;
end;
  FColumnsSortDir[0] := False;
  FColumnToSort := 0;
  ListView1.AlphaSort;
  Result := False;
  if ShowModal = mrOk then
  begin
    SetStoredProcProperty;
    Result := True;
  end;
end;

procedure TPSQLStoredProcProp.SetStoredProcProperty;
begin
 FStoredProc.StoredProcName := ListView1.Selected.Caption;
 FStoredProc.Overload := strtoint(ListView1.Selected.SubItems[0]);
end;

procedure TPSQLStoredProcProp.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  FColumnsSortDir[FColumnToSort] := not FColumnsSortDir[FColumnToSort];
  (Sender as TCustomListView).AlphaSort;
end;

procedure TPSQLStoredProcProp.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  case FColumnToSort of
   0:
    begin
     Compare := CompareText(Item1.Caption,Item2.Caption);
     If FColumnsSortDir[0] then Compare := -1*Compare;
    end;
   1:
    begin
     Compare := (strtoint(Item1.SubItems[0]) - strtoint(Item2.SubItems[0]));
     If FColumnsSortDir[1] then Compare := -1*Compare;
    end;
   else
    begin
     Compare := CompareText(Item1.SubItems[1],Item2.SubItems[1]);
     If FColumnsSortDir[2] then Compare := -1*Compare;
    end;
  end;
end;

procedure TPSQLStoredProcProp.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  Okbtn.Enabled := Selected = True;
end;

procedure TPSQLStoredProcProp.GetStoredProcList;
Var SL: TStringList;
    i: integer;
    LI: TlistItem;
    ParamString: string;
    Q: TPSQLQuery;
    OldCursor: TCursor;
    SQL: string;
begin
  OldCursor := Screen.Cursor;
  Sl := TStringList.Create;
  try
    Screen.Cursor := crSQLWait;
    Application.ProcessMessages; //added to defroze
    FStoredProc.Database.GetStoredProcNames('',SL);
    Q := TPSQLQuery.Create(nil);
    try
      Q.Database := FStoredProc.Database;
      Q.RequestLive := False;
      Q.ParamCheck := False;
      Q.Options := [dsoOIDAsInt];
      ListView1.Items.BeginUpdate;
      ListView1.Items.Clear;
      PB.Min := 0;
      PB.Max := Sl.Count-1;
      SQL := GetSQLForParams(FStoredProc.Database.ServerVersionAsInt);
      for i:=0 to Sl.Count-1 do
       begin
        LI := ListView1.Items.Add;
        LI.Caption := SL[I];
        LI.SubItems.Append(inttostr(integer(SL.Objects[I])));
        Q.SQL.Text := Format(SQL,[integer(SL.Objects[I])]);
        Q.Open;
        Q.First;
        ParamString := '';
        while not Q.Eof do
         begin
          if not Q.Fields[2].IsNull then
            case Q.Fields[2].AsString[1] of
             'o': ParamString := ParamString + 'OUT ';
             'b': ParamString := ParamString + 'INOUT ';
            end;
          If Q.Fields[0].AsString > '' then
            ParamString := ParamString + Q.Fields[0].AsString + ' ';
          ParamString := ParamString + Q.Fields[3].AsString + '; ';
          Q.Next;
         end;
        LI.SubItems.Append('('+Copy(ParamString,1,Length(ParamString)-2)+')');
        PB.Position := i;
        Application.ProcessMessages;
       end;
      ListView1.Items.EndUpdate;
    finally
     Q.Free;
    end;
  finally
   Sl.Free;
   Screen.Cursor := OldCursor;
  end;
end;

end.
