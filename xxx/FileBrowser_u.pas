unit FileBrowser_u;

interface

uses Advanced,ComCtrls,Classes,Windows,SysUtils,
      Dialogs,Graphics,ShellApi,SignatureDetect_u;

  Type TDisplayMode=(dmArchiver,dmBrowser);

  Procedure Expand(Node:TTreeNode);
  procedure ChangeTree(Node:TTreeNode);
  Procedure FileListDblClick;
  Procedure GetDirTree;
  procedure GoToAddress;

implementation

uses Main_u;

function DirectoryName(name: string): boolean;
begin
  result := (name > '.') and (name > '..');
end;

procedure NextLevel(ParentNode: TTreeNode);
var
  sr, srChild: TSearchRec;
  node: TTreeNode;
  path: string;
begin
  node := ParentNode;
  path := '';
  repeat
    path := node.Text + '\' + path;
    node := node.Parent;
  until
    node = nil;
  if FindFirst(path + '\*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory > 0) and DirectoryName(sr.name) then
      begin
        If (sr.Name='.')Or(sr.name='..')Then Continue;
        node := FrmMain.DirTree.Items.AddChild(ParentNode, sr.name);
        node.ImageIndex := 0;
        node.SelectedIndex := 1;
        node.HasChildren := false;
        if FindFirst(path + sr.name + '\*.*', faAnyFile, srChild) = 0 then
        begin
          repeat
            if (srChild.Attr and faDirectory > 0) and
            DirectoryName(srChild.name) then
              node.HasChildren := true;  
          until
            (FindNext(srChild) > 0) or node.HasChildren;
        end;
        FindClose(srChild);
      end;
    until
      FindNext(sr) > 0;
  end
  else
    ParentNode.HasChildren := false;
  FindClose(sr);
end;

Procedure GoToNode(SubDir:String);
Var Noddy:TTreeNode;
    Searching:Boolean;
Begin
FrmMain.FilesList.Items.BeginUpdate;
Noddy := FrmMain.DirTree.Items[0];
Searching := true;
while (Searching) and (Noddy <> nil) do
begin
  if UpperCase(Noddy.text) = UpperCase(SubDir) then
  begin
    Searching := False;
    Noddy.Expanded:=True;
    //Noddy.Expanded:=False;
    Noddy.Selected:=True;
    FrmMain.DirTree.Selected := Noddy;
    {FrmMain.DirTree.SetFocus;
    FrmMain.FilesList.SetFocus; }        //Установить фокус обратно на TListView
  end
  else
  begin
    Noddy := Noddy.GetNext;
    //Noddy.HasChildren := true;
  end;
end;
FrmMain.FilesList.Items.EndUpdate;
End;

Procedure GetDirTree;
var
  c: char;
  s: string;
  node: TTreeNode;
  DriveType: integer;
  i: integer;
begin
  FrmMain.DirTree.Items.BeginUpdate;
  for c := 'A' to 'Z' do
  begin
    s := c + ':';
    DriveType := GetDriveType(PChar(s));
    if DriveType = 1 then
      continue;
    node := FrmMain.DirTree.Items.AddChild(nil, s);
    case DriveType of
      DRIVE_REMOVABLE: node.ImageIndex := 2;
      DRIVE_FIXED: node.ImageIndex := 3;
      DRIVE_REMOTE: node.ImageIndex := 4;
      DRIVE_CDROM: node.ImageIndex := 5;
      else
        node.ImageIndex := 6;
    end;
    node.SelectedIndex := node.ImageIndex;
    node.HasChildren := true;
  end;
  FrmMain.DirTree.Items.EndUpdate;
End;

Procedure ShowFiles(Dir:String);
var ListItem: TListItem;
    sr: tsearchrec;
    NewColumn: TListColumn;
    A:TDateTime;
    Icon:TIcon;
    FileInfo: SHFILEINFO;
begin
  if length(Dir)<1 Then Exit;
  FrmMain.BrowserIcons.Clear;
  FrmMain.BrowserIconsBig.Clear;
  FrmMain.BrowserIcons.BkColor:=clWindow;
  FrmMain.BrowserIconsBig.BkColor:=clWindow;
  FrmMain.BrowserIcons.BlendColor:=clWindow;
  FrmMain.BrowserIconsBig.BlendColor:=clWindow;
  While Dir[Length(Dir)]='\' Do
    Delete(Dir,Length(Dir),1);
  FrmMain.FilesList.Items.Clear;
  if FindFirst(dir + '\*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory > 0) {and DirectoryName(sr.name) }then
          begin
            if (sr.Name='.')or (sr.Name='..') then continue;
            ListItem:=FrmMain.FilesList.Items.Add;
            ListItem.Caption:=sr.Name;
            ListItem.ImageIndex:=1;
            ListItem.SubItems.Add(IntToStr(sr.Size));//Size
            ListItem.SubItems.Add('Disable');//PackedSize
            ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                FileCreationTime(Dir+'\'+sr.Name))));//Creation
            ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                FileModifyTime(Dir+'\'+sr.Name))));//Modify
            ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                FileAccessTime(Dir+'\'+sr.Name))));//Access
            ListItem.SubItems.Add(GetAttributesAsString(sr.Attr));//Attr
            ListItem.SubItems.Add('Disable');//CRC
            ListItem.SubItems.Add(Dir+'\');
            Icon:=TIcon.Create;
            SHGetFileInfo(PChar(Dir +'\'+ sr.Name), 0, FileInfo,
             SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
            icon.Handle := FileInfo.hIcon;
            ListItem.ImageIndex := FrmMain.BrowserIcons.AddIcon(Icon);
            Icon.Free;
            //
            Icon:=TIcon.Create;
            SHGetFileInfo(PChar(Dir +'\'+ sr.Name), 0, FileInfo,
             SizeOf(FileInfo), SHGFI_ICON or SHGFI_LARGEICON);
            icon.Handle := FileInfo.hIcon;
            ListItem.ImageIndex := FrmMain.BrowserIconsBig.AddIcon(Icon);
            Icon.Free;
          end;
        until
        (FindNext(sr) > 0);
       end;

      if FindFirst(dir + '\*.*', faAnyFile, sr) = 0 then
        begin
          repeat
            if (sr.Attr and faDirectory <= 0)  then
              begin
                ListItem:=FrmMain.FilesList.Items.Add;
                ListItem.Caption:=sr.Name;
                ListItem.ImageIndex:=0;
                ListItem.SubItems.Add(IntToStr(sr.Size));//Size
                ListItem.SubItems.Add('Disable');//PackedSize
                ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                    FileCreationTime(Dir+'\'+sr.Name))));//Creation
                ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                    FileModifyTime(Dir+'\'+sr.Name))));//Modify
                ListItem.SubItems.add(FormatDateTime(TimeFormat,FileDateToDateTime(
                    FileAccessTime(Dir+'\'+sr.Name))));//Access
                ListItem.SubItems.Add(GetAttributesAsString(sr.Attr));//Attr
                ListItem.SubItems.Add('Disable');//CRC
                ListItem.SubItems.Add(Dir+'\');
                Icon:=TIcon.Create;
                SHGetFileInfo(PChar(Dir +'\'+ sr.Name), 0, FileInfo,
                  SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
                icon.Handle := FileInfo.hIcon;
                ListItem.ImageIndex := FrmMain.BrowserIcons.AddIcon(Icon);
                Icon.Free;
                //
                Icon:=TIcon.Create;
                SHGetFileInfo(PChar(Dir +'\'+ sr.Name), 0, FileInfo,
                  SizeOf(FileInfo), SHGFI_ICON or SHGFI_LARGEICON);
                icon.Handle := FileInfo.hIcon;
                ListItem.ImageIndex := FrmMain.BrowserIconsBig.AddIcon(Icon);
                Icon.Free;
            end;
        until
        (FindNext(sr) > 0);
      end;
End;

procedure FileListDblClick;
var Temp:String;
begin
  If (FrmMain.FilesList.ItemIndex=-1) Then Exit;
  If CharInString('D',FrmMain.FilesList.Items.Item[FrmMain.FilesList.ItemIndex].SubItems.Strings[5]) Then
    Begin
      Temp:=FrmMain.FilesList.Items.Item[FrmMain.FilesList.ItemIndex].Caption;
      {If Temp='..'Then
        Begin
          //TreeView1.OnExpanding:=Nil;
          //TreeView1.Selected:=TreeView1.Selected.Parent;
          Temp:=FrmMain.cbAddress.Text;
          FrmMain.cbAddress.Text:=GetLastDir(Temp);
          //ShowFiles(Edit1.Text);
          //NodeAtIndex(ListView1.Selected.Index).Selected := True;
          GoToNode(GetLastDirName(GetLastDir(Temp)));
          Exit;
        End;   }
      FrmMain.cbAddress.Text:=FrmMain.cbAddress.Text+'\'+Temp;
      //ShowFiles(Edit1.Text);
      //NodeAtIndex(0).Selected := True;
      GoToNode(Temp);
    End Else
      Begin
        Temp:=FrmMain.FilesList.Items.Item[FrmMain.FilesList.ItemIndex].Caption;
        {Case GetArchiveTypeBySignature(FrmMain.cbAddress.Text+'\'+Temp) Of
          ftMZF:}FrmMain.OpenArchive(FrmMain.cbAddress.Text+'\'+Temp);
          {ftAnyFile:ShellOpenFile(FrmMain.Handle,FrmMain.cbAddress.Text+'\'+Temp);
        End; }
      End;

end;

procedure GoToAddress;
Var Index:Word;
    Temp:String;
    s:string;
    a:boolean;
begin
  Temp:=FrmMain.cbAddress.Text;
  If FileExists(Temp) Then
    Begin
      {Case GetArchiveTypeBySignature(Temp) Of
        ftMZF:}FrmMain.OpenArchive(Temp);
        {ftAnyFile:ShellOpenFile(FrmMain.Handle,Temp);
      End;}
      Exit;
    End;
  //ShowFiles(Temp);
  s:='';
  FrmMain.DirTree.Tag:=1;
  If Length(Temp)>1 Then
  if (Temp[length(Temp)]<>'\') then Temp:=Temp+'\';
  index:=0;
  while pos('\',Temp)<>0 do
    begin
      Index:=pos('\',Temp);
      GoToNode(copy(Temp,1,Index-1));
      //ShowMessage(copy(Temp,1,Index-1));
      s:=s+copy(Temp,1,Index-1)+'\';
      delete(Temp,1,Index);
    end;
  ShowFiles(s);
  FrmMain.DirTree.Tag:=0;                
end;

procedure ChangeTree(Node:TTreeNode);
Begin
    If FrmMain.DirTree.Tag<>0 Then Exit;

      FrmMain.cbAddress.Text:=GetPathToNode(Node,'\');
      //ShowFiles(Edit1.Text);
      //GoToNewAddress;
      GoToAddress;
      node.Expanded:=true;
End;

Procedure Expand(Node:TTreeNode);
Begin
    FrmMain.DirTree.Items.BeginUpdate;
  {if (treeview1.Selected=node)then
    begin }
      node.DeleteChildren;
      NextLevel(node);
    {end;}
  FrmMain.DirTree.Items.EndUpdate;
End;


end.
 