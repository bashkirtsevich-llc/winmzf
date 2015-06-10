unit ShellContextMenu;

interface

uses Windows, SysUtils, Forms, ShlObj, ActiveX;

function  ExecuteContextMenu (const FileName, Cmd: string;Parent:hWnd): Boolean;

implementation

function  ExecuteContextMenu (const FileName, Cmd: string;Parent:hWnd): Boolean;
  var
    ShellFolder             : IShellFolder;
    FileObject              : IShellFolder;
    Menu                    : IContextMenu;
    lpItemIdList, lpNameList: PItemIDList;
    WideStr                 : WideString;
    Malloc                  : IMalloc;
    Attributes, uLength     : ULONG;
    Command                 : TCMInvokeCommandInfo;
    hr: HRESULT;
begin
  Result := FALSE;
  Menu := nil;
  Malloc := nil;
  FileObject := nil;
  ShellFolder := nil;

  try
    hr := SHGetMalloc (Malloc);

    if  SUCCEEDED(hr)then
      begin
        hr := SHGetDesktopFolder (ShellFolder);

        if  SUCCEEDED(hr)then
          begin
            WideStr := ExtractFilePath (FileName);
            hr := ShellFolder.ParseDisplayName (0, nil,
                                PWideChar (WideStr), uLength,
                                lpItemIdList, Attributes);

          if  SUCCEEDED(hr)then
          begin
            hr := ShellFolder.BindToObject (lpItemIdList, nil,
                          IID_IShellFolder, Pointer (FileObject));

            if  SUCCEEDED(hr)then
            begin
              WideStr := ExtractFileName (FileName);
              hr := FileObject.ParseDisplayName (0, nil,
                  PWideChar (WideStr), uLength, lpNameList, Attributes);

              if  SUCCEEDED(hr)then
              begin
                hr := FileObject.GetUIObjectOf (0, 1, lpNameList,
                              IID_IContextMenu, nil, Pointer (Menu));

                if  SUCCEEDED(hr)then
                begin
                  FillChar (Command, sizeof (Command), 0);
                  Command.cbSize := sizeof (Command);
                  Command.hwnd   := Parent;
                  Command.lpVerb := PChar (Cmd);
                  Command.nShow  := SW_SHOW;
                  Result := SUCCEEDED (Menu.InvokeCommand (Command));
                end;
              end;
            end;
          end;
        end;
      end;
  finally
    Menu := nil;
    FileObject := nil;

    if  Malloc <> nil then Malloc.Free (lpItemIdList);
    Malloc := nil;
    ShellFolder := nil;
  end;
end;

end.
