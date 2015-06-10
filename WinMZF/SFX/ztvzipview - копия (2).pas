(**********************************************************************

  Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

**********************************************************************)

(*********************************************************************)
(*  NOTE: Adding "Flat ScrollBar" support                            *)
(*********************************************************************)
(* Some older versions of Windows 95 will not allow ZipTV to install *)
(* due to an incompatibility with older version of comctl32.dll.     *)
(* Therefore, we've had to place a conditional define (this          *)
(* condition isn't defined by the ZipTV component suite).  It is up  *)
(* to the developer to add flat scroll bar support.                  *)
(* To add support, in delphi add {$DEFINE USE_FLAT_SB} at the top of *)
(* this unit.                                                        *)
(*                                                                   *)
(* C++ Builder had problems dealing with the flat scroll bar, so     *)
(* we placed a conditional define on C++ Builder.                    *)
(*                                                                   *)
(* v4.5: - removed TztvListBox.wmLMouseDown                          *)
(*       - created TZipView.DoOnClick to replace the                 *)
(*         TztvListBox.wmLMouse event.                               *)
(*                                                                   *)
(*********************************************************************)
Unit ztvZipView;

Interface

Uses
   Windows,
   Messages,
   Graphics,
   Forms,
   ExtCtrls,
   ComCtrls,
   StdCtrls,
   Classes,
   ShellApi,
   Controls,
   SysUtils,
   Dialogs,
   ztvBase,
   ztvGbls,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines
{$i compiler.inc}


Type
   TFoundData = Class(TObject)
      Data: TWin32FindData;
   End;

   Str1 = String[1];

   THeaderVertPos = (dtBottom, dtCenter, dtTop);
   TSortDirection = (sdAscending, sdDecending);
   TImageSize = (imLargeImages, imSmallImages);
   TSortType = (stString, stDateTime, stTime, stInteger, stFloat);
   TScrollBarKind = (sbHorizontal, sbVertical);

   TClickEvent = Procedure(Sender: TObject; row, col: Integer) Of Object;
   TScrlEvent = Procedure(Sender: TObject; Kind: TScrollBarKind) Of Object;
   TCompareEvent = Procedure(Sender: TObject; Var Item1, Item2: String; Column: Integer) Of Object;
   TWinDropFilesEvent = Procedure(Sender: TObject; FileList: TStringList) Of Object;
   TWinDropFilesStartEvent = Procedure(Sender: TObject; Var FileSpec: String) Of Object;
   TWinDropFilesEndEvent = Procedure(Sender: TObject) Of Object;

   TZipView = Class;
   TztvListBox = Class;
   TListHeader = Class;
   TLongEdit = Class;
   TScrlBox = Class;

   TScrlBar = Class(TPersistent)
   Private
      FOldTopIndex: Integer;
      FControl: TZipView;
      FScrlBox: TScrlBox;
      FKind: TScrollBarKind;
      FMin: Integer;
      FMax: Integer;
      FPage: Integer;
      FPosition: Integer;
      FTrackPos: Integer;
      FLargeChange: Integer;
      FSmallChange: Integer;
      FThumbValue: Integer;
      FVisible: Boolean;
      FScrollInfo: TScrollInfo;
      Procedure SetMin(value: Integer);
      Procedure SetMax(value: Integer);
      Procedure SetSmallChange(value: Integer);
      Procedure SetLargeChange(value: Integer);
      Procedure SetPage(value: Integer);
      Procedure SetPosition(value: Integer);
      Function GetTrackPos: Integer;
      Procedure SetScrollData(value: Integer; Mask: Cardinal; MinMax: Boolean);
      Function GetScrollData(Mask: Cardinal): Integer;
      Procedure ScrollMessage(Var msg: TWMScroll);
      Procedure SetVisible(value: Boolean);
      Function GetBarKind: word;
      Constructor Create(AControl: TScrlBox; AKind: TScrollBarKind);
   Protected
   Public
      Procedure Assign(source: TPersistent); Override;
      Property Kind: TScrollBarKind Read FKind;
      Property TrackPos: Integer Read GetTrackPos;
      Property Min: Integer Read FMin Write SetMin;
      Property Max: Integer Read FMax Write SetMax;
      Property Position: Integer Read FPosition Write SetPosition;
   Published
      Property SmallChange: Integer Read FSmallChange Write SetSmallChange;
      Property LargeChange: Integer Read FLargeChange Write SetLargeChange;
      Property ThumbValue: Integer Read FThumbValue Write FThumbValue;
      Property Visible: Boolean Read FVisible Write SetVisible;
   End;

   THorzScrollBar = Class(TScrlBar);
   TVertScrollBar = Class(TScrlBar);

   TScrlBox = Class(TWinControl)
   Private
      //FColor: TColor;
      FVersion: Boolean;
      FBorderStyle: TBorderStyle;
      FHorzScrollBar: THorzScrollBar;
      FVertScrollBar: TVertScrollBar;
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
      FFlat: Boolean;
{$ENDIF}
{$ENDIF}
		fOnWinDropFilesStart: TWinDropFilesStartEvent;
      fOnWinDropFilesEnd: TWinDropFilesEndEvent;
      FOnResize: TNotifyEvent;
      fOnChange: TScrlEvent;
      Procedure WMHScroll(Var message: TWMHScroll); Message WM_HSCROLL;
      Procedure WMVSCroll(Var message: TWMVScroll); Message WM_VSCROLL;
      Procedure WMSize(Var message: TWMSize); Message WM_SIZE;
      Procedure WMNCHitTest(Var message: TMessage); Message WM_NCHITTEST;
      Procedure CMCtl3DChanged(Var message: TMessage); Message CM_CTL3DCHANGED;
      Procedure SetHorzScrollBar(value: THorzScrollBar);
      Procedure SetVertScrollBar(value: TVertScrollBar);
      Procedure SetBorderStyle(value: TBorderStyle);
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
      Procedure SetFlat(value: Boolean);
{$ENDIF}
{$ENDIF}
   Protected
      Procedure CreateParams(Var Params: TCreateParams); Override;
      Procedure CreateWnd; Override;

{$IFNDEF DEL4_OR_HIGHER}
      Procedure Resize; Dynamic;
{$ENDIF}

      Property HorzScrollBar: THorzScrollBar Read FHorzScrollBar Write SetHorzScrollBar;
      Property VertScrollBar: TVertScrollBar Read FVertScrollBar Write SetVertScrollBar;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
   Published
      Property Align;
      Property BorderStyle: TBorderStyle Read FBorderStyle Write SetBorderStyle Default bsSingle;
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
      Property Flat: Boolean Read FFlat Write SetFlat;
{$ENDIF}
{$ENDIF}
      Property Color Nodefault;
      Property Ctl3D;
      Property Font;
      Property ParentColor;
      Property ParentCtl3D;
      Property ParentFont;
      Property ParentShowHint;
      Property PopupMenu;
      Property ShowHint;
      Property TabOrder;
      Property TabStop;
      Property Visible;
      Property OnClick;
      Property OnDblClick;
      Property OnDragDrop;
      Property OnDroppedFilesEnd: TWinDropFilesEndEvent Read fOnWinDropFilesEnd Write fOnWinDropFilesEnd;
      Property OnDroppedFilesStart: TWinDropFilesStartEvent Read fOnWinDropFilesStart Write fOnWinDropFilesStart;
      Property OnDragOver;
      Property OnEndDrag;
      Property OnEnter;
      Property OnExit;
      Property OnMouseDown;
      Property OnMouseMove;
      Property OnMouseUp;
      Property OnResize: TNotifyEvent Read FOnResize Write FOnResize;
      Property OnChange: TScrlEvent Read fOnChange Write fOnChange;
   End;

   TZipView = Class(TScrlBox)
      Procedure DefaultHandler(Var message); Override;
   Private
   	Hndl: Integer;
      //FOnRClick: TClickEvent;
      ZipView_InternalPasteMessage: Cardinal; //integer;
      ListRect: TRect;
      FList: TList;
      FSortType: TSortType;
      FOldSectionIndex: Integer;
      FSortDirection: TSortDirection;
      GlyphOffset: Integer;
      fWinDropFiles: TWinDropFilesEvent;
      //fDragDrop_DirSpec: String;

      {IFNDEF VER120}
      (* LongList *)
      LongLabel: TLongEdit;
      OldIndex: Integer;
      OldCol: Integer;
      ColLeft: Integer;
      fBackColor: TColor;
      fTextColor: TColor;
      fOnlyWhenFocused: Boolean;
      OldFormMouseMove: TMouseMoveEvent;
      (* end LongList *)
      {ENDIF}

      FClickEvent: TClickEvent;
      FSortCompare: TCompareEvent;

      FDelimiter: Str1;
      FImageList: TImageSize;
      FBitmap: Graphics.TBitmap;
      SmallImages: TImageList;
      LargeImages: TImageList;
      FHeader: TListHeader;
      FHeaderTextVPos: THeaderVertPos;
      FSectionClick: TSectionNotifyEvent;
      FSelectColor: TColor;
      FSepColor: TColor;
      FSepWidth: Integer;
      FShowIcons: Boolean;
      FExpandColText: Boolean;
      FTransparentImage: Boolean;
      Function FocusLeftSection: Integer;
      Function FocusRightSection: Integer;
      //function FocusLastSection: integer;
      Function GetSections: THeaderSections;
      Procedure DoHeaderSized(HeaderControl: THeaderControl; Section: THeaderSection);
      Procedure SetSections(value: THeaderSections);
      //function Compare(i: integer; Item2: string): integer;
      Procedure QuickSort(L, R: Integer);
      //procedure QuickSort(SortList: PPointerList; L, R: Integer; SCompare: TListSortCompare);
      //procedure QuickSort1(L, R: Integer);
   Protected
      Procedure CreateWnd; Override;
      Function GetImageIndex(Index: Integer): Integer;
      Function GetImageList(Option: Integer): TImageList;
      //procedure DrawItem( Control: TWinControl; Index: integer; Rect: TRect; state: TOwnerDrawState ); override;
      //procedure ItemDrawn( Control: TWinControl; Index: integer; Rect: TRect; state: TOwnerDrawState );
      Function GetColor: TColor;
      Procedure SetColor(C: TColor);
      Function GetColumn(x, y: Integer): String;

{$IFDEF DEL4_OR_HIGHER}
      Function GetEnabled: Boolean; Override;
      Procedure SetEnabled(b: Boolean); Override;
{$ELSE}
      Function GetEnabled: Boolean;
      Procedure SetEnabled(b: Boolean);
{$ENDIF}

      Function GetExtended: Boolean;
      Procedure SetExtended(b: Boolean);
      Function GetDelimiter: Str1;
      Procedure SetDelimiter(s: Str1);
      Function GetFont: TFont;
      Procedure SetFont(f: TFont);
      //function GetHeaderAlign: TAlign;
      //procedure SetHeaderAlign( al: TAlign );
      Function GetHeaderHeight: Integer;
      Procedure SetHeaderHeight(H: Integer);
      Function GetHeaderFont: TFont;
      Procedure SetHeaderFont(f: TFont);
      Function GetHeaderVisible: Boolean;
      Procedure SetHeaderTextVPos(shtvp: THeaderVertPos);
      Procedure SetHeaderVisible(shv: Boolean);
      Function GetHeaderEnabled: Boolean;
      Procedure SetHeaderEnabled(she: Boolean);
      Function GetItemHeight: Integer;
      Procedure SetItemHeight(n: Integer);
      Procedure DoOnKeyUp(Sender: TObject; Var key: word; Shift: TShiftState);
      Procedure DoOnKeypress(Sender: TObject; Var key: char);
      Procedure DoOnExit(Sender: TObject);
      Procedure DoOnEnter(Sender: TObject);
      Procedure DoOnStartDrag(Sender: TObject; Var DragObject: TDragObject);
      Procedure DoOnEndDrag(Sender, Target: TObject; x, y: Integer);
      Procedure DoOnDragOver(Sender, source: TObject; x, y: Integer; state: TDragState; Var Accept: Boolean);
      Procedure DoOnDragDrop(Sender, source: TObject; x, y: Integer);
      Procedure DoOnClick(Sender: TObject);
      Procedure DoOnDblClick(Sender: TObject);
      Procedure DoOnKeyDown(Sender: TObject; Var key: word; Shift: TShiftState);
      Function GetMulti: Boolean;
      Procedure SetMulti(b: Boolean);
      Function GetItemIndex: Integer;
      Procedure SetItemIndex(n: Integer);
      //Function GetIntegralHeight: Boolean;
      //Procedure SetIntegralHeight( sih: Boolean );
      Function GetItems: TStrings;
      Procedure SetItems(str: TStrings);
      Function GetSelected(n: Integer): Boolean;
      Procedure SetSelected(n: Integer; b: Boolean);
      Function GetSorted: Boolean;
      Procedure SetSorted(b: Boolean);
      Function GetList: TztvListBox;    //TListBox;
      Function GetSelCount: Integer;
      Procedure SetTransparentImage(value: Boolean);
      Function IsVertScrollBarVisible: Boolean;
      {.$ifndef DEL4_OR_HIGHER}
      Procedure Resize; Override;
      {.$endif}
      {IFNDEF VER120}
      (* LongList *)
      Procedure FListMouseMove(Sender: TObject; Shift: TShiftState; x, y: Integer); //override;
      Procedure FormMouseMove(Sender: TObject; Shift: TShiftState; x, y: Integer);
      Function ColAtPoint(t: TPoint): Integer;
      (* end LongList *)
      {ENDIF}
   Public
      FListBox: TztvListBox;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Loaded; Override;
      Procedure Sort;
      Procedure Clear;
      Function DeleteSelected: Integer;
      Property Column[x, y: Integer]: String Read GetColumn;
      Procedure SetFocus; Override;
      Procedure DoSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
      Function ItemidxColidxAtPos(Pos: TPoint; Existing: Boolean): TPoint;
      Function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
      Property ListBox: TztvListBox Read GetList;
      Property ItemIndex: Integer Read GetItemIndex Write SetItemIndex;
      Property SelCount: Integer Read GetSelCount;
      Property Selected[x: Integer]: Boolean Read GetSelected Write SetSelected;
      Property Showing;
      Property SortDirection: TSortDirection Read FSortDirection Write FSortDirection Default sdAscending;
      Property Sorted: Boolean Read GetSorted Write SetSorted;
      Property SortType: TSortType Read FSortType Write FSortType Default stString;
      {IFNDEF VER120}
           (* LongList *)
      Property OnlyWhenFocused: Boolean Read fOnlyWhenFocused Write fOnlyWhenFocused;
      Property BackColor: TColor Read fBackColor Write fBackColor;
      Property TextColor: TColor Read fTextColor Write fTextColor;
      (* end LongList *)
  {ENDIF}
   Published
      Property Align;
      //property BorderStyle;
      Property Color: TColor Read GetColor Write SetColor;
      Property Ctl3D;
      Property Delimiter: Str1 Read GetDelimiter Write SetDelimiter;
      Property DragCursor;
      Property DragMode;
      Property Enabled: Boolean Read GetEnabled Write SetEnabled;
      Property ExtendedSelect: Boolean Read GetExtended Write SetExtended Default True;
      //Property DragDrop_DirSpec: String Read fDragDrop_DirSpec Write fDragDrop_DirSpec;
      Property Font: TFont Read GetFont Write SetFont;
      Property HeaderFont: TFont Read GetHeaderFont Write SetHeaderFont;
      //property HeaderAlign: TAlign read GetHeaderAlign write SetHeaderAlign default alTop;
      Property HeaderEnabled: Boolean Read GetHeaderEnabled Write SetHeaderEnabled Default True;
      Property HeaderHeight: Integer Read GetHeaderHeight Write SetHeaderHeight;
      Property HeaderSections: THeaderSections Read GetSections Write SetSections;
      Property HeaderTextVPos: THeaderVertPos Read FHeaderTextVPos Write SetHeaderTextVPos Default dtCenter;
      Property HeaderVisible: Boolean Read GetHeaderVisible Write SetHeaderVisible Default True;
      Property Hint;
      Property ImageList: TImageSize Read FImageList Write FImageList Default imSmallImages;
      //property IntegralHeight: Boolean read GetIntegralHeight write SetIntegralHeight default True;
      Property ItemHeight: Integer Read GetItemHeight Write SetItemHeight;
      Property Items: TStrings Read GetItems Write SetItems;
      Property MultiSelect: Boolean Read GetMulti Write SetMulti Default True;
      Property ParentColor;
      Property ParentCtl3D;
      Property ParentFont;
      Property ParentShowHint;
      Property PopupMenu;
      Property ShowIcons: Boolean Read FShowIcons Write FShowIcons Default True;
      {IFNDEF VER120}
      Property ExpandColText: Boolean Read FExpandColText Write FExpandColText Default True;
      {ENDIF}
      Property TabOrder;
      Property TabStop;
      Property TransparentImage: Boolean Read FTransparentImage Write SetTransparentImage;
      Property Visible;
      Property SelectColor: TColor Read FSelectColor Write FSelectColor Default clBlack;
      Property SeperatorColor: TColor Read FSepColor Write FSepColor Default clSilver;
      Property SeperatorWidth: Integer Read FSepWidth Write FSepWidth Default 1;
      Property ShowHint;
      //property Sorted: Boolean read GetSorted write SetSorted;
      Property OnClick: TClickEvent Read FClickEvent Write FClickEvent;
      Property OnDblClick;
      Property OnDragDrop;
      Property OnDragOver;
      Property OnDroppedFiles: TWinDropFilesEvent Read fWinDropFiles Write fWinDropFiles;
      Property OnEndDrag;
      Property OnStartDrag;
      Property OnEnter;
      Property OnExit;
      Property OnHeaderClick: TSectionNotifyEvent Read FSectionClick Write FSectionClick;
      Property OnKeyDown;
      Property OnKeyPress;
      Property OnKeyUp;
      Property OnMouseDown;
      Property OnMouseMove;
      Property OnMouseUp;
      //property OnRClick: TClickEvent read FOnRClick write FOnRClick;
      Property OnSortCompare: TCompareEvent Read FSortCompare Write FSortCompare;
   End;

   TztvListBox = Class(TCustomListBox)
   Private
      FControl: TZipView;
      //Procedure wmLMouseDown(Var msg: TWMLButtonDown); Message wm_LButtonDown;
      //procedure wmRMouseDown(var msg:TWMLButtonDown); message wm_RButtonDown;
   Protected
      Procedure DrawItem(Index: Integer; rect: TRect; state: TOwnerDrawState); Override;
   Public
      Fx, Fy: Integer;
   Published
   End;

   TListHeader = Class(THeaderControl)
   Private
      FControl: TZipView;
      FSectionWidths: TList;
   Protected
      Procedure DrawSection(Section: THeaderSection; Const rect: TRect;
         Pressed: Boolean); Override;
      Procedure CreateWnd; Override;
      Procedure Resize; Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
   Published
   End;

   TLongEdit = Class(TEdit)
   Private
      FControl: TZipView;
      Procedure wmSetFocus(Var msg: TWmSetFocus); Message wm_setfocus;
      Procedure wmLMouseDown(Var msg: TWMLButtonDown); Message wm_LButtonDown;
      Procedure wmLButtonDblClk(Var msg: TWMLButtonDblClk); Message wm_LButtonDblClk;
      Procedure wmLButtonUp(Var msg: TWMLButtonUp); Message wm_LButtonUp;
   Public
      TheList: TComponent;
   End;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
Function InitializeFlatSB(HWND: HWND): Integer; Stdcall;
Function UninitializeFlatSB(HWND: HWND): Integer; Stdcall;
Function FlatSB_ShowScrollBar(HWND: HWND; SBKind: TScrollBarKind;
   Show: Boolean): Integer; Stdcall;
Function FlatSB_EnableScrollBar(HWND: HWND; SBKind: TScrollBarKind;
   value: Integer): Integer; Stdcall;
Function FlatSB_GetScrollInfo(HWND: HWND; BarFlag: Integer;
   Var ScrollInfo: TScrollInfo): Integer; Stdcall;
Function FlatSB_SetScrollInfo(HWND: HWND; BarFlag: Integer;
   Var ScrollInfo: TScrollInfo; Redraw: Boolean): Integer; Stdcall;

Function CheckComctlVersion: Boolean;
Function GetComCtlVersion(Var Minor: Integer; Var Major: Integer): Boolean;
{$ENDIF}
{$ENDIF}

Implementation

Type
   PDllVersionInfo = ^TDllVersionInfo;
   TDllVersionInfo = Packed Record
      size: Integer;
      MajorVersion: Integer;
      MinorVersion: Integer;
      BuildNumber: Integer;
      PlatformID: Integer;
   End;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
Var
   _GetDllVersionInfo: Function(Var dvInfo: TDllVersionInfo): Integer; Stdcall;
Const
   cctrl = 'comctl32.dll';
{$ENDIF}
{$ENDIF}

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
Function InitializeFlatSB; External cctrl name 'InitializeFlatSB';
Function UninitializeFlatSB; External cctrl name 'UninitializeFlatSB';
Function FlatSB_ShowScrollBar; External cctrl name 'FlatSB_ShowScrollBar';
Function FlatSB_EnableScrollBar; External cctrl name 'FlatSB_EnableScrollBar';
Function FlatSB_GetScrollInfo; External cctrl name 'FlatSB_GetScrollInfo';
Function FlatSB_SetScrollInfo; External cctrl name 'FlatSB_SetScrollInfo';
{$ENDIF}
{$ENDIF}

//-------------------------------------------------------------

//Procedure TztvListBox.wmLMouseDown(Var msg: TWMLButtonDown);
//Var
//   apoint: TPoint;
//Begin
//   Inherited;
//   {If Assigned(FControl.OnClick) Then
//   Begin
//      apoint := Point(Fx, Fy);
//      apoint := FControl.ItemidxColidxAtPos(apoint, True);
//      If apoint.x > -1 Then
//         FControl.OnClick(FControl.FListBox, apoint.x, apoint.y);
//   End;
//End;
//-------------------------------------------------------------
{procedure TztvListBox.WMRMouseDown(var msg:TWMRButtonDown);
var
 apoint: TPoint;
begin
 inherited;
 if Assigned( FControl.OnClick ) then
  begin
   apoint := point(Fx, Fy);
  apoint := FControl.ItemidxColidxAtPos( apoint, true );
     if apoint.x > -1 then
    FControl.OnRClick( FControl.FListBox, apoint.x, apoint.y );
  end;
end;}
//-------------------------------------------------------------

Constructor TListHeader.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TListHeader.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TListHeader.CreateWnd;
Var
   i: Integer;
Begin
   Inherited CreateWnd;
   If Sections.Count = 0 Then
   Begin
      For i := 0 To 13 Do
         Sections.Add;

      Sections[0].Text := 'Filename';
      Sections[1].Text := 'Date / Time';
      Sections[2].Text := 'Packed';
      Sections[3].Text := 'Unpacked';
      Sections[4].Text := 'Ratio';
      Sections[5].Text := 'Method';
      Sections[6].Text := 'Attr';
      Sections[7].Text := 'Folder';
      Sections[8].Text := 'File Type';
      Sections[9].Text := 'Crc';
      Sections[10].Text := 'File Offset';
      Sections[11].Text := 'Encrypted';
      Sections[12].Text := 'Volume ID';
      Sections[13].Text := 'File Comment';

      Sections[0].Width := 120;
      Sections[1].Width := 100;
      Sections[2].Width := 75;
      Sections[3].Width := 75;
      Sections[4].Width := 70;
      Sections[5].Width := 90;
      Sections[6].Width := 60;
      Sections[7].Width := 150;
      Sections[8].Width := 120;
      Sections[9].Width := 90;
      Sections[10].Width := 80;
      Sections[11].Width := 60;
      Sections[12].Width := 120;
      Sections[13].Width := 120;
   End;
End;
//-------------------------------------------------------------

Procedure TListHeader.Resize;
Begin
   Inherited Resize;
End;
//-------------------------------------------------------------
//procedure TListHeader.DoDrawnSection(HeaderControl: THeaderControl;	Section: THeaderSection; const Rect: TRect; Pressed: Boolean);

Procedure TListHeader.DrawSection(Section: THeaderSection; Const rect: TRect; Pressed: Boolean);
Var
   R: TRect;
   w: Byte;
   i, ww {, diff}: Integer;
Begin
   With {HeaderControl.}  Canvas Do
   Begin
      If Pressed Then
      Begin
         Font.Color := clRed;
      End
      Else
      Begin
         Font.Color := clBlack;

         If Section.Width <> Integer(FSectionWidths[Section.Index]) Then
         Begin
            //diff := Section.Width - Integer(FSectionWidths[Section.Index]);
            FSectionWidths[Section.Index] := Pointer(Section.Width);

            ww := 0;
            For i := 0 To Sections.Count - 1 Do
               ww := ww + Integer(FSectionWidths[i]);

            //if (ww - FControl.ClientWidth {- 16}) > FControl.HorzScrollBar.Max then
            //begin
            FControl.HorzScrollBar.Max := ww - FControl.ClientWidth;
            FControl.FHeader.Width := ww;
            FControl.FListBox.Width := ww + 16;
            //FControl.Resize;
         //end;
         End;

      End;

      Case FControl.FHeaderTextVPos Of
         dtBottom: w := DT_BOTTOM;
         dtCenter: w := DT_VCENTER;
         dtTop: w := DT_TOP;
      Else
         w := DT_VCENTER;
      End;

      R := rect;
      inc(R.left, (Font.size Div 2));
      Case Section.Alignment Of
         taLeftJustify:
            //TextRect( rect, rect.left + Font.Size, rect.Top + 2, Section.Text )
            DrawText(Canvas.Handle, PChar(Section.Text), -1, R,
               {DT_NOCLIP or } DT_SINGLELINE Or DT_LEFT Or w);
         taRightJustify:
            DrawText(Canvas.Handle, PChar(Section.Text), -1, R,
               {DT_NOCLIP or } DT_SINGLELINE Or DT_RIGHT Or w);
         taCenter:
            DrawText(Canvas.Handle, PChar(Section.Text), -1, R,
               {DT_NOCLIP or } DT_SINGLELINE Or DT_CENTER Or w);
      End;

   End;
End;
//-------------------------------------------------------------

Procedure TLongEdit.wmSetFocus(Var msg: TWmSetFocus);
Begin
End;
//-------------------------------------------------------------
(* Send the onclick event on to the ListBox *)

Procedure TLongEdit.wmLMouseDown(Var msg: TWMLButtonDown);
Begin
   If TheList Is TZipView Then
      With FControl.FListBox Do
         SendMessage(Handle, msg.msg, msg.keys, Makelong(Fx, Fy));
End;
//-------------------------------------------------------------

Procedure TLongEdit.wmLButtonUp(Var msg: TWMLButtonUp);
Begin
   If TheList Is TZipView Then
      With FControl.FListBox Do
         SendMessage(Handle, msg.msg, msg.keys, Makelong(Fx, Fy));
End;
//-------------------------------------------------------------

Procedure TLongEdit.wmLButtonDblClk(Var msg: TWMLButtonDblClk);
Begin
   If TheList Is TZipView Then
      With FControl.FListBox Do
         SendMessage(Handle, msg.msg, msg.keys, Makelong(Fx, Fy));
End;
//-------------------------------------------------------------

Function GetParentForm(Control: TControl): TForm;
Begin
   While Control.Parent <> Nil Do
      Control := Control.Parent;
   Result := Nil;
   If Control Is TForm Then Result := TForm(Control);
End;
//-------------------------------------------------------------

Constructor TZipView.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);

   FOldSectionIndex := -1;
   FImageList := imSmallImages;
   FDelimiter := ';';
   FShowIcons := True;
   FExpandColText := True;
   FSelectColor := clGreen;
   FSepColor := clSilver;
   FSepWidth := 1;
   //Ctl3d := false;
   ParentCtl3D := False;
   FList := TList.Create;
   FBitmap := Graphics.TBitmap.Create;
   LargeImages := GetImageList(SHGFI_LARGEICON);
   SmallImages := GetImageList(SHGFI_SMALLICON);
   FSortType := stString;
   FSortDirection := sdAscending;
   FHeaderTextVPos := dtCenter;

   HorzScrollBar.FControl := Self;
   VertScrollBar.FControl := Self;

   FHeader := TListHeader.Create(Self);
   With FHeader Do
   Begin
      Parent := Self;
      FControl := Self;
      FSectionWidths := TList.Create;
      Align := alNone;
      ParentFont := True;
      Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Color := clBtnFace;
      ParentFont := False;
      Enabled := True;
      TabStop := False;
      Ctl3D := False;
      Height := 21;
      //OnDrawSection := DoDrawnSection;
      OnSectionResize := DoHeaderSized;
      OnSectionClick := DoSectionClick;
   End;

   FListBox := TztvListBox.Create(Self);
   With FListBox Do
   Begin
      Parent := Self;
      FControl := Self;
      BorderStyle := bsNone;
      Align := alNone;
      ParentFont := True;
      Style := lbOwnerDrawFixed;
      IntegralHeight := False;
      Cursor := crArrow;
      MultiSelect := True;
      ItemHeight := 16;
      Sorted := False;
      OnClick := DoOnClick;
      OnDblClick := DoOnDblClick;
      OnDragDrop := DoOnDragDrop;
      OnDragOver := DoOnDragOver;
      OnStartDrag := DoOnStartDrag;
      OnEndDrag := DoOnEndDrag;
      //OnDrawItem := ItemDrawn;
      OnEnter := DoOnEnter;
      OnExit := DoOnExit;
      OnMouseMove := FListMouseMove;
      OnKeyDown := DoOnKeyDown;
      OnKeyPress := DoOnKeypress;
      OnKeyUp := DoOnKeyUp;
   End;

   (* LongList *)
   LongLabel := Nil;
   OldIndex := -1;
   OldCol := -1;
   ColLeft := 1;
   fOnlyWhenFocused := False;
   fBackColor := $00EAFFFF;
   fTextColor := clBlack;
   (* end LongList *)

   Color := FListBox.Color;
   BorderStyle := bsSingle;

   If FHeader.Visible Then
      FListBox.Top := FHeader.Height
   Else
      FListBox.Top := 0;

   ZipView_InternalPasteMessage := RegisterWindowMessage('ZipView DragDrop PasteMsg');
   //DragAcceptFiles( Handle, True );
End;
//-------------------------------------------------------------

Procedure TZipView.CreateWnd;
Begin
   Inherited CreateWnd;
End;
//-------------------------------------------------------------

Procedure TZipView.Loaded;
Var
   (* LongList *)
   aform: TForm;
   amousemove: TMouseMoveEvent;
   (* end LongList *)
   i, w: Integer;
Begin
   Inherited Loaded;

   Hndl := Handle;
   DragAcceptFiles(Handle, True);

   (* LongList *)
   aform := GetParentForm(Self);
   If aform <> Nil Then
   Begin
      amousemove := aform.OnMouseMove;
      If Assigned(amousemove) Then
         If @amousemove <> @OldFormMouseMove Then
            OldFormMouseMove := aform.OnMouseMove
         Else
            OldFormMouseMove := Nil;
      aform.OnMouseMove := FormMouseMove;
   End;
   (* end LongList *)

   FHeader.FSectionWidths.Clear;
   w := 0;
   For i := 0 To FHeader.Sections.Count - 1 Do
   Begin
      FHeader.FSectionWidths.Add(Pointer(FHeader.Sections[i].Width));
      FHeader.Sections[i].Style := hsOwnerDraw;
      w := w + FHeader.Sections[i].Width;
   End;

   If w < ClientWidth Then
      w := ClientWidth + 16;

   FHeader.left := 0;
   FHeader.Top := 0;
   FHeader.Width := w;

   (* Height is defined in TZipView.Resize *)
   //if FHeader.Visible then
   //	FListBox.Top := FHeader.height
   //else
   //	FListBox.Top := 0;

   FListBox.left := 0;
   FListBox.Width := w + 16;            (* include width of scrollbar *)

   VertScrollBar.Position := 0;
   VertScrollBar.Min := 0;
   VertScrollBar.Max := FListBox.Height + FHeader.Height - ClientHeight;
   VertScrollBar.FSmallChange := ItemHeight;

   HorzScrollBar.Position := 0;
   HorzScrollBar.Min := 0;
   HorzScrollBar.Max := w - ClientWidth;

   If HorzScrollBar.Max > 100 Then
      HorzScrollBar.FLargeChange := w Div FHeader.Sections.Count;

   If FHeader.Visible Then
      FListBox.Top := FHeader.Height
   Else
      FListBox.Top := 0;

   If FHeader.Visible Then
      FListBox.Height := ClientHeight - FHeader.Height
   Else
      FListBox.Height := ClientHeight;

End;
//-------------------------------------------------------------

Destructor TZipView.Destroy;
Begin
   FBitmap.Free();
   FHeader.Free();
   FListBox.Free();
   FList.Free();
   LargeImages.Free();
   SmallImages.Free();

   If Bool(Hndl) Then
   Begin
   	DragAcceptFiles(Hndl, False);
   	Hndl := 0;
   End;

   Inherited Destroy();
End;
//-------------------------------------------------------------

Procedure TZipView.SetFocus;
Begin
   FListBox.SetFocus();
End;
//-------------------------------------------------------------

Function TZipView.GetDelimiter: Str1;
Begin
   Result := FDelimiter;
End;
//-------------------------------------------------------------

Function TZipView.ColAtPoint(t: TPoint): Integer;
Var
   i, w: Integer;
   R: TRect;
Begin
   w := 0;
   Result := 0;
   For i := 0 To FHeader.Sections.Count - 1 Do
   Begin
      inc(w, FHeader.Sections[i].Width);
      If w > t.x Then
      Begin
         Dec(w, FHeader.Sections[i].Width);
         R := FListBox.Canvas.ClipRect;
         ColLeft := w - R.left;
         Result := i;
         break;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.FListMouseMove(Sender: TObject; Shift: TShiftState; x, y: Integer);
Var
   apoint: TPoint;
   AnItemIndex: longint;
   theitemrect: TRect;
   aHandle: HWND;
   col: Integer;
   tWidth: Integer;
   s: AnsiString;
Begin
   Inherited MouseMove(Shift, x, y);

   FListBox.Fx := x;
   FListBox.Fy := y;

   aHandle := getfocus;
   If (Not FExpandColText) Or ((fOnlyWhenFocused) And (aHandle <> Handle)) Then
      Exit;

   apoint := Point(x, y);
   col := ColAtPoint(apoint);
   AnItemIndex := ItemAtPos(apoint, True);

   If (OldIndex <> AnItemIndex) Or (OldCol <> col) Then
   Begin
      OldIndex := AnItemIndex;
      OldCol := col;

      If AnItemIndex > -1 Then
      Begin

         If LongLabel <> Nil Then
         Begin
            LongLabel.Free;
            LongLabel := Nil;
         End;

         s := Column[AnItemIndex, col];
         tWidth := FListBox.Canvas.textwidth(s);
         If (col = 0) And ShowIcons Then
            inc(tWidth, ItemHeight + 6 + 2);

         If tWidth > FHeader.Sections[col].Width Then
         Begin
            LongLabel := TLongEdit.Create(Self);
            LongLabel.Visible := False;
            LongLabel.Parent := GetParentForm(Self);
            LongLabel.FControl := Self;
            LongLabel.TheList := Self;
            theitemrect := FListBox.itemrect(AnItemIndex);
            LongLabel.BorderStyle := bsSingle;
            LongLabel.Cursor := FListBox.Cursor;
            LongLabel.Color := fBackColor;
            LongLabel.Font.Color := fTextColor;
            LongLabel.ReadOnly := True;
            LongLabel.Ctl3D := False;
            LongLabel.Font.Assign(FListBox.Font);
            //        Canvas.Font.Assign(FControl.FListBox.Font);
            LongLabel.Width := tWidth + 8;
            If col = 0 Then
               //LongLabel.left := ColLeft + 1 + (ItemHeight + 6 + 1)
               LongLabel.left := ColLeft + 1 + (GlyphOffset)
            Else
               LongLabel.left := ColLeft - 1;
            If FHeader.Visible Then
               LongLabel.Top := Top + theitemrect.Top + FHeader.Height - 1
            Else
               LongLabel.Top := Top + theitemrect.Top;
            //Longlabel.Height := Canvas.TextHeight( s ) + 4;
            LongLabel.Height := FListBox.Canvas.TextHeight(s) + 4;
            LongLabel.Text := s;
            LongLabel.BringToFront;
            LongLabel.Show;
         End;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.FormMouseMove(Sender: TObject; Shift: TShiftState; x, y: Integer);
Begin
   If Assigned(OldFormMouseMove) Then
      OldFormMouseMove(Sender, Shift, x, y);
   If LongLabel <> Nil Then
   Begin
      LongLabel.Free();
      LongLabel := Nil;
      OldIndex := -1;
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.SetDelimiter(s: Str1);
Begin
   FDelimiter := s;
End;
//-------------------------------------------------------------

Function TZipView.GetSections: THeaderSections;
Begin
   Result := FHeader.Sections;
End;
//-------------------------------------------------------------

Procedure TZipView.SetSections(value: THeaderSections);
Begin
   FHeader.Sections.Assign(value);
End;
//-------------------------------------------------------------
{function TZipView.GetHeaderAlign: TAlign;
begin
 Result := FHeader.Align;
end;}
//-------------------------------------------------------------
{procedure TZipView.SetHeaderAlign( al: TAlign );
begin
 FHeader.Align := al;
end;}
//-------------------------------------------------------------

Function TZipView.GetHeaderHeight: Integer;
Begin
   Result := FHeader.Height;
End;
//-------------------------------------------------------------

Procedure TZipView.SetHeaderHeight(H: Integer);
Begin
   FHeader.Height := H;
   If FHeader.Visible Then
   Begin
      FListBox.Top := FHeader.Height;
      FListBox.Height := ClientHeight - FHeader.Height
   End
   Else
      FListBox.Height := ClientHeight;
End;
//-------------------------------------------------------------

Function TZipView.GetHeaderFont: TFont;
Begin
   Result := FHeader.Font;
End;
//-------------------------------------------------------------

Procedure TZipView.SetHeaderFont(f: TFont);
Begin
   FHeader.Font.Assign(f);
End;
//-------------------------------------------------------------

Function TZipView.GetHeaderVisible: Boolean;
Begin
   Result := FHeader.Visible;
End;
//-------------------------------------------------------------

Procedure TZipView.SetHeaderVisible(shv: Boolean);
Begin
   FHeader.Visible := shv;
End;
//-------------------------------------------------------------

Procedure TZipView.SetHeaderTextVPos(shtvp: THeaderVertPos);
Begin
   FHeaderTextVPos := shtvp;
   FHeader.Repaint;
End;
//-------------------------------------------------------------

Function TZipView.GetHeaderEnabled: Boolean;
Begin
   Result := FHeader.Enabled;
End;
//-------------------------------------------------------------

Procedure TZipView.SetHeaderEnabled(she: Boolean);
Begin
   FHeader.Enabled := she;
End;
//-------------------------------------------------------------

Procedure TZipView.DoHeaderSized(HeaderControl: THeaderControl; Section: THeaderSection);
Begin
   FListBox.Refresh();
End;
//-------------------------------------------------------------

Procedure TZipView.DoSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
Begin
   If (Assigned(OnHeaderClick)) And (FListBox.Items.Count > 0) Then
   Begin
      If Section.Index <> FOldSectionIndex Then
      Begin
         FOldSectionIndex := Section.Index;
         FSortDirection := sdAscending;
      End
      Else
         Case FSortDirection Of
            sdAscending: FSortDirection := sdDecending;
            sdDecending: FSortDirection := sdAscending;
         End;

      OnHeaderClick(HeaderControl, Section);
   End;
End;
//-------------------------------------------------------------

Function TZipView.GetImageList(Option: Integer): TImageList;
Var
   SHFileInfo: TSHFileInfo;
Begin
   Result := TImageList.Create(Nil);
   With Result Do
   Begin
      ShareImages := True;
      Handle := ShGetFileInfo('' {'*.*'}, 0, SHFileInfo, SizeOf(TSHFileInfo),
         Option Or SHGFI_ATTRIBUTES Or SHGFI_SYSICONINDEX)
   End
End;
//-------------------------------------------------------------

Function TZipView.GetImageIndex(Index: Integer): Integer;
Var
   SHFileInfo: TSHFileInfo;
   flags: Integer;
   s: String;
Begin
   If Index > -1 Then
   Begin
      flags := SHGFI_USEFILEATTRIBUTES Or SHGFI_SYSICONINDEX;

      If FImageList = imSmallImages Then
         flags := flags Or SHGFI_SMALLICON
      Else
         flags := flags Or SHGFI_LARGEICON;

      s := FListBox.Items.Strings[Index];
      s := GetToken(s, FDelimiter);
      ShGetFileInfo(PChar(s),
         0,
         SHFileInfo,
         SizeOf(TSHFileInfo),
         flags);

      Result := SHFileInfo.iIcon;

   End
   Else
      Result := Index;
End;
//-------------------------------------------------------------
//procedure TZipView.ItemDrawn( Control: TWinControl; Index: integer; Rect: TRect; state: TOwnerDrawState );

Procedure TztvListBox.DrawItem(Index: Integer; rect: TRect; state: TOwnerDrawState);
Var
   i: Integer;
   sItem: String;
   rectDraw: TRect;
   cTemp: TColor;
   TransColor: TColor;
   ImageIndex: Integer;
   BmpWidth: Integer;
Begin

   With Canvas Do
   Begin

      If odSelected In state Then
         Brush.Color := FControl.FSelectColor
      Else
         Brush.Color := Color;

      Brush.Style := bsSolid;
      FillRect(rect);

      If FControl.FShowIcons Then
      Begin

         ImageIndex := FControl.GetImageIndex(Index);
         If FControl.FImageList = imSmallImages Then
            FControl.SmallImages.GetBitmap(ImageIndex, FControl.FBitmap)
         Else
            FControl.LargeImages.GetBitmap(ImageIndex, FControl.FBitmap);

         With FControl.FBitmap Do
         Begin
            If Width Mod Height = 0 Then
               BmpWidth := Width Div (Width Div Height)
            Else
               BmpWidth := Width;

            If FControl.FTransparentImage Then
               TransColor := FControl.FBitmap.TransparentColor
            Else
               TransColor := clNone;
         End;

         BrushCopy(
            Bounds(rect.left + 2, (rect.Top + rect.Bottom - ItemHeight) Div 2, ItemHeight, ItemHeight),
            FControl.FBitmap,
            Bounds(0, 0, BmpWidth, FControl.FBitmap.Height),
            TransColor);
         FControl.GlyphOffset := ItemHeight + 6;

      End
      Else
         FControl.GlyphOffset := 0;

      Brush.Style := bsClear;
      cTemp := Font.Color;

      (* foreground color of selected item *)
      If odSelected In state Then
         Font.Color := clWhite;

      sItem := Items[Index];
      rectDraw := rect;
      rectDraw.right := rectDraw.left - 2;

      For i := 0 To FControl.FHeader.Sections.Count - 1 Do
      Begin

         inc(rectDraw.right, FControl.FHeader.Sections[i].Width);

         If i = 0 Then
            TextRect(rectDraw, rectDraw.left + FControl.GlyphOffset + 2, rectDraw.Top, GetToken(sItem, FControl.FDelimiter))
         Else
            TextRect(rectDraw, rectDraw.left + 2, rectDraw.Top, GetToken(sItem, FControl.FDelimiter));

         Pen.Color := FControl.FSepColor;
         Pen.Width := FControl.FSepWidth;
         MoveTo(rectDraw.right, rectDraw.Top);
         If Pen.Width > 0 Then
            LineTo(rectDraw.right, rectDraw.Bottom);
         rectDraw.left := rectDraw.right;
      End;
      Font.Color := cTemp;
   End;
   FControl.VertScrollBar.Max := ((Items.Count + 2) * ItemHeight) - FControl.ClientHeight;
   FControl.VertScrollBar.FLargeChange := FControl.ListRect.Bottom - FControl.ListRect.Top;
End;
//-------------------------------------------------------------

Function TZipView.GetList: TztvListBox;
Begin
   Result := FListBox;
End;
//-------------------------------------------------------------

Function TZipView.IsVertScrollBarVisible: Boolean;
Begin
   Result := Items.Count * ItemHeight > ClientHeight;
End;
//-------------------------------------------------------------

{.$ifndef DEL4_OR_HIGHER}

Procedure TZipView.Resize;
Begin
   Inherited Resize;
   If FHeader.Visible Then
      FListBox.Height := ClientHeight - FHeader.Height
   Else
      FListBox.Height := ClientHeight;

   ListRect := FListBox.Canvas.ClipRect;
   If (FHeader.Width - ListRect.left < ClientWidth) Or (FHeader.Width < ClientWidth) Then
   Begin
      If ListRect.left > ClientWidth - (ListRect.right - ListRect.left) Then
      Begin
         If IsVertScrollBarVisible Then
         Begin
            FListBox.left := -(ListRect.right - ClientWidth);
            FHeader.left := FListBox.left;
         End
         Else
         Begin
            FListBox.left := -(ListRect.right - 16 - ClientWidth);
            FHeader.left := FListBox.left;
         End
      End
      Else
         If FListBox.Width > ClientWidth Then
         Begin
            FListBox.Width := FListBox.Width + (FListBox.Width - ListRect.left) + 16;
            FHeader.Width := FHeader.Width + (FListBox.Width - ListRect.left);
         End
         Else
         Begin
            FHeader.Width := ClientWidth;
            FListBox.Width := ClientWidth + 16;
         End
   End;
   HorzScrollBar.Max := FHeader.Width - ClientWidth
End;
{.$endif}
//-------------------------------------------------------------

Procedure TZipView.Clear;
Begin
   FListBox.Clear;
   FOldSectionIndex := -1;
End;
//-------------------------------------------------------------

Function TZipView.DeleteSelected: Integer;
Var
   i: Integer;
Begin
   Result := 0;
   i := 0;
   While i < Items.Count Do
   Begin
      If Selected[i] Then
      Begin
         Items.Delete(i);
         inc(Result);
      End
      Else
         inc(i);
   End;
End;
//-------------------------------------------------------------
(* Do not use! *)
{function TZipView.GetIntegralHeight: Boolean;
begin
 Result := FListBox.FIntegralHeight;
end;}
//-------------------------------------------------------------
(* Do not use! *)
{procedure TZipView.SetIntegralHeight( sih: Boolean );
begin
 FListBox.FIntegralHeight := sih;
end;}
//-------------------------------------------------------------

Function TZipView.GetItems: TStrings;
Begin
   Result := FListBox.Items;
End;
//-------------------------------------------------------------

Procedure TZipView.SetItems(str: TStrings);
Begin
   FListBox.Items.Assign(str);
End;
//-------------------------------------------------------------

Function TZipView.GetItemHeight: Integer;
Begin
   Result := FListBox.ItemHeight;
End;
//-------------------------------------------------------------

Procedure TZipView.SetItemHeight(n: Integer);
Begin
   FListBox.ItemHeight := n;
End;
//-------------------------------------------------------------

Function TZipView.GetColumn(x, y: Integer): String;
Var
   i: Integer;
   s: String;
Begin
   Result := '';
   If (x > -1) And
      (x < FListBox.Items.Count) And
      (y < FHeader.Sections.Count) Then
   Begin
      s := FListBox.Items[x];
      For i := 0 To y Do
         Result := GetToken(s, FDelimiter)
   End;
End;
//-------------------------------------------------------------

Function TZipView.GetEnabled: Boolean;
Begin
   Result := FListBox.Enabled;
End;
//-------------------------------------------------------------

Procedure TZipView.SetEnabled(b: Boolean);
Begin
   FListBox.Enabled := b;
   FHeader.Enabled := b;
End;
//-------------------------------------------------------------

Function TZipView.FocusLeftSection: Integer;
Var
   i, Index: Integer;
   R: TRect;
Begin
   Result := 0;
   R := FListBox.Canvas.ClipRect;
   For Index := 0 To FHeader.Sections.Count - 1 Do
   Begin
      inc(Result, FHeader.Sections[Index].Width);
      If Result > R.left Then
      Begin
         i := Index;
         While (Result >= R.left) And (Result > 0) Do
         Begin
            Dec(Result, FHeader.Sections[i].Width);
            Dec(i);
         End;
         break;
      End;
   End;
End;
//-------------------------------------------------------------

Function TZipView.FocusRightSection: Integer;
Var
   deltax: Integer;
Begin
   Result := 0;
   For deltax := 0 To FHeader.Sections.Count - 1 Do
   Begin
      inc(Result, FHeader.Sections[deltax].Width);
      If Result > HorzScrollBar.Position + Width Then
      Begin
         Dec(Result, Width);
         break;
      End;
   End;
End;
//-------------------------------------------------------------
{function TZipView.FocusLastSection: integer;
var
 r: TRect;
begin
 r := FListBox.Canvas.ClipRect;
  Result := r.Right;
end;}
//-------------------------------------------------------------

Procedure TZipView.DoOnKeyUp(Sender: TObject; Var key: word; Shift: TShiftState);
Begin
   If Assigned(OnKeyUp) Then
   	OnKeyUp(Self, key, Shift);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnKeypress(Sender: TObject; Var key: char);
Begin
   If Assigned(OnKeyPress) Then
   	OnKeyPress(Self, key);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnExit(Sender: TObject);
Begin
   If Assigned(OnExit) Then OnEnter(Self);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnEnter(Sender: TObject);
Begin
   If Assigned(OnEnter) Then OnEnter(Self);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnStartDrag(Sender: TObject; Var DragObject: TDragObject);
Begin
   If Assigned(OnStartDrag) Then
      OnStartDrag(Self, DragObject);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnEndDrag(Sender, Target: TObject; x, y: Integer);
Begin
   If Assigned(OnEndDrag) Then
      OnEndDrag(Self, Target, x, y);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnDragOver(Sender, source: TObject; x, y: Integer; state: TDragState; Var Accept: Boolean);
Begin
   If Assigned(OnDragOver) Then
      OnDragOver(Self, source, x, y, state, Accept);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnDragDrop(Sender, source: TObject; x, y: Integer);
Begin
   If Assigned(OnDragDrop) Then
      OnDragDrop(Self, source, x, y);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnClick(Sender: TObject);
Var
   apoint: TPoint;
Begin
   If Assigned(OnClick) Then
   Begin
      apoint := Point(FListBox.Fx, FListBox.Fy);
      apoint := ItemidxColidxAtPos(apoint, True);
      //If apoint.x > -1 Then
      	//FControl.OnClick(FControl.FListBox, apoint.x, apoint.y);
   		OnClick(Self, apoint.x, apoint.y);
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnDblClick(Sender: TObject);
Begin
   If Assigned(OnDblClick) Then OnDblClick(Self);
End;
//-------------------------------------------------------------

Procedure TZipView.DoOnKeyDown(Sender: TObject; Var key: word; Shift: TShiftState);
Begin
   If LongLabel <> Nil Then
   Begin
      LongLabel.Free;
      LongLabel := Nil;
   End;

   If (Shift = [ssCTRL]) Then
      Case key Of
         VK_RIGHT:
            Begin
               HorzScrollBar.Position := HorzScrollBar.Max;
               FHeader.left := -HorzScrollBar.Position;
               FListBox.left := -HorzScrollBar.Position;
               FHeader.Repaint();
               key := 0;
            End;
         VK_LEFT:
            Begin
               HorzScrollBar.Position := 0;
               FHeader.left := 0;
               FListBox.left := 0;
               key := 0;
            End;
      End
   Else
      If (Shift = [ssALT]) Then
      Else
         Case key Of
            VK_RIGHT:
               Begin
                  HorzScrollBar.Position := FocusRightSection;
                  If HorzScrollBar.Position <= HorzScrollBar.Max Then
                  Begin
                     FHeader.left := -HorzScrollBar.Position;
                     FListBox.left := -HorzScrollBar.Position;
                     FHeader.Repaint();
                  End
                  Else
                     HorzScrollBar.Position := HorzScrollBar.Max;
                  key := 0;
               End;
            VK_LEFT:
               Begin
                  HorzScrollBar.Position := FocusLeftSection;
                  FHeader.left := -HorzScrollBar.Position;
                  FListBox.left := -HorzScrollBar.Position;
                  FHeader.Repaint();
                  key := 0;
               End;
            VK_DOWN: ;                  (* Deals with vertScrollBar *)
            VK_UP: ;                    (* Deals with vertScrollBar *)
            VK_PRIOR: ;
            VK_NEXT: ;
         End;

End;
//-------------------------------------------------------------

Function TZipView.GetMulti: Boolean;
Begin
   Result := FListBox.MultiSelect;
End;
//-------------------------------------------------------------

Procedure TZipView.SetMulti(b: Boolean);
Begin
   FListBox.MultiSelect := b;
End;
//-------------------------------------------------------------

Function TZipView.GetItemIndex: Integer;
Begin
   Result := FListBox.ItemIndex;
End;
//-------------------------------------------------------------

Procedure TZipView.SetItemIndex(n: Integer);
Begin
   FListBox.ItemIndex := n;
End;
//-------------------------------------------------------------

Function TZipView.GetSelected(n: Integer): Boolean;
Begin
   Result := FListBox.Selected[n];
End;
//-------------------------------------------------------------

Procedure TZipView.SetSelected(n: Integer; b: Boolean);
Begin
	FListBox.Selected[n] := b;
End;
//-------------------------------------------------------------

Function TZipView.GetSelCount: Integer;
Begin
   Result := FListBox.SelCount;
End;
//-------------------------------------------------------------

Function TZipView.GetSorted: Boolean;
Begin
   Result := FListBox.Sorted;
End;
//-------------------------------------------------------------

Procedure TZipView.SetSorted(b: Boolean);
Begin
   FListBox.Sorted := b;
End;
//-------------------------------------------------------------

Function TZipView.GetColor: TColor;
Begin
   Result := FListBox.Color;
End;
//-------------------------------------------------------------

Procedure TZipView.SetColor(C: TColor);
Begin
   FListBox.Color := C;
End;
//-------------------------------------------------------------

Function TZipView.GetExtended: Boolean;
Begin
   Result := FListBox.ExtendedSelect;
End;
//-------------------------------------------------------------

Procedure TZipView.SetExtended(b: Boolean);
Begin
   FListBox.ExtendedSelect := b;
End;
//-------------------------------------------------------------

Function TZipView.GetFont: TFont;
Begin
   Result := FListBox.Font;
End;
//-------------------------------------------------------------

Procedure TZipView.SetFont(f: TFont);
Begin
   FListBox.Font.Assign(f);
End;
//-------------------------------------------------------------

Function TZipView.ItemidxColidxAtPos(Pos: TPoint; Existing: Boolean): TPoint;
Begin
   Result.x := FListBox.ItemAtPos(Pos, Existing);
   Result.y := ColAtPoint(Pos);
End;
//-------------------------------------------------------------

Function TZipView.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
Begin
   Result := FListBox.ItemAtPos(Pos, Existing);
End;
//-------------------------------------------------------------

Procedure TZipView.SetTransparentImage(value: Boolean);
Begin
   If FTransparentImage <> value Then
   Begin
      FTransparentImage := value;
      Invalidate;
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.Sort;
Var
   i: Integer;
   FSortedList: TStringList;
Begin
   FListBox.Items.BeginUpdate;
   FSortedList := TStringList.Create;
   Try
      FSortedList.Assign(FListBox.Items);
      Try
         For i := 0 To FListBox.Items.Count - 1 Do
            FList.Add(Pointer(i));

         QuickSort(0, FList.Count - 1);
      Finally
         VertScrollBar.Position := 0;
         FListBox.Items.Clear;
         For i := 0 To FList.Count - 1 Do
            FListBox.Items.Add(FSortedList.Strings[Integer(FList.Items[i])]);
      End;
   Finally
      FListBox.Items.EndUpdate;
      FList.Clear;
      FSortedList.Free;
   End;
End;
//-------------------------------------------------------------
{procedure TZipView.QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do Inc(I);
      while SCompare(SortList^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;}

Procedure TZipView.QuickSort(l, R: Integer);

//-------------------------------------------------------------

   Function Compare(i: Integer; Item2: String): Integer;
      //-------------------------------------------------------------

      Function StringCompare(Str1, Str2: String): Integer;
      Begin
         Result := AnsiCompareText(Str1, Str2);
      End;
      //-------------------------------------------------------------

      Function TimeCompare(Str1, Str2: String): Integer;
      Var
         Val1, Val2: TDateTime;
      Begin
         Try
            Val1 := StrToTime(Str1);
            Val2 := StrToTime(Str2);
            If Val1 < Val2 Then
               Result := -1
            Else
               If Val2 < Val1 Then
                  Result := 1
               Else
                  Result := 0;
         Except
            On EConvertError Do Result := 0;
         End;
      End;
      //-------------------------------------------------------------

      Function DateCompare(Str1, Str2: String): Integer;
      Var
         Val1, Val2: TDateTime;
      Begin
         Val1 := StrToDateTime(Str1);
         Val2 := StrToDateTime(Str2);
         //Result := Round( 1E5 * ( Val1 - Val2 ) );
         If Val1 < Val2 Then
            Result := -1
         Else
            If Val2 < Val1 Then
               Result := 1
            Else
               Result := 0;
      End;
      //-------------------------------------------------------------

      Function FloatCompare(Str1, Str2: String): Integer;
      Var
         Val1, Val2: Extended;
      Begin
         Val1 := StrToFloat(Str1);
         Val2 := StrToFloat(Str2);
         If Val1 < Val2 Then
            Result := -1
         Else
            If Val2 < Val1 Then
               Result := 1
            Else
               Result := 0;
      End;
      //-------------------------------------------------------------

      Function IntegerCompare(Str1, Str2: String): Integer;
      Var
         Val1, Val2: Integer;
      Begin
         Val1 := StrToInt(Str1);
         Val2 := StrToInt(Str2);
         If Val1 < Val2 Then
            Result := -1
         Else
            If Val2 < Val1 Then
               Result := 1
            Else
               Result := 0;
      End;
      //-------------------------------------------------------------
   Var
      Item1: String;
   Begin
      Item1 := GetColumn(i, FOldSectionIndex);
      If Assigned(OnSortCompare) Then
         OnSortCompare(Self, Item1, Item2, FOldSectionIndex);

      Case FSortType Of
         stString: Result := StringCompare(Item1, Item2);
         stDateTime: Result := DateCompare(Item1, Item2);
         stTime: Result := TimeCompare(Item1, Item2);
         stFloat: Result := FloatCompare(Item1, Item2);
         stInteger: Result := IntegerCompare(Item1, Item2);
      Else
         Result := StringCompare(Item1, Item2);
      End;
      If FSortDirection = sdDecending Then Result := -Result;
   End;
   //-------------------------------------------------------------
Var
   i, j, Mid, t: Integer;
   Item2: String;
Begin
   If (l >= R) Then Exit;
   Try
      Repeat
         i := l;
         j := R;
         Mid := (l + R) Shr 1;
         Item2 := GetColumn(Integer(FList[Mid]), FOldSectionIndex);
         Repeat
            While Compare(Integer(FList[i]), Item2) < 0 Do
               inc(i);
            While Compare(Integer(FList[j]), Item2) > 0 Do
               Dec(j);

            If (i <= j) Then
            Begin
               t := Integer(FList[i]);
               FList[i] := FList[j];
               FList[j] := Pointer(t);
               inc(i);
               Dec(j);
            End
            Else
               If i = j Then
               Begin
                  inc(i);
                  Dec(j);
               End;
         Until i > j;

         If l < j Then QuickSort(l, j);
         l := i;

      Until i >= R;
   Except
      On EConvertError Do
         ;
   End;
End;
//-------------------------------------------------------------

Procedure TZipView.DefaultHandler(Var message);
Var
   DroppedFilesCnt: Integer;
   DropIndex: Integer;
   DragDrop_DirSpec,
   FileName: String;
   FilenameList: TStringList;
   Buffer: PChar;
   BufferSize: Integer;
   FindData: TWin32FindData;
   FoundData: TFoundData;
   i: Integer;
   Handle: THandle;
Begin
   Inherited DefaultHandler(message);
   With TMessage(message) Do
      If (msg = WM_DROPFILES) Or
         (msg = ZipView_InternalPasteMessage) Then
      Begin
      	DragDrop_DirSpec := '*.*';
      	If Assigned(fOnWinDropFilesStart) Then
         Begin
         	fOnWinDropFilesStart(Self, DragDrop_DirSpec);
            If Length(DragDrop_DirSpec) = 0 Then
            	DragDrop_DirSpec := '*.*';
         End;

         FilenameList := TStringList.Create();
         Try
            DroppedFilesCnt := DragQueryFile(wParam, $FFFFFFFF, PChar(0), 0);
            For DropIndex := 0 To DroppedFilesCnt - 1 Do
            Begin
               BufferSize := DragQueryFile(wParam, DropIndex, PChar(0), 0);
               GetMem(Buffer, BufferSize + 1);
               Try
                  DragQueryFile(wParam, DropIndex, Buffer, BufferSize + 1);
                  FileName := StrPas(Buffer);

                  FoundData := TFoundData.Create();

                  Handle := FindFirstFile(Buffer, FindData);
                  Try
                     If Handle <> INVALID_HANDLE_VALUE Then
                     Begin
                        If FindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0 Then
                           If Length(DragDrop_DirSpec) > 0 Then
                              FileName := SlashSep(FileName, DragDrop_DirSpec)
                           Else
                              FileName := SlashSep(FileName, '*.*');

                        CopyMem(@FindData, @FoundData.Data, SizeOf(FoundData.Data));
                        FilenameList.AddObject(FileName, FoundData);
                     End;
                  Finally
                     Windows.FindClose(Handle);
                  End;

               Finally
                  FreeMem(Buffer);
               End;

            End;

            If DroppedFilesCnt > 0 Then
               If Assigned(OnDroppedFiles) Then
                  OnDroppedFiles(Self, FilenameList);

         Finally
            If msg = WM_DROPFILES Then
               DragFinish(wParam);


            For i := 0 To FilenameList.Count - 1 Do
               FilenameList.Objects[i].Free();

            FilenameList.Free();

            //For i := 0 To FilesList.Count - 1 Do
            //Begin
            //	pFindData := FilesList.Items[i];
            //	Dispose( pFindData );
            //End;
            //FilesList.Free();

				If Assigned(fOnWinDropFilesEnd) Then
            	fOnWinDropFilesEnd(Self);
         End;
      End;

End;
//-------------------------------------------------------------

{ TScrlBox }

//-------------------------------------------------------------

Constructor TScrlBox.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);

   FHorzScrollBar := THorzScrollBar.Create(Self, sbHorizontal);
   FVertScrollBar := TVertScrollBar.Create(Self, sbVertical);

   ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
      csSetCaption, csDoubleClicks, csOpaque];

   //check the comctl32.dll version first
   //if not version 4.71 or higher, only normal scrollbar can ba used
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   FVersion := CheckComctlVersion;
{$ENDIF}
{$ENDIF}

   Width := 185;
   Height := 97;
   FBorderStyle := bsNone;

   //Default use normal scrollbar
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   FFlat := False;
{$ENDIF}
{$ENDIF}
End;
//-------------------------------------------------------------

Procedure TScrlBox.CreateParams(Var Params: TCreateParams);
Const
   BorderStyles: Array[TBorderStyle] Of DWord = (0, WS_BORDER);
Begin
   Inherited CreateParams(Params);
   With Params Do
   Begin
      Style := Style Or BorderStyles[FBorderStyle];
      //WindowClass.style := WindowClass.style or CS_HREDRAW or CS_VREDRAW;
      If NewStyleControls And Ctl3D And (FBorderStyle = bsSingle) Then
      Begin
         Style := Style And Not WS_BORDER;
         If FVersion = True Then
         Begin
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
            If FFlat = True Then
               //for better visual effect if use flat scrollbar
               ExStyle := ExStyle Or WS_EX_STATICEDGE
            Else
{$ENDIF}
{$ENDIF}
               ExStyle := ExStyle Or WS_EX_CLIENTEDGE;
         End
         Else
            ExStyle := ExStyle Or WS_EX_CLIENTEDGE;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TScrlBox.CreateWnd;
Begin
   Inherited CreateWnd;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   If FVersion = True Then
   Begin
      If FFlat = True Then
         InitializeFlatSB(Handle)
      Else
         UninitializeFlatSB(Handle);

      If FHorzScrollBar.Visible = True Then
         FlatSB_ShowScrollBar(Handle, sbHorizontal, True);

      If FVertScrollBar.Visible = True Then
         FlatSB_ShowScrollBar(Handle, sbVertical, True);
   End
   Else
   Begin
{$ENDIF}
{$ENDIF}
      If FHorzScrollBar.Visible = True Then
         ShowScrollBar(Handle, SB_HORZ, True);

      If FVertScrollBar.Visible = True Then
         ShowScrollBar(Handle, SB_VERT, True);
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   End;
{$ENDIF}
{$ENDIF}
End;
//-------------------------------------------------------------

Destructor TScrlBox.Destroy;
Begin
   FHorzScrollBar.Free();
   FVertScrollBar.Free();
   Inherited Destroy();
End;
//-------------------------------------------------------------
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}

Function CheckComctlVersion: Boolean;
Var
   Minor, Major: Integer;
Begin
   Result := GetComCtlVersion(Minor, Major);
End;
{$ENDIF}
{$ENDIF}
//-------------------------------------------------------------

Procedure TScrlBox.WMHScroll(Var message: TWMHScroll);
Begin
   If (message.ScrollBar = 0) And FHorzScrollBar.Visible Then
      FHorzScrollBar.ScrollMessage(message)
   Else
      Inherited;
End;
//-------------------------------------------------------------

Procedure TScrlBox.WMVSCroll(Var message: TWMVScroll);
Begin
   If (message.ScrollBar = 0) And FVertScrollBar.Visible Then
      FVertScrollBar.ScrollMessage(message)
   Else
      Inherited;
End;
//-------------------------------------------------------------

Procedure TScrlBox.SetHorzScrollBar(value: THorzScrollBar);
Begin
   FHorzScrollBar.Assign(value);
End;
//-------------------------------------------------------------

Procedure TScrlBox.SetVertScrollBar(value: TVertScrollBar);
Begin
   FVertScrollBar.Assign(value);
End;
//-------------------------------------------------------------

Procedure TScrlBox.SetBorderStyle(value: TBorderStyle);
Begin
   If value <> FBorderStyle Then
   Begin
      FBorderStyle := value;
      RecreateWnd;
   End;
End;
//-------------------------------------------------------------
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}

Procedure TScrlBox.SetFlat(value: Boolean);
Begin
   If (FVersion = False) And value Then
   Begin
      ShowMessage(LoadStr(E_FLATSB));
      Exit;
   End;

   If FFlat <> value Then
   Begin
      FFlat := value;
      RecreateWnd;
   End;
End;
{$ENDIF}
{$ENDIF}
//-------------------------------------------------------------

{$IFNDEF DEL4_OR_HIGHER}

Procedure TScrlBox.Resize;
Begin
   If Assigned(FOnResize) Then FOnResize(Self);
End;
{$ENDIF}
//-------------------------------------------------------------

Procedure TScrlBox.WMSize(Var message: TWMSize);
Begin
   Inherited;
   {.$ifdef DEL4_OR_HIGHER}
   If Not (csLoading In ComponentState) Then Resize;
   {.$endif}
End;
//-------------------------------------------------------------

Procedure TScrlBox.WMNCHitTest(Var message: TMessage);
Begin
   DefaultHandler(message);
End;
//-------------------------------------------------------------

Procedure TScrlBox.CMCtl3DChanged(Var message: TMessage);
Begin
   If NewStyleControls And (FBorderStyle = bsSingle) Then RecreateWnd;
   Inherited;
End;
//-------------------------------------------------------------

(*********************** TScrlBar ****************************)

//-------------------------------------------------------------

Constructor TScrlBar.Create(AControl: TScrlBox; AKind: TScrollBarKind);
Begin
   Inherited Create;

   FScrlBox := AControl;
   FKind := AKind;
   FVisible := True;

   FSmallChange := 1;
   FLargeChange := 10;

   FMin := 0;
   FMax := 100;
   FPage := 1;
   FPosition := 0;
   FTrackPos := 0;
   FThumbValue := 1;

   FScrollInfo.cbSize := SizeOf(TScrollInfo);
   FScrollInfo.nMin := FMin;
   FScrollInfo.NMAX := FMax;
   FScrollInfo.nPage := FPage;
   FScrollInfo.nPos := FPosition;
   FScrollInfo.nTrackPos := FTrackPos;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetScrollData(value: Integer; Mask: Cardinal; MinMax: Boolean);
Begin
   FScrollInfo.fMask := Mask;

   If (Mask = SIF_RANGE) And (MinMax = True) Then
      FScrollInfo.nMin := value
   Else
      If (Mask = SIF_RANGE) And (MinMax = False) Then
         FScrollInfo.NMAX := value
      Else
         If Mask = SIF_PAGE Then
            FScrollInfo.nPage := value
         Else
            If Mask = SIF_POS Then
               FScrollInfo.nPos := value;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   If FScrlBox {FControl}.FVersion = True Then
      FlatSB_SetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, FScrollInfo, True)
   Else
{$ENDIF}
{$ENDIF}
      SetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, FScrollInfo, True);
End;
//-------------------------------------------------------------

Function TScrlBar.GetScrollData(Mask: Cardinal): Integer;
Begin
   FScrollInfo.fMask := Mask;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
   If FScrlBox {FControl}.FVersion = True Then
      FlatSB_GetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, FScrollInfo)
   Else
{$ENDIF}
{$ENDIF}
      GetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, FScrollInfo);

   Result := FScrollInfo.nTrackPos;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetMin(value: Integer);
Begin
   If FMin <> value Then
   Begin
      FMin := value;
      SetScrollData(value, SIF_RANGE, True);
   End;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetMax(value: Integer);
Begin
   If FMax <> value Then
   Begin
      FMax := value;
      SetScrollData(value, SIF_RANGE, False);
   End;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetPage(value: Integer);
Begin
   If FPage <> value Then
   Begin
      FPage := value;
      SetScrollData(value, SIF_PAGE, True);
   End;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetPosition(value: Integer);
Begin
   If FPosition <> value Then
   Begin
      FPosition := value;
      SetScrollData(value, SIF_POS, True);
   End;
End;
//-------------------------------------------------------------

Function TScrlBar.GetTrackPos: Integer;
Begin
   FTrackPos := GetScrollData(SIF_ALL);
   Result := FTrackPos;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetSmallChange(value: Integer);
Begin
   If value <> FSmallChange Then
      FSmallChange := value;
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetLargeChange(value: Integer);
Begin
   If value <> FLargeChange Then
   Begin
      FLargeChange := value;

      (* Page value of scrollbar is the same as LargeChage
         update the page value of scrollbar here		*)
      SetPage(value);
   End;
End;
//-------------------------------------------------------------

Procedure TScrlBar.Assign(source: TPersistent);
Begin
   If source Is TScrlBar Then
   Begin
      Visible := TScrlBar(source).Visible;
      Min := TScrlBar(source).Min;
      Max := TScrlBar(source).Max;
      SmallChange := TScrlBar(source).SmallChange;
      LargeChange := TScrlBar(source).LargeChange;
      Position := TScrlBar(source).Position;
      ThumbValue := TScrlBar(source).ThumbValue;
      Exit;
   End;
   Inherited Assign(source);
End;
//-------------------------------------------------------------

Procedure TScrlBar.ScrollMessage(Var msg: TWMScroll);
Var
   value: Integer;
   NewTopIndex: Integer;
Begin
   value := 0;
   With msg Do
   Begin
      Case ScrollCode Of
         SB_LINELEFT: value := FPosition - FSmallChange;
         SB_LINERIGHT: value := FPosition + FSmallChange;
         SB_PAGELEFT: value := FPosition - FLargeChange;
         SB_PAGERIGHT: value := FPosition + FLargeChange;
         SB_THUMBPOSITION: value := TrackPos - TrackPos Mod ThumbValue;
         SB_THUMBTRACK:
            Begin
               If FKind = sbVertical Then
                  value := TrackPos - TrackPos Mod ThumbValue
               Else
                  Exit;
            End;
         SB_TOP: value := FMin;
         SB_BOTTOM: value := FMax;
         SB_ENDSCROLL:
            Begin
               Exit;
            End;
      End;
   End;

   If value <= FMin Then value := FMin;
   If value >= FMax Then value := FMax;

   SetPosition(value);

   With FControl Do
      If FKind = sbHorizontal Then
      Begin
         FHeader.left := -value;
         FListBox.left := -value;
         FHeader.Repaint();
      End
      Else
      Begin
         //FControl.FListBox.Top := -Value + FControl.FHeader.Height;
         NewTopIndex := value Div FListBox.ItemHeight;
         If NewTopIndex <> FOldTopIndex Then
         Begin
            //FListBox.SetTopIndex( NewTopIndex );
              //FListBox.SetBounds( 0, FHeader.Height, Width, Height - FHeader.Height );
            SendMessage(FListBox.Handle, LB_SETTOPINDEX, NewTopIndex, 0);
            FOldTopIndex := NewTopIndex;
         End;
      End;

   If Assigned(FControl.fOnChange) Then FControl.fOnChange(Self, FKind);
End;
//-------------------------------------------------------------

Procedure TScrlBar.SetVisible(value: Boolean);
Var
   ScrollInfo: TScrollInfo;
Begin
   If FVisible <> value Then
   Begin
      FVisible := value;

      ScrollInfo.fMask := SIF_ALL;
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.nMin := 0;
      If FVisible = True Then
         ScrollInfo.NMAX := FMax
      Else
         ScrollInfo.NMAX := 0;

      ScrollInfo.nPage := FPage;
      ScrollInfo.nPos := FPosition;
      ScrollInfo.nTrackPos := FTrackPos;

{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}
      If FScrlBox {FControl}.FVersion = True Then
         FlatSB_SetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, ScrollInfo, True)
      Else
{$ENDIF}
{$ENDIF}
         SetScrollInfo(FScrlBox {FControl}.Handle, GetBarKind, ScrollInfo, True);
   End;
End;
//-------------------------------------------------------------

Function TScrlBar.GetBarKind: word;
Begin
   Result := SB_HORZ;
   If Kind = sbVertical Then
      Result := SB_VERT;
End;
//-------------------------------------------------------------
{$IFDEF DEL4_OR_HIGHER}
{$IFDEF USE_FLAT_SB}

Function GetComCtlVersion(Var Minor: Integer; Var Major: Integer): Boolean;
Var
   hInst: THandle;
   dvInfo: TDllVersionInfo;
   VerResult: Integer;
Begin
   hInst := LoadLibrary('comctl32.dll');

   //if (hInst >= 0) and (hInst < 32) then
   If hInst <> 0 Then
      Result := False
   Else
   Begin
      @_GetDllVersionInfo := GetProcAddress(hInst, 'DllGetVersion');

      //only the new version of comctl32.dll has this function
      If @_GetDllVersionInfo <> Nil Then
      Begin
         Result := True;
         dvInfo.size := SizeOf(TDllVersionInfo);

         VerResult := _GetDllVersionInfo(dvInfo);

         If VerResult = 0 Then
         Begin
            Minor := dvInfo.MinorVersion;
            Major := dvInfo.MajorVersion;
         End
         Else
            Result := False;
      End
      Else
      Begin
         Result := False;
      End;
   End;
   FreeLibrary(hInst);
End;
{$ENDIF}
{$ENDIF}
//-------------------------------------------------------------

End.
