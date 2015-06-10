{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
{           Copyright (c) 1995-2007 CodeGear            }
{                                                       }
{*******************************************************}

unit Forms;

{$P+,S-,W-,R-,T-,H+,X+}
{$C PRELOAD}
{$WARN SYMBOL_PLATFORM OFF}

interface

{$IFDEF LINUX}
uses WinUtils, Messages, Libc, Windows, SysUtils, Classes, Graphics, Menus,
  Controls, Imm, ActnList, MultiMon, HelpIntfs;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses Messages, Windows, SysUtils, Classes, Graphics, Menus, Controls, Imm,
  ActnList, MultiMon, HelpIntfs;
{$ENDIF}

type

{ Forward declarations }

  TScrollingWinControl = class;
  TCustomForm = class;
  TForm = class;
  TMonitor = class;

{ TControlScrollBar }

  TScrollBarKind = (sbHorizontal, sbVertical);
  TScrollBarInc = 1..32767;
  TScrollBarStyle = (ssRegular, ssFlat, ssHotTrack);

  TControlScrollBar = class(TPersistent)
  private
    FControl: TScrollingWinControl;
    FIncrement: TScrollBarInc;
    FPageIncrement: TScrollbarInc;
    FPosition: Integer;
    FRange: Integer;
    FCalcRange: Integer;
    FKind: TScrollBarKind;
    FMargin: Word;
    FVisible: Boolean;
    FTracking: Boolean;
    FScaled: Boolean;
    FSmooth: Boolean;
    FDelay: Integer;
    FButtonSize: Integer;
    FColor: TColor;
    FParentColor: Boolean;
    FSize: Integer;
    FStyle: TScrollBarStyle;
    FThumbSize: Integer;
    FPageDiv: Integer;
    FLineDiv: Integer;
    FUpdateNeeded: Boolean;
    constructor Create(AControl: TScrollingWinControl; AKind: TScrollBarKind);
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    procedure DoSetRange(Value: Integer);
    function GetScrollPos: Integer;
    function NeedsScrollBarVisible: Boolean;
    function IsIncrementStored: Boolean;
    procedure ScrollMessage(var Msg: TWMScroll);
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetParentColor(Value: Boolean);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TScrollBarStyle);
    procedure SetThumbSize(Value: Integer);
    procedure SetVisible(Value: Boolean);
    function IsRangeStored: Boolean;
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeBiDiPosition;
    property Kind: TScrollBarKind read FKind;
    function IsScrollBarVisible: Boolean;
    property ScrollPos: Integer read GetScrollPos;
  published
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
    property Color: TColor read FColor write SetColor default clBtnHighlight;
    property Increment: TScrollBarInc read FIncrement write FIncrement stored IsIncrementStored default 8;
    property Margin: Word read FMargin write FMargin default 0;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer read FRange write SetRange stored IsRangeStored default 0;
    property Smooth: Boolean read FSmooth write FSmooth default False;
    property Size: Integer read FSize write SetSize default 0;
    property Style: TScrollBarStyle read FStyle write SetStyle default ssRegular;
    property ThumbSize: Integer read FThumbSize write SetThumbSize default 0;
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

{ TScrollingWinControl }

  TWindowState = (wsNormal, wsMinimized, wsMaximized);

  TScrollingWinControl = class(TWinControl)
  private
    FHorzScrollBar: TControlScrollBar;
    FVertScrollBar: TControlScrollBar;
    FAutoScroll: Boolean;
    FAutoRangeCount: Integer;
    FUpdatingScrollBars: Boolean;
    procedure CalcAutoRange;
    procedure ScaleScrollBars(M, D: Integer);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
    procedure UpdateScrollBars;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function AutoScrollEnabled: Boolean; virtual;
    procedure AutoScrollInView(AControl: TControl); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoFlipChildren; override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default False;
    procedure Resizing(State: TWindowState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure ScrollInView(AControl: TControl);
  published
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property HorzScrollBar: TControlScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property VertScrollBar: TControlScrollBar read FVertScrollBar write SetVertScrollBar;
  end;

{ TScrollBox }

  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow,
    bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;

  TScrollBox = class(TScrollingWinControl)
  private
    FBorderStyle: TBorderStyle;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoScroll default True;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground default False;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TCustomFrame }

  TCustomFrame = class(TScrollingWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCustomFrameClass = class of TCustomFrame;

{ TFrame }

  TFrame = class(TCustomFrame)
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property Padding;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ IDesignerHook }

  IDesignerHook = interface(IDesignerNotify)
    ['{1E431DA5-2BEA-4DE7-A330-CC45FD2FB1EC}']
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    procedure PaintGrid;
    procedure PaintMenu;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string);
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  end;

{ IOleForm }

  IOleForm = interface
    ['{CD02E1C1-52DA-11D0-9EA6-0020AF3D82DA}']
    procedure OnDestroy;
    procedure OnResize;
  end;

{ TCustomForm }

  TPopupWnd = record
    ID: Integer;
    ControlWnd: HWND;
  end;
  TPopupWndArray = array of TPopupWnd;

  TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop);
  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;
  TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly,
    poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
  TDefaultMonitor = (dmDesktop, dmPrimary, dmMainForm, dmActiveForm);
  TPrintScale = (poNone, poProportional, poPrintToFit);
  TShowAction = (saIgnore, saRestore, saMinimize, saMaximize);
  TTileMode = (tbHorizontal, tbVertical);
  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender: TObject;
    var CanClose: Boolean) of object;
  TFormState = set of (fsCreating, fsVisible, fsShowing, fsModal,
    fsCreatedMDIChild, fsActivated);
  TShortCutEvent = procedure (var Msg: TWMKey; var Handled: Boolean) of object;
  THelpEvent = function(Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;
  TPopupMode = (pmNone, pmAuto, pmExplicit);

  TCustomForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState;
    FShowAction: TShowAction;
    FKeyPreview: Boolean;
    FActive: Boolean;
    FFormStyle: TFormStyle;
    FPosition: TPosition;
    FDefaultMonitor: TDefaultMonitor;
    FTileMode: TTileMode;
    FDropTarget: Boolean;
    FOldCreateOrder: Boolean;
    FPrintScale: TPrintScale;
    FCanvas: TControlCanvas;
    FHelpFile: string;
    FIcon: TIcon;
    FInCMParentBiDiModeChanged: Boolean;
    FMenu: TMainMenu;
    FModalResult: TModalResult;
    FDesigner: IDesignerHook;
    FClientHandle: HWND;
    FWindowMenu: TMenuItem;
    FPixelsPerInch: Integer;
    FObjectMenuItem: TMenuItem;
    FOleForm: IOleForm;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FTextHeight: Integer;
    FDefClientProc: TFarProc;
    FClientInstance: TFarProc;
    FActiveOleControl: TWinControl;
    FSavedBorderStyle: TFormBorderStyle;
    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnDeactivate: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShow: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FPopupChildren: TList;
    FPopupMode: TPopupMode;
    FPopupParent: TCustomForm;
    FRecreateChildren: TList;
    FPopupWnds: TPopupWndArray;
    FInternalPopupParent: TCustomForm;
    FInternalPopupParentWnd: HWND;
    FScreenSnap: Boolean;
    FSnapBuffer: Integer;
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;
    procedure RefreshMDIMenu;
    procedure ClientWndProc(var Message: TMessage);
    function GetActiveMDIChild: TForm;
    function GetCanvas: TCanvas;
    function GetIconHandle: HICON;
    function GetLeft: Integer;
    function GetMDIChildCount: Integer;
    function GetMDIChildren(I: Integer): TForm;
    function GetMonitor: TMonitor;
    function GetPixelsPerInch: Integer;
    function GetPopupChildren: TList;
    function GetRecreateChildren: TList;
    function GetScaled: Boolean;
    function GetTextHeight: Integer;
    function GetTop: Integer;
    procedure IconChanged(Sender: TObject);
    function IsAutoScrollStored: Boolean;
    function IsClientSizeStored: Boolean;
    function IsForm: Boolean;
    function IsFormSizeStored: Boolean;
    function IsIconStored: Boolean;
    procedure MergeMenu(MergeState: Boolean);
    procedure ReadIgnoreFontProperty(Reader: TReader);
    procedure ReadTextHeight(Reader: TReader);
    procedure SetActive(Value: Boolean);
    procedure SetActiveControl(Control: TWinControl);
    procedure SetActiveOleControl(Control: TWinControl);
    procedure SetBorderIcons(Value: TBorderIcons);
    procedure SetBorderStyle(Value: TFormBorderStyle);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientWidth(Value: Integer);
    procedure SetDesigner(ADesigner: IDesignerHook);
    procedure SetFormStyle(Value: TFormStyle);
    procedure SetIcon(Value: TIcon);
    procedure SetLeft(Value: Integer);
    procedure SetMenu(Value: TMainMenu);
    procedure SetPixelsPerInch(Value: Integer);
    procedure SetPosition(Value: TPosition);
    procedure SetPopupMode(Value: TPopupMode);
    procedure SetPopupParent(Value: TCustomForm);
    procedure SetScaled(Value: Boolean);
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWindowFocus;
    procedure SetWindowMenu(Value: TMenuItem);
    procedure SetObjectMenuItem(Value: TMenuItem);
    procedure SetWindowState(Value: TWindowState);
    procedure SetWindowToMonitor;
    procedure WritePixelsPerInch(Writer: TWriter);
    procedure WriteTextHeight(Writer: TWriter);
    function NormalColor: TColor;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMIconEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ICONERASEBKGND;
    procedure WMQueryDragIcon(var Message: TWMQueryDragIcon); message WM_QUERYDRAGICON;
    procedure WMNCCreate(var Message: TWMNCCreate); message WM_NCCREATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMInitMenuPopup(var Message: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure WMMenuChar(var Message: TWMMenuChar); message WM_MENUCHAR;
    procedure WMMenuSelect(var Message: TWMMenuSelect); message WM_MENUSELECT;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMMDIActivate(var Message: TWMMDIActivate); message WM_MDIACTIVATE;
    procedure WMNextDlgCtl(var Message: TWMNextDlgCtl); message WM_NEXTDLGCTL;
    procedure WMEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure CMActionExecute(var Message: TMessage); message CM_ACTIONEXECUTE;
    procedure CMActionUpdate(var Message: TMessage); message CM_ACTIONUPDATE;
    procedure CMActivate(var Message: TCMActivate); message CM_ACTIVATE;
    procedure CMAppSysCommand(var Message: TMessage); message CM_APPSYSCOMMAND;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMDeactivate(var Message: TCMDeactivate); message CM_DEACTIVATE;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMIconChanged(var Message: TMessage); message CM_ICONCHANGED;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMUIActivate(var Message); message CM_UIACTIVATE;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMPopupHwndDestroy(var Message: TCMPopupHWndDestroy); message CM_POPUPHWNDDESTROY;
    procedure CMIsShortCut(var Message: TWMKey); message CM_ISSHORTCUT;
    procedure SetLayeredAttribs;
    procedure SetAlphaBlend(const Value: Boolean);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetTransparentColor(const Value: Boolean);
    procedure SetTransparentColorValue(const Value: TColor);
    procedure InitAlphaBlending(var Params: TCreateParams);
  protected
    FFormState: TFormState;
    procedure Activate; dynamic;
    procedure ActiveChanged; dynamic;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure BeginAutoDrag; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure CloseModal;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Deactivate; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyHandle; override;
    procedure DestroyWindowHandle; override;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure GetBorderIconStyles(var Style, ExStyle: Cardinal); dynamic;
    procedure GetBorderStyles(var Style, ExStyle, ClassStyle: Cardinal); dynamic;
    function GetClientRect: TRect; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetFloating: Boolean; override;
    function GetOwnerWindow: HWND; dynamic;
    function HandleCreateException: Boolean; dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; dynamic;
    procedure PaintWindow(DC: HDC); override;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure ReadState(Reader: TReader); override;
    procedure RequestAlign; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetParentBiDiMode(Value: Boolean); override;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateActions; virtual;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
    procedure VisibleChanging; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Resizing(State: TWindowState); override;
    property ActiveMDIChild: TForm read GetActiveMDIChild;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons stored IsForm
      default [biSystemMenu, biMinimize, biMaximize];
    property AutoScroll stored IsAutoScrollStored;
    property ClientHandle: HWND read FClientHandle;
    property ClientHeight write SetClientHeight stored IsClientSizeStored;
    property ClientWidth write SetClientWidth stored IsClientSizeStored;
    property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
    property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;
    property Ctl3D default True;
    property DefaultMonitor: TDefaultMonitor read FDefaultMonitor write FDefaultMonitor
      stored IsForm default dmActiveForm;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle
      stored IsForm default fsNormal;
    property Height stored IsFormSizeStored;
    property HorzScrollBar stored IsForm;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property MDIChildCount: Integer read GetMDIChildCount;
    property MDIChildren[I: Integer]: TForm read GetMDIChildren;
    property OldCreateOrder: Boolean read FOldCreateOrder write FOldCreateOrder;
    property ObjectMenuItem: TMenuItem read FObjectMenuItem write SetObjectMenuItem
      stored IsForm;
    property PixelsPerInch: Integer read GetPixelsPerInch write SetPixelsPerInch
      stored False;
    property ParentFont default False;
    property PopupMenu stored IsForm;
    property PopupChildren: TList read GetPopupChildren;
    property Position: TPosition read FPosition write SetPosition stored IsForm
      default poDefaultPosOnly;
    property PrintScale: TPrintScale read FPrintScale write FPrintScale stored IsForm
      default poProportional;
    property Scaled: Boolean read GetScaled write SetScaled stored IsForm default True;
    property TileMode: TTileMode read FTileMode write FTileMode default tbHorizontal;
    property VertScrollBar stored IsForm;
    property Visible write SetVisible default False;
    property Width stored IsFormSizeStored;
    property WindowMenu: TMenuItem read FWindowMenu write SetWindowMenu stored IsForm;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate stored IsForm;
    property OnCanResize stored IsForm;
    property OnClick stored IsForm;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery
      stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate stored IsForm;
    property OnDblClick stored IsForm;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy stored IsForm;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate stored IsForm;
    property OnDragDrop stored IsForm;
    property OnDragOver stored IsForm;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide stored IsForm;
    property OnKeyDown stored IsForm;
    property OnKeyPress stored IsForm;
    property OnKeyUp stored IsForm;
    property OnMouseActivate stored IsForm;
    property OnMouseDown stored IsForm;
    property OnMouseMove stored IsForm;
    property OnMouseUp stored IsForm;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint stored IsForm;
    property OnResize stored IsForm;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow stored IsForm;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Close;
    function CloseQuery: Boolean; virtual;
    procedure DefaultHandler(var Message); override;
    procedure DefocusControl(Control: TWinControl; Removing: Boolean);
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); override;
    procedure FocusControl(Control: TWinControl);
    function GetFormImage: TBitmap;
    procedure Hide;
    function IsShortCut(var Message: TWMKey): Boolean; dynamic;
    procedure MakeFullyVisible(AMonitor: TMonitor = nil);
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure Print;
    procedure RecreateAsPopup(AWindowHandle: HWND);
    procedure Release;
    procedure SendCancelMode(Sender: TControl);
    procedure SetFocus; override;
    function SetFocusedControl(Control: TWinControl): Boolean; virtual;
    procedure Show;
    function ShowModal: Integer; virtual;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; virtual;
    property Active: Boolean read FActive;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl
      stored IsForm;
    property Action;
    property ActiveOleControl: TWinControl read FActiveOleControl write SetActiveOleControl;
    property BorderStyle: TFormBorderStyle read FBorderStyle write SetBorderStyle
      stored IsForm default bsSizeable;
    property Canvas: TCanvas read GetCanvas;
    property Caption stored IsForm;
    property Color nodefault;
    property Designer: IDesignerHook read FDesigner write SetDesigner;
    property DropTarget: Boolean read FDropTarget write FDropTarget;
    property Font;
    property FormState: TFormState read FFormState;
    property HelpFile: string read FHelpFile write FHelpFile;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview
      stored IsForm default False;
    property Menu: TMainMenu read FMenu write SetMenu stored IsForm;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property Monitor: TMonitor read GetMonitor;
    property OleFormObject: IOleForm read FOleForm write FOleForm;
    property PopupMode: TPopupMode read FPopupMode write SetPopupMode default pmNone;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
    property ScreenSnap: Boolean read FScreenSnap write FScreenSnap default False;
    property SnapBuffer: Integer read FSnapBuffer write FSnapBuffer;
    property WindowState: TWindowState read FWindowState write SetWindowState
      stored IsForm default wsNormal;
  published
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
  end;

  TCustomFormClass = class of TCustomForm;

  { TCustomActiveForm }

  TActiveFormBorderStyle = (afbNone, afbSingle, afbSunken, afbRaised);

  TCustomActiveForm = class(TCustomForm)
  private
    FAxBorderStyle: TActiveFormBorderStyle;
    procedure SetAxBorderStyle(Value: TActiveFormBorderStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; override;
    property Visible;
  published
    property ActiveControl;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property AxBorderStyle: TActiveFormBorderStyle read FAxBorderStyle
      write SetAxBorderStyle default afbSingle;
    property BorderWidth;
    property Caption stored True;
    property Color;
    property Constraints;
    property Font;
    property Height stored True;
    property HorzScrollBar;
    property KeyPreview;
    property Padding;
    property OldCreateOrder;
    property PixelsPerInch;
    property PopupMenu;
    property PrintScale;
    property Scaled;
    property ShowHint;
    property VertScrollBar;
    property Width stored True;
    property OnActivate;
    property OnClick;
    property OnCreate;
    property OnContextPopup;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDragDrop;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
  end;

{ TForm }

  TForm = class(TCustomForm)
  public
    procedure ArrangeIcons;
    procedure Cascade;
    procedure Next;
    procedure Previous;
    procedure Tile;
    property ActiveMDIChild;
    property ClientHandle;
    property DockManager;
    property MDIChildCount;
    property MDIChildren;
    property TileMode;
  published
    property Action;
    property ActiveControl;
    property Align;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property TransparentColor default False;
    property TransparentColorValue default 0;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    property FormStyle;
    property Height;
    property HelpFile;
    property HorzScrollBar;
    property Icon;
    property KeyPreview;
    property Padding;
    property Menu;
    property OldCreateOrder;
    property ObjectMenuItem;
    property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property PrintScale;
    property Scaled;
    property ScreenSnap default False;
    property ShowHint;
    property SnapBuffer default 10;
    property VertScrollBar;
    property Visible;
    property Width;
    property WindowState;
    property WindowMenu;
    property OnActivate;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
  end;

  TFormClass = class of TForm;

{ TCustomDockForm }

  TCustomDockForm = class(TCustomForm)
  private
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AutoScroll default False;
    property BorderStyle default bsSizeToolWin;
    property FormStyle default fsStayOnTop;
  published
    property PixelsPerInch;
  end;

{ TMonitor }

  TMonitor = class(TObject)
  private
    FHandle: HMONITOR;
    FMonitorNum: Integer;
    function GetLeft: Integer;
    function GetHeight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    function GetBoundsRect: TRect;
    function GetWorkareaRect: TRect;
    function GetPrimary: Boolean;
  public
    property Handle: HMONITOR read FHandle;
    property MonitorNum: Integer read FMonitorNum;
    property Left: Integer read GetLeft;
    property Height: Integer read GetHeight;
    property Top: Integer read GetTop;
    property Width: Integer read GetWidth;
    property BoundsRect: TRect read GetBoundsRect;
    property WorkareaRect: TRect read GetWorkareaRect;
    property Primary: Boolean read GetPrimary;
  end;

{ TScreen }

  PCursorRec = ^TCursorRec;
  TCursorRec = record
    Next: PCursorRec;
    Index: Integer;
    Handle: HCURSOR;
  end;

  TMonitorDefaultTo = (mdNearest, mdNull, mdPrimary);

  TScreen = class(TComponent)
  private
    FFonts: TStrings;
    FImes: TStrings;
    FDefaultIme: string;
    FDefaultKbLayout: HKL;
    FPixelsPerInch: Integer;
    FCursor: TCursor;
    FCursorCount: Integer;
    FForms: TList;
    FCustomForms: TList;
    FDataModules: TList;
    FMonitors: TList;
    FCursorList: PCursorRec;
    FDefaultCursor: HCURSOR;
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FLastActiveControl: TWinControl;
    FLastActiveCustomForm: TCustomForm;
    FFocusedForm: TCustomForm;
    FSaveFocusedList: TList;
    FHintFont: TFont;
    FIconFont: TFont;
    FMenuFont: TFont;
    FAlignLevel: Word;
    FControlState: TControlState;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure AlignForm(AForm: TCustomForm);
    procedure AlignForms(AForm: TCustomForm; var Rect: TRect);
    procedure AddDataModule(DataModule: TDataModule);
    procedure AddForm(AForm: TCustomForm);
    procedure ClearMonitors;
    procedure CreateCursors;
    procedure DeleteCursor(Index: Integer);
    procedure DestroyCursors;
    function FindMonitor(Handle: HMONITOR): TMonitor;
    procedure IconFontChanged(Sender: TObject);
    function GetCustomFormCount: Integer;
    function GetCustomForms(Index: Integer): TCustomForm;
    function GetCursors(Index: Integer): HCURSOR;
    function GetDataModule(Index: Integer): TDataModule;
    function GetDataModuleCount: Integer;
    function GetDefaultIME: String;
    function GetDesktopTop: Integer;
    function GetDesktopLeft: Integer;
    function GetDesktopHeight: Integer;
    function GetDesktopWidth: Integer;
    function GetDesktopRect: TRect;
    function GetWorkAreaRect: TRect;
    function GetWorkAreaHeight: Integer;
    function GetWorkAreaLeft: Integer;
    function GetWorkAreaTop: Integer;
    function GetWorkAreaWidth: Integer;
    function GetImes: TStrings;
    function GetHeight: Integer;
    function GetMonitor(Index: Integer): TMonitor;
    function GetMonitorCount: Integer;
    procedure GetMonitors;
    function GetFonts: TStrings;
    function GetForm(Index: Integer): TForm;
    function GetFormCount: Integer;
    procedure GetMetricSettings;
    function GetWidth: Integer;
    procedure InsertCursor(Index: Integer; Handle: HCURSOR);
    procedure RemoveDataModule(DataModule: TDataModule);
    procedure RemoveForm(AForm: TCustomForm);
    procedure SetCursors(Index: Integer; Handle: HCURSOR);
    procedure SetCursor(Value: TCursor);
    procedure SetHintFont(Value: TFont);
    procedure SetIconFont(Value: TFont);
    procedure SetMenuFont(Value: TFont);
    procedure UpdateLastActive;
    function GetPrimaryMonitor: TMonitor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableAlign;
    procedure EnableAlign;
    function MonitorFromPoint(const Point: TPoint;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromRect(const Rect: TRect;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromWindow(const Handle: THandle;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    procedure Realign;
    procedure ResetFonts;
    property ActiveControl: TWinControl read FActiveControl;
    property ActiveCustomForm: TCustomForm read FActiveCustomForm;
    property ActiveForm: TForm read FActiveForm;
    property CustomFormCount: Integer read GetCustomFormCount;
    property CustomForms[Index: Integer]: TCustomForm read GetCustomForms;
    property CursorCount: Integer read FCursorCount;
    property Cursor: TCursor read FCursor write SetCursor;
    property Cursors[Index: Integer]: HCURSOR read GetCursors write SetCursors;
    property DataModules[Index: Integer]: TDataModule read GetDataModule;
    property DataModuleCount: Integer read GetDataModuleCount;
    property FocusedForm: TCustomForm read FFocusedForm write FFocusedForm;
    property SaveFocusedList: TList read FSaveFocusedList;
    property MonitorCount: Integer read GetMonitorCount;
    property Monitors[Index: Integer]: TMonitor read GetMonitor;
    property DesktopRect: TRect read GetDesktopRect;
    property DesktopHeight: Integer read GetDesktopHeight;
    property DesktopLeft: Integer read GetDesktopLeft;
    property DesktopTop: Integer read GetDesktopTop;
    property DesktopWidth: Integer read GetDesktopWidth;
    property WorkAreaRect: TRect read GetWorkAreaRect;
    property WorkAreaHeight: Integer read GetWorkAreaHeight;
    property WorkAreaLeft: Integer read GetWorkAreaLeft;
    property WorkAreaTop: Integer read GetWorkAreaTop;
    property WorkAreaWidth: Integer read GetWorkAreaWidth;
    property HintFont: TFont read FHintFont write SetHintFont;
    property IconFont: TFont read FIconFont write SetIconFont;
    property MenuFont: TFont read FMenuFont write SetMenuFont;
    property Fonts: TStrings read GetFonts;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForm;
    property Imes: TStrings read GetImes;
    property DefaultIme: string read GetDefaultIme;
    property DefaultKbLayout: HKL read FDefaultKbLayout;
    property Height: Integer read GetHeight;
    property PixelsPerInch: Integer read FPixelsPerInch;
    property PrimaryMonitor: TMonitor read GetPrimaryMonitor;
    property Width: Integer read GetWidth;
    property OnActiveControlChange: TNotifyEvent
      read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent
      read FOnActiveFormChange write FOnActiveFormChange;
  end;

{ TApplication }

  TTimerMode = (tmShow, tmHide);

  PHintInfo = ^THintInfo;
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: THintWindowClass;
    HintPos: TPoint;
    HintMaxWidth: Integer;
    HintColor: TColor;
    CursorRect: TRect;
    CursorPos: TPoint;
    ReshowTimeout: Integer;
    HideTimeout: Integer;
    HintStr: string;
    HintData: Pointer;
  end;

  TCMHintShow = record
    Msg: Cardinal;
    Reserved: Integer;
    HintInfo: PHintInfo;
    Result: Integer;
  end;

  TCMHintShowPause = record
    Msg: Cardinal;
    WasActive: Integer;
    Pause: PInteger;
    Result: Integer;
  end;

  TPopupForm = record
    FormID: Integer;
    Form: TCustomForm;
    WasPopup: Boolean;
  end;
  TPopupFormArray = array of TPopupForm;

  TMessageEvent = procedure (var Msg: TMsg; var Handled: Boolean) of object;
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TGetHandleEvent = procedure(var Handle: HWND) of object;
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;
  TShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean;
    var HintInfo: THintInfo) of object;
  TWindowHook = function (var Message: TMessage): Boolean of object;
  TSettingChangeEvent = procedure (Sender: TObject; Flag: Integer; const Section: string; var Result: Longint) of object;

  TApplication = class(TComponent)
  private
    FHandle: HWnd;
    FBiDiMode: TBiDiMode;
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string;
    FObjectInstance: Pointer;
    FMainForm: TForm;
    FMouseControl: TControl;
    FHelpSystem : IHelpSystem;
    FHelpFile: string;
    FHint: string;
    FHintActive: Boolean;
    FUpdateFormatSettings: Boolean;
    FUpdateMetricSettings: Boolean;
    FShowMainForm: Boolean;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintCursorRect: TRect;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FHintWindow: THintWindow;
    FShowHint: Boolean;
    FTimerMode: TTimerMode;
    FTimerHandle: Word;
    FTitle: string;
    FTopMostList: TList;
    FTopMostLevel: Integer;
    FPopupOwners: TList;
    FPopupLevel: Integer;
    FIcon: TIcon;
    FTerminate: Boolean;
    FActive: Boolean;
    FAllowTesting: Boolean;
    FTestLib: THandle;
    FHandleCreated: Boolean;
    FRunning: Boolean;
    FWindowHooks: TList;
    FWindowList: Pointer;
    FDialogHandle: HWnd;
    FAutoDragDocking: Boolean;
    FActionUpdateDelay: Integer;
    FModalLevel: Integer;
    FPopupControlWnd: HWnd;
    FCurrentPopupID: Integer;
    FPopupForms: TPopupFormArray;
    FModalPopupMode: TPopupMode;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnGetActiveFormHandle: TGetHandleEvent;
    FOnGetMainFormHandle: TGetHandleEvent;
    FOnMessage: TMessageEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShowHint: TShowHintEvent;
    FOnSettingChange: TSettingChangeEvent;
    function CheckIniChange(var Message: TMessage): Boolean;
    function DispatchAction(Msg: Longint; Action: TBasicAction): Boolean;
    procedure DoActionIdle;
    function DoMouseIdle: TControl;
    procedure DoNormalizeTopMosts(IncludeMain: Boolean);
    function DoOnHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
    procedure DoShowOwnedPopups(Show: Boolean);
    function GetCurrentHelpFile: string;
    function GetDialogHandle: HWND;
    function GetActiveFormHandle: HWND;
    function GetMainFormHandle: HWND;
    function GetExeName: string;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure HintTimerExpired;
    procedure IconChanged(Sender: TObject);
    function InvokeHelp(Command: Word; Data: Longint): Boolean;
    procedure NotifyForms(Msg: Word);
    procedure PopupControlProc(var Message: TMessage);
    function ProcessMessage(var Msg: TMsg): Boolean;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetDialogHandle(Value: HWnd);
    procedure SetHandle(Value: HWnd);
    procedure SetHint(const Value: string);
    procedure SetHintColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetShowHint(Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure SettingChange(var Message: TWMSettingChange);
    procedure StartHintTimer(Value: Integer; TimerMode: TTimerMode);
    procedure StopHintTimer;
    procedure WndProc(var Message: TMessage);
    procedure UpdateVisible;
    function  ValidateHelpSystem: Boolean;
    procedure WakeMainThread(Sender: TObject);
  protected
    procedure Idle(const Msg: TMsg);
    function IsDlgMsg(var Msg: TMsg): Boolean;
    function IsHintMsg(var Msg: TMsg): Boolean;
    function IsKeyMsg(var Msg: TMsg): Boolean;
    function IsMDIMsg(var Msg: TMsg): Boolean;
    function IsShortCut(var Message: TWMKey): Boolean;
    function IsPreProcessMessage(var Msg: TMsg): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(CursorPos: TPoint);
    function AddPopupForm(APopupForm: TCustomForm): Integer;
    procedure BringToFront;
    procedure ControlDestroyed(Control: TControl);
    procedure CancelHint;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure CreateHandle;
    procedure DoApplicationIdle;
    function ExecuteAction(Action: TBasicAction): Boolean; reintroduce;
    procedure HandleException(Sender: TObject);
    procedure HandleMessage;
    function HelpCommand(Command: Integer; Data: Longint): Boolean;
    function HelpContext(Context: THelpContext): Boolean;
    function HelpJump(const JumpID: string): Boolean;
    function HelpKeyword(const Keyword: string): Boolean;
    function HelpShowTableOfContents: Boolean;
    procedure HideHint;
    procedure HintMouseMessage(Control: TControl; var Message: TMessage);
    procedure HookMainWindow(Hook: TWindowHook);
    procedure HookSynchronizeWakeup;
    procedure Initialize;
    function IsRightToLeft: Boolean;
    function MessageBox(const Text, Caption: PChar; Flags: Longint = MB_OK): Integer;
    procedure Minimize;
    procedure ModalStarted;
    procedure ModalFinished;
    procedure NormalizeAllTopMosts;
    procedure NormalizeTopMosts;
    procedure ProcessMessages;
    procedure RemovePopupForm(APopupForm: TCustomForm);
    procedure Restore;
    procedure RestoreTopMosts;
    procedure Run;
    procedure ShowException(E: Exception);
    procedure Terminate;
    procedure UnhookMainWindow(Hook: TWindowHook);
    procedure UnhookSynchronizeWakeup;
    function UpdateAction(Action: TBasicAction): Boolean; reintroduce;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;
    property Active: Boolean read FActive;
    property ActionUpdateDelay: Integer read FActionUpdateDelay write FActionUpdateDelay default 0;
    property ActiveFormHandle: HWND read GetActiveFormHandle;
    property AllowTesting: Boolean read FAllowTesting write FAllowTesting;
    property AutoDragDocking: Boolean read FAutoDragDocking write FAutoDragDocking default True;
    property HelpSystem: IHelpSystem read FHelpSystem;
    property CurrentHelpFile: string read GetCurrentHelpFile;
    property DialogHandle: HWnd read GetDialogHandle write SetDialogHandle;
    property ExeName: string read GetExeName;
    property Handle: HWnd read FHandle write SetHandle;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor;
    property HintHidePause: Integer read FHintHidePause write FHintHidePause;
    property HintPause: Integer read FHintPause write FHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write FHintShortCuts;
    property HintShortPause: Integer read FHintShortPause write FHintShortPause;
    property Icon: TIcon read FIcon write SetIcon;
    property MainForm: TForm read FMainForm;
    property MainFormHandle: HWND read GetMainFormHandle;
    property ModalLevel: Integer read FModalLevel;
    property ModalPopupMode: TPopupMode read FModalPopupMode write FModalPopupMode default pmNone;
    property BiDiMode: TBiDiMode read FBiDiMode
      write SetBiDiMode default bdLeftToRight;
    property BiDiKeyboard: string read FBiDiKeyboard write FBiDiKeyboard;
    property NonBiDiKeyboard: string read FNonBiDiKeyboard write FNonBiDiKeyboard;
    property PopupControlWnd: HWND read FPopupControlWnd;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property ShowMainForm: Boolean read FShowMainForm write FShowMainForm;
    property Terminated: Boolean read FTerminate;
    property Title: string read GetTitle write SetTitle;
    property UpdateFormatSettings: Boolean read FUpdateFormatSettings
      write FUpdateFormatSettings;
    property UpdateMetricSettings: Boolean read FUpdateMetricSettings
      write FUpdateMetricSettings;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnGetActiveFormHandle: TGetHandleEvent read FOnGetActiveFormHandle
      write FOnGetActiveFormHandle;
    property OnGetMainFormHandle: TGetHandleEvent read FOnGetMainFormHandle
      write FOnGetMainFormHandle;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnSettingChange: TSettingChangeEvent read FOnSettingChange write FOnSettingChange;
  end;

{ Global objects }

var
  Application: TApplication;
  Screen: TScreen;
  Ctl3DBtnWndProc: Pointer = nil;  { obsolete }
  Ctl3DDlgFramePaint: function(Window: HWnd; Msg, wParam, lParam: Longint): Longint stdcall = nil; { obsolete }
  Ctl3DCtlColorEx: function(Window: HWnd; Msg, wParam, lParam: Longint): Longint stdcall = nil; { obsolete }
  HintWindowClass: THintWindowClass = THintWindow;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
function ValidParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;

function DisableTaskWindows(ActiveWindow: HWnd): Pointer;
procedure EnableTaskWindows(WindowList: Pointer);

function MakeObjectInstance(Method: TWndMethod): Pointer; deprecated; { moved to Classes.pas }
{$EXTERNALSYM MakeObjectInstance}
procedure FreeObjectInstance(ObjectInstance: Pointer);    deprecated; { moved to Classes.pas }
{$EXTERNALSYM FreeObjectInstance}

function IsAccel(VK: Word; const Str: string): Boolean;

function  Subclass3DWnd(Wnd: HWnd): Boolean;     deprecated;  { obsolete }
procedure Subclass3DDlg(Wnd: HWnd; Flags: Word); deprecated;  { obsolete }
procedure SetAutoSubClass(Enable: Boolean);      deprecated;  { obsolete }
function AllocateHWnd(Method: TWndMethod): HWND; deprecated;  { moved to Classes.pas }
{$EXTERNALSYM AllocateHWnd}
procedure DeallocateHWnd(Wnd: HWND);             deprecated;  { moved to Classes.pas }
{$EXTERNALSYM DeallocateHWnd}
procedure DoneCtl3D;                             deprecated;  { obsolete }
procedure InitCtl3D;                             deprecated;  { obsolete }

function KeysToShiftState(Keys: Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;
function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState; overload;
function KeyboardStateToShiftState: TShiftState; overload;

function ForegroundTask: Boolean;

type
  TFocusState = type Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

type
  TGlassFrame = class(TPersistent)
  private
    FClient: TCustomForm;
    FEnabled: Boolean;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FOnChange: TNotifyEvent;
    FSheetOfGlass: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetExtendedFrame(Index: Integer; Value: Integer);
    procedure SetSheetOfGlass(Value: Boolean);
  protected
    procedure Change; virtual;
  public
    constructor Create(Client: TCustomForm);
    procedure Assign(Source: TPersistent); override;
    function FrameExtended: Boolean;
    function IntersectsControl(Control: TControl): Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Left: Integer index 0 read FLeft write SetExtendedFrame default 0;
    property Top: Integer index 1 read FTop write SetExtendedFrame default 0;
    property Right: Integer index 2 read FRight write SetExtendedFrame default 0;
    property Bottom: Integer index 3 read FBottom write SetExtendedFrame default 0;
    property SheetOfGlass: Boolean read FSheetOfGlass write SetSheetOfGlass default False;
  end;

  TCustomFormHelper = class helper for TCustomForm
  private
    function GetGlassFrame: TGlassFrame;
    procedure ReadGlassFrameBottom(Reader: TReader);
    procedure ReadGlassFrameEnabled(Reader: TReader);
    procedure ReadGlassFrameLeft(Reader: TReader);
    procedure ReadGlassFrameRight(Reader: TReader);
    procedure ReadGlassFrameSheetOfGlass(Reader: TReader);
    procedure ReadGlassFrameTop(Reader: TReader);
    procedure SetGlassFrame(const Value: TGlassFrame);
    procedure WriteGlassFrameBottom(Writer: TWriter);
    procedure WriteGlassFrameEnabled(Writer: TWriter);
    procedure WriteGlassFrameLeft(Writer: TWriter);
    procedure WriteGlassFrameRight(Writer: TWriter);
    procedure WriteGlassFrameSheetOfGlass(Writer: TWriter);
    procedure WriteGlassFrameTop(Writer: TWriter);
  public
    procedure UpdateGlassFrame(Sender: TObject);
    property GlassFrame: TGlassFrame read GetGlassFrame write SetGlassFrame;
  end;

  TApplicationHelper = class helper for TApplication
  private
    procedure SetEnumAllWindowsOnActivateHint(Flag: Boolean);
    function GetEnumAllWindowsOnActivateHint: Boolean;
    procedure SetMainFormOnTaskBar(const Value: Boolean);
    function GetMainFormOnTaskBar: Boolean;
    procedure InternalRestore;
  public
    property EnumAllWindowsOnActivateHint: Boolean read GetEnumAllWindowsOnActivateHint write SetEnumAllWindowsOnActivateHint;
    property MainFormOnTaskBar: Boolean read GetMainFormOnTaskBar write SetMainFormOnTaskBar;
  end;

// The following functions exist solely for the purpose of allowing C++Builder applications access
// to the new helper class properties introduced in Delphi 2007.  They are marked as deprecated because
// then will be removed in a future release and no one should be referencing them except indirectly through
// the C++Builder layer. 
procedure SetCustomFormGlassFrame(const CustomForm: TCustomForm; const GlassFrame: TGlassFrame); deprecated;
function GetCustomFormGlassFrame(const CustomForm: TCustomForm): TGlassFrame; deprecated;
procedure SetApplicationMainFormOnTaskBar(const Application: TApplication; Value: Boolean); deprecated;
function GetApplicationMainFormOnTaskBar(const Application: TApplication): Boolean; deprecated;

implementation

uses
  ActiveX, Math, Printers, Consts, RTLConsts, CommCtrl, FlatSB, StdActns,
  Themes, DwmApi, UxTheme;

var
  FocusMessages: Boolean = True;
  FocusCount: Integer = 0;

{ RM_TaskbarCreated is a registered window message that the OS sends to
  top-level windows of running UI interactive services when a new user logs
  onto the local workstation desktop.
  VCL listens for this message to send around color, font, and settings
  change notifications to controls in the app so that UI interactive
  service apps built with VCL will conform to the new user's UI preferences. }
var
  RM_TaskbarCreated: DWORD;
  EnumAllWinOnActivateHintFlag: Boolean = False;
  AppIconic: Boolean;

// If the Application's window handle isn't being used for min/maximize
// as a result of the Application.MainFormOnTaskBar property use AppIconic instead.
function IsIconic(const hWnd: HWND): BOOL;
begin
  if hWnd = Application.Handle then
    Result := AppIconic
  else
    Result := Windows.IsIconic(hWnd);
end;

// The focus state is just the focus count for now. To save having to allocate
// anything, I just map the Integer to the TFocusState.
function SaveFocusState: TFocusState;
begin
  Result := TFocusState(FocusCount);
end;

procedure RestoreFocusState(FocusState: TFocusState);
begin
  FocusCount := Integer(FocusState);
end;

const
  DefHintColor = clInfoBk;  { default hint window color }
  DefHintPause = 500;       { default pause before hint window displays (ms) }
  DefHintShortPause = 0;    { default reshow pause to 0, was DefHintPause div 10 }
  DefHintHidePause = DefHintPause * 5;

procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
var
  Style: Longint;
begin
  if ClientHandle <> 0 then
  begin
    Style := GetWindowLong(ClientHandle, GWL_EXSTYLE);
    if ShowEdge then
      if Style and WS_EX_CLIENTEDGE = 0 then
        Style := Style or WS_EX_CLIENTEDGE
      else
        Exit
    else if Style and WS_EX_CLIENTEDGE <> 0 then
      Style := Style and not WS_EX_CLIENTEDGE
    else
      Exit;
    SetWindowLong(ClientHandle, GWL_EXSTYLE, Style);
    SetWindowPos(ClientHandle, 0, 0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

procedure ChangeAppWindow(const Handle: THandle; const SetAppWindow, RestoreVisibility: Boolean);
var
  Style: Integer;
  WasVisible, WasIconic: Boolean;
begin
  Style := GetWindowLong(Handle, GWL_EXSTYLE);
  if Handle = Application.FHandle then
    Style := Style or WS_EX_NOACTIVATE;
  WasIconic := Windows.IsIconic(Handle);
  WasVisible := IsWindowVisible(Handle) or ((Handle = Application.FHandle) and Application.ShowMainForm);
  if WasVisible or WasIconic then
    ShowWindow(Handle, SW_HIDE);
  if SetAppWindow then
    SetWindowLong(Handle, GWL_EXSTYLE, Style or WS_EX_APPWINDOW)
  else
    SetWindowLong(Handle, GWL_EXSTYLE, Style and not WS_EX_APPWINDOW);
  if (RestoreVisibility and WasVisible) or WasIconic then
  begin
    if WasIconic then
      ShowWindow(Handle, SW_MINIMIZE)
    else
      ShowWindow(Handle, SW_SHOW);
  end;
end;

{ Task window management }

type
  PTaskWindow = ^TTaskWindow;
  TTaskWindow = record
    Next: PTaskWindow;
    Window: HWnd;
  end;

var
  TaskActiveWindow: HWnd = 0;
  TaskFirstWindow: HWnd = 0;
  TaskFirstTopMost: HWnd = 0;
  DisablingWindows: Boolean = False;
  TaskWindowList: PTaskWindow = nil;

procedure DoneApplication;
begin
  with Application do
  begin
    if Handle <> 0 then DoShowOwnedPopups(False);
    ShowHint := False;
    Destroying;
    DestroyComponents;
  end;
end;

function DoDisableWindow(Window: HWnd; Data: Longint): Bool; stdcall;
var
  P: PTaskWindow;
begin
  if (Window <> TaskActiveWindow) and IsWindowVisible(Window) and
    IsWindowEnabled(Window) then
  begin
    New(P);
    P^.Next := TaskWindowList;
    P^.Window := Window;
    TaskWindowList := P;
    EnableWindow(Window, False);
  end;
  Result := True;
end;

function DisableTaskWindows(ActiveWindow: HWnd): Pointer;
var
  SaveActiveWindow: HWND;
  SaveWindowList: Pointer;

  procedure ProcessWMEnableMessages;
  var
    Msg: TMsg;
  begin
    while PeekMessage(Msg, 0, WM_ENABLE, WM_ENABLE, PM_REMOVE) do
      DispatchMessage(Msg);
  end;

begin
  { The following is to work-around an issue with WindowsXP that causes
    disabled windows to be re-enabled if the application doesn't process
    messages for a certain timeout period.  Windows posts a WM_ENABLE message
    that tells the window to re-enable, so unless we process that message,
    we don't know that it is about to be re-enabled so DoDisableWindow will
    ignore the window since it thinks it is still disabled.  So when the app
    begins to process messages again, the WM_ENABLE is allowed through and the
    window is then re-enabled causing dialogs to show behind other windows. }
//  ProcessWMEnableMessages;
  Result := nil;
  SaveActiveWindow := TaskActiveWindow;
  SaveWindowList := TaskWindowList;
  TaskActiveWindow := ActiveWindow;
  TaskWindowList := nil;
  try
    DisablingWindows := True;
    try
      EnumThreadWindows(GetCurrentThreadID, @DoDisableWindow, 0);
      Result := TaskWindowList;
    except
      EnableTaskWindows(TaskWindowList);
      raise;
    end;
  finally
    DisablingWindows := False;
    TaskWindowList := SaveWindowList;
    TaskActiveWindow := SaveActiveWindow;
  end;
end;

procedure EnableTaskWindows(WindowList: Pointer);
var
  P: PTaskWindow;
begin
  while WindowList <> nil do
  begin
    P := WindowList;
    if IsWindow(P^.Window) then EnableWindow(P^.Window, True);
    WindowList := P^.Next;
    Dispose(P);
  end;
end;

function DoFindWindow(Window: HWnd; Param: Longint): Bool; stdcall;
begin
  if (Window <> TaskActiveWindow) and (Window <> Application.FHandle) and
    IsWindowVisible(Window) and IsWindowEnabled(Window) then
    if GetWindowLong(Window, GWL_EXSTYLE) and WS_EX_TOPMOST = 0 then
    begin
      if TaskFirstWindow = 0 then TaskFirstWindow := Window;
    end else
    begin
      if TaskFirstTopMost = 0 then TaskFirstTopMost := Window;
    end;
  Result := True;
end;

function FindTopMostWindow(ActiveWindow: HWnd): HWnd;
begin
  TaskActiveWindow := ActiveWindow;
  TaskFirstWindow := 0;
  TaskFirstTopMost := 0;
  EnumThreadWindows(GetCurrentThreadID, @DoFindWindow, 0);
  if TaskFirstWindow <> 0 then
    Result := TaskFirstWindow else
    Result := TaskFirstTopMost;
end;

function SendFocusMessage(Window: HWnd; Msg: Word): Boolean;
var
  Count: Integer;
begin
  Count := FocusCount;
  SendMessage(Window, Msg, 0, 0);
  Result := FocusCount = Count;
end;

{ Check if this is the active Windows task }

type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: HWnd;
    Found: Boolean;
  end;


function CheckTaskWindow(Window: HWnd; Data: Longint): Bool; stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data)^.FocusWnd = Window then
  begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;

function CheckTaskWindowAll(Window: HWnd; Data: Longint): Bool; stdcall;
var
 WinProcessId: DWORD; 
begin
  Result := True;
  GetWindowThreadProcessId(Window, @WinProcessId); 
  if (PCheckTaskInfo(Data)^.FocusWnd = Window) and
     (GetCurrentProcessId=WinProcessId) then     
  begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;


function ForegroundTaskCheck(CheckAll: Boolean): Boolean; 
var
  Info: TCheckTaskInfo;
begin
  Info.FocusWnd := GetActiveWindow;
  Info.Found := False;
  if (CheckAll) then
    EnumWindows(@CheckTaskWindowAll, Longint(@Info))
  else
    EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
end;


function ForegroundTask: Boolean;
begin
 Result := ForegroundTaskCheck(False);
end;
 

function FindGlobalComponent(const Name: string): TComponent;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    Result := Screen.Forms[I];
    if not (csInline in Result.ComponentState) and
      (CompareText(Name, Result.Name) = 0) then Exit;
  end;
  for I := 0 to Screen.DataModuleCount - 1 do
  begin
    Result := Screen.DataModules[I];
    if CompareText(Name, Result.Name) = 0 then Exit;
  end;
  Result := nil;
end;

{ CTL3D32.DLL support for NT 3.51 has been removed.  Ctl3D properties of
  VCL controls use extended window style flags on Win95 and later OS's.  }

procedure InitCtl3D;
begin
end;

procedure DoneCtl3D;
begin
end;

function Subclass3DWnd(Wnd: HWnd): Boolean;
begin
  Result := False;
end;

procedure Subclass3DDlg(Wnd: HWnd; Flags: Word);
begin
end;

procedure SetAutoSubClass(Enable: Boolean);
begin
end;

{ Allocate an object instance }

function MakeObjectInstance(Method: TWndMethod): Pointer;
begin
{$IFDEF LINUX}
  Result := WinUtils.MakeObjectInstance(Method);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := Classes.MakeObjectInstance(Method);
{$ENDIF}
end;

{ Free an object instance }

procedure FreeObjectInstance(ObjectInstance: Pointer);
begin
{$IFDEF LINUX}
  WinUtils.FreeObjectInstance(ObjectInstance);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Classes.FreeObjectInstance(ObjectInstance);
{$ENDIF}
end;

function AllocateHWnd(Method: TWndMethod): HWND;
begin
{$IFDEF LINUX}
  Result := WinUtils.AllocateHWnd(Method);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := Classes.AllocateHWnd(Method);
{$ENDIF}
end;

procedure DeallocateHWnd(Wnd: HWND);
begin
{$IFDEF LINUX}
  WinUtils.DeallocateHWnd(Wnd);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Classes.DeallocateHWnd(Wnd);
{$ENDIF}
end;

{ Utility mapping functions }

{ Convert mouse message to TMouseButton }

function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

{ Convert keyboard message data to TShiftState }

function KeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
{$IFDEF LINUX}
  CtrlMask = $10000000;
  ShiftMask = $08000000;
{$ENDIF}
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and AltMask <> 0 then Include(Result, ssAlt);
{$IFDEF LINUX}
  if KeyData and CtrlMask <> 0 then Include(Result, ssCtrl);
  if KeyData and ShiftMask <> 0 then Include(Result, ssShift);
{$ENDIF}
end;

{ Convert GetKeyboardState output to TShiftState }

function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
end;

{ Calls GetKeyboardState and converts output to TShiftState }

function KeyboardStateToShiftState: TShiftState; overload;
var
  KeyState: TKeyBoardState;
begin
  GetKeyboardState(KeyState);
  Result := KeyboardStateToShiftState(KeyState);
end;

function IsAccel(VK: Word; const Str: string): Boolean;
begin
  Result := CompareText(Char(VK), GetHotKey(Str)) = 0;
end;

{ Form utility functions }

function GetRealParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
begin
  while (TopForm or not (Control is TCustomForm)) and (Control.Parent <> nil) do
    Control := Control.Parent;
  if Control is TCustomForm then
    Result := TCustomForm(Control) else
    Result := nil;
end;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
begin
  // Override the "TopForm" parameter if the control passed in is in design mode
  // This makes controls calling this function operate correctly when the designer
  // is embedded
  if csDesigning in Control.ComponentState then
    TopForm := False;
  Result := GetRealParentForm(Control, TopForm);
end;

function ValidParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
begin
  Result := GetParentForm(Control, TopForm);
  if Result = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Control.Name]);
end;

{ TControlScrollBar }

constructor TControlScrollBar.Create(AControl: TScrollingWinControl;
  AKind: TScrollBarKind);
begin
  inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPageIncrement := 80;
  FIncrement := FPageIncrement div 10;
  FVisible := True;
  FDelay := 10;
  FLineDiv := 4;
  FPageDiv := 12;
  FColor := clBtnHighlight;
  FParentColor := True;
  FUpdateNeeded := True;
end;

function TControlScrollBar.IsIncrementStored: Boolean;
begin
  Result := not Smooth;
end;

procedure TControlScrollBar.Assign(Source: TPersistent);
begin
  if Source is TControlScrollBar then
  begin
    Visible := TControlScrollBar(Source).Visible;
    Range := TControlScrollBar(Source).Range;
    Position := TControlScrollBar(Source).Position;
    Increment := TControlScrollBar(Source).Increment;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TControlScrollBar.ChangeBiDiPosition;
begin
  if Kind = sbHorizontal then
    if IsScrollBarVisible then
      if not FControl.UseRightToLeftScrollBar then
        Position := 0
      else
        Position := Range;
end;

procedure TControlScrollBar.CalcAutoRange;
var
  I: Integer;
  NewRange, AlignMargin: Integer;

  procedure ProcessHorz(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alLeft, alNone:
          if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
            NewRange := Max(NewRange, Position + Control.Left + Control.Width);
        alRight: Inc(AlignMargin, Control.Width);
      end;
  end;

  procedure ProcessVert(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alTop, alNone:
          if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
            NewRange := Max(NewRange, Position + Control.Top + Control.Height);
        alBottom: Inc(AlignMargin, Control.Height);
      end;
  end;

begin
  if FControl.FAutoScroll then
  begin
    if FControl.AutoScrollEnabled then
    begin
      NewRange := 0;
      AlignMargin := 0;
      for I := 0 to FControl.ControlCount - 1 do
        if Kind = sbHorizontal then
          ProcessHorz(FControl.Controls[I]) else
          ProcessVert(FControl.Controls[I]);
      DoSetRange(NewRange + AlignMargin + Margin);
    end
    else DoSetRange(0);
  end;
end;

function TControlScrollBar.IsScrollBarVisible: Boolean;
var
  Style: Longint;
begin
  Style := WS_HSCROLL;
  if Kind = sbVertical then Style := WS_VSCROLL;
  Result := (Visible) and
            (GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0);
end;

function TControlScrollBar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not ControlSB then
      if AssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else if not AssumeSB and ScrollBarVisible(Code) then
        Result := GetSystemMetrics(Metric) - BorderAdjust;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(FControl.Handle, GWL_STYLE) and
    (WS_BORDER or WS_THICKFRAME) <> 0);
  if Kind = sbVertical then
    Result := FControl.ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL) else
    Result := FControl.ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

function TControlScrollBar.GetScrollPos: Integer;
begin
  Result := 0;
  if Visible then Result := Position;
end;

function TControlScrollBar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False);
end;

procedure TControlScrollBar.ScrollMessage(var Msg: TWMScroll);
var
  Incr, FinalIncr, Count: Integer;
  CurrentTime, StartTime, ElapsedTime: DWORD;

  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(FControl.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
  begin
    if FSmooth and (ScrollCode in [SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN]) then
    begin
      case ScrollCode of
        SB_LINEUP, SB_LINEDOWN:
          begin
            Incr := FIncrement div FLineDiv;
            FinalIncr := FIncrement mod FLineDiv;
            Count := FLineDiv;
          end;
        SB_PAGEUP, SB_PAGEDOWN:
          begin
            Incr := FPageIncrement;
            FinalIncr := Incr mod FPageDiv;
            Incr := Incr div FPageDiv;
            Count := FPageDiv;
          end;
      else
        Count := 0;
        Incr := 0;
        FinalIncr := 0;
      end;
      CurrentTime := 0;
      while Count > 0 do
      begin
        StartTime := GetCurrentTime;
        ElapsedTime := StartTime - CurrentTime;
        if ElapsedTime < DWORD(FDelay) then
          Sleep(DWORD(FDelay) - ElapsedTime);
        CurrentTime := StartTime;
        case ScrollCode of
          SB_LINEUP: SetPosition(FPosition - Incr);
          SB_LINEDOWN: SetPosition(FPosition + Incr);
          SB_PAGEUP: SetPosition(FPosition - Incr);
          SB_PAGEDOWN: SetPosition(FPosition + Incr);
        end;
        FControl.Update;
        Dec(Count);
      end;
      if FinalIncr > 0 then
      begin
        case ScrollCode of
          SB_LINEUP: SetPosition(FPosition - FinalIncr);
          SB_LINEDOWN: SetPosition(FPosition + FinalIncr);
          SB_PAGEUP: SetPosition(FPosition - FinalIncr);
          SB_PAGEDOWN: SetPosition(FPosition + FinalIncr);
        end;
      end;
    end
    else
      case ScrollCode of
        SB_LINEUP: SetPosition(FPosition - FIncrement);
        SB_LINEDOWN: SetPosition(FPosition + FIncrement);
        SB_PAGEUP: SetPosition(FPosition - ControlSize(True, False));
        SB_PAGEDOWN: SetPosition(FPosition + ControlSize(True, False));
        SB_THUMBPOSITION:
            if FCalcRange > 32767 then
              SetPosition(GetRealScrollPosition) else
              SetPosition(Pos);
        SB_THUMBTRACK:
          if Tracking then
            if FCalcRange > 32767 then
              SetPosition(GetRealScrollPosition) else
              SetPosition(Pos);
        SB_TOP: SetPosition(0);
        SB_BOTTOM: SetPosition(FCalcRange);
        SB_ENDSCROLL: begin end;
      end;
  end;
end;

procedure TControlScrollBar.SetButtonSize(Value: Integer);
const
  SysConsts: array[TScrollBarKind] of Integer = (SM_CXHSCROLL, SM_CXVSCROLL);
var
  NewValue: Integer;
begin
  if Value <> ButtonSize then
  begin
    NewValue := Value;
    if NewValue = 0 then
      Value := GetSystemMetrics(SysConsts[Kind]);
    FButtonSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
    if NewValue = 0 then
      FButtonSize := 0;
  end;
end;

procedure TControlScrollBar.SetColor(Value: TColor);
begin
  if Value <> Color then
  begin
    FColor := Value;
    FParentColor := False;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TControlScrollBar.SetParentColor(Value: Boolean);
begin
  if ParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then Color := clBtnHighlight;
  end;
end;

procedure TControlScrollBar.SetPosition(Value: Integer);
var
  Code: Word;
  Form: TCustomForm;
  OldPos: Integer;
begin
  if csReading in FControl.ComponentState then
    FPosition := Value
  else
  begin
    if Value > FCalcRange then Value := FCalcRange
    else if Value < 0 then Value := 0;
    if Kind = sbHorizontal then
      Code := SB_HORZ else
      Code := SB_VERT;
    if Value <> FPosition then
    begin
      OldPos := FPosition;
      FPosition := Value;
      if Kind = sbHorizontal then
        FControl.ScrollBy(OldPos - Value, 0) else
        FControl.ScrollBy(0, OldPos - Value);
      if csDesigning in FControl.ComponentState then
      begin
        Form := GetParentForm(FControl, False);
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
      end;
    end;
    if FlatSB_GetScrollPos(FControl.Handle, Code) <> FPosition then
      FlatSB_SetScrollPos(FControl.Handle, Code, FPosition, True);
  end;
end;

procedure TControlScrollBar.SetSize(Value: Integer);
const
  SysConsts: array[TScrollBarKind] of Integer = (SM_CYHSCROLL, SM_CYVSCROLL);
var
  NewValue: Integer;
begin
  if Value <> Size then
  begin
    NewValue := Value;
    if NewValue = 0 then
      Value := GetSystemMetrics(SysConsts[Kind]);
    FSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
    if NewValue = 0 then
      FSize := 0;
  end;
end;

procedure TControlScrollBar.SetStyle(Value: TScrollBarStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TControlScrollBar.SetThumbSize(Value: Integer);
begin
  if Value <> ThumbSize then
  begin
    FThumbSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TControlScrollBar.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.SetRange(Value: Integer);
begin
  FControl.FAutoScroll := False;
  FScaled := True;
  DoSetRange(Value);
end;

function TControlScrollBar.IsRangeStored: Boolean;
begin
  Result := not FControl.AutoScroll;
end;

procedure TControlScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TControlScrollBar.Update(ControlSB, AssumeSB: Boolean);
type
  TPropKind = (pkStyle, pkButtonSize, pkThumbSize, pkSize, pkBkColor);
const
  Props: array[TScrollBarKind, TPropKind] of Integer = (
    { Horizontal }
    (WSB_PROP_HSTYLE, WSB_PROP_CXHSCROLL, WSB_PROP_CXHTHUMB, WSB_PROP_CYHSCROLL,
     WSB_PROP_HBKGCOLOR),
    { Vertical }
    (WSB_PROP_VSTYLE, WSB_PROP_CYVSCROLL, WSB_PROP_CYVTHUMB, WSB_PROP_CXVSCROLL,
     WSB_PROP_VBKGCOLOR));
  Kinds: array[TScrollBarKind] of Integer = (WSB_PROP_HSTYLE, WSB_PROP_VSTYLE);
  Styles: array[TScrollBarStyle] of Integer = (FSB_REGULAR_MODE,
    FSB_ENCARTA_MODE, FSB_FLAT_MODE);
var
  Code: Word;
  ScrollInfo: TScrollInfo;

  procedure UpdateScrollProperties(Redraw: Boolean);
  begin
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkStyle], Styles[Style], Redraw);
    if ButtonSize > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkButtonSize], ButtonSize, False);
    if ThumbSize > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkThumbSize], ThumbSize, False);
    if Size > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkSize], Size, False);
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkBkColor],
      ColorToRGB(Color), False);
  end;

begin
  FCalcRange := 0;
  Code := SB_HORZ;
  if Kind = sbVertical then Code := SB_VERT;
  if Visible then
  begin
    FCalcRange := Range - ControlSize(ControlSB, AssumeSB);
    if FCalcRange < 0 then FCalcRange := 0;
  end;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  if FCalcRange > 0 then
    ScrollInfo.nMax := Range else
    ScrollInfo.nMax := 0;
  ScrollInfo.nPage := ControlSize(ControlSB, AssumeSB) + 1;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;
  UpdateScrollProperties(FUpdateNeeded);
  FUpdateNeeded := False;
  FlatSB_SetScrollInfo(FControl.Handle, Code, ScrollInfo, True);
  SetPosition(FPosition);
  FPageIncrement := (ControlSize(True, False) * 9) div 10;
  if Smooth then FIncrement := FPageIncrement div 10;
end;

{ TScrollingWinControl }

constructor TScrollingWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  FHorzScrollBar := TControlScrollBar.Create(Self, sbHorizontal);
  FVertScrollBar := TControlScrollBar.Create(Self, sbVertical);
  FAutoScroll := False;
end;

destructor TScrollingWinControl.Destroy;
begin
  FHorzScrollBar.Free;
  FVertScrollBar.Free;
  inherited Destroy;
end;

procedure TScrollingWinControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TScrollingWinControl.CreateWnd;
begin
  inherited CreateWnd;
  //! Scroll bars don't move to the Left side of a TScrollingWinControl when the
  //! WS_EX_LEFTSCROLLBAR flag is set and InitializeFlatSB is called.
  //! A call to UnInitializeFlatSB does nothing.
  if not SysLocale.MiddleEast and
     not CheckWin32Version(5, 1) then
    InitializeFlatSB(Handle);
  UpdateScrollBars;
end;

procedure TScrollingWinControl.AlignControls(AControl: TControl; var ARect: TRect);
begin
  CalcAutoRange;
  inherited AlignControls(AControl, ARect);
end;

function TScrollingWinControl.AutoScrollEnabled: Boolean;
begin
  Result := not AutoSize and not (DockSite and UseDockManager);
end;

procedure TScrollingWinControl.DoFlipChildren;
var
  Loop: Integer;
  TheWidth: Integer;
  ScrollBarActive: Boolean;
  FlippedList: TList;
begin
  FlippedList := TList.Create;
  try
    TheWidth := ClientWidth;
    with HorzScrollBar do begin
      ScrollBarActive := (IsScrollBarVisible) and (TheWidth < Range);
      if ScrollBarActive then
      begin
        TheWidth := Range;
        Position := 0;
      end;
    end;

    for Loop := 0 to ControlCount - 1 do with Controls[Loop] do
    begin
      FlippedList.Add(Controls[Loop]);
      Left := TheWidth - Width - Left;
    end;

    { Allow controls that have associations to realign themselves }
    for Loop := 0 to FlippedList.Count - 1 do
      TControl(FlippedList[Loop]).Perform(CM_ALLCHILDRENFLIPPED, 0, 0);

    if ScrollBarActive then
      HorzScrollBar.ChangeBiDiPosition;
  finally
     FlippedList.Free;
  end;
end;

procedure TScrollingWinControl.CalcAutoRange;
begin
  if FAutoRangeCount <= 0 then
  begin
    HorzScrollBar.CalcAutoRange;
    VertScrollBar.CalcAutoRange;
  end;
end;

procedure TScrollingWinControl.SetAutoScroll(Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then CalcAutoRange else
    begin
      HorzScrollBar.Range := 0;
      VertScrollBar.Range := 0;
    end;
  end;
end;

procedure TScrollingWinControl.SetHorzScrollBar(Value: TControlScrollBar);
begin
  FHorzScrollBar.Assign(Value);
end;

procedure TScrollingWinControl.SetVertScrollBar(Value: TControlScrollBar);
begin
  FVertScrollBar.Assign(Value);
end;

procedure TScrollingWinControl.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then
    try
      FUpdatingScrollBars := True;
      if FVertScrollBar.NeedsScrollBarVisible then
      begin
        FHorzScrollBar.Update(False, True);
        FVertScrollBar.Update(True, False);
      end
      else if FHorzScrollBar.NeedsScrollBarVisible then
      begin
        FVertScrollBar.Update(False, True);
        FHorzScrollBar.Update(True, False);
      end
      else
      begin
        FVertScrollBar.Update(False, False);
        FHorzScrollBar.Update(True, False);
      end;
    finally
      FUpdatingScrollBars := False;
    end;
end;

procedure TScrollingWinControl.AutoScrollInView(AControl: TControl);
begin
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) then
    ScrollInView(AControl);
end;

procedure TScrollingWinControl.DisableAutoRange;
begin
  Inc(FAutoRangeCount);
end;

procedure TScrollingWinControl.EnableAutoRange;
begin
  if FAutoRangeCount > 0 then
  begin
    Dec(FAutoRangeCount);
    if (FAutoRangeCount = 0) and (FHorzScrollBar.Visible or
      FVertScrollBar.Visible) then CalcAutoRange;
  end;
end;

procedure TScrollingWinControl.ScrollInView(AControl: TControl);
var
  Rect: TRect;
begin
  if AControl = nil then Exit;
  Rect := AControl.ClientRect;
  Dec(Rect.Left, HorzScrollBar.Margin);
  Inc(Rect.Right, HorzScrollBar.Margin);
  Dec(Rect.Top, VertScrollBar.Margin);
  Inc(Rect.Bottom, VertScrollBar.Margin);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Left < 0 then
    with HorzScrollBar do Position := Position + Rect.Left
  else if Rect.Right > ClientWidth then
  begin
    if Rect.Right - Rect.Left > ClientWidth then
      Rect.Right := Rect.Left + ClientWidth;
    with HorzScrollBar do Position := Position + Rect.Right - ClientWidth;
  end;
  if Rect.Top < 0 then
    with VertScrollBar do Position := Position + Rect.Top
  else if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    with VertScrollBar do Position := Position + Rect.Bottom - ClientHeight;
  end;
end;

procedure TScrollingWinControl.ScaleScrollBars(M, D: Integer);
begin
  if M <> D then
  begin
    if not (csLoading in ComponentState) then
    begin
      HorzScrollBar.FScaled := True;
      VertScrollBar.FScaled := True;
    end;
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
    if not FAutoScroll then
    begin
      with HorzScrollBar do if FScaled then Range := MulDiv(Range, M, D);
      with VertScrollBar do if FScaled then Range := MulDiv(Range, M, D);
    end;
  end;
  HorzScrollBar.FScaled := False;
  VertScrollBar.FScaled := False;
end;

procedure TScrollingWinControl.ChangeScale(M, D: Integer);
begin
  ScaleScrollBars(M, D);
  inherited ChangeScale(M, D);
end;

procedure TScrollingWinControl.Resizing(State: TWindowState);
begin
  // Overridden by TCustomFrame
end;

procedure TScrollingWinControl.WMSize(var Message: TWMSize);
var
  NewState: TWindowState;
  SaveUpdatingScrollBars: Boolean;
begin
  Inc(FAutoRangeCount);
  try
    inherited;
    NewState := wsNormal;
    case Message.SizeType of
      SIZENORMAL: NewState := wsNormal;
      SIZEICONIC: NewState := wsMinimized;
      SIZEFULLSCREEN: NewState := wsMaximized;
    end;
    Resizing(NewState);
  finally
    Dec(FAutoRangeCount);
  end;
  SaveUpdatingScrollBars := FUpdatingScrollBars;
  FUpdatingScrollBars := True;
  try
    CalcAutoRange;
  finally
    FUpdatingScrollBars := SaveUpdatingScrollBars;
  end;
  if FHorzScrollBar.Visible or FVertScrollBar.Visible then
    UpdateScrollBars;
end;

procedure TScrollingWinControl.WMHScroll(var Message: TWMHScroll);
begin
  if (Message.ScrollBar = 0) and FHorzScrollBar.Visible then
    FHorzScrollBar.ScrollMessage(Message) else
    inherited;
end;

procedure TScrollingWinControl.WMVScroll(var Message: TWMVScroll);
begin
  if (Message.ScrollBar = 0) and FVertScrollBar.Visible then
    FVertScrollBar.ScrollMessage(Message) else
    inherited;
end;

procedure TScrollingWinControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := Bounds(-HorzScrollBar.Position, -VertScrollBar.Position,
    Max(HorzScrollBar.Range, ClientWidth), Max(ClientHeight,
    VertScrollBar.Range));
  inherited AdjustClientRect(Rect);
end;

procedure TScrollingWinControl.CMBiDiModeChanged(var Message: TMessage);
var
  Save: Integer;
begin
  Save := Message.WParam;
  try
    { prevent inherited from calling Invalidate & RecreateWnd }
    if not (Self is TScrollBox) then Message.wParam := 1;
    inherited;
  finally
    Message.wParam := Save;
  end;
  if HandleAllocated then
  begin
    HorzScrollBar.ChangeBiDiPosition;
    UpdateScrollBars;
  end;
end;

{ TScrollBox }

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csPannable];
  AutoScroll := True;
  Width := 185;
  Height := 41;
  FBorderStyle := bsSingle;
end;

procedure TScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TScrollBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TScrollBox.WMNCHitTest(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

procedure TScrollBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TScrollBox.PaintWindow(DC: HDC);
begin
  //  Do nothing
end;

{ TCustomFrame }

constructor TCustomFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csParentBackground, csPannable];
  if (ClassType <> TFrame) and not (csDesignInstance in ComponentState) then
  begin
    if not InitInheritedComponent(Self, TFrame) then
      raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
  end
  else
  begin
    Width := 320;
    Height := 240;
  end;
end;

procedure TCustomFrame.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Parent = nil then
    Params.WndParent := Application.Handle;
end;

procedure TCustomFrame.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

procedure TCustomFrame.PaintWindow(DC: HDC);
begin
  // Paint a grid if designing a frame that paints its own background
  if (csDesigning in ComponentState) and (Parent is TForm) then
    with TForm(Parent) do
      if (Designer <> nil) and (Designer.GetRoot = Self) and 
        (not ThemeServices.ThemesEnabled or not Self.ParentBackground) then
        Designer.PaintGrid
end;

procedure TCustomFrame.SetParent(AParent: TWinControl);
var
  LRecreate: Boolean;
begin
  LRecreate := HandleAllocated;
  if LRecreate then 
    UpdateRecreatingFlag(True);
  try
    if (Parent = nil) and LRecreate then
      DestroyHandle;
    inherited;
  finally
    if LRecreate then
      UpdateRecreatingFlag(False);
  end;
end;

{ TCustomActiveForm }

constructor TCustomActiveForm.Create(AOwner: TComponent);
begin
  FAxBorderStyle := afbSingle;
  inherited Create(AOwner);
  BorderStyle := bsNone;
  BorderIcons := [];
  TabStop := True;
end;

procedure TCustomActiveForm.SetAxBorderStyle(Value: TActiveFormBorderStyle);
begin
  if FAxBorderStyle <> Value then
  begin
    FAxBorderStyle := Value;
    if not (csDesigning in ComponentState) then RecreateWnd;
  end;
end;

procedure TCustomActiveForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    with Params do
    begin
      Style := Style and not WS_CAPTION;
      case FAxBorderStyle of
        afbNone: ;// do nothing
        afbSingle: Style := Style or WS_BORDER;
        afbSunken: ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        afbRaised:
          begin
            Style := Style or WS_DLGFRAME;
            ExStyle := ExStyle or WS_EX_WINDOWEDGE;
          end;
      end;
    end;
end;

function TCustomActiveForm.WantChildKey(Child: TControl; var Message: TMessage): Boolean;
begin
  Result := ((Message.Msg = WM_CHAR) and (Message.WParam = VK_TAB)) or
    (Child.Perform(CN_BASE + Message.Msg, Message.WParam,
      Message.LParam) <> 0);
end;

{ Hack to overlay GlassFrame on PixelsPerInch in TCustomForm }

type
  PPixelsPerInchOverload = ^TPixelsPerInchOverload;
  TPixelsPerInchOverload = record
    PixelsPerInch: Integer;
    GlassFrame: TGlassFrame;
    RefreshGlassFrame: Boolean;
  end;

function GetFPixelsPerInch(FPixelsPerInch: Integer): Integer; inline;
begin
  Result := PPixelsPerInchOverload(FPixelsPerInch).PixelsPerInch;
end;

procedure SetFPixelsPerInch(FPixelsPerInch, Value: Integer); inline;
begin
  PPixelsPerInchOverload(FPixelsPerInch).PixelsPerInch := Value;
end;

function GetRefreshGlassFrame(FPixelsPerInch: Integer): Boolean; inline;
begin
  Result := PPixelsPerInchOverload(FPixelsPerInch).RefreshGlassFrame;
end;

procedure SetRefreshGlassFrame(FPixelsPerInch: Integer; Value: Boolean); inline;
begin
  PPixelsPerInchOverload(FPixelsPerInch).RefreshGlassFrame := Value;
end;

{ TCustomForm }

constructor TCustomForm.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        if not InitInheritedComponent(Self, TForm) then
          raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      finally
        Exclude(FFormState, fsCreating);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TCustomForm.AfterConstruction;
begin
  if not OldCreateOrder then DoCreate;
  if fsActivated in FFormState then
  begin
    Activate;
    Exclude(FFormState, fsActivated);
  end;
end;

constructor TCustomForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  Pointer(FPixelsPerInch) := AllocMem(SizeOf(TPixelsPerInchOverload));
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csPannable];
  Left := 0;
  Top := 0;
  Width := 320;
  Height := 240;
  FIcon := TIcon.Create;
  FIcon.Width := GetSystemMetrics(SM_CXSMICON);
  FIcon.Height := GetSystemMetrics(SM_CYSMICON);
  FIcon.OnChange := IconChanged;
  FPosition := poDefaultPosOnly;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FBorderStyle := bsSizeable;
  FWindowState := wsNormal;
  FDefaultMonitor := dmActiveForm;
  FInCMParentBiDiModeChanged := False;
  SetFPixelsPerInch(FPixelsPerInch, Screen.PixelsPerInch);
  PPixelsPerInchOverload(FPixelsPerInch).GlassFrame := TGlassFrame.Create(Self);
  GlassFrame.OnChange := UpdateGlassFrame;
  SetRefreshGlassFrame(FPixelsPerInch, False);
  FPrintScale := poProportional;
  FloatingDockSiteClass := TWinControlClass(ClassType);
  FAlphaBlendValue := 255;
  FTransparentColorValue := 0;
  Visible := False;
  ParentColor := False;
  ParentFont := False;
  Ctl3D := True;
  Screen.AddForm(Self);
  FSnapBuffer := 10;
end;

procedure TCustomForm.BeforeDestruction;
begin
  GlobalNameSpace.BeginWrite;
  Destroying;
  Screen.FSaveFocusedList.Remove(Self);
  RemoveFixupReferences(Self, '');
  if FOleForm <> nil then FOleForm.OnDestroy;
  if FormStyle <> fsMDIChild then Hide;
  if not OldCreateOrder then DoDestroy;
end;

destructor TCustomForm.Destroy;
begin
  Application.RemovePopupForm(Self);
  if not (csDestroying in ComponentState) then GlobalNameSpace.BeginWrite;
  try
    if OldCreateOrder then DoDestroy;
    MergeMenu(False);
    if HandleAllocated then DestroyWindowHandle;
    Screen.RemoveForm(Self);
    FCanvas.Free;
    FIcon.Free;
    FreeAndNil(FPopupChildren);
    FreeAndNil(FRecreateChildren);
    GlassFrame.Free;
    FreeMem(Pointer(FPixelsPerInch));
    inherited Destroy;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TCustomForm.DoCreate;
begin
  if Assigned(FOnCreate) then
  try
    FOnCreate(Self);
  except
    if not HandleCreateException then
      raise;
  end;
  if fsVisible in FFormState then Visible := True;
end;

procedure TCustomForm.DoDestroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomForm.Loaded;
var
  Control: TWinControl;
begin
  inherited Loaded;
  if (ActiveControl <> nil) and ((Parent = nil) or not (csDesigning in ComponentState)) then
  begin
    Control := ActiveControl;
    FActiveControl := nil;
    if Control.CanFocus then SetActiveControl(Control);
  end;
  if GlassFrame.FrameExtended then
    UpdateGlassFrame(nil);
end;

procedure TCustomForm.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  case Operation of
    opInsert:
      begin
        if not (csLoading in ComponentState) and (Menu = nil) and
          (AComponent.Owner = Self) and (AComponent is TMainMenu) then
          Menu := TMainMenu(AComponent);
      end;
    opRemove:
      begin
        if Menu = AComponent then Menu := nil;
        if WindowMenu = AComponent then WindowMenu := nil;
        if ObjectMenuItem = AComponent then ObjectMenuItem := nil;
        if PopupParent = AComponent then PopupParent := nil;
        if Assigned(FPopupChildren) then
        begin
          I := FPopupChildren.IndexOf(AComponent);
          if I >= 0 then
          begin
            FPopupChildren.Delete(I);
            AComponent.RemoveFreeNotification(Self);
          end;
        end;
        if Assigned(FRecreateChildren) then
        begin
          I := FRecreateChildren.IndexOf(AComponent);
          if I >= 0 then
          begin
            FRecreateChildren.Delete(I);
            AComponent.RemoveFreeNotification(Self);
          end;
        end;
        if FInternalPopupParent = AComponent then FInternalPopupParent := nil;
        if FActiveOleControl = AComponent then FActiveOleControl := nil;
      end;
  end;
  if FDesigner <> nil then
    FDesigner.Notification(AComponent, Operation);
end;

procedure TCustomForm.ReadState(Reader: TReader);
var
  NewTextHeight: Integer;
  Scaled: Boolean;
begin
  DisableAlign;
  try
    FClientWidth := 0;
    FClientHeight := 0;
    FTextHeight := 0;
    Scaled := False;
    if ClassParent = TForm then
      FOldCreateOrder := not ModuleIsCpp;
    inherited ReadState(Reader);
    if (GetFPixelsPerInch(FPixelsPerInch) <> 0) and (FTextHeight > 0) then
    begin
      if (sfFont in ScalingFlags) and (GetFPixelsPerInch(FPixelsPerInch) <> Screen.PixelsPerInch) then
        Font.Height := MulDiv(Font.Height, Screen.PixelsPerInch, GetFPixelsPerInch(FPixelsPerInch));
      SetFPixelsPerInch(FPixelsPerInch, Screen.PixelsPerInch);
      NewTextHeight := GetTextHeight;
      if FTextHeight <> NewTextHeight then
      begin
        Scaled := True;
        ScaleScrollBars(NewTextHeight, FTextHeight);
        ScaleConstraints(NewTextHeight, FTextHeight);
        ScaleControls(NewTextHeight, FTextHeight);
        if sfWidth in ScalingFlags then
          FClientWidth := MulDiv(FClientWidth, NewTextHeight, FTextHeight);
        if sfHeight in ScalingFlags then
          FClientHeight := MulDiv(FClientHeight, NewTextHeight, FTextHeight);
        if sfDesignSize in ScalingFlags then
        begin
          FDesignSize.X := MulDiv(FDesignSize.X, NewTextHeight, FTextHeight);
          FDesignSize.Y := MulDiv(FDesignSize.Y, NewTextHeight, FTextHeight);
        end;
      end;
    end;
    if FClientWidth > 0 then inherited ClientWidth := FClientWidth;
    if FClientHeight > 0 then inherited ClientHeight := FClientHeight;
    ScalingFlags := [];
    if not Scaled then
    begin
      { Forces all ScalingFlags to [] }
      ScaleScrollBars(1, 1);
      ScaleControls(1, 1);
    end;
    Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  finally
    EnableAlign;
  end;
end;

procedure TCustomForm.DefineProperties(Filer: TFiler);

type
  TGlassFrameElemnt = (geLeft, geTop, geRight, geBottom, geEnabled, geSheetOfGlass);

  function DoWriteGlassFrame(Element: TGlassFrameElemnt): Boolean;
  begin
    case Element of
      geLeft: Result := GlassFrame.Left <> 0;
      geTop: Result := GlassFrame.Top <> 0;
      geRight: Result := GlassFrame.Right <> 0;
      geBottom: Result := GlassFrame.Bottom <> 0;
      geEnabled: Result := GlassFrame.Enabled;
      geSheetOfGlass: Result := GlassFrame.SheetOfGlass;
    else
      Result := False; // remove compiler warning
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('PixelsPerInch', nil, WritePixelsPerInch, not IsControl);
  Filer.DefineProperty('TextHeight', ReadTextHeight, WriteTextHeight, not IsControl);
  Filer.DefineProperty('IgnoreFontProperty', ReadIgnoreFontProperty, nil, False);
  Filer.DefineProperty('GlassFrame.Bottom', ReadGlassFrameBottom, WriteGlassFrameBottom, DoWriteGlassFrame(geBottom));
  Filer.DefineProperty('GlassFrame.Enabled', ReadGlassFrameEnabled, WriteGlassFrameEnabled, DoWriteGlassFrame(geEnabled));
  Filer.DefineProperty('GlassFrame.Left', ReadGlassFrameLeft, WriteGlassFrameLeft, DoWriteGlassFrame(geLeft));
  Filer.DefineProperty('GlassFrame.Right', ReadGlassFrameRight, WriteGlassFrameRight, DoWriteGlassFrame(geRight));
  Filer.DefineProperty('GlassFrame.SheetOfGlass', ReadGlassFrameSheetOfGlass, WriteGlassFrameSheetOfGlass, DoWriteGlassFrame(geSheetOfGlass));
  Filer.DefineProperty('GlassFrame.Top', ReadGlassFrameTop, WriteGlassFrameTop, DoWriteGlassFrame(geTop));
end;

procedure TCustomForm.ReadIgnoreFontProperty(Reader: TReader);
begin   // reroute BCB IgnoreFontProperty to use VCL locale font solution
  if Reader.ReadBoolean then
    ParentFont := True;
end;

procedure TCustomForm.ReadTextHeight(Reader: TReader);
begin
  FTextHeight := Reader.ReadInteger;
end;

procedure TCustomForm.WriteTextHeight(Writer: TWriter);
begin
  Writer.WriteInteger(GetTextHeight);
end;

procedure TCustomForm.WritePixelsPerInch(Writer: TWriter);
begin
  Writer.WriteInteger(GetPixelsPerInch);
end;

function TCustomForm.GetLeft: Integer;
begin
  if (csDesigning in ComponentState) and (Parent <> nil) then
    Result := SmallInt((DesignInfo and $FFFF0000) shr 16)
  else
    Result := inherited Left;
end;

function TCustomForm.GetTop: Integer;
begin
  if (csDesigning in ComponentState) and (Parent <> nil) then
    Result := SmallInt(DesignInfo and $0000FFFF)
  else
    Result := inherited Top;
end;

function TCustomForm.GetTextHeight: Integer;
begin
  Result := Canvas.TextHeight('0');
end;

procedure TCustomForm.BeginAutoDrag;
begin
  { Do nothing }
end;

procedure TCustomForm.ChangeScale(M, D: Integer);
var
  PriorHeight: Integer;
begin
  ScaleScrollBars(M, D);
  ScaleConstraints(M, D);
  ScaleControls(M, D);
  if IsClientSizeStored then
  begin
    PriorHeight := ClientHeight;
    ClientWidth := MulDiv(ClientWidth, M, D);
    ClientHeight := MulDiv(PriorHeight, M, D);
  end;
  Font.Size := MulDiv(Font.Size, M, D);
end;

procedure TCustomForm.IconChanged(Sender: TObject);
begin
  if NewStyleControls then
  begin
    if HandleAllocated and (BorderStyle <> bsDialog) then
      SendMessage(Handle, WM_SETICON, 1, GetIconHandle);
  end else
    if IsIconic(Handle) then Invalidate;
end;

function TCustomForm.IsClientSizeStored: Boolean;
begin
  Result := not IsFormSizeStored;
end;

function TCustomForm.IsFormSizeStored: Boolean;
begin
  Result := AutoScroll or (HorzScrollBar.Range <> 0) or
    (VertScrollBar.Range <> 0);
end;

function TCustomForm.IsAutoScrollStored: Boolean;
begin
  Result := IsForm and
    (AutoScroll = (BorderStyle in [bsSizeable, bsSizeToolWin]));
end;

procedure TCustomForm.DoClose(var Action: TCloseAction);
begin
  if Assigned(FOnClose) then FOnClose(Self, Action);
end;

procedure TCustomForm.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TCustomForm.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

function TCustomForm.GetClientRect: TRect;
begin
  if IsIconic(Handle) then
  begin
    SetRect(Result, 0, 0, 0, 0);
    AdjustWindowRectEx(Result, GetWindowLong(Handle, GWL_STYLE),
      Menu <> nil, GetWindowLong(Handle, GWL_EXSTYLE));
    SetRect(Result, 0, 0,
      Width - Result.Right + Result.Left,
      Height - Result.Bottom + Result.Top);
  end else
    Result := inherited GetClientRect;
end;

procedure TCustomForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

function TCustomForm.GetFloating: Boolean;
begin
  Result := (HostDockSite = nil) and (FloatingDockSiteClass = ClassType);
end;

function TCustomForm.GetOwnerWindow: HWND;
begin
  if WindowHandle <> 0 then
    Result := GetWindow(WindowHandle, GW_OWNER)
  else
    Result := 0;
end;

procedure TCustomForm.SetChildOrder(Child: TComponent; Order: Integer);
var
  I, J: Integer;
begin
  if Child is TControl then
    inherited SetChildOrder(Child, Order)
  else
  begin
    Dec(Order, ControlCount);
    J := -1;
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent then
      begin
        Inc(J);
        if J = Order then
        begin
          Child.ComponentIndex := I;
          Exit;
        end;
      end;
  end;
end;

procedure TCustomForm.SetParentBiDiMode(Value: Boolean);
begin
  if ParentBiDiMode <> Value then
  begin
    inherited;
    { if there is no parent, then the parent is Application }
    if Parent = nil then
      Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  end;
end;

procedure TCustomForm.SetClientWidth(Value: Integer);
begin
  if csReadingState in ControlState then
  begin
    FClientWidth := Value;
    ScalingFlags := ScalingFlags + [sfWidth];
  end else inherited ClientWidth := Value;
end;

procedure TCustomForm.SetClientHeight(Value: Integer);
begin
  if csReadingState in ControlState then
  begin
    FClientHeight := Value;
    ScalingFlags := ScalingFlags + [sfHeight];
  end else inherited ClientHeight := Value;
end;

procedure TCustomForm.SetVisible(Value: Boolean);
begin
  if fsCreating in FFormState then
    if Value then
      Include(FFormState, fsVisible) else
      Exclude(FFormState, fsVisible)
  else
  begin
    if Value and (Visible <> Value) then SetWindowToMonitor;
    inherited Visible := Value;
  end;
end;

procedure TCustomForm.VisibleChanging;
begin
  if (FormStyle = fsMDIChild) and Visible and (Parent = nil) then
    raise EInvalidOperation.Create(SMDIChildNotVisible);
  if (Self = Application.MainForm) and not (csDestroying in ComponentState) then
  begin
    if Visible then
    begin
      ChangeAppWindow(Handle, not Application.MainFormOnTaskBar, True);
      ChangeAppWindow(Application.Handle, Application.ShowMainForm, True);
    end
    else
    begin
      ChangeAppWindow(Application.Handle, not Application.MainFormOnTaskBar, False);
      ChangeAppWindow(Handle, Application.MainFormOnTaskBar, True);
    end;
  end;
end;

function TCustomForm.WantChildKey(Child: TControl; var Message: TMessage): Boolean;
begin
  Result := False;
end;

procedure TCustomForm.SetParent(AParent: TWinControl);
var
  LRecreate: Boolean;
begin
  if (Parent <> AParent) and (AParent <> Self) then
  begin
    LRecreate := Parent = nil;
    if LRecreate then
      UpdateRecreatingFlag(True);
    try
      if LRecreate then
        DestroyHandle;
      if not (csDesigning in ComponentState) then
        FActiveControl := nil;
      FFocusedControl := nil;
      inherited SetParent(AParent);
      if Parent = nil then UpdateControlState;
    finally
      if LRecreate then
        UpdateRecreatingFlag(False);
    end;
  end;
end;

procedure TCustomForm.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  inherited ValidateRename(AComponent, CurName, NewName);
  if FDesigner <> nil then
    FDesigner.ValidateRename(AComponent, CurName, NewName);
end;

type
  TMenuItemAccess = class(TMenuItem);

procedure TCustomForm.WndProc(var Message: TMessage);
var
  FocusHandle: HWND;
  SaveIndex: Integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  LControl: TWinControl;
  DC: HDC;
  ParentWnd: HWND;
  ParentForm: TCustomForm;
begin
  with Message do
    case Msg of
      WM_SYSCOMMAND:
        if (WParam and $FFF0 = SC_RESTORE) and (Self = Application.MainForm) and Application.MainFormOnTaskBar then
          Application.InternalRestore;
      WM_ACTIVATE, WM_SETFOCUS, WM_KILLFOCUS:
        begin
          if not FocusMessages then Exit;
          if (Msg = WM_SETFOCUS) and not (csDesigning in ComponentState) then
          begin
            FocusHandle := 0;
            if FormStyle = fsMDIForm then
            begin
              if ActiveMDIChild <> nil then FocusHandle := ActiveMDIChild.Handle;
            end
            else if (FActiveControl <> nil) and (FActiveControl <> Self) then
              FocusHandle := FActiveControl.Handle;
            if FocusHandle <> 0 then
            begin
              Windows.SetFocus(FocusHandle);
              Exit;
            end;
          end;
        end;
      CM_SETACTIVECONTROL:
        begin
          ParentWnd := HWND(Message.WParam);
          LControl := nil;
          while (ParentWnd <> 0) and (LControl = nil) do
          begin
            LControl := FindControl(ParentWnd);
            ParentWnd := GetParent(ParentWnd);
          end;
          if LControl <> nil then
          begin
            ParentForm := GetParentForm(LControl);
            if ((FActiveControl <> LControl) or (LControl <> Screen.FActiveControl)) and
               (ParentForm <> nil) and
               ((ParentForm = Self) or (ParentForm <> Screen.FActiveCustomForm)) then
            begin
              if (ParentForm.ActiveControl <> nil) and (ParentForm.ActiveControl <> LControl) then
                ParentForm.ActiveControl.PerForm(WM_KILLFOCUS, 0, 0);
              ParentForm.SetFocusedControl(LControl);
            end;
          end;
        end;
      CM_EXIT:
        if HostDockSite <> nil then DeActivate;
      CM_ENTER:
        if HostDockSite <> nil then Activate;
      WM_WINDOWPOSCHANGING:
        if ([csLoading, csDesigning] * ComponentState = [csLoading]) then
        begin
          if (Position in [poDefault, poDefaultPosOnly]) and
            (WindowState <> wsMaximized) then
            with PWindowPos(Message.lParam)^ do flags := flags or SWP_NOMOVE;
          if (Position in [poDefault, poDefaultSizeOnly]) and
            (BorderStyle in [bsSizeable, bsSizeToolWin]) then
            with PWindowPos(Message.lParam)^ do flags := flags or SWP_NOSIZE;
        end;
      WM_DRAWITEM:
        with PDrawItemStruct(Message.LParam)^ do
          if (CtlType = ODT_MENU) and Assigned(Menu) then
          begin
            MenuItem := Menu.FindItem(itemID, fkCommand);
            if MenuItem <> nil then
            begin
              Canvas := TControlCanvas.Create;
              with Canvas do
              try
                SaveIndex := SaveDC(hDC);
                try
                  Handle := hDC;
                  Font := Screen.MenuFont;
                  Menus.DrawMenuItem(MenuItem, Canvas, rcItem,
                    TOwnerDrawState(LongRec(itemState).Lo));
                finally
                  Handle := 0;
                  RestoreDC(hDC, SaveIndex)
                end;
              finally
                Free;
              end;
              Exit;
            end;
          end;
      WM_MEASUREITEM:
        with PMeasureItemStruct(Message.LParam)^ do
          if (CtlType = ODT_MENU) and Assigned(Menu) then
          begin
            MenuItem := Menu.FindItem(itemID, fkCommand);
            if MenuItem <> nil then
            begin
              DC := GetWindowDC(Handle);
              try
                Canvas := TControlCanvas.Create;
                with Canvas do
                try
                  SaveIndex := SaveDC(DC);
                  try
                    Handle := DC;
                    Font := Screen.MenuFont;
                    TMenuItemAccess(MenuItem).MeasureItem(Canvas,
                      Integer(itemWidth), Integer(itemHeight));
                  finally
                    Handle := 0;
                    RestoreDC(DC, SaveIndex);
                  end;
                finally
                  Canvas.Free;
                end;
              finally
                ReleaseDC(Handle, DC);
              end;
              Exit;
            end;
          end;
      WM_DWMCOMPOSITIONCHANGED,
      WM_DWMNCRENDERINGCHANGED:
        UpdateGlassFrame(nil);
    else if Message.Msg = RM_TaskbarCreated then
      begin
        Perform(CM_WININICHANGE, 0, 0);
        Perform(CM_SYSCOLORCHANGE, 0, 0);
        Perform(CM_SYSFONTCHANGED, 0, 0);
        Perform(CM_PARENTCOLORCHANGED, 0, 0);
        Perform(CM_PARENTFONTCHANGED, 0, 0);
        Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
      end;
    end;
  inherited WndProc(Message);
end;

procedure TCustomForm.ClientWndProc(var Message: TMessage);

  procedure Default;
  begin
    with Message do
      Result := CallWindowProc(FDefClientProc, ClientHandle, Msg, wParam, lParam);
  end;

  function MaximizedChildren: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to MDIChildCount - 1 do
      if MDIChildren[I].WindowState = wsMaximized then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  DC: HDC;
  PS: TPaintStruct;
  R: TRect;
begin
  with Message do
    case Msg of
      WM_NCHITTEST:
        begin
          Default;
          if Result = HTCLIENT then Result := HTTRANSPARENT;
        end;
      WM_ERASEBKGND:
        begin
          FillRect(TWMEraseBkGnd(Message).DC, ClientRect, Brush.Handle);
          { Erase the background at the location of an MDI client window }
          if (FormStyle = fsMDIForm) and (FClientHandle <> 0) then
          begin
            Windows.GetClientRect(FClientHandle, R);
            FillRect(TWMEraseBkGnd(Message).DC, R, Brush.Handle);
          end;
          Result := 1;
        end;
      $3F://!
        begin
          Default;
          if FFormStyle = fsMDIForm then
            ShowMDIClientEdge(FClientHandle, (MDIChildCount = 0) or
              not MaximizedChildren);
        end;
      WM_PAINT:
        begin
          DC := TWMPaint(Message).DC;
          if DC = 0 then
            TWMPaint(Message).DC := BeginPaint(ClientHandle, PS);
          try
            if DC = 0 then
            begin
              GetWindowRect(FClientHandle, R);
              R.TopLeft := ScreenToClient(R.TopLeft);
              MoveWindowOrg(TWMPaint(Message).DC, -R.Left, -R.Top);
            end;
            PaintHandler(TWMPaint(Message));
          finally
            if DC = 0 then
              EndPaint(ClientHandle, PS);
          end;
        end;
    else
      Default;
    end;
end;

procedure TCustomForm.AlignControls(AControl: TControl; var Rect: TRect);
var
  R: TRect;
begin
  inherited AlignControls(AControl, Rect);
  if ClientHandle <> 0 then
  begin
    with Rect do
      SetWindowPos(FClientHandle, HWND_BOTTOM, Left, Top, Right - Left,
        Bottom - Top, SWP_NOZORDER + SWP_NOACTIVATE);
    if FormStyle = fsMDIForm then
      if Windows.GetClientRect(FClientHandle, R) then
        InvalidateRect(ClientHandle, nil, True);
  end;
end;

procedure TCustomForm.CMBiDiModeChanged(var Message: TMessage);
var
  ExStyle: DWORD;
  Loop: Integer;
begin
  inherited;
  { inherited does not call RecreateWnd, so we need to call SetWindowLong }
  if HandleAllocated then
  begin
    ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE))and (not WS_EX_RIGHT) and
      (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
    AddBiDiModeExStyle(ExStyle);
    SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
  end;
  { Menus derive from TComponent, so we need to update them here. We cannot
    use FMenu because forms can have many menus. }
  for Loop := 0 to ComponentCount - 1 do
    if Components[Loop] is TMenu then
      TMenu(Components[Loop]).ParentBiDiModeChanged;
end;

procedure TCustomForm.CMParentBiDiModeChanged(var Message: TMessage);
begin
  { Prevent needless recursion }
  if FInCMParentBiDiModeChanged then Exit;
  FInCMParentBiDiModeChanged := True;
  try
    if ParentBiDiMode then
    begin
      { if there is no parent, then the parent is Application }
      if Parent = nil then
        BiDiMode := Application.BiDiMode
      else
        BiDiMode := Parent.BiDiMode;
      ParentBiDiMode := True;
    end;
  finally
    FInCMParentBiDiModeChanged := False;
  end;
end;

procedure TCustomForm.CMPopupHwndDestroy(var Message: TCMPopupHWndDestroy);
var
  Index: Integer;
begin
  if (FindControl(Message.PopupFormInfo.PopupWnd) = nil) or not Message.PopupFormInfo.IsPopup then
  begin
    Index := Length(FPopupWnds);
    SetLength(FPopupWnds, Index + 1);
    FPopupWnds[Index].ID := Message.PopupFormInfo.PopupID;
    FPopupWnds[Index].ControlWnd := Message.PopupControlWnd;
  end;
end;

procedure TCustomForm.SetDesigner(ADesigner: IDesignerHook);
begin
  FDesigner := ADesigner;
end;

procedure TCustomForm.GetBorderIconStyles(var Style, ExStyle: Cardinal);
var
  LIcons: TBorderIcons;
  LBorderStyle: TFormBorderStyle;
begin
  // Clear existing border icon styles
  Style := Style and not (WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_SYSMENU);
  ExStyle := ExStyle and not WS_EX_CONTEXTHELP;

  // Account for special casses with MDI children
  LBorderStyle := FBorderStyle;
  if (FormStyle = fsMDIChild) and (LBorderStyle in [bsNone, bsDialog]) then
    LBorderStyle := bsSizeable;

  // Adjust icons based on border style
  LIcons := FBorderIcons;
  case LBorderStyle of
    bsNone: LIcons := [];
    bsDialog: LIcons := LIcons * [biSystemMenu, biHelp];
    bsToolWindow,
    bsSizeToolWin: LIcons := LIcons * [biSystemMenu];
  end;

  // Set border icon styles
  if LBorderStyle in [bsSingle, bsSizeable, bsNone] then
  begin
    if (FormStyle <> fsMDIChild) or (biSystemMenu in LIcons) then
    begin
      if biMinimize in LIcons then Style := Style or WS_MINIMIZEBOX;
      if biMaximize in LIcons then Style := Style or WS_MAXIMIZEBOX;
    end;
  end;
  if biSystemMenu in LIcons then Style := Style or WS_SYSMENU;
  if biHelp in LIcons then ExStyle := ExStyle or WS_EX_CONTEXTHELP;
end;

procedure TCustomForm.SetBorderIcons(Value: TBorderIcons);
var
  LStyle, LExStyle: Cardinal;
begin
  if FBorderIcons <> Value then
  begin
    FBorderIcons := Value;
    if HandleAllocated and (not (csDesigning in ComponentState) or
      ((csDesigning in ComponentState) and (Parent <> nil))) then
    begin
      if not ((FFormStyle = fsMDIChild) and (FWindowState = wsMaximized)) then
      begin
        LStyle := GetWindowLong(Handle, GWL_STYLE);
        LExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
        GetBorderIconStyles(LStyle, LExStyle);

        SetWindowLong(Handle, GWL_STYLE, LStyle);
        SetWindowLong(Handle, GWL_EXSTYLE, LExStyle);
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or
          SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
      end
      else
        RecreateWnd; // SetWindowLong doesn't remove icons for MDI children
    end;
  end;
end;

procedure TCustomForm.GetBorderStyles(var Style, ExStyle, ClassStyle: Cardinal);
var
  LBorderStyle: TFormBorderStyle;
begin
  // Clear existing border styles
  Style := Style and not (WS_POPUP or WS_CAPTION or WS_BORDER or WS_THICKFRAME or WS_DLGFRAME or DS_MODALFRAME);
  ExStyle := ExStyle and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW);
  ClassStyle := ClassStyle and not (CS_SAVEBITS or CS_BYTEALIGNWINDOW);

  // Account for special casses with MDI children
  LBorderStyle := FBorderStyle;
  if (FormStyle = fsMDIChild) and (LBorderStyle in [bsNone, bsDialog]) then
    LBorderStyle := bsSizeable;

  // Set new border styles
  case LBorderStyle of
    bsNone:
      if (Parent = nil) and (ParentWindow = 0) then
        Style := Style or WS_POPUP;
    bsSingle, bsToolWindow:
      Style := Style or (WS_CAPTION or WS_BORDER);
    bsSizeable, bsSizeToolWin:
      Style := Style or (WS_CAPTION or WS_THICKFRAME);
    bsDialog:
      begin
        if (csDesigning in ComponentState) and (Parent <> nil) then
          Style := Style or WS_CAPTION
        else
          Style := Style or WS_POPUP or WS_CAPTION;
        ExStyle := ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
        if not NewStyleControls then
          Style := Style or WS_DLGFRAME or DS_MODALFRAME;
        ClassStyle := ClassStyle or CS_DBLCLKS or CS_SAVEBITS or CS_BYTEALIGNWINDOW;
      end;
  end;
  if (LBorderStyle in [bsToolWindow, bsSizeToolWin]) and (FormStyle <> fsMDIChild) then
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
end;

procedure TCustomForm.SetBorderStyle(Value: TFormBorderStyle);
var
  LStyle, LExStyle, LClassStyle: Cardinal;
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    AutoScroll := AutoScroll and (FBorderStyle in [bsSizeable, bsSizeToolWin]);
    if HandleAllocated and (not (csDesigning in ComponentState) or
      ((csDesigning in ComponentState) and (Parent <> nil))) then
    begin
      LStyle := GetWindowLong(Handle, GWL_STYLE);
      LExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
      LClassStyle := GetClassLong(Handle, GCL_STYLE);

      GetBorderStyles(LStyle, LExStyle, LClassStyle);
      GetBorderIconStyles(LStyle, LExStyle);

      SetWindowLong(Handle, GWL_STYLE, LStyle);
      SetWindowLong(Handle, GWL_EXSTYLE, LExStyle);
      SetClassLong(Handle, GCL_STYLE, LClassStyle);

      if HandleAllocated then
      begin
        // Update icon on window frame
        if NewStyleControls then
          if BorderStyle <> bsDialog then
            SendMessage(Handle, WM_SETICON, 1, GetIconHandle)
          else
            SendMessage(Handle, WM_SETICON, 1, 0);

        // Reset system menu based on new border style
        GetSystemMenu(Handle, True);
        Perform(WM_NCCREATE, 0, 0);
      end;

      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or
        SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
      Invalidate;
    end;
  end;
end;

procedure TCustomForm.Dock(NewDockSite: TWinControl; ARect: TRect);
var
  PrevDockSite, PrevParent: TWinControl;
begin
  PrevParent := Parent;
  PrevDockSite := HostDockSite;
  inherited Dock(NewDockSite, ARect);
  if (Parent <> nil) and (Parent = PrevParent) and
    (PrevDockSite <> HostDockSite) then RecreateWnd;
end;

procedure TCustomForm.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  if (NewDockSite <> HostDockSite) and
    ((NewDockSite = nil) or Floating) then
    if NewDockSite = nil then
      FBorderStyle := FSavedBorderStyle
    else begin
      FSavedBorderStyle := BorderStyle;
      FBorderStyle := bsNone;
    end;
  inherited DoDock(NewDockSite, ARect);
end;

function TCustomForm.GetActiveMDIChild: TForm;
begin
  Result := nil;
  if (FormStyle = fsMDIForm) and (FClientHandle <> 0) then
    Result := TForm(FindControl(SendMessage(FClientHandle, WM_MDIGETACTIVE, 0,
      0)));
end;

function TCustomForm.GetMDIChildCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  if (FormStyle = fsMDIForm) and (FClientHandle <> 0) then
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].FormStyle = fsMDIChild then Inc(Result);
end;

function TCustomForm.GetMDIChildren(I: Integer): TForm;
var
  J: Integer;
begin
  if (FormStyle = fsMDIForm) and (FClientHandle <> 0) then
    for J := 0 to Screen.FormCount - 1 do
    begin
      Result := Screen.Forms[J];
      if Result.FormStyle = fsMDIChild then
      begin
        Dec(I);
        if I < 0 then Exit;
      end;
    end;
  Result := nil;
end;

function EnumMonitorsProc(hm: HMONITOR; dc: HDC; r: PRect; Data: Pointer): Boolean; stdcall;
var
  L: TList;
  M: TMonitor;
begin
  L := TList(Data);
  M := TMonitor.Create;
  M.FHandle := hm;
  M.FMonitorNum := L.Count;
  L.Add(M);
  Result := True;
end;

function TCustomForm.GetMonitor: TMonitor;
var
  HM: HMonitor;
  I: Integer;
begin
  Result := nil;
  HM := MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST);
  for I := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[I].Handle = HM then
    begin
      Result := Screen.Monitors[I];
      Exit;
    end;

  //if we get here, the Monitors array has changed, so we need to clear and reinitialize it
  Screen.GetMonitors;
  for I := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[I].Handle = HM then
    begin
      Result := Screen.Monitors[I];
      Exit;
    end;
end;

function TCustomForm.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TCustomForm.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

function TCustomForm.IsForm: Boolean;
begin
  Result := not IsControl;
end;

function TCustomForm.IsIconStored: Boolean;
begin
  Result := IsForm and (Icon.Handle <> 0);
end;

procedure TCustomForm.SetFormStyle(Value: TFormStyle);
const
  HWND_STYLE: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
var
  OldStyle: TFormStyle;
  LRecreate: Boolean;
begin
  if FFormStyle <> Value then
  begin
    if ((Value = fsNormal) and (FFormStyle = fsStayOnTop)) or
       ((Value = fsStayOnTop) and (FFormStyle = fsNormal)) then
    begin
      FFormStyle := Value;
      if not (csDesigning in ComponentState) and HandleAllocated then
        SetWindowPos(Handle, HWND_STYLE[FFormStyle = fsStayOnTop], 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end
    else
    begin
      if (Value = fsMDIChild) and (Position = poDesigned) then
        Position := poDefault;
      LRecreate := not (csDesigning in ComponentState);
      if LRecreate then
        UpdateRecreatingFlag(True);
      try
        if LRecreate then DestroyHandle;
        OldStyle := FFormStyle;
        FFormStyle := Value;
        if ((Value = fsMDIForm) or (OldStyle = fsMDIForm)) and not Ctl3d then
          Color := NormalColor;
        if LRecreate then UpdateControlState;
        if Value = fsMDIChild then Visible := True;
      finally
        if LRecreate then
          UpdateRecreatingFlag(False);
      end;
    end;
  end;
end;

procedure TCustomForm.RefreshMDIMenu;
var
  MenuHandle, WindowMenuHandle: HMenu;
  Redraw: Boolean;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
  begin
    MenuHandle := 0;
    if Menu <> nil then MenuHandle := Menu.Handle;
    WindowMenuHandle := 0;
    if WindowMenu <> nil then WindowMenuHandle := WindowMenu.Handle;
    Redraw := Windows.GetMenu(Handle) <> MenuHandle;
    SendMessage(ClientHandle, WM_MDISETMENU, MenuHandle, WindowMenuHandle);
    if Redraw then DrawMenuBar(Handle);
  end;
end;

procedure TCustomForm.SetObjectMenuItem(Value: TMenuItem);
begin
  FObjectMenuItem := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    Value.Enabled := False;
  end;
end;

procedure TCustomForm.SetWindowMenu(Value: TMenuItem);
begin
  if FWindowMenu <> Value then
  begin
    FWindowMenu := Value;
    if Value <> nil then Value.FreeNotification(Self);
    RefreshMDIMenu;
  end;
end;

procedure TCustomForm.SetMenu(Value: TMainMenu);
var
  I: Integer;
begin
  if Value <> nil then
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I].Menu = Value) and (Screen.Forms[I] <> Self) then
        raise EInvalidOperation.CreateFmt(sDuplicateMenus, [Value.Name]);
  if FMenu <> nil then FMenu.WindowHandle := 0;
  if (csDestroying in ComponentState) or
    ((Value <> nil) and (csDestroying in Value.ComponentState)) then
    Value := nil;
  FMenu := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if (Value <> nil) and ((csDesigning in ComponentState) or
   (BorderStyle <> bsDialog)) then
  begin
    if not (Menu.AutoMerge or (FormStyle = fsMDIChild)) or
      (csDesigning in ComponentState) then
    begin
      if HandleAllocated then
      begin
        if Windows.GetMenu(Handle) <> Menu.Handle then
          Windows.SetMenu(Handle, Menu.Handle);
        Value.WindowHandle := Handle;
      end;
    end
    else if FormStyle <> fsMDIChild then
      if HandleAllocated then Windows.SetMenu(Handle, 0);
  end
  else if HandleAllocated then Windows.SetMenu(Handle, 0);
  if Active then MergeMenu(True);
  RefreshMDIMenu;

  if (FDesigner <> nil) and (csDesigning in ComponentState) and
    (Parent <> nil) then
  begin
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or
      SWP_NOACTIVATE or SWP_NOZORDER or SWP_DRAWFRAME);
    Perform(WM_NCPAINT, 0, 0);
    Perform(WM_PAINT, 0, 0);
  end;
end;

function TCustomForm.GetPixelsPerInch: Integer;
begin
  Result := GetFPixelsPerInch(FPixelsPerInch);
  if Result = 0 then Result := Screen.PixelsPerInch;
end;

function TCustomForm.GetPopupChildren: TList;
begin
  if not Assigned(FPopupChildren) then
    FPopupChildren := TList.Create;
  Result := FPopupChildren;
end;

function TCustomForm.GetRecreateChildren: TList;
begin
  if not Assigned(FRecreateChildren) then
    FRecreateChildren := TList.Create;
  Result := FRecreateChildren;
end;

procedure TCustomForm.SetPixelsPerInch(Value: Integer);
begin
  if (Value <> GetPixelsPerInch) and ((Value = 0) or (Value >= 36))
     and (not (csLoading in ComponentState) or (GetFPixelsPerInch(FPixelsPerInch) <> 0)) then
    SetFPixelsPerInch(FPixelsPerInch, Value);
end;

procedure TCustomForm.SetPosition(Value: TPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if not (csDesigning in ComponentState) then RecreateWnd;
  end;
end;

procedure TCustomForm.SetPopupMode(Value: TPopupMode);
begin
  if Value <> FPopupMode then
  begin
    FPopupMode := Value;
    if (Value = pmAuto) and Assigned(FPopupParent) then
      PopupParent := nil;
    FInternalPopupParent := nil;
    FInternalPopupParentWnd := 0;
    if HandleAllocated and not (csDesigning in ComponentState) then
      RecreateWnd;
  end;
end;

procedure TCustomForm.SetPopupParent(Value: TCustomForm);
begin
  if Value <> FPopupParent then
  begin
    if FPopupParent <> nil then
      FPopupParent.RemoveFreeNotification(Self);
    FPopupParent := Value;
    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      FPopupMode := pmExplicit;
    end;
    FInternalPopupParent := nil;
    FInternalPopupParentWnd := 0;
    if HandleAllocated and not (csDesigning in ComponentState) then
      RecreateWnd;
  end;
end;

function TCustomForm.GetScaled: Boolean;
begin
  Result := GetFPixelsPerInch(FPixelsPerInch) <> 0;
end;

procedure TCustomForm.SetScaled(Value: Boolean);
begin
  if Value <> GetScaled then
  begin
    SetFPixelsPerInch(FPixelsPerInch, 0);
    if Value then
      SetFPixelsPerInch(FPixelsPerInch, Screen.PixelsPerInch);
  end;
end;

procedure TCustomForm.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if FCanvas <> nil then FCanvas.Brush.Color := Color;
end;

function TCustomForm.NormalColor: TColor;
begin
  Result := clWindow;
  if FormStyle = fsMDIForm then Result := clAppWorkSpace;
end;

procedure TCustomForm.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  if Ctl3D then
  begin
     if Color = NormalColor then Color := clBtnFace
  end
  else if Color = clBtnFace then Color := NormalColor;
end;

procedure TCustomForm.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FCanvas <> nil then FCanvas.Font := Font;
end;

procedure TCustomForm.CMMenuChanged(var Message: TMessage);
begin
  RefreshMDIMenu;
  SetMenu(FMenu);
end;

procedure TCustomForm.SetWindowState(Value: TWindowState);
const
  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
begin
  if FWindowState <> Value then
  begin
    FWindowState := Value;
    if not (csDesigning in ComponentState) and Showing then
      ShowWindow(Handle, ShowCommands[Value]);
  end;
end;

procedure TCustomForm.SetWindowToMonitor;
var
  AppMon, WinMon: HMONITOR;
  I, J: Integer;
  ALeft, ATop: Integer;
begin
    if (FDefaultMonitor <> dmDesktop) and (Application.MainForm <> nil) then
    begin
      AppMon := 0;
      if FDefaultMonitor = dmMainForm then
        AppMon := Application.MainForm.Monitor.Handle
      else if (FDefaultMonitor = dmActiveForm) and (Screen.ActiveCustomForm <> nil) then
        AppMon := Screen.ActiveCustomForm.Monitor.Handle
      else if FDefaultMonitor = dmPrimary then
        AppMon := Screen.PrimaryMonitor.Handle;
      WinMon := Monitor.Handle;
      for I := 0 to Screen.MonitorCount - 1 do
        if (Screen.Monitors[I].Handle = AppMon) then
          if (AppMon <> WinMon) then
            for J := 0 to Screen.MonitorCount - 1 do
              if (Screen.Monitors[J].Handle = WinMon) then
              begin
                if FPosition = poScreenCenter then
                  SetBounds(Screen.Monitors[I].Left + ((Screen.Monitors[I].Width - Width) div 2),
                    Screen.Monitors[I].Top + ((Screen.Monitors[I].Height - Height) div 2),
                     Width, Height)
                else
                if FPosition = poMainFormCenter then
                begin
                  SetBounds(Screen.Monitors[I].Left + ((Screen.Monitors[I].Width - Width) div 2),
                    Screen.Monitors[I].Top + ((Screen.Monitors[I].Height - Height) div 2),
                     Width, Height)
                end
                else
                begin
                  ALeft := Screen.Monitors[I].Left + Left - Screen.Monitors[J].Left;
                  if ALeft + Width > Screen.Monitors[I].Left + Screen.Monitors[I].Width then
                    ALeft := Screen.Monitors[I].Left + Screen.Monitors[I].Width - Width;
                  ATop := Screen.Monitors[I].Top + Top - Screen.Monitors[J].Top;
                  if ATop + Height > Screen.Monitors[I].Top + Screen.Monitors[I].Height then
                    ATop := Screen.Monitors[I].Top + Screen.Monitors[I].Height - Height;
                  SetBounds(ALeft, ATop, Width, Height);
                end;
              end;
    end;
end;

function GetNonToolWindowPopupParent(WndParent: HWND): HWND;
begin
  Result := GetParent(WndParent);
  while (Result <> 0) and (GetWindowLong(Result, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) do
    Result := GetParent(WndParent);
  if Result = 0 then
  begin
    if Assigned(Application.MainForm) and
       (GetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
      Result := Application.MainForm.Handle
    else
      Result := Application.Handle;
  end;
end;

procedure TCustomForm.CreateParams(var Params: TCreateParams);
var
  LParent: TCustomForm;
  CreateStyle: TFormBorderStyle;
  LPopupMode: TPopupMode;
begin
  inherited CreateParams(Params);
  InitAlphaBlending(Params);
  with Params do
  begin
    if (Parent = nil) and (ParentWindow = 0) then
    begin
      LParent := nil;
      if csDesigning in ComponentState then
        LPopupMode := pmExplicit
      else if (fsModal in FormState) and (FPopupMode = pmNone) then
        LPopupMode := pmAuto
      else if FormStyle = fsNormal then
        LPopupMode := FPopupMode
      else
        LPopupMode := pmNone;
      if (FInternalPopupParent = nil) and (FInternalPopupParentWnd = 0) then
        case LPopupMode of
          pmNone:
            begin
              if Application.MainFormOnTaskBar and Assigned(Application.MainForm) and
                 Application.MainForm.HandleAllocated then
                WndParent := Application.MainFormHandle
              else
                WndParent := Application.Handle;
            end;
          pmAuto:
            begin
              WndParent := Application.ActiveFormHandle;
              if (WndParent <> 0) and (IsIconic(WndParent) or not IsWindowVisible(WndParent) or
                not IsWindowEnabled(WndParent)) then
                WndParent := 0;
              if (WndParent <> 0) and
                 (GetWindowLong(WndParent, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) then
                WndParent := GetNonToolWindowPopupParent(WndParent);
              if (WndParent <> 0) and (Screen.ActiveForm <> nil) and
                (Screen.ActiveForm.WindowHandle = WndParent) then
                LParent := Screen.ActiveForm
              else if WndParent = 0 then
                if Assigned(Application.MainForm) and Application.MainFormOnTaskBar and
                   Application.MainForm.HandleAllocated then
                  WndParent := Application.MainFormHandle
                else
                  WndParent := Application.Handle;
            end;
          pmExplicit:
            begin
              if Assigned(FPopupParent) and not (csDesigning in ComponentState) then
              begin
                WndParent := FPopupParent.Handle;
                LParent := FPopupParent;
              end
              else
                WndParent := Application.MainFormHandle;
                if (WndParent <> 0) and (Application.MainForm <> nil) and
                  (Application.MainForm.WindowHandle = WndParent) then
                  LParent := Application.MainForm
                else if WndParent = 0 then
                  WndParent := Application.Handle;
            end;
        end
      else if FInternalPopupParentWnd <> 0 then
        WndParent := FInternalPopupParentWnd
      else
      begin
        LParent := FInternalPopupParent;
        WndParent := FInternalPopupParent.Handle;
      end;
      if Assigned(LParent) then
      begin
        if LParent.PopupChildren.IndexOf(Self) < 0 then
          LParent.PopupChildren.Add(Self);
        FreeNotification(LParent);
        FInternalPopupParent := LParent;
      end else if WndParent <> Application.Handle then
        FInternalPopupParentWnd := WndParent;
      Style := Style and not (WS_CHILD or WS_GROUP or WS_TABSTOP);
    end;
    WindowClass.style := CS_DBLCLKS;
    if (csDesigning in ComponentState) and (Parent = nil) then
      Style := Style or (WS_CAPTION or WS_THICKFRAME or WS_MINIMIZEBOX or
        WS_MAXIMIZEBOX or WS_SYSMENU)
    else
    begin
      if (not(csDesigning in ComponentState) or (Parent = nil)) and (FPosition in [poDefault, poDefaultPosOnly]) then
      begin
        X := Integer(CW_USEDEFAULT);
        Y := Integer(CW_USEDEFAULT);
      end;

      GetBorderStyles(Style, ExStyle, WindowClass.Style);
      CreateStyle := FBorderStyle;
      if (FormStyle = fsMDIChild) and (CreateStyle in [bsNone, bsDialog]) then
        CreateStyle := bsSizeable;
      if (CreateStyle in [bsSizeable, bsSizeToolWin]) and
         (not(csDesigning in ComponentState) or (Parent = nil)) and
         (FPosition in [poDefault, poDefaultSizeOnly]) then
        begin
          Width := Integer(CW_USEDEFAULT);
          Height := Integer(CW_USEDEFAULT);
        end;
      if CreateStyle in [bsSingle, bsSizeable, bsNone] then
      begin
        if not (csDesigning in ComponentState) then
          if FWindowState = wsMinimized then Style := Style or WS_MINIMIZE else
            if FWindowState = wsMaximized then Style := Style or WS_MAXIMIZE;
      end
      else
        FWindowState := wsNormal;
      if csInline in ComponentState then
        Style := Style and not WS_CAPTION;
      if FormStyle = fsMDIChild then WindowClass.lpfnWndProc := @DefMDIChildProc;
      GetBorderIconStyles(Style, ExStyle);
      if ((csDesigning in ComponentState) or (csRecreating in ControlState)) and
         (FormStyle <> fsMDIChild) and Application.MainFormOnTaskBar and
         (Self = Application.MainForm) then
        ExStyle := ExStyle or WS_EX_APPWINDOW;
    end;
  end;
end;

procedure TCustomForm.CreateWnd;
var
  I: Integer;
  ClientCreateStruct: TClientCreateStruct;
begin
  inherited CreateWnd;
  if NewStyleControls then
    if BorderStyle <> bsDialog then
      SendMessage(Handle, WM_SETICON, 1, GetIconHandle) else
      SendMessage(Handle, WM_SETICON, 1, 0);
  if not (csDesigning in ComponentState) then
    case FormStyle of
      fsMDIForm:
        begin
          with ClientCreateStruct do
          begin
            idFirstChild := $FF00;
            hWindowMenu := 0;
            if FWindowMenu <> nil then hWindowMenu := FWindowMenu.Handle;
          end;
          FClientHandle := Windows.CreateWindowEx(WS_EX_CLIENTEDGE, 'MDICLIENT',
            nil, WS_CHILD or WS_VISIBLE or WS_GROUP or WS_TABSTOP or
            WS_CLIPCHILDREN or WS_HSCROLL or WS_VSCROLL or WS_CLIPSIBLINGS or
            MDIS_ALLCHILDSTYLES, 0, 0, ClientWidth, ClientHeight, Handle, 0,
            HInstance, @ClientCreateStruct);
{$IFDEF LINUX}
          FClientInstance := WinUtils.MakeObjectInstance(ClientWndProc);
{$ENDIF}
{$IFDEF MSWINDOWS}
          FClientInstance := Classes.MakeObjectInstance(ClientWndProc);
{$ENDIF}
          FDefClientProc := Pointer(GetWindowLong(FClientHandle, GWL_WNDPROC));
          SetWindowLong(FClientHandle, GWL_WNDPROC, Longint(FClientInstance));
        end;
      fsStayOnTop:
        SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
          SWP_NOSIZE or SWP_NOACTIVATE);
    end;
  if Assigned(FRecreateChildren) then
  begin
    for I := 0 to FRecreateChildren.Count - 1 do
      TCustomForm(FRecreateChildren[I]).UpdateControlState;
    FRecreateChildren.Clear;
  end;
  for I := Low(FPopupWnds) to High(FPopupWnds) do
    SendMessage(FPopupWnds[I].ControlWnd, CM_CREATEPOPUP, FPopupWnds[I].ID, WindowHandle);
  SetLength(FPopupWnds, 0);
  if not (csLoading in ComponentState) and GlassFrame.FrameExtended then
    UpdateGlassFrame(nil);
end;

procedure TCustomForm.CreateWindowHandle(const Params: TCreateParams);
var
  CreateStruct: TMDICreateStruct;
  NewParams: TCreateParams;
begin
  if (FormStyle = fsMDIChild) and not (csDesigning in ComponentState) then
  begin
    if (Application.MainForm = nil) or
      (Application.MainForm.ClientHandle = 0) then
      raise EInvalidOperation.Create(SNoMDIForm);
    with CreateStruct do
    begin
      szClass := Params.WinClassName;
      szTitle := Params.Caption;
      hOwner := HInstance;
      X := Params.X;
      Y := Params.Y;
      cX := Params.Width;
      cY := Params.Height;
      style := Params.Style;
      lParam := Longint(Params.Param);
    end;
    WindowHandle := SendMessage(Application.MainForm.ClientHandle,
      WM_MDICREATE, 0, Longint(@CreateStruct));
    Include(FFormState, fsCreatedMDIChild);
  end else
  begin
    NewParams := Params;
    NewParams.ExStyle := NewParams.ExStyle and not WS_EX_LAYERED;
    inherited CreateWindowHandle(NewParams);
    Exclude(FFormState, fsCreatedMDIChild);
  end;
  SetLayeredAttribs;
end;

type
  PDestroyPopupData = ^TDestroyPopupData;
  TDestroyPopupData = record
    Owner: HWND;
    Recreating: Boolean;
  end;

function DestroyPopupWindow(Window: HWND; Data: PDestroyPopupData): BOOL; stdcall;
begin
  if (Window <> Data^.Owner) and (FindControl(Window) = nil) and
    (GetWindow(Window, GW_OWNER) = Data^.Owner) then
    SendMessage(Window, CM_DESTROYHANDLE, Byte(Data^.Recreating), 0);
  Result := True;
end;

procedure TCustomForm.DestroyHandle;
var
  I: Integer;
  LData: TDestroyPopupData;
begin
  if WindowHandle <> 0 then
  begin
    if Assigned(FPopupChildren) then
    begin
      for I := 0 to FPopupChildren.Count - 1 do
      begin
        if (csRecreating in ControlState) then
          TCustomForm(FPopupChildren[I]).UpdateRecreatingFlag(True);
        try
          TCustomForm(FPopupChildren[I]).DestroyHandle;
        finally
          if (csRecreating in ControlState) then
            TCustomForm(FPopupChildren[I]).UpdateRecreatingFlag(False);
        end;
      end;
    end;
    LData.Owner := WindowHandle;
    LData.Recreating := csRecreating in ControlState;
    EnumThreadWindows(GetCurrentThreadID, @DestroyPopupWindow, Integer(@LData));
  end;
  inherited DestroyHandle;
end;

procedure TCustomForm.DestroyWindowHandle;
begin
  if fsCreatedMDIChild in FFormState then
    SendMessage(Application.MainForm.ClientHandle, WM_MDIDESTROY, Handle, 0)
  else
    inherited DestroyWindowHandle;
  FClientHandle := 0;
end;

procedure TCustomForm.DefaultHandler(var Message);
begin
  if ClientHandle <> 0 then
    with TMessage(Message) do
      if Msg = WM_SIZE then
        Result := DefWindowProc(Handle, Msg, wParam, lParam) else
        Result := DefFrameProc(Handle, ClientHandle, Msg, wParam, lParam)
  else
    inherited DefaultHandler(Message)
end;

procedure TCustomForm.SetActiveControl(Control: TWinControl);
begin
  if FActiveControl <> Control then
  begin
    if not ((Control = nil) or (csDesigning in ComponentState) or ((Control <> Self) and
      (GetRealParentForm(Control) = Self) and ((csLoading in ComponentState) or
        Control.CanFocus))) then
      raise EInvalidOperation.Create(SCannotFocus);
    FActiveControl := Control;
    if not (csLoading in ComponentState) then
    begin
      if FActive then SetWindowFocus;
      ActiveChanged;
    end;
  end;
end;

procedure TCustomForm.SetActiveOleControl(Control: TWinControl);
begin
  if FActiveOleControl <> Control then
  begin
    FActiveOleControl := Control;
    if Control <> nil then Control.FreeNotification(Self);
  end;
end;

procedure TCustomForm.DefocusControl(Control: TWinControl; Removing: Boolean);
begin
  if Removing and Control.ContainsControl(FFocusedControl) then
    FFocusedControl := Control.Parent;
  if Control.ContainsControl(FActiveControl) then SetActiveControl(nil);
end;

procedure TCustomForm.FocusControl(Control: TWinControl);
var
  WasActive: Boolean;
begin
  WasActive := FActive;
  SetActiveControl(Control);
  if not WasActive then SetFocus;
end;

function TCustomForm.SetFocusedControl(Control: TWinControl): Boolean;
var
  FocusHandle: HWnd;
  TempControl: TWinControl;
begin
  Result := False;
  Inc(FocusCount);
  if FDesigner = nil then
    if Control <> Self then
      FActiveControl := Control else
      FActiveControl := nil;
  Screen.FActiveControl := Control;
  Screen.FActiveCustomForm := Self;
  Screen.FCustomForms.Remove(Self);
  Screen.FCustomForms.Insert(0, Self);
  if Self is TForm then
  begin
    Screen.FActiveForm := TForm(Self);
    Screen.FForms.Remove(Self);
    Screen.FForms.Insert(0, Self);
  end
  else Screen.FActiveForm := nil;
  if not (csFocusing in Control.ControlState) then
  begin
    Control.ControlState := Control.ControlState + [csFocusing];
    try
      if Screen.FFocusedForm <> Self then
      begin
        if Screen.FFocusedForm <> nil then
        begin
          FocusHandle := Screen.FFocusedForm.Handle;
          Screen.FFocusedForm := nil;
          if not SendFocusMessage(FocusHandle, CM_DEACTIVATE) then Exit;
        end;
        Screen.FFocusedForm := Self;
        if not SendFocusMessage(Handle, CM_ACTIVATE) then Exit;
      end;
      if FFocusedControl = nil then FFocusedControl := Self;
      if FFocusedControl <> Control then
      begin
        while (FFocusedControl <> nil) and not
          FFocusedControl.ContainsControl(Control) do
        begin
          FocusHandle := FFocusedControl.Handle;
          FFocusedControl := FFocusedControl.Parent;
          if not SendFocusMessage(FocusHandle, CM_EXIT) then Exit;
        end;
        while FFocusedControl <> Control do
        begin
          TempControl := Control;
          while TempControl.Parent <> FFocusedControl do
            TempControl := TempControl.Parent;
          FFocusedControl := TempControl;
          if not SendFocusMessage(TempControl.Handle, CM_ENTER) then Exit;
        end;
        TempControl := Control.Parent;
        if not (csDesigning in Control.ComponentState) then
          while TempControl <> nil do
          begin
            if TempControl is TScrollingWinControl then
              TScrollingWinControl(TempControl).AutoScrollInView(Control);
            TempControl := TempControl.Parent;
          end;
        Perform(CM_FOCUSCHANGED, 0, Longint(Control));
        if (FActiveOleControl <> nil) and (FActiveOleControl <> Control) then
          FActiveOleControl.Perform(CM_UIDEACTIVATE, 0, 0);
      end;
    finally
      Control.ControlState := Control.ControlState - [csFocusing];
    end;
    Screen.UpdateLastActive;
    Result := True;
  end;
end;

procedure TCustomForm.ActiveChanged;
begin
end;

procedure TCustomForm.SetWindowFocus;
var
  FocusControl: TWinControl;
begin
  if (FActiveControl <> nil) and (FDesigner = nil) then
    FocusControl := FActiveControl
  else if (Parent <> nil) and (FDesigner = nil) then
  begin
    FocusControl := FindNextControl(nil, True, True, False);
    if FocusControl = nil then
      FocusControl := Self;
  end else
    FocusControl := Self;
  Windows.SetFocus(FocusControl.Handle);
  if GetFocus = FocusControl.Handle then
    FocusControl.Perform(CM_UIACTIVATE, 0, 0);
end;

procedure TCustomForm.SetActive(Value: Boolean);
begin
  FActive := Value;
  if FActiveOleControl <> nil then
    FActiveOleControl.Perform(CM_DOCWINDOWACTIVATE, Ord(Value), 0);
  if Value then
  begin
    if (ActiveControl = nil) and not (csDesigning in ComponentState) then
      ActiveControl := FindNextControl(nil, True, True, False);
    MergeMenu(True);
    SetWindowFocus;
  end;
end;

procedure TCustomForm.SendCancelMode(Sender: TControl);
begin
  if Active and (ActiveControl <> nil) then
    ActiveControl.Perform(CM_CANCELMODE, 0, Longint(Sender));
  if (FormStyle = fsMDIForm) and (ActiveMDIChild <> nil) then
    ActiveMDIChild.SendCancelMode(Sender);
end;

procedure TCustomForm.MergeMenu(MergeState: Boolean);
var
  AMergeMenu: TMainMenu;
begin
  if not (fsModal in FFormState) and
    (Application.MainForm <> nil) and
    (Application.MainForm.Menu <> nil) and
    (Application.MainForm <> Self) and
    ((FormStyle = fsMDIChild) or (Application.MainForm.FormStyle <> fsMDIForm)) then
  begin
    AMergeMenu := nil;
    if not (csDesigning in ComponentState) and (Menu <> nil) and
      (Menu.AutoMerge or (FormStyle = fsMDIChild)) then AMergeMenu := Menu;
    with Application.MainForm.Menu do
      if MergeState then Merge(AMergeMenu) else Unmerge(AMergeMenu);
  end;
end;

procedure DoNestedActivation(Msg: Cardinal; Control: TWinControl; Form: TCustomForm);
begin
  if Control = nil then Exit;
  { Find the closest parent which is a form }
  while (Control.Parent <> nil) and not (Control is TCustomForm) do
    Control := Control.Parent;
  if Assigned(Control) and (Control <> Form) then
    SendMessage(Control.Handle, Msg, 0, 0)
end;

procedure TCustomForm.Activate;
begin
  DoNestedActivation(CM_ACTIVATE, ActiveControl, Self);
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TCustomForm.Deactivate;
begin
  DoNestedActivation(CM_DEACTIVATE, ActiveControl, Self);
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TCustomForm.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TCustomForm.GetIconHandle: HICON;
begin
  Result := FIcon.Handle;
  if Result = 0 then Result := Application.GetIconHandle;
end;

procedure TCustomForm.PaintWindow(DC: HDC);
var
  LClientRect: TRect;
  SaveIndex: Integer;
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      SaveIndex := SaveDC(DC);
      try
        with GlassFrame do
        begin
          if FrameExtended or ((FDesigner <> nil) and Enabled) then
          begin
            LClientRect := ClientRect;
            if not SheetOfGlass and not GetRefreshGlassFrame(FPixelsPerInch) then
              ExcludeClipRect(DC, Left, Top, LClientRect.Right - Right, LClientRect.Bottom - Bottom)
            else
              SetRefreshGlassFrame(FPixelsPerInch, False);

            if FDesigner = nil then
              FillRect(FCanvas.Handle, LClientRect, GetStockObject(BLACK_BRUSH))
            else
            begin
              FCanvas.Brush.Color := clActiveCaption;
              FCanvas.Brush.Style := bsBDiagonal;
              SetBkColor(FCanvas.Handle, ColorToRGB(Color));
              FCanvas.FillRect(LClientRect);
            end;
          end;
        end;
      finally
        RestoreDC(DC, SaveIndex);
      end;
      if FDesigner <> nil then
        FDesigner.PaintGrid
      else
        Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

function TCustomForm.PaletteChanged(Foreground: Boolean): Boolean;
var
  I: Integer;
  Active, Child: TForm;
begin
  Result := False;
  Active := ActiveMDIChild;
  if Assigned(Active) then
    Result := Active.PaletteChanged(Foreground);
  for I := 0 to MDIChildCount-1 do
  begin
    if Foreground and Result then Exit;
    Child := MDIChildren[I];
    if Active = Child then Continue;
    Result := Child.PaletteChanged(Foreground) or Result;
  end;
  if Foreground and Result then Exit;
  Result := inherited PaletteChanged(Foreground);
end;

procedure TCustomForm.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  if not IsIconic(Handle) then
  begin
    ControlState := ControlState + [csCustomPaint];
    inherited;
    ControlState := ControlState - [csCustomPaint];
  end
  else
  begin
    DC := BeginPaint(Handle, PS);
    DrawIcon(DC, 0, 0, GetIconHandle);
    EndPaint(Handle, PS);
  end;
end;

procedure TCustomForm.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if (FDesigner <> nil) and (Parent <> nil) then FDesigner.PaintMenu;
end;

procedure TCustomForm.WMIconEraseBkgnd(var Message: TWMIconEraseBkgnd);
begin
  if FormStyle = fsMDIChild then
  if (FormStyle = fsMDIChild) and not (csDesigning in ComponentState) then
    FillRect(Message.DC, ClientRect, Application.MainForm.Brush.Handle)
  else inherited;
end;

procedure TCustomForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not IsIconic(Handle) then inherited else
  begin
    Message.Msg := WM_ICONERASEBKGND;
    DefaultHandler(Message);
  end;
end;

procedure TCustomForm.WMQueryDragIcon(var Message: TWMQueryDragIcon);
begin
  Message.Result := GetIconHandle;
end;

procedure TCustomForm.WMNCCreate(var Message: TWMNCCreate);

  procedure ModifySystemMenu;
  var
    SysMenu: HMENU;
  begin
    if (FBorderStyle <> bsNone) and (biSystemMenu in FBorderIcons) and
      (FormStyle <> fsMDIChild) then
    begin
      { Modify the system menu to look more like it's s'pose to }
      SysMenu := GetSystemMenu(Handle, False);
      if FBorderStyle = bsDialog then
      begin
        { Make the system menu look like a dialog which has only
          Move and Close }
        DeleteMenu(SysMenu, SC_TASKLIST, MF_BYCOMMAND);
        DeleteMenu(SysMenu, 7, MF_BYPOSITION);
        DeleteMenu(SysMenu, 5, MF_BYPOSITION);
        DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
        DeleteMenu(SysMenu, SC_MINIMIZE, MF_BYCOMMAND);
        DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
        DeleteMenu(SysMenu, SC_RESTORE, MF_BYCOMMAND);
      end else
      begin
        { Else just disable the Minimize and Maximize items if the
          corresponding FBorderIcon is not present }
        if not (biMinimize in FBorderIcons) then
          EnableMenuItem(SysMenu, SC_MINIMIZE, MF_BYCOMMAND or MF_GRAYED);
        if not (biMaximize in FBorderIcons) then
          EnableMenuItem(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND or MF_GRAYED);
      end;
    end;
  end;

begin
  inherited;
  SetMenu(FMenu);
  if not (csDesigning in ComponentState) then ModifySystemMenu;
end;

procedure TCustomForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if (csDesigning in ComponentState) and (Parent <> nil) then
    DefaultHandler(Message)
  else
    inherited;
end;

procedure TCustomForm.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  if (Message.HitTest = HTCAPTION) and (DragKind = dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(Handle) then
  begin
    { Activate window since we override WM_NCLBUTTON behavior }
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or
      SWP_NOSIZE);
    PostMessage(Handle, WM_NCLBUTTONUP, TMessage(Message).WParam,
      TMessage(Message).LParam);
    if Active then BeginDrag(not Floating);
  end
  else
    inherited;
end;

procedure TCustomForm.WMDestroy(var Message: TWMDestroy);
var
  I: Integer;
  OwnerWnd: HWND;
  PopupFormInfo: TPopupFormInfo;
begin
  if Assigned(FPopupChildren) then
  begin
    for I := 0 to FPopupChildren.Count - 1 do
      if TCustomForm(FPopupChildren[I]).HandleAllocated then
        GetRecreateChildren.Add(FPopupChildren[I]);
  end;
  if FInternalPopupParent <> nil then
  begin
    if (FInternalPopupParent.GetRecreateChildren.IndexOf(Self) < 0) and
      (FInternalPopupParent.GetPopupChildren.IndexOf(Self) > -1) then
      FInternalPopupParent.GetRecreateChildren.Add(Self);
  end else
  begin
    OwnerWnd := GetOwnerWindow;
    if (OwnerWnd <> 0) and (OwnerWnd <> Application.Handle) then
    begin
      PopupFormInfo.PopupID := Application.AddPopupForm(Self);
      PopupFormInfo.PopupWnd := WindowHandle;
      PopupFormInfo.IsPopup := FInternalPopupParentWnd <> 0;
      SendMessage(OwnerWnd, CM_POPUPHWNDDESTROY, Integer(@PopupFormInfo), Longint(Application.PopupControlWnd));
    end;
    FInternalPopupParentWnd := 0;
  end;
  if NewStyleControls then SendMessage(Handle, WM_SETICON, 1, 0);
  if (FMenu <> nil) and (FormStyle <> fsMDIChild) then
  begin
    Windows.SetMenu(Handle, 0);
    FMenu.WindowHandle := 0;
  end;
  inherited;
end;

procedure TCustomForm.WMCommand(var Message: TWMCommand);
begin
  with Message do
    if (Ctl <> 0) or (Menu = nil) or not Menu.DispatchCommand(ItemID) then
      inherited;
end;

procedure TCustomForm.WMInitMenuPopup(var Message: TWMInitMenuPopup);
begin
  if FMenu <> nil then FMenu.DispatchPopup(Message.MenuPopup);
end;

procedure TCustomForm.WMMenuChar(var Message: TWMMenuChar);
begin
  if (Menu <> nil) then
  begin
    Menu.ProcessMenuChar(Message);
    if Message.Result = MNC_IGNORE then
      // if we don't know what to do with it, give the default handler a try
      // Specifically, this covers odd MDI system hotkeys, like Alt+Minus
      inherited;
  end
  else
    inherited;
end;

procedure TCustomForm.WMMenuSelect(var Message: TWMMenuSelect);
var
  MenuItem: TMenuItem;
  ID: Integer;
  FindKind: TFindItemKind;
begin
  if FMenu <> nil then
    with Message do
    begin
      MenuItem := nil;
      if (MenuFlag <> $FFFF) or (IDItem <> 0) then
      begin
        FindKind := fkCommand;
        ID := IDItem;
        if MenuFlag and MF_POPUP <> 0 then
        begin
          FindKind := fkHandle;
          ID := GetSubMenu(Menu, ID);
        end;
        MenuItem := FMenu.FindItem(ID, FindKind);
      end;
      if MenuItem <> nil then
        Application.Hint := GetLongHint(MenuItem.Hint) else
        Application.Hint := '';
    end;
end;

procedure TCustomForm.WMActivate(var Message: TWMActivate);
begin
  if not (GetWindowLong(Handle, GWL_STYLE) and WS_CHILD = WS_CHILD) and
     (FormStyle <> fsMDIForm) or (csDesigning in ComponentState) then
    SetActive(Message.Active <> WA_INACTIVE);
end;

procedure TCustomForm.Resizing(State: TWindowState);
begin
  if not (csDesigning in ComponentState) and not (csRecreating in ControlState) then
    FWindowState := State;
  if State <> wsMinimized then
    RequestAlign;
  if FOleForm <> nil then FOleForm.OnResize;
end;

procedure TCustomForm.WMClose(var Message: TWMClose);
begin
  Close;
end;

procedure TCustomForm.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  Message.Result := Integer(CloseQuery);
end;

procedure TCustomForm.CMAppSysCommand(var Message: TMessage);
type
  PWMSysCommand = ^TWMSysCommand;
begin
  Message.Result := 0;
  if (csDesigning in ComponentState) or (FormStyle = fsMDIChild) or
   (Menu = nil) or Menu.AutoMerge then
    with PWMSysCommand(Message.lParam)^ do
    begin
      SendCancelMode(nil);
      if SendAppMessage(CM_APPSYSCOMMAND, CmdType, Key) <> 0 then
        Message.Result := 1;;
    end;
end;

procedure TCustomForm.WMSysCommand(var Message: TWMSysCommand);
begin
  with Message do
  begin
    if (CmdType and $FFF0 = SC_MINIMIZE) and (Application.MainForm = Self) then
      Application.WndProc(TMessage(Message))
    else if (CmdType and $FFF0 <> SC_MOVE) or (csDesigning in ComponentState) or
      (Align = alNone) or (WindowState = wsMinimized) then
      inherited;
    if ((CmdType and $FFF0 = SC_MINIMIZE) or (CmdType and $FFF0 = SC_RESTORE)) and
      not (csDesigning in ComponentState) and (Align <> alNone) then
      RequestAlign;
  end;
end;

procedure TCustomForm.WMShowWindow(var Message: TWMShowWindow);
const
  ShowCommands: array[saRestore..saMaximize] of Integer =
    (SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
begin
  with Message do
    case Status of
      SW_PARENTCLOSING:
        begin
          if IsIconic(Handle) then FShowAction := saMinimize else
            if IsZoomed(Handle) then FShowAction := saMaximize else
              FShowAction := saRestore;
          inherited;
        end;
      SW_PARENTOPENING:
        if FShowAction <> saIgnore then
        begin
          ShowWindow(Handle, ShowCommands[FShowAction]);
          FShowAction := saIgnore;
          // This occurs when MainFormOnTaskbar is False
          // and the ShowDesktop button is clicked.
          if Self = Application.MainForm then
            AppIconic := False;
        end;
    else
      inherited;
    end;
end;

procedure TCustomForm.WMMDIActivate(var Message: TWMMDIActivate);
var
  IsActive: Boolean;
begin
  inherited;
  if FormStyle = fsMDIChild then
  begin
    IsActive := Message.ActiveWnd = Handle;
    SetActive(IsActive);
    if IsActive and (csPalette in Application.MainForm.ControlState) then
      Application.MainForm.PaletteChanged(True);
  end;
end;

procedure TCustomForm.WMNextDlgCtl(var Message: TWMNextDlgCtl);
begin
  with Message do
    if Handle then
      Windows.SetFocus(Message.CtlFocus) else
      SelectNext(FActiveControl, not BOOL(CtlFocus), True);
end;

procedure TCustomForm.WMEnterMenuLoop(var Message: TMessage);
begin
  SendCancelMode(nil);
  inherited;
end;

procedure TCustomForm.WMHelp(var Message: TWMHelp);

  function GetMenuHelpContext(Menu: TMenu): Integer;
  begin
    Result := 0;
    if Menu = nil then Exit;
    Result := Menu.GetHelpContext(Message.HelpInfo.iCtrlID, True);
    if Result = 0 then
      Result := Menu.GetHelpContext(Message.HelpInfo.hItemHandle, False);
  end;

  function ControlHasHelp(const Control: TWinControl) : Boolean;
  begin
    Result := False;
    if (Control.HelpType = htContext) and (Control.HelpContext <> 0)
      then Result := True
    else if (Control.HelpType = htKeyword) and (Control.HelpKeyword <> '') then
      Result := True;
  end;

  procedure GetHelpInfo(const Control: TWinControl; var HType: THelpType; var ContextID: Integer; var Keyword: String);
  begin
    if Control.HelpType = htContext then
    begin
      HType := htContext;
      ContextID := Control.HelpContext;
    end else
    begin
      HType := htKeyword;
      Keyword := Control.HelpKeyword;
    end;
  end;

var
  Control: TWinControl;
  ContextID: Integer;
  HType: THelpType;
  Keyword: String;
  Pt: TSmallPoint;
begin
  if csDesigning in ComponentState then
    Exit
  else
  begin
    with Message.HelpInfo^ do
    begin
      if iContextType = HELPINFO_WINDOW then
      begin
        Control := FindControl(hItemHandle);
        while (Control <> nil) and ( not ControlHasHelp(Control)) do
          Control := Control.Parent;
        if Control = nil then Exit;
        GetHelpInfo(Control, HType, ContextID, Keyword);
        Pt := PointToSmallPoint(Control.ClientToScreen(Point(0, 0)));
      end
      else  { Message.HelpInfo.iContextType = HELPINFO_MENUITEM }
      begin
        HType := htContext;
        ContextID := GetMenuHelpContext(FMenu);
        if ContextID = 0 then
          ContextID := GetMenuHelpContext(PopupMenu);
        Pt := PointToSmallPoint(ClientToScreen(Point(0,0)));
      end;
    end;
    if (biHelp in BorderIcons) and (HType = htContext) then
    begin
      Application.HelpCommand(HELP_SETPOPUP_POS, Longint(Pt));
      Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID);
    end
    else
    begin
      if HType = htContext then
        Application.HelpContext(ContextID)
      else if HType = htKeyword then
        Application.HelpKeyword(Keyword);
    end;
  end;
end;

procedure TCustomForm.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  if not (csReading in ComponentState) and FSizeChanging then
    with Message.MinMaxInfo^, Constraints do
    begin
      with ptMinTrackSize do
      begin
        if MinWidth > 0 then X := MinWidth;
        if MinHeight > 0 then Y := MinHeight;
      end;
      with ptMaxTrackSize do
      begin
        if MaxWidth > 0 then X := MaxWidth;
        if MaxHeight > 0 then Y := MaxHeight;
      end;
      ConstrainedResize(ptMinTrackSize.X, ptMinTrackSize.Y, ptMaxTrackSize.X,
        ptMaxTrackSize.Y);
    end;
  inherited;
end;

procedure TCustomForm.WMWindowPosChanging(var Message: TWMWindowPosChanging);

  procedure HandleEdge(var Edge: Integer; SnapToEdge: Integer;
    SnapDistance: Integer = 0);
  begin
    if (Abs(Edge + SnapDistance - SnapToEdge) < FSnapBuffer) then
      Edge := SnapToEdge - SnapDistance;
  end;

var
  DeltaY, DeltaX: Integer;
  LClientRect, RefreshRect: TRect;
begin
  if FScreenSnap and ((Parent = nil) or not (csDesigning in ComponentState)) and
    ((Message.WindowPos^.X <> 0) or (Message.WindowPos^.Y <> 0)) and
    ((Message.WindowPos^.cx = Width) and (Message.WindowPos^.cy = Height)) then
    with Message.WindowPos^, Monitor.WorkareaRect do
    begin
      HandleEdge(x, Left, Monitor.WorkareaRect.Left);
      HandleEdge(y, Top, Monitor.WorkareaRect.Top);
      HandleEdge(x, Right, Width);
      HandleEdge(y, Bottom, Height);
    end;
  inherited;
  with GlassFrame do
    if FrameExtended and not SheetOfGlass and Visible then
    begin
      if (Message.WindowPos^.cx <> 0) or (Message.WindowPos^.cy <> 0) then
      begin
        DeltaX := Message.WindowPos^.cx - Width;
        DeltaY := Message.WindowPos^.cy - Height;
        if (DeltaX <> 0) or (DeltaY <> 0) then
        begin
          LClientRect := ClientRect;
          RefreshRect := Rect(Left, Top, LClientRect.Right - Right, LClientRect.Bottom - Bottom);

          if (RefreshRect.Bottom <= Top) and (DeltaY > 0) then
          begin
            RefreshRect.Bottom := RefreshRect.Top - DeltaY;
            RefreshRect.Top := Bottom;
            SetRefreshGlassFrame(FPixelsPerInch, True);
          end
          else
          begin
            if DeltaY > 0 then
              Inc(RefreshRect.Bottom, DeltaY);
            if RefreshRect.Bottom < RefreshRect.Top then
                RefreshRect.Bottom := RefreshRect.Top;
          end;

          if (RefreshRect.Right < Left) and (DeltaX > 0) then
          begin
            RefreshRect.Left := RefreshRect.Right - DeltaX;
            RefreshRect.Right := Left;
            SetRefreshGlassFrame(FPixelsPerInch, True);
          end
          else
          begin
            if DeltaX > 0 then
              Inc(RefreshRect.Right, DeltaX);
            if RefreshRect.Right < RefreshRect.Left then
              RefreshRect.Right := RefreshRect.Left;
          end;

          InvalidateRect(Handle, @RefreshRect, False);
        end;
      end;
    end;
end;

procedure TCustomForm.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  NCCalcSizeParams: PNCCalcSizeParams;
begin
  inherited;

  if (csDesigning in ComponentState) and (Parent <> nil) and
     (Menu <> nil) and (Menu.Items.Count > 0) then
  begin
    NCCalcSizeParams := Message.CalcSize_Params;
    Inc(NCCalcSizeParams.rgrc[0].Top, GetSystemMetrics(SM_CYMENU));
  end;
end;

procedure TCustomForm.CMActivate(var Message: TCMActivate);
begin
  if not (csReading in ComponentState) then
    Activate else
    Include(FFormState, fsActivated);
end;

procedure TCustomForm.CMDeactivate(var Message: TCMDeactivate);
begin
  if not (csReading in ComponentState) then
    Deactivate else
    Exclude(FFormState, fsActivated);
end;

procedure TCustomForm.CMDialogKey(var Message: TCMDialogKey);
begin
  if GetKeyState(VK_MENU) >= 0 then
    with Message do
      case CharCode of
        VK_TAB:
          if GetKeyState(VK_CONTROL) >= 0 then
          begin
            SelectNext(FActiveControl, GetKeyState(VK_SHIFT) >= 0, True);
            Result := 1;
            Exit;
          end;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          begin
            if FActiveControl <> nil then
            begin
              TForm(FActiveControl.Parent).SelectNext(FActiveControl,
                (CharCode = VK_RIGHT) or (CharCode = VK_DOWN), False);
              Result := 1;
            end;
            Exit;
          end;
      end;
  inherited;
end;

procedure TCustomForm.CMShowingChanged(var Message: TMessage);
const
  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
var
  X, Y: Integer;
  NewActiveWindow: HWnd;
  CenterForm: TCustomForm;
begin
  if not (csDesigning in ComponentState) and (fsShowing in FFormState) then
    raise EInvalidOperation.Create(SVisibleChanged);
  Application.UpdateVisible;
  Include(FFormState, fsShowing);
  try
    if not (csDesigning in ComponentState) then
    begin
      if Showing then
      begin
        try
          DoShow;
        except
          Application.HandleException(Self);
        end;
        if (FPosition = poScreenCenter) or
           ((FPosition = poMainFormCenter) and (FormStyle = fsMDIChild)) then
        begin
          if FormStyle = fsMDIChild then
          begin
            X := (Application.MainForm.ClientWidth - Width) div 2;
            Y := (Application.MainForm.ClientHeight - Height) div 2;
          end else
          begin
            X := (Screen.Width - Width) div 2;
            Y := (Screen.Height - Height) div 2;
          end;
          if X < Screen.DesktopLeft then
            X := Screen.DesktopLeft;
          if Y < Screen.DesktopTop then
            Y := Screen.DesktopTop;
          SetBounds(X, Y, Width, Height);
          if Visible then SetWindowToMonitor;
        end
        else if FPosition in [poMainFormCenter, poOwnerFormCenter] then
        begin
          CenterForm := Application.MainForm;
          if (FPosition = poOwnerFormCenter) and (Owner is TCustomForm) then
            CenterForm := TCustomForm(Owner);
          if Assigned(CenterForm) then
          begin
            X := ((CenterForm.Width - Width) div 2) + CenterForm.Left;
            Y := ((CenterForm.Height - Height) div 2) + CenterForm.Top;
          end else
          begin
            X := (Screen.Width - Width) div 2;
            Y := (Screen.Height - Height) div 2;
          end;
          if X < Screen.DesktopLeft then
            X := Screen.DesktopLeft;
          if Y < Screen.DesktopTop then
            Y := Screen.DesktopTop;
          SetBounds(X, Y, Width, Height);
          if Visible then SetWindowToMonitor;
        end
        else if FPosition = poDesktopCenter then
        begin
          if FormStyle = fsMDIChild then
          begin
            X := (Application.MainForm.ClientWidth - Width) div 2;
            Y := (Application.MainForm.ClientHeight - Height) div 2;
          end else
          begin
            X := ((Screen.DesktopWidth div 2) + Screen.DesktopLeft - (Width div 2));
            Y := ((Screen.DesktopHeight div 2) + Screen.DesktopTop - (Height div 2));
          end;
          if X < Screen.DesktopLeft then X := Screen.DesktopLeft;
          if Y < Screen.DesktopTop then Y := Screen.DesktopTop;
          SetBounds(X, Y, Width, Height);
        end;
        FPosition := poDesigned;
        if FormStyle = fsMDIChild then
        begin
          { Fake a size message to get MDI to behave }
          if FWindowState = wsMaximized then
          begin
            SendMessage(Application.MainForm.ClientHandle, WM_MDIRESTORE, Handle, 0);
            ShowWindow(Handle, SW_SHOWMAXIMIZED);
          end
          else
          begin
            ShowWindow(Handle, ShowCommands[FWindowState]);
            CallWindowProc(@DefMDIChildProc, Handle, WM_SIZE, SIZE_RESTORED,
              Width or (Height shl 16));
            BringToFront;
          end;
          SendMessage(Application.MainForm.ClientHandle,
            WM_MDIREFRESHMENU, 0, 0);
        end
        else
          ShowWindow(Handle, ShowCommands[FWindowState]);
      end else
      begin
        try
          DoHide;
        except
          Application.HandleException(Self);
        end;
        if Screen.ActiveForm = Self then
          MergeMenu(False);
        if FormStyle = fsMDIChild then
          DestroyHandle
        else if fsModal in FFormState then
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or
            SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE)
        else
        begin
          NewActiveWindow := 0;
          if (GetActiveWindow = Handle) and not IsIconic(Handle) then
            NewActiveWindow := FindTopMostWindow(Handle);
          if NewActiveWindow <> 0 then
          begin
            SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or
              SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
            SetActiveWindow(NewActiveWindow);
          end
          else
            ShowWindow(Handle, SW_HIDE);
        end;
      end;
    end else if (csDesigning in ComponentState) and (Parent <> nil) and Showing then
      ShowWindow(Handle, SW_SHOWNORMAL);
  finally
    Exclude(FFormState, fsShowing);
  end;
end;

procedure TCustomForm.CMIconChanged(var Message: TMessage);
begin
  if FIcon.Handle = 0 then IconChanged(nil);
end;

procedure TCustomForm.CMRelease;
begin
  Free;
end;

procedure TCustomForm.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if (FormStyle = fsMDIChild) and (Application.MainForm <> nil) and
    (Application.MainForm.ClientHandle <> 0) then
    SendMessage(Application.MainForm.ClientHandle, WM_MDIREFRESHMENU, 0, 0);
end;

procedure TCustomForm.CMUIActivate(var Message);
begin
  inherited;
end;

procedure TCustomForm.CMParentFontChanged(var Message: TMessage);
var
  F: TFont;
begin
  if ParentFont then
    if Message.wParam <> 0 then
      Font.Assign(TFont(Message.lParam))
    else
    begin
      F := TFont.Create;  // get locale defaults
      try
        Font.Assign(F);
      finally
        F.Free
      end;
    end;
end;

procedure TCustomForm.CMIsShortCut(var Message: TWMKey);
begin
  if IsShortCut(Message) then
    Message.Result := 1
  else
    Message.Result := 0;
end;

procedure TCustomForm.Close;
var
  CloseAction: TCloseAction;
begin
  if fsModal in FFormState then
    ModalResult := mrCancel
  else
    if CloseQuery then
    begin
      if FormStyle = fsMDIChild then
        if biMinimize in BorderIcons then
          CloseAction := caMinimize else
          CloseAction := caNone
      else
        CloseAction := caHide;
      DoClose(CloseAction);
      if CloseAction <> caNone then
        if Application.MainForm = Self then Application.Terminate
        else if CloseAction = caHide then Hide
        else if CloseAction = caMinimize then WindowState := wsMinimized
        else Release;
    end;
end;

function TCustomForm.CloseQuery: Boolean;
var
  I: Integer;
begin
  if FormStyle = fsMDIForm then
  begin
    Result := False;
    for I := 0 to MDIChildCount - 1 do
      if not MDIChildren[I].CloseQuery then Exit;
  end;
  Result := True;
  if Assigned(FOnCloseQuery) then FOnCloseQuery(Self, Result);
end;

procedure TCustomForm.CloseModal;
var
  CloseAction: TCloseAction;
begin
  try
    CloseAction := caNone;
    if CloseQuery then
    begin
      CloseAction := caHide;
      DoClose(CloseAction);
    end;
    case CloseAction of
      caNone: ModalResult := 0;
      caFree: Release;
    end;
  except
    ModalResult := 0;
    Application.HandleException(Self);
  end;
end;

function TCustomForm.GetFormImage: TBitmap;
var
  Ofs: Integer;
begin
  Result := TBitmap.Create;
  try
    Result.Width := ClientWidth;
    Result.Height := ClientHeight;
    Result.Canvas.Brush := Brush;
    Result.Canvas.FillRect(ClientRect);
    Result.Canvas.Lock;
    try
      if GetWindowLong(Handle, GWL_STYLE) and WS_BORDER <> 0 then
        Ofs := -1  // Don't draw form border
      else
        Ofs := 0;  // There is no border
      PaintTo(Result.Canvas.Handle, Ofs, Ofs);
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TCustomForm.Print;
var
  FormImage: TBitmap;
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
begin
  Printer.BeginDoc;
  try
    FormImage := GetFormImage;
    Canvas.Lock;
    try
      { Paint bitmap to the printer }
      with Printer, Canvas do
      begin
        Bits := FormImage.Handle;
        GetDIBSizes(Bits, InfoSize, ImageSize);
        Info := AllocMem(InfoSize);
        try
          Image := AllocMem(ImageSize);
          try
            GetDIB(Bits, 0, Info^, Image^);
            with Info^.bmiHeader do
            begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
            end;
            case PrintScale of
              poProportional:
                begin
                  PrintWidth := MulDiv(DIBWidth, GetDeviceCaps(Handle,
                    LOGPIXELSX), PixelsPerInch);
                  PrintHeight := MulDiv(DIBHeight, GetDeviceCaps(Handle,
                    LOGPIXELSY), PixelsPerInch);
                end;
              poPrintToFit:
                begin
                  PrintWidth := MulDiv(DIBWidth, PageHeight, DIBHeight);
                  if PrintWidth < PageWidth then
                    PrintHeight := PageHeight
                  else
                  begin
                    PrintWidth := PageWidth;
                    PrintHeight := MulDiv(DIBHeight, PageWidth, DIBWidth);
                  end;
                end;
            else
              PrintWidth := DIBWidth;
              PrintHeight := DIBHeight;
            end;
            StretchDIBits(Canvas.Handle, 0, 0, PrintWidth, PrintHeight, 0, 0,
              DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
            FreeMem(Image, ImageSize);
          end;
        finally
          FreeMem(Info, InfoSize);
        end;
      end;
    finally
      Canvas.Unlock;
      FormImage.Free;
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TCustomForm.Hide;
begin
  Visible := False;
end;

procedure TCustomForm.Show;
begin
  Visible := True;
  BringToFront;
end;

procedure TCustomForm.SetFocus;
begin
  if not FActive then
  begin
    if not (Visible and Enabled) then
      { Consider the case of the embedded form designer, where the form will
        not be visible, but the handle will be allocated, and the designing state
        will be set. }
      if not ((csDesigning in ComponentState) and (HandleAllocated)) then
        raise EInvalidOperation.Create(SCannotFocus);
    SetWindowFocus;
  end;
end;

procedure TCustomForm.RecreateAsPopup(AWindowHandle: HWND);
begin
  FInternalPopupParentWnd := AWindowHandle;
  FInternalPopupParent := nil;
  if HandleAllocated then
    RecreateWnd
  else
    UpdateControlState;
end;

procedure TCustomForm.Release;
begin
  PostMessage(Handle, CM_RELEASE, 0, 0);
end;

function TCustomForm.ShowModal: Integer;
var
  WindowList: Pointer;
  SaveFocusState: TFocusState;
  SaveCursor: TCursor;
  SaveCount: Integer;
  ActiveWindow: HWnd;
begin
  CancelDrag;
  if Visible or not Enabled or (fsModal in FFormState) or
    (FormStyle = fsMDIChild) then
    raise EInvalidOperation.Create(SCannotShowModal);
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  ReleaseCapture;
  Application.ModalStarted;
  try
    Include(FFormState, fsModal);
    if (PopupMode = pmNone) and (Application.ModalPopupMode <> pmNone) then
    begin
      RecreateWnd;
      HandleNeeded;
    end;
    ActiveWindow := GetActiveWindow;
    SaveFocusState := Forms.SaveFocusState;
    Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
    Screen.FocusedForm := Self;
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crDefault;
    SaveCount := Screen.CursorCount;
    WindowList := DisableTaskWindows(0);
    try
      Show;
      try
        SendMessage(Handle, CM_ACTIVATE, 0, 0);
        ModalResult := 0;
        repeat
          Application.HandleMessage;
          if Application.Terminated then ModalResult := mrCancel else
            if ModalResult <> 0 then CloseModal;
        until ModalResult <> 0;
        Result := ModalResult;
        SendMessage(Handle, CM_DEACTIVATE, 0, 0);
        if GetActiveWindow <> Handle then ActiveWindow := 0;
      finally
        Hide;
      end;
    finally
      if Screen.CursorCount = SaveCount then
        Screen.Cursor := SaveCursor
      else Screen.Cursor := crDefault;
      EnableTaskWindows(WindowList);
      if Screen.SaveFocusedList.Count > 0 then
      begin
        Screen.FocusedForm := Screen.SaveFocusedList.First;
        Screen.SaveFocusedList.Remove(Screen.FocusedForm);
      end else Screen.FocusedForm := nil;
      if ActiveWindow <> 0 then SetActiveWindow(ActiveWindow);
      RestoreFocusState(SaveFocusState);
      Exclude(FFormState, fsModal);
    end;
  finally
    Application.ModalFinished;
  end;
end;

procedure TCustomForm.UpdateActions;
var
  I: Integer;

  procedure TraverseClients(Container: TWinControl);
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if (csActionClient in Control.ControlStyle) and Control.Visible then
            Control.InitiateAction;
        if Control is TWinControl then
          TraverseClients(TWinControl(Control));
      end;
  end;

begin
  if (csDesigning in ComponentState) or not Showing then Exit;
  { Update form }
  InitiateAction;
  { Update main menu's top-most items }
  if Menu <> nil then
    for I := 0 to Menu.Items.Count - 1 do
      with Menu.Items[I] do
        if Visible then InitiateAction;
  { Update any controls }
  TraverseClients(Self);
end;

procedure TCustomForm.UpdateWindowState;
var
  Placement: TWindowPlacement;
begin
  if HandleAllocated then
  begin
    Placement.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @Placement);
    case Placement.showCmd of
      SW_SHOWMINIMIZED: FWindowState := wsMinimized;
      SW_SHOWMAXIMIZED: FWindowState := wsMaximized;
    else
      FWindowState := wsNormal;
    end;
  end;
end;

procedure TCustomForm.RequestAlign;
begin
  if Parent = nil then
    Screen.AlignForm(Self)
  else if not (csDesigning in ComponentState) then
    inherited RequestAlign;
end;

procedure TCustomForm.WMSettingChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = SPI_SETWORKAREA then
    RequestAlign;
end;

procedure TCustomForm.CMActionExecute(var Message: TMessage);

  function ProcessExecute(Control: TControl): Boolean;
  begin
    Result := (Control <> nil) and
      Control.ExecuteAction(TBasicAction(Message.LParam));
  end;

  function TraverseClients(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible and ProcessExecute(Control) or
          (Control is TWinControl) and TraverseClients(TWinControl(Control)) then
        begin
          Result := True;
          Exit;
        end;
      end;
    Result := False;
  end;

begin
  if (csDesigning in ComponentState) or not Showing then Exit;
  { Find a target for given Command (Message.LParam). }
  if ProcessExecute(ActiveControl) or ProcessExecute(Self) or
    TraverseClients(Self) then
    Message.Result := 1;
end;

procedure TCustomForm.CMActionUpdate(var Message: TMessage);

  function ProcessUpdate(Control: TControl): Boolean;
  begin
    Result := (Control <> nil) and
      Control.UpdateAction(TBasicAction(Message.LParam));
  end;

  function TraverseClients(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible and ProcessUpdate(Control) or
          (Control is TWinControl) and TraverseClients(TWinControl(Control)) then
        begin
          Result := True;
          Exit;
        end;
      end;
    Result := False;
  end;

begin
  if (csDesigning in ComponentState) or not Showing then Exit;
  { Find a target for given Command (Message.LParam). }
  if ProcessUpdate(ActiveControl) or ProcessUpdate(Self) or
    TraverseClients(Self) then
    Message.Result := 1;
end;

function TCustomForm.IsShortCut(var Message: TWMKey): Boolean;

  function DispatchShortCut(const Owner: TComponent) : Boolean;
  var
    I: Integer;
    Component: TComponent;
  begin
    Result := False;
    { Dispatch to all children }
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      Component := Owner.Components[I];
      if Component is TCustomActionList then
      begin
        if TCustomActionList(Component).IsShortCut(Message) then
        begin
          Result := True;
          Exit;
        end
      end
      else
      begin
        Result := DispatchShortCut(Component);
        if Result then
          Break;
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(FOnShortCut) then FOnShortCut(Message, Result);
  Result := Result or (Menu <> nil) and (Menu.WindowHandle <> 0) and
    Menu.IsShortCut(Message) or DispatchShortCut(Self);
end;

function TCustomForm.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // Route the QueryInterface through the Designer first
  if (Designer = nil) or (Designer.QueryInterface(IID, Obj) <> 0) then
    Result := inherited QueryInterface(IID, Obj)
  else
    Result := 0;
end;

procedure TCustomForm.MouseWheelHandler(var Message: TMessage);
begin
  with Message do
  begin
    if FFocusedControl <> nil then
      Result := FFocusedControl.Perform(CM_MOUSEWHEEL, WParam, LParam)
    else
      inherited MouseWheelHandler(Message);
  end;
end;

function TCustomForm.HandleCreateException: Boolean;
begin
  Application.HandleException(Self);
  Result := True;
end;

procedure TCustomForm.SetLayeredAttribs;
const
  cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
  cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
var
  AStyle: Integer;
begin
  if not (csDesigning in ComponentState) and
    (Assigned(SetLayeredWindowAttributes)) and HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if FAlphaBlend or FTransparentColor then
    begin
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
      SetLayeredWindowAttributes(Handle, FTransparentColorValue, FAlphaBlendValue,
        cUseAlpha[FAlphaBlend] or cUseColorKey[FTransparentColor]);
    end
    else
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    end;
  end;
end;

procedure TCustomForm.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomForm.SetAlphaBlendValue(const Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomForm.SetTransparentColorValue(const Value: TColor);
begin
  if FTransparentColorValue <> Value then
  begin
    FTransparentColorValue := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomForm.SetTransparentColor(const Value: Boolean);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    if FTransparentColor and GlassFrame.Enabled then
      GlassFrame.Enabled := False; // GlassFrame and TransparentColor are mutually exclusive
    SetLayeredAttribs;
  end;
end;

procedure TCustomForm.InitAlphaBlending(var Params: TCreateParams);
begin
  if not (csDesigning in ComponentState) and (Assigned(SetLayeredWindowAttributes)) then
    if FAlphaBlend or FTransparentColor then
      Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

procedure TCustomForm.MakeFullyVisible(AMonitor: TMonitor);
var
  ALeft: Integer;
  ATop: Integer;
begin
  if AMonitor = nil then
    AMonitor := Monitor;
  ALeft := Left;
  ATop := Top;
  if Left + Width > AMonitor.Left + AMonitor.Width then
    ALeft := AMonitor.Left + AMonitor.Width - Width;
  if Left < AMonitor.Left then
    ALeft := AMonitor.Left;
  if Top + Height > AMonitor.Top + AMonitor.Height then
    ATop := AMonitor.Top + AMonitor.Height - Height;
  if Top < AMonitor.Top then
    ATop := AMonitor.Top;
  SetBounds(ALeft, ATop, Width, Height);
end;

procedure TCustomForm.SetLeft(Value: Integer);
begin
  if (csDesigning in ComponentState) and (Parent <> nil) then
  begin
    DesignInfo := (DesignInfo and $0000FFFF) or (Value shl 16);
    if not (csLoading in ComponentState) and (Position <> poDefaultSizeOnly) then
      Position := poDesigned;
  end else
    inherited Left := Value;
end;

procedure TCustomForm.SetTop(Value: Integer);
begin
  if (csDesigning in ComponentState) and (Parent <> nil) then
  begin
    DesignInfo := (DesignInfo and $FFFF0000) or (Cardinal(Value) and $FFFF);
    if not (csLoading in ComponentState) and (Position <> poDefaultSizeOnly) then
      Position := poDesigned;
  end else
    inherited Top := Value;
end;

{ TForm }

const
  TileParams: array[TTileMode] of Word = (MDITILE_HORIZONTAL, MDITILE_VERTICAL);

procedure TForm.Tile;
begin
  if (FFormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDITILE, TileParams[FTileMode], 0);
end;

procedure TForm.Cascade;
begin
  if (FFormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDICASCADE, 0, 0);
end;

procedure TForm.ArrangeIcons;
begin
  if (FFormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDIICONARRANGE, 0, 0);
end;

procedure TForm.Next;
begin
  if (FFormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDINEXT, 0, 0);
end;

procedure TForm.Previous;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(FClientHandle, WM_MDINEXT, 0, 1);
end;

{ TCustomDockForm }

constructor TCustomDockForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  AutoScroll := False;
  BorderStyle := bsSizeToolWin;
  DockSite := True;
  FormStyle := fsStayOnTop;
end;

procedure TCustomDockForm.DoAddDockClient(Client: TControl; const ARect: TRect);
var
  S: string;
  I: Integer;
begin
  if DockClientCount = 1 then
  begin
    { Use first docked control }
    with Client do
    begin
      SetString(S, nil, GetTextLen + 1);
      GetTextBuf(PChar(S), Length(S));
      { Search for first CR/LF and end string there }
      for I := 1 to Length(S) do
        if (S[I] in [#13, #10]) then
        begin
          SetLength(S, I - 1);
          Break;
        end;
    end;
    Caption := S;
  end;
  inherited DoAddDockClient(Client, ARect);
  Client.Align := alClient;
  if not (csLoading in ComponentState) then
    Visible := True;
end;

procedure TCustomDockForm.DoRemoveDockClient(Client: TControl);
begin
  inherited DoRemoveDockClient(Client);
  if DockClientCount = 0 then Release;
end;

procedure TCustomDockForm.Loaded;
var
  I: Integer;
begin
  { Make sure we dock controls after streaming }
  for I := 0 to ControlCount - 1 do
    Controls[I].Dock(Self, ClientRect);
  inherited Loaded;
end;

procedure TCustomDockForm.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := DockClientCount = 0;
end;

procedure TCustomDockForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) and (Message.Result = HTCLIENT) then
    Message.Result := HTCAPTION;
end;

procedure TCustomDockForm.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  if (Message.HitTest = HTCAPTION) and (DragKind <> dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(Handle) and
    (DockClientCount > 0) then
  begin
    { Activate window since we override WM_NCLBUTTON behavior }
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or
      SWP_NOSIZE);
    PostMessage(Handle, WM_NCLBUTTONUP, TMessage(Message).WParam,
      TMessage(Message).LParam);
    if Active then DockClients[0].BeginDrag(True);
  end
  else
    inherited;
end;

procedure TCustomDockForm.CMControlListChange(var Message: TMessage);
begin
  inherited;
  if Message.LParam = 0 then
  begin
    Perform(CM_UNDOCKCLIENT, 0, Message.WParam);
    if TControl(Message.WParam).HostDockSite = Self then
      TControl(Message.WParam).Dock(NullDockSite, TControl(Message.WParam).BoundsRect);
  end;
end;

procedure TCustomDockForm.CMDockNotification(var Message: TCMDockNotification);
var
  S: string;
  I: Integer;
begin
  inherited;
  case Message.NotifyRec^.ClientMsg of
    CM_VISIBLECHANGED: Visible := Message.Client.Visible;
    WM_SETTEXT:
      begin
        SetString(S, nil, Message.Client.GetTextLen + 1);
        Message.Client.GetTextBuf(PChar(S), Length(S));
        { Search for first CR/LF and end string there }
        for I := 1 to Length(S) do
          if S[I] in [#13, #10] then
          begin
            SetLength(S, I - 1);
            Break;
          end;
        Caption := S;
      end;
  end;
end;

procedure TCustomDockForm.CMUnDockClient(var Message: TCMUnDockClient);
begin
  inherited;
  Message.Client.Align := alNone;
end;

procedure TCustomDockForm.CMVisibleChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) then
    for I := 0 to DockClientCount - 1 do
      DockClients[I].Visible := Visible;
end;

{ TMonitor }

function TMonitor.GetLeft: Integer;
begin
  Result := BoundsRect.Left;
end;

function TMonitor.GetHeight: Integer;
begin
  with BoundsRect do
    Result := Bottom - Top;
end;

function TMonitor.GetTop: Integer;
begin
  Result := BoundsRect.Top;
end;

function TMonitor.GetWidth: Integer;
begin
  with BoundsRect do
    Result := Right - Left;
end;

function TMonitor.GetBoundsRect: TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  GetMonitorInfo(FHandle, @MonInfo);
  Result := MonInfo.rcMonitor;
end;

function TMonitor.GetWorkareaRect: TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  GetMonitorInfo(FHandle, @MonInfo);
  Result := MonInfo.rcWork;
end;

function TMonitor.GetPrimary: Boolean;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  GetMonitorInfo(FHandle, @MonInfo);
  Result := (MonInfo.dwFlags and MONITORINFOF_PRIMARY) <> 0;
end;

{ TScreen }

const
  IDC_NODROP =    PChar(32767);
  IDC_DRAG   =    PChar(32766);
  IDC_HSPLIT =    PChar(32765);
  IDC_VSPLIT =    PChar(32764);
  IDC_MULTIDRAG = PChar(32763);
  IDC_SQLWAIT =   PChar(32762);
  IDC_HANDPT =   PChar(32761);

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;

constructor TScreen.Create(AOwner: TComponent);
var
  DC: HDC;
begin
  inherited Create(AOwner);
  Classes.AddDataModule := AddDataModule;
  Classes.RemoveDataModule := RemoveDataModule;
  CreateCursors;
  FDefaultKbLayout := GetKeyboardLayout(0);
  FForms := TList.Create;
  FCustomForms := TList.Create;
  FDataModules := TList.Create;
  FMonitors := TList.Create;
  FSaveFocusedList := TList.Create;
  DC := GetDC(0);
  FPixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
  EnumDisplayMonitors(0, nil, @EnumMonitorsProc, LongInt(FMonitors));
  FIconFont := TFont.Create;
  FMenuFont := TFont.Create;
  FHintFont := TFont.Create;
  GetMetricSettings;
  FIconFont.OnChange := IconFontChanged;
  FMenuFont.OnChange := IconFontChanged;
  FHintFont.OnChange := IconFontChanged;
end;

destructor TScreen.Destroy;
var
  I: Integer;
begin
  FHintFont.Free;
  FMenuFont.Free;
  FIconFont.Free;
  FDataModules.Free;
  FCustomForms.Free;
  FForms.Free;
  FFonts.Free;
  FImes.Free;
  FSaveFocusedList.Free;
  if FMonitors <> nil then
    for I := 0 to FMonitors.Count - 1 do
      TMonitor(FMonitors[I]).Free;
  FMonitors.Free;
  DestroyCursors;
  Classes.AddDataModule := nil;
  Classes.RemoveDataModule := nil;
  inherited Destroy;
end;

function TScreen.GetHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYSCREEN);
end;

function TScreen.GetWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXSCREEN);
end;

function TScreen.GetDesktopTop: Integer;
begin
  Result := GetSystemMetrics(SM_YVIRTUALSCREEN);
end;

function TScreen.GetDesktopLeft: Integer;
begin
  Result := GetSystemMetrics(SM_XVIRTUALSCREEN);
end;

function TScreen.GetDesktopHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYVIRTUALSCREEN);
end;

function TScreen.GetDesktopWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVIRTUALSCREEN);
end;

function TScreen.GetMonitor(Index: Integer): TMonitor;
begin
  Result := FMonitors[Index];
end;

function TScreen.GetMonitorCount: Integer;
begin
  if FMonitors.Count = 0 then
    Result := GetSystemMetrics(SM_CMONITORS)
  else
    Result := FMonitors.Count;
end;

function TScreen.GetForm(Index: Integer): TForm;
begin
  Result := FForms[Index];
end;

function TScreen.GetFormCount: Integer;
begin
  Result := FForms.Count;
end;

function TScreen.GetCustomForms(Index: Integer): TCustomForm;
begin
  Result := FCustomForms[Index];
end;

function TScreen.GetCustomFormCount: Integer;
begin
  Result := FCustomForms.Count;
end;

procedure TScreen.UpdateLastActive;
begin
  if FLastActiveCustomForm <> FActiveCustomForm then
  begin
    FLastActiveCustomForm := FActiveCustomForm;
    if Assigned(FOnActiveFormChange) then FOnActiveFormChange(Self);
  end;
  if FLastActiveControl <> FActiveControl then
  begin
    FLastActiveControl := FActiveControl;
    if Assigned(FOnActiveControlChange) then FOnActiveControlChange(Self);
  end;
end;

procedure TScreen.AddForm(AForm: TCustomForm);
begin
  FCustomForms.Add(AForm);
  if AForm is TForm then
  begin
    FForms.Add(AForm);
    Application.UpdateVisible;
  end;
end;

procedure TScreen.RemoveForm(AForm: TCustomForm);
begin
  FCustomForms.Remove(AForm);
  FForms.Remove(AForm);
  Application.UpdateVisible;
  if (FCustomForms.Count = 0) and (Application.FHintWindow <> nil) then
    Application.FHintWindow.ReleaseHandle;
end;

procedure TScreen.AddDataModule(DataModule: TDataModule);
begin
  FDataModules.Add(DataModule);
end;

procedure TScreen.RemoveDataModule(DataModule: TDataModule);
begin
  FDataModules.Remove(DataModule);
end;

{$IFDEF LINUX}
function LoadCursorResData(hInstance: HINST; const IconName: string): HICON;
var
  ResInfo: HRSRC;
  ResHandle: HGLOBAL;
  ResData, Data: PChar;
begin
  Result := 0;
  ResInfo := FindResource(hInstance, PChar(IconName), RT_RCDATA);
  if ResInfo <> 0 then
  begin
    ResHandle := LoadResource(hInstance, ResInfo);
    if ResHandle <> 0 then
    begin
      ResData := LockResource(ResHandle);
      Data := AllocMem(SizeOfResource(hInstance, ResInfo));
      try
        Inc(ResData, SizeOf(Graphics.TCursorOrIcon));
        PSmallPoint(Data).X := PIconRec(ResData).Reserved1;
        PSmallPoint(Data).Y := PIconRec(ResData).Reserved2;
        Inc(Data, SizeOf(TSmallPoint));
        Inc(ResData, SizeOf(TIconRec));
        Move(ResData^, Data^, SizeOfResource(hInstance, ResInfo) - SizeOf(TCursorOrIcon) - SizeOf(TIconRec));
        Dec(Data, SizeOf(TSmallPoint));
        Result := CreateIconFromResource(PByte(Data), SizeOfResource(hInstance, ResInfo) -
          SizeOf(TCursorOrIcon) - SizeOf(TIconRec) + SizeOf(TSmallPoint), False, $00030000);
      finally
        FreeMem(Data);
      end;
    end;
  end;
end;
{$ENDIF}


procedure TScreen.CreateCursors;
const
  CursorMap: array[crSizeAll..crArrow] of PChar = (
    IDC_SIZEALL, IDC_HANDPT, IDC_HELP, IDC_APPSTARTING, IDC_NO, IDC_SQLWAIT,
    IDC_MULTIDRAG, IDC_VSPLIT, IDC_HSPLIT, IDC_NODROP, IDC_DRAG, IDC_WAIT,
    IDC_UPARROW, IDC_SIZEWE, IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEALL,
    IDC_IBEAM, IDC_CROSS, IDC_ARROW);
var
  I: Integer;
{$IFDEF MSWINDOWS}
  Instance: THandle;
{$ENDIF}
begin
  FDefaultCursor := LoadCursor(0, IDC_ARROW);
  for I := Low(CursorMap) to High(CursorMap) do
  begin
    if ((I >= crSqlWait) and (I <= crDrag)) or (I = crHandPoint) then
{$IFDEF LINUX}
      InsertCursor(I, LoadCursorResData(HInstance, 'C' + IntToStr(Integer(CursorMap[I]))))
    else
      InsertCursor(I, LoadCursor(0, CursorMap[I]));
{$ENDIF}
{$IFDEF MSWINDOWS}
      Instance := HInstance else
      Instance := 0;
    InsertCursor(I, LoadCursor(Instance, CursorMap[I]));
{$ENDIF}
  end;
end;

procedure TScreen.DestroyCursors;
var
  P, Next: PCursorRec;
  Hdl: THandle;
begin
  P := FCursorList;
  while P <> nil do
  begin
    if ((P^.Index >= crSqlWait) and (P^.Index <= crDrag)) or
      (P^.Index = crHandPoint) or (P^.Index > 0) then
      DestroyCursor(P^.Handle);
    Next := P^.Next;
    Dispose(P);
    P := Next;
  end;
  Hdl := LoadCursor(0, IDC_ARROW);
  if Hdl <> FDefaultCursor then
    DestroyCursor(FDefaultCursor);
end;

procedure TScreen.DeleteCursor(Index: Integer);
var
  P, Q: PCursorRec;
begin
  P := FCursorList;
  Q := nil;
  while (P <> nil) and (P^.Index <> Index) do
  begin
    Q := P;
    P := P^.Next;
  end;
  if P <> nil then
  begin
    DestroyCursor(P^.Handle);
    if Q = nil then FCursorList := P^.Next else Q^.Next := P^.Next;
    Dispose(P);
  end;
end;

function TScreen.FindMonitor(Handle: HMONITOR): TMonitor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MonitorCount - 1 do
    if Monitors[I].Handle = Handle then
    begin
      Result := Monitors[I];
      break;
    end;
end;

procedure TScreen.InsertCursor(Index: Integer; Handle: HCURSOR);
var
  P: PCursorRec;
begin
  New(P);
  P^.Next := FCursorList;
  P^.Index := Index;
  P^.Handle := Handle;
  FCursorList := P;
end;

function TScreen.GetImes: TStrings;
const
  KbLayoutRegkeyFmt = 'System\CurrentControlSet\Control\Keyboard Layouts\%.8x';  // do not localize
  KbLayoutRegSubkey = 'layout text'; // do not localize
var
  TotalKbLayout, I, Bufsize: Integer;
  KbList: array[0..63] of HKL;
  qKey: HKey;
  ImeFileName: array [Byte] of Char;
  RegKey: array [0..63] of Char;
begin
  if FImes = nil then
  begin
    FImes := TStringList.Create;

    FDefaultIme := '';
    TotalKbLayout := GetKeyboardLayoutList(64, KbList);

    for I := 0 to TotalKbLayout - 1 do
    begin
      if Imm32IsIME(KbList[I]) then
      begin
        if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
          StrFmt(RegKey, KbLayoutRegKeyFmt, [KbList[I]]), 0, KEY_READ,
          qKey) = ERROR_SUCCESS then
        try
          Bufsize := sizeof(ImeFileName);
          if RegQueryValueEx(qKey, KbLayoutRegSubKey, nil, nil,
               @ImeFileName, @Bufsize) = ERROR_SUCCESS then
          begin
            FImes.AddObject(ImeFileName, TObject(KbList[I]));
            if KbList[I] = FDefaultKbLayout then
              FDefaultIme := ImeFileName;
          end;
        finally
          RegCloseKey(qKey);
        end;
      end;
    end;
    TStringList(FImes).Duplicates := dupIgnore;
    TStringList(FImes).Sorted := TRUE;
  end;
  Result := FImes;
end;

function TScreen.GetDefaultIme: String;
begin
  GetImes;  // load Ime list, find default
  Result := FDefaultIme;
end;

procedure TScreen.IconFontChanged(Sender: TObject);
begin
  Application.NotifyForms(CM_SYSFONTCHANGED);
  if (Sender = FHintFont) and Assigned(Application) and Application.ShowHint then
  begin
    Application.ShowHint := False;
    Application.ShowHint := True;
  end;
end;

function TScreen.GetDataModule(Index: Integer): TDataModule;
begin
  Result := FDataModules[Index];
end;

function TScreen.GetDataModuleCount: Integer;
begin
  Result := FDataModules.Count;
end;

function TScreen.GetCursors(Index: Integer): HCURSOR;
var
  P: PCursorRec;
begin
  Result := 0;
  if Index <> crNone then
  begin
    P := FCursorList;
    while (P <> nil) and (P^.Index <> Index) do P := P^.Next;
    if P = nil then Result := FDefaultCursor else Result := P^.Handle;
  end;
end;

procedure TScreen.SetCursor(Value: TCursor);
var
  P: TPoint;
  Handle: HWND;
  Code: Longint;
begin
  if Value <> Cursor then
  begin
    FCursor := Value;
    if Value = crDefault then
    begin
      { Reset the cursor to the default by sending a WM_SETCURSOR to the
        window under the cursor }
      GetCursorPos(P);
      Handle := WindowFromPoint(P);
      if (Handle <> 0) and
        (GetWindowThreadProcessId(Handle, nil) = GetCurrentThreadId) then
      begin
        Code := SendMessage(Handle, WM_NCHITTEST, 0, LongInt(PointToSmallPoint(P)));
        SendMessage(Handle, WM_SETCURSOR, Handle, MakeLong(Code, WM_MOUSEMOVE));
        Exit;
      end;
    end;
    Windows.SetCursor(Cursors[Value]);
  end;
  Inc(FCursorCount);
end;

procedure TScreen.SetCursors(Index: Integer; Handle: HCURSOR);
begin
  if Index = crDefault then
    if Handle = 0 then
      FDefaultCursor := LoadCursor(0, IDC_ARROW)
    else
      FDefaultCursor := Handle
  else if Index <> crNone then
  begin
    DeleteCursor(Index);
    if Handle <> 0 then InsertCursor(Index, Handle);
  end;
end;

procedure TScreen.SetHintFont(Value: TFont);
begin
  FHintFont.Assign(Value);
end;

procedure TScreen.SetIconFont(Value: TFont);
begin
  FIconFont.Assign(Value);
end;

procedure TScreen.SetMenuFont(Value: TFont);
begin
  FMenuFont.Assign(Value);
end;

procedure TScreen.GetMetricSettings;
var
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
  SaveShowHint: Boolean;
begin
  SaveShowHint := False;
  if Assigned(Application) then SaveShowHint := Application.ShowHint;
  try
    if Assigned(Application) then Application.ShowHint := False;
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
      FIconFont.Handle := CreateFontIndirect(LogFont)
    else
      FIconFont.Handle := GetStockObject(SYSTEM_FONT);
    NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    begin
      FHintFont.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont);
      FMenuFont.Handle := CreateFontIndirect(NonClientMetrics.lfMenuFont);
    end else
    begin
      FHintFont.Size := 8;
      FMenuFont.Handle := GetStockObject(SYSTEM_FONT);
    end;
    FHintFont.Color := clInfoText;
    FMenuFont.Color := clMenuText;
  finally
    if Assigned(Application) then Application.ShowHint := SaveShowHint;
  end;
end;

procedure TScreen.DisableAlign;
begin
  Inc(FAlignLevel);
end;

procedure TScreen.EnableAlign;
begin
  Dec(FAlignLevel);
  if (FAlignLevel = 0) and (csAlignmentNeeded in FControlState) then Realign;
end;

procedure TScreen.Realign;
begin
  AlignForm(nil);
end;

procedure TScreen.AlignForms(AForm: TCustomForm; var Rect: TRect);
var
  AlignList: TList;

  function InsertBefore(C1, C2: TCustomForm; AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop: Result := C1.Top < C2.Top;
      alBottom: Result := (C1.Top + C1.Height) > (C2.Top + C2.Height);
      alLeft: Result := C1.Left < C2.Left;
      alRight: Result := (C1.Left + C1.Width) > (C2.Left + C2.Width);
    end;
  end;

  procedure DoPosition(Form: TCustomForm; AAlign: TAlign);
  var
    NewLeft, NewTop, NewWidth, NewHeight: Integer;
  begin
    with Rect do
    begin
      NewWidth := Right - Left;
      if (NewWidth < 0) or (AAlign in [alLeft, alRight]) then
        NewWidth := Form.Width;
      NewHeight := Bottom - Top;
      if (NewHeight < 0) or (AAlign in [alTop, alBottom]) then
        NewHeight := Form.Height;
      if (AAlign = alTop) and (Form.WindowState = wsMaximized) then
      begin
        NewLeft := Form.Left;
        NewTop := Form.Top;
        NewWidth := GetSystemMetrics(SM_CXMAXIMIZED);
      end
      else
      begin
        NewLeft := Left;
        NewTop := Top;
      end;
      case AAlign of
        alTop: Inc(Top, NewHeight);
        alBottom:
          begin
            Dec(Bottom, NewHeight);
            NewTop := Bottom;
          end;
        alLeft: Inc(Left, NewWidth);
        alRight:
          begin
            Dec(Right, NewWidth);
            NewLeft := Right;
          end;
      end;
    end;
    Form.SetBounds(NewLeft, NewTop, NewWidth, NewHeight);
    if Form.WindowState = wsMaximized then
    begin
      Dec(NewWidth, NewLeft);
      Dec(NewHeight, NewTop);
    end;
    { Adjust client rect if Form didn't resize as we expected }
    if (Form.Width <> NewWidth) or (Form.Height <> NewHeight) then
      with Rect do
        case AAlign of
          alTop: Dec(Top, NewHeight - Form.Height);
          alBottom: Inc(Bottom, NewHeight - Form.Height);
          alLeft: Dec(Left, NewWidth - Form.Width);
          alRight: Inc(Right, NewWidth - Form.Width);
          alClient:
            begin
              Inc(Right, NewWidth - Form.Width);
              Inc(Bottom, NewHeight - Form.Height);
            end;
        end;
  end;

  procedure DoAlign(AAlign: TAlign);
  var
    I, J: Integer;
    Form: TCustomForm;
  begin
    AlignList.Clear;
    if (AForm <> nil) and (AForm.Parent = nil) and
      not (csDesigning in AForm.ComponentState) and
      AForm.Visible and (AForm.Align = AAlign) and
      (AForm.WindowState <> wsMinimized) then
      AlignList.Add(AForm);
    for I := 0 to CustomFormCount - 1 do
    begin
      Form := TCustomForm(CustomForms[I]);
      if (Form.Parent = nil) and (Form.Align = AAlign) and
        not (csDesigning in Form.ComponentState) and
        Form.Visible and (Form.WindowState <> wsMinimized) then
      begin
        if Form = AForm then Continue;
        J := 0;
        while (J < AlignList.Count) and not InsertBefore(Form,
          TCustomForm(AlignList[J]), AAlign) do Inc(J);
        AlignList.Insert(J, Form);
      end;
    end;
    for I := 0 to AlignList.Count - 1 do
      DoPosition(TCustomForm(AlignList[I]), AAlign);
  end;

  function AlignWork: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := CustomFormCount - 1 downto 0 do
      with TCustomForm(CustomForms[I]) do
        if (Parent = nil) and not (csDesigning in ComponentState) and
          (Align <> alNone) and Visible and (WindowState <> wsMinimized) then Exit;
    Result := False;
  end;

begin
  if AlignWork then
  begin
    AlignList := TList.Create;
    try
      DoAlign(alTop);
      DoAlign(alBottom);
      DoAlign(alLeft);
      DoAlign(alRight);
      DoAlign(alClient);
    finally
      AlignList.Free;
    end;
  end;
end;

procedure TScreen.AlignForm(AForm: TCustomForm);
var
  Rect: TRect;
begin
  if FAlignLevel <> 0 then
    Include(FControlState, csAlignmentNeeded)
  else
  begin
    DisableAlign;
    try
      SystemParametersInfo(SPI_GETWORKAREA, 0, @Rect, 0);
      AlignForms(AForm, Rect);
    finally
      Exclude(FControlState, csAlignmentNeeded);
      EnableAlign;
    end;
  end;
end;

function TScreen.GetFonts: TStrings;
var
  DC: HDC;
  LFont: TLogFont;
begin
  if FFonts = nil then
  begin
    FFonts := TStringList.Create;
    DC := GetDC(0);
    try
      FFonts.Add('Default');
      FillChar(LFont, sizeof(LFont), 0);
      LFont.lfCharset := DEFAULT_CHARSET;
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LongInt(FFonts), 0);
      TStringList(FFonts).Sorted := TRUE;
    finally
      ReleaseDC(0, DC);
    end;
  end;
  Result := FFonts;
end;

procedure TScreen.ResetFonts;
begin
  FreeAndNil(FFonts);
end;

{ Hint functions }

function GetHint(Control: TControl): string;
begin
  while Control <> nil do
    if Control.Hint = '' then
      Control := Control.Parent
    else
    begin
      Result := Control.Hint;
      Exit;
    end;
  Result := '';
end;

function GetHintControl(Control: TControl): TControl;
begin
  Result := Control;
  while (Result <> nil) and not Result.ShowHint do Result := Result.Parent;
  if (Result <> nil) and (csDesigning in Result.ComponentState) then Result := nil;
end;

procedure HintTimerProc(Wnd: HWnd; Msg, TimerID, SysTime: Longint); stdcall;
begin
  if Application <> nil then
  try
    Application.HintTimerExpired;
  except
    Application.HandleException(Application);
  end;
end;

{ DLL specific hint routines - Only executed in the context of a DLL to
  simulate hooks the .EXE has in the message loop }
var
  HintThreadID: DWORD;
  HintDoneEvent: THandle;

procedure HintMouseThread(Param: Integer); stdcall;
var
  P: TPoint;
begin
  HintThreadID := GetCurrentThreadID;
  while WaitForSingleObject(HintDoneEvent, 100) = WAIT_TIMEOUT do
  begin
    if (Application <> nil) and (Application.FHintControl <> nil) then
    begin
      GetCursorPos(P);
      if FindVCLWindow(P) = nil then
        Application.CancelHint;
    end;
  end;
end;

var
  HintHook: HHOOK;
  HintThread: THandle;

function HintGetMsgHook(nCode: Integer; wParam: Longint; var Msg: TMsg): Longint; stdcall;
begin
  Result := CallNextHookEx(HintHook, nCode, wParam, Longint(@Msg));
  if (nCode >= 0) and (Application <> nil) then Application.IsHintMsg(Msg);
end;

procedure HookHintHooks;
var
  ThreadID: DWORD;
begin
  if not Application.FRunning then
  begin
    if HintHook = 0 then
      HintHook := SetWindowsHookEx(WH_GETMESSAGE, @HintGetMsgHook, 0, GetCurrentThreadID);
    if HintDoneEvent = 0 then
      HintDoneEvent := CreateEvent(nil, False, False, nil);
    if HintThread = 0 then
      HintThread := CreateThread(nil, 1000, @HintMouseThread, nil, 0, ThreadID);
  end;
end;

procedure UnhookHintHooks;
begin
  if HintHook <> 0 then UnhookWindowsHookEx(HintHook);
  HintHook := 0;
  if HintThread <> 0 then
  begin
    SetEvent(HintDoneEvent);
    if GetCurrentThreadId <> HintThreadID then
      WaitForSingleObject(HintThread, INFINITE);
    CloseHandle(HintThread);
    HintThread := 0;
  end;
end;

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0 else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWnd; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then SetAnimation(True);
end;

function TScreen.GetDesktopRect: TRect;
begin
  Result := Bounds(DesktopLeft, DesktopTop, DesktopWidth, DesktopHeight);
end;

function TScreen.GetWorkAreaHeight: Integer;
begin
  with WorkAreaRect do
    Result := Bottom - Top;
end;

function TScreen.GetWorkAreaLeft: Integer;
begin
  Result := WorkAreaRect.Left;
end;

function TScreen.GetWorkAreaRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

function TScreen.GetWorkAreaTop: Integer;
begin
  Result := WorkAreaRect.Top;
end;

function TScreen.GetWorkAreaWidth: Integer;
begin
  with WorkAreaRect do
    Result := Right - Left;
end;

const
  MonitorDefaultFlags: array[TMonitorDefaultTo] of DWORD = (MONITOR_DEFAULTTONEAREST,
    MONITOR_DEFAULTTONULL, MONITOR_DEFAULTTOPRIMARY);

function TScreen.MonitorFromPoint(const Point: TPoint;
  MonitorDefault: TMonitorDefaultTo): TMonitor;
begin
  Result := FindMonitor(MultiMon.MonitorFromPoint(Point,
    MonitorDefaultFlags[MonitorDefault]));
end;

function TScreen.MonitorFromRect(const Rect: TRect;
  MonitorDefault: TMonitorDefaultTo): TMonitor;
begin
  Result := FindMonitor(MultiMon.MonitorFromRect(@Rect,
    MonitorDefaultFlags[MonitorDefault]));
end;

function TScreen.MonitorFromWindow(const Handle: THandle;
  MonitorDefault: TMonitorDefaultTo): TMonitor;
begin
  Result := FindMonitor(MultiMon.MonitorFromWindow(Handle,
    MonitorDefaultFlags[MonitorDefault]));
end;

procedure TScreen.ClearMonitors;
var
  I: Integer;
begin
  for I := 0 to FMonitors.Count - 1 do
    TMonitor(FMonitors[I]).Free;
  Screen.FMonitors.Clear;
end;

procedure TScreen.GetMonitors;
begin
  ClearMonitors;
  EnumDisplayMonitors(0, nil, @EnumMonitorsProc, LongInt(Screen.FMonitors));
end;

function TScreen.GetPrimaryMonitor: TMonitor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MonitorCount - 1 do
  begin
    if Monitors[I].Primary then
    begin
      Result := Monitors[I];
      Exit;
    end;
  end;
  { If we didn't find the primary monitor, reset the display and try
    again (it may have changeed) }
  GetMonitors;
  for I := 0 to MonitorCount - 1 do
  begin
    if Monitors[I].Primary then
    begin
      Result := Monitors[I];
      Break;
    end;
  end;
end;

{ TApplication }

var
  WindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TApplication');

constructor TApplication.Create(AOwner: TComponent);
var
  P: PChar;
  ModuleName: array[0..255] of Char;
begin
  inherited Create(AOwner);
  if not Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException := HandleException;
  if not Assigned(Classes.ApplicationShowException) then
    Classes.ApplicationShowException := ShowException;
  FBiDiMode := bdLeftToRight;
  FTopMostList := TList.Create;
  FPopupOwners := TList.Create;
  FWindowHooks := TList.Create;
  FHintControl := nil;
  FHintWindow := nil;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FHintShortCuts := True;
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowHint := False;
  FActive := True;
  FAutoDragDocking := True;
  FIcon := TIcon.Create;
{$IFDEF MSWINDOWS}
  FIcon.Handle := LoadIcon(MainInstance, 'MAINICON');
{$ENDIF}
  FIcon.OnChange := IconChanged;
  GetModuleFileName(MainInstance, ModuleName, SizeOf(ModuleName));
  OemToAnsi(ModuleName, ModuleName);
  P := AnsiStrRScan(ModuleName, '\');
  if P <> nil then StrCopy(ModuleName, P + 1);
  P := AnsiStrScan(ModuleName, '.');
  if P <> nil then P^ := #0;
  AnsiLower(CharNext(ModuleName));
  FTitle := ModuleName;
  FModalPopupMode := pmNone;
  FPopupControlWnd := Classes.AllocateHWnd(PopupControlProc);
  if not IsLibrary then CreateHandle;
  UpdateFormatSettings := True;
  UpdateMetricSettings := True;
  FShowMainForm := True;
  FAllowTesting := True;
  FTestLib := 0;
  ValidateHelpSystem;
  HookSynchronizeWakeup;
end;

destructor TApplication.Destroy;
type
  TExceptionEvent = procedure (E: Exception) of object;
var
  P: TNotifyEvent;
  E: TExceptionEvent;
begin
  UnhookSynchronizeWakeup;
  P := HandleException;
  if @P = @Classes.ApplicationHandleException then
    Classes.ApplicationHandleException := nil;
  E := ShowException;
  if @E = @Classes.ApplicationShowException then
    Classes.ApplicationShowException := nil;
  if FTestLib <> 0 then
    FreeLibrary(FTestLib);
  FActive := False;
  CancelHint;
  ShowHint := False;
  inherited Destroy;
  if FPopupControlWnd <> 0 then
    Classes.DeallocateHWnd(FPopupControlWnd);
  UnhookMainWindow(CheckIniChange);
  if (FHandle <> 0) and FHandleCreated then
  begin
    if NewStyleControls then SendMessage(FHandle, WM_SETICON, 1, 0);
    DestroyWindow(FHandle);
  end;
  if FHelpSystem <> nil then FHelpSystem := nil;
{$IFDEF LINUX}
  if FObjectInstance <> nil then WinUtils.FreeObjectInstance(FObjectInstance);
{$ENDIF}
{$IFDEF MSWINDOWS}
  if FObjectInstance <> nil then Classes.FreeObjectInstance(FObjectInstance);
{$ENDIF}
  FWindowHooks.Free;
  FTopMostList.Free;
  FPopupOwners.Free;
  FIcon.Free;
end;

procedure TApplication.CreateHandle;
var
  TempClass: TWndClass;
  SysMenu: HMenu;
begin
  if not FHandleCreated
{$IFDEF MSWINDOWS}
    and not IsConsole then
{$ENDIF}
{$IFDEF LINUX}
    then
{$ENDIF}
  begin
{$IFDEF LINUX}
    FObjectInstance := WinUtils.MakeObjectInstance(WndProc);
{$ENDIF}
{$IFDEF MSWINDOWS}
    FObjectInstance := Classes.MakeObjectInstance(WndProc);
{$ENDIF}
    WindowClass.lpfnWndProc := @DefWindowProc;
    if not GetClassInfo(HInstance, WindowClass.lpszClassName, TempClass) then
    begin
      WindowClass.hInstance := HInstance;
      if Windows.RegisterClass(WindowClass) = 0 then
        raise EOutOfResources.Create(SWindowClass);
    end;
    FHandle := CreateWindowEx(WS_EX_NOACTIVATE, WindowClass.lpszClassName, PChar(FTitle),
      WS_POPUP or WS_CAPTION or WS_CLIPSIBLINGS or WS_SYSMENU
      or WS_MINIMIZEBOX,
      GetSystemMetrics(SM_CXSCREEN) div 2,
      GetSystemMetrics(SM_CYSCREEN) div 2,
      0, 0, 0, 0, HInstance, nil);
    FHandleCreated := True;
    SetWindowLong(FHandle, GWL_WNDPROC, Longint(FObjectInstance));
    if NewStyleControls then
    begin
      SendMessage(FHandle, WM_SETICON, 1, GetIconHandle);
      SetClassLong(FHandle, GCL_HICON, GetIconHandle);
    end;
    SysMenu := GetSystemMenu(FHandle, False);
    DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
    DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
    if NewStyleControls then DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);
  end;
end;

procedure TApplication.ControlDestroyed(Control: TControl);
begin
  if FMainForm = Control then FMainForm := nil;
  if FMouseControl = Control then FMouseControl := nil;
  if Screen.FActiveControl = Control then Screen.FActiveControl := nil;
  if Screen.FActiveCustomForm = Control then
  begin
    Screen.FActiveCustomForm := nil;
    Screen.FActiveForm := nil;
  end;
  if Screen.FFocusedForm = Control then Screen.FFocusedForm := nil;
  if FHintControl = Control then FHintControl := nil;
  Screen.UpdateLastActive;
end;

type
  PTopMostEnumInfo = ^TTopMostEnumInfo;
  TTopMostEnumInfo = record
    TopWindow: HWND;
    IncludeMain: Boolean;
  end;

function GetTopMostWindows(Handle: HWND; Info: Pointer): BOOL; stdcall;
begin
  Result := True;
  if Assigned(Application) and (GetWindow(Handle, GW_OWNER) = Application.Handle) then
    if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0) and
      ((Application.MainForm = nil) or PTopMostEnumInfo(Info)^.IncludeMain or
      (Handle <> Application.MainForm.Handle)) then
      Application.FTopMostList.Add(Pointer(Handle))
    else
    begin
      PTopMostEnumInfo(Info)^.TopWindow := Handle;
      Result := False;
    end;
end;

procedure TApplication.DoNormalizeTopMosts(IncludeMain: Boolean);
var
  I: Integer;
  Info: TTopMostEnumInfo;
begin
  if FHandle <> 0 then
  begin
    if FTopMostLevel = 0 then
    begin
      Info.TopWindow := Handle;
      Info.IncludeMain := IncludeMain;
      EnumWindows(@GetTopMostWindows, Longint(@Info));
      if FTopMostList.Count <> 0 then
      begin
        Info.TopWindow := GetWindow(Info.TopWindow, GW_HWNDPREV);
        if GetWindowLong(Info.TopWindow, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then
          Info.TopWindow := HWND_NOTOPMOST;
        for I := FTopMostList.Count - 1 downto 0 do
          SetWindowPos(HWND(FTopMostList[I]), Info.TopWindow, 0, 0, 0, 0,
            SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
      end;
    end;
    Inc(FTopMostLevel);
  end;
end;

procedure TApplication.ModalStarted;
begin
  Inc(FModalLevel);
  if (FModalLevel = 1) and Assigned(FOnModalBegin) then
    FOnModalBegin(Self);
end;

procedure TApplication.ModalFinished;
begin
  Dec(FModalLevel);
  if (FModalLevel = 0) and Assigned(FOnModalEnd) then
    FOnModalEnd(Self);
end;

procedure TApplication.NormalizeTopMosts;
begin
  DoNormalizeTopMosts(False);
end;

procedure TApplication.NormalizeAllTopMosts;
begin
  DoNormalizeTopMosts(True);
end;

procedure TApplication.RemovePopupForm(APopupForm: TCustomForm);
var
  I: Integer;
begin
  for I := Low(FPopupForms) to High(FPopupForms) do
    if FPopupForms[I].Form = APopupForm then
    begin
      FPopupForms[I].Form := nil;
      FPopupForms[I].FormID := -1;
      Exit;
    end;
end;

procedure TApplication.RestoreTopMosts;
var
  I: Integer;
begin
  if (FHandle <> 0) and (FTopMostLevel > 0) then
  begin
    Dec(FTopMostLevel);
    if FTopMostLevel = 0 then
    begin
      for I := FTopMostList.Count - 1 downto 0 do
        SetWindowPos(HWND(FTopMostList[I]), HWND_TOPMOST, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
      FTopMostList.Clear;
    end;
  end;
end;

var
  OwnedAppWnds: array of HWND;

function GetPopupOwnerWindows(Handle: HWND; Info: Pointer): BOOL; stdcall;
var
  LOwner: HWND;
  Pid: DWORD;
begin
  Result := True;
  LOwner := GetWindow(Handle, GW_OWNER);
  if (LOwner <> 0) and (LOwner <> Application.Handle) then
    GetWindowThreadProcessId(LOwner, Pid)
  else
    Pid := 0;
  if (Pid = GetCurrentProcessId) and (Application.FPopupOwners.IndexOf(Pointer(LOwner)) < 0) then
    Application.FPopupOwners.Add(Pointer(LOwner));

  if (LOwner <> 0) and (Handle <> Application.Handle) and (LOwner = Application.Handle) and
     (Handle <> HWND(Info)) and IsWindowVisible(Handle) then
  begin
    SetLength(OwnedAppWnds, Length(OwnedAppWnds) + 1);
    OwnedAppWnds[Length(OwnedAppWnds) - 1] := Handle;
  end;
end;

procedure TApplication.DoShowOwnedPopups(Show: Boolean);
var
  I: Integer;
begin
  if Application.Handle <> 0 then
  begin
    if not Show then
    begin
      if FPopupLevel = 0 then
      begin
        EnumWindows(@GetPopupOwnerWindows, MainFormHandle);
        if (MainForm <> nil) and Application.MainFormOnTaskBar then
          for I := High(OwnedAppWnds) downto Low(OwnedAppWnds) do
            ShowWindow(OwnedAppWnds[I], SW_HIDE);
        for I := FPopupOwners.Count - 1 downto 0 do
          ShowOwnedPopups(HWND(FPopupOwners[I]), Show);
      end;
      Inc(FPopupLevel);
    end else if FPopupLevel > 0 then
    begin
      Dec(FPopupLevel);
      if FPopupLevel = 0 then
      begin
        if (MainForm <> nil) and Application.MainFormOnTaskBar then
          for I := High(OwnedAppWnds) downto Low(OwnedAppWnds) do
            ShowWindow(OwnedAppWnds[I], SW_SHOW);
        for I := FPopupOwners.Count - 1 downto 0 do
          ShowOwnedPopups(HWND(FPopupOwners[I]), Show);
         FPopupOwners.Clear;
        SetLength(OwnedAppWnds, 0);
      end;
    end;
  end;
end;

function TApplication.IsRightToLeft: Boolean;
begin
  Result := SysLocale.MiddleEast and (FBiDiMode <> bdLeftToRight);
end;

function TApplication.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (FBiDiMode <> bdLeftToRight);
end;

function TApplication.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (FBiDiMode = bdRightToLeft);
end;

function TApplication.UseRightToLeftScrollBar: Boolean;
begin
  Result := SysLocale.MiddleEast and
            (FBiDiMode in [bdRightToLeft, bdRightToLeftNoAlign]);
end;

function TApplication.CheckIniChange(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Message.Msg = RM_TaskbarCreated) or
     (Message.Msg = WM_WININICHANGE) then
  begin
    if UpdateFormatSettings then
    begin
      SetThreadLocale(LOCALE_USER_DEFAULT);
      GetFormatSettings;
    end;
    if UpdateMetricSettings then
      Screen.GetMetricSettings;

    if Message.Msg = RM_TaskbarCreated then
    begin
      Screen.ResetFonts;
    end;
  end;
end;

procedure TApplication.SettingChange(var Message: TWMSettingChange);
begin
  if Assigned(FOnSettingChange) then
    with Message do
      FOnSettingChange(Self, Flag, Section, Result);
end;

var
  LastActive: HWND;  // This is the window handle of the last active popup

procedure TApplication.WndProc(var Message: TMessage);
var
  I: Integer;
  SaveFocus, TopWindow: HWnd;

  procedure Default;
  begin
    with Message do
      Result := DefWindowProc(FHandle, Msg, WParam, LParam);
  end;

  procedure DrawAppIcon;
  var
    DC: HDC;
    PS: TPaintStruct;
  begin
    with Message do
    begin
      DC := BeginPaint(FHandle, PS);
      DrawIcon(DC, 0, 0, GetIconHandle);
      EndPaint(FHandle, PS);
    end;
  end;

begin
  try
    Message.Result := 0;
    for I := 0 to FWindowHooks.Count - 1 do
      if TWindowHook(FWindowHooks[I]^)(Message) then Exit;
    CheckIniChange(Message);
    with Message do
      case Msg of
        WM_SYSCOMMAND:
          case WParam and $FFF0 of
            SC_MINIMIZE: Minimize;
            SC_RESTORE: Restore;
          else
            Default;
          end;
        WM_SIZE:
          if WParam = SIZE_MINIMIZED then
            AppIconic := True;
        WM_CLOSE:
          if MainForm <> nil then MainForm.Close;
        WM_PAINT:
          if IsIconic(FHandle) then DrawAppIcon else Default;
        WM_ERASEBKGND:
          begin
            Message.Msg := WM_ICONERASEBKGND;
            Default;
          end;
        WM_QUERYDRAGICON:
          Result := GetIconHandle;
        WM_SETFOCUS:
          begin
            PostMessage(FHandle, CM_ENTER, 0, 0);
            Default;
          end;
        WM_ACTIVATEAPP:
          begin
            Default;
            FActive := TWMActivateApp(Message).Active;
            if TWMActivateApp(Message).Active then
            begin
              RestoreTopMosts;
              PostMessage(FHandle, CM_ACTIVATE, 0, 0)
            end
            else
            begin
              NormalizeTopMosts;
              PostMessage(FHandle, CM_DEACTIVATE, 0, 0);
            end;
          end;
        WM_ENABLE:
          if TWMEnable(Message).Enabled then
          begin
            if not DisablingWindows then
            begin
              RestoreTopMosts;
              if FWindowList <> nil then
              begin
                EnableTaskWindows(FWindowList);
                FWindowList := nil;
              end;
            end;
            Default;
          end else
          begin
            Default;
            if (FWindowList = nil) and not DisablingWindows then
              FWindowList := DisableTaskWindows(Handle);
            NormalizeAllTopMosts;
          end;
        WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
          Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
        WM_ENDSESSION:
          if TWMEndSession(Message).EndSession then
          begin
            CallTerminateProcs;
            Halt;
          end;
        WM_QUERYENDSESSION:
          Message.Result := 1;
        CM_ACTIONEXECUTE, CM_ACTIONUPDATE:
          Message.Result := Ord(DispatchAction(Message.Msg, TBasicAction(Message.LParam)));
        CM_APPKEYDOWN:
          if IsShortCut(TWMKey(Message)) then Result := 1;
        CM_APPSYSCOMMAND:
          if MainForm <> nil then
            with MainForm do
              if (Handle <> 0) and IsWindowEnabled(Handle) and
                IsWindowVisible(Handle) then
              begin
                FocusMessages := False;
                SaveFocus := GetFocus;
                Windows.SetFocus(Handle);
                Perform(WM_SYSCOMMAND, WParam, LParam);
                Windows.SetFocus(SaveFocus);
                FocusMessages := True;
                Result := 1;
              end;
        CM_ACTIVATE:
          begin
            if Assigned(FOnActivate) then FOnActivate(Self);
            if Assigned(Application.MainForm) and Application.MainFormOnTaskBar and
               not IsWindowEnabled(Application.MainForm.Handle) and
               (LastActive <> MainForm.Handle) then
              Windows.SetFocus(LastActive);
            LastActive := 0;
          end;
        CM_DEACTIVATE:
          begin
            LastActive := GetLastActivePopup(FHandle);
            if Assigned(FOnDeactivate) then FOnDeactivate(Self);
          end;
        CM_ENTER:
          if not IsIconic(FHandle) and (GetFocus = FHandle) then
          begin
            TopWindow := FindTopMostWindow(0);
            if TopWindow <> 0 then Windows.SetFocus(TopWindow);
          end;
        WM_HELP,
        CM_INVOKEHELP: InvokeHelp(WParam, LParam);
        CM_WINDOWHOOK:
          if wParam = 0 then
            HookMainWindow(TWindowHook(Pointer(LParam)^)) else
            UnhookMainWindow(TWindowHook(Pointer(LParam)^));
        CM_DIALOGHANDLE:
          if wParam = 1 then
            Result := FDialogHandle
          else
            FDialogHandle := lParam;
        WM_SETTINGCHANGE:
          begin
            Mouse.SettingChanged(wParam);
            SettingChange(TWMSettingChange(Message));
            Default;
          end;
        WM_FONTCHANGE:
          begin
            Screen.ResetFonts;
            Default;
          end;
        WM_THEMECHANGED:
          ThemeServices.ApplyThemeChange;
        WM_NULL:
          CheckSynchronize;
      else
        Default;
      end;
  except
    HandleException(Self);
  end;
end;

function TApplication.GetIconHandle: HICON;
begin
  Result := FIcon.Handle;
  if Result = 0 then Result := LoadIcon(0, IDI_APPLICATION);
end;

procedure TApplication.Minimize;
begin
  if not IsIconic(FHandle) then
  begin
    AppIconic := True;
    NormalizeTopMosts;
    SetActiveWindow(FHandle);
    DoShowOwnedPopups(False);
    if (MainForm <> nil) and (ShowMainForm or MainForm.Visible)
       and IsWindowEnabled(MainForm.Handle) then
    begin
      if MainFormOnTaskBar then
        MainForm.WindowState := wsMinimized
      else
      begin
        SetWindowPos(FHandle, MainForm.Handle, MainForm.Left, MainForm.Top,
          MainForm.Width, 0, SWP_SHOWWINDOW);
        DefWindowProc(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
      end;
    end
    else
      ShowWinNoAnimate(FHandle, SW_MINIMIZE);
    if Assigned(FOnMinimize) then FOnMinimize(Self);
  end;
end;

procedure TApplication.Restore;
begin
  if IsIconic(FHandle) then
  begin
    AppIconic := False;
    SetActiveWindow(FHandle);
    if (MainForm <> nil) then
      if MainFormOnTaskBar then
        ShowWindow(MainForm.Handle, SW_RESTORE)
      else
      begin
        if (ShowMainForm or MainForm.Visible) and IsWindowEnabled(MainForm.Handle) then
          DefWindowProc(FHandle, WM_SYSCOMMAND, SC_RESTORE, 0)
        else
          ShowWinNoAnimate(FHandle, SW_RESTORE);
        SetWindowPos(FHandle, 0, GetSystemMetrics(SM_CXSCREEN) div 2,
          GetSystemMetrics(SM_CYSCREEN) div 2, 0, 0, SWP_SHOWWINDOW);
      end;
    if (FMainForm <> nil) and (FMainForm.FWindowState = wsMinimized) and
      not FMainForm.Visible then
    begin
      FMainForm.WindowState := wsNormal;
      FMainForm.Show;
    end;
    RestoreTopMosts;
    DoShowOwnedPopups(True);
    if Screen.ActiveControl <> nil then
      Windows.SetFocus(Screen.ActiveControl.Handle);
    if Assigned(FOnRestore) then FOnRestore(Self);
  end;
end;

procedure TApplication.BringToFront;
var
  TopWindow: HWnd;
begin
  if Handle <> 0 then
  begin
    TopWindow := GetLastActivePopup(Handle);
    if (TopWindow <> 0) and (TopWindow <> Handle) and
      IsWindowVisible(TopWindow) and IsWindowEnabled(TopWindow) then
      SetForegroundWindow(TopWindow);
  end;
end;

function TApplication.GetTitle: string;
var
  Buffer: array[0..255] of Char;
begin
  if FHandleCreated and not MainFormOnTaskBar then
  begin
    SetString(Result, Buffer, GetWindowText(FHandle, Buffer, SizeOf(Buffer)));
    if Length(Result) = 0 then
      Result := FTitle;
  end
  else
    Result := FTitle;
end;

procedure TApplication.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TApplication.SetBiDiMode(Value: TBiDiMode);
var
  Loop: Integer;
begin
  if FBiDiMode <> Value then
  begin
    FBiDiMode := Value;
    with Screen do
      for Loop := 0 to FormCount-1 do
        Forms[Loop].Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  end;
end;

procedure TApplication.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    if FHandleCreated then
      if MainFormOnTaskBar then
        SetWindowText(FHandle, nil)
      else
        SetWindowText(FHandle, PChar(Value));
    FTitle := Value;
  end;
end;

procedure TApplication.SetHandle(Value: HWnd);
begin
  if not FHandleCreated and (Value <> FHandle) then
  begin
    if FHandle <> 0 then UnhookMainWindow(CheckIniChange);
    FHandle := Value;
    if FHandle <> 0 then HookMainWindow(CheckIniChange);
  end;
end;

function TApplication.IsDlgMsg(var Msg: TMsg): Boolean;
begin
  Result := False;
  if FDialogHandle <> 0 then
    if IsWindowUnicode(Msg.hwnd) then
      Result := IsDialogMessageW(FDialogHandle, Msg)
    else
      Result := IsDialogMessageA(FDialogHandle, Msg);
end;

function TApplication.IsMDIMsg(var Msg: TMsg): Boolean;
begin
  Result := False;
  if (MainForm <> nil) and (MainForm.FormStyle = fsMDIForm) and
     (Screen.ActiveForm <> nil) and (Screen.ActiveForm.FormStyle = fsMDIChild) then
    Result := TranslateMDISysAccel(MainForm.ClientHandle, Msg);
end;

function TApplication.IsKeyMsg(var Msg: TMsg): Boolean;
var
  Wnd: HWND;
  WndProcessID, ProcessID: Cardinal;
begin
  Result := False;
  with Msg do
    if (Message >= WM_KEYFIRST) and (Message <= WM_KEYLAST) then
    begin
      Wnd := GetCapture;
      if Wnd = 0 then
      begin
        Wnd := HWnd;
        if (MainForm <> nil) and (Wnd = MainForm.ClientHandle) then
          Wnd := MainForm.Handle
        else
        begin
          // Find the nearest VCL component.  Non-VCL windows wont know what
          // to do with CN_BASE offset messages anyway.
          // TOleControl.WndProc needs this for TranslateAccelerator
          while not IsVCLControl(Wnd) and (Wnd <> 0) do
            Wnd := GetParent(Wnd);
          if Wnd = 0 then Wnd := HWnd;
        end;
        if IsWindowUnicode(Wnd) then
        begin
          if SendMessageW(Wnd, CN_BASE + Message, WParam, LParam) <> 0 then
            Result := True;
        end else if SendMessageA(Wnd, CN_BASE + Message, WParam, LParam) <> 0 then
            Result := True;
      end
      else
      begin
        GetWindowThreadProcessId(Wnd, WndProcessId);
        GetWindowThreadProcessId(Handle, ProcessId);
        if (WndProcessID = ProcessID) then
          if SendMessage(Wnd, CN_BASE + Message, WParam, LParam) <> 0 then
            Result := True;
      end;
    end;
end;

function TApplication.IsHintMsg(var Msg: TMsg): Boolean;
begin
  Result := False;
  if (FHintWindow <> nil) and FHintWindow.IsHintMsg(Msg) then
    CancelHint;
end;

function TApplication.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := False;
  if Assigned(FOnShortCut) then FOnShortCut(Message, Result);
  Result := Result or (MainForm <> nil) and IsWindowEnabled(MainForm.Handle) and
    MainForm.IsShortCut(TWMKey(Message))
end;

procedure TApplication.PopupControlProc(var Message: TMessage);
var
  I: Integer;
begin
  case Message.Msg of
    CM_CREATEPOPUP:
      begin
        for I := Low(FPopupForms) to High(FPopupForms) do
          if (FPopupForms[I].FormID = TCMCreatePopup(Message).PopupID) and
            (FPopupForms[I].Form <> nil) then
          begin
            if FPopupForms[I].WasPopup then
              FPopupForms[I].Form.RecreateAsPopup(TCMCreatePopup(Message).OwnerWnd)
            else
            begin
              FPopupForms[I].Form.HandleNeeded;
              FPopupForms[I].Form.UpdateControlState;
            end;
            Exit;
          end;
      end;
    WM_QUERYENDSESSION:
      Message.Result := 1;
  end;
end;

function TApplication.ProcessMessage(var Msg: TMsg): Boolean;
var
  Unicode: Boolean;
  Handled: Boolean;
  MsgExists: Boolean;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
  begin
    Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
    if Unicode then
      MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
    else
      MsgExists := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
    if not MsgExists then Exit;
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      Handled := False;
      if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
      if not IsPreProcessMessage(Msg) and not IsHintMsg(Msg) and
        not Handled and not IsMDIMsg(Msg) and
        not IsKeyMsg(Msg) and not IsDlgMsg(Msg) then
      begin
        TranslateMessage(Msg);
        if Unicode then
          DispatchMessageW(Msg)
        else
          DispatchMessage(Msg);
      end;
    end
    else
      FTerminate := True;
  end;
end;

procedure TApplication.ProcessMessages;
var
  Msg: TMsg;
begin
  while ProcessMessage(Msg) do {loop};
end;

procedure TApplication.HandleMessage;
var
  Msg: TMsg;
begin
  if not ProcessMessage(Msg) then Idle(Msg);
end;

procedure TApplication.HookMainWindow(Hook: TWindowHook);
var
  WindowHook: ^TWindowHook;
begin
  if not FHandleCreated then
  begin
    if FHandle <> 0 then
      SendMessage(FHandle, CM_WINDOWHOOK, 0, Longint(@@Hook));
  end else
  begin
    FWindowHooks.Expand;
    New(WindowHook);
    WindowHook^ := Hook;
    FWindowHooks.Add(WindowHook);
  end;
end;

procedure TApplication.UnhookMainWindow(Hook: TWindowHook);
var
  I: Integer;
  WindowHook: ^TWindowHook;
begin
  if not FHandleCreated then
  begin
    if FHandle <> 0 then
      SendMessage(FHandle, CM_WINDOWHOOK, 1, Longint(@@Hook));
  end else
    for I := 0 to FWindowHooks.Count - 1 do
    begin
      WindowHook := FWindowHooks[I];
      if (TMethod(WindowHook^).Code = TMethod(Hook).Code) and
        (TMethod(WindowHook^).Data = TMethod(Hook).Data) then
      begin
        Dispose(WindowHook);
        FWindowHooks.Delete(I);
        Break;
      end;
    end;
end;

procedure TApplication.Initialize;
begin
  if InitProc <> nil then TProcedure(InitProc);
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
var
  Instance: TComponent;
begin
  Instance := TComponent(InstanceClass.NewInstance);
  TComponent(Reference) := Instance;
  try
    Instance.Create(Self);
  except
    TComponent(Reference) := nil;
    raise;
  end;
  if (FMainForm = nil) and (Instance is TForm) then
  begin
    TForm(Instance).HandleNeeded;
    FMainForm := TForm(Instance);
  end;
end;

procedure TApplication.Run;
begin
  FRunning := True;
  try
    AddExitProc(DoneApplication);
    if FMainForm <> nil then
    begin
      case CmdShow of
        SW_SHOWMINNOACTIVE: FMainForm.FWindowState := wsMinimized;
        SW_SHOWMAXIMIZED: MainForm.WindowState := wsMaximized;
      end;
      if FShowMainForm then
        if FMainForm.FWindowState = wsMinimized then
        begin
          if not MainFormOnTaskBar then
          begin
            SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_NOACTIVATE);
            ShowWindow(FHandle, SW_SHOW);
          end;
          Minimize;
        end else
          FMainForm.Visible := True;
      repeat
        try
          HandleMessage;
        except
          HandleException(Self);
        end;
      until Terminated;
    end;
  finally
    FRunning := False;
  end;
end;

procedure TApplication.Terminate;
begin
  if CallTerminateProcs then PostQuitMessage(0);
end;

function IsClass(Obj: TObject; Cls: TClass): Boolean;
var
  Parent: TClass;
begin
  Parent := Obj.ClassType;
  while (Parent <> nil) and (Parent.ClassName <> Cls.ClassName) do
    Parent := Parent.ClassParent;
  Result := Parent <> nil;  
end;

procedure TApplication.HandleException(Sender: TObject);
begin
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  if IsClass(ExceptObject, Exception) then
  begin
    if not IsClass(ExceptObject, EAbort) then
      if Assigned(FOnException) then
        FOnException(Sender, Exception(ExceptObject))
      else
        ShowException(Exception(ExceptObject));
  end else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

function TApplication.MessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
var
  ActiveWindow, TaskActiveWindow: HWnd;
  WindowList: Pointer;
  MBMonitor, AppMonitor: HMonitor;
  MonInfo: TMonitorInfo;
  Rect: TRect;
  FocusState: TFocusState;
begin
  ActiveWindow := ActiveFormHandle;
  if ActiveWindow = 0 then
    TaskActiveWindow := Handle
  else
    TaskActiveWindow := ActiveWindow;
  MBMonitor := MonitorFromWindow(ActiveWindow, MONITOR_DEFAULTTONEAREST);
  AppMonitor := MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST);
  if MBMonitor <> AppMonitor then
  begin
    MonInfo.cbSize := Sizeof(TMonitorInfo);
    GetMonitorInfo(MBMonitor, @MonInfo);
    GetWindowRect(Handle, Rect);
    SetWindowPos(Handle, 0,
      MonInfo.rcMonitor.Left + ((MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left) div 2),
      MonInfo.rcMonitor.Top + ((MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top) div 2),
      0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
  end;
  WindowList := DisableTaskWindows(ActiveWindow);
  FocusState := SaveFocusState;
  if UseRightToLeftReading then Flags := Flags or MB_RTLREADING;
  try
    Result := Windows.MessageBox(TaskActiveWindow, Text, Caption, Flags);
  finally
    if MBMonitor <> AppMonitor then
      SetWindowPos(Handle, 0,
        Rect.Left + ((Rect.Right - Rect.Left) div 2),
        Rect.Top + ((Rect.Bottom - Rect.Top) div 2),
        0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
end;

procedure TApplication.ShowException(E: Exception);
var
  Msg: string;
begin
  Msg := E.Message;
  if (Msg <> '') and (AnsiLastChar(Msg) > '.') then Msg := Msg + '.';
  MessageBox(PChar(Msg), PChar(GetTitle), MB_OK + MB_ICONSTOP);
end;

function TApplication.InvokeHelp(Command: Word; Data: Longint): Boolean;
var
  CallHelp: Boolean;
  HelpHandle: HWND;
  ActiveForm: TCustomForm;
begin
  ActiveForm := nil;
  CallHelp := True;
  Result := DoOnHelp(Command, Data, CallHelp);

  if not Result then
  begin
    ActiveForm := Screen.ActiveCustomForm;
    if Assigned(ActiveForm) and (csDesigning in ActiveForm.ComponentState) then
      CallHelp := False;
    if Assigned(ActiveForm) and (ActiveForm.TabOrder = -1) and (ActiveForm.Visible = False) and
       (not Assigned(ActiveForm.ActiveControl)) then
        CallHelp := False;
  end;

  if CallHelp then
  begin
    if Assigned(ActiveForm) and ActiveForm.HandleAllocated and (ActiveForm.FHelpFile <> '') then
    begin
      HelpHandle := ActiveForm.Handle;
      if ValidateHelpSystem then
        Result := HelpSystem.Hook(Longint(HelpHandle), ActiveForm.FHelpFile, Command, Data);
    end
    else
    if FHelpFile <> '' then
    begin
      HelpHandle := Handle;
      if FMainForm <> nil then HelpHandle := FMainForm.Handle;
      if ValidateHelpSystem then Result := HelpSystem.Hook(Longint(HelpHandle), FHelpFile, Command, Data);
    end
    else
      if not FHandleCreated then
        PostMessage(FHandle, CM_INVOKEHELP, Command, Data);
  end;
end;

function TApplication.DoOnHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
var
  ActiveForm: TCustomForm;
begin
  Result := False;
  CallHelp := True;
  ActiveForm := nil;

  { If the active control is on an embedded form, try it's OnHelp first. }
  if Screen.ActiveControl <> nil then
  begin
    ActiveForm := GetParentForm(Screen.ActiveControl, False);
    if Assigned(ActiveForm) and Assigned(ActiveForm.FOnHelp) then
      Result := ActiveForm.FOnHelp(Command, Data, CallHelp);
  end;

  if not Result then
  begin
    { Next, try the Screen.ActiveCustomForm, it not already tried }
    if (Screen.ActiveCustomForm <> nil) and (Screen.ActiveCustomForm <> ActiveForm) then
    begin
      ActiveForm := Screen.ActiveCustomForm;
      if Assigned(ActiveForm) and Assigned(ActiveForm.FOnHelp) then
        Result := ActiveForm.FOnHelp(Command, Data, CallHelp)
    end;
    { Try the application hook }
    if (not Result) and Assigned(FOnHelp) then
      Result := FOnHelp(Command, Data, CallHelp);
  end;
end;

function TApplication.HelpKeyword(const Keyword: String): Boolean;
var
  CallHelp: Boolean;
begin
  Result := DoOnHelp(HELP_COMMAND, Integer(PChar(Keyword)), CallHelp);
  if CallHelp then
  begin
    if ValidateHelpSystem then
    begin
      { We have to asume ShowHelp worked }
      Result := True;
      HelpSystem.ShowHelp(Keyword, GetCurrentHelpFile);
    end
    else
      Result := False;
  end;
end;

function TApplication.HelpContext(Context: THelpContext): Boolean;
var
  CallHelp: Boolean;
begin
  Result := DoOnHelp(HELP_CONTEXT, Context, CallHelp);
  if CallHelp then
  begin
    if ValidateHelpSystem then
    begin
      { We have to asume ShowContextHelp worked }
      Result := True;
      HelpSystem.ShowContextHelp(Context, GetCurrentHelpFile);
    end
    else
      Result := False;
  end;
end;

function TApplication.HelpCommand(Command: Integer; Data: Longint): Boolean;
begin
  Result := InvokeHelp(Command, Data);
end;

function TApplication.HelpJump(const JumpID: string): Boolean;
var
  CallHelp: Boolean;
begin
  Result := DoOnHelp(HELP_COMMAND, Integer(PChar(JumpID)), CallHelp);
  if CallHelp then
  begin
    if ValidateHelpSystem then
    begin
      { We have to asume ShowTopicHelp worked }
      Result := True;
      HelpSystem.ShowTopicHelp(JumpID, GetCurrentHelpFile);
    end
    else
      Result := False;
  end;
end;

function TApplication.HelpShowTableOfContents: Boolean;
begin
  Result := ValidateHelpSystem;
  if Result then
    Result := HelpSystem.Hook(Handle, GetCurrentHelpFile, HELP_CONTENTS, 0);
end;

function TApplication.GetExeName: string;
begin
  Result := ParamStr(0);
end;

procedure TApplication.SetShowHint(Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    if FShowHint then
    begin
      FHintWindow := HintWindowClass.Create(Self);
      FHintWindow.Color := FHintColor;
    end
    else
    begin
      FHintWindow.Free;
      FHintWindow := nil;
    end;
  end;
end;

procedure TApplication.SetHintColor(Value: TColor);
begin
  if FHintColor <> Value then
  begin
    FHintColor := Value;
    if FHintWindow <> nil then
      FHintWindow.Color := FHintColor;
  end;
end;

procedure TApplication.DoActionIdle;
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
    with Screen.CustomForms[I] do
      if HandleAllocated and IsWindowVisible(Handle) and
        IsWindowEnabled(Handle) then
        UpdateActions;
end;

function TApplication.DoMouseIdle: TControl;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := FindDragTarget(P, True);
  if FMouseControl <> Result then
    FMouseControl := Result;
end;

var
  IdleTimerHandle: Integer;

procedure IdleTimerProc(Wnd: HWnd; Msg, TimerID, SysTime: Longint); stdcall;
begin
  if Application <> nil then
  try
    KillTimer(0, IdleTimerHandle);
    IdleTimerHandle := 0;
    Application.DoActionIdle;
  except
    Application.HandleException(Application);
  end;
end;

procedure TApplication.Idle(const Msg: TMsg);
var
  Control: TControl;
  Done: Boolean;
begin
  Control := DoMouseIdle;
  if FShowHint and (FMouseControl = nil) then
    CancelHint;
  Application.Hint := GetLongHint(GetHint(Control));
  Done := True;
  try
    if Assigned(FOnIdle) then FOnIdle(Self, Done);
    if Done then
      if FActionUpdateDelay <= 0 then
        DoActionIdle
      else
        if IdleTimerHandle = 0 then
        begin
          IdleTimerHandle := SetTimer(0, 0, FActionUpdateDelay, @IdleTimerProc);
          if IdleTimerHandle = 0 then
            DoActionIdle
        end;
  except
    HandleException(Self);
  end;
{$IFDEF MSWINDOWS}
  if (GetCurrentThreadID = MainThreadID) and CheckSynchronize then
{$ENDIF}
{$IFDEF LINUX}
  if (Libc.GetCurrentThreadID = MainThreadID) and CheckSynchronize then
{$ENDIF}
    Done := False;
  if Done then WaitMessage;
end;

procedure TApplication.DoApplicationIdle;
var
  Msg: TMsg;
begin
  Msg.message := 0;
  Msg.hwnd := 0;
  Msg.wParam := 0;
  Msg.lParam := 0;
  Idle(Msg);
end;

procedure TApplication.NotifyForms(Msg: Word);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do Screen.Forms[I].Perform(Msg, 0, 0);
end;

procedure TApplication.IconChanged(Sender: TObject);
begin
  if NewStyleControls then
  begin
    SendMessage(FHandle, WM_SETICON, 1, GetIconHandle);
    SetClassLong(FHandle, GCL_HICON, GetIconHandle);
  end
  else
    if IsIconic(FHandle) then InvalidateRect(FHandle, nil, True);
  NotifyForms(CM_ICONCHANGED);
end;

procedure TApplication.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    if Assigned(FOnHint) then
      FOnHint(Self)
    else
      { Fire THintAction to anyone interested }
      with THintAction.Create(Self) do
      begin
        Hint := Value;
        try
          Execute;
        finally
          Free;
        end;
      end;
  end;
end;

var
  AppVisible: Boolean = False;

procedure TApplication.UpdateVisible;

  procedure SetVisible(Value: Boolean);
  const
    ShowFlags: array[Boolean] of Word = (
      SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
      SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
  begin
    // Dont auto-update visibility if somebody else has hidden app window
    if (IsWindowVisible(FHandle) = AppVisible) and (AppVisible <> Value) then
    begin
      SetWindowPos(FHandle, 0, 0, 0, 0, 0, ShowFlags[Value]);
      AppVisible := Value;
    end;
  end;

var
  I: Integer;
  Form: TForm;
begin
  if FHandle <> 0 then
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      Form := Screen.Forms[I];
      if Form.Visible and ((Form.ParentWindow = 0) or not Form.HandleAllocated or
        not IsChild(Form.Handle, Form.ParentWindow)) then
      begin
        SetVisible(True);
        Exit;
      end;
    end;
    SetVisible(False);
  end;
end;

function TApplication.ValidateHelpSystem: Boolean;
begin

  if FHelpSystem = nil then
  begin
    HelpIntfs.GetHelpSystem(FHelpSystem);
  end;
  Result := FHelpSystem <> nil;
end;

{ Hint window processing }

procedure TApplication.StartHintTimer(Value: Integer; TimerMode: TTimerMode);
begin
  StopHintTimer;
  FTimerHandle := SetTimer(0, 0, Value, @HintTimerProc);
  FTimerMode := TimerMode;
  if FTimerHandle = 0 then CancelHint;
end;

procedure TApplication.StopHintTimer;
begin
  if FTimerHandle <> 0 then
  begin
    KillTimer(0, FTimerHandle);
    FTimerHandle := 0;
  end;
end;

procedure TApplication.HintMouseMessage(Control: TControl; var Message: TMessage);
var
  NewHintControl: TControl;
  Pause: Integer;
  WasHintActive: Boolean;
  P: TPoint;
begin
  NewHintControl := GetHintControl(FindDragTarget(Control.ClientToScreen(SmallPointToPoint(TWMMouse(Message).Pos)), True));
  if (NewHintControl = nil) or not NewHintControl.ShowHint then
    CancelHint
  else
  begin
    if (NewHintControl <> FHintControl) or
      (not PtInRect(FHintCursorRect, Control.ClientToScreen(SmallPointToPoint(TWMMouse(Message).Pos)))) then
    begin
      WasHintActive := FHintActive;
      if WasHintActive then
        Pause := FHintShortPause else
        Pause := FHintPause;
      NewHintControl.Perform(CM_HINTSHOWPAUSE, Ord(WasHintActive), Longint(@Pause));
      { Show hint immediately if no pause }
      if WasHintActive and (Pause = 0) then
      begin
        FHintActive := WasHintActive;
        FHintControl := NewHintControl;
        GetCursorPos(P);
        ActivateHint(P);
      end
      else
      begin
        CancelHint;
        FHintActive := WasHintActive;
        FHintControl := NewHintControl;
        StartHintTimer(Pause, tmShow);
      end;
    end;
  end;
end;

procedure TApplication.HintTimerExpired;
var
  P: TPoint;
begin
  StopHintTimer;
  case FTimerMode of
    tmHide:
      HideHint;
    tmShow:
      begin
        GetCursorPos(P);
        ActivateHint(P);
      end;
  end;
end;

procedure TApplication.HideHint;
begin
  if (FHintWindow <> nil) and (FHintWindow.HandleAllocated) then
  begin
    if IsWindowVisible(FHintWindow.Handle) then
      if FHintWindow.ShouldHideHint then
      begin
        ShowWindow(FHintWindow.Handle, SW_HIDE);
        { Keep visible in sync with the actual visible-ness of the property }
        FHintWindow.Visible := False;
      end
      else
      begin
        { Restart the timer, since we want hide hint to be called again }
        StartHintTimer(FHintHidePause, tmHide)
      end;
  end;
end;

procedure TApplication.CancelHint;
begin
  if FHintControl <> nil then
  begin
    HideHint;
    FHintControl := nil;
    FHintActive := False;
    UnhookHintHooks;
    StopHintTimer;
  end;
end;

procedure TApplication.ActivateHint(CursorPos: TPoint);
var
  ClientOrigin, ParentOrigin: TPoint;
  HintInfo: THintInfo;
  CanShow: Boolean;
  HintWinRect: TRect;
  Delta: Integer;

  { Return number of scanlines between the scanline containing cursor hotspot
    and the last scanline included in the cursor mask. }
  function GetCursorHeightMargin: Integer;
  var
    IconInfo: TIconInfo;
    BitmapInfoSize, BitmapBitsSize, ImageSize: DWORD;
    Bitmap: PBitmapInfoHeader;
    Bits: Pointer;
    BytesPerScanline: Integer;

      function FindScanline(Source: Pointer; MaxLen: Cardinal;
        Value: Cardinal): Cardinal; assembler;
      asm
              PUSH    ECX
              MOV     ECX,EDX
              MOV     EDX,EDI
              MOV     EDI,EAX
              POP     EAX
              REPE    SCASB
              MOV     EAX,ECX
              MOV     EDI,EDX
      end;

  begin
    { Default value is entire icon height }
    Result := GetSystemMetrics(SM_CYCURSOR);
    if GetIconInfo(GetCursor, IconInfo) then
    try
      GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
      Bitmap := AllocMem(DWORD(BitmapInfoSize) + BitmapBitsSize);
      try
      Bits := Pointer(DWORD(Bitmap) + BitmapInfoSize);
      if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and
        (Bitmap^.biBitCount = 1) then
      begin
        { Point Bits to the end of this bottom-up bitmap }
        with Bitmap^ do
        begin
          BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
          ImageSize := biWidth * BytesPerScanline;
          Bits := Pointer(DWORD(Bits) + BitmapBitsSize - ImageSize);
          { Use the width to determine the height since another mask bitmap
            may immediately follow }
          Result := FindScanline(Bits, ImageSize, $FF);
          { In case the and mask is blank, look for an empty scanline in the
            xor mask. }
          if (Result = 0) and (biHeight >= 2 * biWidth) then
            Result := FindScanline(Pointer(DWORD(Bits) - ImageSize),
            ImageSize, $00);
          Result := Result div BytesPerScanline;
        end;
        Dec(Result, IconInfo.yHotSpot);
      end;
      finally
        FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
      end;
    finally
      if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
      if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
    end;
  end;

  procedure ValidateHintWindow(HintClass: THintWindowClass);
  begin
    if HintClass = nil then HintClass := HintWindowClass;
    if (FHintWindow = nil) or (FHintWindow.ClassType <> HintClass) then
    begin
      FHintWindow.Free;
      FHintWindow := HintClass.Create(Self);
    end;
  end;

  function MultiLineWidth(const Value: string): Integer;
  var
    W: Integer;
    P, Start: PChar;
    S: string;
  begin
    Result := 0;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do
          P := StrNextChar(P);
        SetString(S, Start, P - Start);
        W := FHintWindow.Canvas.TextWidth(S);
        if W > Result then
          Result := W;
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  end;

begin
  FHintActive := False;
  HintInfo.ReshowTimeout := 0;
  if FShowHint and (FHintControl <> nil) and ForegroundTaskCheck(EnumAllWindowsOnActivateHint) and
    (FHintControl = GetHintControl(FindDragTarget(CursorPos, True))) then
  begin
    HintInfo.HintControl := FHintControl;
    HintInfo.HintPos := CursorPos;
    Inc(HintInfo.HintPos.Y, GetCursorHeightMargin);
    HintInfo.HintMaxWidth := Screen.Width;
    HintInfo.HintColor := FHintColor;
    HintInfo.CursorRect := FHintControl.BoundsRect;
    ClientOrigin := FHintControl.ClientOrigin;
    ParentOrigin.X := 0;
    ParentOrigin.Y := 0;
    if FHintControl.Parent <> nil then
      ParentOrigin := FHintControl.Parent.ClientOrigin
    else if (FHintControl is TWinControl) and
      (TWinControl(FHintControl).ParentWindow <> 0) then
      Windows.ClientToScreen(TWinControl(FHintControl).ParentWindow, ParentOrigin);
    OffsetRect(HintInfo.CursorRect, ParentOrigin.X - ClientOrigin.X,
      ParentOrigin.Y - ClientOrigin.Y);
    HintInfo.CursorPos := FHintControl.ScreenToClient(CursorPos);
    HintInfo.HintStr := GetShortHint(GetHint(FHintControl));
    HintInfo.HideTimeout := FHintHidePause;
    HintInfo.HintWindowClass := HintWindowClass;
    HintInfo.HintData := nil;
    CanShow := FHintControl.Perform(CM_HINTSHOW, 0, Longint(@HintInfo)) = 0;
    if CanShow and Assigned(FOnShowHint) then
      FOnShowHint(HintInfo.HintStr, CanShow, HintInfo);
    FHintActive := CanShow and (FHintControl <> nil);
    if FHintActive and (HintInfo.HintStr <> '') then
    begin
      ValidateHintWindow(HintInfo.HintWindowClass);
      { make the hint have the same BiDiMode as the activating control }
      FHintWindow.BiDiMode := FHintControl.BiDiMode;
      { calculate the width of the hint based on HintStr and MaxWidth }
      with HintInfo do
        HintWinRect := FHintWindow.CalcHintRect(HintMaxWidth, HintStr, HintData);
      OffsetRect(HintWinRect, HintInfo.HintPos.X, HintInfo.HintPos.Y);
      if FHintWindow.UseRightToLeftAlignment then
        with HintWinRect do
        begin
          Delta := MultiLineWidth(HintInfo.HintStr) + 5;
          Dec(Left, Delta);
          Dec(Right, Delta);
        end;

      { Convert the client's rect to screen coordinates }
      with HintInfo do
      begin
        FHintCursorRect.TopLeft := FHintControl.ClientToScreen(CursorRect.TopLeft);
        FHintCursorRect.BottomRight := FHintControl.ClientToScreen(CursorRect.BottomRight);
      end;

      FHintWindow.Color := HintInfo.HintColor;
      FHintWindow.ActivateHintData(HintWinRect, HintInfo.HintStr, HintInfo.HintData);
      HookHintHooks;
      if HintInfo.ReshowTimeout > 0 then
        StartHintTimer(HintInfo.ReshowTimeout, tmShow)
      else
        StartHintTimer(HintInfo.HideTimeout, tmHide);
      Exit;
    end;
  end;
  if HintInfo.ReshowTimeout > 0 then
    StartHintTimer(HintInfo.ReshowTimeout, tmShow)
  else
    CancelHint;
end;

function TApplication.AddPopupForm(APopupForm: TCustomForm): Integer;
var
  I: Integer;
begin
  I := Low(FPopupForms);
  while I < Length(FPopupForms) do
  begin
    if FPopupForms[I].Form = APopupForm then
    begin
      Result := FPopupForms[I].FormID;
      Exit;
    end else if FPopupForms[I].FormID = -1 then
      Break;
    Inc(I);
  end;
  Result := FCurrentPopupID;
  Inc(FCurrentPopupID);
  if I >= Length(FPopupForms) then
  begin
    I := Length(FPopupForms);
    SetLength(FPopupForms, I + 1);
  end;
  FPopupForms[I].FormID := Result;
  FPopupForms[I].Form := APopupForm;
  FPopupForms[I].WasPopup := APopupForm.FInternalPopupParentWnd <> 0;
end;

function TApplication.GetCurrentHelpFile: string;
var
  ActiveForm: TCustomForm;
begin
  ActiveForm := Screen.ActiveCustomForm;
  if Assigned(ActiveForm) and (ActiveForm.FHelpFile <> '') then
    Result := ActiveForm.HelpFile
  else
    Result := HelpFile;
end;

function TApplication.GetDialogHandle: HWND;
begin
  if not FHandleCreated then
    Result := SendMessage(Handle, CM_DIALOGHANDLE, 1, 0)
  else
    Result := FDialogHandle;
end;

procedure TApplication.SetDialogHandle(Value: HWND);
begin
  if not FHandleCreated then
    SendMessage(Handle, CM_DIALOGHANDLE, 0, Value);
  FDialogHandle := Value;
end;

function TApplication.GetActiveFormHandle: HWND;
begin
  Result := 0;
  if Assigned(FOnGetActiveFormHandle) then
    FOnGetActiveFormHandle(Result);
  if Result = 0 then
    Result := GetActiveWindow;
  if Result = 0 then
    Result := GetLastActivePopup(Handle);
end;

function TApplication.GetMainFormHandle: HWND;
begin
  Result := 0;
  if Assigned(FOnGetMainFormHandle) then
    FOnGetMainFormHandle(Result);
  if (Result = 0) and Assigned(FMainForm) then
    Result := FMainForm.Handle;
end;

function TApplication.DispatchAction(Msg: Longint; Action: TBasicAction): Boolean;
var
  Form: TCustomForm;
begin
  Form := Screen.ActiveCustomForm;
  Result := (Form <> nil) and (Form.Perform(Msg, 0, Longint(Action)) = 1) or
    (MainForm <> Form) and (MainForm <> nil) and
    (MainForm.Perform(Msg, 0, Longint(Action)) = 1);
  { Disable action if no "user" handler is available }
  if not Result and (Action is TCustomAction) and TCustomAction(Action).Enabled and
     TCustomAction(Action).DisableIfNoHandler and
     not (csDesigning in Action.ComponentState) then
    TCustomAction(Action).Enabled := Assigned(Action.OnExecute);
end;

function TApplication.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnActionExecute) then FOnActionExecute(Action, Result);
end;

function TApplication.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnActionUpdate) then FOnActionUpdate(Action, Result);
end;

procedure InitProcs;
const
  sUser32 = 'User32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
     @SetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
end;

procedure TApplication.WakeMainThread(Sender: TObject);
begin
  PostMessage(Handle, WM_NULL, 0, 0);
end;

procedure TApplication.HookSynchronizeWakeup;
begin
  Classes.WakeMainThread := WakeMainThread;
end;

procedure TApplication.UnhookSynchronizeWakeup;
begin
  Classes.WakeMainThread := nil;
end;

function TApplication.IsPreProcessMessage(var Msg: TMsg): Boolean;
var
  Wnd: HWND;
  Control: TWinControl;
begin
  Result := False;
  Wnd := GetCapture;
  if Wnd = 0 then
  begin
    Wnd := Msg.hwnd;
    if (MainForm <> nil) and (Wnd = MainForm.ClientHandle) then
      Control := MainForm
    else
    begin
      Control := FindControl(Wnd);
      while Control = nil do
      begin
        Wnd := GetParent(Wnd);
        if Wnd <> 0 then
          Control := FindControl(Wnd)
        else
          Break;
      end;
    end;
    if Control <> nil then
      Result := Control.PreProcessMessage(Msg);
  end;
end;

{ TCustomFormHelper }

function TCustomFormHelper.GetGlassFrame: TGlassFrame;
begin
  Result := PPixelsPerInchOverload(FPixelsPerInch).GlassFrame;
end;

procedure TCustomFormHelper.ReadGlassFrameBottom(Reader: TReader);
begin
  GlassFrame.FBottom := Reader.ReadInteger;
end;

procedure TCustomFormHelper.ReadGlassFrameEnabled(Reader: TReader);
begin
  GlassFrame.FEnabled := Reader.ReadBoolean;
end;

procedure TCustomFormHelper.ReadGlassFrameLeft(Reader: TReader);
begin
  GlassFrame.FLeft := Reader.ReadInteger;
end;

procedure TCustomFormHelper.ReadGlassFrameRight(Reader: TReader);
begin
  GlassFrame.FRight := Reader.ReadInteger;
end;

procedure TCustomFormHelper.ReadGlassFrameSheetOfGlass(Reader: TReader);
begin
  GlassFrame.FSheetOfGlass := Reader.ReadBoolean;
end;

procedure TCustomFormHelper.ReadGlassFrameTop(Reader: TReader);
begin
  GlassFrame.FTop := Reader.ReadInteger;
end;

procedure TCustomFormHelper.SetGlassFrame(const Value: TGlassFrame);
begin
  PPixelsPerInchOverload(FPixelsPerInch).GlassFrame.Assign(Value);
end;

procedure TCustomFormHelper.UpdateGlassFrame(Sender: TObject);
var
  LMargins: UxTheme.TMargins;
begin
  if DwmCompositionEnabled and HandleAllocated then
  begin
    with LMargins, GlassFrame do
    begin
      if Enabled then
      begin
        if not SheetOfGlass then
        begin
          cxLeftWidth := Left;
          cxRightWidth := Right;
          cyTopHeight := Top;
          cyBottomHeight := Bottom;
        end
        else
        begin
          cxLeftWidth := -1;
          cxRightWidth := -1;
          cyTopHeight := -1;
          cyBottomHeight := -1;
        end;
      end
      else
        FillChar(LMargins, SizeOf(LMargins), 0);
    end;
    if (csDesigning in ComponentState) then
      InvalidateRect(Handle, nil, True)
    else
    begin
      DwmExtendFrameIntoClientArea(Handle, LMargins);
      Invalidate;
    end;
  end
  else
    if (csDesigning in ComponentState) then
      InvalidateRect(Handle, nil, True)
end;

procedure TCustomFormHelper.WriteGlassFrameBottom(Writer: TWriter);
begin
  Writer.WriteInteger(GlassFrame.FBottom);
end;

procedure TCustomFormHelper.WriteGlassFrameEnabled(Writer: TWriter);
begin
  Writer.WriteBoolean(GlassFrame.FEnabled);
end;

procedure TCustomFormHelper.WriteGlassFrameLeft(Writer: TWriter);
begin
  Writer.WriteInteger(GlassFrame.FLeft);
end;

procedure TCustomFormHelper.WriteGlassFrameRight(Writer: TWriter);
begin
  Writer.WriteInteger(GlassFrame.FRight);
end;

procedure TCustomFormHelper.WriteGlassFrameSheetOfGlass(Writer: TWriter);
begin
  Writer.WriteBoolean(GlassFrame.FSheetOfGlass);
end;

procedure TCustomFormHelper.WriteGlassFrameTop(Writer: TWriter);
begin
  Writer.WriteInteger(GlassFrame.FTop);
end;

{ TGlassFrame }

constructor TGlassFrame.Create(Client: TCustomForm);
begin
  inherited Create;
  FClient := Client;
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  FSheetOfGlass := False;
end;

procedure TGlassFrame.Assign(Source: TPersistent);
begin
  if Source is TGlassFrame then
  begin
    FEnabled := TGlassFrame(Source).Enabled;
    FLeft := TGlassFrame(Source).Left;
    FTop := TGlassFrame(Source).Top;
    FRight := TGlassFrame(Source).Right;
    FBottom := TGlassFrame(Source).Bottom;
    FSheetOfGlass := TGlassFrame(Source).SheetOfGlass;
    Change;
  end else
    inherited Assign(Source);
end;

procedure TGlassFrame.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TGlassFrame.FrameExtended: Boolean;
begin
  Result := FEnabled and DwmCompositionEnabled and
    (FSheetOfGlass or (Left <> 0) or (Top <> 0) or (Right <> 0) or (Bottom <> 0));
end;

function TGlassFrame.IntersectsControl(Control: TControl): Boolean;
var
  ControlRect: TRect;
  NonGlassRect: TRect;
begin
  Result := False;
  if FEnabled and DwmCompositionEnabled then
  begin
    Result := FSheetOfGlass;
    if not Result then
    begin
      ControlRect := Control.ClientRect;
      ControlRect.TopLeft := Control.ClientToParent(ControlRect.TopLeft, FClient);
      ControlRect.BottomRight := Control.ClientToParent(ControlRect.BottomRight, FClient);
      NonGlassRect := FClient.ClientRect;
      NonGlassRect := Rect(FLeft, FTop, NonGlassRect.Right - FRight,
        NonGlassRect.Bottom - FBottom);
      Result := not (PtInRect(NonGlassRect, ControlRect.TopLeft) and
        PtInRect(NonGlassRect, ControlRect.BottomRight));
    end;
  end;
end;

procedure TGlassFrame.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled and FClient.TransparentColor then
      FClient.TransparentColor := False;  // GlassFrame and TransparentColor are mutually exclusive 
    Change;
  end;  
end;

procedure TGlassFrame.SetExtendedFrame(Index: Integer; Value: Integer);
begin
  case Index of
    0:
      if Value <> FLeft then
      begin
        FLeft := Value;
        if Value = -1 then
          FSheetOfGlass := True;
      end;
    1:
      if Value <> FTop then
      begin
        FTop := Value;
        if Value = -1 then
          FSheetOfGlass := True;
      end;
    2:
      if Value <> FRight then
      begin
        FRight := Value;
        if Value = -1 then
          FSheetOfGlass := True;
      end;
    3:
      if Value <> FBottom then
      begin
        FBottom := Value;
        if Value = -1 then
          FSheetOfGlass := True;
      end;
    else
      Exit;
  end;
  Change;
end;

procedure TGlassFrame.SetSheetOfGlass(Value: Boolean);
begin
  if (Value <> FSheetOfGlass) and not
     (FSheetOfGlass and ((FLeft = -1) or (FTop = -1) or (FRight = -1) or (FBottom = -1))) then
  begin
    FSheetOfGlass := Value;
    Change;
  end;
end;

{ TApplicationHelper }

var
  AppMainFormOnTaskBar: Boolean;

function TApplicationHelper.GetMainFormOnTaskBar: Boolean;
begin
  Result := AppMainFormOnTaskBar;
end;

// Allow a MainForm which is functioning as the TaskBar button for the application
// to properly handle restoring the application
procedure TApplicationHelper.InternalRestore;
begin
  if IsIconic(FHandle) then
  begin
    AppIconic := False;
    SetActiveWindow(FHandle);
    if (FMainForm.FWindowState = wsMinimized) and not FMainForm.Visible then
    begin
      FMainForm.WindowState := wsNormal;
      FMainForm.Show;
    end;
    RestoreTopMosts;
    DoShowOwnedPopups(True);
    if Screen.ActiveControl <> nil then
      Windows.SetFocus(Screen.ActiveControl.Handle);
    if Assigned(FOnRestore) then FOnRestore(Self);
  end;
end;

procedure TApplicationHelper.SetEnumAllWindowsOnActivateHint(Flag: Boolean);
begin
  EnumAllWinOnActivateHintFlag := Flag;
end;

procedure TApplicationHelper.SetMainFormOnTaskBar(const Value: Boolean);
var
  ActivateFrm: Boolean;
begin
  if Value <> AppMainFormOnTaskBar then
  begin
    if (MainForm <> nil) and MainForm.HandleAllocated and
       not (csDesigning in MainForm.ComponentState) and
       not (csReading in MainForm.ComponentState) and (MainForm.FormStyle <> fsMDIChild) then
      if Value then
      begin
        ChangeAppWindow(FHandle, False, False);
        ChangeAppWindow(MainForm.Handle, True, MainForm.Visible);
      end
      else
      begin
        ActivateFrm := GetForeGroundWindow = MainForm.Handle;
        ChangeAppWindow(MainForm.Handle, False, MainForm.Visible);
        // Restore the TApplication window's taskbar style
        ChangeAppWindow(FHandle, True, True);
        if ActivateFrm then
          SetForeGroundWindow(MainForm.Handle);
      end;
    // Prevent TApplication Window from appearing in Task Manager's Application list
    if Value then
      SetWindowText(FHandle, nil)
    else
      SetWindowText(FHandle, PChar(FTitle));
    AppMainFormOnTaskBar := Value;
  end;
end;

function TApplicationHelper.GetEnumAllWindowsOnActivateHint: Boolean;
begin
  Result := EnumAllWinOnActivateHintFlag;
end;

procedure SetCustomFormGlassFrame(const CustomForm: TCustomForm; const GlassFrame: TGlassFrame);
begin
  CustomForm.GlassFrame := GlassFrame;
end;

function GetCustomFormGlassFrame(const CustomForm: TCustomForm): TGlassFrame;
begin
  Result := CustomForm.GlassFrame;
end;

procedure SetApplicationMainFormOnTaskBar(const Application: TApplication; Value: Boolean);
begin
  Application.MainFormOnTaskBar := Value;
end;

function GetApplicationMainFormOnTaskBar(const Application: TApplication): Boolean;
begin
  Result := Application.MainFormOnTaskBar;
end;

initialization
  InitProcs;
  RM_TaskBarCreated := RegisterWindowMessage('TaskbarCreated');
  Classes.RegisterFindGlobalComponentProc(FindGlobalComponent);
  IdleTimerHandle := 0;

finalization
  if Application <> nil then
    DoneApplication;
  if HintDoneEvent <> 0 then
    CloseHandle(HintDoneEvent);
  Classes.UnregisterFindGlobalComponentProc(FindGlobalComponent);

end.