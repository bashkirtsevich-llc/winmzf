unit PNGToolBar_u;

interface

uses PngImage,Graphics,Controls;

procedure LoadIcons(H,W:Byte;BackGround:TColor;Images:Array Of String;Max:Byte;ImgList:TImageList);

implementation

procedure LoadIcons(H,W:Byte;BackGround:TColor;Images:Array Of String;Max:Byte;ImgList:TImageList);
var
   PNG: TPNGObject;
   BMP: TBitmap;
   I: integer;
begin
  BMP := TBitmap.Create;
  BMP.Width:=W;
  BMP.Height:=H;
  //BMP.Canvas.Brush.Color:=BackGround;
  //BMP.Canvas.FillRect(BMP.Canvas.ClipRect);
  {ImgList.BlendColor:=BackGround;
  ImgList.BkColor:=BackGround;
  }
  PNG:= TPNGObject.Create;
  PNG.TransparentColor:=BackGround;
  PNG.Transparent:=True;
  
  For I:=0 to Max-1 do
  Begin
    //BMP.Canvas.Brush.Color:=BackGround;
    //BMP.Canvas.FillRect(BMP.Canvas.ClipRect);
    PNG.LoadFromResourceName(hInstance,Images[i]);
    PNG.CreateAlpha;
    BMP.Canvas.Draw(0, 0, PNG);
    BMP.Transparent:=True;
    ImgList.Add(bmp,Nil);
  End;
  {BMP.Canvas.Draw(0, 0, PNG);
  ImgList.Add(bmp,nil);}
  PNG.free;
  BMP.free;
end;

end.
 