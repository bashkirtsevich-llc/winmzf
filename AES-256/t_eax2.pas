{-Test prog for EAX, we Jun.2004}
{-Reproduce AES part of Tom St Denis' EAX_TV.TXT}

program T_EAX2;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  AES_Type, AES_EAX, Mem_Util;



{---------------------------------------------------------------------------}
procedure test;
  {-Reproduce AES part of Tom St Denis' EAX_TV.TXT}
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
  buf32: array[0..31] of byte = ($64,$d8,$42,$b6,$67,$96,$a7,$97,
                                 $c2,$b4,$c6,$90,$57,$42,$fd,$f2,
                                 $14,$8f,$fc,$44,$5e,$19,$2f,$9e,
                                 $03,$b5,$38,$10,$c0,$82,$f7,$88);

  tag32: array[0..15] of byte = ($97,$78,$b3,$45,$ec,$12,$d2,$22,
                                 $dc,$c6,$db,$ab,$d2,$65,$17,$50);
var
  err,n: integer;
  ctx: TAES_EAXContext;
  key, tag: TAESBlock;
  buf: array[0..63] of byte;
begin
  {Uppercase from HexStr}
  HexUpper := true;
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=0 to 32 do begin
    err := AES_EAX_Init(key, 128, hex32, n, ctx);
    if err=0 then err := AES_EAX_Provide_Header(@hex32,n,ctx);
    if err=0 then err := AES_EAX_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then begin
      AES_EAX_Final(tag, ctx);
      writeln(n:3,': ', HexStr(@buf,n), ', ', HexStr(@tag,16));
      {key for step n>1 is the tag of the previous step repeated}
      key := tag;
    end
    else begin
      writeln('Error ',err);
      halt;
    end;
  end;
  {compare final values}
  if not compmem(@buf32, @buf, sizeof(buf32)) then writeln('** Diff: buf32');
  if not compmem(@tag32, @tag, sizeof(tag32)) then writeln('** Diff: tag32');
end;

begin
  test;
end.
