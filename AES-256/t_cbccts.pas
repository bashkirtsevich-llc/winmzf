{-Test prog for AES CBC cipher text stealing, we Sep.2003}

program T_CBCCTS;

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
  aes_type, aes_cbc, mem_util;


const
  BSIZE = $400;

var
  Context: TAESContext;
  i,n,Err: integer;
  pt, pt0, ct, ct0, pd: array[1..BSIZE+2] of byte;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);
      IV : TAESBlock =            ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

begin
  writeln;
  writeln('=====================================');
  writeln('Test for AES-CBC cipher text stealing');

  for i:=1 to BSIZE do pt0[i] := random(256);
  pt := pt0;

  for n:=1 to BSIZE do begin
    Err := AES_CBC_Init_Encr(key128, 128, IV, context);
    Err := AES_CBC_Encrypt(@pt, @ct, n, context);
    if not compmem(@pt,@pt0,n+2) then begin
      writeln('Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      Err := AES_CBC_Init_Decr(key128, 128, IV, context);
      Err := AES_CBC_Decrypt(@ct, @pd, n, context);
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln(n:6, ' Diff');
      end;
      if not compmem(@ct,@ct0,n+2) then begin
        writeln('Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then writeln(n:6, ' Error: ', Err);
  end;
end.
