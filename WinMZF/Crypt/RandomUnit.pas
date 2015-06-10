unit RandomUnit;

interface

procedure RandomizeMT;
procedure SetRandSeedMT(seed: LongWord);
function GenRandMT: double;
function GenRandIntMT: LongWord;

implementation

const
  { Period parameters }
  N = 624;
  M = 397;
  MATRIX_A   = $9908b0df;   { constant vector a }
  UPPER_MASK = $80000000; { most significant w-r bits }
  LOWER_MASK = $7fffffff; { least significant r bits }
{ Tempering parameters }
  TEMPERING_MASK_B = $9d2c5680;
  TEMPERING_MASK_C = $efc60000;
  mti: LongInt = N+1; { mti==N+1 means mt[N] is not initialized }

var
  RandSeed: Longint;
  mt: array[0..N - 1] of Int64; { the array for the state vector  }

{ initializing the array with a NONZERO seed }
procedure SetRandSeedMT(seed: LongWord);
begin
  mt[0] := seed and $ffffffff;
  mti := 1;
  while(mti<N)do
  begin
    mt[mti] := ((69069 * mt[mti-1]) and $ffffffff);
    Inc(mti);
  end;
end;

function GenRandMT: double;
const
  mag01: array[0..1] of LongWord =($0, MATRIX_A);
var
  y: LongWord;
  kk: LongInt;
begin
  if (mti >= N) then
  begin
    if (mti = (N+1))then
      SetRandSeedMT(4357);
    kk := 0;
    while(kk<(N-M))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+M] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    while(kk<(n-1))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+(M-N)] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    y := (mt[N-1]and UPPER_MASK)or(mt[0]and LOWER_MASK);
    mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];
    mti := 0;
  end;
  y := mt[mti];
  Inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7) and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y * 2.3283064370807974e-10;
end;

function GenRandIntMT: LongWord;
const
  mag01: array[0..1] of LongWord =($0, MATRIX_A);
var
  y: LongWord;
  kk: LongInt;
begin
  if (mti >= N) then
  begin
    if (mti = (N+1))then
      SetRandSeedMT(4357);
    kk := 0;
    while(kk<(N-M))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+M] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    while(kk<(n-1))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+(M-N)] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    y := (mt[N-1]and UPPER_MASK)or(mt[0]and LOWER_MASK);
    mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];
    mti := 0;
  end;
  y := mt[mti];
  Inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7) and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y;
end;

procedure RandomizeMT;
var
  OldRandSeed: longint;
begin
  OldRandSeed := System.randseed;
  System.randomize;
  SetRandSeedMT(System.randSeed);
  System.randseed := OldRandSeed;
end;
