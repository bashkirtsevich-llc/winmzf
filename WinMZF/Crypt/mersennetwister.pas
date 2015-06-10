unit mersennetwister; 

interface 

{ $DEFINE WITHMAIN} 
{$IFDEF WITHMAIN}uses SysUtils;{$ENDIF} 

// Period parameters 
const 
  N=624; 
  M=397; 
  MATRIX_A=Cardinal($9908b0df);   // constant vector a 
  UPPER_MASK=Cardinal($80000000); // most significant w-r bits 
  LOWER_MASK=Cardinal($7fffffff); // least significant r bits 

var 
  mt: Array [0..N-1] of Cardinal;  // the array for the state vector 
  mti: Cardinal=N+1;  // mti==N+1 means mt[N] is not initialized 

procedure init_genrand(s: Cardinal); 
procedure init_by_array(init_key: Array of Cardinal; key_length: Integer); 
function genrand_int32(): Cardinal; 
function genrand_int31(): Longint; 
function genrand_real1(): Double; 
function genrand_real2(): Double; 
function genrand_real3(): Double; 
function genrand_res53(): Double; 
{$IFDEF WITHMAIN}function main(): Integer;{$ENDIF} 

implementation 

//initializes mt[N] with a seed 
procedure init_genrand(s: Cardinal); 
begin 
    mt[0]:=s and Cardinal($ffffffff); 
    mti:=1; 
    while mti         mt[mti] := 
       (Cardinal(1812433253) * (mt[mti-1] xor (mt[mti-1] shr 30)) + mti); 
        (* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */ 
        /* In the previous versions, MSBs of the seed affect   */ 
        /* only MSBs of the array mt[].                        */ 
        /* 2002/01/09 modified by Makoto Matsumoto             *) 
        mt[mti] := mt[mti] and Cardinal($ffffffff); 
        // for >32 bit machines 
        inc(mti); 
    end; 
end; 

(* initialize by an array with array-length */ 
/* init_key is the array for initializing keys */ 
/* key_length is its length */ 
/* slight change for C++, 2004/2/26 *) 
procedure init_by_array(init_key: Array of Cardinal; key_length: Integer); 
var 
  i, j, k: Integer; 
begin 
    init_genrand(19650218); 
    i:=1; 
    j:=0; 
    if N>key_length then k:=N 
                    else k:=key_length; 

    while k<>0 do begin 
        mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1664525)) 
          + init_key[j] + j; // non linear 
        mt[i] := mt[i] and $ffffffff; // for WORDSIZE > 32 machines 
        inc(i); 
        inc(j); 
        if (i>=N) then begin 
          mt[0] := mt[N-1]; 
          i := 1; 
        end; 
        if (j>=key_length) then j:=0; 
        dec(k); 
    end; 
    k:=N-1; 
    while k<>0 do begin 
        mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1566083941)) 
          - i; // non linear 
        mt[i] := mt[i] and $ffffffff; // for WORDSIZE > 32 machines 
        inc(i); 
        if (i>=N) then begin 
          mt[0] := mt[N-1]; 
          i := 1; 
        end; 
        dec(k); 
    end; 

    mt[0] := Cardinal($80000000); // MSB is 1; assuring non-zero initial array 
end; 

// generates a random number on [0,0xffffffff]-interval 
function genrand_int32(): Cardinal; 
var 
  y: Cardinal; 
  mag01: Array [0..1] of Cardinal; 
  kk: Integer; 
begin 
    mag01[0]:=0; 
    mag01[1]:=MATRIX_A; 
    // mag01[x] = x * MATRIX_A  for x=0,1 

    if (mti >= N) then begin // generate N words at one time 

        if (mti = N+1) then   // if init_genrand() has not been called, 
            init_genrand(5489); // a default initial seed is used 

        kk:=0; 
        while kk             y := (mt[kk] and UPPER_MASK) or (mt[kk+1] and LOWER_MASK); 
            mt[kk] := mt[kk+M] xor (y shr 1) xor mag01[y and 1]; 
            inc(kk); 
        end; 
        while kk             y := (mt[kk] and UPPER_MASK) or (mt[kk+1] and LOWER_MASK); 
            mt[kk] := mt[kk+(M-N)] xor (y shr 1) xor mag01[y and 1]; 
            inc(kk); 
        end; 
        y := (mt[N-1] and UPPER_MASK) or (mt[0] and LOWER_MASK); 
        mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and 1]; 

        mti := 0; 
    end; 

    y := mt[mti]; 
    inc(mti); 

    // Tempering 
    y := y xor (y shr 11); 
    y := y xor (y shl 7) and Cardinal($9d2c5680); 
    y := y xor (y shl 15) and Cardinal($efc60000); 
    y := y xor (y shr 18); 

    Result := y; 
end; 

// generates a random number on [0,0x7fffffff]-interval 
function genrand_int31(): Longint; 
begin 
    Result:=Longint(genrand_int32() shr 1); 
end; 

// generates a random number on [0,1]-real-interval 
function genrand_real1(): Double; 
begin 
    Result := genrand_int32()*(1.0/4294967295.0); 
    // divided by 2^32-1 
end; 

// generates a random number on [0,1)-real-interval 
function genrand_real2(): Double; 
begin 
    Result := genrand_int32()*(1.0/4294967296.0); 
    // divided by 2^32 
end; 

// generates a random number on (0,1)-real-interval 
function genrand_real3(): Double; 
begin 
    Result := ((genrand_int32()) + 0.5)*(1.0/4294967296.0); 
    // divided by 2^32 
end; 

// generates a random number on [0,1) with 53-bit resolution 
function genrand_res53(): Double; 
var 
  a,b: Cardinal; 
begin 
    a:=genrand_int32() shr 5; 
    b:=genrand_int32() shr 6; 
    Result := (a*67108864.0+b)*(1.0/9007199254740992.0); 
end; 
// These real versions are due to Isaku Wada, 2002/01/09 added 

{$IFDEF WITHMAIN} 
function main(): Integer; 
var 
  i: Integer; 
  init: Array [0..3] of Cardinal; 
  length: Cardinal; 
begin 
    init[0]:=$123; 
    init[1]:=$234; 
    init[2]:=$345; 
    init[3]:=$456; 
    length:=4; 

    init_by_array(init, length); 
    WriteLn('1000 outputs of genrand_int32()'); 
    i:=0; 
    while i<1000 do begin 
      Write(Format('%10u ', [genrand_int32()])); 
      if (i mod 5=4) then WriteLn(''); 
      inc(i); 
    end; 
    WriteLn(#13#10'1000 outputs of genrand_real2()'); 
    DecimalSeparator:='.'; 
    i:=0; 
    while i<1000 do begin 
      Write(Format('%10.8f ', [genrand_real2()])); 
      if (i mod 5=4) then WriteLn(''); 
      inc(i); 
    end; 
    Result := 0; 
end; 
{$ENDIF} 

end.