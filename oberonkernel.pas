(* ETH Oberon, Copyright 1990-2003 Computer Systems Institute, ETH Zurich, CH-8092 Zurich.
Refer to the license.txt file provided with this distribution. *)

Unit OberonKernel;    (** non-portable *) (* rc/ard/nm/jm/jdv/pjm/rml*)

Interface

(** Module Kernel is responsible for memory allocation, garbage collection and other run-time
support for the Oberon language, as well as interfacing to the underlying machine.  Using features
exported from module Kernel may result in an unportable module, as not all Oberon Kernels are the same.
This module is of little importance to most Oberon programmers. *)
CONST
  (* timer *)
  TimeUnit = 1000;   (** timer ticks per second, returned by GetTimer *)

TYPE
  ADDRESS = LONGINT;
//  PTR = ^ANYREC;
  Name = ARRAY[0..31] of CHAR;   (* module name *)
  Proc = PROCEDURE;

  Cmd = RECORD
    name: Name;
    adr: ADDRESS;
  END;

  Module = ^ModuleDesc;    (* module descriptor *)
  ExportPtr = ^ExportDesc;

  ExportDesc = RECORD
    fp: LONGINT;
    adr: LONGINT;
    nofExp: INTEGER;
    dsc: ^ExportDescArray;
  END;

  ExportDescArray = Array of ExportDesc;

  LongIntArray = Array of LongInt;
  ArrayPtr = ^LongIntArray;

  ModuleDesc = RECORD    (* Note: if this is changed, also update the static Linker *)
        next: Module;
        name: Name;
        init, trapped: BOOLEAN;
        refcnt, sb: LONGINT;
        entries: ^AddressArray;
        cmds: ^CmdArray;
        ptrTab, tdescs: ^AddressArray;
        imports: ^ModuleArray;
        data, code, refs: ^CharArray;
        publics, privates: LONGINT;   (* for slim binaries *)
        nofimp: INTEGER; import: ArrayPtr;
        nofstrc: INTEGER; struct: ArrayPtr;
        nofreimp: INTEGER; reimp: ArrayPtr;
        export: ExportDesc;
        term: Proc
    END;
    (* type descriptors *)
Tag = ^TypeDesc;
TypeDesc = RECORD
    size: LONGINT;
    ptroff: LONGINT
END;

    (* interrupt handling *)
TrapHandler = PROCEDURE (err, fp, pc, pf: LONGINT);

    (* heap/GC *)
FreeBlock = RECORD
    (* off-4 *) tag: Tag;
    (* off0 *) size: LONGINT;   (* field size aligned to 8-byte boundary, size MOD B = B-4 *)
    (* off4 *) next: ADDRESS
END;
BlockPtr = ^Block;
Block = RECORD
    lastElemToMark, currElem, firstElem: BlockPtr
END;
Blockm4 = RECORD
    tag: Tag;
    lastElemToMark, currElem, firstElem: LONGINT
END;

TTag = RECORD tag: Tag; z0, z1, z2, z3, z4, z5, z6, z7: LONGINT END;
InitPtr = ^TTag;
    (* the following type is used indirectly in InitHeap *)

Finalizer = PROCEDURE (obj: Pointer);
FinObj = ^FinObjNode;
FinObjNode = RECORD
    next: FinObj;
    obj: LONGINT;
    marked: BOOLEAN;
    fin: Finalizer;
END;

    (* memory management *)
GateDescriptor = RECORD
    offsetBits0to15: INTEGER;
    selector: INTEGER;
    gateType: INTEGER;
    offsetBits16to31: INTEGER
END;
SegmentDescriptor = RECORD
    limit0to15: INTEGER;
    base0to15: INTEGER;
    base16to23: CHAR;
    accessByte: CHAR;
    granularityByte: CHAR;
    base24to31: CHAR
END;

MilliTimer = RECORD target: LONGINT END;

  AddressArray = Array of Address;
  CmdArray = Array of Cmd;
  CharArray = Array of Char;
  ModuleArray = Array of Module;

VAR
    (* exported variables *)
version: ARRAY [0..63] OF CHAR;
modules : Module;  (** list of modules, patched by Linker *)
StackOrg : ADDRESS;    (** highest address on stack (single-process system) *)
bt: ADDRESS;   (** boot table *)
tspeed: LONGINT;   (** <=0: no V24 tracing, >0: tspeed = speed of com output *)
break : BOOLEAN;   (** has ctrl-break been pressed? *)
copro: BOOLEAN;    (** is a coprocessor present? *)
inGC: BOOLEAN; (** executing inside GC? *)
EnableGC, DisableGC: Proc;    (** Enable or Disable the GC *)
timer: Proc;   (** for internal use *)
runtime: ARRAY [0..2] OF ADDRESS;   (** for internal use *)
traceConsole: BOOLEAN; (** trace on console? *)
shutdown: LONGINT; (** system shutdown code 0=running, 1=powering off, 2=rebooting *)
tlpt: LONGINT;

  (* heap/GC *)
memTop, heapTop: ADDRESS;

    (* interrupt handling/memory management *)
handler: TrapHandler;   (* trap handlers *)
loop: Proc; (* main loop *)
traceBufAdr: LONGINT;

    (* misc. *)
pspeed: LONGINT;    (* previous trace speed *)
pageheap, pageheap0, pageheap1: ADDRESS;
displayPos: LONGINT;
kpar: ARRAY [0..1] OF LONGINT;


PROCEDURE WriteString(s: ARRAY OF CHAR);

PROCEDURE WriteHex(x, w: LONGINT);

PROCEDURE WriteChar(c: CHAR);

Procedure WriteLn;



Implementation

Uses ObxPal;

(** Module Kernel is responsible for memory allocation, garbage collection and other run-time 
support for the Oberon language, as well as interfacing to the underlying machine.  Using features 
exported from module Kernel may result in an unportable module, as not all Oberon Kernels are the same. 
This module is of little importance to most Oberon programmers. *)



(** WriteString - Write a string *)
PROCEDURE WriteString(s: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE s[i] <> #0 DO
  begin
    WriteChar(s[i]);
    INC(i)
  END
END; // WriteString;

(** WriteInt - Write "x" as a decimal number.  "w" is the field width. *)
PROCEDURE WriteInt(x, w: LONGINT);
VAR
  i: LONGINT;
  x0: LONGINT;
  a: Array[0..11] of char;
  s: Array[0..1] of Char;
BEGIN
  IF x < 0 THEN
    begin
      IF x = -2147483648 THEN
        begin
          WriteString('-2147483648');
          Exit;
        end
      ELSE
        begin
          DEC(w);
          x0 := -x
        end
    end
    ELSE
      x0 := x;

    i := 0;
    REPEAT
        a[i] := CHR(x0 MOD 10 + $30);
        x0 := x0 DIV 10;
        INC(i)
    UNTIL x0 = 0;

    s[0] := ' ';  s[1] := #0;
    WHILE w > i DO
    begin
      WriteString(s);
      DEC(w)
    END;
    s[0] := '-';
    IF x < 0 THEN WriteString(s);
    REPEAT
      DEC(i);
      s[0] := a[i];
      WriteString(s)
    UNTIL (i = 0);
END; // WriteInt;

(** WriteHex - Write "x" as a hexadecimal number. The absolute value of "w" is the field width.  
If "w" is negative, two hex digits are printed (x MOD 100H), otherwise 8 digits are printed. *)
PROCEDURE WriteHex(x, w: LONGINT);
VAR
    buf: ARRAY[0..9] OF CHAR;
    i, j: LONGINT;
BEGIN
    IF w >= 0 THEN j := 8 ELSE begin j := 2; w := -w END;
    FOR i := j+1 TO w DO WriteChar(' ');
    FOR i := j-1 downto 0 DO
    begin
        buf[i] := CHR(x MOD $10 + 48);
        IF buf[i] > '9' THEN
            buf[i] := CHR(ORD(buf[i]) - 48 + 65 - 10);
        x := x DIV $10;
    END;
    buf[j] := #0;
    WriteString(buf)
END; // WriteHex;

(** WriteChar - Write a character to the trace output (not reentrant). *)
PROCEDURE WriteChar(c: CHAR);
BEGIN
  ObxPal.printChar(c)
END; // WriteChar;

(** WriteLn - Skip to the next line on trace output *)
PROCEDURE WriteLn;
BEGIN
    WriteChar(#13);  WriteChar(#10)
END; // WriteLn

(** InstallTermHandler - Install a procedure to execute when a module is freed.  Normally used to 
uninstall interrupt handlers or tasks and perform other cleanup duties.  Never perform upcalls in proc, 
because the upcalled module will no longer be in memory! *)
PROCEDURE InstallTermHandler (h: Proc);    (* can also be used by modules below Modules, e.g. FileDir *)
// TODO
Begin
END; // InstallTermHandler;

(** GetConfig - Return value of configuration string.  Returns empty val if name not found. *)
PROCEDURE GetConfig(name: ARRAY OF CHAR;  VAR val: ARRAY OF CHAR);
// TODO
Begin
END; // GetConfig;

(** Used - Return the size in bytes of the amount of memory currently in use in the heap. *) 
Function Used: LONGINT;
BEGIN
    Used := 0; // TODO
END; // Used;

(** SetTimer - Set timer to expire in approximately "ms" milliseconds. *)
PROCEDURE SetTimer(VAR t: MilliTimer;  ms: LONGINT);
// TODO
Begin
END; // SetTimer;

(** Expired - Test if a timer has expired.  Interrupts must be on. *)
Function Expired(VAR t: MilliTimer): BOOLEAN;
BEGIN
    Expired := FALSE; // TODO
END; // Expired;

(** RegisterObject - Register an object (POINTER TO RECORD or POINTER TO ARRAY, not SysBlk) for 
finalization.  Never perform upcalls in the fin procedure! basic = FALSE *)
PROCEDURE RegisterObject (obj: Pointer;  fin: Finalizer;  basic: BOOLEAN);
// TODO
Begin
END; // RegisterObject;

(** GetTimer - Return "ticks" since initialisation (Kernel.TimeUnit ticks per second) *)
Function GetTimer: LONGINT;
BEGIN
    GetTimer := ObxPal.time();
END; // GetTimer;

(** InstallLoop - Install procedure to which control is transferred after a trap *)
PROCEDURE InstallLoop(p: Proc);
BEGIN
    loop := p
END; // InstallLoop;

(** InstallTrap - Install the trap handler *)
PROCEDURE InstallTrap(p: TrapHandler);
BEGIN
    handler := p
END; // InstallTrap;

(** GetLog - Return characters from trace log. *)
PROCEDURE GetLog(VAR val: ARRAY OF CHAR);
Begin
END; // GetLog;

(** Shutdown - Terminate Oberon after executing all module terminators.  If code = 1, 
perform an APM power-down, if code = 2, perform a soft reboot, or else just switch off 
interrupts and loop endlessly. *)
PROCEDURE OberonShutdown(code: LONGINT);
// TODO
Begin
END; // Shutdown;

(** GetMod - Return the loaded module that contains code address pc. *)
Function GetMod(pc : LONGINT): Module;
Begin
  GetMod := modules;  // TODO
END; // GetMod;

(** LargestAvailable - Return the size in bytes of the largest free available memory block. 
Allocating objects with a size greater than this size will cause the memory allocation to fail. *) 
Function LargestAvailable: LONGINT;
Begin
  Result := 0; // TODO
END; // LargestAvailable;

(** Available - Return the size in bytes of the remaining free heap space *)
Function Available: LONGINT;
Begin
  Result := 0; // TODO
END; // Available;

(** Idle - Called to save power when system is idle. *)
PROCEDURE Idle(code: LONGINT); (* code currently unused *)
// TODO
Begin
END; // Idle;

(** GC - Immediately activate the garbage collector. *)
PROCEDURE GC;
Begin
  // TODO
END; // GC;

(** GetClock - Return current time and date *)
(** day = d MOD 32, month = d DIV 32 MOD 16, year = 1900+d DIV 512,
      hour = t DIV 4096 MOD 32, minute = t DIV 64 MOD 64, second = t MOD 64 *)
PROCEDURE GetClock(VAR time, date: LONGINT);
BEGIN
{  ObxPal.clock(hour,minute,second);
  ObxPal.date(year,month,day);
  time := hour[0] * 4096 + minute[0] * 64 + second[0];
  date := (year[0]-1900) * 512 + month[0] * 32 + day[0];
}
  time := 0;
  date := 0; //todo
END; // GetClock;

(** SetClock - Set current time and date *)
PROCEDURE SetClock(time, date: LONGINT);
// TODO
Begin
END; // SetClock;

(* Init code called from OBL.  EAX = boot table offset.  2k stack is available. *)
BEGIN
    //SYSTEM.GETREG(0, bt); (* EAX from bootstrap loader *)
    //SYSTEM.GETREG(6, kpar[0]);  SYSTEM.GETREG(7, kpar[1]);    (* par0 & par1 *)
    tspeed := 0;  pspeed := 0;  displayPos := 0;  traceBufAdr := 0;  timer := NIL;
    traceConsole := FALSE;  shutdown := 0;  tlpt := 0;
    //ReadBootTable;    (* set memTop, pageheap, configadr *)
    //CheckMemory;  (* adjust memTop *)
    pageheap0 := pageheap;  pageheap1 := 0;  heapTop := memTop;
    //InitTracing(heapTop);
    version := 'OBX PAL / based on PC Native 2.3.6';
    WriteString('ETH Oberon / ');  WriteString(version);  WriteLn;
    //InitProcessor;  InitInterrupts;  InitTrapHandling;
    //InitMemory;  InitHeap;
        (* switch to level 3 (after InitHeap inits StackOrg) *)
    //SwitchToLevel3(UserStackSel, StackOrg, UserCodeSel);
        (* on new stack *)
    //InitTimer;  (*InitBeeps;*)
    //InitClock;  InitRuntime
    IF ObxPal.version() < 7 THEN
    begin
      System.WriteLn('incompatible PAL version!');
      HALT(0);
    END
END. // Kernel.

(*
TraceHeap:
0   1   NEW record
1   2   NEW array
2   4   SYSTEM.NEW
3   8   deallocate record
4   16  deallocate array
5   32  deallocate sysblk
6   64  finalisation
7   128 free pages
8   256 show free blocks
9   512 NewDMA
10  1024    before/after memory
11  2048    live/dead
16  65536   termhandlers
*)

