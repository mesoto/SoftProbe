; ---------------------------------------------------------------------
; Written by: Mehdi Sotoodeh
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ---------------------------------------------------------------------

Save_Context    macro
        ; make sure CF is preserved.
        cli
        pop     cs:save_ip
        pop     cs:save_cs
        pop     cs:save_fl
        mov     cs:save_sp,sp
        mov     cs:save_ss,ss
        push    cs
        pop     ss
        mov     sp,offset save_es+2
        push    es
        push    ds
        push    di
        push    si
        push    bp
        push    dx
        push    cx
        push    bx
        push    ax
        push    cs
        pop     ds
        cld
        endm

byte0   equ     this byte
word0   equ     this word
code0   equ     $
kbdflg  equ     417h

boot_FileBeg:
        IFDEF   _BOOT
        dd      -1
        dw      8000h
        dw      0
        dw      offset justRet
        ELSE
        ; .sys info
        dd      -1
        dw      8000h
service dw      offset deviceInit
        dw      offset justRet
        ENDIF
monSign:        Probe_Signature queue, justRet, montdev
        ; ----------------------------- ; ----------------------------------
workingMemLen   dw  10h                 ; These words are referenced by
workingMemSeg   dw  0                   ; other SoftProbes (do not move)
macroPointers   dw  macroPtrs           ; start of macro buffer
        ; ----------------------------- ; ----------------------------------

        even
List_Ptr        dd  0                   ; ptr list of lists (52h)
DosVer          dw  0
MCB_Head        dw  0
Null_Ptr        dw  0,0
InDosPtr        dw  0,0

                copyrightMsg 'SoftProbe'

;----------------------------------------------------------------------------
        org 1a0h
;----------------------------------------------------------------------------
stackTop        equ $
save_ax         dw  Break2_Trap     ;used for int brk2Int init
save_bx         dw  4*brk2Int       ;used for int brk2Int
fix_Break2W4    equ $-2
save_cx         dw  Int03_trap
save_dx         dw  4*3
save_bp         dw  0
save_si         dw  0
save_di         dw  0
save_ds         dw  0
save_es         dw  0
save_sp         dw  0
save_ss         dw  0
save_ip         dw  0
save_cs         dw  0
save_fl         dw  0

save_fs         dw  0
save_gs         dw  0

prev_ax         dw  0
prev_bx         dw  0
prev_cx         dw  0
prev_dx         dw  0
prev_bp         dw  0
prev_si         dw  0
prev_di         dw  0
prev_ds         dw  0
prev_es         dw  0
prev_sp         dw  0
prev_ss         dw  0
prev_ip         dw  0
prev_cs         dw  0
prev_fl         dw  0
zero            dw  0

Marked_Regs     dw  14 dup(0)
;saved_Vectors   dw  4  dup(0)   ;saved int08 and int09

scstart         dw  0b000h

findlen         dw  0
findstr         db  16 dup(0)
replstr         db  16 dup(0)

watch_ptr       dw  0,0
eff_addr        dw  0,0
code_ptr        dw  0,0
code_marker     dw  20 dup(0)   ;10 markers (should follow code_ptr)
data_ptr        dw  0,0
data_marker     dw  20 dup(0)   ;10 markers (should follow data_ptr)

keycode         dw  0

format_Types    dw disp_DataByte, disp_DataWord, disp_DataDword, disp_DataAscii
data_Format     dw disp_DataByte
                db 0
iflag           db 0            ;bit 1=IntHook active
;definition of iflag
F_JFILTER       equ 80h         ;Junk filter
F_USRSCR        equ 40h         ;who's screen displayed?
F_HOOKING       equ 20h
F_I1SCRN        equ 10h

Int01_OldVector dw  Int_Ret,0   ;original int 01
Int03_OldVector dw  Int_Ret,0   ;original int 03
trc_Counter     dw  1

watch_Format    dw disp_DataByte
                db 0
err_Flag        db 0
intmask         db  0ffh        ;no interrupt mask

BreakInfoStru   struc
  brk_Flags     db  0
  brk_LinAdrLo  db  0
  brk_LinAdrHi  dw  0
  brk_AppAddr   dw  0,0
  brk_AppInst   dw  0
BreakInfoStru   ends

; Bit fields of brk_Flags
BRK_INUSE       equ 80h
BRK_ACTIVE      equ 40h
BRK_TEMP        equ 20h
BRK_COUNT       equ 02h
BRK_TYPE2       equ 01h

break_Info1     BreakInfoStru <>  ;1
break_Info2     BreakInfoStru <>  ;2
                BreakInfoStru <>  ;3
                BreakInfoStru <>  ;4
                BreakInfoStru <>  ;5
                BreakInfoStru <>  ;6
                BreakInfoStru <>  ;7
                BreakInfoStru <>  ;8
                BreakInfoStru <>  ;9
                BreakInfoStru <>  ;A
                BreakInfoStru <>  ;B
                BreakInfoStru <>  ;C
                BreakInfoStru <>  ;D
                BreakInfoStru <>  ;E
                BreakInfoStru <>  ;F
break_Info_End:
break_Info_Len  equ (break_Info2 - break_Info1)

;input: DX:BP   instruction pointer (DS=CS)
;output DX:BP   next instruction
;       DS:SI   disassembled buffer
;input: DS:SI  pointer to first byte of instruction
;       SS=CS
;output C=0    DS:SI  Effective address of instruction
;       C=1    No effective address

        include sp_dasm.asm

PUBPROC FindBreak
        ; In:   DS:SI=ptr far address
        ;       DS=CS
        ; Out:  CF=0: found, bx=ptr BreakInfo, cx=break No
        call    absaddr
        xchg    ax, si
        mov     bx, offset break_Info1
        xor     cx, cx
find_BreakLoop:
        inc     cx
        test    [bx].brk_Flags, BRK_INUSE
        jz      find_NextBreak
        cmp     si, [bx].brk_LinAdrHi
        jne     find_NextBreak
        cmp     al, [bx].brk_LinAdrLo
        je      found_Break
find_NextBreak:
        add     bx, break_Info_Len
        cmp     bx, offset break_Info_End
        jb      find_BreakLoop
        stc
found_Break:
        ret

PUBPROC AddBreak
        push    dx
        mov     bp, code_ptr[0]
        mov     dx, code_ptr[2]
        call    dispins
        pop     dx
        dec     bp
        sub     bp, code_ptr[0]
        jz      add_Break
        or      dl, BRK_TYPE2           ; make it type 2
add_Break:
        mov     si, offset code_ptr
        ; In:   DS:SI=ptr far address
        ;       DL=Flags
        ;       DS=CS
        ; Out:  CF=0: ok, bx=ptr BreakInfo
        mov     bx, offset break_Info1
add_BreakLoop:
        test    [bx].brk_Flags, BRK_INUSE
        jz      add_NewBreak
        add     bx, break_Info_Len
        cmp     bx, offset break_Info_End
        jb      add_BreakLoop
        call    displayMsg
        db      ' All breakpoints used.',d_el
        jmp     Menu_Error

add_NoWrite:
        call    displayMsg
        db      ' Can not put break here.',d_el
Menu_Error:
        mov     cs:err_Flag, 1
        jmp     MenuCommand

add_NewBreak:
        les     di, dword ptr ds:[si]
        mov     al, 0cch
        xchg    al, es:[di]
        xchg    al, es:[di]
        cmp     al, 0cch
        jne     add_NoWrite
        mov     [bx].brk_AppAddr[0], di
        mov     [bx].brk_AppAddr[2], es
        call    absaddr
        xchg    ax, si
        mov     [bx].brk_LinAdrLo, al
        mov     [bx].brk_LinAdrHi, si
        mov     [bx].brk_Flags, dl
        clc
        ret

Apply_Breaks:
        ; Restore all breakpoints before exit
        ; In:   DL = filter (selection flags)
        mov     bx, offset break_Info1
        xor     cx, cx
brk_ApplyLoop:
        mov     al, [bx].brk_Flags
        test    al, BRK_ACTIVE
        jz      brk_ApplyNext
        test    al, dl
        jz      brk_ApplyNext
        les     di, dword ptr [bx].brk_AppAddr
        and     al, BRK_TYPE2
        jz      put_brk1
        mov     ax, brk2Int*100h + 0CDh
fix_Break2B1    equ $-1
        xchg    ax, es:[di]
        jmp     short put_brk2
put_brk1:
        inc     cx
        mov     al, 0cch                ; Int 3 (CCh)
        xchg    al, es:[di]
put_brk2:
        mov     [bx].brk_AppInst, ax
brk_ApplyNext:
        add     bx, break_Info_Len
        cmp     bx, offset break_Info_End
        jb      brk_ApplyLoop

        xor     bx, bx
        mov     es, bx
        mov     ax, cs
        mov     word ptr es:[4*brk2Int+0], offset Break2_Trap
fix_Break2W1    equ $-4
        mov     word ptr es:[4*brk2Int+2], ax
fix_Break2S1    equ $-2
        ; Do we need to set int 3 vectors?
        jcxz    brk_ApplyDone
        xchg    ax, es:[bx+4*3+2]
        mov     Int03_OldVector[2], ax
        mov     ax, offset Int03_trap
        xchg    ax, es:[bx+4*3+0]
        mov     Int03_OldVector[0], ax
brk_ApplyDone:
        ret

Remove_Breaks:
        ; Remove all breakpoints on entry
        mov     bx, offset break_Info1
        xor     cx, cx
rmv_BreakLoop:
        mov     al, [bx].brk_Flags
        test    al, BRK_ACTIVE
        jz      rmv_BreakNext
        test    al, BRK_TEMP
        jz      rmv_BreakPut1
        mov     [bx].brk_Flags, 0
rmv_BreakPut1:
        les     di, dword ptr [bx].brk_AppAddr
        and     al, BRK_TYPE2
        mov     ax, [bx].brk_AppInst
        jz      rmv_BreakPut2
        cmp     word ptr es:[di], brk2Int*100h + 0CDh
fix_Break2B2    equ $-1
        jne     rmv_BreakNext
        stosw
        jmp     short rmv_BreakNext
rmv_BreakPut2:
        inc     cx               ; count int 3's only
        cmp     byte ptr es:[di], 0CCh
        jne     rmv_BreakNext
        stosb
rmv_BreakNext:
        add     bx, break_Info_Len
        cmp     bx, offset break_Info_End
        jb      rmv_BreakLoop

        jcxz    rmv_BreaksDone
        xor     bx, bx
        mov     es, bx
        mov     ax, cs
        mov     si, offset Int03_OldVector
        mov     di, 4*3
        movsw                   ;restore int 3 vector
        movsw
rmv_BreaksDone:
        ret

prtlin          db  0
partabl         db  'NONE'
bittabl         db  '51  61  71  81  '      ;databits, stopbits
                db  '51.562  72  82  '

;       INT 08h -- Timer tick entry point
timer_Int:
        pushf
        db      9ah             ;call seg:off
timrvec dw      timer_Int, 4*8
timrsrv db      0cfh            ;iret(CF) for idle, CS:(2e) serveice
        cmp     byte ptr whereto, 0
        jz      Int_Ret
        cli
        push    es
        push    ax
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        cld
        db      0e8h            ;call xxxx
timrplc dw      0
timradr equ     $
        pop     di
        pop     si 
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     es
Int_Ret:iret

timer_Setup     equ $-timradr
;       Timed setup of keyboard and timer tick
        push    ds
        xor     ax, ax
        mov     ds, ax
        mov     word0[4*09h+0], offset kbd_Int ;set keyboard driver
        mov     word0[4*09h+2], cs
        mov     word0[4*08h+0], offset timer_Int ;set timer tick
        mov     word0[4*08h+2], cs
        mov     al, 0
setmsk1 equ     $-1
        out     21h, al
        pop     ds
        ret

timer_Com       equ $-timradr
        push    ds
        push    cs
        pop     ds
        mov     dx, 3fbh
COMport equ     $-2
        in      al, dx           ;Line control register
        and     ax, 7
        shl     al, 1
        shl     al, 1
        mov     si, offset bittabl
        add     si, ax
        lodsw                   ;data bits
        mov     datbits, al
        mov     byte ptr stopbit[0], ah
        lodsw
        mov     word ptr stopbit[1], ax
        in      al, dx
        shr     al, 1
        shr     al, 1
        shr     al, 1
        and     al, 3           ;parity
        mov     bx, offset partabl
        xlat                    ;None/Odd/None/Even
        mov     paribit, al
        inc     dx              ;3fc
        in      al, dx          ;Modem Control Register
        and     ax, 3
        shr     al, 1
        adc     ah, '0'
        add     al, '0'
        mov     comDTR, ah
        mov     comRTS, al
        inc     dx
        inc     dx              ;3fe
        in      al, dx          ;Modem status register
        mov     ah, 18h
        shl     ax, 1
        mov     comDCD, ah
        mov     ah, 18h
        shl     ax, 1
        mov     comRI, ah
        mov     ah, 18h
        shl     ax, 1
        mov     comDSR,ah
        mov     ah, 18h
        shl     ax, 1
        mov     comCTS,ah
        sub     dx, 3           ;3fb
        in      al, dx          ;get Line Control Register
        mov     cx, ax          ;save it
        or      al, 80h         ;DLAB on
        out     dx, al
        sub     dx, 3           ;3f8
        mov     di, dx
        in      ax, dx          ;DLAB
        xchg    ax, cx
        add     dx, 3           ;3fb
        out     dx, al          ;restore Line Control Register
        pop     ds

        jcxz    wrtbaud
        mov     ax, 2d00h
        cwd
        div     cx              ;ax=baud/10
        xchg    ax, cx
wrtbaud:push    cx              ;baud rate/10
        push    di              ;port address
        xor     di, di
        call    display
        db      a_b,'  COM'
ComNumb db      '1:',d_stk16,'  BAUD=',d_decim,'0   '
paribit db      'N '
datbits db      '8 '
stopbit db      '1.5 DTR='
comDTR  db      '1  RTS='
comRTS  db      '1  DSR='
comDSR  db      '1  CTS='
comCTS  db      '1  DCD='
comDCD  db      '1  RI='
comRI   db      '1',d_dup,6,' ',d_el
        ret

memdisp dd      0
timer_Mem       equ $-timradr
        push    ds
        lds     dx, cs:dword ptr memdisp
        xor     di, di
        push    dx
        push    ds
        call    display
        db      a_f,d_stk32,'  ',d_mem08,16,d_el
        sub     dx, 16
        call    display
        db      '³ ',d_ascii,16,' ³',d_el
        pop     ds
        ret

timer_Port      equ $-timradr
        xor     di, di
        call    display
        db      a_h,'  PORT  ',d_el
        mov     cx, 8
        mov     dx, 3bch
monPortAddr     equ $-2
prtdsp1:push    cx
        in      al, dx
        push    ax
        push    dx
        call    display
        db      d_stk16,':',d_stk08,'  ',d_el
        inc     dx
        pop     cx
        loop    prtdsp1
        ret

        include sp_disp.asm      ;display routines
        include sp_keyb.asm      ;keyboard routines

kbd_Int:push    ax
        in      al,60h
        pushf
        db      9ah             ;call seg:off
kbdvec  dw      kbd_Int,4*9
        cli
        push    ds
        mov     ds, cs:zero
        mov     ah, ds:[0417h]  ;current shift flags
        mov     cs:keycode, ax
        db      0ebh            ;jmp short intr_kb OR extr_kb
whereto db      exkb            ;0=intr_kb,  exkb=extr_kb

inkb    equ     $
;                               flush kb-buffer
        mov     ax, ds:[041ah]  ;kb-buffer-head
        mov     ds:[041ch], ax  ;kb-buffer-tail
        pop     ds
no_trap:pop     ax
        iret

exkb    equ     $-inkb
        pop     ds
        and     ah, 0fh
        cmp     ax, 0336h       ;L-R shifts
        je      trap_it
        cmp     ax, 0736h       ;Ctrl-L-Rshifts
        jne     no_trap
        stc
trap_it:pop     ax
        db      75h             ;JNZ (skip CLC)
;               INT brk2Int -- SoftProbe entry point
SoftProbeEntry:
        clc
        Save_Context
        jnc     @f
        call    ResetVideo
        mov     ax, scstart             ;page 0
        mov     cursor[2], ax
@@:     jmp     show_CSIP

Int03_trap:
        db      0a8h            ;test al, xx    (CF=0)
Break2_Trap:
        stc
        Save_Context
        mov     dx, 1
        adc     dl, dh          ;dx=1 (int 3) or 2 (brk2)
        mov     si, offset save_ip
        sub     [si], dx
        call    FindBreak
        jc      brk_SetIP       ;did not found, its embeded
        mov     ax, save_ip
        cmp     ax, word ptr [last_BreakOff]
        jne     show_CSIP       ;1st hit, stop on it.
        mov     ax, save_cs
        cmp     ax, word ptr [last_BreakSeg]
        jne     show_CSIP       ;1st hit, stop on it.
        ; its the second hit of the breakpoint
        mov     byte ptr [trc_next-1], break_Trace - trc_next
        call    Remove_Breaks
        jmp     trace_CsIp

brk_SetIP:
        add     save_ip, dx     ;+x if embeded int
show_CSIP:
        mov     byte ptr [trc_next-1], trace_next - trc_next
i386_fix1       dw  0c0c7h, save_fs     ;mov ax, offset save_fs
fixup_1         equ 0268ch              ;mov save_fs, fs
i386_fix2       dw  0c0c7h, save_gs     ;mov ax, offset save_gs
fixup_2         equ 02e8ch              ;mov save_gs, gs
        xor     ax, ax
;       mov     ds, ax
;       push    cs
;       pop     es
;       mov     si, 4*8
;       mov     di, offset saved_Vectors
;       movsw                   ; int 08
;       movsw
;       mov     word ptr ds:[si-4], offset timer_Int
;       mov     word ptr ds:[si-2], cs
;       movsw                   ; int 09
;       movsw
;       mov     word ptr ds:[si-4], offset kbd_Int
;       mov     word ptr ds:[si-2], cs

        mov     es, ax
        mov     es:word0[4*brk2Int+0], offset Break2_Trap
fix_Break2W2    equ $-4
        mov     es:word0[4*brk2Int+2], cs
fix_Break2S2    equ $-2
;       push    cs
;       pop     ds

        mov     whereto, al
        mov     keycode, ax
        call    Remove_Breaks
        sti
cmd_DispCsIp:
        mov     ax, save_ip
        mov     code_ptr[0], ax
        mov     word ptr [last_BreakOff], ax
        mov     ax, save_cs
        mov     code_ptr[2], ax
        mov     word ptr [last_BreakSeg], ax
UpdateScreen:
        test    iflag, F_USRSCR
        jnz     DisplayAll
        call    XchScreen
DisplayAll:
        call    DisplayRegisters
        call    displayWatchData
        les     si, dword ptr save_ip
        call    eff_adr
        ; cf=1  No effective address
        ; cf=0  es:si=Effective address
        ;       dx=address of display routine
        jc      NewCodeWin
        mov     eff_addr[0], si
        mov     eff_addr[2], es
        mov     al, es:[si+0]
        mov     ah, es:[si+1]
        push    ax
        push    si
        push    es
        mov     di, POS_VIEW - 160 + 52
        call    display
        db      a_h,d_stk32,d_mv,1,d_stk16,d_el
NewCodeWin:
        lds     dx, dword ptr cs:code_ptr
        push    dx
        push    ds
        IFNDEF  _BOOT
        call    FindOwner
        mov     dx, si
        mov     di, POS_VIEW - 160 + 22 ;on the frame
        call    display
        db      a_f,d_dup,8,'Í',d_mv,-18,a_h,d_stk32,d_mv,1,d_text,8,d_el
        ELSE
        mov     di, POS_VIEW - 160 + 2 ;on the frame
        call    display
        db      a_h,d_stk32,d_el
        ENDIF
        push    cs
        pop     ds
        call    displayCodeWin  ;display code_ptr
MenuCommand:
        mov     cs:cursor, POS_STATUS+158
        call    readkey
ExecCommand:
        mov     sp, offset stackTop
        push    cs
        pop     ds
        mov     es, cursor[2]
        shr     err_Flag, 1
        jnc     gomenu0
        push    ax
        call    ClearStatusLine
        pop     ax
gomenu0:call    toUpper
        cmp     al, 'A'
        jb      gomenu1
        cmp     al, 'Z'+1
        jae     gomenu1
        test    ah, K_RS + K_LS
        jnz     gomenu2
gomenu1:sub     al, 20h
gomenu2:cmp     al, K_sF10-20h+1
        jae     MenuCommand
        mov     bl, al
        mov     bh, 0
        shl     bx, 1
        DBGBRK
        jmp     MainMenuCmdTable[bx]

cmd_DispSize:
        mov     si, offset data_Format
        test    ah, K_ALT
        jz      new_Format
        xor     iflag, F_JFILTER
        jmp     DisplayAll

cmd_WatchSize:
        mov     si, offset watch_Format
new_Format:
        inc     byte ptr [si+2]
        mov     bx, 3
        and     bl, byte ptr [si+2]
        shl     bx, 1
        mov     ax, format_Types[bx]
        mov     [si], ax
        jmp     DisplayAll

WindowScroll:
        pop     cx
        mov     si, data_Format
        mov     al, [si-1]              ; size of each line
        and     ah, K_CTL
        jz      winScr1
        cbw
        shl     ax, 1
        shl     ax, 1                   ;*4 for page
winScr1:call    cx
dspdata:call    displayDataWin
        jmp     MenuCommand

cmd_DataDn:
        call    WindowScroll
        add     data_ptr, ax
        ret

cmd_DataUp:
        call    WindowScroll
        sub     data_ptr, ax
        ret

cmd_DataDec:
        dec     data_ptr
        jmp     dspdata
cmd_DataInc:
        inc     data_ptr
        jmp     dspdata
cmd_DataSegUp:
        dec     data_ptr[2]
        jmp     dspdata
cmd_DataSegDn:
        inc     data_ptr[2]
        jmp     dspdata
cmd_DataNorm:
        mov     si,offset data_ptr
        call    absaddr
        mov     data_ptr[0],si
        mov     data_ptr[2],ax
        jmp     dspdata
cmd_DataAlign:
        and     byte ptr data_ptr,0f0h
        jmp     dspdata

cmd_DispData2:  ; Shf-D
        mov     si, offset code_ptr
        mov     di, offset data_ptr
CodeDataPtr2:
;       test    ah, K_ALT
        test    ah, K_CTL
        jz      loadNewPtr
        mov     si, word ptr [stackRegOff]
        mov     cx, save_ss
        jmp     CodeDataPtr4

CodeDataPtr1:
        mov     si, offset eff_addr
        test    ah, K_ALT
        jnz     loadNewPtr
        mov     si, offset watch_ptr
loadNewPtr:
        push    cs
        pop     es
        movsw
        movsw
        jmp     displayAll

cmd_DispCode1:
        mov     di, offset code_ptr
        test    ah, K_ALT + K_CTL
        jnz     CodeDataPtr1
        mov     bx, POS_VIEW - 160 + 2 ;on the frame
        call    gethexd
        jmp     NewCodeWin

cmd_DispCode2:
        mov     di, offset code_ptr
        mov     si, offset data_ptr
        jmp     CodeDataPtr2

watch2data:
        mov     ah, K_CTL
cmd_DispData1:
        mov     di, offset data_ptr
        test    ah, K_ALT + K_CTL
        jnz     CodeDataPtr1
        mov     bx, POS_DATAWIN - 160 + 2 ; on the frame
        call    gethexd
        jmp     dspdata

switchSpBp:
        xor     word ptr [stackRegOff], (save_sp-code0) XOR (save_bp-code0)
        xor     byte ptr [stackRegName], 'S' XOR 'B'
        jmp     DisplayAll

cmd_DispStk1:
        test    ah, K_ALT + K_CTL
        jz      switchSpBp
        ; retn
        mov     cx, save_cs
        lds     si, dword ptr save_sp
        jmp     short CodeDataPtr3

cmd_DispStk2:
        ; retf
        lds     si, dword ptr save_sp
        mov     cx, 2[si]
CodeDataPtr3:
        mov     di, offset code_ptr
        test    ah, K_CTL
        jnz     CodeDataPtr4
        mov     di, offset data_ptr
CodeDataPtr4:
        push    cs
        pop     es
        movsw
        xchg    ax, cx
        stosw
        jmp     displayAll

cmd_CodeDn:
        test    ah, K_ALT
        jnz     code_Seg_Inc
        test    ah, K_CTL
        jnz     code_Page_Dn
        call    CodeWinDown1
        jmp     NewCodeWin
code_Seg_Inc:
        inc     code_ptr[2]
        jmp     NewCodeWin
code_Page_Dn:
        mov     cx, 14
        call    CodeWinDown
        jmp     NewCodeWin

cmd_CodeUp:
        test    ah, K_ALT
        jnz     code_Seg_Dec
        test    ah, K_CTL
        jnz     code_Page_Up
        call    CodeWinUp
        jmp     NewCodeWin
code_Seg_Dec:
        dec     code_ptr[2]
        jmp     NewCodeWin
code_Page_Up:
        mov     cx, 14
code_PageUp:
        push    cx
        call    CodeWinUp
        pop     cx
        loop    code_PageUp
        jmp     NewCodeWin

cmd_CodeOffInc:
        inc     code_ptr[0]
        jmp     NewCodeWin

cmd_CodeOffDec:
        dec     code_ptr[0]
        jmp     NewCodeWin

Int01_Trap:
        Save_Context
        mov     al, intmask
i386_fix4:      out     21h, al         ;enable interrupts
fixup_4         equ     0c033h          ;xor ax, ax
        xor     ax, ax
        mov     es, ax
        mov     si, offset Int01_OldVector
        mov     di, 4
        movsw
        movsw
        sti
next_Trace:
        and     byte ptr save_fl[1], 0feh ;clear trace flag
        jmp     short trace_next ;to:   trace_next      normal
trc_next:                        ;      break_Trace     passing break
                                 ;      trc_here        trace here
                                 ;      trc_up          trace up
break_Trace:
        cmp     save_cs, 7788h
last_BreakSeg   equ $-2
        jne     put_break_back
        mov     ax, save_ip
        sub     ax, 5566h
last_BreakOff   equ $-2
        shr     ax, 1
        jnz     put_break_back
        jmp     trace_CsIp      ; still on the break

put_break_back:
        mov     word ptr [last_BreakOff], -1 ; break on next hit
        mov     byte ptr [trc_next-1], trace_next - trc_next
        mov     dl, BRK_ACTIVE          ; apply all active breaks
        call    Apply_Breaks
        jmp     retToHost

trc_up: cmp     save_ss, 7777h
trc_RefSS       equ $-2
        jne     show_CSIP1
        cmp     save_sp, 7777h
trc_RefSP       equ $-2
        ja      show_CSIP1
        jmp     short trace_CsIp

trc_here:
        cmp     save_ip, 7777h
trc_RefIP       equ $-2
        jne     trace_CsIp
        cmp     save_cs, 7777h
trc_RefCS       equ $-2
        jne     trace_CsIp
show_CSIP1:
        jmp     show_CSIP

trace_next:
        mov     ax, trc_Counter
        dec     ax
        jz      show_CSIP1
        ; ax = trcCount-1
        cmp     byte ptr keycode, 1     ;ESC=interrupt?
        je      show_CSIP1
        jmp     short set_Trace

cmd_TraceUp:
        test    ah, K_ALT + K_CTL
        jz      cmd_Trace
        test    ah, K_ALT
        jnz     trace_here
        mov     ax, save_sp
        mov     word ptr [trc_RefSP], ax
        mov     ax, save_ss
        mov     word ptr [trc_RefSS], ax
        mov     byte ptr [trc_next-1], trc_up - trc_next
        jmp     short xch_and_trace

trace_here:
        mov     ax, code_ptr[0]
        mov     word ptr [trc_RefIP], ax
        mov     ax, code_ptr[2]
        mov     word ptr [trc_RefCS], ax
        mov     byte ptr [trc_next-1], trc_here - trc_next
xch_and_trace:
        call    XchScreen
        jmp     short trace_Start

cmd_Step:
        les     si, dword ptr save_ip
        call    stepIns
        jnc     cmd_Trace
        ; step over
        ; es:si=ptr next inst
        mov     code_ptr[0], si
        mov     code_ptr[2], es
        jmp     put_TmpBreak

cmd_Trace:
        mov     ax, 1
set_Trace:
        mov     trc_Counter, ax
trace_Start:
        call    UpdatePrevRegs
trace_CsIp:
        les     si, dword ptr save_ip
        lods    byte ptr es:[si]
        xor     bx, bx
        cmp     al, 0cdh
        je      trace_Int
        cmp     al, 9ch          ;pushf
        je      trace_Pushf
        in      al, 21h
        mov     intmask, al
i386_fix5:      mov     al, 0ffh
                out     21h, al         ;make sure NO-INT
fixup_5         equ     0c0c7h          ;mov ax, 21e6h
        mov     es, bx
        mov     ax, offset Int01_Trap
        xchg    ax, es:[bx+4]
        mov     word ptr Int01_OldVector[0], ax
        mov     ax, cs
        xchg    ax, es:[bx+6]
        mov     word ptr Int01_OldVector[2], ax
        or      byte ptr save_fl[1], 1  ;set trace flag ON
        test    iflag, F_I1SCRN
        jz      retToHost
swapAndGo:
        call    XchScreen
retToHost:
;       xor     cx, cx
;       mov     es, cx
;       mov     di, 4*8
;       mov     si, offset saved_Vectors
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     bp
;       cli
;       movsw                   ; int 08
;       movsw
;       movsw                   ; int 09
;       movsw
        pop     si
        pop     di
        cli
        mov     whereto, exkb
        pop     ds
        pop     es
        mov     ss, cs:save_ss
        mov     sp, cs:save_sp
        push    cs:save_fl
        push    cs:save_cs
        push    cs:save_ip
        iret

trace_Pushf:
        inc     save_ip
        sub     save_sp, 2
        les     di, dword ptr save_sp
        jmp     short setClientFlags

trace_Int:
        ; bx=0, es:si=ptr next inst
        lods    byte ptr es:[si]
        cmp     al, brk2Int
fix_Break2B3    equ $-1
        je      retToHost
        mov     es, bx
        mov     bl, al
        shl     bx, 1
        shl     bx, 1
        mov     ax, es:[bx+0]
        mov     dx, es:[bx+2]
        mov     save_ip, ax
        xchg    save_cs, dx
        sub     save_sp, 6
        les     di, dword ptr save_sp
        xchg    ax, si
        stosw
        xchg    ax, dx
        stosw
setClientFlags:
        mov     ax, save_fl
        stosw
        and     byte ptr save_fl[1], not 2 ;IF=0 in int handler
        jmp     next_Trace

cmd_Go: test    ah, K_CTL + K_ALT
        jz      set_Go
        test    ah, K_ALT
        jz      set_CSIP

        IFNDEF  _BOOT
        ; AppExit:
        call    CheckInDos
        mov     save_ip, offset dosTerminate
        mov     save_cs, cs
        jmp     short swapAndGo1
        ENDIF
set_Go: mov     dl, BRK_ACTIVE          ; apply all active breaks
apply_and_go:
        call    Apply_Breaks
swapAndGo1:
        call    UpdatePrevRegs
        jmp     swapAndGo

cmd_GoHere:
        test    ah, K_CTL
        jnz     set_CSIP                ; go from here
put_TmpBreak:
        mov     word ptr [last_BreakOff], -1 ; break on next hit
        mov     dl, BRK_INUSE + BRK_ACTIVE + BRK_TEMP
        call    AddBreak
        mov     dl, BRK_TEMP            ; only temp breaks
        jmp     apply_and_go

set_CSIP:
        mov     ax, code_ptr[0]
        mov     save_ip, ax
        mov     ax, code_ptr[2]
        mov     save_cs, ax
        jnz     swapAndGo1
        ; set cs:ip here
        jmp     DisplayAll

cmd_TraceCnt:
        test    ah, K_ALT + K_CTL
        jz      use_TraceCnt
        call    displayMsg
        db      ' Trace C=',d_ed1
        _getw   trcCount, 1
        db      d_ed2
use_TraceCnt:
        mov     ax, trcCount
        jmp     set_Trace

cmd_Break2:
        mov     dl, BRK_INUSE + BRK_ACTIVE + BRK_TYPE2
        test    ah, K_CTL + K_ALT
        jz      brk_Toggle
brk_Disable:
        call    LocateBreak
        db      'Disable',d_el
        and     [bx].brk_Flags, not BRK_ACTIVE
        ret
brk_Enable:
        call    LocateBreak
        db      'Enable',d_el
        or      [bx].brk_Flags, BRK_ACTIVE
        ret
brk_Remove:
        call    LocateBreak
        db      'Remove',d_el
        mov     [bx].brk_Flags, 0
        ret

cmd_Break1:
        test    ah, K_ALT
        jnz     brk_Remove
        test    ah, K_CTL
        jnz     brk_Enable
cmd_Break:
        mov     dl, BRK_INUSE + BRK_ACTIVE
brk_Toggle:
        mov     si, offset code_ptr
        call    FindBreak
        jc      new_Break
        mov     [bx].brk_Flags, 0       ;remove break
        jmp     NewCodeWin

new_Break:
        call    AddBreak
        jmp     NewCodeWin

cmd_MemCopy:
        call    displayMsg
        db      'Copy from ',d_ed1
        _getd   moveFrom, 0,0
        db      ' - '
        _getw   moveEnd, 0
        db      ' To '
        _getd   moveTo, 0,0
        db      d_ed2
        mov     si, offset moveTo
        call    absaddr
        mov     es, ax
        mov     di, si
        xchg    bp, ax
        mov     si, offset moveFrom
        mov     cx, moveEnd
        sub     cx, [si]
        jc      cmd_MemCopy
        call    absaddr
        mov     ds, ax
        cmp     ax, bp
        jne     memcpy1
        cmp     si, di
memcpy1:jae     memcpy2
        add     si, cx
        add     di, cx
        std
memcpy2:rep     movsb
        movsb
        cld
        jmp     DisplayAll

cmd_ReadWMem:
        call    displayMsg
        db      'Read from',d_el
        ;bx=0
        jmp     short SaveWM1

cmd_WriteWMem:
        call    displayMsg
        db      'Write to',d_el
        inc     bx
SaveWM1:push    bx
        call    display
        db      ' address ',d_ed1
        _getd   storeAddr, 0,0
        db      '  Len '
        _getw   storeLen, 0
        db      ' paragraphs',d_ed2
        mov     ax, workingMemLen
        mov     cx, storeLen
        jcxz    storerr
        cmp     ax, cx
        jae     SaveWM2
storerr:mov     storeLen, ax
        call    displayMsg
        db      'Invalid Length.',d_el
        jmp     Menu_Error
SaveWM2:mov     bp, cx
        mov     si, offset storeAddr
        call    absaddr
        xor     di, di
        mov     dx, workingMemSeg
        pop     cx
        jcxz    SaveWM3
        xchg    dx, ax
        xchg    si, di
SaveWM3:mov     ds, ax
        mov     es, dx
        mov     cx, 8
        rep     movsw
        sub     si, 16
        sub     di, 16
        inc     ax
        inc     dx
        dec     bp
        jnz     SaveWM3
        jmp     DisplayAll

cmd_Compare:
        call    displayMsg
        db      'Compare ',d_ed1
        _getd   cmprFrom, 0,0
        db      ' Len '
        _getw   cmprLen, 0
        db      ' With '
        _getd   cmprWith, 0,0
        db      d_ed2
        mov     cx, cmprLen
        jcxz    cmd_Compare
        mov     cursor, POS_STATUS+158
        les     di, dword ptr cmprWith
        lds     si, dword ptr cmprFrom
        xor     bp, bp
compar1:cmpsb
        je      compar2
        push    cx
        push    si
        push    ds
        push    di
        push    es

        dec     di
        mov     al, es:[di]
        push    ax
        push    di
        push    es
        dec     si
        mov     al, ds:[si]
        push    ax
        push    si
        push    ds
        call    displayMsg
        db      ' ',d_stk32,' ',d_stk08,d_mv,5,d_stk32,' ',d_stk08
        db      ' ³ '
Esc4Cancel      db 'ESC=Cancel.',d_el
        call    readkey
        pop     es
        pop     di
        pop     ds
        pop     si
        pop     cx
        inc     bp
        cmp     al, K_ESC
        je      compar3
compar2:loop    compar1
compar3:push    bp
        call    displayMsg
        db      ' ',d_decim,' differences found.',d_el
        jmp     Menu_Error

cmd_Find:
        call    displayMsg
        db      '    Find: ',d_ed1
        _gets   findstr
        db      d_ed2
        jcxz    get_ReplStr
        add     cx, 0ffh                ;+1 if cl>0
        mov     byte ptr findlen, ch
get_ReplStr:
        call    displayMsg
        db      ' Replace: ',d_ed1
        _gets   replstr
        db      d_ed2
        cmp     ax, 0fe00h+K_CR
        je      cmd_Find
cmd_FindNext:
        mov     cx, findlen
        jcxz    cmd_Find
        call    Find_StrNext
find1:  jmp     DisplayAll

cmd_Replace:
        test    ah, K_ALT
        jnz     Cmp_Marks
        test    ah, K_CTL
        jnz     Replace
        jmp     cmd_RegChange

Replace:
        mov     cx, findlen
        jcxz    cmd_Find
        call    Find_String
        mov     si, offset replstr
        mov     cx, findlen
        sub     di, cx
        rep     movsb
        jmp     find1

Find_StrNext:
        inc     data_ptr
Find_String:
        mov     bx, offset data_ptr
findStr1:
        mov     cx, 0FF00h
        sub     cx, [bx]
        ja      findStr2
        sub     byte ptr [bx+1], 80h
        add     byte ptr [bx+3], 08h
        jnc     findStr1
        call    displayMsg
        db      ' Not found!',d_el
        jmp     Menu_Error

findStr2:
        les     di, dword ptr [bx]
        mov     si, offset findstr
        lodsb
        repne   scasb
        mov     [bx], di
        jne     Find_String
        mov     cx, findlen
        dec     cx
        repe    cmpsb
        jne     Find_String
        mov     si, bx
        call    absaddr
        cmp     ax, 7777h
findstr_Fix     equ $-2
        je      Find_StrNext
        dec     word ptr [bx]
        ret

Mark_Regs:
        mov     di, offset Marked_Regs
        call    Save_Registers
Cmp_Marks:
        call    display
        db      d_clrvu,d_el
        inc     di
        inc     di
        mov     si, offset Marked_Regs
        mov     dx, 13
cmpRegs:mov     bx, [si]
        xor     bx, [si + save_ax - Marked_Regs]
        neg     bx
        sbb     bx, bx
        mov     ah, Attrib[bx+1+a_b-a_first]
        call    hex_16_si
        add     di, 160-8
        dec     dx
        jnz     cmpRegs
        lodsw                   ;Marked_Flags
        push    ax              ;flags
        xor     ax, save_fl
        push    ax              ;attr for flags
        call    display1
        ;      '....ODITSZ.A.P.C'
        _bits   0000111111010101b, 'OoDdIiTtSsZzAaPpCc'
        db      d_el
@@:     jmp     MenuCommand

cmd_SaveRegs:
        test    ah, K_ALT
        jnz     mark_Regs

cmd_386Regs:
.386p
i386_fix3:      jmp short @b
fixup_3         equ 5b58h               ;pop ax
                                        ;pop bx
        pop     cx
        pop     dx
        pop     bp
        pop     si
        pop     di
        push    di
        push    si
        push    bp
        push    dx
        push    cx
        push    bx
        push    ax

        sub     sp, 12
        sidt    fword ptr save_ax[-6]
        sgdt    fword ptr save_ax[-12]
        pushfd
        popf
        push    save_fl
        push    edi
        push    esi
        push    ebp
        push    edx
        push    ecx
        push    ebx
        push    eax
        mov     dx, sp
        smsw    ax
        push    ax
        push    gs
        push    fs
        call    display
        db      d_clrvu,' '
        db      'EAX=',d_mem32,1,d_mv,66
        db      'EBX=',d_mem32,1,d_mv,66
        db      'ECX=',d_mem32,1,d_mv,66
        db      'EDX=',d_mem32,1,d_mv,66
        db      'EBP=',d_mem32,1,d_mv,66
        db      'ESI=',d_mem32,1,d_mv,66
        db      'EDI=',d_mem32,1,d_mv,66
        db      'FS=',d_stk16,d_mv,73
        db      'GS=',d_stk16,d_mv,73
        db      'EFLAGS=',d_mem32,1,d_mv,63
        db      'GDT: LIMIT=',d_mem16,1,'  BASE=',d_mem32,1,d_mv,47
        db      'IDT: LIMIT=',d_mem16,1,'  BASE=',d_mem32,1,d_mv,47
        db      'MSW=',d_stk16
        db      d_el
.8086
        jmp     MenuCommand

cmd_UsrScreen:
        call    XchScreen
        call    readkey
        cmp     al, K_TAB
        jne     @f
        call    SwitchScreen
        jmp     short SwapRefScreen

@@:     cmp     al, K_BKSP
        je      ResetUsrScr
        call    XchScreen
        jmp     MenuCommand

ResetUsrScr:
        call    ResetVideo
SwapRefScreen:
        call    XchScreen
cmd_RefScreen:
        call    InitScreen
        jmp     UpdateScreen

cmd_HexCalc:
        call    displayMsg
        db      ' Hex: ',d_ed1
        _getw   calarg1, 0
        db      ','
        _getw   calarg2, 0
        db      d_ed2
        mov     si, calarg1
        mov     bx, calarg2
        jmp     short calcul0

cmd_DecCalc:
        call    displayMsg
        db      ' Dec: ',d_ed1
        _getn   calarg3, 0
        db      ','
        _getn   calarg4, 0
        db      d_ed2
        mov     si, calarg3
        mov     bx, calarg4

calcul0:push    bx
        push    si
        mov     ax, si
        xor     ax, bx
        push    ax
        mov     ax, si
        or      ax, bx
        push    ax
        mov     ax, si
        and     ax, bx
        push    ax
        mov     cx, bx
        jcxz    calcul1
        xor     dx, dx
        mov     ax, si
        div     cx
        mov     cx, dx
calcul1:push    cx
        push    ax
        mov     ax, si
        mul     bx
        push    ax
        push    dx
        mov     ax, si
        sub     ax, bx
        push    ax
        lea     ax, [si+bx]
        push    ax
        push    bx
        push    si
        call    displayMsg
        db      ' ',d_stk16,',',d_stk16,' +',d_stk16,' -',d_stk16
        db      ' *',d_stk16,d_stk16,' /',d_stk16,' %',d_stk16,' &',d_stk16
        db      ' |',d_stk16,' ^',d_stk16,'   [',d_decim,',',d_decim
        db      ']',d_el
        jmp     MenuCommand

cmd_InByte:
        xor     bx, bx
cmd_InWord:
        push    bx
        call    displayMsg
        db      ' Input from port ',d_ed1
        _getw   InPortNum, 3bch
        db      ' ',d_ed2
        mov     dx, InPortNum
        pop     cx
        jcxz    in_byte
        in      ax, dx
        push    ax
        call    display
        db      a_r,'= ',d_stk16,d_el
        jmp     MenuCommand

in_byte:in      al, dx
        push    ax
        call    display
        db      a_r,'= ',d_stk08,d_el
        jmp     MenuCommand

Restore_Ints:
        call    displayMsg
        db      ' F1=Restore all ints, Other keys to cancel',d_el
        call    readkey
        xor     al, K_F1
        jnz     intskip
        xor     di, di
        mov     es, di
        mov     si, offset int_buf
        mov     cx, 2*100h
        cli
        rep     movsw
        sti
intskip:jmp     clealin

cmd_SavInts:
        xor     si, si
        mov     ds, si
        push    cs
        pop     es
        mov     di, offset int_buf
        mov     cx, 2*100h
        cli
        rep     movsw
        sti
        call    displayMsg
        db      ' Interrupt vectors loaded.',d_el
        jmp     DisplayAll

cmd_ListInts:
        test    ah, K_ALT
        jnz     Restore_Ints            ;~n=List ints, ~@N=Restore ints

        call    scrollLines
        db      'INT     WAS        NOW',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        test    bh, bh
        jz      listInt1
        mov     cx, 256
        stc
        ret

cmd_NewInts:
        test    ah, K_ALT
        jnz     cmd_SavInts             ;N=New ints, ^N=Load ints
        call    scrollLines
        db      'INT     WAS        NOW',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        mov     bp, di                  ;save di
        xor     cx, cx
        mov     es, cx
        xor     dx, dx                  ;int#
cmpInt1:mov     di, dx
        shl     di, 1
        shl     di, 1
        lea     si, int_buf[di]
        cmpsw
        jne     cmpInt2
        cmpsw
        je      cmpInt3
        dec     di
        dec     di
cmpInt2:cmp     cx, bx
        je      cmpInt4
        inc     cx
cmpInt3:inc     dl
        jnz     cmpInt1
        ; cx=no. of records
        stc
        ret

listInt1:
        mov     dx, bx
        mov     bp, di
        shl     bx, 1
        shl     bx, 1
        xor     cx, cx
        mov     es, cx
        lea     di, [bx+2]
cmpInt4:mov     ax, es:[di-2]
        push    ax
        mov     bx, es:[di+0]
        push    bx
        push    word ptr int_buf[di-2]
        push    word ptr int_buf[di+0]
        push    dx
        mov     di, bp
        IFNDEF  _BOOT
        mov     cl, 4
        shr     ax, cl
        add     ax, bx
        call    OwnerName
        ENDIF
        call    display
        db      a_n,' ',d_stk08,'  ',d_stk32,'  ',d_stk32
        IFNDEF  _BOOT
        db      '  ',d_string,41
        ENDIF
        db      d_el
        push    cs
        pop     ds
        clc
        ret

cmd_OutByte:
        call    displayMsg
        db      ' Output ',d_ed1
        _getb   Byte2Port, 0
        db      ' to port '
        _getw   OutPortB, 3bch
        db      d_ed2
        mov     dx, OutPortB
        mov     al, Byte2Port
        out     dx, al
        jmp     MenuCommand

cmd_OutWord:
        call    displayMsg
        db      ' Output ',d_ed1
        _getw   Word2Port, 0
        db      ' to port '
        _getw   OutPortW, 3bch
        db      d_ed2
        mov     dx, OutPortW
        mov     ax, Word2Port
        out     dx, al
        jmp     MenuCommand

        IFDEF   _BOOT
cmd_MapMemory:
cmd_DevList:
cmd_CurPsp:
cmd_NoSupport:
        call    displayMsg
        db      ' Command not supported by BootProbe.',d_el
        jmp     Menu_Error
        ELSE
cmd_MapMemory:
        call    scrollLines
        db      'START  LEN   ENV   PSP   OWNER',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        xor     si, si
        mov     ax, cs:MCB_Head
        mov     cx, bx
        jcxz    mapmem3
        xor     bx, bx
mapmem1:mov     ds, ax
        stc
        adc     ax, ds:[si+3]   ;len
        jc      mapmem4
        inc     bx
        cmp     byte ptr [si], 'Z'
        jne     mapmem2
        mov     ds, ax
        cmp     byte ptr [si], 'M'
        jne     mapmem4
mapmem2:loop    mapmem1
mapmem3:mov     ds, ax
        inc     ax              ;START
        push    word ptr [si+1] ;PSP
        push    cx              ;ENV
        push    word ptr [si+3] ;LEN
        push    ax              ;START
        call    OwnerName
        ; bx=psp, cx=env, ds:si=ds:dx=name
        mov     bp, sp
        mov     [bp+4], cx      ;ENV
        call    display
        db      a_n,' ',d_stk16,'  ',d_stk16,'  ',d_stk16,'  '
        db      d_stk16,'  ',d_string,43,d_el
        ;clc
        ret
mapmem4:mov     cx, bx          ;no. of records
        stc
        ret

driveNames      db 'A: - B: '

cmd_DevList:
        call    scrollLines
        db      '  Header    Stra  Intr  Attr  Device',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        mov     dx, bx
        push    cs
        pop     ds
        mov     bx, offset Null_Ptr
        mov     al, 'A'
cntdevs:lds     bx, dword ptr [bx]
        cmp     bx, -1
        je      lstdev0
        test    byte ptr 5[bx], 80h
        jnz     cntdevs
        add     al, 10[bx]
        jnc     cntdevs
lstdev0:mov     cs:driveNames[0], al
        push    cs
        pop     es
        mov     bx, offset Null_Ptr
        mov     cx, -1
lstdev1:inc     cx
        les     bx, dword ptr es:[bx]
        cmp     bx, -1
        je      lstdev4
        push    es
        pop     ds
        lea     si, 10[bx]      ;ds:si=dev name
        test    byte ptr [bx+5], 80h
        jnz     lstdev3
        mov     al, [si]
        push    cs
        pop     ds
        mov     si, offset driveNames
        sub     [si+0], al
        dec     al
        jz      lstdev2
        add     al, [si+0]
        mov     [si+5], al
        mov     al, '-'
lstdev2:mov     [si+3], al
lstdev3:cmp     cx, dx
        jne     lstdev1

        push    word ptr es:[bx+4] ;attribute
        push    word ptr es:[bx+8] ;Interrupt
        push    word ptr es:[bx+6] ;Strategy
        push    bx              ;
        push    es              ;
        mov     dx, si
        ; ds:dx = ptr device name
        call    display
        db      ' ',d_stk32,'  ',d_stk16,'  ',d_stk16,'  ',d_stk16
        db      '  ',d_string,8,d_el
        ;clc
        ret

lstdev4:;cx=#records
        stc
        ret

cmd_CurPsp:
        mov     ah,62h
        call    dos
        lea     ax, [bx-1]
        push    bx              ;PSP
        call    OwnerName
        ; ds:dx=ptr name
        call    displayMsg
        db      ' PSP=',d_stk16,'  Owner=',d_text,62,d_el
        jmp     MenuCommand
        ENDIF

cmd_MonTimer:
        mov     timrsrv, 0cfh   ;set IDLE mode
        call    displayMsg
        db      ' F1=Memory  F2=I/O Port  F3-F6=COM1-COM4  F7=Setup  Other keys=Idle',d_el
        call    readkey
        sub     al, K_F1
        jz      timerF1
        cbw
        dec     ax
        jz      timerF2
        cmp     al, 5
        jb      timerF3
        jnz     clealin
        in      al, 21h
        mov     byte ptr [setmsk1], al
        mov     ax, timer_Setup
        jmp     short timserv

timerF1:mov     ax, data_ptr[2]
        mov     word ptr memdisp[2], ax
        mov     ax, data_ptr
        mov     word ptr memdisp[0], ax
        mov     ax, timer_Mem
timserv:mov     timrplc, ax     ;ax=service-timradr
        mov     timrsrv, 2eh    ;service (CS: for whereto)
clealin:call    ClearStatusLine
        jmp     MenuCommand

timerF2:call    displayMsg
        db      'Port: ',d_ed1
        _getw   portadr, 3bch
        db      d_ed2
        mov     ax, portadr
        mov     word ptr [monPortAddr], ax
        mov     ax, timer_Port
        jmp     timserv

timerF3:mov     bx, ax
        add     al, '0'
        mov     ComNumb, al
        shl     bx, 1
        mov     es, zero
        mov     ax, es:[bx+3feh] ;get the port address
        add     ax, 3            ;line control
        mov     word ptr [COMport], ax
        mov     ax, timer_Com
        jmp     timserv

fillData        db 16 dup(0)

cmd_FillMem:
        call    displayMsg
        db      ' Fill from ',d_ed1
        _getd   fillFrom, 0,0
        db      ' To '
        _getw   fillEnd, 0
        db      d_ed2
        call    displayMsg
        db      ' Fill with ',d_ed1
        _gets   fillData
        db      d_ed2
        xor     bx, bx
        cmp     bl, cl          ;c=1 if cl>0
        adc     bl, ch
        les     di, dword ptr fillFrom
        mov     ax, fillEnd
        inc     ax
        mov     cx, bx
        add     bx, di
        cmp     ax, bx
        jb      cmd_FillMem
        push    di
        mov     si, offset fillData
        rep     movsb
        pop     si
        sub     ax, di
        xchg    cx, ax
        push    es
        pop     ds
        rep     movsb
go_DispAll:
        jmp     DisplayAll

cmd_EditMem:
        lds     si, dword ptr data_ptr
editMem1:
        push    si
        push    cs
        pop     es
        mov     di, offset fillData
        push    si
        push    ds
        mov     cx, 16
        rep     movsb
        push    cs
        pop     ds
        call    displayMsg
        db      ' ',d_stk32,'  ',d_ed1
        _gets   fillData
        db      d_ed2
        les     bx, dword ptr data_ptr
        pop     di
        mov     cx, 16
        mov     si, offset fillData
        rep     movsb
        cmp     ah, 0feh                ; 0ff00h/0fe00h + K_CR
        jb      go_DispAll
        mov     si, data_Format
        mov     cl, [si-1]              ;window line size
        je      editMem2
        sub     di, 32
editMem2:
        push    es
        push    di
        sub     di, bx
        shr     di, 1
        shr     di, 1
        cmp     di, cx
        jb      editMem3
        add     bx, cx
        cmp     di, 65
        jb      editMem3
        sub     bx, cx
        sub     bx, cx
editMem3:
        mov     data_ptr[0], bx
        call    displayWatchData
        call    displayCodeWin  ;display code_ptr
        pop     si
        pop     ds
        jmp     editMem1

cmd_Print:
        test    ah, K_ALT
        jnz     print_word
        test    ah, K_CTL
        jnz     print_byte

        call    print_function
        mov     bp, si
        mov     dx, ds
        push    cs
        pop     ds
        call    dispins
        mov     di, offset dissBuffEnd
        mov     si, bp
        ret

print_byte:
        call    print_function
        mov     cx, 16
printByte1:
        lodsb
        call    hexb
        mov     al, ' '
        stosb
        loop    printByte1
printAsci1:
        mov     ah, '|'
        stosw
        mov     cl, 16
        sub     si, cx
printAsci2:
        lodsb
        cmp     al, ' '
        jge     printAsci3
        mov     al, '.'
printAsci3:
        stosb
        loop    printAsci2
        mov     al, '|'
        stosb
        ret

print_word:
        call    print_function
        mov     cx, 8
printWord1:
        lodsw
        call    hexw
        mov     ax, '  '
        stosw
        loop    printWord1
        jmp     short printAsci1

print_function:
        call    displayMsg
        db      ' Print ',d_ed1
        _getd   printAddr, 0,0
        db      ' - '
        _getw   printEnd, 0
        db      d_ed2
        jmp     short printNext
print_loop:
        mov     ax, ds
        push    cs
        pop     es
        mov     di, offset dissBuffSeg
        call    hexw
        mov     al, ':'
        stosb
        mov     ax, si
        call    hexw
        mov     ax, '  '
        stosw
        pop     ax
        push    ax

        call    ax
        mov     cs:printAddr[0], si
        mov     ax, 000ah
        stosw

        mov     si, offset dissBuffSeg
        call    printLine
        call    key_check
        jnz     printDone
printNext:
        lds     si, dword ptr cs:printAddr
        cmp     si, cs:printEnd
        jbe     print_loop
printDone:
        jmp     clealin

IntHookStkStru  struc
  mi_flg        dw ?
  mi_ax         dw ?
  mi_bx         dw ?
  mi_cx         dw ?
  mi_dx         dw ?
  mi_bp         dw ?
  mi_si         dw ?
  mi_di         dw ?
  mi_ds         dw ?
  mi_es         dw ?
  mi_params     dw ?
  mi_ip         dw ?
  mi_cs         dw ?
  mi_flags      dw ?
IntHookStkStru  ends

IntHookInfoStru struc
; mon_call      db 3 dup (?)    ; call Handle_Hooks
  mon_flags     db 0            ; MON_??? flags
  mon_intNum    db 0            ; int number
  mon_funNum1   dw 0            ; min value for AX
  mon_funNum2   dw 0ffffh       ; max value for AX
  mon_oldVector dw 0,0          ; original vector
IntHookInfoStru ends

; Bit fields of mon_flags
MON_INUSE       equ 80h
MON_ACTIVE      equ 40h
MON_WAIT        equ 08h
MON_PRINT       equ 04h
MON_SETUP       equ 02h
MON_CUSTOM      equ 01h

IntHook_Beg:
        call    Handle_Hooks
IntHook1:
        IntHookInfoStru <>
        call    Handle_Hooks
IntHook2:
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
        call    Handle_Hooks
        IntHookInfoStru <>
IntHook_End:
Hook_Info_Len   equ (IntHook2 - IntHook1)

cmd_ListHooks:
        call    scrollLines
        db      ' Hooks:',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib

        mov     bp, offset IntHook_Beg
        xor     dx, dx
        xor     cx, cx
monLst1:inc     dx
        test    [bp+3].mon_flags, MON_INUSE
        jz      monLst2
        cmp     cx, bx
        je      monLst3
        inc     cx
monLst2:add     bp, Hook_Info_Len
        cmp     bp, offset IntHook_End
        jb      monLst1
        ;cx=#records
        stc
        ret

monLst3:push    word ptr [bp+3].mon_flags
        xor     cx, cx                          ;normal attr
        push    cx                              ;attr for _bits
        push    word ptr [bp+3].mon_oldVector[0]
        push    word ptr [bp+3].mon_oldVector[2]
        push    word ptr [bp+3].mon_funNum2
        push    word ptr [bp+3].mon_funNum1
        push    word ptr [bp+3].mon_intNum
        push    dx
        call    display
        db      ' Hook  #',d_stk08,'  Int ',d_stk08
        db      '  If (',d_stk16,' ó AX ó ',d_stk16,')  ',d_stk32,'  '
        ;      '-...-...-E..WPSC'
        _bits   0000000001001111b, 'EdW P S C '
        db      d_el
        ;clc
        ret

cmd_ListBreaks:
        call    scrollLines
        db      ' Breakpoints:',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib

        mov     bp, offset break_Info1
        xor     dx, dx
        xor     cx, cx
listBrk1:
        inc     dx
        test    [bp].brk_Flags, BRK_INUSE
        jz      listBrk2
        cmp     cx, bx
        je      listBrk3
        inc     cx
listBrk2:
        add     bp, break_Info_Len
        cmp     bp, offset break_Info_End
        jb      listBrk1
        ;cx=#records
        stc
        ret

listBrk3:
        push    word ptr [bp].brk_Flags
        xor     cx, cx                          ;normal attr
        push    cx                              ;attr for _bits
        push    [bp].brk_AppAddr[0]
        push    [bp].brk_AppAddr[2]
        push    dx
        call    display
        db      ' Break #',d_stk08,'  ',d_stk32,'  '
        ;      '.........E.....2'
        _bits   0000000001000001b, 'Ed21'
        db      d_el
        ;clc
        ret

LocateBreak:
        mov     bx, offset break_Info1
countBrk1:
        test    [bx].brk_Flags, BRK_INUSE
        jnz     countBrk2
        add     bx, break_Info_Len
        cmp     bx, offset break_Info_End
        jb      countBrk1
        call    displayMsg
        db      ' No breakpoints in use.',d_el
        jmp     Menu_Error

countBrk2:
        pop     dx
        call    displayMsg
        db      ' ',d_text,20,d_el
        push    dx                      ;eom=ret address
        call    display
        db      ' Break #',d_ed1
        _getb   breakNumber, 0
        db      '  [0=All]',d_ed2
        pop     bp
        mov     al, break_Info_Len
        mul     byte ptr breakNumber
        mov     bx, offset break_Info_End
        test    ax, ax
        jz      brk_LoopAll
        add     ax, offset break_Info1 - break_Info_Len
        cmp     ax, bx
        jae     brk_BadNum
        xchg    bx, ax
        test    [bx].brk_Flags, BRK_INUSE
        jz      brk_NotInUse
        call    bp
        jmp     DisplayAll

brk_LoopAll:
        sub     bx, break_Info_Len
        test    [bx].brk_Flags, BRK_INUSE
        jz      brk_LoopNext
        call    bp
brk_LoopNext:
        cmp     bx, offset break_Info1 + break_Info_Len
        jae     brk_LoopAll
        jmp     DisplayAll

brk_BadNum:
brk_NotInUse:
        call    displayMsg
        db      ' Bad break number.',d_el
        jmp     Menu_Error

IntHook_Enable:
        call    LocateHook
        db      'Enable',d_el
        or      [bx+3].mon_flags, MON_ACTIVE
        ret

IntHook_Remove:
        call    LocateHook
        db      'Remove',d_el
        lea     si, [bx+3].mon_oldVector ;restore previous int
        xor     ax, ax
        mov     es, ax
        mov     al, [bx+3].mon_intNum
        shl     ax, 1
        shl     ax, 1
        xchg    ax, di
        cmp     es:[di+0], bx
        jne     IntHookDisable
        mov     ax, cs
        xor     ax, es:[di+2]
        jne     IntHookDisable
        cli
        movsw
        movsw
        sti
        mov     [bx+3].mon_flags, al
        ; reset bx to force LocateHook routine to rescan (if all)
        mov     bx, offset IntHook_End
        ret

cmd_HookDisable:
        test    ah, K_CTL + K_ALT
        jz      cmd_HookAdd
IntHook_Disable:
        call    LocateHook
        db      'Disable',d_el
IntHookDisable:
        and     [bx+3].mon_flags, not MON_ACTIVE
        ret

cmd_HookAdd:
        test    ah, K_ALT
        jnz     IntHook_Remove
        test    ah, K_CTL
        jnz     IntHook_Enable
        mov     bx, offset IntHook_Beg
        xor     dx, dx
monFnd1:inc     dx
        test    [bx+3].mon_flags, MON_INUSE
        jz      monFnd2
        add     bx, Hook_Info_Len
        cmp     bx, offset IntHook_End
        jb      monFnd1
        call    displayMsg
        db      ' All hooks used.',d_el
        jmp     Menu_Error

monFnd2:push    bx
        push    dx                      ; hook number
        call    displayMsg
        db      ' Hook #',d_stk08,': INT #',d_ed1
        _getb   moniNum, 0
        db      ' If ('
        _getw   moniSubNum1, 0
        db      ' ó AX ó '
        _getw   moniSubNum2, 0ffffh
        db      ')  Wait:'
        _yesno  moniWait, 'Y'
        db      '  Print:'
        _yesno  moniPrint, 'N'
        db      '  Setup:'
        _yesno  moniSetup, 'N'
        db      '  Custom:'
        _yesno  moniCustom, 'N'
        db      d_ed2
        pop     bx
        in      al, 21h
        mov     byte ptr [setmsk2], al
        mov     ax, moniSubNum1
        mov     [bx+3].mon_funNum1, ax
        mov     ax, moniSubNum2
        mov     [bx+3].mon_funNum2, ax
        mov     al, moniNum
        mov     [bx+3].mon_intNum, al
        mov     ah, 0
        shl     ax, 1
        shl     ax, 1
        xchg    ax, di
        xor     ax, ax
        push    es
        mov     es, ax
        mov     ax, cs
        mov     dx, bx
        cli
        xchg    dx, es:[di]
        xchg    ax, es:2[di]
        mov     [bx+3].mon_oldVector[0], dx
        mov     [bx+3].mon_oldVector[2], ax
        sti
        pop     es
        mov     al, (MON_ACTIVE + MON_INUSE)/16
        cmp     moniWait, 'Y'
        rcl     al, 1                   ; MON_WAIT
        cmp     moniPrint, 'Y'
        rcl     al, 1                   ; MON_PRINT
        cmp     moniSetup, 'Y'
        rcl     al, 1                   ; MON_SETUP
        cmp     moniCustom, 'Y'
        rcl     al, 1                   ; MON_CUSTOM
        xor     al, 15
        mov     [bx+3].mon_flags, al
        jmp     DisplayAll

LocateHook:
        pop     dx
        call    displayMsg
        db      ' ',d_text,20,' Hook #',d_el
        push    dx                      ;eom=ret address
        call    display
        db      d_ed1
        _getb   IntHookNum, 0
        db      '  [0=all]',d_ed2
        pop     bp
        mov     bx, offset IntHook_End
        mov     ax, Hook_Info_Len
        xchg    ah, byte ptr IntHookNum
        dec     ah
        js      applyForAll

        mul     ah
        add     ax, offset IntHook_Beg
        cmp     ax, bx
        jae     IntHookNotInUse
        xchg    bx, ax
        test    [bx+3].mon_flags, MON_INUSE
        jz      IntHookNotInUse
        call    bp
        jmp     MenuCommand

loopForAll:
        test    [bx+3].mon_flags, MON_INUSE
        jz      applyForAll
        call    bp
applyForAll:
        sub     bx, Hook_Info_Len
        cmp     bx, offset IntHook_Beg
        jae     loopForAll
        mov     keycode, 1              ;force scrolllines return
        jmp     cmd_ListHooks

IntHookNotInUse:
        call    display
        db      a_r,'  NOT IN USE.',d_el
        jmp     Menu_Error

Handle_Hooks:
        push    es
        push    ds
        push    di     
        push    si
        push    bp
        push    dx
        push    cx
        push    bx
        push    ax
        pushf
        mov     bp, sp
        push    cs
        pop     ds
        mov     bx, [bp].mi_params

        test    [bx].mon_flags, MON_ACTIVE
        jz      monRet
        test    iflag, F_HOOKING
        jnz     monRet
        cmp     whereto, 0
        je      monRet
        cmp     ax, [bx].mon_funNum1
        jb      monRet
        cmp     ax, [bx].mon_funNum2
        jbe     monDisplay
monRet: mov     ax, [bx].mon_oldVector[0]
        xchg    ax, [bp].mi_es
        mov     es, ax
        mov     ax, [bx].mon_oldVector[2]
        mov     [bp].mi_params, ax
        popf
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     bp
        pop     si
        pop     di
        pop     ds
        retf

monDisplay:
        test    [bx].mon_flags, MON_CUSTOM
        jz      @f
        mov     bl, [bx].mon_intNum
        mov     bh, 0
        mov     ds, [bp].mi_ds
        ; ss:bp = ptr registers
        ; bl    = int#
        db      9ah                     ; call ssss:oooo
customCall      dw 0,0
        ; ret with CF=0 to process the interrupt, CF=1 to ignore
        push    cs
        pop     ds
        mov     bp, sp                  ; ss:bp => mi-flg, mi_ax, mi_bx...
        mov     bx, [bp].mi_params
        jc      monRet
@@:     or      iflag, F_HOOKING
        push    cs
        pop     es
        cld
        mov     di, offset registr[79]
        lea     si, [bp].mi_ax
        mov     cx, 10          ;flags, ax, bx,... ds, es
        mov     ax, [bp].mi_flags
@@:     call    hexw1
        lods    word ptr ss:[si]
        loop    @b

        mov     ax, ss          ;ss
        call    hexw1
        lea     ax, [bp].mi_flags[2] ;sp
        call    hexw1
        lds     si, dword ptr [bp].mi_ip
        mov     dl, 'h'
        cmp     byte ptr -2[si],0cdh
        jne     @f
        mov     dl, 's'
        dec     si
        dec     si
@@:     mov     ax, ds          ;cs
        call    hexw1
        mov     ax, si          ;ip
        call    hexw1
        push    cs
        pop     ds              ;ds=cs
        inc     di
        mov     al, [bx].mon_intNum
        call    hexb
        mov     [di], dl
        add     di, 3
        mov     ax, bx
        sub     ax, offset IntHook1 - Hook_Info_Len
        mov     cl, Hook_Info_Len
        div     cl
        call    hexb
        test    cs:[bx].mon_flags, MON_SETUP
        jz      prtint0
        cli
        mov     ds, cs:zero
        mov     word0[4*brk2Int+0], offset Break2_Trap
fix_Break2W3    equ $-4
        mov     word0[4*brk2Int+2], cs
fix_Break2S3    equ $-2
        mov     word0[4*9+0], offset kbd_Int
        mov     word0[4*9+2], cs
        mov     word0[4*8+0], offset timer_Int ;set timer tick
        mov     word0[4*8+2], cs
        mov     al, 0
setmsk2 equ     $-1
        out     21h, al
        sti
prtint0:push    cs
        pop     ds
        test    [bx].mon_flags, MON_PRINT
        jz      regdisp
        mov     si, offset regvalu
        dec     prtlin
        jns     prtint1
        mov     prtlin, 60
        mov     si, offset newpage
prtint1:call    printLine
        test    [bx].mon_flags, MON_WAIT
        jnz     regdisp
        jmp     montret
regdisp:mov     es, cursor[2]
        mov     cx, 160
        mov     si, offset registr
        xor     di, di
dspreg: lodsb
        mov     ah, Attrib[a_h-a_first]
        xchg    ax, es:[di]
        mov     monbuf[di], ax
        inc     di
        inc     di
        loop    dspreg
        mov     cl, 160
        mov     si, offset monbuf
        xor     di, di
        test    [bx].mon_flags, MON_WAIT
        jz      nowait
        cli
        mov     ds, di
        mov     ax, offset kbd_Int
        xchg    ax, 0[di+4*9]
        push    ax
        mov     ax, cs
        xchg    ax, 2[di+4*9]
        push    ax
        mov     ax, offset timer_Int
        xchg    ax, 0[di+4*8]
        push    ax
        mov     ax, cs
        xchg    ax, 2[di+4*8]
        push    ax
        in      al, 21h
        push    ax
        mov     al, 0fch
        out     21h, al
        sti
        push    cs
        pop     ds
w4rshft:xor     ax, ax
        xchg    al, byte ptr keycode
        cmp     al, 15          ;TAB?   step into
        je      monStepIn
        cmp     al, 56          ;ALT?   step over
        je      monStepOvr
        xor     al, 29          ;CTRL?  next
        jne     w4rshft
        mov     ds,di
        cli
        pop     ax
        out     21h, al
        pop     word ptr [di+4*8+2]
        pop     word ptr [di+4*8+0]
        pop     word ptr [di+4*9+2]
        pop     word ptr [di+4*9+0]
        push    cs
        pop     ds
nowait: rep     movsw
montret:and     iflag,not F_HOOKING
        jmp     monRet

monStepOvr:
        mov     ds, di
        cli
        pop     ax
        out     21h, al
        pop     word ptr [di+4*8+2]
        pop     word ptr [di+4*8+0]
        pop     word ptr [di+4*9+2]
        pop     word ptr [di+4*9+0]
        push    cs
        pop     ds
        rep     movsw

        and     iflag, not F_HOOKING
        mov     bx, [bp].mi_flags
        xchg    bx, [bp].mi_params
        mov     si, [bx].mon_oldVector[0]
        mov     di, [bx].mon_oldVector[2]
        xchg    si, [bp].mi_si
        xchg    di, [bp].mi_di
        mov     ds, [bp].mi_ds
        mov     es, [bp].mi_es
        mov     [bp].mi_ds, offset copyFlags
        mov     [bp].mi_es, cs
        popf
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     bp
        retf

monStepIn:
        rep     movsw
        cli
        mov     ds, cx
        pop     ax
        out     21h, al
        pop     word0[4*8+2]
        pop     word0[4*8+0]
        pop     word0[4*9+2]
        pop     word0[4*9+0]
        push    cs
        pop     ds
        and     iflag, not F_HOOKING
        pop     bx                      ;flags
        xchg    bx, [bp].mi_params
        mov     si, [bx].mon_oldVector[0]
        mov     di, [bx].mon_oldVector[2]
        xchg    si, [bp].mi_ds
        xchg    di, [bp].mi_es
        mov     ds, si
        mov     es, di
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     bp
        pop     si
        pop     di
        jmp     SoftProbeEntry

copyFlags:
        push    bp
        mov     bp, sp
        pushf
        pop     6[bp]                   ;to client flags
        pop     bp
        jmp     SoftProbeEntry

        IFNDEF  _BOOT
Psp_Name        db 9 dup (0)
Blk_Files       db 'Files',0
Blk_Fcbs        db 'Fcbs',0
Blk_Buffers     db 'Buffers',0
Blk_LastDriv    db 'LastDriv',0
Blk_Stack       db 'Stacks',0

PUBPROC FindOwner
        mov     cl, 4
        shr     dx, cl
        mov     ax, ds
        add     ax, dx
        jc      noPname
PUBPROC FindPsp
        ; In:   ax = segment
        ; Out:  BX=PSP, DS:SI=Psp name
        mov     si, cs:MCB_Head
        xor     bx, bx
        cmp     ax, si
        jb      noPname
FndPsp1:mov     ds, si
FndPsp2:stc
        adc     si, [bx+3]       ;+LEN+1 = next block
        jc      noPname
        cmp     ax, si
        jb      FndPsp3
        cmp     byte ptr [bx], 'Z'
        jne     FndPsp1
        mov     ds, si
        cmp     byte ptr [bx], 'M'
        je      FndPsp2
noPname:push    cs
        pop     ds
        mov     si, offset Psp_Name+8
        ret

FndPsp3:mov     bx, 1[bx]       ;PSP
        test    bx, bx
        jz      noPname
        dec     bx
        mov     ds, bx
        inc     bx
        cmp     bx, word0[1]    ;belongs to PSP?
        jne     noPname
        ; ds:8 is the psp name if its either a PSP or a device driver

        cmp     cs:DosVer[1], 5
        jb      noPname
        mov     al, byte0[0]
        cmp     al, 'M'                 ;PSP or ENV
        je      hasName
        cmp     al, 'D'                 ;device=
        je      hasName
        cmp     al, 'Z'
        je      hasName
        mov     si, offset Blk_Files
        cmp     al, 'F'
        je      pspnam1
        mov     si, offset Blk_Fcbs
        cmp     al, 'X'
        je      pspnam1
        mov     si, offset Blk_Buffers
        cmp     al, 'B'
        je      pspnam1
        mov     si, offset Blk_LastDriv
        cmp     al, 'L'
        je      pspnam1
        cmp     al, 'S'
        jne     noPname
        mov     si, offset Blk_Stack
pspnam1:push    cs
        pop     ds
        ret
hasName:mov     si, 8                   ;ds:8=psp name
        ret

PUBPROC OwnerName
        ; In:   AX=Segment
        ; Out:  DS:SI=DS:DX=name, BX=PSP, CX=Env
        call    FindPsp
        ; bx=psp, ds:si=psp name
        ; look for file name from env.

        push    bx
        mov     cx, 8           ;max length of name
        xor     bx, bx
owner1: lodsb
        mov     cs:Psp_Name[bx], al
        inc     bx
        loop    owner1
        pop     bx
        ; cx = 0 assume no env

        dec     bx
        mov     ds, bx
        inc     bx
        cmp     word ptr ds:[10h], 20cdh ;int 20 as marker
        jne     owner3
        mov     cx, word ptr ds:[2ch+16] ;environment
        jcxz    owner3
        dec     cx
        mov     ds, cx
        inc     cx
        cmp     bx, word0[1]
        jne     owner3
        mov     si, 16
        lodsb
owner2: mov     ah, al
        lodsb
        test    ax, ax
        jnz     owner2
        lodsw                           ;1,0 if progname there
        dec     ax
        jz      owner4
owner3: mov     si, offset Psp_Name
        push    cs
        pop     ds
owner4: mov     dx, si
        ret
        ENDIF

strLen: xor     cx, cx
strln1: lodsb
        inc     cx
        cmp     al,0
        jne     strln1
        sub     si, cx
        dec     cx                      ;do not count the null
        ret

absaddr:push    cx
        lodsw
        mov     cx, 0f04h
        and     ch, al
        shr     ax, cl
        add     ax, [si]
        mov     cl, ch
        mov     ch, 0
        mov     si, cx
        pop     cx
        ret

dosTerminate:
        mov     ax,4cffh
dos:    pushf
        db      9ah             ;call seg:off
i21vec  dw      0,4*21h
        ret

ResetVideo:
        mov     ax,3
        pushf
        db      9ah             ;call seg:off
vidvec  dw      0,4*10h         ;video driver
        ret

printLine:
        push    dx
        mov     ax, 13
prntch1:push    si
        mov     dx, 0
printPort       equ $-2
        pushf
        db      9ah             ;call seg:off
prtvec  dw      0,4*17h         ;printer vector
        pop     si
        lods    byte ptr cs:[si]
        and     ax, 0ffh
        jnz     prntch1
        pop     dx
        ret

watchExpStruc   struc
  wwin_segv     dw ?            ; value for the segment (if not a reg)
  wwin_seg      dw ?            ; pointer to the register/value
  wwin_off1     dw ?            ; pointer to the first offset
  wwin_off2     dw ?            ; pointer to the second offset
  wwin_dspl     dw ?            ; displacement
  wwin_expr     db 16 dup (?)   ; experssion (should be last field)
watchExpStruc   ends

watch_Exp  macro n, w, s
           local l1
watchExp&n watchExpStruc <w>
l1:
           org l1-16
           db  s
           org l1
           endm

watch_Exp 1, <zero,save_ds,save_si,zero,0>,'DS:SI'
watch_Exp 2, <zero,save_es,save_di,zero,0>,'ES:DI'
watch_Exp 3, <zero,save_ds,save_dx,zero,0>,'DS:DX'
watch_Exp 4, <zero,save_es,save_bx,zero,0>,'ES:BX'
watch_Exp 5, <zero,save_ss,save_sp,zero,0>,'SS:SP'
watch_Exp 6, <zero,save_ss,save_bp,zero,0>,'SS:BP'
watch_Exp 7, <zero,save_cs,save_ip,zero,0>,'CS:IP'
watch_Exp 8, <zero,save_es,save_si,zero,0>,'ES:SI'
watchExpEnd:
watchExpLen     equ (watchExp2 - watchExp1)

cmd_Watch1:
        test    ah, K_ALT
        jnz     ChgWatchExp
        mov     al, 1
nxtWatch:
        add     al, watchNo
        and     al, 7
        mov     cl, al
        add     al, '0'
        mov     watchNo, al
        mov     al, watchExpLen
        mul     cl
        add     ax, offset watchExp1
        mov     word ptr [watchWinExp], ax
        jmp     DisplayAll

cmd_Watch2:
PrevWatchExp:
        mov     al, -1
        jmp     nxtWatch

ChgWatchExp:
        mov     si, word ptr [watchWinExp]
        push    cs
        pop     es
        mov     di, offset dissBuffSeg
        mov     cx, watchExpLen
        rep     movsb
editWatch:
        mov     al, watchNo
        and     al, 7
        push    ax
        call    displayMsg
        db      ' Watch #',d_stk08,' [',d_ed1
        _geta   dissBuffSeg.wwin_expr, 15
        db      ']',d_ed2
        push    cs
        pop     es
        mov     bp, offset dissBuffSeg
        mov     si, offset dissBuffSeg.wwin_expr
getWSeg:call    watchReg
        jnc     setWSeg
        call    getHexWord
        jc      editWatch
        mov     [bp].wwin_segv, ax
        mov     ax, word ptr [watchWinExp]
setWSeg:mov     [bp].wwin_seg, ax
        lodsb
        cmp     al, ':'
        jne     editWatch

        ; now get the offset
        mov     [bp].wwin_dspl, 0
        mov     ax, offset zero
        mov     [bp].wwin_off1, ax
        mov     [bp].wwin_off2, ax
        call    watchReg
        jc      getWDsp
        mov     [bp].wwin_off1, ax
        lodsb
        cmp     al, '+'
        jne     @f
        call    watchReg
        jc      getWDsp
        mov     [bp].wwin_off2, ax
        lodsb
        cmp     al, '+'
        je      getWDsp
@@:     cmp     al, '-'
        jne     chkWEnd
        dec     si
getWDsp:call    getHexWord
        jc      editWatch
        mov     [bp].wwin_dspl, ax
        lodsb
chkWEnd:cmp     al, 0
        jne     editWatch
        mov     si, bp
        mov     di, word ptr [watchWinExp]
        mov     cx, watchExpLen -16     ; exclude wwin_expr
        rep     movsb
        mov     cl, 16
@@:     lodsb
        call    toUpper
        stosb
        loop    @b
        jmp     DisplayAll

reg_address     db save_ax-save_ax, save_cx-save_ax
                db save_dx-save_ax, save_bx-save_ax
                db save_sp-save_ax, save_bp-save_ax
                db save_si-save_ax, save_di-save_ax
                ; segment registers
                db save_es-save_ax, save_cs-save_ax
                db save_ss-save_ax, save_ds-save_ax
                db save_fs-save_ax, save_gs-save_ax
watchReg:
        call    get_RegWord
        jnc     watchR1
        call    parse_line      ;see RM__xxxx
        CmdDef  'IP'    _RELGOTO, watchR2-$+3, save_ip-save_ax
        db      0
        call    get_segRegisters
        jc      watchR3
        ; convert 00sss000
        mov     cl, 3
        shr     ax, cl
        add     al, 8
watchR1:mov     bx, offset reg_address
        xlat
watchR2:add     ax, offset save_ax
watchR3:ret

displayCodeWin:
        mov     bp, code_ptr[0]
        mov     dx, code_ptr[2]
        mov     di, POS_VIEW
dispCodeLoop:
        push    di
        push    dx
        push    bp
        mov     si, sp
        call    FindBreak
        cmc
        sbb     bx, bx                  ;-1(break) or 0(no break)
        mov     ah, Attrib[bx+a_n-a_first]
        and     bx, cx                  ;0 or break No.
        pop     bp
        pop     dx
        mov     al, ' '
        cmp     dx, save_cs
        jne     @f
        cmp     bp, save_ip
        jne     @f
        mov     al, 04h
@@:     push    bx
        push    ax
        call    dispins
        pop     ax                      ;ah=attr, al=IP marker
        pop     bx
        pop     di
        mov     cx, VIEW_COLS
        mov     es, cursor[2]
displn1:stosw
        lodsb
        cmp     al, 0
        loopne  displn1
        mov     al, ' '
        rep     stosw
        test    bx, bx
        jz      displn2
        push    bx
        call    display
        db      d_mv,-4,'<',d_stk08,'>',d_el
displn2:add     di, 24
        cmp     di, POS_VIEW + 160*VIEW_ROWS
        jb      dispCodeLoop
        ret

CodeWinDown1:
        mov     cx, 1
CodeWinDown:
        mov     bp, code_ptr[0]
CodeWinDown2:
        mov     dx, code_ptr[2]
        push    cx
        call    dispins
        pop     cx
        loop    CodeWinDown2
        mov     code_ptr[0], bp
        ret

CodeWinUp:
        mov     bp, code_ptr[0]
        mov     dx, code_ptr[2]
        sub     bp, 24
        jnc     CodeWinUp2
        xor     bp, bp
CodeWinUp2:
        push    bp
        call    dispins
        pop     ax
        mov     bx, bp
        sub     bx, code_ptr[0]
        js      CodeWinUp2
        mov     code_ptr[0], ax
        ret

displayWatchData:
        push    cs
        pop     ds
        mov     di, offset watchExp1
watchWinExp     equ $-2
        mov     si, [di].wwin_off1
        lodsw
        mov     si, [di].wwin_off2
        add     ax, [si]
        add     ax, [di].wwin_dspl      ; displacement
        push    ax                      ; offset
        mov     watch_ptr[0], ax
        mov     si, [di].wwin_seg
        lodsw
        push    ax                      ; segment
        mov     watch_ptr[2], ax
        lea     dx, [di].wwin_expr
        mov     di, POS_WATCHWIN - 160 + 2 ; on the frame
        call    display
        db      a_h,d_stk32,a_f,d_dup,16,'Í',d_mv,38,a_h,'['
watchNo db      '0]',d_mv,-56,d_text,15,d_el
        lds     dx, dword ptr watch_ptr
        mov     di, POS_WATCHWIN + 2
        call    word ptr cs:watch_Format
        call    word ptr cs:watch_Format
displayDataWin:
        lds     dx, dword ptr cs:data_ptr
        push    dx
        push    ds
        IFNDEF  _BOOT
        call    FindOwner
        mov     dx, si
        mov     di, POS_DATAWIN - 160 + 22 ;on the frame
        call    display
        db      a_f,d_dup,8,'Í',d_mv,-18,a_h,d_stk32,d_mv,1,d_text,8,d_el
        lds     dx, dword ptr cs:data_ptr
        ELSE
        mov     di, POS_DATAWIN - 160 + 2
        call    display
        db      a_h,d_stk32,d_el
        ENDIF
        mov     bp, 4
        mov     di, POS_DATAWIN + 2
memdsp1:call    word ptr cs:data_Format
        dec     bp
        jnz     memdsp1
subrret:push    cs
        pop     ds
        ret

        db      16
disp_DataByte:
        call    display
        db      a_n,d_mem08,16,d_el
        sub     dx, 16
        call    display
        db      '³ ',d_ascii,16,d_el
        add     di, 28
        ret

        db      16
disp_DataWord:
        push    dx
        call    display
        db      a_h,d_stk16,a_n,' ',d_mem16,8,'   ',d_el
        sub     dx, 16
        call    display
        db      '³ ',d_ascii,16,d_el
        add     di, 28
        ret

        db      16
disp_DataDword:
        push    dx
        call    display
        db      a_h,d_stk16,a_n,' ',d_mem32,4,'   ',d_el
        sub     dx, 16
        call    display
        db      '³ ',d_ascii,16,d_el
        add     di, 28
        ret

        db      64
disp_DataAscii:
        call    display
        db      a_n,d_ascii,64,'  ',d_el
        add     di, 28
        ret

toUpper:cmp     al, 'a'
        jb      uprRet
        cmp     al, 'z'+1
        jae     uprRet
        sub     al, 'a' - 'A'
uprRet: ret

cmd_AsciiChr:
        call    scrollLines
        db      'HEX  DEC  CHR ³ HEX  DEC  CHR ³ HEX  DEC  CHR ³ HEX  DEC  CHR',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        mov     cx, 256/4
        cmp     bx, cx
        jnc     asciiChr2
asciiChr1:
        push    bx
        mov     dx, sp                  ; ds:dx=ptr chr
        push    bx
        push    bx
        call    display
        db      ' ',d_stk08,d_decim,d_mv,-5,'  ',d_mv,6,d_string,1,d_mv,4,d_el
        pop     bx
        add     bl, 40h
        jnc     asciiChr1
asciiChr2:
        cmc
        ret

Error_Msg       db 'Error...',d_el

cmd_Assemble:
        mov     di, code_ptr[0]
asm_Loop:
        mov     dx, offset Esc4Cancel
        mov     bx, 40
asm_Buf_Init:
        mov     dissBuffOpcode[bx], bh
        dec     bx
        jns     asm_Buf_Init
asm_Error:
        push    di                      ;save it for Assemble
        push    di
        push    code_ptr[2]
        call    displayMsg
        db      ' ',d_stk32,' [',d_ed1
        _geta   dissBuffOpcode, 40
        db      '] ³ ',d_text,20,d_ed2
;; if empty line, go next instruction
        pop     bp
        mov     si, offset dissBuffOpcode
        cmp     byte ptr [si], 0
        jne     compileLine
        mov     dx, code_ptr[2]
        call    dispins
        push    bp
        jmp     short FitInWindow
compileLine:
        push    bp
        call    Assemble
        mov     cx, di
        pop     di
        mov     dx, offset Error_Msg
        jc      asm_Error
        mov     si, offset asmBuffer
        sub     cx, si
        mov     es, code_ptr[2]
        rep     movsb
        ;display new code
        push    di
        call    displayWatchData
FitInWindow:
        call    displayCodeWin  ;display code_ptr
        pop     di
        cmp     di, bp
        jb      asm_Loop
        push    di
        call    CodeWinDown1
        jmp     FitInWindow

doMark9:mov     bl, 2*9
        jmp     short doMarkers

doMark8:mov     bl, 2*8
        jmp     short doMarkers

doMark7:mov     bl, 2*7
        jmp     short doMarkers

doMark6:mov     bl, 2*6
        jmp     short doMarkers

doMark2:mov     bl, 2*2
        jmp     short doMarkers

doMark0:mov     bl, 0
doMark5:
doMark4:
doMark3:
doMark1:
doMarkers:
        test    ah, K_ALT       ;shift-altN=record macro N
        jnz     record_keys
        mov     si, offset code_ptr
        shl     bx, 1           ;4 bytes per entry
        test    ah, K_CTL       ;shift-ctrlN=set code marker N
        jnz     set_marker
        mov     si, offset data_ptr
set_marker:
        lodsw
        mov     [si+bx+2], ax
        lodsw
        mov     [si+bx+2], ax
        jmp     MenuCommand

record_keys:
        Record_A_Macro
        jmp     MenuCommand

goMarker:
        and     bl, 1eh         ;2*0..2*9
        test    ah, K_ALT       ;altN=play macro N
        jz      goCorDmarker
        Play_A_Macro
        jmp     MenuCommand

goCorDmarker:
        mov     si, offset code_ptr
        shl     bx, 1           ;4 bytes per entry
        test    ah, K_CTL       ;ctrlN=set code marker N
        jnz     get_marker
        mov     si, offset data_ptr
get_marker:
        mov     ax, [si+bx+4][0]
        mov     dx, [si+bx+4][2]
        mov     [si+0], ax
        mov     [si+2], dx
        jnz     updateCWin
        jmp     dspdata
updateCWin:
        jmp     NewCodeWin

        IFDEF   _BOOT
cmd_ListFiles   equ cmd_NoSupport
        ELSE

CheckInDos:
        lds     si, dword ptr cs:InDosPtr
        lodsb
        push    cs
        pop     ds
        cmp     al,0
        jne     is_inDos
        ret

is_inDos:
        call    displayMsg
        db      ' Already in DOS. Finish current DOS call and try again.',d_el
        jmp     Menu_Error

cmd_ListFiles:
        call    scrollLines
        db      ' FILENAME.EXT  AC  #REF  POSITION  OWNER',d_el
        ; bx=number of records to skip (logical record no.)
        ; es:di=screen ptr
        ; ah=attrib
        xor     dx, dx
        lds     bp, cs:List_Ptr
        add     bp, 4
ListFile1:
        lds     bp, dword ptr ds:[bp]   ; ptr list of Dos file tables
        cmp     bp, -1
        je      ListFile4
        mov     si, bp                  ; ds:si=ptr file table
        mov     cx, word ptr ds:[si+4]  ; number of files in this table
        jcxz    ListFile1
ListFile2:
        cmp     word ptr ds:[si+6+00h], 0 ; # of file handles to this file
        jz      ListFile3
        cmp     dx, bx
        je      ListFile5
        inc     dx
ListFile3:
        add     si, 35h                 ; 28(dos 2.x), 35(3.1), 3b(4.x)
dos_fix1        equ $-1
        loop    ListFile2
        jmp     ListFile1

ListFile4:
        mov     cx, dx
        stc
        ret

ListFile5:
        lea     dx, [si+6+20h]          ; FCB format file name
                                        ; 04(2.x), 20(3.1+)
        mov     bp, ds:[si+6+31h]       ; owner PSP
        push    bp                      ; owner PSP
        push    word ptr ds:[si+6+15h]  ; current offset in file (lo)
        push    word ptr ds:[si+6+17h]  ; current offset in file (hi)
        push    word ptr ds:[si+6+00h]  ; # of file handles to this file
        push    word ptr ds:[si+6+02h]  ; access mode
        call    display
        db      ' ',d_string,8,'.',d_string,3,'  ',d_stk08,'  '
        db      d_stk16,'  ',d_stk16,d_stk16,'  ',d_stk16,d_el
        xchg    ax, bp
        call    FindPsp
        mov     dx, si
        call    display
        db      a_n,'  ',d_text,8,d_el
        ;clc
        ret

        ENDIF

PUBPROC UpdatePrevRegs
        mov     di, offset prev_ax
Save_Registers:
        push    cs
        pop     es
        mov     si, offset save_ax
        mov     cx, 14
        rep     movsw
        and     byte ptr prev_fl[1], not 1 ;ignore trace
        ret

        even
MainMenuCmdTable label word
        dw      MenuCommand     ;SPACE
        dw      doMark1         ;!=~1   set: dataMarker, ^codeMarker, @macro
        dw      MenuCommand     ;"
        dw      doMark3         ;#=~3
        dw      doMark4         ;$=~4
        dw      doMark5         ;%=~5
        dw      doMark7         ;&=~7
        dw      MenuCommand     ;'
        dw      doMark9         ;(=~9
        dw      doMark0         ;)=~0
        dw      doMark8         ;*=~8
        dw      cmd_Trace       ;+
        dw      MenuCommand     ;,
        dw      cmd_Step        ;-
        dw      cmd_DispCsIp    ;.
        dw      MenuCommand     ;/
        dw      goMarker        ;0
        dw      goMarker        ;1
        dw      goMarker        ;2
        dw      goMarker        ;3
        dw      goMarker        ;4
        dw      goMarker        ;5
        dw      goMarker        ;6
        dw      goMarker        ;7
        dw      goMarker        ;8
        dw      goMarker        ;9
        dw      MenuCommand     ;:
        dw      MenuCommand     ;;
        dw      MenuCommand     ;<
        dw      cmd_Trace       ;=
        dw      MenuCommand     ;>
        dw      displayHelp     ;?
        dw      doMark2         ;@=~2
        dw      cmd_Assemble    ;A      Assemble
        dw      cmd_Break1      ;B
        dw      cmd_MemCopy     ;C
        dw      cmd_DispData1   ;D
        dw      cmd_EditMem     ;E
        dw      cmd_FindNext    ;F
        dw      cmd_Go          ;G
        dw      cmd_HookAdd     ;H      ^H=enable, @H=remove
        dw      cmd_InByte      ;I      IN b/w
        dw      cmd_DispSize    ;J
        dw      cmd_DispStk1    ;K
        dw      cmd_ListBreaks  ;L
        dw      MenuCommand     ;M
        dw      cmd_NewInts     ;N
        dw      cmd_OutByte     ;O
        dw      MenuCommand     ;P
        dw      cmd_Go          ;Q      Quit/Go
        dw      cmd_Replace     ;R
        dw      cmd_Step        ;S
        dw      cmd_TraceUp     ;T
        dw      cmd_DispCode1   ;U
        dw      cmd_UsrScreen   ;V
        dw      cmd_Watch1      ;W
        dw      cmd_HexCalc     ;X
        dw      cmd_ReadWMem    ;Y
        dw      MenuCommand     ;Z
        dw      MenuCommand     ;[
        dw      MenuCommand     ;\
        dw      MenuCommand     ;]
        dw      doMark6         ;^=~6
        dw      MenuCommand     ;_
        dw      MenuCommand     ;`
        dw      cmd_AsciiChr    ;a
        dw      cmd_Break2      ;b
        dw      cmd_Compare     ;c
        dw      cmd_DispData2   ;d
        dw      MenuCommand     ;e
        dw      cmd_Find        ;f
        dw      cmd_GoHere      ;g
        dw      cmd_HookDisable ;h
        dw      cmd_InWord      ;i
        dw      cmd_WatchSize   ;j
        dw      cmd_DispStk2    ;k
        dw      cmd_ListHooks   ;l
        dw      cmd_FillMem     ;m
        dw      cmd_ListInts    ;n
        dw      cmd_OutWord     ;o
        dw      cmd_Print       ;p
        dw      cmd_GoHere      ;q
        dw      cmd_SaveRegs    ;r
        dw      MenuCommand     ;s
        dw      cmd_TraceCnt    ;t
        dw      cmd_DispCode2   ;u
        dw      cmd_UsrScreen   ;v
        dw      cmd_Watch2      ;w
        dw      cmd_DecCalc     ;x (decimal)
        dw      cmd_WriteWMem   ;y
        dw      MenuCommand     ;z
        dw      MenuCommand     ;{
        dw      MenuCommand     ;|
        dw      MenuCommand     ;}
        dw      MenuCommand     ;~
        dw      MenuCommand     ;del
        dw      cmd_DataUp      ;UP
        dw      cmd_DataDn      ;DOWN
        dw      cmd_DataInc     ;RIGHT
        dw      cmd_DataDec     ;LEFT
        dw      cmd_CodeUp      ;PGUP
        dw      cmd_CodeDn      ;PGDN
        dw      watch2data      ;HOME
        dw      cmd_DispCsIp    ;END
        dw      cmd_DataSegUp   ;~UP
        dw      cmd_DataSegDn   ;~DOWN
        dw      cmd_DataNorm    ;~RIGHT
        dw      cmd_DataAlign   ;~LEFT
        dw      cmd_CodeOffDec  ;~PGUP
        dw      cmd_CodeOffInc  ;~PGDN
        dw      cmd_DispCode2   ;~HOME
        dw      cmd_DispData2   ;~END
        dw      displayHelp     ;F1
        dw      cmd_386Regs     ;F2
        dw      cmd_RefScreen   ;F3
        dw      cmd_UsrScreen   ;F4
        dw      cmd_Go          ;F5
        dw      MenuCommand     ;F6
        dw      MenuCommand     ;F7
        dw      cmd_Trace       ;F8
        dw      cmd_Break       ;F9
        dw      cmd_Step        ;F10
        dw      cmd_MonTimer    ;~F1
        dw      MenuCommand     ;~F2
        dw      MenuCommand     ;~F3
        dw      MenuCommand     ;~F4
        dw      MenuCommand     ;~F5
        dw      MenuCommand     ;~F6
        dw      cmd_CurPsp      ;~F7
        dw      cmd_ListFiles   ;~F8
        dw      cmd_DevList     ;~F9
        dw      cmd_MapMemory   ;~F10

        include sp_help.asm

newpage db      13,10,10,10
registr db      'flgs  ax   bx   cx   dx   bp   si   di   ds   es   ss   sp   cs   ip  int     ',13,10
regvalu db      '0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000:0000 0000:0000 00H H00 ',13,10,0
        even
int_buf dw      200h dup(0)     ;int vector saved area
monbuf  dw      160 dup(0)

savedSCREEN     label word
;----------------------------------------------------------------------------
; This area can be used during start up only as a safe area for loading WMem.
;----------------------------------------------------------------------------
        dw      MAC_FILE_MARKER         ;startup uses this area for file i/o
        dw      2*MAX_MACRO_SIZE
        dw      macroPtrs               ;start of macro buffer
        db      (savedSCREEN + 25*160 - $) dup (0) ;screen image buffer
;----------------------------------------------------------------------------
; Start of working memory
;----------------------------------------------------------------------------

        align   16
resident_Size   equ ($-code0+15)/16

workingAreaBeg:
        clc             ; process int
        retf            ; default for custom hook
        copyrightMsg 'SoftProbe'
        db      200 dup(0)