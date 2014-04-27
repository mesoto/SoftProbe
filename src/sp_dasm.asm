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

ScrachBuffer    label byte
asmBuffer       db 2 dup (0)            ;can use part of dissBuff
dissBuffSeg     db '0000:'
dissBuffOff     db '0000 '              ;address
dissBuffHex     db 13 dup(' ')          ;instruction hex value
dissBuffOpcode  db 07 dup(' ')          ;instruction opcode
dissBuffOprs    db 50 dup(' ')          ;instruction operand
dissBuffEnd     db 10,0                 ;end of buffer
ScrachBufferLen equ (dissBuffEnd - ScrachBuffer)

        even
asmBufPtr       dw 0
asmRmPtr        dw 0            ;points to the r/m byte
asmInputPtr     dw 0
actual_IP       dw 0

regbank         dw rm__00,rm__01,rm__02,rm__03,rm__04,rm__05,rm__06,rm__07
r32index        dw save_ax, save_cx, save_dx, save_bx
                dw save_sp, save_bp, save_si, save_di

OPR32   equ     1
ADR32   equ     2
rm_type db      0               ;bit0=32 bit operand size override
                                ;bit1=32 bit address size override
                                ;bit7=16 bit operand size override
sg_name db      0               ;override seg name (keep it with rm_type)

Arg_Type macro  name
        &name = @argt
        dw arg_&name
        @argt = @argt + 1
        endm

        @argt = 1       ;0 is used by _RELGOTO
        _RELGOTO = 0    ;forward jump only
even
arg_table label word
 Arg_Type _RETVALU      ;
 Arg_Type _GOTOADR      ;
 Arg_Type _NOPRND1      ;
 Arg_Type _NOPRND2      ;
 Arg_Type _STRSIZE      ;B/W/D in string operations
 Arg_Type _WRDSIZE      ;D in push(a/f)/pop(a/f)
 Arg_Type _PREFINS      ;
 Arg_Type _OPTWORD      ;
 Arg_Type _RELJMP1      ;
 Arg_Type _MULTYPE      ;
 Arg_Type _R16_MEM      ;
 Arg_Type _WRD_BYT      ;
 Arg_Type _RM8____      ; hh ll 00+rmb          SETcc
 Arg_Type _REGMEM_      ; hh+w ll+rm            NOT
 Arg_Type _0F_MEM_      ; 0F hh ll+rmw
 Arg_Type _ROTATE_      ; hh+i+c+w ll+rm        SHL  rm,1/cl/nn
 Arg_Type _SHLRD__      ; hh ll+w rm            SHLD r,rm,cl/nn
 Arg_Type _MEMADR_      ; memory address (not register)

OprndTypeTable label word
 dw @Opr_dword          ;r/m (dword) (used in opcode2) (0=inst2)
 dw @Opr_none           ;no operand inst's
 dw @Opr_prefix         ;Prefix instructions
 dw @Opr_segovr         ;seg: override
 dw @Opr_r16            ;r16    .....rrr
 dw @Opr_seg            ;seg    ...ss...
 dw @Opr_ax_r16         ;ax,r16 .....rrr
 dw @Opr_reg_rm         ;reg,r/m (8/16)
 dw @Opr_rm_reg         ;r/m,reg (8/16)
 dw @Opr_seg_rm         ;seg,r/m
 dw @Opr_rm_seg         ;r/m,seg
 dw @Opr_acc_addr       ;acc,address
 dw @Opr_addr_acc       ;address,acc
 dw @Opr_acc_imm        ;acc,imm
 dw @Opr_reg_imm        ;reg,imm  ....wrrr imm 
 dw @Opr_r16_rm         ;reg,r/m (word)
 dw @Opr_nn             ;nn
 dw @Opr_nnnn           ;nnnn (16)
 dw @Opr_nnnnw          ;nnnn (16/32) (push)
 dw @Opr_sx8            ;+nn/-nn
 dw @Opr_nnnn_nn        ;nnnn,nn (enter)
 dw @Opr_reg_rm_sx8     ;reg,r/m,+n/-n (imul)
 dw @Opr_reg_rm_nnnn    ;reg,r/m,nnnn  (imul)
 dw @Opr_rm             ;r/m    .......w mm...r/m (byte/word)
 dw @Opr_rmb            ;r/m    ........ mmxxxr/m (setcc byte)
 dw @Opr_rmw            ;r/m (16/32)
 dw @Opr_rmw_nn         ;bit operations r/m16/32,imm8
 dw @Opr_rm_imm         ;r/m,imm  .......w r/m imm
 dw @Opr_rm_sx8         ;r/m,+n/-n
 dw @Opr_rm_sc          ;r/m,1/cl/nn
 dw @Opr_mem24          ;r/m (gdt/idt)
 dw @Opr_r16_rm8        ;r16/32,r/m8 (movsx/movzx)
 dw @Opr_r32_rm16       ;r32,r/m16 (movsx/movzx)
 dw @Opr_lea_reg_rm     ;lea reg,r/m
 dw @Opr_rm_reg_sc      ;r/m,reg,cl (shld/shrd)
 dw @Opr_rel8           ;rel.8
 dw @Opr_rel16          ;rel.16
 dw @Opr_seg_off        ;seg:off
 dw @Opr_0fp            ;0F prefix
 dw @Opr_32ovr          ;32 bit override (op32/ad32)
 dw @Opr_float          ;ESC nn,r/m
 dw @Opr_fs_gs          ;push/pop fs/gs
 dw @Opr_0f_2x          ;mov CRn/DRn/TRn,r/m
 dw @Opr_in_nn          ;in al/ax/eax,nn
 dw @Opr_in_dx          ;in al/ax/eax,dx
 dw @Opr_out_nn         ;out nn,al/ax/eax
 dw @Opr_out_dx         ;out dx,al/ax/eax
 dw @Opr_sb_or_sw       ;sw/sd in ins lods/scas/movs/cmps/stos
 dw @Opr_d_for_32       ;push/pop f/a (fd/ad when 32)

TABSIZE equ     7

; Flags used in DX during assemble command
; DL flags
RM__WORD        equ 01h         ; implied word/dword size
RM__BYTE        equ 02h         ; implied byte size
RM__DWRD        equ 04h         ; 32-bit register/dword
RM__BASE        equ 10h         ;
RM__REGS        equ 20h         ; uses registers for addressing
RM__ADDR        equ 40h         ; memory reference not registers
; DH flags - instruction prefix
PRE_OPR32       equ 80h
PRE_ADR32       equ 40h
PRE_SMASK       equ 07h
PRE_ES          equ 1
PRE_CS          equ 2
PRE_SS          equ 3
PRE_DS          equ 4
PRE_FS          equ 5
PRE_GS          equ 6
seg_Prefix      db  26h, 2eh, 36h, 3eh, 64h, 65h

CmdDef  macro c, t, a, b
        local t1, t2
        db    t2
t1      db    c
t2      equ   ($-t1)+8*t
        ifb   <b>
        dw    a
        else
        db    b, a
        endif
        endm

skipBlanks:
        lodsb
        cmp     al, 9
        je      skipBlanks
        cmp     al, ' '
        je      skipBlanks
        dec     si
        ret

; IN:   DS:SI   = ptr data
; OUT:  CF=0    CX:AX = hex data
;       CF=1    no hex data
getHexValue:
        push    bx
        xor     bx, bx
        xor     cx, cx
        xor     ah, ah
        call    skipBlanks
        cmp     al, '+'
        je      gh_Val1
        cmp     al, '-'
        jne     gh_Val2
        mov     ah, 80h
gh_Val1:inc     si
gh_Val2:lodsb
        call    asc2hex
        jc      gh_Val4
gh_Val3:shl     bx, 1
        rcl     cx, 1
        jc      gh_Val6
        add     al, 40h                 ; loop 4 times
        jnc     gh_Val3
        or      bl, al
        inc     ah
        jmp     gh_Val2
gh_Val4:dec     si
        shl     ah, 1
        jnc     gh_Val5
        not     cx
        neg     bx
        adc     cx, 0
gh_Val5:sub     ah, 1                   ; CF=1 if 0
gh_Val6:xchg    ax, bx
        pop     bx
        ret

getHexWord:
        call    getHexValue
        jc      @f
        jcxz    @f
        add     cx, 1
        cmc
@@:     ret

PUBPROC parse_line
; In:   SP+0    = offset CmdDef's...0
;       SP+2    = offset return address
;       DS:SI   = ptr data/command to parse
;       DS      = CS
; Out:  CF=0    Found: Jump to arg_table[xx]
;       CF=1    Not found: AX=0, Jump to inst after CmdDef's...0
; Kills:        bx
        call    skipBlanks
        DBGBRK
PUBPROC parse_here
        pop     ax
        push    di              ;save di
        xchg    di, ax
        jmp     short parse_ln3
parse_ln1:
        inc     bx
        cmp     bl, ah
        jae     short parse_ln4
parse_ln2:
        mov     al, [si+bx]
        call    toUpper
        xor     al, [di+bx]
        jz      short parse_ln1
        mov     bl, ah
        lea     di, [di+bx+2]
parse_ln3:
        xor     bx, bx
        mov     ah, [di]        ;len + type*8
        inc     di
        and     ax, 700h        ;ah=len
        jnz     short parse_ln2
        mov     bx, di
        pop     di              ;restore di
        DBGBRK
        stc
        jmp     bx              ;not found (CF=1, AX=0)
parse_ln4:
        DBGBRK
        add     si, bx
        mov     ax, [di+bx]
        xor     bl, [di-1]      ;[fffff000]
        jz      arg__RELGOTO
        pop     di              ;restore di
        push    ax
        shr     bl, 1
        shr     bl, 1
        jmp     arg_table[bx-2]

arg__RELGOTO:
        xchg    bl, ah          ;relative address to di
        add     bx, di
        pop     di
        jmp     bx

@Opr_rel16:     ;rel.16
        call    getHexValue
        jc      @f
        sub     ax, actual_IP
        sub     ax, 3           ; jumps are 3 bytes
        stosw
        clc
@@:     ret

arg__STRSIZE:
        pop     bx              ;opcode
        lodsb                   ;B/W/D
        and     al, not 20h
        cmp     al, 'B'
        je      arg_put2
        inc     bx              ;bit0=1 for word/dword
        or      dl, RM__WORD
        cmp     al, 'W'
        je      arg_put2
        cmp     al, 'D'
        jne     arg_Error
        or      dh, PRE_OPR32
arg_put2:
        xchg    ax, bx
        stosb
        call    getSegOvr
        clc
        ret

arg__RELJMP1:
        pop     ax              ;opcode
        stosb
@Opr_rel8:      ;rel.8
        call    getHexValue
        jc      arg_Error
        sub     ax, actual_IP
        sub     ax, 2           ; short jumps are 2 bytes
        mov     bx, ax
        cbw
        xor     bx, ax
        je      arg_put1
arg_Error:
        stc
        ret

arg__RETVALU:
        pop     ax
arg__GOTOADR:
        ret

arg__WRDSIZE:
        mov     al, [si]
        and     al, not 20h
        cmp     al, 'D'
        jne     arg__NOPRND1
        inc     si
        or      dh, PRE_OPR32
arg__NOPRND1:
        pop     ax
arg_put1:
        stosb
        clc
        ret

arg__NOPRND2:
        pop     ax
        stosw
        ret

arg__OPTWORD:
        call    skipBlanks
        cmp     al, 0
        je      arg__NOPRND1
        pop     ax
        dec     ax
        stosb
        jmp     opr_Word

arg__MULTYPE:
        pop     bp
        mov     asmInputPtr, si
decode_Loop:
        mov     di, asmBufPtr
decode_L1:
        mov     al, [bp]
        inc     bp
        stosb
        mov     ah, 0
        mov     bx, ax
        shl     bx, 1
        mov     cx, opcode1[bx]
decode_L2:
        mov     bx, 0fch
        and     bl, ch
        jnz     decode_L3
        mov     al, [bp]
        inc     bp
        stosb
        mov     bl, 38h
        and     bl, al
        shr     bl, 1
        shr     bl, 1
        add     bx, cx
        mov     bl, byte ptr opcode2[bx+1]
        and     bx, 00fch
decode_L3:
        shr     bx, 1
        call    OprndTypeTable[bx]
        jc      decode_L4
        call    skipBlanks
        cmp     al, 0
        je      decode_L6
decode_L4:
        mov     si, asmInputPtr
        xor     dx, dx                  ; reset flags
        cmp     byte ptr [bp], 0f1h ; nop as end of list
        jne     decode_Loop
decode_L5:
        stc                     ; not found
decode_L6:
        ret

@Opr_0fp:         ;0F prefix
        mov     al, [bp]
        inc     bp
        stosb
        mov     bl, al
        mov     cl, 3
        shr     bl, cl
        mov     cl, al
        and     al, 7
        add     al, sysinst[bx]
        js      decode_L5               ;invalid
        mov     bl, al
        shl     bl, 1
        mov     cx, opcode_0f[bx]
        pop     bx                      ;remove ret from stack
        jmp     decode_L2

@Opr_none:        ;no operand inst's
@Opr_prefix:      ;Prefix instructions
@Opr_segovr:      ;seg: override
        clc
        ret

Dual_Operand_A0:
        mov     al, 0
        stosb
Dual_Operand:
        mov     asmRmPtr, di    ; passed r/m byte
        pop     bx
        lea     cx, 6[bx]
        push    cx
        push    bx
        call    [bx+0]
        pop     bx
        jc      Dual_Oprnd1
        mov     cx, [bx+4]
        call    [bx+2]          ; cx should be preserved here
        jc      Dual_Oprnd1
        call    skipBlanks
        cmp     al, ','
        jne     Bad_Operand
        inc     si
        call    cx
        jnc     Dual_OprRet
Dual_Oprnd1:
        pop     bx
Bad_Operand:
        stc
Dual_OprRet:
        ret

@Opr_in_nn:     ;in al/ax/eax,nn
        call    Dual_Operand
        dw      opr_Accu
        dw      just_ret
        dw      opr_Byte
        ret

@Opr_in_dx:     ;in al/ax/eax,dx
        call    Dual_Operand
        dw      opr_Accu
        dw      just_ret
        dw      opr_Dx
        ret

@Opr_out_nn:    ;out nn,al/ax/eax
        call    Dual_Operand
        dw      opr_Byte
        dw      just_ret
        dw      opr_Accu
just_ret:
        ret

@Opr_out_dx:    ;out dx,al/ax/eax
        call    Dual_Operand
        dw      opr_Dx
        dw      just_ret
        dw      opr_Accu
        ret

@Opr_addr_acc:  ;address,acc
        call    Dual_Operand
        dw      get_MemAddr
        dw      chk_direct
        dw      opr_Accu
        ret

@Opr_r16:       ;r16    .....rrr
        call    get_RegWordDword
        jnc     or_reg
        ret

or_wrrr:        ; 0000wrrr
        test    dl, RM__WORD
        jz      or_reg
        or      al, 8           ; w-bit
or_reg: or      [di-1], al
        ret

@Opr_seg:       ;seg    ...ss...
        call    get_segRegister1
        jnc     or_reg
        ret

@Opr_fs_gs:     ;push/pop fs/gs
        call    get_segRegister2
        jnc     or_reg
        ret

opr_Dx: call    parse_line
        CmdDef  'DX'   _RETVALU, 0000h
        db 0
        ret

Int_Opcode:
        mov     al, 0cdh
        stosb
        call    opr_Byte
        jc      intOp2
        cmp     al, 3
        jne     intOp1
        dec     di
        mov     byte ptr [di-1], 0cch
intOp1: clc
intOp2: ret

@Opr_nn:        ;nn
opr_Byte:
        call    getHexWord
        stosb
        adc     ah, 0ffh        ; cf if CF=1 or ah>0
        ret

@Opr_sx8:       ;+nn/-nn
opr_sx8:call    getHexWord
        jc      @f
        mov     cx, ax
        cbw
        xor     ah, ch
        neg     ah
        stosb
@@:     ret

opr_ByteWord:
        test    dl, RM__BYTE
        jnz     opr_Byte
@Opr_nnnnw:     ;nnnn (16/32) (push)
opr_WordDword:
        test    dh, PRE_OPR32
        jz      opr_Word
        ; get dword
        call    getHexValue
        stosw
        xchg    ax, cx
        stosw
        ret

@Opr_nnnn:      ;nnnn (16)
opr_Word:
        call    getHexWord
        stosw
        ret

arg__WRD_BYT:   ;
        pop     ax
        stosb
@Opr_nnnn_nn:   ;nnnn,nn (enter)
        call    Dual_Operand
        dw      opr_Word
        dw      just_ret
        dw      opr_Byte
        ret

@Opr_reg_imm:   ;reg,imm  ....wrrr imm 
        call    Dual_Operand
        dw      get_Register
        dw      or_wrrr         ; 0000wrrr
        dw      opr_ByteWord
        ret

@Opr_acc_imm:   ;acc,imm
        call    Dual_Operand
        dw      opr_Accu
        dw      just_ret
        dw      opr_ByteWord
        ret

@Opr_ax_r16:    ;ax,r16 .....rrr
        dec     byte ptr [di-1]         ; 91 used instead of 90 (=nop)
        call    chk_ax_r16
        jnc     @f
        call    Dual_Operand
        dw      get_RegWordDword
        dw      or_reg
        dw      opr_Accu
        cmp     al, 1
        je      @f
        stc
@@:     ret

chk_ax_r16:
        call    Dual_Operand
        dw      opr_Accu
        dw      chk_4word
        dw      get_RegWordDword
        jmp     or_reg

chk_4word:
        test    dl, RM__WORD
        jnz     @f
        stc
@@:     ret

@Opr_reg_rm_sx8:  ;reg,r/m,+n/-n (imul)
        call    Dual_Operand
        dw      @Opr_reg_rm
        dw      just_ret
        dw      opr_Byte
        ret

@Opr_reg_rm_nnnn: ;reg,r/m,nnnn  (imul)
        call    Dual_Operand
        dw      @Opr_reg_rm
        dw      just_ret
        dw      opr_WordDword
        ret

arg__R16_MEM:     ;
        pop     ax
        test    ah, ah
        jz      @f
        stosb
        xchg    al, ah
@@:     stosb
@Opr_r16_rm:    ;reg,r/m (word)
@Opr_lea_reg_rm:;lea reg,r/m
        or      dl, RM__WORD
@Opr_reg_rm:    ;reg,r/m (8/16)
        call    Dual_Operand_A0
        dw      get_Register
        dw      put_reg_w
        dw      get_RegOrMem
        ret
@Opr_rm_reg:    ;r/m,reg (8/16)
        call    Dual_Operand_A0
        dw      get_RegOrMem
        dw      just_ret
        dw      get_Register
put_reg_w:
        call    or_00rrr000
clr_w_bit:
        test    dl, RM__BYTE
        jz      @f
        mov     bx, asmRmPtr
        and     byte ptr [bx-2], not 1 ; w-bit
@@:     ret

@Opr_rm:        ;r/m    .......w mm...r/m (byte/word)
        call    get_RegOrMem
        jnc     clr_w_bit
        ret

arg__SHLRD__:   ; hh ll+w rm    SHLD  r,rm,cl/nn
        pop     ax              ; opcode for rmb,nn
        stosw
@Opr_rm_reg_sc: ;r/m,reg,cl (shld/shrd)
        or      dl, RM__WORD
        call    Dual_Operand
        dw      @Opr_rm_reg
        dw      just_ret
        dw      opr_SC
        neg     ah              ; cf if nn is the count
        mov     bx, asmRmPtr
        sbb     byte ptr [bx-2], 0
        ret

@Opr_acc_addr:  ;acc,address
        call    Dual_Operand
        dw      opr_Accu
        dw      just_ret
        dw      get_MemAddr
chk_direct:
        mov     bx, asmRmPtr
        xor     [bx-1], al
        xor     al, 6
        add     al, 0ffh                ; CF if not 6
        ret

@Opr_seg_rm:    ;seg,r/m
        call    Dual_Operand_A0
        dw      get_segRegisters
        dw      or_reg
        dw      get_RegOrMemWord
        ret

@Opr_rm_seg:    ;r/m,seg
        call    Dual_Operand_A0
        dw      get_RegOrMemWord
        dw      just_ret
        dw      get_segRegisters
        jmp     or_rmByte

@Opr_rmw_nn:    ;bit operations r/m16/32,imm8
        call    Dual_Operand
        dw      get_RegOrMem
        dw      chk_4word
        dw      opr_Byte
        ret

@Opr_rm_sx8:    ;r/m,+n/-n
        call    Dual_Operand
        dw      get_RegOrMem
        dw      chk_4word
        dw      opr_Byte
        ret

@Opr_r16_rm8:   ;r16/32,r/m8 (movsx/movzx)
        call    Dual_Operand_A0
        dw      get_RegWordDword
        dw      or_00rrr000_dl0
        dw      get_RegOrMemByte
        ret

or_00rrr000_c32:
        test    dh, PRE_OPR32
        jz      oprError1
or_00rrr000_dl0:
        mov     dl, 0
or_00rrr000:
        shl     al, 1
        shl     al, 1
        shl     al, 1
or_rmByte:
        mov     bx, asmRmPtr
        or      [bx-1], al
        ret

@Opr_r32_rm16:  ;r32,r/m16 (movsx/movzx)
        call    Dual_Operand_A0
        dw      get_RegWordDword
        dw      or_00rrr000_c32
        dw      get_RegOrMemWord
        ret

spc_reg:call    Dual_Operand
        dw      get_SpcialReg
        dw      or_spcreg
        dw      get_RegWordDword
spc_is32:
        and     dh, not PRE_OPR32
        jmp     or_reg

@Opr_0f_2x:     ;mov CRn/DRn/TRn,r/m
        mov     al, 0c0h
        stosb
        call    spc_reg
        jnc     spc_ret
        mov     di, asmRmPtr
        call    Dual_Operand
        dw      get_RegWordDword
        dw      spc_is32
        dw      get_SpcialReg
        xor     ah, al
or_spcreg:
        lodsb
        sub     al, '0'
        cmp     al, 8
        jae     oprError1
        or      [di-2], ah
        jmp     or_00rrr000

get_SpcialReg:
        call    parse_line
        CmdDef  'CR'   _RETVALU, 2202h
        CmdDef  'DR'   _RETVALU, 2302h
        CmdDef  'TR'   _RETVALU, 2602h
        db      0
@Opr_float:     ;ESC nn,r/m
@Opr_d_for_32:  ;push/pop f/a (fd/ad when 32)
@Opr_32ovr:     ;32 bit override (op32/ad32)
@Opr_sb_or_sw:  ;sw/sd in ins lods/scas/movs/cmps/stos
oprError1:
        stc
spc_ret:
r32Ret: ret


@Opr_seg_off:   ;seg:off
        call    getHexWord
        jc      oprError1
        xchg    bx, ax
        call    skipBlanks
        cmp     al, ':'
        jne     oprError1
        inc     si
        call    getHexValue
        stosw
        xchg    bx, ax
        stosw
        ret

arg__ROTATE_:     ; hh+i+c+w ll+rm        SHL  rm,1/cl/nn
        pop     ax              ; opcode for rmb,nn
        stosw
@Opr_rm_sc:     ;r/m,1/cl/nn
        call    Dual_Operand
        dw      get_RegOrMem
        dw      clr_w_bit
        dw      opr_SC
        test    ah, ah
        jz      rmscRet         ; if CL is count
        cmp     al, 1
        jne     @f
        mov     ah, 2
        dec     di
@@:     mov     bx, asmRmPtr
        xor     [bx-2], ah
rmscRet:ret

opr_SC: call    parse_line
        CmdDef  'CL'   _RETVALU, 0000h
        db 0
        call    opr_Byte
        mov     ah, 12h
        ret

; **-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**
; 
; **-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**-**

arg__0F_MEM_:     ; 0F hh ll+rmw
        mov     al, 0fh
        stosb
        pop     ax
        stosw
        jmp     short get_RegOrMemWord

arg__REGMEM_:   ; hh+w ll+rm            NOT
        pop     ax
        stosw
        jmp     short get_RegOrMem

@Opr_rm_imm:    ;r/m,imm  .......w r/m imm
        call    Dual_Operand
        dw      get_RegOrMem
        dw      clr_w_bit
        dw      opr_ByteWord
        ret

arg__RM8____:   ; hh ll 00+rmb          SETcc
        pop     ax
        stosw
@Opr_rmb:       ;r/m    ........ mmxxxr/m (setcc byte)
        mov     al, 0
        stosb
get_RegOrMemByte:
        or      dl, RM__BYTE
        jmp     short get_RegOrMem
@Opr_rmw:       ;r/m (16/32)
@Opr_mem24:     ;r/m (gdt/idt)
get_RegOrMemWord:
        or      dl, RM__WORD
get_RegOrMem:
        call    get_Register0
        jc      get_MemAddr
        or      al, 0c0h
        or      [di-1], al
        ret

arg__MEMADR_:   ; memory address (not registers)
        pop     ax
        stosw
@Opr_dword:     ; r/m (dword) (used in opcode2) (0=inst2)
get_MemAddr:
        ; In:   DI-1 => [mmxxxr/m]
        ;       DS:SI => x[r1+r2+y]
        xor     ax, ax
        mov     [di+0], ax              ; base address
        mov     [di+2], ax
getMem1:call    parse_line
        CmdDef  '['     _GOTOADR, getMem4
        CmdDef  'BYTE'  _RELGOTO, getMem2-$+5, RM__BYTE
        CmdDef  'WORD'  _RELGOTO, getMem2-$+5, RM__WORD
        CmdDef  'DWORD' _RELGOTO, getMem2-$+6, RM__DWRD
        db 0
getSegOvr:
        call    parse_line
        CmdDef  'CS:'   _RELGOTO, getMem3-$+4, PRE_CS
        CmdDef  'DS:'   _RELGOTO, getMem3-$+4, PRE_DS
        CmdDef  'ES:'   _RELGOTO, getMem3-$+4, PRE_ES
        CmdDef  'SS:'   _RELGOTO, getMem3-$+4, PRE_SS
        CmdDef  'FS:'   _RELGOTO, getMem3-$+4, PRE_FS
        CmdDef  'GS:'   _RELGOTO, getMem3-$+4, PRE_GS
        db 0
getMemErr:
        stc
        ret

getMem2:or      dl, al
        xor     al, RM__DWRD + RM__WORD + RM__BYTE
        and     al, dl
        jnz     getMemErr
        call    parse_line
        CmdDef  'PTR'   _GOTOADR, getMem1
        db 0
        jmp     getMem1

getMem3:test    dh, PRE_SMASK
        jnz     getMemErr
        or      dh, al
        jmp     getMem1

getMem4:or      dl, RM__ADDR
getMem5:call    get_RegAddress
        jc      getMem6
        test    dl, RM__REGS
        jnz     getMemErr
        or      dl, RM__REGS
        or      [di-1], al
        jmp     short getMem7
getMem6:call    getHexValue
        jc      getMemErr
        add     [di+0], ax
        adc     [di+2], cx
        or      dl, RM__BASE
getMem7:call    skipBlanks
        cmp     al, '-'
        je      getMem6
        inc     si
        cmp     al, '+'
        je      getMem5
        cmp     al, ']'
        jne     getMemErr
        mov     cx, [di+2]
        jcxz    gmStdAdr
        mov     ax, [di+0]
        shl     ah, 1
        adc     cx, 0
        jnz     getMemErr               ; 32-bit displacement
gmStdAdr:
        mov     al, 06h                 ; assume direct address
        test    dl, RM__REGS
        jz      gmDirect
        mov     ax, [di+0]
        cbw
        cmp     ax, [di+0]
        jne     gmDisp16
        xchg    cx, ax
        mov     al, [di-1]
        and     al, 7
        xor     al, 6                   ; [bp] => [bp+0]
        jz      short_displ
        jcxz    do_w_bit
short_displ:
        ; 8-bit displacement
        mov     al, 40h
        inc     di
        or      [di-2], al
do_w_bit:
        ret
gmDisp16:       ; 16-bit displacement
        mov     al, 80h
gmDirect:       ; direct address
        scasw
        xor     [di-3], al
        ret

get_RegWordDword:
        call    parse_line      ;see RM__xxxx
        CmdDef  'EAX'   _RELGOTO, opr_Exx-$+4, 00h
        CmdDef  'ECX'   _RELGOTO, opr_Exx-$+4, 01h
        CmdDef  'EDX'   _RELGOTO, opr_Exx-$+4, 02h
        CmdDef  'EBX'   _RELGOTO, opr_Exx-$+4, 03h
        CmdDef  'ESP'   _RELGOTO, opr_Exx-$+4, 04h
        CmdDef  'EBP'   _RELGOTO, opr_Exx-$+4, 05h
        CmdDef  'ESI'   _RELGOTO, opr_Exx-$+4, 06h
        CmdDef  'EDI'   _RELGOTO, opr_Exx-$+4, 07h
        db 0
get_RegWord:
        call    parse_line      ;see RM__xxxx
        CmdDef  'AX'    _RELGOTO, opr_Wx-$+3, 00h
        CmdDef  'CX'    _RELGOTO, opr_Wx-$+3, 01h
        CmdDef  'DX'    _RELGOTO, opr_Wx-$+3, 02h
        CmdDef  'BX'    _RELGOTO, opr_Wx-$+3, 03h
        CmdDef  'SP'    _RELGOTO, opr_Wx-$+3, 04h
        CmdDef  'BP'    _RELGOTO, opr_Wx-$+3, 05h
        CmdDef  'SI'    _RELGOTO, opr_Wx-$+3, 06h
        CmdDef  'DI'    _RELGOTO, opr_Wx-$+3, 07h
        db 0
        ret

get_Register0:
        mov     asmRmPtr, di
get_Register:
        call    get_RegWordDword
        jnc     regByteRet
get_RegByte:
        call    parse_line
        CmdDef  'AL'   _RELGOTO, opr_Bx-$+3, 0
        CmdDef  'CL'   _RELGOTO, opr_Bx-$+3, 1
        CmdDef  'DL'   _RELGOTO, opr_Bx-$+3, 2
        CmdDef  'BL'   _RELGOTO, opr_Bx-$+3, 3
        CmdDef  'AH'   _RELGOTO, opr_Bx-$+3, 4
        CmdDef  'CH'   _RELGOTO, opr_Bx-$+3, 5
        CmdDef  'DH'   _RELGOTO, opr_Bx-$+3, 6
        CmdDef  'BH'   _RELGOTO, opr_Bx-$+3, 7
        db 0
regByteRet:
        ret

opr_Accu:
        call    parse_line
        CmdDef  'AL'   _RELGOTO, opr_Bx-$+3, 0
        CmdDef  'AX'   _RELGOTO, opr_Wx-$+3, 1
        CmdDef  'EAX'  _RELGOTO, opr_Exx-$+4, 1
        db 0
        ret
opr_Exx:or      dh, PRE_OPR32
opr_Wx: or      dl, RM__WORD
        ret
opr_Bx: mov     bx, asmRmPtr
        and     byte ptr [bx-1], not 1 ; w-bit
        or      dl, RM__BYTE
        ret


get_segRegister1:
        call    parse_line
        CmdDef  'ES'    _RETVALU, 0*8
        CmdDef  'CS'    _RETVALU, 1*8
        CmdDef  'SS'    _RETVALU, 2*8
        CmdDef  'DS'    _RETVALU, 3*8
        db 0
        ret

get_segRegisters:
        call    get_segRegister1
        jnc     get_segRet
get_segRegister2:
        call    parse_line
        CmdDef  'FS'    _RETVALU, 4*8
        CmdDef  'GS'    _RETVALU, 5*8
        db 0
get_segRet:
        ret

Opcode_Movzx:   db 00fh, 0b6h   ; _r16_rm8
                db 00fh, 0b7h   ; _r32_rm16
                db 0f1h
Opcode_Movsx:   db 00fh, 0beh   ; _r16_rm8
                db 00fh, 0bfh   ; _r32_rm16
                db 0f1h
Opcode_Mov:     db 0a1h         ; _acc_addr (b/w)
                db 0a3h         ; _addr_acc (b/w)
                db 08bh         ; _reg_rm   (b/w)
                db 08eh         ; _seg_rm
                db 0b0h         ; _reg_imm  (b/w)
                db 0c7h, 000h   ; _rm_imm   (b/w)
                db 089h         ; _rm_reg   (b/w)
                db 08ch         ; _rm_seg
                db 00fh, 020h   ; _0f_2x    (CRn/DRn/TRn)
                db 0f1h
Opcode_Push:    db 050h         ; _r16
                db 006h         ; _seg
                db 00fh, 0a0h   ; _fs_gs
                db 06ah         ; _sx8
                db 068h         ; _nnnnw
                db 0ffh, 30h    ; _rmw
                db 0f1h
Opcode_Pop:     db 058h         ; _r16
                db 007h         ; _seg
                db 00fh, 0a1h   ; _fs_gs
                db 08fh, 00h    ; _rmw
                db 0f1h
Opcode_Inc:     db 040h         ; _r16
                db 0ffh, 000h   ; _rm
                db 0f1h
Opcode_Dec:     db 048h         ; _r16
                db 0ffh, 008h   ; _rm
                db 0f1h
Opcode_Xchg:    db 091h         ; _ax_r16
                db 087h         ; _rm_reg
                db 0f1h
Opcode_Add:     db 003h         ; _reg_rm
                db 001h         ; _rm_reg
                db 083h, 000h   ; _rm_sx8
                db 005h         ; _acc_imm
                db 081h, 000h   ; _rm_imm
                db 0f1h
Opcode_Or:      db 00bh         ; _reg_rm
                db 009h         ; _rm_reg
                db 083h, 008h   ; _rm_sx8
                db 00dh         ; _acc_imm
                db 081h, 008h   ; _rm_imm
                db 0f1h
Opcode_Adc:     db 013h         ; _reg_rm
                db 011h         ; _rm_reg
                db 083h, 010h   ; _rm_sx8
                db 015h         ; _acc_imm
                db 081h, 010h   ; _rm_imm
                db 0f1h
Opcode_Sbb:     db 01bh         ; _reg_rm
                db 019h         ; _rm_reg
                db 083h, 018h   ; _rm_sx8
                db 01dh         ; _acc_imm
                db 081h, 018h   ; _rm_imm
                db 0f1h
Opcode_And:     db 023h         ; _reg_rm
                db 021h         ; _rm_reg
                db 083h, 020h   ; _rm_sx8
                db 025h         ; _acc_imm
                db 081h, 020h   ; _rm_imm
                db 0f1h
Opcode_Sub:     db 02bh         ; _reg_rm
                db 029h         ; _rm_reg
                db 083h, 028h   ; _rm_sx8
                db 02dh         ; _acc_imm
                db 081h, 028h   ; _rm_imm
                db 0f1h
Opcode_Xor:     db 033h         ; _reg_rm
                db 031h         ; _rm_reg
                db 083h, 030h   ; _rm_sx8
                db 035h         ; _acc_imm
                db 081h, 030h   ; _rm_imm
                db 0f1h
Opcode_Cmp:     db 03bh         ; _reg_rm
                db 039h         ; _rm_reg
                db 083h, 038h   ; _rm_sx8
                db 03dh         ; _acc_imm
                db 081h, 038h   ; _rm_imm
                db 0f1h
Opcode_Test:    db 085h         ; _rm_reg
                db 0a9h         ; _acc_imm
                db 0f7h, 000h   ; _rm_imm
                db 0f1h
Opcode_Jmp:     db 0ffh, 020h   ; _rmw
                db 0ffh, 028h   ; _dword
                db 0eah         ; _seg_off
                db 0ebh         ; _rel8
                db 0e9h         ; _rel16
                db 0f1h
Opcode_Call:    db 0ffh, 010h   ; _rmw
                db 0ffh, 018h   ; _dword
                db 09ah         ; _seg_off
                db 0e8h         ; _rel16
                db 0f1h
Opcode_Imul:    db 0f7h, 028h   ; _rm
                db 069h         ; _reg_rm_nnnn
                db 06bh         ; _reg_rm_sx8
                db 00fh, 0afh   ; _reg_rm
                db 0f1h
Opcode_Mul:     db 0f7h, 020h   ; _rm
                db 0f1h
Opcode_Idiv:    db 0f7h, 038h   ; _rm
                db 0f1h
Opcode_Div:     db 0f7h, 030h   ; _rm
                db 0f1h
Opcode_In:      db 0edh             ; _out_dx
                db 0e5h             ; _out_nn
                db 0f1h
Opcode_Out:     db 0efh             ; _out_dx
                db 0e7h             ; _out_nn
                db 0f1h
Opcode_Bts:     db 00fh, 0abh       ; _rm_reg
                db 00fh, 0bah, 028h ; _rmw_nn
                db 0f1h
Opcode_Btr:     db 00fh, 0b3h       ; _rm_reg
                db 00fh, 0bah, 030h ; _rmw_nn
                db 0f1h
Opcode_Btc:     db 00fh, 0bbh       ; _rm_reg
                db 00fh, 0bah, 038h ; _rmw_nn
                db 0f1h
Opcode_Bt:      db 00fh, 0a3h       ; _rm_reg
                db 00fh, 0bah, 020h ; _rmw_nn
                db 0f1h

PUBPROC Assemble
        ;In:    DS:SI=ptr instruction
        ;       BP=IP for first byte of instruction
        ;Out:   CF=0: asmBuffer = opcodes
        ;             ES:DI=ptr end of asmBuffer
        ;       CF=1: error
        mov     actual_IP, bp
        push    cs
        pop     es
        mov     di, offset asmBuffer
        call    get_Opcode
        jc      asmbly1
        call    skipBlanks
        cmp     al, 0
        jne     asmbly1
        test    dl, RM__BYTE
        jz      asmbly3
        test    dl, RM__WORD
        jz      asmbly3                 ;error if both byte & word set
asmbly1:stc
asmbly2:ret
asmbly3:test    dh, PRE_OPR32
        mov     al, 66h                 ;PRE_OPR32
        call    Insert_Prefix
        test    dh, PRE_ADR32
        mov     al, 67h                 ;PRE_ADR32
        call    Insert_Prefix
        mov     bx, PRE_SMASK
        and     bl, dh
        mov     al, seg_Prefix[bx-1]
Insert_Prefix:
        jz      asmbly2
        mov     si, asmBufPtr
        inc     di
InsPre1:xchg    al, [si]
        inc     si
        cmp     si, di
        jb      InsPre1
        ret

insName label word
X0      equ $
_invl:  db 1,'?'

arg__PREFINS:
        pop     ax
        stosb
get_Opcode:
        xor     dx, dx                  ; reset flags
        mov     asmBufPtr, di           ; start of next instruction
        call    parse_line
_movzx: CmdDef  'MOVZX'    _MULTYPE, Opcode_Movzx
_movsx: CmdDef  'MOVSX'    _MULTYPE, Opcode_Movsx
_movs:  CmdDef  'MOVS'     _STRSIZE, 0A5A4h
_mov:   CmdDef  'MOV'      _MULTYPE, Opcode_Mov
_pushf: CmdDef  'PUSHF'    _WRDSIZE, 9C9Ch
_popf:  CmdDef  'POPF'     _WRDSIZE, 9D9Dh
_pusha: CmdDef  'PUSHA'    _WRDSIZE, 6060h
_popa:  CmdDef  'POPA'     _WRDSIZE, 6161h
_push:  CmdDef  'PUSH'     _MULTYPE, Opcode_Push
_pop:   CmdDef  'POP'      _MULTYPE, Opcode_Pop
_inc:   CmdDef  'INC'      _MULTYPE, Opcode_Inc
_dec:   CmdDef  'DEC'      _MULTYPE, Opcode_Dec
_lods:  CmdDef  'LODS'     _STRSIZE, 0ADACh
_stos:  CmdDef  'STOS'     _STRSIZE, 0ABAAh
_scas:  CmdDef  'SCAS'     _STRSIZE, 0AFAEh
_cmps:  CmdDef  'CMPS'     _STRSIZE, 0A7A6h
_repe:  CmdDef  'REPE'     _PREFINS, 00F3h
_repne: CmdDef  'REPNE'    _PREFINS, 00F2h
_xchg:  CmdDef  'XCHG'     _MULTYPE, Opcode_Xchg
_and:   CmdDef  'AND'      _MULTYPE, Opcode_And
_test:  CmdDef  'TEST'     _MULTYPE, Opcode_Test
_xor:   CmdDef  'XOR'      _MULTYPE, Opcode_Xor
_or:    CmdDef  'OR'       _MULTYPE, Opcode_Or
_add:   CmdDef  'ADD'      _MULTYPE, Opcode_Add
_adc:   CmdDef  'ADC'      _MULTYPE, Opcode_Adc
_sub:   CmdDef  'SUB'      _MULTYPE, Opcode_Sub
_sbb:   CmdDef  'SBB'      _MULTYPE, Opcode_Sbb
_cmp:   CmdDef  'CMP'      _MULTYPE, Opcode_Cmp
_leave: CmdDef  'LEAVE'    _NOPRND1, 00C9h
_lea:   CmdDef  'LEA'      _R16_MEM, 008dh
_lds:   CmdDef  'LDS'      _R16_MEM, 00c5h
_les:   CmdDef  'LES'      _R16_MEM, 00c4h
_retf:  CmdDef  'RETF'     _OPTWORD, 0CACBh
_ret:   CmdDef  'RET'      _OPTWORD, 0C2C3h
_iret:  CmdDef  'IRET'     _WRDSIZE, 00CFh
_clc:   CmdDef  'CLC'      _NOPRND1, 00F8h
_cmc:   CmdDef  'CMC'      _NOPRND1, 00F5h
_stc:   CmdDef  'STC'      _NOPRND1, 00F9h
_cld:   CmdDef  'CLD'      _NOPRND1, 00FCh
_std:   CmdDef  'STD'      _NOPRND1, 00FDh
_cli:   CmdDef  'CLI'      _NOPRND1, 00FAh
_sti:   CmdDef  'STI'      _NOPRND1, 00FBh
_jcxz:  CmdDef  'JCXZ'     _RELJMP1, 00e3h
_jo:    CmdDef  'JO'       _RELJMP1, 0070h
_jno:   CmdDef  'JNO'      _RELJMP1, 0071h
_jc:    CmdDef  'JC'       _RELJMP1, 0072h
_jnc:   CmdDef  'JNC'      _RELJMP1, 0073h
_je:    CmdDef  'JE'       _RELJMP1, 0074h
_jne:   CmdDef  'JNE'      _RELJMP1, 0075h
_ja:    CmdDef  'JA'       _RELJMP1, 0077h
_jbe:   CmdDef  'JBE'      _RELJMP1, 0076h
_js:    CmdDef  'JS'       _RELJMP1, 0078h
_jns:   CmdDef  'JNS'      _RELJMP1, 0079h
_jp:    CmdDef  'JP'       _RELJMP1, 007ah
_jnp:   CmdDef  'JNP'      _RELJMP1, 007bh
_jge:   CmdDef  'JGE'      _RELJMP1, 007dh
_jg:    CmdDef  'JG'       _RELJMP1, 007fh
_jle:   CmdDef  'JLE'      _RELJMP1, 007eh
_jl:    CmdDef  'JL'       _RELJMP1, 007ch
_loopn: CmdDef  'LOOPNZ'   _RELJMP1, 00e0h
_loope: CmdDef  'LOOPZ'    _RELJMP1, 00e1h
_loop:  CmdDef  'LOOP'     _RELJMP1, 00e2h
_jmp:   CmdDef  'JMP'      _MULTYPE, Opcode_Jmp
_call:  CmdDef  'CALL'     _MULTYPE, Opcode_Call
_imul:  CmdDef  'IMUL'     _MULTYPE, Opcode_Imul
_mul:   CmdDef  'MUL'      _MULTYPE, Opcode_Mul
_idiv:  CmdDef  'IDIV'     _MULTYPE, Opcode_Idiv
_div:   CmdDef  'DIV'      _MULTYPE, Opcode_Div
_int3:  CmdDef  'INT3'     _NOPRND1, 00CCh
_into:  CmdDef  'INTO'     _NOPRND1, 00CEh
_int:   CmdDef  'INT'      _GOTOADR, Int_Opcode
_nop:   CmdDef  'NOP'      _NOPRND1, 0090h
_xlat:  CmdDef  'XLAT'     _NOPRND1, 00D7h
_lahf:  CmdDef  'LAHF'     _NOPRND1, 009Fh
_sahf:  CmdDef  'SAHF'     _NOPRND1, 009Eh
_aaa:   CmdDef  'AAA'      _NOPRND1, 0037h
_daa:   CmdDef  'DAA'      _NOPRND1, 0027h
_aas:   CmdDef  'AAS'      _NOPRND1, 003Fh
_das:   CmdDef  'DAS'      _NOPRND1, 002Fh
_cbw:   CmdDef  'CBW'      _NOPRND1, 0098h
        CmdDef  'CWDE'     _NOPRND2, 9866h
_cwd:   CmdDef  'CWD'      _NOPRND1, 0099h
_ins:   CmdDef  'INS'      _STRSIZE, 6D6Ch
_outs:  CmdDef  'OUTS'     _STRSIZE, 6F6Eh
_hlt:   CmdDef  'HLT'      _NOPRND1, 00F4h
_wait:  CmdDef  'WAIT'     _NOPRND1, 009Bh
_lock:  CmdDef  'LOCK'     _PREFINS, 00F0h
_aam:   CmdDef  'AAM'      _NOPRND2, 0AD4h
_aad:   CmdDef  'AAD'      _NOPRND2, 0AD5h
_in:    CmdDef  'IN'       _MULTYPE, Opcode_In
_out:   CmdDef  'OUT'      _MULTYPE, Opcode_Out
;---------------------------------------- 286 instructions -------------------
_enter: CmdDef  'ENTER'    _WRD_BYT, 00C8h
_bound: CmdDef  'BOUND'    _R16_MEM, 0062h
_arpl:  CmdDef  'ARPL'     _R16_MEM, 0063h
;---------------------------------------- 286 prefix (0fh) -------------------
_clts:  CmdDef  'CLTS'     _NOPRND2, 060Fh
_sldt:  CmdDef  'SLDT'     _0F_MEM_, 0000h
_str:   CmdDef  'STR'      _0F_MEM_, 0800h
_lldt:  CmdDef  'LLDT'     _0F_MEM_, 1000h
_ltr:   CmdDef  'LTR'      _0F_MEM_, 1800h
_verr:  CmdDef  'VERR'     _0F_MEM_, 2000h
_verw:  CmdDef  'VERW'     _0F_MEM_, 2800h
_sgdt:  CmdDef  'SGDT'     _0F_MEM_, 0001h
_sidt:  CmdDef  'SIDT'     _0F_MEM_, 0801h
_lgdt:  CmdDef  'LGDT'     _0F_MEM_, 1001h
_lidt:  CmdDef  'LIDT'     _0F_MEM_, 1801h
_smsw:  CmdDef  'SMSW'     _0F_MEM_, 2001h
_lmsw:  CmdDef  'LMSW'     _0F_MEM_, 3001h
_lar:   CmdDef  'LAR'      _R16_MEM, 020Fh
_lsl:   CmdDef  'LSL'      _R16_MEM, 030Fh
_ldall: CmdDef  'LOADALL'  _NOPRND2, 050fh
_cpuid: CmdDef  'CPUID'    _NOPRND2, 0a20fh
;---------------------------------------- 386 prefix (0fh) -------------------
_bts:   CmdDef  'BTS'      _MULTYPE, Opcode_Bts
_btr:   CmdDef  'BTR'      _MULTYPE, Opcode_Btr
_btc:   CmdDef  'BTC'      _MULTYPE, Opcode_Btc
_bt:    CmdDef  'BT'       _MULTYPE, Opcode_Bt
_bsf:   CmdDef  'BSF'      _R16_MEM, 0BC0Fh
_bsr:   CmdDef  'BSR'      _R16_MEM, 0BD0Fh
_shld:  CmdDef  'SHLD'     _SHLRD__, 0A50Fh
_shrd:  CmdDef  'SHRD'     _SHLRD__, 0AD0Fh
_lss:   CmdDef  'LSS'      _R16_MEM, 0B20Fh
_lfs:   CmdDef  'LFS'      _R16_MEM, 0B40Fh
_lgs:   CmdDef  'LGS'      _R16_MEM, 0B50Fh
_seto:  CmdDef  'SETO'     _RM8____, 900Fh
_setno: CmdDef  'SETNO'    _RM8____, 910Fh
_setc:  CmdDef  'SETC'     _RM8____, 920Fh
_setnc: CmdDef  'SETNC'    _RM8____, 930Fh
_sete:  CmdDef  'SETE'     _RM8____, 940Fh
_setne: CmdDef  'SETNE'    _RM8____, 950Fh
_seta:  CmdDef  'SETBE'    _RM8____, 960Fh
_setbe: CmdDef  'SETA'     _RM8____, 970Fh
_sets:  CmdDef  'SETS'     _RM8____, 980Fh
_setns: CmdDef  'SETNS'    _RM8____, 990Fh
_setp:  CmdDef  'SETP'     _RM8____, 9A0Fh
_setnp: CmdDef  'SETNP'    _RM8____, 9B0Fh
_setl:  CmdDef  'SETL'     _RM8____, 9C0Fh
_setge: CmdDef  'SETGE'    _RM8____, 9D0Fh
_setle: CmdDef  'SETLE'    _RM8____, 9E0Fh
_setg:  CmdDef  'SETG'     _RM8____, 9F0Fh

;;** instructions after this line are not used in opcode1

_shl:   CmdDef  'SHL'      _ROTATE_, 020D3h
_shr:   CmdDef  'SHR'      _ROTATE_, 028D3h
_rol:   CmdDef  'ROL'      _ROTATE_, 000D3h
_ror:   CmdDef  'ROR'      _ROTATE_, 008D3h
_rcl:   CmdDef  'RCL'      _ROTATE_, 010D3h
_rcr:   CmdDef  'RCR'      _ROTATE_, 018D3h
_sar:   CmdDef  'SAR'      _ROTATE_, 038D3h
_not:   CmdDef  'NOT'      _REGMEM_, 010F7h
_neg:   CmdDef  'NEG'      _REGMEM_, 018F7h

_ss:    CmdDef  'SS:'      _PREFINS, 0036h
_cs:    CmdDef  'CS:'      _PREFINS, 002Eh
_ds:    CmdDef  'DS:'      _PREFINS, 003Eh
_es:    CmdDef  'ES:'      _PREFINS, 0026h
_gs:    CmdDef  'GS:'      _PREFINS, 0065h
_fs:    CmdDef  'FS:'      _PREFINS, 0064h
_esc:   CmdDef  'F'        _GOTOADR, X87_Opcodes

;if ($ - X0) gt 400h
;.err Instruction text too long
;endif

;** Alternative names used by Assembler

        CmdDef  'CDQ'      _NOPRND2, 9966h
        CmdDef  'REPZ'     _PREFINS, 00F3h
        CmdDef  'REPNZ'    _PREFINS, 00F2h
        CmdDef  'REP'      _PREFINS, 00F3h

        db 0
        ret             ; return with CF=1

X87_Opcodes:
         call    parse_here ; do not skip blanks
;---------------------------------------- 287 instructions -------------------
_fld1:   CmdDef  'LD1'      _NOPRND2, 0E8D9h
_fldl2t: CmdDef  'LDL2T'    _NOPRND2, 0E9D9h
_fldl2e: CmdDef  'LDL2E'    _NOPRND2, 0EAD9h
_fldpi:  CmdDef  'LDPI'     _NOPRND2, 0EBD9h
_fldlg2: CmdDef  'LDLG2'    _NOPRND2, 0ECD9h
_fldln2: CmdDef  'LDLN2'    _NOPRND2, 0EDD9h
_fldz:   CmdDef  'LDZ'      _NOPRND2, 0EED9h
_fnop:   CmdDef  'NOP'      _NOPRND2, 0D0D9h
_fchs:   CmdDef  'CHS'      _NOPRND2, 0E0D9h
_fabs:   CmdDef  'ABS'      _NOPRND2, 0E1D9h
_ftst:   CmdDef  'TST'      _NOPRND2, 0E4D9h
_fxam:   CmdDef  'XAM'      _NOPRND2, 0E5D9h
_fprem1: CmdDef  'PREM1'    _NOPRND2, 0F5D9h
_fprem:  CmdDef  'PREM'     _NOPRND2, 0F8D9h
_fyl2xp1:CmdDef  'YL2XP1'   _NOPRND2, 0F9D9h
_fyl2x:  CmdDef  'YL2X'     _NOPRND2, 0F1D9h
_f2xm1:  CmdDef  '2XM1'     _NOPRND2, 0F0D9h
_fptan:  CmdDef  'PTAN'     _NOPRND2, 0F2D9h
_fpatan: CmdDef  'PATAN'    _NOPRND2, 0F3D9h
_fsqrt:  CmdDef  'SQRT'     _NOPRND2, 0FAD9h
_frndint:CmdDef  'RNDINT'   _NOPRND2, 0FCD9h
_fscale: CmdDef  'SCALE'    _NOPRND2, 0FDD9h
_fsincos:CmdDef  'SINCOS'   _NOPRND2, 0FBD9h
_fsin:   CmdDef  'SIN'      _NOPRND2, 0FED9h
_fcos:   CmdDef  'COS'      _NOPRND2, 0FFD9h
_fxtract:CmdDef  'XTRACT'   _NOPRND2, 0F4D9h
_feni:   CmdDef  'ENI'      _NOPRND2, 0E0DBh
_fdisi:  CmdDef  'DISI'     _NOPRND2, 0E1DBh
_fclex:  CmdDef  'CLEX'     _NOPRND2, 0E2DBh
_finit:  CmdDef  'INIT'     _NOPRND2, 0E3DBh
_fsetpm: CmdDef  'SETPM'    _NOPRND2, 0E4DBh
_fdecstp:CmdDef  'DECSTP'   _NOPRND2, 0F6D9h
_fincstp:CmdDef  'INCSTP'   _NOPRND2, 0F7D9h
_fldenv: CmdDef  'LDENV'    _MEMADR_, 020D9h
_fldcw:  CmdDef  'LDCW'     _MEMADR_, 028D9h
_fstenv: CmdDef  'STENV'    _MEMADR_, 030D9h
_fstcw:  CmdDef  'STCW'     _MEMADR_, 038D9h
_frstor: CmdDef  'RSTOR'    _MEMADR_, 020DDh
_fsave:  CmdDef  'SAVE'     _MEMADR_, 030DDh
_fstsw:  CmdDef  'STSW'     _MEMADR_, 038DDh
_fadd:   CmdDef  'ADD'      _RELGOTO, x87_Check4P-$+4, 11000100b
_fmul:   CmdDef  'MUL'      _RELGOTO, x87_Check4P-$+4, 11001100b
_fsubr:  CmdDef  'SUBR'     _RELGOTO, x87_Check4P-$+5, 11101100b
_fsub:   CmdDef  'SUB'      _RELGOTO, x87_Check4P-$+4, 11100100b
_fcomp:  CmdDef  'COMP'     _RELGOTO, x87_Check4P-$+5, 11011100b
_fcom:   CmdDef  'COM'      _RELGOTO, x87_Check4P-$+4, 11010100b
_fdivr:  CmdDef  'DIVR'     _RELGOTO, x87_Check4P-$+5, 11111100b
_fdiv:   CmdDef  'DIV'      _RELGOTO, x87_Check4P-$+4, 11110100b
_fstp:   CmdDef  'STP'      _RELGOTO, x87_MemAddr-$+4, 11011101b
_fst:    CmdDef  'ST'       _RELGOTO, x87_MemAddr-$+3, 11010101b
_fld:    CmdDef  'LD'       _RELGOTO, x87_MemAddr-$+3, 11000001b
_fucomp: CmdDef  'UCOMP'    _RELGOTO, x87_ST_n-$+6,    11101101b
_fucom:  CmdDef  'UCOM'     _RELGOTO, x87_ST_n-$+5,    11100101b
_fxch:   CmdDef  'XCH'      _RELGOTO, x87_ST_n-$+4,    11001001b
_ffree:  CmdDef  'FREE'     _RELGOTO, x87_ST_n-$+5,    11000101b
         db 0
         ret
_f???:   db 1,'?'

x87_Check4P:    ; x87 memory types
        mov     ah, not 20h
        and     ah, [si]
        cmp     ah, 'P'
        jne     x87_Mem1
        inc     si
        ; it can only be fxxxp st(n)
        or      al, 6                   ; DE group
x87_ST_n:       ; x87 register
        mov     ah, al
        and     ax, 0f807h
        or      al, 0d8h
        stosw
get_x87Register:
        call    parse_line
        CmdDef  'ST'   _GOTOADR, opr_STi
        db 0
        ret
opr_STi:mov     al, [si]
        sub     al, '0'
        cmp     al, 8
        jae     @f
        inc     si
        or      [di-1], al
        ret
@@:     xor     al, al
x87_MemDone:
        ret

x87_Mem1:
        or      al, 4                   ; assume fxxx STi DC
        call    x87_ST_n
        jc      x87_Mem2
        cmp     al, 0
        jne     x87_MemDone
        call    skipBlanks
        xor     al, ','
        jnz     x87_MemDone
        ; its ST,STi    D8 C0+
        and     byte ptr [di-2], not 4
        inc     si
        jmp     get_x87Register

x87_MemAddr:    ; x87 memory types
        call    x87_ST_n
        jnc     x87_MemDone
        call    parse_line 
_R80:   CmdDef  'R80'   _RELGOTO, x87_Mem4-$+4, 2bh
_I64:   CmdDef  'I64'   _RELGOTO, x87_Mem4-$+4, 2fh
_B80:   CmdDef  'B80'   _RELGOTO, x87_Mem4-$+4, 27h
        db 0
x87_Mem2:
        call    parse_line 
_R32:   CmdDef  'R32'   _RELGOTO, x87_Mem3-$+4, 0d8h
_I32:   CmdDef  'DWORD' _RELGOTO, x87_Mem3-$+6, 0dah
_R64:   CmdDef  'R64'   _RELGOTO, x87_Mem3-$+4, 0dch
_I16:   CmdDef  'WORD'  _RELGOTO, x87_Mem3-$+5, 0deh
        db 0
        mov     al, 0deh                ; WORD by default
x87_Mem3:
        and     word ptr [di-2], 3801h
        xor     [di-2], ax
        jmp     get_MemAddr

x87_Mem4:
        and     word ptr [di-2], 1000h
        mov     ah, al
        and     ax, 3807h
        or      al, 0d8h
        jmp     x87_Mem3

_byte:  db 'BYTE '
_word   equ _I16+1

get_RegAddress:
        call    parse_line
adrmod0:CmdDef  'BX+SI' _RETVALU, 0000h
        CmdDef  'SI+BX' _RETVALU, 0000h
adrmod1:CmdDef  'BX+DI' _RETVALU, 0001h
        CmdDef  'DI+BX' _RETVALU, 0001h
adrmod2:CmdDef  'BP+SI' _RETVALU, 0002h
        CmdDef  'SI+BP' _RETVALU, 0002h
adrmod3:CmdDef  'BP+DI' _RETVALU, 0003h
        CmdDef  'DI+BP' _RETVALU, 0003h
adrmod4:CmdDef  'SI'    _RETVALU, 0004h
adrmod5:CmdDef  'DI'    _RETVALU, 0005h
adrmod6:CmdDef  'BP'    _RETVALU, 0006h
adrmod7:CmdDef  'BX'    _RETVALU, 0007h
        db 0
        ret

        even
modtbl  dw adrmod0, adrmod1, adrmod2, adrmod3
        dw adrmod4, adrmod5, adrmod6, adrmod7

I_cnt   = 0

I_type  macro   t
        t = I_cnt
        I_cnt = I_cnt + 1
        dw offset _&t
        dw offset _a&t
        endm

TypeTable label word
 I_type _dword       ;r/m (dword) (used in opcode2) (0=inst2)
 I_type _none        ;no operand inst's
 I_type _prefix      ;Prefix instructions
 I_type _segovr      ;seg: override
 I_type _r16         ;r16    .....rrr
 I_type _seg         ;seg    ...ss...
 I_type _ax_r16      ;ax,r16 .....rrr
 I_type _reg_rm      ;reg,r/m (8/16)
 I_type _rm_reg      ;r/m,reg (8/16)
 I_type _seg_rm      ;seg,r/m
 I_type _rm_seg      ;r/m,seg
 I_type _acc_addr    ;acc,address
 I_type _addr_acc    ;address,acc
 I_type _acc_imm     ;acc,imm
 I_type _reg_imm     ;reg,imm  ....wrrr imm 
 I_type _r16_rm      ;reg,r/m (word)
 I_type _nn          ;nn
 I_type _nnnn        ;nnnn (16)
 I_type _nnnnw       ;nnnn (16/32) (push)
 I_type _sx8         ;+nn/-nn
 I_type _nnnn_nn     ;nnnn,nn (enter)
 I_type _reg_rm_sx8  ;reg,r/m,+n/-n (imul)
 I_type _reg_rm_nnnn ;reg,r/m,nnnn  (imul)
 I_type _rm          ;r/m    .......w mm...r/m (byte/word)
 I_type _rmb         ;r/m    ........ mmxxxr/m (setcc byte)
 I_type _rmw         ;r/m (16/32)
 I_type _rmw_nn      ;bit operations r/m16/32,imm8
 I_type _rm_imm      ;r/m,imm  .......w r/m imm
 I_type _rm_sx8      ;r/m,+n/-n
 I_type _rm_sc       ;r/m,1/cl/nn
 I_type _mem24       ;r/m (gdt/idt)
 I_type _r16_rm8     ;r16/32,r/m8 (movsx/movzx)
 I_type _r32_rm16    ;r32,r/m16 (movsx/movzx)
 I_type _lea_reg_rm  ;lea reg,r/m
 I_type _rm_reg_sc   ;r/m,reg,cl/nn (shld/shrd)
 I_type _rel8        ;rel.8
 I_type _rel16       ;rel.16
 I_type _seg_off     ;seg:off
 I_type _0fp         ;0F prefix
 I_type _32ovr       ;32 bit override (op32/ad32)
 I_type _float       ;ESC nn,r/m
 I_type _fs_gs       ;push/pop fs/gs
 I_type _0f_2x       ;mov CRn/DRn/TRn,r/m
 I_type _in_nn       ;in al/ax/eax,nn
 I_type _in_dx       ;in al/ax/eax,dx
 I_type _out_nn      ;out nn,al/ax/eax
 I_type _out_dx      ;out dx,al/ax/eax
 I_type _sb_or_sw    ;sw/sd in ins lods/scas/movs/cmps/stos
 I_type _d_for_32    ;push/pop f/a (fd/ad when 32)

K_cnt   = 0

K_type  macro   t
        &t    = K_cnt
        K_cnt = K_cnt + 2
        dw offset @_&t
        endm

StepType label word
 K_type @S_Pf           ;prefix
 K_type @S_Ad           ;32-bit address prefix
 K_type @S_Op           ;32-bit operand prefix
 K_type @S_0f           ;0f-xx
 K_type @S_ff           ;ff-rm
 K_type @S_Tr           ;Trace it
 K_type @S_S1           ;Step 1 inst
 K_type @S_S2           ;Step 2 inst
 K_type @S_L3           ;call rel16/32
 K_type @S_M2           ;xx-rm
 K_type @S_Sf           ;ssss:oooo

inst1   macro   a,t,x
        if      (a - X0) GT 3FFh
        .ERR Address &a out off range.
        endif
        dw      a - X0 + 400h * t
        endm

inst2   macro   a
        dw      a - opcode2
        endm

opcode2 equ this word
_grp80  equ this word
_grp81  equ this word
_grp82  equ this word
 inst1 _add   _rm_imm           ;add r/m,nn
 inst1 _or    _rm_imm           ;or  r/m,nn
 inst1 _adc   _rm_imm           ;adc r/m,nn
 inst1 _sbb   _rm_imm           ;sbb r/m,nn
 inst1 _and   _rm_imm           ;and r/m,nn
 inst1 _sub   _rm_imm           ;sub r/m,nn
 inst1 _xor   _rm_imm           ;xor r/m,nn
 inst1 _cmp   _rm_imm           ;cmp r/m,nn
_grp83  equ this word
 inst1 _add   _rm_sx8           ;add r/m,+nn/-nn
 inst1 _or    _rm_sx8           ;or  r/m,+nn/-nn
 inst1 _adc   _rm_sx8           ;adc r/m,+nn/-nn
 inst1 _sbb   _rm_sx8           ;sbb r/m,+nn/-nn
 inst1 _and   _rm_sx8           ;and r/m,+nn/-nn
 inst1 _sub   _rm_sx8           ;sub r/m,+nn/-nn
 inst1 _xor   _rm_sx8           ;xor r/m,+nn/-nn
 inst1 _cmp   _rm_sx8           ;cmp r/m,+nn/-nn
_grp8f  equ this word
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
 inst1 _pop   _rmw              ;pop r/m
_grpc0  equ this word
_grpc1  equ this word
_grpd0  equ this word
_grpd1  equ this word
_grpd2  equ this word
_grpd3  equ this word
 inst1 _rol   _rm_sc            ;rol r/m,1/cl/nn
 inst1 _ror   _rm_sc            ;ror r/m,1/cl/nn
 inst1 _rcl   _rm_sc            ;rcl r/m,1/cl/nn
 inst1 _rcr   _rm_sc            ;rcr r/m,1/cl/nn
 inst1 _shl   _rm_sc            ;shl r/m,1/cl/nn
 inst1 _shr   _rm_sc            ;shr r/m,1/cl/nn
 inst1 _shl   _rm_sc            ;sal r/m,1/cl/nn
 inst1 _sar   _rm_sc            ;sar r/m,1/cl/nn
_grpc6  equ this word
_grpc7  equ this word
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
 inst1 _mov   _rm_imm           ;mov r/m,nn/nnnn
_grpf6  equ this word
_grpf7  equ this word
 inst1 _test  _rm_imm           ;test r/m,nn
 inst1 _invl  _none             ;?
 inst1 _not   _rm               ;not r/m
 inst1 _neg   _rm               ;neg r/m
 inst1 _mul   _rm               ;mul r/m
 inst1 _imul  _rm               ;imul r/m
 inst1 _div   _rm               ;div r/m
 inst1 _idiv  _rm               ;idiv r/m
_grpfe  equ this word
 inst1 _inc   _rm               ;inc r/m
 inst1 _dec   _rm               ;dec r/m
 inst1 _invl  _none             ;?
 inst1 _invl  _none             ;?
_gr0fba equ this word
 inst1 _invl  _none             ;
 inst1 _invl  _none             ;
 inst1 _invl  _none             ;
 inst1 _invl  _none             ;
 inst1 _bt    _rmw_nn           ;
 inst1 _bts   _rmw_nn           ;
 inst1 _btr   _rmw_nn           ;
 inst1 _btc   _rmw_nn           ;
_grpff  equ this word
 inst1 _inc   _rm               ;inc r/m
 inst1 _dec   _rm               ;dec r/m
 inst1 _call  _rmw      stepm2  ;call r/m
 inst1 _call  _dword    stepm2  ;call dword ptr r/m
 inst1 _jmp   _rmw      trace   ;jmp  r/m
 inst1 _jmp   _dword    trace   ;jmp  dword ptr r/m
 inst1 _push  _rmw              ;push r/m
 inst1 _invl  _none             ;?
_gr0f00 equ this word
 inst1 _sldt  _rmw              ;sldt r/m
 inst1 _str   _rmw              ;str  r/m
 inst1 _lldt  _rmw      stepm2  ;lldt r/m
 inst1 _ltr   _rmw      stepm2  ;ltr  r/m
 inst1 _verr  _rmw              ;verr r/m
 inst1 _verw  _rmw              ;verw r/m
 inst1 _invl  _none             ;
 inst1 _invl  _none             ;
_gr0f01 equ this word
 inst1 _sgdt  _mem24            ;sgdt r/m
 inst1 _sidt  _mem24            ;sidt r/m
 inst1 _lgdt  _mem24    trace   ;lgdt r/m
 inst1 _lidt  _mem24    trace   ;lidt r/m
 inst1 _smsw  _rmw              ;smsw r/m
 inst1 _invl  _none             ;
 inst1 _lmsw  _rmw      stepm2  ;lmsw r/m
 inst1 _invl  _none             ;

opcode1 equ this word
 inst1 _add   _rm_reg           ;00;add r/m,reg byte
 inst1 _add   _rm_reg              ;add r/m,reg word
 inst1 _add   _reg_rm              ;add reg,r/m byte
 inst1 _add   _reg_rm              ;add reg,r/m word
 inst1 _add   _acc_imm             ;add al,imm
 inst1 _add   _acc_imm             ;add ax,imm
 inst1 _push  _seg                 ;push es
 inst1 _pop   _seg                 ;pop es
 inst1 _or    _rm_reg           ;08;or  r/m,reg byte
 inst1 _or    _rm_reg              ;or  r/m,reg word
 inst1 _or    _reg_rm              ;or  reg,r/m byte
 inst1 _or    _reg_rm              ;or  reg,r/m word
 inst1 _or    _acc_imm             ;or  al,imm
 inst1 _or    _acc_imm             ;or  ax,imm
 inst1 _push  _seg                 ;push cs
 inst1 _invl  _0fp      0f         ;SYSTEM 286
 inst1 _adc   _rm_reg           ;10;adc r/m,reg byte
 inst1 _adc   _rm_reg              ;adc r/m,reg word
 inst1 _adc   _reg_rm              ;adc reg,r/m byte
 inst1 _adc   _reg_rm              ;adc reg,r/m word
 inst1 _adc   _acc_imm             ;adc al,imm
 inst1 _adc   _acc_imm             ;adc ax,imm
 inst1 _push  _seg                 ;push ss
 inst1 _pop   _seg                 ;pop ss
 inst1 _sbb   _rm_reg           ;18;sbb r/m,reg byte
 inst1 _sbb   _rm_reg              ;sbb r/m,reg word
 inst1 _sbb   _reg_rm              ;sbb reg,r/m byte
 inst1 _sbb   _reg_rm              ;sbb reg,r/m word
 inst1 _sbb   _acc_imm             ;sbb al,imm
 inst1 _sbb   _acc_imm             ;sbb ax,imm
 inst1 _push  _seg                 ;push ds
 inst1 _pop   _seg                 ;pop ds
 inst1 _and   _rm_reg           ;20;and r/m,reg byte
 inst1 _and   _rm_reg              ;and r/m,reg word
 inst1 _and   _reg_rm              ;and reg,r/m byte
 inst1 _and   _reg_rm              ;and reg,r/m word
 inst1 _and   _acc_imm             ;and al,imm
 inst1 _and   _acc_imm             ;and ax,imm
 inst1 _es    _segovr   prefix     ;es:
 inst1 _daa   _none                ;daa
 inst1 _sub   _rm_reg           ;28;sub r/m,reg byte
 inst1 _sub   _rm_reg              ;sub r/m,reg word
 inst1 _sub   _reg_rm              ;sub reg,r/m byte
 inst1 _sub   _reg_rm              ;sub reg,r/m word
 inst1 _sub   _acc_imm             ;sub al,imm
 inst1 _sub   _acc_imm             ;sub ax,imm
 inst1 _cs    _segovr   prefix     ;cs:
 inst1 _das   _none                ;das
 inst1 _xor   _rm_reg           ;30;xor r/m,reg byte
 inst1 _xor   _rm_reg              ;xor r/m,reg word
 inst1 _xor   _reg_rm              ;xor reg,r/m byte
 inst1 _xor   _reg_rm              ;xor reg,r/m word
 inst1 _xor   _acc_imm             ;xor al,imm
 inst1 _xor   _acc_imm             ;xor ax,imm
 inst1 _ss    _segovr   prefix     ;ss:
 inst1 _aaa   _none                ;aaa
 inst1 _cmp   _rm_reg           ;38;cmp r/m,reg byte
 inst1 _cmp   _rm_reg              ;cmp r/m,reg word
 inst1 _cmp   _reg_rm              ;cmp reg,r/m byte
 inst1 _cmp   _reg_rm              ;cmp reg,r/m word
 inst1 _cmp   _acc_imm             ;cmp al,imm
 inst1 _cmp   _acc_imm             ;cmp ax,imm
 inst1 _ds    _segovr   prefix     ;ds:
 inst1 _aas   _none                ;aas
 inst1 _inc   _r16              ;40;inc ax
 inst1 _inc   _r16                 ;inc cx
 inst1 _inc   _r16                 ;inc dx
 inst1 _inc   _r16                 ;inc bx
 inst1 _inc   _r16                 ;inc sp
 inst1 _inc   _r16                 ;inc bp
 inst1 _inc   _r16                 ;inc si
 inst1 _inc   _r16                 ;inc di
 inst1 _dec   _r16              ;48;dec ax
 inst1 _dec   _r16                 ;dec cx
 inst1 _dec   _r16                 ;dec dx
 inst1 _dec   _r16                 ;dec bx
 inst1 _dec   _r16                 ;dec sp
 inst1 _dec   _r16                 ;dec bp
 inst1 _dec   _r16                 ;dec si
 inst1 _dec   _r16                 ;dec di
 inst1 _push  _r16              ;50;push ax
 inst1 _push  _r16                 ;push cx
 inst1 _push  _r16                 ;push dx
 inst1 _push  _r16                 ;push bx
 inst1 _push  _r16                 ;push sp
 inst1 _push  _r16                 ;push bp
 inst1 _push  _r16                 ;push si
 inst1 _push  _r16                 ;push di
 inst1 _pop   _r16              ;58;pop ax
 inst1 _pop   _r16                 ;pop cx
 inst1 _pop   _r16                 ;pop dx
 inst1 _pop   _r16                 ;pop bx
 inst1 _pop   _r16                 ;pop sp
 inst1 _pop   _r16                 ;pop bp
 inst1 _pop   _r16                 ;pop si
 inst1 _pop   _r16                 ;pop di
 inst1 _pusha _d_for_32         ;60;pusha
 inst1 _popa  _d_for_32            ;popa
 inst1 _bound _r16_rm              ;bound
 inst1 _arpl  _rm_reg   stepm1     ;arpl
 inst1 _fs    _segovr   prefix     ;fs:
 inst1 _gs    _segovr   prefix     ;gs:
 inst1 _invl  _32ovr    opr32      ;operand size
 inst1 _invl  _32ovr    adr32      ;address size
 inst1 _push  _nnnnw            ;68;push nnnn (16/32)
 inst1 _imul  _reg_rm_nnnn         ;imul reg,r/m,nnnn
 inst1 _push  _sx8                 ;push +n/-n
 inst1 _imul  _reg_rm_sx8          ;imul reg,r/m,+n/-n
 inst1 _ins   _sb_or_sw step1      ;insb
 inst1 _ins   _sb_or_sw step1      ;insw/d
 inst1 _outs  _sb_or_sw step1      ;outsb
 inst1 _outs  _sb_or_sw step1      ;outsw/d
 inst1 _jo    _rel8     trace   ;70;jo  label
 inst1 _jno   _rel8     trace      ;jno label
 inst1 _jc    _rel8     trace      ;jc  label
 inst1 _jnc   _rel8     trace      ;jnc label
 inst1 _je    _rel8     trace      ;je  label
 inst1 _jne   _rel8     trace      ;jne label
 inst1 _jbe   _rel8     trace      ;jbe label
 inst1 _ja    _rel8     trace      ;ja  label
 inst1 _js    _rel8     trace   ;78;jo  label
 inst1 _jns   _rel8     trace      ;jno label
 inst1 _jp    _rel8     trace      ;jc  label
 inst1 _jnp   _rel8     trace      ;jnc label
 inst1 _jl    _rel8     trace      ;je  label
 inst1 _jge   _rel8     trace      ;jne label
 inst1 _jle   _rel8     trace      ;jbe label
 inst1 _jg    _rel8     trace      ;ja  label
 inst2 _grp80                   ;80;
 inst2 _grp81
 inst2 _grp82
 inst2 _grp83
 inst1 _test  _rm_reg              ;test r/m,reg byte
 inst1 _test  _rm_reg              ;test r/m,reg word
 inst1 _xchg  _rm_reg              ;xchg r/m,reg byte
 inst1 _xchg  _rm_reg              ;xchg r/m,reg word
 inst1 _mov   _rm_reg           ;88;mov  r/m,reg byte
 inst1 _mov   _rm_reg              ;mov  r/m,reg word
 inst1 _mov   _reg_rm              ;mov  reg,r/m byte
 inst1 _mov   _reg_rm              ;mov  reg,r/m word
 inst1 _mov   _rm_seg              ;mov  r/m,seg
 inst1 _lea   _lea_reg_rm          ;lea  reg,r/m
 inst1 _mov   _seg_rm              ;mov  seg,r/m
 inst2 _grp8f
 inst1 _nop   _none             ;90;nop
 inst1 _xchg  _ax_r16              ;xchg cx,ax
 inst1 _xchg  _ax_r16              ;xchg dx,ax
 inst1 _xchg  _ax_r16              ;xchg bx,ax
 inst1 _xchg  _ax_r16              ;xchg sp,ax
 inst1 _xchg  _ax_r16              ;xchg bp,ax
 inst1 _xchg  _ax_r16              ;xchg si,ax
 inst1 _xchg  _ax_r16              ;xchg di,ax
 inst1 _cbw   _d_for_32         ;98;cbw
 inst1 _cwd   _d_for_32            ;cwd
 inst1 _call  _seg_off  stepf      ;call seg:off
 inst1 _wait  _none                ;wait
 inst1 _pushf _d_for_32 step1      ;pushf
 inst1 _popf  _d_for_32 step1      ;popf
 inst1 _sahf  _none                ;sahf
 inst1 _lahf  _none                ;lahf
 inst1 _mov   _acc_addr         ;a0;mov al,address
 inst1 _mov   _acc_addr            ;mov ax,address
 inst1 _mov   _addr_acc            ;mov address,al
 inst1 _mov   _addr_acc            ;mov address,ax
 inst1 _movs  _sb_or_sw step1      ;movsb
 inst1 _movs  _sb_or_sw step1      ;movsw
 inst1 _cmps  _sb_or_sw step1      ;cmpsb
 inst1 _cmps  _sb_or_sw step1      ;cmpsw
 inst1 _test  _acc_imm          ;a8;test al,nn
 inst1 _test  _acc_imm             ;test ax,nnnn
 inst1 _stos  _sb_or_sw step1      ;stosb
 inst1 _stos  _sb_or_sw step1      ;stosw
 inst1 _lods  _sb_or_sw step1      ;lodsb
 inst1 _lods  _sb_or_sw step1      ;lodsw
 inst1 _scas  _sb_or_sw step1      ;scasb
 inst1 _scas  _sb_or_sw step1      ;scasw
 inst1 _mov   _reg_imm          ;b0;mov al,nn
 inst1 _mov   _reg_imm             ;mov cl,nn
 inst1 _mov   _reg_imm             ;mov dl,nn
 inst1 _mov   _reg_imm             ;mov bl,nn
 inst1 _mov   _reg_imm             ;mov ah,nn
 inst1 _mov   _reg_imm             ;mov ch,nn
 inst1 _mov   _reg_imm             ;mov dh,nn
 inst1 _mov   _reg_imm             ;mov bh,nn
 inst1 _mov   _reg_imm          ;b8;mov ax,nnnn
 inst1 _mov   _reg_imm             ;mov cx,nnnn
 inst1 _mov   _reg_imm             ;mov dx,nnnn
 inst1 _mov   _reg_imm             ;mov bx,nnnn
 inst1 _mov   _reg_imm             ;mov sp,nnnn
 inst1 _mov   _reg_imm             ;mov bp,nnnn
 inst1 _mov   _reg_imm             ;mov si,nnnn
 inst1 _mov   _reg_imm             ;mov di,nnnn
 inst2 _grpc0                   ;c0;
 inst2 _grpc1
 inst1 _ret   _nnnn     trace      ;ret nnnn (16)
 inst1 _ret   _none     trace      ;ret
 inst1 _les   _r16_rm              ;les reg,r/m
 inst1 _lds   _r16_rm              ;lds reg,r/m
 inst2 _grpc6
 inst2 _grpc7
 inst1 _enter _nnnn_nn          ;c8;enter nnnn,nn
 inst1 _leave _none                ;leave
 inst1 _retf  _nnnn     trace      ;retf nnnn (16)
 inst1 _retf  _none     trace      ;retf
 inst1 _int3  _none     step1      ;int 3
 inst1 _int   _nn       step2      ;int nn
 inst1 _into  _none     step1      ;into
 inst1 _iret  _none     trace      ;iret
 inst2 _grpd0                   ;d0;
 inst2 _grpd1
 inst2 _grpd2
 inst2 _grpd3
 inst1 _aam   _nn                  ;aam
 inst1 _aad   _nn                  ;aad
 inst1 _invl  _none                ;?
 inst1 _xlat  _none                ;xlat
 inst1 _esc   _float            ;d8;ESC
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _esc   _float
 inst1 _loopn _rel8     step2   ;e0;loopne label
 inst1 _loope _rel8     step2      ;loope  label
 inst1 _loop  _rel8     step2      ;loop   label
 inst1 _jcxz  _rel8     trace      ;jcxz   label
 inst1 _in    _in_nn    step2      ;inb port
 inst1 _in    _in_nn    step2      ;inw port
 inst1 _out   _out_nn   step2      ;outb port
 inst1 _out   _out_nn   step2      ;outw port
 inst1 _call  _rel16    step3a  ;e8;call label
 inst1 _jmp   _rel16    trace      ;jmp  rel.16
 inst1 _jmp   _seg_off  trace      ;jmp  seg:off
 inst1 _jmp   _rel8     trace      ;jmp  rel.8
 inst1 _in    _in_dx    step1      ;inb dx
 inst1 _in    _in_dx    step1      ;inw dx
 inst1 _out   _out_dx   step1      ;outb dx
 inst1 _out   _out_dx   step1      ;outw dx
 inst1 _lock  _prefix   prefix  ;f0;lock
 inst1 _invl  _none                ;?
 inst1 _repne _prefix   prefix     ;repne
 inst1 _repe  _prefix   prefix     ;repe
 inst1 _hlt   _none     step1      ;hlt
 inst1 _cmc   _none                ;cmc
 inst2 _grpf6
 inst2 _grpf7
 inst1 _clc   _none             ;f8;clc
 inst1 _stc   _none                ;stc
 inst1 _cli   _none     step1      ;cli
 inst1 _sti   _none     step1      ;sti
 inst1 _cld   _none                ;cld
 inst1 _std   _none                ;std
 inst2 _grpfe
 inst2 _grpff

opcode_0f equ this word
op_0f00 equ ($-opcode_0f)/2
 inst2 _gr0f00                 ;0f00;
 inst2 _gr0f01
 inst1 _lar   _r16_rm   stepm1      ;lar reg,r/m
 inst1 _lsl   _r16_rm   stepm1      ;lsl reg,r/m
 inst1 _invl  _none                 ;
 inst1 _ldall _none                 ;loadall (undocumented, loads from abs 800h)
 inst1 _clts  _none     step1       ;clts
 inst1 _ldall _none                 ;loadalld
op_0f20 equ ($-opcode_0f)/2
 inst1 _mov   _0f_2x           ;0f20;mov r/m,CRn
 inst1 _mov   _0f_2x                ;mov r/m,DRn
 inst1 _mov   _0f_2x    trace       ;mov CRn,r/m
 inst1 _mov   _0f_2x                ;mov DRn,r/m
 inst1 _mov   _0f_2x                ;mov r/m,TRn
 inst1 _invl  _none                 ;
 inst1 _mov   _0f_2x                ;mov TRn,r/m
 inst1 _invl  _none                 ;
op_0f80 equ ($-opcode_0f)/2
 inst1 _jo    _rel16    trace  ;0f80;jo  label
 inst1 _jno   _rel16    trace       ;jno label
 inst1 _jc    _rel16    trace       ;jc  label
 inst1 _jnc   _rel16    trace       ;jnc label
 inst1 _je    _rel16    trace       ;je  label
 inst1 _jne   _rel16    trace       ;jne label
 inst1 _jbe   _rel16    trace       ;jbe label
 inst1 _ja    _rel16    trace       ;ja  label
op_0f88 equ ($-opcode_0f)/2
 inst1 _js    _rel16    trace  ;0f88;jo  label
 inst1 _jns   _rel16    trace       ;jno label
 inst1 _jp    _rel16    trace       ;jc  label
 inst1 _jnp   _rel16    trace       ;jnc label
 inst1 _jl    _rel16    trace       ;je  label
 inst1 _jge   _rel16    trace       ;jne label
 inst1 _jle   _rel16    trace       ;jbe label
 inst1 _jg    _rel16    trace       ;ja  label

op_0f90 equ ($-opcode_0f)/2
 inst1 _seto  _rmb             ;0f90;seto  r/m8
 inst1 _setno _rmb                  ;setno r/m8
 inst1 _setc  _rmb                  ;setc  r/m8
 inst1 _setnc _rmb                  ;setnc r/m8
 inst1 _sete  _rmb                  ;sete  r/m8
 inst1 _setne _rmb                  ;setne r/m8
 inst1 _setbe _rmb                  ;setbe r/m8
 inst1 _seta  _rmb                  ;seta  r/m8
op_0f98 equ ($-opcode_0f)/2
 inst1 _sets  _rmb             ;0f98;seto  r/m8
 inst1 _setns _rmb                  ;setno r/m8
 inst1 _setp  _rmb                  ;setc  r/m8
 inst1 _setnp _rmb                  ;setnc r/m8
 inst1 _setl  _rmb                  ;sete  r/m8
 inst1 _setge _rmb                  ;setne r/m8
 inst1 _setle _rmb                  ;setbe r/m8
 inst1 _setg  _rmb                  ;seta  r/m8
op_0fa0 equ ($-opcode_0f)/2
 inst1 _push  _fs_gs           ;0fa0;push fs
 inst1 _pop   _fs_gs                ;pop fs
 inst1 _cpuid _none                 ;cpuid (486+, EAX=cmd)
 inst1 _bt    _rm_reg               ;bt r/m16,reg16 (16/32)
 inst1 _shld  _rm_reg_sc            ;shld r/m,reg,imm
 inst1 _shld  _rm_reg_sc            ;shld r/m,reg,cl
 inst1 _invl  _none
 inst1 _invl  _none
op_0fa8 equ ($-opcode_0f)/2
 inst1 _push  _fs_gs             ;a8;push gs
 inst1 _pop   _fs_gs                ;pop gs
 inst1 _invl  _none
 inst1 _bts   _rm_reg               ;bts
 inst1 _shrd  _rm_reg_sc            ;shrd r/m,reg,imm
 inst1 _shrd  _rm_reg_sc            ;shrd r/m,reg,cl
 inst1 _invl  _none
 inst1 _imul  _reg_rm               ;imul
op_0fb0 equ ($-opcode_0f)/2
 inst1 _invl  _none            ;0fb0;
 inst1 _invl  _none
 inst1 _lss   _r16_rm               ;lss reg,r/m
 inst1 _btr   _rm_reg               ;bt
 inst1 _lfs   _r16_rm               ;lfs reg,r/m
 inst1 _lgs   _r16_rm               ;lgs reg,r/m
 inst1 _movzx _r16_rm8              ;r16/32,r/m8
 inst1 _movzx _r32_rm16             ;r32,r/m16
op_0fb8 equ ($-opcode_0f)/2
 inst1 _invl  _none            ;0fb8;
 inst1 _invl  _none
 inst2 _gr0fba
 inst1 _btc   _r16_rm               ;btc r16,r/m
 inst1 _bsf   _r16_rm               ;bsf r16,r/m
 inst1 _bsr   _r16_rm               ;bsr r16,r/m
 inst1 _movsx _r16_rm8              ;r16/32,r/m8
 inst1 _movsx _r32_rm16             ;r32,r/m16

_ndp_gr3 label word
_d9d0   dw _fnop            ;fnop
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
_d9e0   dw _fchs            ;fchs
        dw _fabs            ;fabs
        dw _f???        
        dw _f???        
        dw _ftst            ;ftst
        dw _fxam            ;fxam
        dw _f???        
        dw _f???        
_d9e8   dw _fld1            ;fld1
        dw _fldl2t          ;fldl2t
        dw _fldl2e          ;fldl2e
        dw _fldpi           ;fldpi
        dw _fldlg2          ;fldlg2
        dw _fldln2          ;fldln2
        dw _fldz            ;fldz
        dw _f???        
_d9f0   dw _f2xm1           ;f2xm1
        dw _fyl2x           ;fyl2x
        dw _fptan           ;fptan
        dw _fpatan          ;fpatan
        dw _fxtract         ;fxtract
        dw _fprem1          ;fprem1
        dw _fdecstp         ;fdecstp
        dw _fincstp         ;fincstp
_d9f8   dw _fprem           ;fprem
        dw _fyl2xp1         ;fyl2xp1
        dw _fsqrt           ;fsqrt
        dw _fsincos         ;fsincos
        dw _frndint         ;frndint
        dw _fscale          ;fscale
        dw _fsin            ;fsin
        dw _fcos            ;fcos
_dbe0   dw _feni            ;feni
        dw _fdisi           ;fdisi
        dw _fclex           ;fclex
        dw _finit           ;finit
        dw _fsetpm          ;fsetpm
        dw _f???        
        dw _f???        
        dw _f???        
_dfe0   dw _fstsw           ;fstsw ax
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        
        dw _f???        

F_cnt   = 1                     ; 0 reserved for group2
F_type  macro   x
        _t_&x = F_cnt
        F_cnt = F_cnt + 1
        dw offset _f_&x
        endm

F_inst1 macro   x, y
        IF      (_&x - X0) GT 0fffh
        .ERR Address _&x off range.
        ENDIF
        dw      (_&x - X0) + _t_&y*1000h
        endm

F_inst2 macro   x
        IF      (_&x - _ndp_gr3) GT 0fffh
        .ERR Address _&x off range.
        ENDIF
        dw      (_&x - _ndp_gr3)
        endm

F_inst  macro   x, y
        IFB     <y>
        F_inst2 x
        ELSE
        F_inst1 x, y
        ENDIF
        endm

        ;     0     2     4     6
ndptype dw  _R32, _I32, _R64, _I16, _R80, _I64, _B80

_ndp_type label word
        F_type  none
        F_type  R32
        F_type  I32
        F_type  R64
        F_type  I16
        F_type  R80
        F_type  I64
        F_type  B80
        F_type  STi
        F_type  MEM

F_insts macro   i1, i2, i3, i4
        F_inst  i1
        F_inst  i2
        F_inst  i3
        F_inst  i4
        endm


_ndp_gr1 label word
;           ==>   ----D9-----   ---DB----   ----DD-----   ----DF----
        F_insts  <fld    R32 > <fld  I32 > <fld    R64 > <fld   I16 >
        F_insts  <f???   none> <f??? none> <f???   none> <f???  none>        
        F_insts  <fst    R32 > <fst  I32 > <fst    R64 > <fst   I16 >
        F_insts  <fstp   R32 > <fstp I32 > <fstp   R64 > <fstp  I16 >
        F_insts  <fldenv MEM > <f??? none> <frstor MEM > <fld   B80 >
        F_insts  <fldcw  MEM > <fld  R80 > <f???   none> <fld   I64 >
        F_insts  <fstenv MEM > <f??? none> <fsave  MEM > <fstp  B80 >
        F_insts  <fstcw  MEM > <fstp R80 > <fstsw  MEM > <fstp  I64 >
_ndp_gr2 label word
        F_insts  <fld    STi > <f??? none> <ffree  STi > <ffree STi >
        F_insts  <fxch   STi > <f??? none> <fxch   STi > <fxch  STi >
        F_insts  <d9d0       > <f??? none> <fst    STi > <fst   STi >
        F_insts  <fstp   STi > <f??? none> <fstp   STi > <fstp  STi >
        F_insts  <d9e0       > <dbe0     > <fucom  STi > <dfe0      >
        F_insts  <d9e8       > <f??? none> <fucomp STi > <f???  none>
        F_insts  <d9f0       > <f??? none> <f???   none> <f???  none>
        F_insts  <d9f8       > <f??? none> <f???   none> <f???  none>

_ndpgr1 dw _fadd           ;00;fadd
        dw _fmul              ;fmul
        dw _fcom              ;fcom
        dw _fcomp             ;fcomp
        dw _fsub           ;04;fsub
        dw _fsubr             ;fsubr
        dw _fdiv              ;fdiv
        dw _fdivr             ;fdivr

regnm8  db  'ALCLDLBLAHCHDHBH'
regnm16 db  'AXCXDXBXSPBPSIDI'
segnam  db  'ESCSSSDSFSGS????'

Step_Groups     label byte
        ;   0f01   0f00  
Step_0f db  @S_Tr, @S_Tr 
        db  @S_Tr, @S_Tr 
        db  @S_M2, @S_M2
        db  @S_M2, @S_M2
        db  @S_Tr, @S_Tr 
        db  @S_Tr, @S_Tr 
        db  @S_M2; @S_Tr 
;       db  @S_Tr, @S_Tr 
SG_20   equ ($ - Step_Groups)
SG_28   equ ($ - Step_Groups)
SG_30   equ ($ - Step_Groups)
SG_38   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Tr; @S_Pf, @S_Tr
SG_F0   equ ($ - Step_Groups)
        db @S_Pf, @S_Tr, @S_Pf, @S_Pf, @S_S1; @S_Tr, @S_Tr, @S_Tr
SG_08   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_0f

SG_E8   equ ($ - Step_Groups)
        db @S_L3, @S_Tr, @S_Tr, @S_Tr, @S_S1, @S_S1, @S_S1, @S_S1
SG_E0   equ ($ - Step_Groups)
        db @S_S2, @S_S2, @S_S2, @S_Tr, @S_S2, @S_S2, @S_S2, @S_S2

Step_ff db @S_Tr, @S_Tr, @S_M2, @S_M2; @S_Tr, @S_Tr, @S_Tr, @S_Tr
SG_C8   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_S1, @S_S2, @S_S1, @S_Tr
SG_68   equ ($ - Step_Groups)
SG_A0   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr;;@S_Tr, @S_Tr, @S_S1, @S_S1, @S_S1, @S_S1
SG_A8   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_S1, @S_S1, @S_S1, @S_S1, @S_S1, @S_S1
SG_98   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_Sf, @S_Tr, @S_S1, @S_S1; @S_Tr, @S_Tr
SG_60   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_Tr, @S_Tr, @S_Pf, @S_Pf, @S_Op, @S_Ad
SG_F8   equ ($ - Step_Groups)
        db @S_Tr, @S_Tr, @S_S1, @S_S1, @S_Tr, @S_Tr, @S_Tr, @S_ff

StepMap db -1, SG_08, -1,    -1, SG_20, SG_28, SG_30, SG_38
        db -1,    -1, -1,    -1, SG_60, SG_68,    -1,    -1
        db -1,    -1, -1, SG_98, SG_A0, SG_A8,    -1,    -1
        db -1, SG_C8, -1,    -1, SG_E0, SG_E8, SG_F0, SG_F8

;input: ES:SI=address of the first byte of instruction
;       DS=CS
;output C=0    use trace
;       C=1    Use step: ES:SI=ptr end of inst

stepIns:mov     cx, 3                   ;ch=override flags (16-bit code)
        xor     ah, ah
@_@S_Pf:lods    byte ptr es:[si]
        mov     bx, ax
        shr     bl, cl
        mov     bl, StepMap[bx]
        inc     bl
        jz      @_@S_Tr
        and     al, 7
        add     bx, ax
        mov     bl, Step_Groups[bx-1]
        jmp     StepType[bx]

@_@S_Tr:clc                     ;use trace
        ret

@_@S_0f:;       286/386 prefix 0fh
        lods    byte ptr es:[si]
        cmp     al, 1
        ja      short @_@S_Tr
        ; only 0f00 and 0f01
        lods    byte ptr es:[si]
        ; al should be preserved for rm_bytes
        mov     bl, al
        rcl     bl, 1
        and     bl, 0fh
        mov     bl, Step_0f[bx]
        jmp     StepType[bx]

@_@S_ff:lods    byte ptr es:[si]
        ; al should be preserved for rm_bytes
        mov     bl, 7*8
        and     bl, al
        shr     bl, cl
        mov     bl, Step_ff[bx]
        jmp     StepType[bx]

@_@S_Op:xor     ch, OPR32
        jmp     short @_@S_Pf

@_@S_Ad:xor     ch, ADR32
        jmp     short @_@S_Pf

@_@S_M2:call    rm_bytes
        stc                     ;use step
        ret
@_@S_Sf:inc     si
        inc     si
@_@S_L3:inc     si
        test    ch, ADR32
        jz      short @_@S_S2
        inc     si
        inc     si
@_@S_S2:inc     si
@_@S_S1:stc                     ;use step
        ret

rm_bytes:
        mov     bl, al
        and     al, 7
        cmp     bl, 0c0h
        jae     short rm_w3
        test    ch, ADR32   ;32 bit addressing mode
        jnz     short rm_d1
        test    bl, 0c0h
        jz      short rm_w0
        test    bl, bl
        jns     short rm_w1
rm_w2:  inc     si
rm_w1:  inc     si
rm_w3:  ret
rm_w0:  cmp     al, 6
        je      short rm_w2
        ret

rm_d1:  cmp     al, 4
        jne     short rm_d2
        lods    byte ptr es:[si] ;SIB
        and     al, 7            ;base
rm_d2:  test    bl, 0c0h
        jz      short rm_l0
        test    bl, bl
        jns     short rm_w1
rm_d3:  add     si, 4
        ret
rm_l0:  cmp     al, 5
        je      short rm_d3
        ret

;input: ES:SI   pointer to first byte of instruction
;       SS=DS=CS
;output C=0     ES:SI = Effective address of instruction
;               CS:DX = Address of display routine
;       C=1     No effective address

eff_adr:xor     di, di                  ;0=no segment override
        xor     cx, cx                  ;ch=override flags
                                        ;bit0=32 bit operand size
                                        ;bit1=32 bit address size
_a_prefix:
effadr0:lods    byte ptr es:[si]
        mov     ah, 0
        mov     bp, ax
        shl     bp, 1
        mov     bp, opcode1[bp]
effadr1:mov     bx, 0fc00h
        and     bx, bp
        jnz     effadr2
        lods    byte ptr es:[si]
        mov     bl, 38h
        and     bl, al
        shr     bl, 1
        shr     bl, 1
        add     bp, bx
        mov     bx, 0fc00h
        and     bh, byte ptr opcode2[bp+1]
effadr2:xchg    bl, bh
        jmp     TypeTable[bx+2]

_a_segovr:
;       Prefix instructions
        cmp     al, 3eh                 ;DS:
        je      ds_prfx
        cmp     al, 36h                 ;SS:
        je      ss_prfx
        cmp     al, 2eh                 ;CS:
        je      cs_prfx
        cmp     al, 26h                 ;ES:
        je      es_prfx
        mov     di, offset save_fs
        cmp     al, 64h                 ;FS:
        je      effadr0
        ; should be GS:
        mov     di, offset save_gs
        jmp     effadr0
es_prfx:mov     di, offset save_es
        jmp     effadr0
ds_prfx:mov     di, offset save_cs
        jmp     effadr0
ss_prfx:mov     di, offset save_ss
        jmp     effadr0
cs_prfx:mov     di, offset save_cs
        jmp     effadr0

_a_32ovr:
        sub     al, 65h                 ;66->1, 67->2
        xor     ch, al
        jmp     effadr0

_a_0fp:
;       286/386 prefix 0fh
        lods    byte ptr es:[si]
        mov     bl, al
        shr     bl, 1
        shr     bl, 1
        shr     bl, 1
        and     al, 7
        add     al, sysinst[bx]
        js      no_efad
        mov     bl, al
        shl     bl, 1
        mov     bp, opcode_0f[bx]
        jmp     effadr1
_a_none:
_a_ins10:
_a_r16:
_a_seg:
_a_rel8:
_a_rel16:
_a_seg_off:
_a_nn:
_a_nnnn:
_a_nnnnw:
_a_ax_r16:
_a_acc_imm:
_a_reg_imm:
_a_sx8:
_a_nnnn_nn:
_a_fs_gs:
 _a_in_nn:
_a_in_dx:
_a_out_nn:
_a_out_dx:
_a_sb_or_sw:
_a_d_for_32:
no_efad:stc
        ret
_a_0f_2x:
_a_reg_rm:
_a_lea_reg_rm:
_a_rm_reg:
_a_seg_rm:
_a_rm_seg:
_a_r16_rm:
_a_r16_rm8:
_a_r32_rm16:
_a_reg_rm_sx8:
_a_reg_rm_nnnn:
_a_rmw_nn:
_a_rm_reg_sc:
_a_float:
_a_rmb:
        lods    byte ptr es:[si]
_a_rm:
_a_rmw:
_a_dword:
_a_rm_imm:
_a_rm_sx8:
_a_rm_sc:
_a_mem24:
;       r/m
        cmp     al, 0c0h
        jae     no_efad
        and     al, 0c7h
        mov     bl, al
        test    ch, ADR32               ;32-bit address?
        jnz     _addr_32
        cmp     al, 6
        je      eff_abs
        shl     bl, 1
        jc      rm_adr2
        mov     al, 0
        jns     rm_adr1
        lods    byte ptr es:[si]
rm_adr1:cbw
        jmp     short rm_adr3
rm_adr2:lods    word ptr es:[si]
rm_adr3:xchg    si, ax
        and     bx, 14
        jmp     regbank[bx]

_a_acc_addr:
_a_addr_acc:
;       Absolute address
eff_abs:mov     si, es:[si]
ds_defl:test    di, di
        jnz     segovrr
        mov     di, offset save_ds
segovrr:mov     es, ds:[di]
;       mov     dx, offset hex_16
        ret

rm__00: add     si, ds:save_bx
rm__04: add     si, ds:save_si
        jmp     ds_defl
rm__01: add     si, ds:save_di
rm__07: add     si, ds:save_bx
        jmp     ds_defl
rm__05: add     si, ds:save_di
        jmp     ds_defl
rm__02: add     si, ds:save_si
rm__06: add     si, ds:save_bp
ss_defl:test    di, di
        jnz     segovrr
        mov     es, ds:save_ss
        ret
rm__03: add     si, ds:save_di
        jmp     rm__06

_addr_32:
        and     al, 7
        cmp     al, 4
        jne     _no_sib

        lods    byte ptr es:[si]
        mov     ah, al
        and     ax, 07f8h
        and     bl, 0c0h
        or      bl, ah                  ;bl=[mm000bbb]

        mov     bp, 38h
        and     bp, ax
        cmp     bp, 4*8                 ;100=no index reg
        je      _no_sib
        shr     bp, 1
        shr     bp, 1
        mov     bp, r32index[bp]
        mov     bp, [bp]
@@:     add     al, 0c0h
        jnc     _done_sib
        shl     bp, 1
        jmp     @b

_no_sib:xor     bp, bp                  ;base
_done_sib:
        lods    word ptr es:[si]
        cmp     bl, 5
        je      _efad3                  ;actualy d32 (ignore high word)
        shl     bl, 1
        jc      _efad1
        jns     _efad2
        cbw
_efad1: add     bp, ax
_efad2: and     bl, 14
        mov     si, r32index[bx]
        add     bp, [si]
        mov     si, bp
        lea     ax, [bx-8]
        and     al, 6*2                 ;100, 101 use SS:
        jz      ss_defl
        jmp     ds_defl

_efad3: add     ax, bp
        xchg    si, ax
        jmp     ds_defl


;input: DX:BP  pointer to first byte of instruction
;       DS=CS
;output DX:BP  pointer to next instruction
;       DS:SI  pointer to buffer

dispins:push    es
        push    cs
        pop     es
        cld
        mov     di, offset dissBuffOff
        mov     ax, bp
        call    hexw
        mov     cx, (dissBuffEnd - dissBuffOff - 4)/2
        mov     ax, 2020h
        rep     stosw
        mov     word ptr rm_type,cx ;clear rm_type, sg_name
        push    bp
        mov     di, offset dissBuffOpcode
dspins1:call    lodbyt
        mov     cl, al
        mov     bx, cx
        shl     bx, 1
        mov     si, opcode1[bx]
dspins3:mov     bx, 0fc00h
        and     bx, si
        jnz     dspins4
        call    lodbyt
        mov     ch, al
        shr     al, 1
        shr     al, 1
        and     ax, 14
        add     si, ax
        mov     si, opcode2[si]
        mov     bh, 0fch
        and     bx, si
dspins4:xor     si, bx
        xchg    bl, bh
        call    lodnam1
        mov     si, di
        cmp     di, offset dissBuffOprs
        jae     @f
        mov     di, offset dissBuffOprs-1
@@:     inc     di
        call    TypeTable[bx]
        mov     di, offset dissBuffHex
        mov     cx, bp
        pop     si
        sub     cx, si
        mov     ds, dx
        cmp     cx, 7
        jb      dsphex
        mov     cx, 6
dsphex: lodsb
        call    hexb
        loop    dsphex
        pop     es
        push    cs
        pop     ds
        mov     si, offset dissBuffOff
        ret

__32ovr:; 32 bit operand (66h), address (67h) override
        sub     cl,65h          ;66->1, 67->2
        xor     rm_type,cl
        lea     di, [si-1]
        pop     si              ;remove ret address
        jmp     dspins1

__segovr:;                      seg: override
        mov     al,[si-3]       ;seg name (d/c/e/s/f/g)
        xchg    sg_name, al     ;seg name (d/c/e/s/f/g)
        cmp     al, 0
        jne     put_sgo
        mov     word ptr [si-2], '  '
        lea     di, [si-3]
        pop     si              ;remove ret address
        jmp     dspins1
put_sgo:mov     [si-3], al
        ret

__prefix:;                      Prefix instructions
        cmp     di, offset dissBuffOprs
        jne     _jret1          ;if not the first one
        pop     si              ;remove ret address
        jmp     dspins1

lodbyt: push    es
        mov     es, dx
        mov     al, es:[bp]
        inc     bp
        pop     es
_jret1: ret

lodwrd: push    es
        mov     es, dx
        mov     al, es:[bp]
        inc     bp
        mov     ah, es:[bp]
        inc     bp
        pop     es
        ret

__reg_rm_sx8:;                  reg,r/m,+n/-n
        call    __r16_rm
        jmp     short comasn
__rm_sx8:;                      r/m,+n/-n
        call    rm_addr
comasn: mov     al,','
        stosb
__sx8:  ;                       +n/-n
        call    lodbyt
        mov     ah,'+'
        test    al,al
        jns     psign
        neg     al
        mov     ah,'-'
psign:  mov     byte ptr[di],ah
        inc     di
        jmp     short hexb
__rel8: ;                       label rel.8
        call    lodbyt
        cbw
        add     ax,bp
        dec     di      ;;      mov     byte ptr[di],'L'
hexw1:  inc     di
hexw:   call    hexah
hexah:  xchg    ah,al
hexb:   push    ax
        shr     al,1
        shr     al,1
        shr     al,1
        shr     al,1
        cmp     al,10
        cmc
        adc     al,'0'
        daa
        stosb
        pop     ax
        and     al,0fh
        cmp     al,10
        cmc
        adc     al,'0'
        daa
        stosb
        ret

lodnam1:add     si, offset X0
lodnam: lodsb
        and     al, 7           ;length field
lodnam2:movsb
        dec     al
        jnz     lodnam2
        ret

__seg_rm:;                      seg,r/m
        call    lodbyt
        mov     ch,al
        call    sgname
        mov     cl,1
        jmp     short and_mem
__rmb:  ;                       r/m8    ........ mmxxxr/m (setcc)
        call    lodbyt
        mov     ch,al
        mov     cl,0
        jmp     short __rm
__rmw:  ;                       word ptr r/m
        mov     cl,1
__rm:   ;                       r/m     .......w mm...r/m
        cmp     ch,0c0h
        jae     rm_addr
        mov     si,offset _byte 
        test    cl,1
        jz      memtypp
        test    rm_type,OPR32
        jz      rm_word
__dword:;                      dword ptr r/m
        mov     al,'D'
        stosb
rm_word:mov     si,offset _word
memtypp:movsw
        movsw
        mov     al, ' '
        stosb
        jmp     short rm_addr
__r32_rm16:;                    movsx/movzx r32,r/m16
        or      rm_type,OPR32
        mov     cl,1            ;16/32 bits
        call    lodbyt
        mov     ch,al
        call    regstr
        and     rm_type,not OPR32 ;16 bits
        mov     al,','
        stosb
        jmp     short rm_word
__r16_rm8:;                     movsx/movzx r16/32,r/m8
        mov     cl,1            ;16/32 bits
        call    lodbyt
        mov     ch,al
        call    regstr
        mov     cl,0            ;byte
        mov     al,','
        stosb
        cmp     ch, 0c0h
        jae     short rm_addr
        mov     si,offset _byte 
        jmp     short memtypp
__lea_reg_rm:;                  lea reg,r/m
        mov     al,rm_type
        and     al,ADR32
        add     al,0ffh
        sbb     al,al
        and     al,OPR32+ADR32
        or      rm_type,al
__r16_rm:;                      r16,r/m
        mov     cl,1
__reg_rm:;                      reg,r/m
        call    lodbyt
        mov     ch,al
reg_memory:
        call    regstr
and_mem:mov     al,','
        stosb
__mem24:;                       r/m (for gdt and idt)
_f_MEM:
rm_addr:mov     bx,7
        and     bl,ch
        shl     bx,1
        cmp     ch,0c0h
        jae     mode3
        mov     al,sg_name
        test    al,al
        jz      memaddr
        stosb
        mov     ax,3a53h        ;xs:
        stosw
memaddr:mov     al,'['
        stosb
        test    rm_type,ADR32   ;32 bit addressing mode
        jnz     rm32bit
        test    ch,0c0h
        jz      mode0
        mov     si, modtbl[bx]
        call    lodnam
        test    ch,ch
        js      mode2
        call    __sx8
        mov     al,']'
        stosb
        ret
mode2:  mov     al,'+'
        stosb
mode2d: call    immwrd
        mov     al,']'
        stosb
        ret
mode0:  cmp     bl,0ch
        je      mode2d
        mov     si, modtbl[bx]
        call    lodnam
        mov     al,']'
        stosb
        ret

__rmw_nn:;                      r/m16/32,imm8
        call    __rmw
        jmp     and_nnb
__rm_reg:;                      r/m,reg
        call    lodbyt
        mov     ch,al
mem_reg:call    rm_addr
        mov     al,','
        stosb
regstr: mov     al,ch
        shr     al,1
        shr     al,1
        and     ax,0eh
        xchg    ax,bx
mode3:  test    cl,1
        jz      reg8
        add     bx,16
        test    rm_type,1       ;32 operand size
        jz      reg8
        mov     al,'E'
        stosb
reg8:   mov     ax,word ptr regnm8[bx]
        stosw
        ret

rm32bit:cmp     bl,8
        jne     chkmod
        call    lodbyt          ;SIB
        mov     bl,al
        and     ax,7*8          ;index bits
        cmp     al,4*8          ;index=4 mean no index register
        je      sibbase
        xchg    si,ax
        shr     si,1
        shr     si,1
        mov     ax,0c001h
        and     ah,bl
        jz      sibscl2
sibscl1:shl     al,1
        sub     ah,40h
        ja      sibscl1
        add     ax,2a30h
        stosw                   ;n*
sibscl2:mov     al,'E'
        stosb
        mov     ax,word ptr regnm16[si]
        stosw
        mov     al,'+'
        stosb
sibbase:and     bl,7
        shl     bl,1
chkmod: test    ch,0c0h         ;mod=00
        jz      mod00
        mov     al,'E'
        stosb
        mov     ax,word ptr regnm16[bx]
        stosw
        test    ch,ch
        js      mod10
        call    __sx8           ;disp8
        mov     al,']'
        stosb
        ret
mod10:  mov     al,'+'
        stosb
mod2d:  call    immdwrd
        mov     al,']'
        stosb
        ret
mod00:  cmp     bl,10
        je      mod2d
        mov     al,'E'
        stosb
        mov     ax,word ptr regnm16[bx]
        stosw
        mov     al,']'
        stosb
        ret
        
__acc_addr:;                    Acc,address
        mov     ch,06h
        mov     al,rm_type
        and     al,ADR32
        neg     al
        sbb     ch,0
        jmp     reg_memory
__addr_acc:;                    address,Acc
        mov     ch,06h
        mov     al,rm_type
        and     al,ADR32
        neg     al
        sbb     ch,0            ;mode 5 for 32 bit address mode
        jmp     mem_reg
__rm_seg:;                      r/m,seg
        call    lodbyt
        mov     ch,al
        mov     cl,1
        call    rm_addr
        mov     al,','
        stosb
sgname: mov     al,ch
        shr     al,1
        shr     al,1
        and     ax,7*2
        xchg    ax,si
        mov     ax,word ptr segnam[si]
        stosw
        ret

__seg_off:;                     seg:off
        mov     bx,di
        add     di,5
        call    ni16_32
        xchg    di,bx
        call    immwrd
        mov     al,':'
        stosb
        mov     di,bx
        ret
__acc_imm:;                     Acc, imm
        shl     cl,1
        shl     cl,1
        shl     cl,1
__reg_imm:;                     reg,imm
        call    r16nam
        test    cl,8
        jz      and_nnb
and_nnw:mov     al,','
        stosb
__nnnnw:;                       push nnnn (16/32)
ni16_32:test    rm_type,OPR32   ;32 bit operand size
        jz      immwrd
immdwrd:call    lodwrd
        push    ax
        call    immwrd
        pop     ax
        jmp     hexw
__nnnn: ;                       nnnn
immwrd: call    lodwrd
        jmp     hexw
__reg_rm_nnnn:;                 reg,r/m,nnnn
        call    __r16_rm
        jmp     and_nnw
__rm_reg_sc:;                   r/m,reg,cl/nn
        rol     cl, 1           ; make it word (always word)
        call    __rm_reg
        test    cl, 2
        jz      and_nnb
and_cl: mov     al,','
        stosb
        mov     ax,4c43h            ;cl
        stosw
        ret
__rm_sc:;                       r/m,1/cl/nn
        call    __rm
        test    cl,16
        jz      and_nnb
        test    cl,2
        jnz     and_cl
        mov     ax,312ch
        stosw                       ;,1
        ret
__nnnn_nn:;                     enter nnnn,nn
        call    immwrd
and_nnb:mov     al,','
        stosb
__nn:   ;                       nn
        call    lodbyt
        jmp     hexb
__rm_imm:;                      r/m,imm (8/16)
        call    rm_addr
        shr     cl,1
        jnc     and_nnb
        jmp     and_nnw
__ax_r16:;                      ax,r16
        test    rm_type,1       ;32 bit op size
        jz      _axnam1
        mov     al,'E'
        stosb
_axnam1:mov     ax,5841h
        stosw
        mov     al,','
        stosb
__r16:  ;                       r16
        or      cl,8
r16nam: mov     bx,15
        and     bl,cl
        shl     bx,1
        test    cl,8            ;word ?
        jz      r16nam1
        test    rm_type,1       ;32 bit op size
        jz      r16nam1
        mov     al,'E'
        stosb
r16nam1:mov     ax,word ptr regnm8[bx]
        stosw
        ret

__in_dx:
        call    accum
and_dx: mov     al,','
        stosb
        mov     ax,5844h
        stosw
        ret
__in_nn:
        call    accum
        jmp     and_nnb
__out_dx:
        mov     ax,5844h
        stosw
and_acc:mov     al,','
        stosb
accum:  mov     ax,4c41h        ;al
        test    cl,1
        jz      accm2
        test    rm_type,OPR32
        jz      accm1
        mov     byte ptr [di],'E'
        inc     di
accm1:  mov     ah,'X'
accm2:  stosw
        ret
__out_nn:
        call    __nn
        jmp     and_acc

__sb_or_sw:;    b/w/d in ins/outs/lods/scas/movs/cmps/stos
        mov     al, 'B'         ;sb
        test    cl,1
        jz      put_sb
        mov     al, 'W'         ;sw
        test    rm_type,OPR32
        jz      put_sb
        mov     al, 'D'         ;sd
put_sb: mov     [si], al        ;to the end of inst
        mov     al, 0
        xchg    al, sg_name     ;seg name (d/c/e/s/f/g)
        test    al, al
        jz      put_ret
        cmp     di, offset dissBuffOprs
        ja      @f
        mov     di, offset dissBuffOprs-1
@@:     inc     di
;;      lea     di, 2[si]
        stosb
        mov     ax, ':S'
        stosw
put_ret:ret

__d_for_32:;    add d to push/pop a/f if 32
        test    rm_type,OPR32
        jz      ret_d4
        mov     byte ptr [si],'D'
ret_d4: ret

sysinst db op_0f00, 80h,     80h,     80h      ; 00-1f
        db op_0f20, 80h,     80h,     80h      ; 20-3f
        db 80h,     80h,     80h,     80h      ; 40-5f
        db 80h,     80h,     80h,     80h      ; 60-7f
        db op_0f80, op_0f88, op_0f90, op_0f98  ; 80-9f
        db op_0fa0, op_0fa8, op_0fb0, op_0fb8  ; a0-bf
        db 80h,     80h,     80h,     80h      ; c0-df
        db 80h,     80h,     80h,     80h      ; e0-ff

__0fp:  ;                       PROTECTION CONTROL
        call    lodbyt
        mov     di, offset dissBuffOpcode
        mov     bl, al
        mov     cl, 3
        shr     bl, cl
        mov     cl, al
        and     al, 7
        add     al, sysinst[bx]
        js      invalid
        mov     bl, al
        shl     bl, 1
        mov     si, opcode_0f[bx]
        pop     bx                      ;remove ret from stack
        jmp     dspins3

__rel16:;                       label rel.16
;;      mov     al,'L'
;;      stosb
        call    lodwrd
        test    rm_type,ADR32
        jz      rel16l1
        xchg    bx,ax           ;adr lo
        call    lodwrd          ;adr hi
        add     bx,bp
        adc     ax,0
        call    hexw
        xchg    ax,bx
        jmp     hexw
rel16l1:add     ax,bp
        jmp     hexw
__ins10:;                       aam,aad
        call    lodbyt
        cmp     al,10
        je      __none
invalid:mov     al, '?'
        stosb
        ret
__0f_2x:;                       mov CRn/DRn/TRn,r/m
        call    lodbyt
        mov     ch,al
        stc
        rcl     cl,1            ;for word by rm_addr
        or      rm_type,1       ;32 bit operand size
        test    cl,4
        jz      rdctlr1
        call    CTLreg
        jmp     and_mem
rdctlr1:call    rm_addr
        mov     al,','
        stosb
CTLreg: mov     ax,5254h            ;TR
        test    cl,8
        jnz     ctlrg1
        mov     al,'C'
        test    cl,2
        jz      ctlrg1
        mov     al,'D'              ;CR or DR
ctlrg1: stosw
        mov     al,ch
        shr     al,1
        shr     al,1
        shr     al,1
        and     al,7
        add     al,'0'
        stosb
        ret
__seg:  ;                       seg     ...ss...
        mov     bx,38h
        and     bl,cl
        shr     bx,1
        shr     bx,1
        mov     ax,word ptr segnam[bx]
        stosw
__none: ;                       No operand Inst's
        ret
__fs_gs:;                       push/pop FS/GS
        mov     al,8
        and     al,cl       ;0=FS, 8=GS
        cmp     al,8
        mov     ax,5347h    ;GS
        sbb     al,0
        stosw
        ret

__float:;                       esc : NDP coprocessor
        call    lodbyt
        mov     ch, al
        mov     bx, 38h
        and     bl, ch
        mov     di, offset dissBuffOpcode+1
        test    cl, 1
        jnz     float3
        shr     bx, 1
        shr     bx, 1
        mov     si, _ndpgr1[bx]
        call    lodnam
        cmp     ch, 0c0h
        jae     float1

        mov     bl, 6
        and     bl, cl
        mov     di, offset dissBuffOprs
f__RM:  mov     si, ndptype[bx]
        call    lodnam
        mov     al, ' '
        stosb
        jmp     rm_addr

_f_R32:
_f_I32:
_f_R64:
_f_I16:
_f_R80:
_f_I64:
_f_B80: sub     bx, 2*_t_R32
        jmp     f__RM

float1: test    cl, 2
        jz      float2
        mov     al, 'P'
        stosb
float2: mov     di, offset dissBuffOprs
        test    cl, 4                   ; 0: ST,STi 1: STi
        jnz     _f_STi
        mov     ax, 'TS'
        stosw
        mov     al, ','
        stosb
_f_STi: mov     ax, 'TS'
        stosw
        mov     al, ch
        and     al, 7
        add     al, '0'
        stosb
_f_none:ret

float3: mov     ah, 6
        and     ah, cl
        add     bl, ah
        cmp     al, 0c0h
        jb      float4
        add     bl, (_ndp_gr2 - _ndp_gr1)
float4: mov     si, _ndp_gr1[bx]
        mov     bx, 0f000h
        and     bx, si
        jnz     float5
        mov     bl, 7
        and     bl, ch
        shl     bl, 1
        mov     si, _ndp_gr3[si+bx]
        jmp     lodnam
float5: xor     si, bx
        call    lodnam1
        mov     di, offset dissBuffOprs
        xchg    bl, bh
        shr     bx, 1
        shr     bx, 1
        shr     bx, 1
        jmp     _ndp_type[bx-2]