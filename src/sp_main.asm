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

; Switches:     _BOOT           Bootable version

include htools.inc
include SoftProb.inc

code    segment 'code'
        assume  cs:code,ds:code,ss:code

include sp_core.asm

;----------------------------------------------------------------------------
; Installer routines
;----------------------------------------------------------------------------
        even
intvecs dw      timrvec, kbdvec, vidvec, prtvec, i21vec, save_ax ; save_cx

Install:        ;-- common procedure for boot/non-boot version
        int     11h
        push    cs
        pop     ds
        cld
        and     al, 30h
        cmp     al, 30h
        je      @f
        call    SwitchScreen
@@:     pushf
        xor     bx, bx
        push    bx
        popf
        pushf
        pop     ax
        mov     bh, 0f0h                ;try to set bits 12-15
        xor     ax, bx
        and     ax, bx
        jz      @f                      ;8086: bits 12-15 always set
        push    bx
        popf
        pushf
        pop     ax
        and     ax, bx
        jz      @f                      ;286: bits 12-15 are cleared
        ; its a 386+, enable the commands
        mov     word ptr [i386_fix1], fixup_1
        mov     word ptr [i386_fix2], fixup_2
        mov     word ptr [i386_fix3], fixup_3
        ; for 386+ int mask is not needed
        mov     word ptr [i386_fix4], fixup_4
        mov     word ptr [i386_fix5], fixup_5
@@:     popf
        push    cs
        pop     es
        mov     di, offset savedSCREEN
        call    InitScreenEsDi
        mov     ax, cs
        mov     Int01_OldVector[2], ax
        mov     Int03_OldVector[2], ax
        add     ax, (findstr-code0+1)/16
        mov     word ptr [findstr_Fix], ax
        add     ax, resident_Size - (findstr-code0+1)/16
        mov     workingMemSeg, ax
        mov     customCall[2], ax
        mov     data_ptr[2], ax
        mov     data_marker[1*4+2], ax
        add     ax, workingMemLen       ; end of TSR
        mov     code_ptr[2], ax
        mov     data_marker[2*4+2], ax

        xor     cx, cx
        mov     ds, cx
        mov     bx, offset intvecs   ;8,9,10,17,21,brk2Int
        mov     cl, 6
        cli
fixint1:mov     di, es:[bx]
        inc     bx
        inc     bx
        mov     ax, es:[di+0]
        mov     si, es:[di+2]
        movsw
        movsw
        test    ax, ax
        jz      fixint2
        mov     [si-4], ax
        mov     [si-2], cs
fixint2:loop    fixint1
        xor     si, si
        mov     di, offset int_buf
        mov     cx, 2*100h
        rep     movsw
        sti
        
        push    cs
        pop     ds
        mov     dx, offset ok_install
        mov     ah, 9
        int     21h
        mov     ax, workingMemSeg
        add     ax, workingMemLen       ; end of resident part
        ret

PrevMonSeg      dw 0
NUL_dev db      'NUL     '

;----------------------------------------------------------------------------
; Start of SoftProb.SYS (.EXE)
;----------------------------------------------------------------------------
deviceInit:
        push    ax
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    bp
        push    ds
        push    es
        lds     si, dword ptr es:[bx+18] ; BPB ptr as command line
        cld                             ; device=SoftProb.exe ...
@@:     lodsb                           ;        ÀÄÄÄÄ>
        cmp     al, 21h
        jae     @b
        dec     si
        call    do_CommandLine
        jc      @f
        call    Install
        ; ax = seg(end of DRV) if no error
@@:     mov     service, offset queue   ; next calls will not come here
        pop     es
        pop     ds
        pop     bp
        pop     di
        pop     si
        pop     dx
        pop     cx
        pop     bx
        mov     word ptr es:[bx+14], 0  ; end address (offset)
        jc      bad_init
        mov     word ptr es:[bx+3], 100h
        mov     es:[bx+16], ax          ; end address (segment)
        mov     cs:service+2, offset montdev
        pop     ax
        retf

bad_init:
        mov     word ptr es:[bx+3], 810ch
        mov     es:[bx+16], cs          ; cs:0 as end address
        pop     ax
        retf

;----------------------------------------------------------------------------
; Start of SoftProb.EXE
;----------------------------------------------------------------------------
start:  push    ds              ;PSP
        mov     si, 81h
        cld
        DBGBRK
        call    do_CommandLine
        jc      @f
        call    Install
@@:     pop     dx              ;PSP
        jc      cant_install
        ; ax = seg(end of TST)
        sub     ax, dx
        push    ax
        mov     ds, dx
        mov     es, ds:2ch
        mov     ah, 49h         ;Free our env
        int     21h
        pop     dx
        mov     ax,3100h
        int     21h

cant_install:
        mov     ax, 4c01h
        int     21h

is_InstMsg      dw  offset is_installed
is_installed    db 'SoftProbe is already loaded.',13,10

ok_install      db 'Press LShift-RShift keys to activate it.',13,10,36

include sp_cmdl.asm

copyright:      db 56 dup ('_'),13,10,13,10
                copyrightMsg 'SoftProbe'
                db 56 dup ('_'),13,10,'$'

cmd_Line        db 128 dup(0)

code    ends

stack   segment para STACK
        dw      200h dup(0)
stack   ends

        end     start