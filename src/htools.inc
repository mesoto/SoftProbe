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

DBGBRK  macro
IF      DBG
        int     3
ENDIF
endm

ver_str equ     '2.3'

copyrightMsg    macro   pname
        db pname
        db ' Version 2.3.',13,10
        db 'Copyright (C) Mehdi Sotoodeh 1995.  '
        db 'All rights reserved.',13,10
        endm

License_Message    macro msg_quit
        endm

Str_Len macro
        local   l1
        xor     cx, cx
l1:     lodsb
        cmp     al, 0
        loopne  l1
        not     cx
        endm

Str_Display     macro
        mov     dx, si
        Str_Len
        mov     bx, 1
        mov     ah, 40h
        int     21h
        endm

;**************************************************************************
;* IN:  AX      = input value
;*      ES:DI   = ptr buffer
;* KILLS:       AX
;**************************************************************************

Bin_2_Hex       macro hexb, hexw
        ifnb    <hexw>
hexw:   call    hexh
hexh:   xchg    al,ah
        endif
hexb:   push    ax
        shr     al,1
        shr     al,1
        shr     al,1
        shr     al,1
        call    hexd
        pop     ax
        and     al,0fh
hexd:   cmp     al,10
        cmc
        adc     al,'0'
        daa
        stosb
        ret
        endm


;**************************************************************************
;* IN:  DS:SI   = ptr buffer
;* OUT: CF = 0  if ok, CX:DX=value
;*      CF = 1  if error
;**************************************************************************

Hex_2_Bin_DWord macro
getHx0: lodsb
        cmp     al, 9
        je      getHx0
        cmp     al, ' '
        je      getHx0
        xor     cx, cx
        xor     dx, dx
getHx1: cmp     al, 'A'
        jb      getHx2
        and     al, not 20h
        sub     al, 7
getHx2: sub     al, '0'
        cmp     al, 16
        jae     getHx4
getHx3: add     dx, dx
        adc     cx, cx
        add     al, 40h
        jnc     getHx3
        or      dl, al
        lodsb
        cmp     al, ' '
        ja      getHx1
        dec     si
        clc
        ret
getHx4: dec     si
        stc
        ret
        endm