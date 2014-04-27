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

include htools.inc
include SoftProb.inc

stack   segment para STACK
        db      140h dup(0)
stack   ends

code    segment 'code'
        assume  cs:code,ds:code,ss:code

byte0   equ     this byte
word0   equ     this word

;----------------------------------------------------------------------------
Load_NoExec_Params label word
                dw 0                    ; env (0=current)
L_CmdLine       dw -1,?                 ; Command line
L_Fcb1          dw 5ch,?                ; FCB1
L_Fcb2          dw 6ch,?                ; FCB2
L_SS_SP         dw 0,0                  ; SS:SP on return
L_CS_IP         dw 0,0                  ; CS:IP on return

errQuit:pop     dx
        push    cs
        pop     ds
        mov     ah, 9
        int     21h
        mov     ax, 4c00h
        int     21h

Terminate_Addr:
        mov     ah, 4dh
        int     21h
        aam
        add     byte ptr cs:[retCode+2], al
        mov     al, ah
        aam
        xchg    al, ah
        add     word ptr cs:[retCode+0], ax
        call    errQuit
        db      13,10,56 dup ('Í')
        db      13,10,'Program terminated with return code '
retCode db      '000.$'

memError:
        call    errQuit
        db      'Memory allocation error.',13,10,36

appLoad:int     21h
        jc      memError
        mov     dx, offset filename     ; ds:dx=Asciz filename
        push    cs
        pop     es
        mov     bx, offset Load_NoExec_Params
        mov     ax, 4b01h
        int     21h
        jc      loadError
        mov     ah, 62h
        int     21h
        mov     es, bx
        mov     ds, bx
        mov     word ptr ds:[0ah], offset Terminate_Addr
        mov     word ptr ds:[0ch], cs

        mov     ax, 0000
drvStat equ     $-2
        xor     bx, bx
        xor     cx, cx
        xor     dx, dx
        xor     si, si
        xor     di, di
        xor     bp, bp
        mov     ss, cs:L_SS_SP[2]
        mov     sp, cs:L_SS_SP[0]
        pushf
        push    cs:L_CS_IP[2]
        push    cs:L_CS_IP[0]
        db      0eah
SoftProbe_Entry dw 0,0

loadError:
        call    errQuit
        db      13,10,'Error loading '
filename        db 80 dup (0)

;----------------------------------------------------------------------------
; Memory below this point will be released before application load.
;----------------------------------------------------------------------------
copyright:      db 56 dup ('_'),13,10,13,10
                copyrightMsg 'SPL, SoftProbe'
                db 56 dup ('_'),13,10,'$'

monSign:        Probe_Signature queue, justRet, montdev

switchChar      db '/'

;----------------------------------------------------------------------------
start:  push    es                      ; PSP
        push    cs
        pop     ds
        mov     dx, offset copyright
        mov     ah, 9
        int     21h

        mov     ax, 3700h
        int     21h
        mov     switchChar, dl

        pop     ax                      ; PSP
        mov     L_CmdLine[2], ax
        mov     L_Fcb1[2], ax
        mov     L_Fcb2[2], ax
        mov     ds, ax
        cld
        mov     si, 81h
skipB1: lodsb
        cmp     al, 9
        je      skipB1
        cmp     al, ' '
        je      skipB1
        ja      copyName
        call    errQuit
        db      'USAGE:  SPL [d:][path\]<progname>[.EXE] [<parameters>]'
        db      13,10,36

copyName:
        push    cs
        pop     es
        mov     di, offset filename
        cmp     byte ptr [si], ':'
        je      copyN1
        mov     ah, 19h
        int     21h
        add     al, 'A'
        stosb
        dec     si
        mov     al, ':'
copyN1: mov     cl, 1
copyN2: stosb
        lodsb
        cmp     al, 'a'
        jb      copyN3
        cmp     al, 'z'+1
        jae     copyN2
        sub     al, 'a'-'A'
        jmp     copyN2
copyN3: cmp     al, 21h
        jb      copyN4
        cmp     al, '\'
        je      copyN1
        cmp     al, '.'
        jne     copyN2
        xor     cx, cx
        jmp     copyN2

copyN4: jcxz    copyN5
        mov     ax, 'E.'                ; .EXE by default
        stosw
        mov     ax, 'EX'
        stosw
copyN5: mov     ax, 2400h               ; \0, $
        stosw
        push    di

        dec     si
        add     cs:L_CmdLine[0], si     ; si-1

        push    ds
        pop     es
        mov     ax, 81h
        sub     ax, si
        add     al, byte ptr ds:[80h]
        mov     [si-1], al

        mov     di, 5ch
        call    nonParam
        mov     byte ptr cs:[drvStat+0], al

        mov     di, 6ch
        call    nextParam
        push    cs
        pop     ds
        mov     byte ptr ds:[drvStat+1], al
        xor     ax, ax                  ; int no. to check
FindMontr1:
        xor     cx, cx
        mov     es, cx
        mov     di, ax
        shl     di, 1
        shl     di, 1
        les     bx, dword ptr es:[di]
        mov     si, offset monSign
        mov     di, 0ah
        mov     cl, montdev - monSign + 1
        repe    cmpsb
        jne     FindMontr2
        scasb                           ; int vector used (60h by default)
        je      FindMontr3
FindMontr2:
        inc     al                      ; next int
        jnz     FindMontr1
        call    errQuit
        db      'SoftProbe is not loaded.',13,10,36

FindMontr3:
        mov     SoftProbe_Entry[0], bx
        mov     SoftProbe_Entry[2], es

        pop     di                      ; end of filename
                                        ; also end of resident section
        mov     ax, L_CmdLine[2]        ; PSP
        mov     es, ax
        mov     cl, 4
        dec     di
        shr     di, cl
        inc     di
        mov     bx, cs
        add     bx, di
        sub     bx, ax
        mov     ah, 4ah
        jmp     appLoad

nextParam:
        lodsb
        cmp     al, 21h
        jae     nextParam
        dec     si
nonParam:
        lodsb
        cmp     al, 9
        je      nonParam
        cmp     al, ' '
        je      nonParam
        cmp     al, cs:switchChar
        je      nextParam
        dec     si
;;      cmp     al, 20h
;;      jb      noParam
        mov     ax, 2901h
        int     21h
        ret
noParam:mov     al, 0
        ret

code    ends

        end     start
