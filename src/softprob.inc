brk2Int equ     60h     ; second break

d_el            = 00h   ;
d_sub           = 01h   ; swap si & bp (same as call)
d_mv            = 02h   ;
d_dup           = 03h   ;
d_clrvu         = 04h   ;
;               = 05h   ;
d_string        = 06h   ; display string ds:dx (fixed)
d_text          = 07h   ; display string ds:dx (variable)
d_decim         = 08h   ; word on stack top
d_stk04         = 09h   ; word on stack top
d_stk08         = 0ah   ; word on stack top
d_stk16         = 0bh   ; word on stack top
d_stk32         = 0ch   ; dword on stack top
d_mem08         = 0dh   ; display mamory at ds:dx
d_mem16         = 0eh   ; display mamory at ds:dx
d_mem32         = 0fh   ; display mamory at ds:dx
d_ascii         = 10h   ; display mamory at ds:dx
d_bits          = 11h   ; edit fields start
d_ed1           = 12h   ; edit fields start
d_yn            = 13h   ; get yes/no
d_geta          = 14h   ; get ascii
d_getb          = 15h   ;
d_getw          = 16h   ;
d_getn          = 17h   ; get decimal number
d_gets          = 18h   ; hex/ascii
d_getd          = 19h   ;
d_ed2           = 1ah   ; edit fields end
a_first         = 1bh
a_b             = 1bh   ; d_bold
a_n             = 1ch   ; d_norm
a_r             = 1dh   ; d_rvrs
a_h             = 1eh   ; d_undr
a_f             = 1fh   ; d_frame
a_cur           = 20h   ; cursor attrib

_bits           macro   m, s
                db      d_bits
                dw      m
                db      s
                endm

_getb           macro   a, v
                db      d_getb
a               db      v
                endm

_getw           macro   a, v
                db      d_getw
a               dw      v
                endm

_getd           macro   a, voff, vseg
                db      d_getd
a               dw      voff, vseg
                endm

_getn           macro   a, v
                db      d_getn
a               dw      v
                endm

_geta           macro   a, len
                db      d_geta
                dw      a
                db      len
                endm

_gets           macro   str
                db      d_gets
                dw      str
                endm

_yesno          macro   a, v
                db      d_yn
a               db      v
                endm

; Screen layout
POS_STACKWIN    equ     (1*160+02)
POS_WATCHWIN    equ     (1*160+22)
POS_DATAWIN     equ     (4*160+22)
POS_REGISTERS   equ     (9*160+02)
POS_VIEW        equ     (9*160+22)
POS_STATUS      equ     (24*160)
POS_RECODING    equ     (23*160+156)

VIEW_COLS       equ     68
VIEW_ROWS       equ     14

PUBPROC         macro   pname
                public  pname
                pname   label near
                endm

;;**----------------------------------------------------------------------**;;
;; KBD definitions
;;**----------------------------------------------------------------------**;;
K_RS     equ 01h
K_LS     equ 02h
K_CTL    equ 04h
K_ALT    equ 08h
K_SCRL   equ 10h
K_NUML   equ 20h
K_CAP    equ 40h
K_INS    equ 80h

K_ESC    equ 1bh
K_sESC   equ 0bh
K_BKSP   equ 08h
K_sBKSP  equ 18h
K_TAB    equ 09h
K_sTAB   equ 19h
K_CR     equ 0ah
K_sCR    equ 1ah
K_SPACE  equ 20h
K_sSPACE equ 1fh
K_DEL    equ 7fh
K_sDEL   equ 00h

K_UP     equ 80h
K_DOWN   equ 81h
K_RIGHT  equ 82h
K_LEFT   equ 83h
K_PGUP   equ 84h
K_PGDN   equ 85h
K_HOME   equ 86h
K_END    equ 87h
K_sUP    equ 88h
K_sDOWN  equ 89h
K_sRIGHT equ 8ah
K_sLEFT  equ 8bh
K_sPGUP  equ 8ch
K_sPGDN  equ 8dh
K_sHOME  equ 8eh
K_sEND   equ 8fh

K_F1    equ 90h
K_F2    equ 91h
K_F3    equ 92h
K_F4    equ 93h
K_F5    equ 94h
K_F6    equ 95h
K_F7    equ 96h
K_F8    equ 97h
K_F9    equ 98h
K_F10   equ 99h

K_sF1   equ 9Ah
K_sF2   equ 9Bh
K_sF3   equ 9Ch
K_sF4   equ 9Dh
K_sF5   equ 9Eh
K_sF6   equ 9Fh
K_sF7   equ 0A0h
K_sF8   equ 0A1h
K_sF9   equ 0A2h
K_sF10  equ 0A3h

MAX_MACRO_SIZE  equ 210
MAC_FILE_MARKER equ 5d1ah

Probe_Signature macro devAck, devRet, devBrk
        ; ----------------------------- ; -----------------------------------
        db      '$OFTPROB'              ; SoftProbe signature starts here
devAck: mov     word ptr es:[bx+3],100h ;
devRet: retf                            ;
devBrk: int     brk2Int                 ; Signature end + Int#
        ; ----------------------------- ; -----------------------------------
SoftProbe_Int     equ $-1
        retf
        endm