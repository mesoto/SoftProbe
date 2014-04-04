;;**----------------------------------------------------------------------**;;
cursor          dw  POS_STATUS, 0b000h

;;**----------------------------------------------------------------------**;;
                align word
dsp_cmdTable    dw  offset dsp_el     ;00
                dw  offset dsp_sub    ;01
                dw  offset dsp_movto  ;02
                dw  offset dsp_dup    ;03
                dw  offset dsp_clrvu  ;04
                dw  offset dsp_el     ;05 (unused)
                dw  offset dsp_string ;06 string ds:dx (fixed)
                dw  offset dsp_text   ;07 string ds:dx (variable)
                dw  offset dsp_decim  ;08 word on stack top
                dw  offset dsp_stk04  ;09 word on stack top
                dw  offset dsp_stk08  ;0a word on stack top
                dw  offset dsp_stk16  ;0b word on stack top
                dw  offset dsp_stk32  ;0c word on stack top
                dw  offset dsp_mem08  ;0d string ds:dx
                dw  offset dsp_mem16  ;0e string ds:dx
                dw  offset dsp_mem32  ;0f string ds:dx
                dw  offset dsp_ascii  ;10 string ds:dx
                dw  offset dsp_bits   ;11
                dw  offset dsp_ed1    ;12
                dw  offset dsp_yn     ;13 get yes/no
                dw  offset dsp_geta   ;14 get ascii
                dw  offset dsp_getb   ;15
                dw  offset dsp_getw   ;16
                dw  offset dsp_getn   ;17 get decimal
                dw  offset dsp_gets   ;18 get hex/ascii
                dw  offset dsp_getd   ;19
                dw  offset dsp_ed2    ;1a
                dw  offset dsp_bold   ;1b a_b
                dw  offset dsp_norm   ;1c a_n
                dw  offset dsp_rvrs   ;1d a_r
                dw  offset dsp_undr   ;1e a_h
                dw  offset dsp_fram   ;1f a_f

                ;   a_b, a_n, a_r, a_h, a_f, a_cur
Attrib          db  0fh, 07h, 70h, 0fh, 07h, 090h
mono_att        db  0fh, 07h, 70h, 0fh, 07h, 090h, 0, 0
color_att       db  1fh, 17h, 70h, 1Eh, 13h, 0cBh
.errnz  color_att - mono_att - 8

ViewLines       db  1

;PDATA_ENDS

;PCODE_SEG
;;**----------------------------------------------------------------------**;;

messag1:stosw
message:lods    byte ptr cs:[si]
        cmp     al, 20h
        jae     messag1
        mov     bl, al
        mov     bh, 0
        shl     bl, 1
        jmp     cs:dsp_cmdTable[bx]

dsp_el: jmp     si

dsp_sub:xchg    si, bp
        jmp     message

dsp_movto:
        xchg    cx, ax
        lods    byte ptr cs:[si]
        cbw
        add     di, ax
        add     di, ax
        xchg    ax, cx
        jmp     message

dsp_norm:
dsp_bold:
dsp_rvrs:
dsp_undr:
dsp_fram:
        mov     bl, al
        mov     ah, cs:Attrib[bx-a_first]
        jmp     message

PUBPROC displayMsg
        call    ClearStatusLine
        mov     di, POS_STATUS
PUBPROC display
        mov     es, cs:cursor[2]
display1:
        pop     si
        jmp     short message

dsp_clrvu:
        mov     es, cs:cursor[2]
        mov     di, POS_VIEW
        mov     bl, VIEW_ROWS
clrvuLine:
        call    ClearViewLine
        add     di, 160
        dec     bx
        jnz     clrvuLine
        mov     di, POS_VIEW
        mov     cs:ViewLines, 1
        jmp     message

dsp_dup:lods    byte ptr cs:[si]
        mov     cl, al
        mov     ch, 0
        lods    byte ptr cs:[si]
        rep     stosw
        jmp     message

dsp_geta:
        push    bx
        push    si
        push    di
        push    dx
        mov     dx, cs:[si]
        inc     si
        inc     si
        call    disp_string
        mov     al, ' '
        rep     stosw
        pop     dx
        jmp     message

dsp_gets:
        push    bx
        push    si
        push    di
        call    disp_gets
        inc     si
        inc     si
        jmp     message

dsp_string:
        call    disp_string
        mov     al, ' '
        rep     stosw
        jmp     message

dsp_text:
        call    disp_string
        jmp     message

dsp_getw:
        push    bx
        push    si
        push    di
        call    hex_16_si
        jmp     message

dsp_getd:
        mov     bl, 2*d_getw
        push    bx
        inc     si                      ;segment first
        inc     si
        push    si
        push    di
        call    hex_16_si
        sub     si, 4                   ;back to offset
        mov     al,':'
        stosw
        push    bx
        push    si
        push    di
        call    hex_16_si
        inc     si
        inc     si
        jmp     message

dsp_stk32:
        pop     cx
        call    hex_16
        mov     al,':'
        stosw
dsp_stk16:
        pop     cx
        call    hex_16
        jmp     message

dsp_getb:
        push    bx
        push    si
        push    di
        lods    byte ptr cs:[si]
        push    ax
dsp_stk08:
        pop     cx
        call    hex_08
        jmp     message

dsp_stk04:
        pop     cx
        call    hex_04
        jmp     message

dsp_getn:
        push    bx
        push    si
        push    di
        push    cs:[si]
        inc     si
        inc     si
dsp_decim:
        pop     cx
        push    dx
        xchg    ax, cx
        mov     bx, 10
        add     di, bx

        ; ax=number, ch=attr
        mov     cl, 5
dispnum1:
        xor     dx, dx
        div     bx
        add     dl, '0'
        mov     dh, ch
        dec     di
        dec     di
        mov     es:[di], dx
        dec     cl
        jnz     dispnum1

        add     di, bx
        xchg    ax, cx                  ;restore attr
        pop     dx
        jmp     message

        ; ds:dx=ptr data
dsp_mem08:
        lods    byte ptr cs:[si] ;length
        mov     bl, al
        xchg    si, dx
dsp_mm081:
        lodsb
        mov     cl, al
        call    hex_08
        mov     al, ' '
        stosw
        dec     bx
        jnz     dsp_mm081
        xchg    si, dx
        jmp     message

        ; ds:dx=ptr data
dsp_mem16:
        lods    byte ptr cs:[si] ;#words
        mov     bl, al
        xchg    si, dx
dsp_mm161:
        call    hex_16_si
        mov     al, ' '
        stosw
        dec     bx
        jnz     dsp_mm161
        xchg    si, dx
        jmp     message

        ; ds:dx=ptr data
dsp_mem32:
        lods    byte ptr cs:[si] ;#words
        mov     bl, al
        xchg    si, dx
dsp_mm321:
        mov     cl, ds:[si+2]
        mov     ch, ds:[si+3]
        call    hex_16
        call    hex_16_si
        inc     si
        inc     si
        mov     al, ' '
        stosw
        stosw
        dec     bx
        jnz     dsp_mm321
        xchg    si, dx
        jmp     message

        ; ds:dx=ptr data
dsp_ascii:
        mov     ch, cs:[si]     ;length
        inc     si
        xchg    si, dx
dsp_asci1:
        lodsb
        test    cs:iflag, F_JFILTER
        jnz     short dsp_asci3
        cmp     al, 20h
        jl      short dsp_asci2
        cmp     al, 7fh
        jne     short dsp_asci3
dsp_asci2:
        mov     al, '.'
dsp_asci3:
        stosw
        dec     ch
        jne     dsp_asci1
        xchg    si, dx
        jmp     message

dsp_yn: push    bx
        push    si
        push    di
        lods    byte ptr cs:[si]
        stosw
        jmp     message

dsp_ed1:mov     bp, sp
;       DBGBRK
        jmp     message

dsp_bits:
        ; DS=CS
        pop     bx              ; attr mask
        pop     cx              ; flag values
        push    dx
        push    bp
        lodsw                   ; mask of bits
        xchg    dx, ax
dspBit1:shl     dx, 1
        jnc     dspBit2
        shl     cx, 1
        sbb     bp, bp
        mov     al, [si+bp+1]
        inc     si
        inc     si
        shl     bx, 1
        sbb     bp, bp
        mov     ah, Attrib[bp+1+a_b-a_first]  ; a_n(0) or a_b(1)
        stosw
        jmp     dspBit1
dspBit2:shl     cx, 1
        shl     bx, 1
        test    dx, dx
        jnz     dspBit1
        pop     bp
        pop     dx
        jmp     message

PUBPROC More
        call    displayMsg
        db      ' Press a key to continue, or ESC to cancel.',d_el
        mov     cs:cursor, POS_STATUS+158
        mov     byte ptr es:[POS_STATUS+158],19h
        call    readkey
        mov     byte ptr es:[POS_STATUS+158],0f0h
        cmp     al, K_ESC
        je      Abort
        xchg    bx, ax                  ; return character

PUBPROC ClearStatusLine
        mov     es, cs:cursor[2]
        mov     di, POS_STATUS
        mov     cx, 79
        mov     ah, cs:Attrib[a_r-a_first]
        mov     al, ' '
        rep     stosw
        ret

Abort:  call    ClearStatusLine
        jmp     MenuCommand

        even
getFunc dw      getYesNo, editText, editHexb, editHexw, editDeci, editStr
getPosn dw      0,        0,        2,        6,        8,        100

dsp_ed2:push    si              ;return address
        push    di              ;end of line
        push    cs
        pop     ds
        mov     dx, bp          ;dx points to past first field
getDta0:sub     bp, 6
getDta1:xor     cx,cx
        mov     ax, 0[bp+0]     ;position
        mov     si, 2[bp+0]     ;offset data area
        mov     bx, 4[bp+0]     ;type of field
        add     ax, getPosn[bx-2*d_yn]
        mov     cursor, ax      ;set cursor position
getDta2:call    readkey         ;read a character
        cmp     al,K_ESC
        je      Abort
        mov     si, 2[bp+0]     ;offset data area
        mov     di, 0[bp+0]     ;position
        mov     bx, 4[bp+0]     ;type also field len*2+d_yn*2
        call    getFunc[bx-2*d_yn]
        cmp     al,K_CR
        je      getDta4
        cmp     al,K_SPACE
        je      getDta3
        cmp     al,K_TAB
        je      getDta3
        cmp     al,K_sTAB
        jne     getDta2
getDta5:add     bp, 6
        cmp     bp, dx
        jb      getDta1
getDta3:sub     bp, 6
        cmp     bp, sp
        ja      getDta1
        mov     bp, dx          ;bp points to past first field
        jmp     getDta0
getDta4:pop     di
        pop     si
        mov     sp, dx
        jmp     si

getYesNo:
        call    toUpper
        cmp     al, 'Y'
        je      yes_no2
        cmp     al, 'N'
        je      yes_no2
        cmp     al, '+'
        je      yes_no1
        cmp     al, '-'
        jne     yes_no3
yes_no1:mov     al, 'Y' xor 'N'
        xor     al, [si]
yes_no2:mov     [si], al
        stosb
yes_no3:ret

editStr:cmp     al, K_HOME
        je      edtStr1
        cmp     al, K_END
        je      edtStr2
        cmp     al, K_RIGHT
        je      edtStr6
        cmp     al, K_LEFT
        je      edtStr3
        cmp     al, K_TAB
        je      edtStr8
        cmp     al, K_sTAB
        je      edtStr8
        cmp     al, K_UP
        je      edtStru
        cmp     al, K_DOWN
        je      edtStrd
        mov     bl, ch          ;index
        mov     si, [si]        ;ptr buffer
        add     si, bx
        shl     bx, 1
        add     di, bx
        cmp     getPosn[2*d_gets-2*d_yn], 100
        jae     edtStr5
        cmp     al, K_SPACE
        je      edtStr6
        add     di, bx          ;+4*col
        add     di, bx          ;+6*col
        call    editHexb
        jc      edtStr4
        dec     ch
        jmp     short edtStr9

edtStr1:mov     ch, 0           ;HOME
        jmp     short edtStr0
edtStr2:mov     ch, 0fh         ;END
        jmp     short edtStr0
edtStr3:dec     ch              ;LEFT
        jns     edtStr0
        mov     ch, 0
edtStru:mov     ax, 0ff00h+K_CR
        ret
edtStrd:mov     ax, 0fe00h+K_CR ;next line command
edtStr4:ret

edtStr8:xor     getPosn[2*d_gets-2*d_yn], 102 ;switch hex/ascii
edtStr0:mov     cl, 0
        jmp     short edtStr7
edtStr6:mov     cl, 0           ;RIGHT
        jmp     short edtStra

edtStr5:cmp     al, 20h
        jl      edtStr4
        mov     [si], al
edtStr9:mov     si, 2[bp+0]     ;offset data area
        mov     di, 0[bp+0]     ;position
        mov     ax, es:[di]     ;use previous attrib
        push    cx
        call    disp_gets
        pop     cx
edtStra:inc     ch
        cmp     ch, 16          ;last column?
        jae     edtStrd
edtStr7:mov     bl, 50
        add     bl, ch
        cmp     getPosn[2*d_gets-2*d_yn], 100
        jae     editCrs
        sub     bl, 49
        add     bl, ch          ;*2
        add     bl, ch          ;*3
        jmp     short editCrs

editText:
        mov     ch, [si+2]      ;max length
editTxt:mov     bl, cl          ;index
        mov     si, [si]        ;ptr buffer
        cmp     al, K_HOME
        je      editT08
        cmp     al, K_END
        je      editT09
        cmp     al, K_RIGHT
        je      editT06
        cmp     al, K_LEFT
        je      editT07
        cmp     al, K_BKSP
        je      editT10
        cmp     al, K_DEL
        je      editT11
        cmp     al, 20h
        jl      editT05
        cmp     cl, ch          ;max length
        jae     editT05
        inc     cx              ;next col
        test    ah, K_INS       ;insert mode on?
        jz      editT01
editT00:xchg    al, [si+bx]
        inc     bx
        cmp     bl, ch
        jb      editT00
        mov     al, 0           ;'\0'
editT01:mov     [si+bx], al
editT02:lodsb
        cmp     al, 0
        jnz     editT03
        dec     si
        mov     al, ' '
editT03:stosb
        inc     di              ;skip attr
        dec     ch
        jnz     editT02
editT04:mov     bl, cl
editCrs:mov     al, 0           ;stay on the same field
        shl     bl, 1
        add     bx, 0[bp+0]     ;position
        mov     cursor, bx
editT05:ret

editT06:call    strLen          ;K_RIGHT
        xchg    bl, cl          ;
        cmp     cl, bl          ;max length
        adc     cl, bh          ;next col if cl<len
        jmp     editT04
editT09:call    strLen          ;K_END
        jmp     editT04
editT07:dec     cl              ;K_LEFT
        jns     editT04
editT08:mov     cl, 0           ;K_HOME
        jmp     editT04
editT10:dec     bl              ;K_BKSP
        js      editT04
        mov     cl, bl
editT11:mov     al, [si+bx+1]   ;K_DEL
        mov     [si+bx], al
        inc     bx
        cmp     bl, ch
        jb      editT11
        jmp     editT02

editHexb:
        cmp     al, '+'
        je      editHb3
        cmp     al, '-'
        je      editHb4
        call    asc2hex
        jc      editHb5
        test    cl, cl
        jnz     editHb1
        mov     [si], cl
editHb1:mov     cl, 4
        shl     byte ptr [si], cl
        or      [si], al
editHb2:mov     al, [si]
        call    wrthxb
        mov     al, 0
editHb5:ret
editHb3:inc     byte ptr [si]
        jmp     editHb2
editHb4:dec     byte ptr [si]
        jmp     editHb2

editHexw:
        cmp     al, '+'
        je      editHw3
        cmp     al, '-'
        je      editHw4
        call    asc2hex
        jc      editHb5
        inc     cx
        loop    editHw1
        mov     [si], cx
editHw1:mov     cl, 4
        shl     word ptr [si], cl
        or      [si], al
editHw2:mov     ax, [si]
        call    wrthxw
        mov     al, 0
        ret
editHw3:inc     word ptr [si]
        jmp     editHw2
editHw4:dec     word ptr [si]
        jmp     editHw2

editDeci:
        push    dx
        cmp     al, '+'
        je      editDc3
        cmp     al, '-'
        je      editDc4
        cmp     al, K_BKSP
        je      editDc7
        cmp     al, '0'
        jb      editDc5
        cmp     al, '9'+1
        jae     editDc5
        and     ax, 0fh
        jcxz    editDc1
        xchg    bx, ax
        mov     ax, [si]
        xor     dx, dx
        mov     cx, 10000
        div     cx
        mov     al, 10
        mul     dx
        add     ax, bx
editDc1:mov     [si], ax
editDc2:mov     ax, [si]
        mov     bx, 8
        mov     cx, 10
editDc6:xor     dx, dx
        div     cx
        add     dl, '0'
        mov     es:[di+bx], dl
        dec     bx
        dec     bx
        jns     editDc6
editDc5:pop     dx
        ret
editDc3:inc     word ptr [si]
        jmp     editDc2
editDc4:dec     word ptr [si]
        jmp     editDc2
editDc7:mov     ax, [si]
        xor     dx, dx
        mov     cx, 10
        div     cx
        jmp     editDc1

disp_gets:
        push    dx
        push    si
        mov     dx, [si]
        call    display
        db      d_mem08,16,d_el
        sub     dx, 16
        call    display
        db      '³ ',d_ascii,16,d_el
        pop     si
        pop     dx
        ret

hex_16_si:
        mov     cl, ds:[si]             ;byte access avoids end of seg fault
        inc     si
        mov     ch, ds:[si]
        inc     si
; CX/CL = hex value, ah = attribute
hex_16: call    hex_ch
hex_ch: xchg    cl, ch
hex_08: mov     al, cl
        shr     al, 1
        shr     al, 1
        shr     al, 1
        shr     al, 1
        cmp     al, 10
        cmc
        adc     al, '0'
        daa
        stosw
hex_04: mov     al, cl
        and     al, 0fh
        cmp     al, 10
        cmc
        adc     al, '0'
        daa
        stosw
        ret

disp_string:
        lods    byte ptr cs:[si] ;max len
        mov     cl, al
        mov     ch, 0
        xchg    si, dx
dsp_strng1:
        lodsb
        cmp     al, 0
        je      dsp_strng2
        stosw
        loop    dsp_strng1
dsp_strng2:
        xchg    si, dx
        ret

wrthxw: push    ax
        mov     al,ah
        call    wrthxb
        pop     ax
wrthxb: push    ax
        shr     al,1
        shr     al,1
        shr     al,1
        shr     al,1
        call    wrthxd
        pop     ax
wrthxd: and     al,0fh
        cmp     al,10
        cmc
        adc     al,'0'
        daa
        stosb
        inc     di
        ret

asc2hex:cmp     al,'0'
        jb      aschex3
        cmp     al,'9'
        jbe     aschex2
        cmp     al,'a'
        jb      aschex1
        cmp     al,'z'
        ja      aschex3
        and     al,0dfh
aschex1:cmp     al,'A'
        jb      aschex3
        cmp     al,'F'
        ja      aschex3
        sub     al,7
aschex2:and     al,15
        ret
aschex3:stc
        ret

PUBPROC SwitchScreen
        mov     bx, 8
        xor     byte ptr cursor[3], bl
        xor     byte ptr scstart[1], bl
        and     bl, byte ptr cursor[3]
        lea     si, [bx+mono_att]
        push    ds
        pop     es
        mov     di, offset Attrib
        movsw
        movsw
        movsw
        ret

PUBPROC XchScreen
        xor     iflag, F_USRSCR
        mov     es, cursor[2]
        xor     di, di
        mov     cx, 2000
xchscr0:mov     ax, es:[di]
        xchg    ax, savedSCREEN[di]
        stosw
        loop    xchscr0
        ret

PUBPROC scrollLines
        pop     dx
        mov     di, POS_VIEW
        call    display
        db      a_r,d_string,68,d_el
        ; dx = callback address
        ; bx = 0
        push    bx
        dec     bx                      ;max records to skip (-1)
        call    ScrClient               ;always fails
        sub     cx, VIEW_ROWS - 1
        jb      scrollHome
        pop     bx
        push    cx                      ;cx=total number of records
        call    displayMsg
        db      ' Use key pad to scroll, or ESC to cancel.',d_el
        jmp     short scrollHome

scrollPgDn:
        add     si, VIEW_ROWS - 1
        cmp     si, cx
        jb      scrollLine0
scrollEnd:
        mov     si, cx                  ;total #records
        jmp     short scrollline0
scrollDown:
        inc     si
        cmp     si, cx
        jbe     scrollLine0
scrollUp:
        dec     si
        js      scrollHome
        jmp     short scrollline0
scrollPgUp:
        sub     si, VIEW_ROWS - 1
        jnc     scrollLine0
scrollHome:
        xor     si, si
scrollLine0:
        mov     bx, si
        mov     di, POS_VIEW + 160
scrollLine1:
        call    ScrClientLine
;       jc      scrollKey
        inc     bx                      ; next logical record
        add     di, 160
        cmp     di, POS_VIEW + 160*VIEW_ROWS
        jb      scrollLine1

scrollKey:
        pop     cx                      ;max #records
        jcxz    scrollDone
        call    readkey
        push    cx
        cmp     al, K_DOWN
        je      scrollDown
        cmp     al, K_UP
        je      scrollUp
        cmp     al, K_END
        je      scrollEnd
        cmp     al, K_HOME
        je      scrollHome
        cmp     al, K_PGUP
        je      scrollPgUp
        cmp     al, K_PGDN
        je      scrollPgDn

        push    ax
        call    ClearStatusLine
        pop     ax
        jmp     ExecCommand


scrollDone:
        jmp     Abort

ScrClientLine:
        call    ClearViewLine
ScrClient:
        ; In:   bx=record #
        ; Out:
        push    es
        push    di
        push    si                      ;si=index of line 1
        push    bp                      ;bp=context
        push    bx                      ;logical record no.
        push    dx
        call    dx
        pop     dx
        pop     bx
        pop     bp
        pop     si
        pop     di
        pop     es
        ; CF=1 if record out of range
        ret

ClearViewLine:
        mov     ah, cs:Attrib[a_n-a_first]
        mov     al, ' '
        push    di
        mov     cx, VIEW_COLS
        rep     stosw
        pop     di
        ret

PUBPROC InitScreen
        xor     di, di
PUBPROC InitScreenEsDi
        call    display1
        db      a_f
        db      'Õ',               d_dup,9,'ÍÑ',       d_dup,68,'Í¸'
        db      '³',               d_dup,9,' ³',       d_dup,68,' ³'
        db      '³',a_h,'+0 =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'+2 =',a_f,d_dup,5,' Æ',       d_dup,68,'Íµ'
        db      '³',a_h,'+4 =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'+6 =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'+8 =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'+A =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      'Æ',               d_dup,9,'ÍØ',       d_dup,68,'Íµ'
        db      '³',a_h,'AX =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'BX =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'CX =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'DX =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'BP =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'SI =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'DI =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'DS =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'ES =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'SP =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'SS =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'IP =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',a_h,'CS =',a_f,d_dup,5,' ³',       d_dup,68,' ³'
        db      '³',               d_dup,9,' ³',       d_dup,68,' ³'
        db      'ÔÍ',a_b,'F1=Help',a_f,    'ÍÏ',       d_dup,68,'Í¾'
                    ;         1         2         3         4
                    ;---------+---------+---------+---------+
        db      a_r,' SoftProbe Version '
        db      ver_str
        db      ' (C) 1995 Mehdi Sotoodeh.'
        db      '  All rights reserved.          ð',d_el
        ret

PUBPROC DisplayRegisters
        mov     dx, cs:save_sp
stackRegOff     equ $-2
        mov     ds, cs:save_ss
        push    dx
        push    ds
        mov     di, POS_STACKWIN - 160 + 4
        call    display
        db      a_h,'SS:'
stackRegName    db 'SP',d_mv,73,d_stk32,a_n,d_el
        mov     di, POS_STACKWIN + 160 + 10
        mov     si, dx
        mov     bx, 6
dispStack:
        call    hex_16_si
        add     di, 160-8
        dec     bx
        jnz     dispStack

        ; display flags
        push    cs
        pop     ds
        mov     di, POS_REGISTERS + 13*160

        mov     cx, save_fl
        push    cx              ;flags
        xor     cx, prev_fl
        push    cx              ;attr for flags
        call    display1
        ;      '....ODITSZ.A.P.C'
        _bits   0000111111010101b, 'OoDdIiTtSsZzAaPpCc'
        db      d_el

        mov     si, offset save_ax
        mov     di, POS_REGISTERS + 10
        mov     dx, 13
dispRegs:
        mov     bx, [si]
        xor     bx, [si + prev_ax - save_ax]
        neg     bx
        sbb     bx, bx
        mov     ah, Attrib[bx+1+a_b-a_first]
        call    hex_16_si
    add di, 160-8
        dec     dx
        jnz     dispregs
        ret

gethexd:push    word ptr [di+2]
        mov     si, sp          ;ptr hi-word
        push    di
        mov     di, bx
        call    gethexw
        add     di, 10
        pop     si              ;ptr lo-word
        call    gethexw
        pop     word ptr [si+2]
        ret

gethexw:push    word ptr [si]
        mov     ax, sp
        push    si
        xchg    si, ax
        lea     ax, 6[di]
        mov     cursor,ax       ;set cursor position
        xor     cx, cx
gethxw1:call    readkey         ;read a character
        cmp     al, K_CR
        je      gethxw2
        cmp     al, K_SPACE
        je      gethxw2
        cmp     al, K_UP
        je      gethxw2
        cmp     al, K_DOWN
        je      gethxw2
        cmp     al, K_ESC
        je      gethex0
        push    di
        call    editHexw
        pop     di
        jmp     gethxw1
gethxw2:pop     si
        pop     word ptr [si]
        ret

chgReg1:inc     si
        inc     si
        add     di, 160
        cmp     si, offset save_fl
        jb      chgReg2
        je      chgReg3
cmd_RegChange:
        mov     si, offset save_ax
        mov     di, POS_REGISTERS + 10
chgReg2:mov     sp, offset stackTop
        call    gethexw
        cmp     al, K_DOWN
        je      chgReg1
        cmp     al, K_UP
        jne     gethex0
        dec     si
        dec     si
        sub     di, 160
        cmp     si, offset save_ax
        jae     chgReg2
chgReg3:push    save_fl                 ; save for ESC
chgReg4:mov     bp, 1
        mov     dx, POS_REGISTERS + 13*160 + 16
chgReg5:push    dx
        call    DisplayRegisters
        pop     dx
        mov     cursor, dx
        call    readkey
        cmp     al, K_LEFT
        je      chgReg8
        cmp     al, K_RIGHT
        je      chgReg9 
        cmp     al, K_UP
        je      chgReg6 
        cmp     al, K_DOWN
        je      cmd_RegChange 
        cmp     al, '+'
        je      chgReg7
        cmp     al, '-'
        je      chgReg7
        cmp     al, K_CR
        je      gethex0
        cmp     al, K_ESC
        jne     chgReg5
        pop     save_fl
gethex0:jmp     DisplayAll
chgReg6:mov     si, offset save_cs
        mov     di, 160*21+12
        jmp     chgReg2
chgReg7:xor     save_fl, bp
chgReg8:shl     bp, 1
        jz      chgReg4
        test    bp, 0fd5h
        jz      chgReg7
        dec     dx
        dec     dx
        jmp     chgReg5
@@:     mov     dx, 22*160
chgReg9:ror     bp, 1
        jc      @b
        test    bp, 0fd5h
        jz      chgReg9
        inc     dx
        inc     dx
        jmp     chgReg5

;PCODE_ENDS

;ICODE_SEG

;END