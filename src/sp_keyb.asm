DefKey  macro k1, k2
        db    k1, k2
        endm

        align word

macroPtrs       dw offset macroBuffer+0
                dw offset macroBuffer+2
                dw offset macroBuffer+4
                dw offset macroBuffer+6
                dw offset macroBuffer+8
                dw offset macroBuffer+10
                dw offset macroBuffer+12
                dw offset macroBuffer+14
                dw offset macroBuffer+16
                dw offset macroBuffer+18
macroBufferLast dw offset macroBuffer+18
macroBuffer     dw MAX_MACRO_SIZE dup (0)
macroBufferEnd  dw 0
playbackPtr     dw offset macroBufferEnd

kbd_code equ this word          ;definition of keys
        DefKey K_ESC, K_sESC    ;001 ESC
        DefKey '1','!'          ;002 1!
        DefKey '2','@'          ;003 2@
        DefKey '3','#'          ;004 3#
        DefKey '4','$'          ;005 4$
        DefKey '5','%'          ;006 5%
        DefKey '6','^'          ;007 6^
        DefKey '7','&'          ;008 7&
        DefKey '8','*'          ;009 8*
        DefKey '9','('          ;010 9(
        DefKey '0',')'          ;011 0)
        DefKey '-','_'          ;012 -_
        DefKey '=','+'          ;013 =+ 
        DefKey K_BKSP, K_sBKSP  ;014 BackSpace
        DefKey K_TAB, K_sTAB    ;015 Tab
        DefKey 'q','Q'          ;016 Q
        DefKey 'w','W'          ;017 W
        DefKey 'e','E'          ;018 E
        DefKey 'r','R'          ;019 R
        DefKey 't','T'          ;020 T
        DefKey 'y','Y'          ;021 Y
        DefKey 'u','U'          ;022 U
        DefKey 'i','I'          ;023 I
        DefKey 'o','O'          ;024 O
        DefKey 'p','P'          ;025 P
        DefKey '[','{'          ;026 [{
        DefKey ']','}'          ;027 ]}
        DefKey K_CR, K_sCR      ;028 CR
        DefKey 0,K_CTL          ;029 CTRL
        DefKey 'a','A'          ;030 A
        DefKey 's','S'          ;031 S
        DefKey 'd','D'          ;032 D
        DefKey 'f','F'          ;033 F
        DefKey 'g','G'          ;034 G
        DefKey 'h','H'          ;035 H
        DefKey 'j','J'          ;036 J
        DefKey 'k','K'          ;037 K
        DefKey 'l','L'          ;038 L
        DefKey ';',':'          ;039 ;:
        DefKey "'",'"'          ;040 '"
        DefKey '`','~'          ;041 `~
        DefKey 0,K_LS           ;042 LSHIFT
        DefKey '\','|'          ;043 \|
        DefKey 'z','Z'          ;044 Z
        DefKey 'x','X'          ;045 X
        DefKey 'c','C'          ;046 C
        DefKey 'v','V'          ;047 V
        DefKey 'b','B'          ;048 B
        DefKey 'n','N'          ;049 N
        DefKey 'm','M'          ;050 M
        DefKey ',','<'          ;051 ,<
        DefKey '.','>'          ;052 .>
        DefKey '/','?'          ;053 /?
        DefKey 0,K_RS           ;054 RSHIFT
        DefKey 0,0              ;055 PRINT SCREEN
        DefKey 0,K_ALT          ;056 ALT
        DefKey ' ',' '          ;057 SPACE
        DefKey 0,K_CAP          ;058 CAPS LOCK
        DefKey K_F1, K_sF1      ;059 F1
        DefKey K_F2, K_sF2      ;060 F2
        DefKey K_F3, K_sF3      ;061 F3
        DefKey K_F4, K_sF4      ;062 F4
        DefKey K_F5, K_sF5      ;063 F5
        DefKey K_F6, K_sF6      ;064 F6
        DefKey K_F7, K_sF7      ;065 F7
        DefKey K_F8, K_sF8      ;066 F8
        DefKey K_F9, K_sF9      ;067 F9
        DefKey K_F10,K_sF10     ;068 F10
        DefKey 0,K_NUML         ;069 NUM LOCK
        DefKey 0,K_SCRL         ;070 SCROLL LOCK
        DefKey K_HOME,K_sHOME   ;071 HOME
        DefKey K_UP,  K_sUP     ;072 UP
        DefKey K_PGUP,K_sPGUP   ;073 PGUP
        DefKey '-', '-'         ;074 MINUS
        DefKey K_LEFT,K_sLEFT   ;075 LEFT
        DefKey 0, 0             ;076 FIVE
        DefKey K_RIGHT,K_sRIGHT ;077 RIGHT
        DefKey '+','+'          ;078 PLUS
        DefKey K_END, K_sEND    ;079 END
        DefKey K_DOWN,K_sDOWN   ;080 DOWN
        DefKey K_PGDN,K_sPGDN   ;081 PGDN
        DefKey 0, 0             ;082 INS
        DefKey K_DEL,K_sDEL     ;083 DEL

max_keys equ ($ - kbd_code)/2

;;**----------------------------------------------------------------------**;;

PUBPROC key_check
        xor     ax, ax
        xchg    al, byte ptr cs:keycode
        dec     ax                      ;1..83 ==> 0..82
        cmp     al, max_keys            ;key in the table
        jae     short KeyChk3           ;jump if unknown keys or key up
        shl     ax, 1
        xchg    bx, ax
        mov     bx, cs:kbd_code[bx]
        xchg    ax, bx

        test    al, al                  ;special key?
        jz      short KeyChk3           ;jump if special key
        ; normal key, is it shifted?
        test    Byte ptr cs:keycode[1], K_CAP ;CAPSlock on?
        jz      short KeyChk1
        cmp     al, 'a'
        jb      short KeyChk1
        cmp     al, 'z'+1
        jae     short KeyChk1
        xchg    al, ah                  ;use upper case
KeyChk1:test    Byte ptr cs:keycode[1], K_LS + K_RS
        jz      short KeyChk2
        xchg    al, ah                  ;use the shifted value
KeyChk2:mov     ah, Byte ptr cs:keycode[1]
        test    al, al
        ret
KeyChk3:xor     ax, ax
        ret

readkey:push    ds
        push    di
        lds     di, dword ptr cs:cursor
        push    ds:[di]
        mov     al, cs:Attrib[a_cur-a_first]
        mov     ds:[di+1], al
        xchg    di, cs:playbackPtr
        call    key_Normal      ; key_Normal/playback or key_Recording
keyProcAddr     label word
        xchg    di, cs:playbackPtr
        pop     word ptr ds:[di]
        pop     di
        pop     ds
        ret

stop_Recording:
        mov     word ptr cs:[keyProcAddr-2], key_Normal-keyProcAddr
; normal operation, key hit -- stop play back
key_Normal:
        call    key_check
        jnz     stop_Playback
        ; playback -- if any
        mov     ax, cs:[di]
        test    ax, ax
        jz      key_Normal
        inc     di
        inc     di
        ret
stop_Playback:
        mov     di, offset macroBufferEnd
        ret

; recording -- check for end-of-recording (~ESC)
key_Recording:
        mov     ax, 8000h+'R'
        or      ah, cs:Attrib[a_b-a_first]
        xchg    ax, word ptr ds:[POS_RECODING]
        push    ax
@@:     call    key_check
        jz      @b
        pop     word ptr ds:[POS_RECODING]
        cmp     di, offset macroBufferEnd
        jae     stop_Recording ; stop recording
        cmp     al, K_sESC
        je      stop_Recording ; stop recording
        mov     cs:[di], ax
        inc     di
        inc     di
        mov     cs:macroBufferLast, di
        mov     word ptr cs:[di], 0 ;terminator
        ret

Play_A_Macro    macro
        ; bx = 2*n (n=0-9)
        mov     ax, macroPtrs[bx]
        mov     playbackPtr, ax
        mov     word ptr ds:[keyProcAddr-2], key_Normal-keyProcAddr
        endm

Record_A_Macro  macro
        ; bx = 2*n (n=0-9)
        ; 1st - remove the recorded key strokes
        push    cs
        pop     es
        mov     dx, macroPtrs[bx]
        mov     cx, macroBufferLast
        sub     cx, dx
        shr     cx, 1
        inc     cx
        mov     si, dx
key_record1:
        lodsw
        test    ax, ax
        loopnz  key_record1
        mov     di, dx
        rep     movsw
        mov     macroPtrs[bx], si
        mov     macroBufferLast, di
        sub     si, di                  ; number of bytes removed
        stosw                           ; terminatore
        mov     cl, 10
        mov     di, offset macroPtrs
key_record2:
        cmp     [di], dx
        jb      key_record3
        sub     [di], si
key_record3:
        inc     di
        inc     di
        loop    key_record2
        mov     ax, macroPtrs[bx]
        mov     playbackPtr, ax
        mov     word ptr ds:[keyProcAddr-2], key_Recording-keyProcAddr
        endm