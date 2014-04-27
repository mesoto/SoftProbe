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

msg_Return:
        pop     dx
        push    cs
        pop     ds
        mov     ah, 9
        int     21h
        stc
        ret

do_CommandLine:
        ; In:   DS:SI=ptr command line
        push    cs
        pop     es
        mov     di, offset cmd_line
copy_CmdLine:
        lodsb
        cmp     al, 9                   ;tab?
        jne     @f
        mov     al, ' '
@@:     stosb
        cmp     al, 20h
        jae     copy_CmdLine
        push    cs
        pop     ds
        mov     byte ptr [di-1], 0

        mov     dx, offset copyright
        mov     ah, 9
        int     21h

        mov     ah,30h
        int     21h
        xchg    al, ah
        cmp     ax, 30ah                ;3.1+
        jae     @f
        call    msg_Return
        db      'DOS version 3.1 or higher needed.',13,10,36
@@:
        IFNDEF  _BOOT
        mov     DosVer, ax
        ; dos_fix1: 28 (dos 2.x), 35(3.1), 3b(4+)
        cmp     ax, 400h
        jb      @f
        mov     byte ptr [dos_fix1], 3bh
@@:     mov     ah, 52h
        int     21h
        mov     word ptr List_Ptr[0], bx
        mov     word ptr List_Ptr[2], es
        mov     ax, es:[bx-2]
        cmp     byte ptr DosVer[1], 5
        jb      @f
        push    ds
        mov     ds, ax
        mov     cl, 'D'
        xor     cl, ds:[10h]
        mov     dx, ds:[11h]
        pop     ds
        jnz     @f
        dec     dx
        dec     dx
        cmp     dx, ax
        jne     @f
        inc     ax              ;ax+1 if dos 5.0+
@@:     mov     MCB_Head, ax
        cld
find_Null:
        inc     bx
        mov     si, offset NUL_dev
        lea     di, 10[bx]
        mov     cx, 8
        repe    cmpsb
        jne     find_Null
        mov     Null_Ptr[0], bx
        mov     Null_Ptr[2], es

        xor     ax, ax                  ; int no. to check
FindMontr1:
        xor     cx, cx
        mov     es, cx
        mov     di, ax
        shl     di, 1
        shl     di, 1
        les     bx, dword ptr es:[di]
        mov     si, offset monSign
        mov     di, si
        mov     cl, montdev - monSign + 1
        repe    cmpsb
        jne     FindMontr2
        scasb                           ; int vector used (60h by default)
        jne     FindMontr2
        mov     PrevMonSeg, es
        jmp     short FindMontr3
FindMontr2:
        inc     al                      ; next int
        jnz     FindMontr1
FindMontr3:
        ENDIF

        ; parse command line now.
        mov     si, offset cmd_line
getOptions:
        call    parse_line
        CmdDef  '/'     _GOTOADR, getOption1
        CmdDef  '-'     _GOTOADR, getOption1
        db 0
        lodsb
        cmp     al, 20h
        jb      ok_2install

badOption:
        call    msg_Return
        db      'Command line error.',13,10
        db      'Use /HELP option for list of commands.',13,10,36

ok_2install:
IFNDEF  _BOOT
        mov     cx, PrevMonSeg
        jcxz    new_install

        mov     dx, is_InstMsg
        test    dx, dx
        jz      @f
        mov     ah, 9
        int     21h             ;
@@:     stc
        ret

new_install:
        mov     ah, 34h
        int     21h             ;get InDos flag ptr
        mov     InDosPtr[0], bx
        mov     InDosPtr[2], es
ENDIF
        clc
        ret

getOption1:
        call    parse_line
        CmdDef  'MLOAD' _GOTOADR, cmdMacroLoad
        CmdDef  'ML'    _GOTOADR, cmdMacroLoad
IFDEF   _BOOT
        CmdDef  'CD'    _GOTOADR, set_MediaCD
ELSE
        CmdDef  'MSAVE' _GOTOADR, cmdMacroSave
        CmdDef  'MS'    _GOTOADR, cmdMacroSave
        CmdDef  'LOAD'  _GOTOADR, cmdLoadWMem
        CmdDef  'L'     _GOTOADR, cmdLoadWMem
        CmdDef  'SAVE'  _GOTOADR, cmdSaveWMem
        CmdDef  'S'     _GOTOADR, cmdSaveWMem
ENDIF
        CmdDef  'MEM'   _GOTOADR, set_WMemSize
        CmdDef  'M'     _GOTOADR, set_WMemSize
        CmdDef  'INT'   _GOTOADR, set_MonInt
        CmdDef  'I'     _GOTOADR, set_MonInt
        CmdDef  'PRN'   _GOTOADR, set_PrtPort
        CmdDef  'P'     _GOTOADR, set_PrtPort
        CmdDef  'HELP'  _GOTOADR, cmdLine_Help
        CmdDef  'H'     _GOTOADR, cmdLine_Help
        CmdDef  '?'     _GOTOADR, cmdLine_Help
        db      0
badOption1:
        jmp     badOption

bad_memSize:
        call    msg_Return
        db      'Invaild size for working memory.',13,10,36

IFDEF   _BOOT
set_MediaCD:
        mov     byte ptr boot_media_type, 1  ; 0=FD, 1=CD
        jmp     getOptions
ENDIF

set_WMemSize:
        call    skipBlanks
        cmp     al, '='
        jne     badOption1
        inc     si
        call    getHexWord
        jc      badOption1
        mov     workingMemLen, ax
        mov     cx, cs
        add     cx, resident_Size
        add     ax, cx
        jc      bad_memSize
        jmp     getOptions

set_PrtPort:
        call    skipBlanks
        cmp     al, '='
        jne     badOption1
        inc     si
        call    getHexWord
        jc      badOption1
        dec     ax
        cmp     ax, 4
        jae     badOption1
        mov     byte ptr ds:[printPort], al
        jmp     getOptions

set_MonInt:
        call    skipBlanks
        cmp     al, '='
        jne     badOption1
        inc     si
        call    getHexWord
        jc      badOption1
        test    ah, ah
        jnz     badOption1
IFNDEF  _BOOT
        mov     byte ptr ds:[SoftProbe_Int], al ; change int 60h -> int xx
ENDIF
        mov     byte ptr ds:[fix_Break2B1], al
        mov     byte ptr ds:[fix_Break2B2], al
        mov     byte ptr ds:[fix_Break2B3], al
        shl     ax, 1
        shl     ax, 1
        mov     word ptr ds:[fix_Break2W1], ax
        mov     word ptr ds:[fix_Break2W2], ax
        mov     word ptr ds:[fix_Break2W3], ax
        mov     word ptr ds:[fix_Break2W4], ax
        add     ax, 2
        mov     word ptr ds:[fix_Break2S1], ax
        mov     word ptr ds:[fix_Break2S2], ax
        mov     word ptr ds:[fix_Break2S3], ax
        jmp     getOptions

Macro_File_Bad:
        call    msg_Return
        db      13,10,'Macro file invalid.',13,10,36

cmdMacroLoad:
        call    getFileName
        mov     ax, 3d00h
        int     21h
        jc      File_Read_Error
        xchg    bx, ax
        mov     dx, offset savedSCREEN+6
        mov     cx, 2*MAX_MACRO_SIZE + 22 + 6 + 2
        mov     ah, 3fh
        int     21h
        jc      File_Read_Error
        push    ax
        mov     ah, 3eh
        int     21h
        pop     ax
        cmp     ax, 2*MAX_MACRO_SIZE + 22 + 6
        jne     Macro_File_Bad
        mov     bp, si                  ; save SI
        mov     si, offset savedSCREEN+6
        lodsw
        cmp     ax, MAC_FILE_MARKER
        jne     Macro_File_Bad
        lodsw
        cmp     ax, 2*MAX_MACRO_SIZE
        jne     Macro_File_Bad

        push    cs
        pop     es
        mov     di, offset macroPtrs    ; start of macro buffer
IFNDEF  _BOOT
        mov     cx, PrevMonSeg
        jcxz    @f
        mov     es, cx
        mov     di, es:macroPointers
@@:     
ENDIF
        mov     cx, 11                  ; include macroBufferLast
        lodsw
        sub     ax, di
        xchg    dx, ax                  ; fix-up
@@:     lodsw
        sub     ax, dx
        stosw
        loop    @b
        mov     cx, MAX_MACRO_SIZE
        rep     movsw
        mov     si, bp
IFNDEF  _BOOT
        mov     is_InstMsg, 0           ; no "is installed" msg
ENDIF
        jmp     getOptions

File_Read_Error:
        call    msg_Return
        db      13,10,'Error Reading File.',13,10,36

IFNDEF  _BOOT
cmdMacroSave:
        push    cs
        pop     es
        push    si
        mov     si, offset macroPtrs    ; start of macro buffer
        mov     cx, PrevMonSeg
        jcxz    @f
        mov     ds, cx
        mov     si, macroPointers
@@:     mov     di, offset savedSCREEN+6
        mov     cx, 2*MAX_MACRO_SIZE + 22
        rep     movsw
        pop     si
        push    cs
        pop     ds
        call    getFileName
        mov     ax, 3c00h
        int     21h
        jc      File_IO_Error
        xchg    bx, ax
        mov     cx, 2*MAX_MACRO_SIZE + 22 + 6
        mov     dx, offset savedSCREEN
        mov     ah, 40h
        int     21h
        jc      File_IO_Error
        cmp     ax, cx
        jne     File_IO_Error
        mov     ah, 3eh
        int     21h
        mov     is_InstMsg, 0           ; no "is installed" msg
        jmp     getOptions

WMemTotal       dw 0,0

No_File_Now:
        call    msg_Return
        db      13,10,'LOAD/SAVE commands are available when SoftProbe is already loaded.'
        db      13,10,36

File_IO_Error:
        call    msg_Return
        db      13,10,'File I/O Error.',13,10,36

File_Io_Chk:
        pop     di
        mov     cx, PrevMonSeg
        jcxz    No_File_Now
        call    getFileName
        mov     ax, [di+0]              ; 3d00 (load) 3c00 (save)
        int     21h
        jc      File_IO_Error
        xchg    bx, ax
        xor     ax, ax
        mov     WMemTotal[0], ax
        mov     WMemTotal[2], ax
        mov     ds, PrevMonSeg
        mov     bp, workingMemLen       ; length in paragraphs
        mov     ds, workingMemSeg
        ; bp, si, di and ds are expected to be preserved during dos calls
WMem_IO:mov     cx, 8000h
        cmp     bp, 800h
        jae     @f
        mov     cl, 4
        mov     ax, bp
        shl     ax, cl
        xchg    cx, ax
@@:     xor     dx, dx
        mov     ah, cs:[di+2]           ; 3f (load) 40 (save)
        int     21h
        jc      File_IO_Error
        add     cs:WMemTotal[0], ax
        adc     cs:WMemTotal[2], 0
        cmp     ax, 8000h
        jne     WMem_Io_Done
        mov     ax, ds
        add     ax, 800h
        mov     ds, ax
        sub     bp, 800h
        jmp     WMem_IO
WMem_Io_Done:
        mov     ah, 3eh
        int     21h
        push    cs
        pop     es
        push    cs
        pop     ds
        add     di, 3
        push    di
        mov     ax, WMemTotal[2]
        call    hexw
        mov     ax, WMemTotal[0]
        call    hexw
        jmp     msg_Return

cmdLoadWMem:
        call    File_Io_Chk
        dw      3d00h
        db      3fh
        db      '00000000 bytes loaded.',13,10,36

cmdSaveWMem:
        call    File_Io_Chk
        dw      3c00h
        db      40h
        db      '00000000 bytes saved.',13,10,36

ENDIF

getFileName:
        call    skipBlanks
        mov     dx, si
        xor     cx, cx
getFileNam1:
        lodsb
        cmp     al, 21h
        jae     getFileNam1
        mov     [si-1], cl
        ret

cmdLine_Help:
        call    msg_Return
 db 13,10,'/MEM=<n>           Set size of working memory (in paragraphs).'
 db 13,10,'/INT=<n>           Use INT n for alternative break.'
 db 13,10,'/PRN=<n>           Select LPTn as printer port.'
 db 13,10,'/MLOAD <filename>  Load macros from filename.'
IFDEF  _BOOT
 db 13,10,'/CD                Create CD boot image (default is floppy).'
ELSE
 db 13,10,'/MSAVE <filename>  Save macros into filename.'
 db 13,10,'/LOAD  <filename>  Load filename into working memory.'
 db 13,10,'/SAVE  <filename>  Save contents of working memory into filename.'
ENDIF
 db 13,10,'/HELP              For this help screen.'
 db 13,10,'/?                 For this help screen.'
 db 13,10
 db 13,10,36