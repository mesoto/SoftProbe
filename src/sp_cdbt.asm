
;----------------------------------------------------------------------------
;- CD boot start (loaded at 0000:7c00) --------------------------------------
;----------------------------------------------------------------------------

cd_stack_layout     struc
                                ; ---------------------- DAP start ----------
cd_dap_len          dw 0        ; 16 expected
cd_sectors2read     dw 0        ; 0-127 expected
cd_disk_read_addr   dd 0        ; seg:off
cd_dap_LBA          dw 0,0,0,0  ; 64-bit sector number
                                ; ---------------------- DAP end ------------
cd_diskNumber       db 0
cd_disk_retry       db 0
cd_stack_layout     ends

cd_local_stack_size equ 20

        even
; _____________________________________________________________________________
cd_start_of_boot_sector:    
        jmp short cd_boot_start
        nop
        db 0,0,0,0,0
        ; offset 8, len=56 filled by mkisofs -boot-info-table
        ; El Torito boot info table
        db 56 dup(0) 
            
        db "SoftProbe V2.1"

cd_boot_start:
        xor     ax,ax
        mov     bp,7c00h - cd_local_stack_size  ;origin at 7C00
        cli
        mov     ss,ax
        mov     sp,bp
        mov     [bp].cd_diskNumber,dl   ;save disk number
        
        ; emulate a breakpoint INT at 0000:7c00-2
        pushf
        push    ax
        mov     ah, 7ch
        push    ax
        sti

        ; save registers used for the real boot
        push    bx
        push    cx
        push    dx
        cld
        push    cs
        pop     ds
        call    cd_display_string
        copyrightMsg 'SoftProbe'
        db      'Loading (drive=',0
        mov     al, [bp].cd_diskNumber
        call    cd_p_hex_b
        call    cd_display_string
        db      ') ....',0
        
if DBG
        mov     si,bp
        call    cd_dump_address
        mov     si,7c00h
        call    cd_dump_address
        mov     si,7c00h+800h
        call    cd_dump_address
        mov     si,7c00h+0c00h
        call    cd_dump_address
        mov     si,sp
        call    cd_dump_address
endif
        ; check for '$OFTPROB' signature at offset 16
        cmp     word ptr ss:[7c00h+800h+10],4F24h
        jz      cd_all_sectors_loaded
        
        call    cd_display_string
        db      13,10,'Incomplete boot. Cannot continue!'
        db      13,10,'Press a key to reboot.',0
        xor     ax,ax
        int     16h             ;wait for keypress
        int     19h             ;re-boot
        
cd_all_sectors_loaded:
        mov     si, 840h   ; source segment (7C00 + 800 / 16)

cd_run_boot_probe:
        xor     ax, ax
        mov     ds, ax
        sub     word ptr ds:[413h], 4444h ;reduce top memory
cd_alloc_KBytes equ $-2
        mov     ax, word ptr ds:[413h]
        mov     cl, 6
        shl     ax, cl
        mov     es, ax
        xor     di, di
        mov     ds, si
        xor     si, si
        mov     cx, (boot_FileEnd - boot_FileBeg + 1)/2
        rep     movsw
        
        mov     bx, offset Install
        push    ax                      ;address to jump to (es:Install)
        push    bx
        retf                            ;jump to Install

ifdef Z_Z_Z_Z
load_the_sectors:      
        xor     ax, ax
        mov     dl, [bp].diskNumber11
        int     13h
        
        mov     dl, [bp].diskNumber
        mov     bx, 55AAh
        mov     ah, 41h                 ; check for disk extension
        int     13h
        jc      no_disk_extension
        cmp     bx, 0AA55h
        jnz     no_disk_extension
        ; CX & 01 = Drive access using the packet structure
        ; CX & 02 = Drive locking and ejecting
        ; CX & 04 = Enhanced disk drive (EDD) support
        shr     cx, 1
        jnc     no_disk_extension
        

        call    @f
        dw      16          ; length of DAP
        dw      (boot_image_size+2047)/2048
        dw      0,1000h     ; buffer address
        dd      27,0        ; 64-bit LBA
@@:     pop     si

        call    read_sector_ex

        mov     si, 1000h   ; source segment
        jmp     run_boot_probe

no_disk_extension:
        call    display_string
        db      13,10,'Disk extention not supported.'
        db      13,10,'Press a key to reboot.',0
        xor     ax,ax
        int     16h             ;wait for keypress
        int     19h             ;re-boot

        ; load softprobe at 1000h:0 to make sure no 64K crossing
        mov     [bp].dap_len, 16
        mov     word ptr [bp+0].disk_read_addr, ax
        mov     word ptr [bp+2].disk_read_addr, 1000h
        mov     [bp].sectors2read, (boot_image_size+2047)/2048
        mov     word ptr [bp+0].dap_LBA, 27 ; volume descriptor
        mov     word ptr [bp+2].dap_LBA, ax
        mov     word ptr [bp+4].dap_LBA, ax
        mov     word ptr [bp+6].dap_LBA, ax
; In:   BP
; ---------------------------------------------------------------------------
read_sector_ex:
        mov     [bp].disk_retry, 10
read_sector_loop:
        push    bp
        ;;mov     si, bp
        mov     dl, [bp].diskNumber
        mov     ah, 42h         ; extended 
        int     13h
        jnc     func_return
        xor     ax, ax
        int     13h
        dec     [bp].disk_retry
        jnz     read_sector_loop

        call    display_string
        db      13,10,'Disk read error.'
        db      13,10,'Press a key to reboot.',0
        xor     ax,ax
        int     16h             ;wait for keypress
        int     19h             ;re-boot
endif
        
cd_disp_char:
        mov     ah,0eh
        mov     bx,7
        int     10h
cd_display_string: 
        pop     si
        lods    byte ptr cs:[si]
        push    si
        or      al,al
        jnz     cd_disp_char
cd_func_return:
        ret

; In:   si = address to dump (16 bytes)
cd_dump_address:
        push    si
        call    cd_display_string
        db      13,10,0
        pop     si
        mov     ax,si
        call    cd_p_hex_w
        mov     al, ':'
        call    cd_p_hex_8b
        mov     al, ' '
cd_p_hex_8b:
        call    cd_display_char
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        call    cd_p_hex_nb
        
cd_p_hex_nb:
        mov     al, ' '
        call    cd_display_char
        lods    byte ptr ss:[si]   
        jmp     cd_p_hex_b
        
cd_p_hex_w:
        push    ax
        xchg    al,ah
        call    cd_p_hex_b
        pop     ax
cd_p_hex_b:  
        push    ax
        shr     al,1
        shr     al,1
        shr     al,1
        shr     al,1
        call    cd_p_hex_d
        pop     ax
        and     al,0fh
cd_p_hex_d:
        cmp     al,10
        cmc
        adc     al,'0'
        daa

cd_display_char:
        mov     ah,0eh
        mov     bx,7
        int     10h
        ret
        
        db      (cd_start_of_boot_sector+2046-$) dup(0)    ; CD sectors are 2048 bytes
        dw      0AA55h                  ; BOOT_SIGNATURE
        
;----------------------------------------------------------------------------
; Start of MKSPCDBT.EXE : Create a CD boot record....
;----------------------------------------------------------------------------
cd_image_FileName  db 'CD_BOOT.DAT',0

cd_file_write   macro data_off, data_size
        mov     dx, offset data_off
        mov     cx, data_size
        call    cd_write_image_file
        endm

cd_create_boot_image:
        ; create CD boot image

        mov     dx, offset cd_image_FileName
        xor     cx, cx                  ;normal
        mov     ah, 3ch                 ;create
        int     21h
        jc      cd_image_create_error
        xchg    bx, ax

        cd_file_write  cd_start_of_boot_sector,2048
        cd_file_write  boot_FileBeg,boot_image_size

        ; pad for sector size
        mov     dx, offset fill_buff
        mov     ax, word ptr [image_offset]
        and     ah, 7                   ;ax & 7ff
        mov     cx, 800h
        sub     cx, ax
        call    cd_write_image_file
        ; close the file
        mov     ah, 3eh
        int     21h
        jc      cd_image_create_error

        mov     dx, offset cd_disk_done
        jmp     dosQuit
        
cd_image_file_error:
        mov     ah, 3eh
        int     21h
cd_image_create_error:
        mov     dx, offset cd_image_write_error
        jmp     dosQuit

; In:   bx = file handle
;       dx = data offset
;       cx = data size
;       ds = cs
cd_write_image_file:
        add     word ptr [image_offset], cx
        adc     word ptr [image_offset+2], 0
        mov     ah, 40h
        int     21h
        jc      cd_image_file_error
cd_write_image_done:        
        ret

cd_disk_done:
        db      13,10,'Boot CD image created successfully.',13,10,'$'
        
cd_image_write_error:
        db      13,10,'Error writing boot image file.',13,10,'$'
        
        
        