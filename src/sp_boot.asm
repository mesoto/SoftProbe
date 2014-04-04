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
        
show_boot:
        push    cs
        pop     ds
        cld
        mov     si, offset boot_from
        lodsb
tv_mesg:push    si
        mov     ah, 0eh
        mov     bx, 7
        int     10h
        pop     si
        lodsb
        or      al, al
        jnz     tv_mesg
        xor     ax, ax
        int     16h
        xor     dx, dx
        cmp     al, ' '
        je      boot_freedos
        or      al, 32
        sub     al, 'a'
        cmp     al, 2           ;A: or B:
        jb      go_boot
        cmp     al, 26          ;C: to Z:
        jae     show_boot
        add     al, 80h-2
go_boot:mov     dl, al
        xor     ax, ax
        mov     ds, ax
        mov     es, ax
        int     13h
        mov     cx, 1
load_boot_sec:
        mov     bx, 7c00h
        mov     ax, 201h
        int     13h
        jc      show_boot
        pop     si              ;original dx
        pop     cx
        pop     bx
        ; SS:SP -> 7c00
        ;          0000
        ;          flags
        jmp     SoftProbeEntry

boot_freedos:
        mov     dl, [bp].diskNumber
        xor     ax, ax
        mov     ds, ax
        mov     es, ax
        int     13h
        mov     dh, [bp].curr_head
        mov     cl, [bp].curr_sector
        mov     ch, [bp].curr_track
        jmp     load_boot_sec
        
boot_from db 13,10,'Select boot drive (A-Z or space bar for Internal DOS) ? ',0

boot_FileEnd:

minWMemSize     equ ($ - workingAreaBeg+15)/16
boot_image_size equ (boot_FileEnd-boot_FileBeg)


;----------------------------------------------------------------------------
;- CD boot start (loaded at 0000:7c00) -----------------------------------------
;----------------------------------------------------------------------------
include sp_cdbt.asm

;----------------------------------------------------------------------------
;- FD boot start (loaded at 0000:7c00) -----------------------------------------
;----------------------------------------------------------------------------

stack_layout    struc
disk_read_addr  dd 0
sectors2read    dw 0
diskNumber      db 0
curr_head       db 0
curr_sector     db 0
curr_track      db 0
disk_retry      db 0
                db 0
stack_layout    ends

local_stack_size equ 12

        even
        
fd_start_of_boot_sector:    
        jmp short fd_boot_start
        nop

        db "BOOTPROB"   ;03 system_id_string: 8 chars
        dw 512      ;11 bytes_per_sector
        db 1        ;13 sectors_per_cluster    
        dw 1        ;14 reserved_sectors
        db 2        ;16 FAT_copies
        dw 224      ;17 directory_entries 112 & 224 are typical
        dw 2880     ;19 total_sectors (18*2*80)
        db 0f0h     ;21 media_descriptor 0f9H, 0f0h, 0fAh, etc.
        dw 9        ;22 sectors_per_FAT
        dw 18       ;24 sectors_per_track
        dw 2        ;26 heads_per_cylinder
        dd 0        ;28 hidden_sectors: always zero for floppies
        dd 0        ;32 total_sectors_2: actual value if total_sectors = 0
        dw 0        ;36 BPB reserved
        db 29h      ;38 marker for volume ID
        dd 0        ;39 serial_number: volume unique ID number
        db "SOFTPROBE  " ;43 volume_label: 11 chars
        db "FAT12   "    ;54 filesystem_id:  8 chars

; _____________________________________________________________________________

fd_disp_char:
        mov     ah,0eh
        mov     bx,7
        int     10h
fd_display_string: 
        pop     si
        lods    byte ptr cs:[si]
        push    si
        or      al,al
        jnz     fd_disp_char
        ret

fd_boot_start:
        xor     ax,ax
        mov     bp,7c00h - local_stack_size  ;origin at 7C00
        cli
        mov     ss,ax
        mov     sp,bp
        mov     [bp].diskNumber,dl   ;save disk number
        
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
        call    fd_display_string
        copyrightMsg 'SoftProbe'
        db      'Loading ....',0

        xor     ax, ax
        mov     [bp].curr_sector, 2
        mov     [bp].curr_track, al
        mov     [bp].curr_head, al
        ; load softprobe at 1000h:0 to make sure no 64K crossing
        mov     word ptr [bp+0].disk_read_addr, ax
        mov     word ptr [bp+2].disk_read_addr, 1000h
        mov     [bp].sectors2read, (boot_image_size+511)/512

boot_load_loop:
        mov     [bp].disk_retry, 10
boot_load_read:
        mov     dl, [bp].diskNumber
        mov     dh, [bp].curr_head
        mov     cl, [bp].curr_sector
        mov     ch, [bp].curr_track
        les     bx, [bp].disk_read_addr
        mov     ax, 201h
        int     13h
        jnc     boot_load_next
        xor     ax, ax
        int     13h
        dec     [bp].disk_retry
        jnz     boot_load_read
boot_load_error:
        call    fd_display_string
        db      13,10,'Boot error.'
        db      13,10,'Press a key to reboot.',0
        xor     ax,ax
        int     16h             ;wait for keypress
        int     19h             ;re-boot

boot_load_next:
        add     word ptr [bp].disk_read_addr, 200h
        inc     [bp].curr_sector
        cmp     [bp].curr_sector, 19
        jb      boot_load_cont
        mov     [bp].curr_sector, 1
        xor     [bp].curr_head, 1
        jnz     boot_load_cont
        inc     [bp].curr_track
boot_load_cont:
        dec     [bp].sectors2read
        jnz     boot_load_loop
        
        xor     ax, ax
        mov     ds, ax
        sub     word ptr ds:[413h], 4444h ;reduce top memory
fd_alloc_KBytes    equ $-2
        mov     ax, word ptr ds:[413h]
        mov     cl, 6
        shl     ax, cl
        mov     es, ax
        xor     di, di
        mov     si, 1000h
        mov     ds, si
        xor     si, si
        mov     cx, (boot_FileEnd - boot_FileBeg + 1)/2
        rep     movsw
        
        mov     bx, offset Install
        push    ax                      ;address to jump to (es:Install)
        push    bx
        retf                            ;jump to Install

        db      (fd_start_of_boot_sector+510-$) dup(0)
        dw      0AA55h                  ; BOOT_SIGNATURE
        
;----------------------------------------------------------------------------
; Start of BOOTPROB.EXE : Create a bootable disk image....
;----------------------------------------------------------------------------
fd_image_FileName  db 'BOOT_FD.IMG',0
dos_image_FileName db 'BOOTPROB.DAT',0
src_handle      dw 0
dst_handle      dw 0

fd_file_write   macro data_off, data_size
        mov     dx, offset data_off
        mov     cx, data_size
        call    fd_write_image_file
        endm
  
fd_file_read    macro data_off, data_size
        mov     dx, offset data_off
        mov     cx, data_size
        mov     bx, src_handle
        mov     ah, 3Fh
        int     21h
        jc      fd_file_read_error
        endm
        
start:  mov     si, 81h
        cld
        DBGBRK
        call    do_CommandLine
        jnc     @f
        mov     ax, 4c01h
        int     21h

@@:     mov     ax, workingMemLen
        cmp     ax, minWMemSize         ;make sure Install resides in RAM
        jae     @f
        mov     ax, minWMemSize
@@:     add     ax, resident_Size -1
        mov     cl, 6
        shr     ax, cl                  ;para's to KB
        inc     ax
        mov     word ptr [fd_alloc_KBytes], ax
        mov     word ptr [cd_alloc_KBytes], ax
        shl     ax, cl
        sub     ax, resident_Size       ;whatever left for WMem
        mov     workingMemLen, ax

        cmp     boot_media_type, 1      ; 0=FD, 1=CD
        jnz     fd_create_boot_image
        jmp     cd_create_boot_image
        
fd_file_read_error:
        mov     dx, offset source_read_error
        jmp     dosQuit
 
fd_create_boot_image:
        ; create floppy image
        mov     dx, offset dos_image_FileName
        mov     ax, 3d00h               ;open for read
        int     21h
        jc      fd_file_read_error
        mov     src_handle, ax

        fd_file_read src_boot_sec,512

        ; copy BPB info from image
        mov     si, offset src_boot_sec+3
        ; reserve enough sectors for sp image + freedos boot sector
        add     word ptr [si+14-3],(boot_image_size+512+511)/512
        
        mov     di, offset fd_start_of_boot_sector+3
        mov     cx, 59                  ; offset 3-62
        push    cs
        pop     es
        rep     movsb
        
        mov     dx, offset fd_image_FileName
        xor     cx, cx                  ;normal
        mov     ah, 3ch                 ;create
        int     21h
        jc      image_create_error
        mov     dst_handle, ax
        
        ; write boot sector
        fd_file_write  fd_start_of_boot_sector,512
        ; use reserved area for the sp_image
        fd_file_write  boot_FileBeg,boot_image_size

        ; write source image
        fd_file_write  src_boot_sec,512

        ; copy rest of the image
        mov     si, word ptr [src_boot_sec+19] ; total number of sectors
        sub     si, (boot_image_size+1024+511)/512 ; sectors left to copy

fd_copy_image_loop:
        fd_file_read   fill_buff,512
        fd_file_write  fill_buff,512
        dec     si
        jnz     fd_copy_image_loop


IFDEF _EMPTY_FD_

        ; first FAT
        fd_file_write  floppy_fat,3
        fd_file_write  fill_buff,1000h
        ; second FAT
        fd_file_write  floppy_fat,3
        fd_file_write  fill_buff,1000h
        ; root directory
        fd_file_write  floppy_dir,26
        fd_file_write  fill_buff,1000h
        ; the data area
fd_fill_data_area:
        ;file_fill   0e5h,512
        fd_file_write  fill_buff,512
        ; 2880 sectors = 1474560 bytes = 168000h
        cmp     word ptr [image_offset+2], 16h
        jb      fd_fill_data_area
        cmp     word ptr [image_offset+0], 8000h
        jb      fd_fill_data_area
ENDIF
        
        ; close the file
        mov     ah, 3eh
        int     21h
        jc      image_create_error
        
        mov     bx, src_handle
        mov     ah, 3eh
        int     21h

        mov     dx, offset disk_done
dosQuit:mov     ah, 9
        int     21h
        mov     ax, 4c00h
        int     21h

image_file_error:
        mov     ah, 3eh
        int     21h
image_create_error:
        mov     dx, offset image_write_error
        jmp     dosQuit

image_offset    dw 0,0

; In:   dx = data offset
;       cx = data size
;       ds = cs
fd_write_image_file:
        mov     bx, dst_handle
        add     word ptr [image_offset], cx
        adc     word ptr [image_offset+2], 0
        mov     ah, 40h
        int     21h
        jc      image_file_error
        ; make it sector-aligned
        mov     ax, word ptr [image_offset]
        and     ax, 1ffh
        jz      write_image_done
        mov     cx, 200h
        sub     cx, ax
        add     word ptr [image_offset], cx
        adc     word ptr [image_offset+2], 0
        
        mov     dx, offset fill_buff
        mov     ah, 40h
        int     21h
        jc      image_file_error
write_image_done:        
        ret

include sp_cmdl.asm

boot_media_type db 0    ; 0=FD, 1=CD

floppy_fat      db 0F0h,0FFh,0FFh
floppy_dir      db 053h,04Fh,046h,054h,050h,052h,04Fh,042h ; 'SOFTPROBE  '
                db 045h,020h,020h,008h,000h,000h,000h,000h ;
                db 000h,000h,000h,000h,000h,000h,0D7h,096h
                db 02Eh,03Eh

copyright:      db 56 dup ('_'),13,10,13,10
                copyrightMsg 'SoftProbe Boot Image Builder'
                db 56 dup ('_'),13,10,'$'

disk_done:      db 13,10,'Boot image created successfully.',13,10,'$'
        
image_write_error:
                db 13,10,'Error writing boot disk image file.',13,10,'$'
source_read_error:
                db 13,10,'Error reading BootProb.dat file.',13,10,'$'

cmd_Line        db 128 dup(0)
fill_buff       db 1000h dup(0)
src_boot_sec    db 200h dup(0)

code    ends

stack   segment para STACK
        dw      200h dup(0)
stack   ends

        end     start
        
        