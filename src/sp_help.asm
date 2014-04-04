displayHelp:
        call    scrollLines
        db ' LEGEND:   ~=Shift   ^=Ctrl   @=Alt   ¯=Cycle choices',d_el
        ; bx=logical record#
        ; es:di=screen address
        ; ah=att (normal)
        mov     bp, offset help_Beg
        mov     cx, bx
        xor     bx, bx
        jcxz    dispHlp3
dispHlp1:
        inc     bx
dispHlp2:
        inc     bp
        cmp     byte ptr [bp-1], d_sub
        jne     dispHlp2
        cmp     bp, offset help_End
        jae     dispHlp4
        loop    dispHlp1
dispHlp3:
        call    display
        db      d_sub,d_el
        clc
        ret
dispHlp4:
        mov     cx, bx                  ;#records
        stc
        ret

HL_NN   macro   t1, t2
        ifnb    <t1>
        db      t1
        else
        db      d_mv,33
        endif
        db      '³'
        ifnb    <t2>
        db      t2
        endif
        db      d_sub
        endm

HL_BN   macro   t1, t2
        db      a_b,t1,a_n,'³'
        ifnb    <t2>
        db      t2
        endif
        db      d_sub
        endm

HL_NB   macro   t1, t2
        ifnb    <t1>
        db      t1
        else
        db      d_mv,33
        endif
        db      '³',a_b,t2,d_sub
        endm

HL_BB   macro   t1, t2
        db      a_b,t1,a_n,'³',a_b,t2,d_sub
        endm

help_Beg:  ;     1         2         3             4         5         6
       ;---------+---------+---------+---  -  -----+---------+---------+--------'
HL_BB <'Data Window'            ,d_mv,22         > <'Code Window'                             >
HL_NN <' Set seg:ofs'           ,d_mv,19    ,'D '> <' Set seg:ofs'         ,d_mv,20       ,'U'>
HL_NN <' Line Up'               ,d_mv,22   ,'Up '> <' Line Up'             ,d_mv,21    ,'PgUp'>
HL_NN <' Line Down'             ,d_mv,20   ,'Dn '> <' Line Down'           ,d_mv,19    ,'PgDn'>
HL_NN <' Page Up'               ,d_mv,21  ,'^Up '> <' Page Up'             ,d_mv,20   ,'^PgUp'>
HL_NN <' Page Down'             ,d_mv,19  ,'^Dn '> <' Page Down'           ,d_mv,18   ,'^PgDn'>
HL_NN <' seg +1'                ,d_mv,22  ,'~Dn '> <' seg +1'              ,d_mv,21   ,'@PgDn'>
HL_NN <' seg -1'                ,d_mv,22  ,'~Up '> <' seg -1'              ,d_mv,21   ,'@PgUp'>
HL_NN <' ofs +1'                ,d_mv,23   ,'Rt '> <' ofs +1'              ,d_mv,21   ,'~PgDn'>
HL_NN <' ofs -1'                ,d_mv,23   ,'Lf '> <' ofs -1'              ,d_mv,21   ,'~PgUp'>
HL_NN <' Display code window'   ,d_mv,10   ,'~d '> <' Display data window' ,d_mv,11      ,'~u'>
HL_NN <' Display effective address',d_mv,4 ,'@D '> <' Display effective address',d_mv,5  ,'@U'>
HL_NN <' Display watch window'  ,d_mv,9    ,'^D '> <' Display watch window',d_mv,10      ,'^U'>
HL_NN <' Display stack window'  ,d_mv,8   ,'~^d '> <' Display stack window',d_mv,9      ,'~^u'>
HL_NN <' Display near RET'      ,d_mv,13   ,'@K '> <' Display near RET'    ,d_mv,14      ,'^K'>
HL_NN <' Display far RET'       ,d_mv,13  ,'~@k '> <' Display far RET'     ,d_mv,14     ,'~^k'>
HL_NN <' Set marker'            ,d_mv,16,'~0..9 '> <' Set marker'          ,d_mv,16  ,'~^0..9'>
HL_NN <' Goto marker'           ,d_mv,16 ,'0..9 '> <' Goto marker'         ,d_mv,16   ,'^0..9'>
HL_NN <'¯Byte/Word/Dword/Ascii' ,d_mv,9     ,'J '> <' Goto CS:IP'          ,d_mv,17   ,'./END'>
HL_NN <'¯ASCII filter: On/Off'  ,d_mv,9    ,'@J '> <                                          >
HL_NB <' Paragraph align'       ,d_mv,13  ,'~Lf '> <'Watch Window'                            >
HL_NN <' Normalize'             ,d_mv,19  ,'~Rt '> <' Set  watch'               ,d_mv,20 ,'@W'>
HL_NN <                                          > <'¯Next watch'               ,d_mv,21  ,'W'>
HL_BN <'Stack Window'             ,d_mv,21       > <'¯Prev watch'               ,d_mv,20 ,'~w'>
HL_NN <'¯SS:SP/SS:BP'             ,d_mv,19  ,'K '> <'¯ASCII filter: On/Off'     ,d_mv,10 ,'@J'>
HL_NN <' Display near RET as code',d_mv,5  ,'^K '> <'¯Byte/Word/Dword/Ascii'    ,d_mv,9  ,'~j'>
HL_NN <' Display far RET as code' ,d_mv,5 ,'~^k '> <                                          >
HL_NB <' Display near RET as data',d_mv,5  ,'@K '> <'Memory'                                  >
HL_NN <' Display far RET as data' ,d_mv,5 ,'~@k '> <' Edit'                     ,d_mv,27  ,'E'>
HL_NN <                                          > <' Fill'                     ,d_mv,26 ,'~m'>
HL_BN <'Registers'             ,d_mv,24          > <' Compare'                  ,d_mv,23 ,'~c'>
HL_NN <' Modify'               ,d_mv,24     ,'R '> <' Copy'                     ,d_mv,27  ,'C'>
HL_NN <' 32-bit Registers'     ,d_mv,10 ,'~r/F2 '> <' Find next'                ,d_mv,22  ,'F'>
HL_NN <' Mark'                 ,d_mv,24   ,'~@r '> <' Find'                     ,d_mv,26 ,'~f'>
HL_NN <' Compare'              ,d_mv,22    ,'@R '> <' Replace'                  ,d_mv,23 ,'^R'>
HL_NN <                                          > <' Working Memory read'      ,d_mv,12  ,'Y'>
HL_BN <'Breakpoints'              ,d_mv,22       > <' Working Memory write'     ,d_mv,10 ,'~y'>
HL_NN <'¯Set/Remove (B1/B2)'      ,d_mv,9,'B/F9 '> <                                          >
HL_NB <'¯Set/Remove (B2)'         ,d_mv,14 ,'~b '> <'Hooks'                                   >
HL_NN <' Enable'                  ,d_mv,23 ,'^B '> <' Set'                     ,d_mv,28   ,'H'>
HL_NN <' Disable'                 ,d_mv,21,'~^b '> <' Remove'                  ,d_mv,24  ,'@H'>
HL_NN <' Remove'                  ,d_mv,23 ,'@B '> <' Enable'                  ,d_mv,24  ,'^H'>
HL_NN <' List'                    ,d_mv,26  ,'L '> <' Disable'                 ,d_mv,22 ,'~^h'>
HL_NN <                                          > <' List'                    ,d_mv,26  ,'~l'>
HL_BN <'Execution'             ,d_mv,24          > <' TimerTick'               ,d_mv,20 ,'~F1'>
HL_NN <' Go'                   ,d_mv,23,'G/Q/F5 '> <                                          >
HL_NB <' Go here'              ,d_mv,19 ,'~g/~q '> <'Interrupts'                              >
HL_NN <' Go from here'         ,d_mv,16   ,'~^g '> <' List All'                 ,d_mv,22 ,'~n'>
HL_NN <' Set CS:IP here'       ,d_mv,15    ,'^G '> <' List New'                 ,d_mv,23  ,'N'>
HL_NN <' Trace 1'              ,d_mv,18,'T/+/F8 '> <' Load'                     ,d_mv,26 ,'@N'>
HL_NN <' Trace n'              ,d_mv,22    ,'~t '> <' Restore'                  ,d_mv,22,'~@n'>
HL_NN <' Set Trace count (n)'  ,d_mv,9,    '~^t '> <                                          >
HL_NB <' Trace Up'             ,d_mv,21    ,'^T '> <'System'                                  >
HL_NN <' Trace here'           ,d_mv,19    ,'@T '> <' Help'                   ,d_mv,24 ,'?/F1'>
HL_NN <' Step'                ,d_mv,20,'S/-/F10 '> <' Memory Map'             ,d_mv,18 ,'~F10'>
HL_NN <' Assemble'             ,d_mv,22     ,'A '> <' Device Drivers'         ,d_mv,15  ,'~F9'>
HL_NN <' Terminate Application',d_mv,7    ,'^@Q '> <' Open Files'             ,d_mv,19  ,'~F8'>
HL_NN <                                          > <' Current task'           ,d_mv,17  ,'~F7'>
HL_BN <'I/O'                      ,d_mv,30       > <' Print (code)'           ,d_mv,18   ,'~p'>
HL_NN <' In byte'                 ,d_mv,23  ,'I '> <' Print (byte)'           ,d_mv,17  ,'~^p'>
HL_NN <' In word'                 ,d_mv,22 ,'~i '> <' Print (word)'           ,d_mv,17  ,'~@p'>
HL_NN <' Out byte'                ,d_mv,22  ,'O '> <' Hex calculator'         ,d_mv,17    ,'X'>
HL_NN <' Out word'                ,d_mv,21 ,'~o '> <' Dec calculator'         ,d_mv,16   ,'~x'>
HL_NN <                                          > <' Char code'              ,d_mv,21   ,'~a'>
HL_BN <'Macros'                ,d_mv,27          > <' Redraw screen'          ,d_mv,17   ,'F3'>
HL_NN <' Record'               ,d_mv,19,'~@0..9 '> <' Popup User window'      ,d_mv,11 ,'V/F4'>
HL_NN <' Execute'              ,d_mv,19 ,'@0..9 '> <' Clear User window'      ,d_mv,11 ,'V-BS'>
help_End: