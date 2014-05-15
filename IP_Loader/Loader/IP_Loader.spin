{IP_Loader.spin

 This is the mini-loader delivered by the first Internet Protocol packet sent to the Propeller over XBee WiFi.  It assists with the remainder
 of the download to deliver the desired Propeller Application in a quick and reliable fashion.

 Any symbols in the assembly's "Constants" section labeled "[host init]" has their values set by the host before this application image is
 transmission via the first packet.

 Host Requirements:

}

CON
        _clkmode = xtal1 + pll16x                                               'Standard clock mode * crystal frequency = 80 MHz
        _xinfreq = 5_000_000

  MaxPacketPayload = 1392                                                       'Maximum size of packet payload (in bytes)

PUB Main

  coginit(0, @Loader, 0)

DAT
{IP Loader PASM.  Runs in cog to receive target application at high-speed (compared to standard Propeller Download Protocol).

Timing: Critical routine timing is shown in comments, like '4 and '6+, indication clock cycles instruction consumes.
        'n       : n clock cycles
        'n+      : n or more clock cycles
        'n/m     : n cycles (if jumping) or m cycles (if not jumping)
        'x/n     : don't care (if executed) or n cycles (if not executed)
        'n![m+]  : n cycles til moment of output/input, routine's iteration time is m or more cycles moment-to-moment
        'n![m/l] : like above except inner-loop iteration time is m cycles moment-to-moment, outer-loop is l cycles moment-to-moment
        }

                            org     0                    
                            {Initialize Tx pin}                                 
  Loader                    mov     outa, TxPin                                 'Tx Pin to output high (resting state)
                            mov     dira, TxPin

 {debug}                    or      outa, #1                                    'Sig Pin to output high
 {debug}                    or      dira, #1

                            {Wait for resting RxPin}
                            mov     BitDelay, BitTime       wc                  'Prep wait for 1/2 bit periods; clear c for first :RxWait
                            shr     BitDelay, #1
                            add     BitDelay, cnt
    :RxWait   if_nc         mov     TimeDelay, #8*20                            'If RxPin active (c=0), reset sample count; 8 bytes * 20 samples/byte
                            waitcnt BitDelay, BitTime
                            test    RxPin, ina              wc                  '   Check Rx state; c=0 (not resting), c=1 (resting)
                            djnz    TimeDelay, #:RxWait     wz                  'Rx busy? Loop until resting 8 byte periods
                                                                                'z=1 (prep'd for normal mode: application receiving)
                            {Send ACK/NAK; "Ready" signal at initial baud first}
  Acknowledge               mov     Bytes, #4                                   'Ready 1 long
    :NextAckByte            mov     SByte, ExpectedID                           'ACK=next packet ID, NAK=previous packet ID
                            ror     ExpectedID, #8
                            call    #Transmit
                            djnz    Bytes, #:NextAckByte
                            mov     BitTime, FBitTime                           'Ensure final bit period for high-speed download
                            
              if_nz         jmp     #Launch {PacketData}                        'Done receiving app and RAM verified? Launch
                                                                                'Else, normal mode
                            {Receive packet into Packet buffer}                        
  GetNextPacket             mov     PacketAddr, #Packet                         'Reset packet pointer
    :NextPacketLong         movd    :NextPacketByte-1, PacketAddr   '4          'Point 'Packet{addr}' (dest field) at Packet buffer
                            movd    :BuffAddr, PacketAddr           '4          
                            movd    :BuffAddr+1, PacketAddr         '4
                            mov     Bytes, #4                       '4          '  Ready for 1 long
                            mov     Packet{addr}, #0                '4          '    Pre-clear 1 buffer long

    :NextPacketByte         mov     TimeDelay, Timeout              '4                  'Get byte (resets if timeout); Reset timeout delay
                            mov     BitDelay, BitTime1_5    wc      '4                  'Prep first bit sample window; clear c for first :RxWait
                            mov     SByte, #0
    :RxWait                 muxc    SByte, #%1_1000_0000    wz      '4                  'Wait for Rx start bit (falling edge); Prep SByte for 8 bits
                            test    RxPin, ina              wc      '4![12/x]           ' Check Rx state; c=0 (not resting), c=1 (resting)
              if_z_or_c     djnz    TimeDelay, #:RxWait             '4/x                ' No start bit (Z OR C)? loop until timeout
              if_z_or_c     clkset  Restart                         'x/4                ' No start bit and timed-out? Reset Propeller
                            add     BitDelay, cnt                   '4                  'Set time to...                   
    :RxBit                  waitcnt BitDelay, BitTime               '6+                 'Wait for center of bit
    
  {debug}                   xor     outa, #1                                             

                            test    RxPin, ina              wc      '4![22/82/110+]     '  Sample bit; c=0/1
                            muxc    SByte, #%1_0000_0000            '4                  '  Store bit
                            shr     SByte, #1               wc      '4                  '  Adjust result; c=0 (continue), c=1 (done)
              if_nc         jmp     #:RxBit                         '4                  'Continue? Loop until done
    
      :BuffAddr             or      Packet{addr}, SByte             '4          '      store into long (low byte first)
                            ror     Packet{addr}, #8                '4          '      and adjust
                            djnz    Bytes, #:NextPacketByte         '4/8        '    Loop for all bytes of long
                            add     PacketAddr, #1                  '4          '  Done, increment packet pointer for next time
                            djnz    PacketSize, #:NextPacketLong wc '4/x        'Loop for all longs of packet; c=0 (prep'd for Check Packet ID)

                            {Check packet ID}
                            cmps    PacketID, ExpectedID    wz                  'Received expected packet? z=yes
              if_z          cmps    ExpectedID, #1          wc,wr               '  (z=1) Ready for next packet (dec ExpectedID; c=new ExpectedID < 0)
                                                                                '  or (z=0) ready for retransmit (ExpectedID untouched; c=0)
                            {If new packet, copy to Main RAM; ignore duplicate}
              if_z_and_nc   sub     PacketAddr, #PacketData                     'Make PacketAddr into a loop counter
  CopyPacket  if_z_and_nc   wrlong  PacketData{addr}, MainRAMAddr               'Write packet long to Main RAM
              if_z_and_nc   add     MainRAMAddr, #4                             '  Increment Main RAM address
              if_z_and_nc   add     CopyPacket, IncDest                         '  Increment PacketData address
                            djnz    PacketAddr, #CopyPacket wz                  'Loop for whole packet; z=1 for normal acknowledge mode
                            movd    CopyPacket, #PacketData                     'Reset PacketData{addr} for next time

              if_nc         jmp     #Acknowledge                                'Loop to acknowledge if ExpectedID >= 0

                            add     ExpectedID, #1          wz                  'Verified RAM already? z=no
              if_nz         jmp     #Acknowledge                                '  Yes? Acknowledge and launch application

                            {Entire Application Received; clear remaining RAM}
                            mov     Longs, EndOfRAM                             'Determine number of registers to clear
                            sub     Longs, MainRAMAddr
                            shr     Longs, #2               wz
    :Clear    if_nz         wrlong  Zero, MainRAMAddr                           'Clear register
              if_nz         add     MainRAMAddr, #4                             '  Increment Main RAM address
              if_nz         djnz    Longs, #:Clear                              'Loop until end; Main RAM Addr = $8000
                            
                            {Insert initial call frame}
                            rdword  Longs, #5<<1                                'Get next stack address
                            sub     Longs, #4                                   'Adjust to previous stack address
                            wrlong  CallFrame, Longs                            'Store initial call frame
                            sub     Longs, #4                                   
                            wrlong  CallFrame, Longs
                            
                            {Verify RAM; calculate checksum}                    '(ExpectedID = 0, MainRAMAddr = $8000)
    :Validate               sub     MainRAMAddr, #1                             'Decrement Main RAM Address
                            rdbyte  Bytes, MainRAMAddr                          '  Read next byte from Main RAM
                            add     ExpectedID, Bytes                           '  Adjust checksum
                            tjnz    MainRAMAddr, #:Validate wz                  'Loop for all RAM; z=1 for normal acknowledge mode
                            neg     ExpectedID, ExpectedID                      'Negate checksum
                            
                            jmp     #Acknowledge                                'ACK=Proper -Checksum, NAK=Improper Checksum


'                           and     ExpectedID, #$FF        wz                  'Low byte (checksum) zero? z=yes

                            {Receive Run/EEPROM command here}

                            {Send EEPROM Checksum ACK/NAK here}

                            {Receive Run command here}

                            {Validate program base and launch application}
  Launch                    rdword  MainRAMAddr, #3<<1                          'Check program base address
                            cmp     MainRAMAddr, #$10       wz                  'nz=Invalid
              if_nz         clkset  Restart                                     'Invalid?  Reset Propeller

'                           rdbyte  Bytes, #4                                   'if xtal/pll enabled, start up now
'                           and     Bytes, #$F8                                 '..while remaining in rcfast mode
'                           clkset  Bytes                                        
                                                                                 
'   :delay                  djnz    time_xtal,#:delay                           'allow 20ms @20MHz for xtal/pll to settle
                                                                                 
'                           rdbyte  Bytes,#4                                    'switch to selected clock
'                           clkset  Bytes                                        
                                                                                 
                            coginit interpreter                                 'Relaunch with Spin Interpreter
                       
{***************************************
 *             Subroutines             *
 ***************************************}

{Transmit byte to host.
 Requirements:  SByte must contain desired byte value to transmit.
 Results:       SByte serially transmitted.
                SByte = 0 upon return.}

  Transmit                  and     SByte, #$FF                                         'Retain low byte only
                            or      SByte, #%1_0000_0000                                'Append stop bit; also acts as loop trigger
                            shl     SByte, #1                                           'Prepend start bit
                            mov     BitDelay, BitTime                                   'Prep first bit window; ensure prev. stop bit window
                            add     BitDelay, cnt                                        
                                                                                         
    :TxBit                  shr     SByte, #1               wc      '4                  'Get next bit
                            waitcnt BitDelay, BitTime               '6+                 'Wait for edge of bit window
                            muxc    outa, TxPin                     '4![18+]            'Output next bit
                            tjnz    SByte, #:TxBit                  '4/8                'Loop for next bit
  Transmit_ret              ret                                     '4                  'Done; return          

'***************************************
'*       Constants and Variables       *
'***************************************

{Initialized Variables}
  Longs         long    0                                                       'Long counter
  MainRAMAddr   long    0                                                       'Address in Main RAM

{Constants}
  Restart       long    %1000_0000                                              'Reboot value (for CLK register)
  IncDest       long    %1_0_00000000                                           'Value to increment a register's destination field
  EndOfRAM      long    $8000                                                   'Address of end of RAM+1
  CallFrame     long    $FFF9_FFFF                                              'Initial call frame value
  Interpreter   long    $0001 << 18 + $3C01 << 4 + %0000                        'Coginit value to launch Spin interpreter
  RxPin         long    |< 31                                                   'Receive pin mask (P31)
  TxPin         long    |< 30                                                   'Transmit pin mask (P30)
  
{Host Initialized Values}                                             
  BitTime                                                                       'Bit period (in clock cycles)
    IBitTime    long    80_000_000 / 115_200                     '[host init]   '  Initial bit period (at startup)
    FBitTime    long    80_000_000 / 230_400                     '[host init]   '  Final bit period (for download)
  BitTime1_5    long    TRUNC(1.5 * 80_000_000.0 / 230_400.0)    '[host init]   '1.5x bit period; used to align to center of received bits
  Timeout       long    80_000_000 * 4 / (2*8)                   '[host init]   'Timeout period (2 seconds worth of RxByte loop iterations)
  ExpectedID    long    0                                        '[host init]   'Expected Packet ID

{Reserved Variables}
  TimeDelay     res     1                                                       'Timout delay
  BitDelay      res     1                                                       'Bit time delay
  SByte         res     1                                                       'Serial Byte; received or to transmit
  Bytes                                                                         'Byte counter
  Zero          res     1                                                       'Zero value (for clearing RAM); 0 when Bytes = 0
  PacketAddr    res     1                                                       'PacketAddr
  Packet                                                                        'Packet buffer
    PacketSize  res     1                                                       '  Header:  Packet Size
    PacketID    res     1                                                       '  Header:  Packet ID number
    PacketData  res     (MaxPacketPayload / 4) - 2                              '  Payload: Packet data (longs); (max size in longs) - header


' CalcBitTime           mov     BitTime1_5, BitTime                             'Calculate 1.5x bit time (BitTime * 3 / 2)
'                       shl     BitTime1_5, #1
'                       add     BitTime1_5, BitTime
'                       shr     BitTime1_5, #1

{                       rol     Packet, #8
                        mov     SByte, Packet
                        call    #Transmit
                        rol     Packet, #8
                        mov     SByte, Packet
                        call    #Transmit
                        rol     Packet, #8
                        mov     SByte, Packet
                        call    #Transmit
                        rol     Packet, #8
                        mov     SByte, Packet
                        call    #Transmit
                        jmp     #$
}


{                           mov     Longs, EndOfRAM
                            mov     MainRAMAddr, #0
    :Read                   rdbyte  SByte, MainRAMAddr                          '  Read next byte from Main RAM
                            call    #Transmit
                            add     MainRAMAddr, #1
                            djnz    Longs, #:Read                               'Loop for all RAM

                            
                            jmp     #$
}

'             if_c          jmp     #PacketData                                 '  Abort (PacketID < ExpectedID)? Reset Propeller


'Possibly adjust BitTime1_5 by sample loop time to true it up
'Possibly remove Longs (or MainRAMAddr) and substitute MainRAMAddr (or Longs) in it's place