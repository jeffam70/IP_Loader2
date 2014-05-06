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

                            or      outa, #1                                     'Sig Pin to output high
                            or      dira, #1
                        
                            {Send ready signal at initial baud rate}
                            jmp     #SendSignal                                 'Send "ready" and switch to final baud rate
                        
                            {Receive packet into Packet buffer}
  GetNextPacket             mov     PacketAddr, #Packet                         'Reset packet pointer
    :NextPacketLong         movd    :NextPacketByte-1, PacketAddr   '4
                            movd    :BuffAddr, PacketAddr           '4          'Point 'Packet{addr}' (dest field) at Packet buffer
                            movd    :BuffAddr+1, PacketAddr         '4
                            mov     Bytes, #4                       '4          '  Ready for 1 long
                            mov     Packet{addr}, #0                '4          '    Pre-clear 1 buffer long
    :NextPacketByte         call    #Receive                        '4          '    Get byte (resets if timeout)
      :BuffAddr             or      Packet{addr}, SByte             '4          '      store into long (low byte first)
                            ror     Packet{addr}, #8                '4          '      and adjust
                            djnz    Bytes, #:NextPacketByte         '4/8        '    Loop for all bytes of long
                            add     PacketAddr, #1                  '4          '  Done, increment packet pointer for next time
                            add     Longs, #1                       '4          '  Increment long count
                            djnz    PacketSize, #:NextPacketLong    '4/x        'Loop for all longs of packet

                            {Check packet ID}
                            cmp     PacketID, ExpectedID    wz                  'Received expected packet? z=yes
              if_z          sub     ExpectedID, #1                              '  Yes? Ready for next; No? Ready for retransmit

                            {Copy packet to Main RAM; ignore duplicate}
  CopyPacket  if_z          wrlong  PacketData{addr}, MainRAMAddr               'Write packet long to Main RAM
              if_z          add     MainRAMAddr, #4                             '  Increment Main RAM address
              if_z          add     CopyPacket, IncDest                         '  Increment PacketData address
              if_z          djnz    Longs, #CopyPacket                          'Loop for whole packet
              if_z          movd    CopyPacket, #PacketData                     'Reset PacketData{addr} for next time

                            {Send packet ACK/NAK}
  SendSignal                mov     Bytes, #4                                   'Ready 1 long
    :NextAckByte            mov     SByte, ExpectedID                           'ACK=next packet ID, NAK=previous packet ID
                            ror     ExpectedID, #8
                            call    #Transmit
                            djnz    Bytes, #:NextAckByte
                            mov     BitTime, FBitTime                           'Ensure final bit period for high-speed download

                            tjnz    ExpectedID, #GetNextPacket                  'Loop for all packets

                            {Clear remaining RAM}
                            mov     Longs, EndOfRAM                             'Determine number of registers to clear
                            sub     Longs, MainRAMAddr
                            shr     Longs, #2               wz
    :Clear    if_nz         wrlong  Zero, MainRAMAddr                           'Clear register
              if_nz         add     MainRAMAddr, #4                             '  Increment Main RAM address
              if_nz         djnz    Longs, #:Clear                              'Loop until end; Main RAM Addr = $8000
                            
                            {Insert initial call frame}
                            rdword  Longs, #5*2                                 'Get next stack address
                            sub     Longs, #4                                   'Adjust to previous stack address
                            wrlong  CallFrame, Longs                            'Store initial call frame
                            sub     Longs, #4                                   
                            wrlong  CallFrame, Longs

                            {Verify RAM}                                        '(SBytes = 0, MainRAMAddr = $8000)
    :Validate               sub     MainRAMAddr, #1                             'Decrement Main RAM Address
                            rdbyte  Bytes, MainRAMAddr                          '  Read next byte from Main RAM
                            add     SByte, Bytes                                '  Adjust checksum
                            tjnz    MainRAMAddr, #:Validate                     'Loop for all RAM
                            and     SByte, #$FF             wz                  'Low byte (checksum) zero? z=yes

                            {Send RAM Checksum ACK/NAK}
                            call    #Transmit                                   'ACK=0, NAK<>0

                            {Receive Run/EEPROM command here}

                            {Send EEPROM Checksum ACK/NAK here}

                            {Receive Run command here}
                        
                            jmp #$
                       
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
'                           mov     Bits, #10                                           'Ready 10 bits
                            mov     BitDelay, BitTime                                   'Prep first bit window; ensure prev. stop bit window
                            add     BitDelay, cnt                                        
                                                                                         
    :TxBit                  shr     SByte, #1               wc      '4                  'Get next bit
                            waitcnt BitDelay, BitTime               '6+                 'Wait for edge of bit window
                            muxc    outa, TxPin                     '4![18+]            'Output next bit
                            tjnz    SByte, #:TxBit                  '4/8                'Loop for next bit
'                           djnz    Bits, #:TxBit                   '4/8                'Loop for next bit                
  Transmit_ret              ret                                     '4                  'Done; return          

'***************************************

{Receive byte from host.
 Requirements:  Timeout value dictates maximum wait for byte.
 Results:       Received byte will be placed into SByte.
                Propeller will reset if no byte received within timeout.}
                
  Receive                   mov     TimeDelay, Timeout              '4                  'Reset timeout delay
                            mov     BitDelay, BitTime1_5    wc      '4                  'Prep first bit sample window; clear c for first :RxWait
'                           mov     Bits, #8                        ''4                 'Ready for 8 bits
                            mov     SByte, #0
    :RxWait                 muxc    SByte, #%1_1000_0000    wz      '4                  'Wait for Rx start bit (falling edge); Prep SByte for 8 bits
    {:RxWait}               test    RxPin, ina              wc      '4![12/x]           ' Check Rx state; c=0 (not resting), c=1 (resting)
              if_z_or_c     djnz    TimeDelay, #:RxWait             '4/x                ' No start bit (Z OR C)? loop until timeout
              if_z_or_c     clkset  Restart                         'x/4                ' No start bit and timed-out? Reset Propeller
'   :RxByte                 test    RxPin, ina              wc      ''4![8/68/96]       'Wait for Rx start bit; c=resting
'             if_c          djnz    TimeDelay, #:RxByte             ''4/8               ' Resting, loop until timeout
'             if_c          clkset  Restart                         ''x/4               ' Resting and timed-out? Reset Propeller
                            add     BitDelay, cnt                   '4                  'Set time to...                   
    :RxBit                  waitcnt BitDelay, BitTime               '6+                 'Wait for center of bit
    
                            xor     outa, #1                                             

                            test    RxPin, ina              wc      '4![22/82/110+]     '  Sample bit; c=0/1
                            muxc    SByte, #%1_0000_0000            '4                  '  Store bit
                            shr     SByte, #1               wc      '4                  '  Adjust result; c=0 (continue), c=1 (done)
              if_nc         jmp     #:RxBit                         '4                  'Continue? Loop until done
'                           shr     SByte, #1                       ''4                 'Adjust result and store bit
'                           muxc    SByte, #%1000_0000              ''4                 'Store bit
'                           djnz    Bits, #:RxBit                   ''4/8                 
  Receive_ret               ret                                     '4                  'Done; return

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
' Bits          res     1                                                       'Bit counter
  Bytes                                                                         'Byte counter
  Zero          res     1                                                       'Zero value (for clearing RAM); 0 when Byte = 0
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


'Possibly adjust BitTime1_5 by sample loop time to true it up
'Possibly remove Longs (or MainRAMAddr) and substitute MainRAMAddr (or Longs) in it's place