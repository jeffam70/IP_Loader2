{IP_Loader.spin

 This is the mini-loader delivered by the first Internet Protocol packet sent to the Propeller over XBee WiFi.  It assists with the remainder
 of the download to deliver the desired Propeller Application in a quick and reliable fashion.

 Any symbols in the assembly's "Constants" section labeled "[host init]" has their values set by the host before this application image is
 transmission via the first packet.
}

CON
        _clkmode = xtal1 + pll16x                                               'Standard clock mode * crystal frequency = 80 MHz
        _xinfreq = 5_000_000

PUB Main

  waitcnt(clkfreq + cnt)

  coginit(0, @Loader, 0)

DAT
                        org     0
                        {Initialize Tx pin}
  Loader                mov     outa, TxPin                                     'Tx Pin to output high (resting state)
                        mov     dira, TxPin

                        {Send ready signal at initial baud rate}
                        jmp     SendSignal                                      'Send "ready" and switch to final baud rate
                        
                        {Receive packet into Packet buffer}
  GetNextPacket         mov     PacketAddr, #Packet                             'Reset packet pointer
    :NextPacketLong     movd    :BuffAddr, PacketAddr                           'Point 'Packet{addr}' (dest field) at Packet buffer
                        movd    :BuffAddr+1, PacketAddr
                        mov     Bytes, #4                                       '  Ready for 1 long
    :NextPacketByte     call    #Receive                                        '    Get byte (resets if timeout)
      :BuffAddr         or      Packet{addr}, SByte                             '      store into long (low byte first)
                        ror     Packet{addr}, #8                                '      and adjust
                        djnz    Bytes, #:NextPacketByte                         '    Loop for all bytes of long
                        add     PacketAddr, #1                                  '  Done, increment packet pointer for next time
                        add     Longs, #1                                       '  Increment long count
                        djnz    PacketSize, #:NextPacketLong                    'Loop for all longs of packet

                        {Check packet ID}
                        cmp     PacketID, ExpectedID    wz                      'Received expected packet? z=yes
              if_z      sub     ExpectedID, #1                                  '  Yes? Ready for next; No? Ready for retransmit

                        {Copy packet to Main RAM; ignore duplicate}
  CopyPacket  if_z      wrlong  PacketData{addr}, MainRAMAddr                   'Write packet long to Main RAM
              if_z      add     MainRAMAddr, #4                                 '  Increment Main RAM address
              if_z      add     CopyPacket, IncDest                             '  Increment PacketData address
              if_z      djnz    Longs, #CopyPacket                              'Loop for whole packet
              if_z      movd    CopyPacket, #PacketData                         'Reset PacketData{addr} for next time

                        {Send packet ACK/NAK}
  SendSignal            mov     Bytes, #4                                       'Ready 1 long
    :NextAckByte        mov     SByte, ExpectedID                               'ACK=next packet ID, NAK=previous packet ID
                        ror     ExpectedID, #8
                        call    #Transmit
                        djnz    Bytes, #:NextAckByte
                        mov     BitTime, FBitTime                               'Ensure final bit period for high-speed download

                        tjnz    ExpectedID, #GetNextPacket                      'Loop for all packets

                        {Send RAM Checksum ACK/NAK here}

                        {Receive Run/EEPROM command here}

                        {Send EEPROM Checksum ACK/NAK here}

                        {Receive Run command here}
                        
                        jmp     #$

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
                       
{***************************************
 *             Subroutines             *
 ***************************************}

{Transmit byte to host.
 Requirements:  SByte must contain desired byte value to transmit.
 Results:       SByte serially transmitted.}

  Transmit              or      SByte, #%1_0000_0000                            'Append stop bit
                        shl     SByte, #1                                       'Prepend start bit
                        mov     Bits, #10                                       'Ready 10 bits
                        mov     BitDelay, BitTime                               'Prep first bit window
                        add     BitDelay, cnt
                        
    :TxBit              shr     SByte, #1               wc                      'Get next bit
                        waitcnt BitDelay, BitTime                               'Wait for edge of bit window
                        muxc    outa, TxPin                                     'Output next bit
                        djnz    Bits, #:TxBit                                   'Loop for next bit                        
  Transmit_ret          ret                                                     'Done; return                  

'***************************************

{Receive byte from host.
 Requirements:  Timeout value dictates maximum wait for byte.
 Results:       Received byte will be placed into SByte.
                Propeller reset if no byte received within timeout.}
                
  Receive               mov     TimeDelay, Timeout                              'Reset timeout delay
                        mov     BitDelay, BitTime1_5                            'Prep for first bit sample
                        mov     Bits, #8                                        'Ready for 8 bits
    :RxWait             test    RxPin, ina              wc                      'Wait for Rx rest; nc=not resting
              if_nc     djnz    TimeDelay, #:RxWait                             '  Not resting, loop until timeout
              if_nc     clkset  Restart                                         '  Not resting and timed-out? Reset Propeller
    :RxByte             test    RxPin, ina              wc                      'Wait for Rx start bit; c=resting
              if_c      djnz    TimeDelay, #:RxByte                             '  Resting, loop until timeout
              if_c      clkset  Restart                                         '  Resting and timed-out? Reset Propeller
                        add     BitDelay, cnt
    :RxBit              waitcnt BitDelay, BitTime                               'Wait for center of bit
'                       xor     outa, TxPin
                        test    RxPin, ina              wc                      'Sample bit
                        shr     SByte, #1                                       'Adjust result and store bit
                        muxc    SByte, #%1000_0000                              'Store bit
                        djnz    Bits, #:RxBit
  Receive_ret           ret                                                     'Done; return

'***************************************
'*       Constants and Variables       *
'***************************************

{Constants}
  Restart     long      %10000000                                               'Reboot value (for CLK register)
  IncDest     long      %1_0_00000000                                           'Value to increment a register's destination field
  RxPin       long      |< 31                                                   'Receive pin mask (P31)
  TxPin       long      |< 30                                                   'Transmit pin mask (P30)
  BitTime                                                                       'Bit period (in clock cycles)
   IBitTime   long      80_000_000 / 115_200                     '[host init]      Initial bit period (at startup)
   FBitTime   long      80_000_000 / 230_400                     '[host init]      Final bit period (for download)
  BitTime1_5  long      TRUNC(1.5 * 80_000_000.0 / 230_400.0)    '[host init]    1.5x bit period; used to align to center of received bits
  Timeout     long      80_000_000 * 4 / (2*8)                   '[host init]    Timeout period (2 seconds worth of RxByte loop iterations)

{Variables (initialized)}
  ExpectedID  long      0                                        '[host init]    Expected Packet ID
  Longs       long      0                                                       'Long counter
  MainRAMAddr long      0                                                       'Address in Main RAM

{Variables}
  TimeDelay   res       1                                                       'Timout delay
  BitDelay    res       1                                                       'Bit time delay
  SByte       res       1                                                       'Byte to transmit, or byte received
  Bits        res       1                                                       'Bit counter
  Bytes       res       1                                                       'Byte counter
  PacketAddr  res       1                                                       'PacketAddr
  Packet                                                                        'Packet buffer
   PacketSize res       1                                                       '  Header:  Packet Size
   PacketID   res       1                                                       '  Header:  Packet ID number
   PacketData res       1                                                       '  Payload: Packet data (longs); must be last, extends to end of Cog RAM


' CalcBitTime           mov     BitTime1_5, BitTime                             'Calculate 1.5x bit time (BitTime * 3 / 2)
'                       shl     BitTime1_5, #1
'                       add     BitTime1_5, BitTime
'                       shr     BitTime1_5, #1


'Possibly adjust BitTime1_5 by sample loop time to true it up
