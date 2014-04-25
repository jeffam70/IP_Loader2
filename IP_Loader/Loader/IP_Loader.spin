CON
        _clkmode = xtal1 + pll16x                                               'Standard clock mode * crystal frequency = 80 MHz
        _xinfreq = 5_000_000

PUB Main

  waitcnt(clkfreq + cnt)

  coginit(0, @Loader, 0)

DAT
                        org     0
  Loader                mov     outa, TxPin                                     'Initialize Tx Pin to output high (resting state)
                        mov     dira, TxPin
                        
                        mov     SByte, #$0
                        call    #Tx
                        mov     BitTime, Final
                        jmp     #$
                        
  Tx                    or      SByte, #%1_00000000                             'Append stop bit
                        shl     SByte, #1                                       'Prepend start bit
                        mov     Bits, #10                                       'Ready 10 bits
                        mov     Delay, BitTime                                  'Prep first bit window
                        add     Delay, cnt
                        
        :NextBit        shr     SByte, #1               wc                      'Get next bit
                        waitcnt Delay, BitTime                                  'Wait for edge of bit window
                        muxc    outa, TxPin                                     'Output next bit
                        djnz    Bits, #:NextBit                                 'Loop for next bit                        
  Tx_ret                ret                                                     'Return                        




                        



  TxPin       long      |< 30                                                   'Transmit pin mask (P30)
  BitTime                                                                       'Bit period (in clock cycles)
    Initial   long      80_000_000 / 115_200                                    '  Initial bit period (right after boot)
    Final     long      80_000_000 / 230_400                                    '  Final bit period (remainder of communication)
  BitTime1_5  long      TRUNC(1.5 * 80_000_000.0 / 230_400.0)                   '1.5x bit period; used to align to center of received bits
  Delay       res       1                                                       'Time delay
  SByte       res       1                                                       'Byte to transmit, or byte received
  Bits        res       1                                                       'Bit counter


' CalcBitTime           mov     BitTime1_5, BitTime                             'Calculate 1.5x bit time (BitTime * 3 / 2)
'                       shl     BitTime1_5, #1
'                       add     BitTime1_5, BitTime
'                       shr     BitTime1_5, #1
  