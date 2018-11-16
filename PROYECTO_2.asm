;*******************************************************************************
;    Filename:    PROYECTO_2.asm
;    Autores: Camilo Perafán Montoya & David Vela Aguilera				
;    Description: 
;*******************************************************************************
#include "p16f887.inc"
; CONFIG1
; __config 0xE0F4
 __CONFIG _CONFIG1, _FOSC_INTRC_NOCLKOUT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF & _LVP_OFF
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _BOR4V_BOR40V & _WRT_OFF
;*******************************************************************************
   GPR_VAR        UDATA
   W_TEMP         RES        1      ; w register for context saving (ACCESS)
   STATUS_TEMP    RES        1      ; status used for context saving
   DELAY1	  RES	    1
   DELAY2	  RES	    1
   POT1           RES        1
   POT2           RES        1
   POT3           RES        1
   POT4           RES        1
   CONST          RES        1
   MOTOR1         RES        1
   MOTOR2         RES        1
   MOTOR3         RES        1
   MOTOR4         RES        1
   NUMPOT         RES        1
   VARTM          RES        1
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;*******************************************************************************
ISR       CODE    0x0004           ; interrupt vector location
PUSH:
    MOVWF W_TEMP
    SWAPF STATUS,W
    MOVWF STATUS_TEMP
ISR:
    BTFSS PIR1, TMR1IF  ; TMR1
    GOTO POP

RTMR1:
    INCF PORTE, F
    BCF PIR1, TMR1IF    ; BORRAMOS LA BANDERA DE OVERFLOW DE TIMER1    
    MOVLW B'11001000'      ; N NECESARIO EN TMR1L
    MOVWF TMR1L
    MOVLW B'11111110'      ; N NECESARIO EN TMR1H
    MOVWF TMR1H

POP:
    SWAPF STATUS_TEMP,W
    MOVWF STATUS
    SWAPF W_TEMP,F
    SWAPF W_TEMP,W
    RETFIE
;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

START
;*******************************************************************************
    CALL    CONFIG_RELOJ		; RELOJ INTERNO DE 1MHz
    CALL    CONFIG_IO
    CALL    CONFIG_TX_RX		; 10417hz
    CALL    CONFIG_ADC			; canal 0, fosc/8, adc on, justificado a la izquierda, Vref interno (0-5V)
    CALL    CONFIG_PWM1
    CALL    CONFIG_TMR1
    CALL    CONFIG_INTER
    BANKSEL PORTA
    MOVLW .1
    MOVWF NUMPOT
;*******************************************************************************
   
;*******************************************************************************
; CICLO INFINITO
;*******************************************************************************
LOOP:
 CALL    PRIMERCANAL 
    CALL    MUESTRA_SEND    ; SE ENVIA EL PRIMER DATO
    CALL DELAY_500US
    
    CALL    SEGUNDOCANAL
    CALL    MUESTRA_SEND    ; SE ENVIA EL SEGUNDO DATO
    CALL DELAY_500US
    
    CALL    TERCERCANAL
    CALL    MUESTRA_SEND    ; SE ENVIA EL TERCER DATO
    CALL DELAY_500US
    
    CALL    CUARTOCANAL
    CALL    MUESTRA_SEND    ; SE ENVIA EL CUARTO DATO
    CALL DELAY_500US
    
    MOVLW   .10                 ; CARACTER DE FIN DE LINEA
    CALL    SEND  
    CALL DELAY_500US
    
    GOTO LOOP
     
    
PRIMERCANAL
    BCF ADCON0, CHS3
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BCF ADCON0, CHS0            ; SELECCIONAMOS ANS0 PARA MUESTREAR
    RETURN   
    
SEGUNDOCANAL
    BCF ADCON0, CHS3
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BSF ADCON0, CHS0            ; SELECCIONAMOS ANS1 PARA MUESTREAR
    RETURN

TERCERCANAL
    BCF ADCON0, CHS3
    BCF ADCON0, CHS2
    BSF ADCON0, CHS1
    BCF ADCON0, CHS0            ; SELECCIONAMOS ANS2 PARA MUESTREAR
    RETURN

CUARTOCANAL
    BCF ADCON0, CHS3
    BCF ADCON0, CHS2
    BSF ADCON0, CHS1
    BSF ADCON0, CHS0            ; SELECCIONAMOS ANS3 PARA MUESTREAR
    RETURN
    
MUESTRA_SEND
    CALL    DELAY_500US         ; DELAY (SI ES NECESARIO)
    BSF     ADCON0, GO          ; EMPIECE LA CONVERSIÓN
    BTFSC   ADCON0, GO          ; revisa que terminó la conversión
    GOTO    $-1
    BCF     PIR1, ADIF          ; borramos la bandera del adc
    MOVF    ADRESH, W
    SUBLW   .10                 ; VEMOS SI ES 10 PARA NO ENVIARLO
    BTFSC   STATUS, Z           
    MOVLW   .11
    BTFSS   STATUS, Z
    MOVF    ADRESH, W
    CALL    SEND
    RETURN
    
SEND
    MOVWF   TXREG               ; ENVIAMOS EL VALOR DE LECTURA POR TXREG
    BTFSS   PIR1, TXIF          ; REVISAR SI YA SE ENVIÓ EL NÚMERO, SI YA TERMINO CONTINUA
    GOTO    $-1
    RETURN 
;*******************************************************************************
    CONFIG_RELOJ
    BANKSEL TRISA
    
    BSF OSCCON, IRCF2
    BCF OSCCON, IRCF1
    BCF OSCCON, IRCF0		    ; FRECUECNIA DE 1MHz
    
    RETURN
 
 ;--------------------------------------------------------
    CONFIG_TX_RX
    BANKSEL TXSTA
    BCF	    TXSTA, SYNC		    ; ASINCRÓNO
    BSF	    TXSTA, BRGH		    ; LOW SPEED
    BANKSEL BAUDCTL
    BSF	    BAUDCTL, BRG16	    ; 8 BITS BAURD RATE GENERATOR
    BANKSEL SPBRG
    MOVLW   .25	    
    MOVWF   SPBRG		    ; CARGAMOS EL VALOR DE BAUDRATE CALCULADO
    CLRF    SPBRGH
    BANKSEL RCSTA
    BSF	    RCSTA, SPEN		    ; HABILITAR SERIAL PORT
    BCF	    RCSTA, RX9		    ; SOLO MANEJAREMOS 8BITS DE DATOS
    BSF	    RCSTA, CREN		    ; HABILITAMOS LA RECEPCIÓN 
    BANKSEL TXSTA
    BSF	    TXSTA, TXEN		    ; HABILITO LA TRANSMISION
    
    BANKSEL PORTD
   
    RETURN
;--------------------------------------
CONFIG_TMR1
    BANKSEL PIE1
    BSF PIE1, TMR1IE       ;SE HABILITA LA INTERRUPCIÓN

    BANKSEL T1CON
    MOVLW B'00110001'      ; PRESCALADOR DE 8
                           ; CARGAMOS 65224
    MOVWF T1CON
    MOVLW B'11001000'      ; N NECESARIO EN TMR1L
    MOVWF TMR1L
    MOVLW B'11111110'      ; N NECESARIO EN TMR1H
    MOVWF TMR1H
    RETURN
;--------------------------------------
CONFIG_IO
    BANKSEL TRISA
    CLRF    TRISA
    
    BSF     TRISA,RA0
    BSF     TRISA,RA1
    BSF     TRISA,RA2
    BSF     TRISA,RA3
    
    CLRF    TRISB
    CLRF    TRISC
    CLRF    TRISD
    CLRF    TRISE
    BANKSEL ANSEL
    CLRF    ANSEL
    
    BSF     ANSEL,0
    BSF     ANSEL,1
    BSF     ANSEL,2
    BSF     ANSEL,3
    
    CLRF    ANSELH
    BANKSEL PORTA
    CLRF    PORTA
    CLRF    PORTB
    CLRF    PORTC
    CLRF    PORTD
    CLRF    PORTE
    RETURN    
;-----------------------------------------------
    CONFIG_ADC
    BANKSEL PORTA
    BCF ADCON0, ADCS1
    BSF ADCON0, ADCS0		; FOSC/8 RELOJ TAD

    BANKSEL TRISA
    BCF ADCON1, ADFM		; JUSTIFICACIÓN A LA IZQUIERDA
    BCF ADCON1, VCFG1		; VSS COMO REFERENCIA VREF-
    BCF ADCON1, VCFG0		; VDD COMO REFERENCIA VREF+
    BANKSEL PORTA
    BSF ADCON0, ADON		; ENCIENDO EL MÓDULO ADC
    
    RETURN
;-----------------------------------------------
CONFIG_PWM1
    BANKSEL TRISC
    BSF	    TRISC, RC1		;SE ESTABLECE RC1 / CCP2 COMO ENTRADA
    MOVLW   .155
    MOVWF   PR2			;SE COLOCA EL VALOR DEL PERIODO DE LA SEÑAL DE 20mS
    
    BANKSEL PORTA
    BSF	    CCP2CON, CCP2M3
    BSF	    CCP2CON, CCP2M2
    BSF	    CCP2CON, CCP2M1
    BSF	    CCP2CON, CCP2M0	;MODO PWM
    
    MOVLW   B'00011011'
    MOVWF   CCPR2L		;MSB DEL DUTY CYCLE
    BSF	    CCP2CON, DC2B0
    BSF	    CCP2CON, DC2B1	;LSB DEL DUTY CYCLE
    
    BCF	    PIR1, TMR2IF
    
    BSF	    T2CON, T2CKPS1
    BSF	    T2CON, T2CKPS0	;PRESCALER 1:16
    
    BSF	    T2CON, TMR2ON       ;SE HABILITA EL TMR2
    BTFSS   PIR1, TMR2IF
    GOTO    $-1
    BCF	    PIR1, TMR2IF
    
    BANKSEL TRISC
    BCF	    TRISC, RC1		;RC1 / CCP2 SALIDA PWM
    RETURN
    
;-----------------------------------------------
CONFIG_INTER
    BANKSEL INTCON
    BSF	INTCON,GIE
    BSF	INTCON,T0IE
    BSF	INTCON,PEIE
    RETURN
  
;-----------------------------------------------
DELAY_50MS
    MOVLW   .100		    ; 1US 
    MOVWF   DELAY2
    CALL    DELAY_500US
    DECFSZ  DELAY2		    ;DECREMENTA CONT1
    GOTO    $-2			    ; IR A LA POSICION DEL PC - 1
    RETURN
    
DELAY_500US
    MOVLW   .250		    ; 1US 
    MOVWF   DELAY1	    
    DECFSZ  DELAY1		    ;DECREMENTA CONT1
    GOTO    $-1			    ; IR A LA POSICION DEL PC - 1
    RETURN
    
    END
