!to "PUSHPUZZLE.d64",d64

; To do:
; Record moves and time(?)
; Choosable seed(?)

!zone Constants
std_irq = $ea31
joy_delay = 10
VIC = $d000
sid = $d400
xPos0 = VIC
yPos0 = VIC+1
xPos1 = VIC+2
yPos1 = VIC+3
xPos2 = VIC+4
yPos2 = VIC+5
xPos3 = VIC+6
yPos3 = VIC+7
xPos4 = VIC+8
yPos4 = VIC+9
xPos5 = VIC+10
yPos5 = VIC+11
xPos6 = VIC+12
yPos6 = VIC+13
xPos7 = VIC+14
yPos7 = VIC+15
xposmsb  = VIC+16
col0  = VIC+39
col1  = VIC+40
col2  = VIC+41
col3  = VIC+42
col4  = VIC+43
col5  = VIC+44
col6  = VIC+45
col7  = VIC+46
CHAR_BASE = $2000
CHAR_NUMBERS_BASE = CHAR_BASE + $180
CL_LIGHTGREY = 15
CL_LIGHTBLUE = 14
CL_BLACK = 0
CL_WHITE = 1
CL_MIDGREY = 12
CL_DARKBLUE = 6
CL_DARKGREY = 11
SPRPTR_0 = 2040
SPRPTR_1 = 2041
SPRPTR_2 = 2042
SPRPTR_3 = 2043
SPRPTR_4 = 2044
SPRPTR_5 = 2045
SPRPTR_6 = 2046
SPRPTR_7 = 2047
DM_SIZE = 0
DM_WON = 1
DM_HELP = 2
DM_INFO = 3
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = $ff
MM_GAME = 0
MM_HELP = 1
GM_MI_NEW = 0
GM_MI_SIZE = 1
GM_MI_EXIT = 2
HM_MI_HELP = 0
HM_MI_ABOUT = 1
;Sprite Blocks
SP_GM_BlueRect = GM_BlueRectAddr/64
SP_HM_BlueRect = HM_BlueRectAddr/64
SP_GM_Boundary = GameMenuBoundaryAddr/64
SP_HM_Boundary = HelpMenuBoundaryAddr/64
SP_HM_LBoundary = HelpMenuLeftBoundaryAddr/64
SP_SelectBox  = SelectBoxAddr/64
SP_Mouse0 = Mousepointer0Addr/64
SP_Mouse1 = Mousepointer1Addr/64
SP_FullBlueBox = FullBlueBoxAddr/64
SP_WhiteLine = WhiteLineAddr/64
SP_ButtonLower = ButtonLowerAddr/64
SP_ButtonUpper = ButtonUpperAddr/64
SP_BulletSelector = BulletSelectorAddr/64
SP_WonDlgLeftBnd = WonDlgLeftBndAddr/64
SP_PushBtnBoundary = PushBtnBoundaryAddr/64

!zone BASIC_stub
*=$0801
;** BASIC-Zeile: 2021 SYS2062
 !word main-2, 0
 !byte $9e
 !text "2061"
 !byte $00,$00,$00

!zone Main
main            jsr InputChoice
                jsr SetColors
                jsr MultiColorOn
                jsr InstallCharset
                jsr SetGlobals
                lda #0
                sta Mode
                jsr InstallIRQ
StartGame       ;jsr RestoreIRQ
                jsr Initialize
                jsr ClearScreen
                jsr Shuffle
                jsr DrawWindow
                jsr PrintNumbers
                jsr DrawCaret
                

;Exit codes:
; 00000001 exit program to BASIC
; 00000010 released fire button
; 00000100 moved mouse pointer
; 00001000 pushed fire button
; 00010000 mode changed
MainLoop        lda bModeChanged
                asl
                ora bFire
                asl
                ora bMoved
                asl
                ora bFireReleased
                asl
                ora bExit
                beq MainLoop
                
                sta exit_code
                and #%00000001
                bne exit        ;<--
                lda exit_code
                and #%00000010
                bne FireReleased;<--
                lda exit_code
                and #%00000100
                bne Moving      ;<--
                lda exit_code
                and #%00001000
                bne Fired       ;<--
                lda exit_code
                and #%00010000
                bne ModeChanged

FireReleased    lda #0
                sta bFireReleased
                jsr OnFireRelease
                jmp MainLoop

exit            lda #0
                sta bExit
                sta 198
                sta VIC+21
                jsr MultiColorOff
                lda #21
                sta $d018
                jsr $e544; clear screen
                jsr RestoreIRQ
                lda #CL_LIGHTBLUE
                sta $d020
                lda #CL_DARKBLUE
                sta $d021
                rts

Moving          lda #0
                sta bMoved
                jsr OnMoved
                jmp MainLoop

Fired           lda #0
                sta bFire
                jsr OnFirePressed
                jmp MainLoop

ModeChanged     lda #0
                sta bModeChanged
                jmp StartGame
;=============================================

!zone ____________Events______________
;============================================= OnFirePress
OnFirePressed   lda FireCounter
                cmp #joy_delay
                beq MayFire
                rts
MayFire         lda #0
                sta FireCounter
                lda GameMode
                beq InNormalMode
                bpl InMenuMode
;----- In dialog mode ------------------------
                jsr IsInDlgBtn
                lda res
                bne DlgBtnClicked                
                lda DialogMode
                cmp #DM_SIZE
                bne no_check
                jsr IsInBulletArea
                lda res
                bne BulletClicked
                jsr IsInSoundArea
                lda res
                bne SoundClicked
no_check        rts
BulletClicked   jsr SelectBullet
                rts
SoundClicked    jsr SelectSound
                rts
DlgBtnClicked   jsr PushDlgBtn
                rts
;----- In normal mode ------------------------
InNormalMode    jsr IsInMenuArea
                lda res
                bne MenuClicked
                jsr IsInButtonField
                lda res
                bne ButtonClicked
                rts
MenuClicked     ldx Mode
                lda VIC
                cmp SprHelpMenuPosX,x
                bcs HelpClicked
                ;Game clicked
                jsr ShowGameMenu
                rts
HelpClicked     jsr ShowHelpMenu                
                rts
ButtonClicked   jsr GetButtonData
                jsr PushButton
                rts
;----- In menu mode --------------------------
InMenuMode      lda MenuMode
                bne HelpMenuShown
                jsr IsInGameMenu
                lda res
                beq GotoNormalMode
                rts
HelpMenuShown   jsr IsInHelpMenu
                lda res
                beq GotoNormalMode             
                rts 
GotoNormalMode  lda #%00000011
                sta VIC+21
                jsr RestoreScreen
                lda #GM_NORMAL
                sta GameMode
                rts
;============================================= OnFireRelease

OnFireRelease   lda GameMode
                beq RelInNormalMode
                bpl RelInMenuMode
;----- In dialog mode ------------------------
                jsr ReleaseDlgBtn
                jsr IsInDlgBtn
                lda res
                bne BtnAction
                rts
BtnAction       lda DialogMode
                cmp #DM_SIZE
                bne normal_weiter
                lda bullet_mode
                cmp Mode
                beq normal_weiter
                sta Mode
                lda #1
                sta bModeChanged
normal_weiter   lda #%00000011
                sta VIC+21
                lda #GM_NORMAL
                sta GameMode
                jsr RestoreScreen
                rts
;----- In normal mode ------------------------
RelInNormalMode lda bButtonPressed
                bne release_it
                rts
release_it      jsr ReleaseButton
                jsr MakeGameMove
                rts
;----- In menu mode --------------------------
RelInMenuMode   lda MenuMode
                bne InHelpMenu
                ; Possibly (!) Clicked in Game Menu
                jsr IsInGameMenu
                lda res
                bne GM_Action
                rts
GM_Action       jsr GotoNormalMode
                jsr GM_Actions
                rts
InHelpMenu      ; Possibly (!) Clicked in Help Menu
                jsr IsInHelpMenu
                lda res
                bne HM_Action
                rts
HM_Action       jsr GotoNormalMode
                jsr HM_Actions
                rts                
;============================================= OnMouseMove

OnMoved         lda GameMode
                beq MovInNormalMode
                bpl MovInMenuMode
;----- In dialog mode ------------------------
                lda bFirePressed
                bne fireDLGpressed
                rts
fireDLGpressed  lda #<DlgBtnRect
                sta $fb
                lda #>DlgBtnRect
                sta $fc
                jsr CopyRect
                jsr IsInRect
                lda res
                bne in_DLGbutton
                jsr ReleaseDlgBtn
                rts
in_DLGbutton    jsr PushDlgBtn
                rts
;----- In normal mode ------------------------
MovInNormalMode lda bFirePressed
                bne fire_is_pressed
                rts
fire_is_pressed lda #<PressedBtnRect
                sta $fb
                lda #>PressedBtnRect
                sta $fc
                jsr CopyRect
                jsr IsInRect
                lda res
                bne in_button
                jsr ReleaseButton
                rts
in_button       jsr PushButton      
                rts
;----- In menu mode --------------------------
MovInMenuMode   lda MenuMode
                bne moved_HM_shown
                ;moved while game menu shown
                jsr IsInGameMenu
                lda res
                bne moved_in_GM
                rts
moved_in_GM     ldx Mode
                lda VIC+1
                sec
                sbc #8
                sec
                sbc SprMenuPosY,x
                lsr
                lsr
                lsr
                sta SelMenuItem
                jsr GM_SelMenuItem
                rts
moved_HM_shown  jsr IsInHelpMenu
                lda res
                bne moved_in_HM
                rts
moved_in_HM     ldx Mode
                lda VIC+1
                sec
                sbc #8
                sec
                sbc SprMenuPosY,x
                lsr
                lsr
                lsr
                sta SelMenuItem
                jsr HM_SelMenuItem
                rts
;=============================================

!zone GameButtonActions
;Draws a button by knowledge of OldCaretPos
DrawBtnAtOldCaret   ;get x/y screen positions first
                lda #<RelBtnMap;;;;;;;;;;;<----------
                sta $fb
                lda #>RelBtnMap;;;;;;;;;;;<----------
                sta $fc
                lda OldCaretPos
                jsr Draw3x3Map; in zone graphics
                rts
                
;Draws the caret by knowledge of CaretPos
DrawCaret       ;get x/y screen positions first
                lda #<CaretMap
                sta $fb
                lda #>CaretMap
                sta $fc
                lda CaretPos
                jsr Draw3x3Map
                rts

;returns bool in res
IsInButtonField ldx Mode
                lda FieldPixelDim,x
                sta Rect+2
                sta Rect+3
                
                lda SprGameMenuPosX,x
                clc
                adc #8
                sta Rect
                clc
                adc Rect+2
                sta Rect+2
                
                lda SprMenuPosY,x
                clc
                adc #16
                sta Rect+1
                clc
                adc Rect+3
                sta Rect+3
                jsr IsInRect
                rts

ReleaseButton   lda bButtonPressed
                bne mayrelease
                rts
mayrelease      lda #<RelBtnMap
                sta $fb
                lda #>RelBtnMap
                sta $fc
                lda #3
                sta MapWidth
                sta MapHeight
                lda PressedBtnIndex
                jsr Draw3x3Map
                lda #0
                sta bButtonPressed
                ldy #41
                jsr AddYtoFDFE
                ldy #0
                lda PrsBtnNoCharInd
                sta ($fd),y
                ;move number by 1 pixel left and up
                lda NumberCharDefLo
                sta $fb
                lda NumberCharDefHi
                sta $fc
                ldy #1
loopy2          iny
                lda ($fb),y
                asl
                dey
                sta ($fb),y
                iny
                cpy #8
                bne loopy2
                ;Sprites
                lda #%00000011
                sta VIC+21
                rts

PushButton      lda bButtonPressed
                beq maypress
                rts
maypress        lda PressedBtnIndex
                cmp CaretPos
                bne isbutton
                rts
isbutton        lda #<PushBtnMap
                sta $fb
                lda #>PushBtnMap
                sta $fc
                lda #3
                sta MapWidth
                sta MapHeight
                lda PressedBtnIndex
                jsr Draw3x3Map
                lda #1
                sta bButtonPressed
                ldy #41
                jsr AddYtoFDFE
                ldy #0
                lda PrsBtnNoCharInd
                sta ($fd),y
                ;move number by 1 pixel right and down
                lda NumberCharDefLo
                sta $fb
                lda NumberCharDefHi
                sta $fc
                ldy #7
loopy           dey
                lda ($fb),y
                lsr
                iny
                sta ($fb),y
                dey
                bne loopy
                ;Sprites
                lda #SP_PushBtnBoundary
                sta SPRPTR_2
                sta SPRPTR_3
                lda #SP_WhiteLine
                sta SPRPTR_4
                lda #CL_DARKGREY
                sta col2
                sta col4
                lda #CL_WHITE
                sta col3
                lda PressedBtnRect
                sta xPos2
                sta xPos4
                clc
                adc #23
                sta xPos3
                lda PressedBtnRect+1
                sta yPos2
                sta yPos3
                sta yPos4
                lda #%00001100; both y-stretched
                sta VIC+23
                lda #0
                sta VIC+29
                sta VIC+28
                lda #%00011111
                sta VIC+21
                rts

;Fills button related bytes depending on 
;current cursor position
GetButtonData   ;cursor pos in screen x,y coords
                lda VIC
                sec
                sbc #24
                lsr
                lsr
                lsr
                sta Rect
                lda VIC+1
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta Rect+1
                ;left and upper bounds of button field
                ldx Mode
                lda WinPosX,x
                clc
                adc #2
                sta Rect+2
                lda WinPosY,x
                clc
                adc #4
                sta Rect+3
                ;get button x-pos and x-index
                lda Rect
                sec
                sbc Rect+2
                jsr Mod3
                sta Rect+2
                lda Rect
                sec
                sbc Rect+2
                sta PressedBtnX; x pos of button
                stx PressedBtnIndX
                ;get button y-pos and y-index
                lda Rect+1
                sec
                sbc Rect+3
                jsr Mod3
                sta Rect+3
                lda Rect+1
                sec
                sbc Rect+3
                sta PressedBtnY; y pos of button
                stx PressedBtnIndY
                ;fill PressedBtnRect
                lda PressedBtnX
                asl
                asl
                asl
                clc
                adc #24
                sta PressedBtnRect
                clc
                adc #23
                sta PressedBtnRect+2
                lda PressedBtnY
                asl
                asl
                asl
                clc
                adc #50
                sta PressedBtnRect+1
                clc
                adc #24
                sta PressedBtnRect+3
                ;get number on button
                ;a * indY + indX
                lda PressedBtnIndY              ;<--
                sta factor
                lda Mode
                clc
                adc #3
                jsr Mult; a = a * indY
                clc
                adc PressedBtnIndX              ;<--
                sta PressedBtnIndex             ;<-- 0 - WH^2-1 (important)
                tax
                lda Numbers,x
                clc
                adc #47;corresponding char index
                sta PrsBtnNoCharInd             ;<--
                sta $fb
                lda #0
                sta $fc
                ;FBFC * 8
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                lda #<CHAR_BASE
                clc
                adc $fb
                sta NumberCharDefLo             ;<--
                lda #>CHAR_BASE
                adc $fc
                sta NumberCharDefHi             ;<--   
                rts


!zone MenuActions
GM_Actions      lda SelMenuItem
                cmp #GM_MI_EXIT
                beq ACTION_EXIT
                cmp #GM_MI_SIZE
                beq ACTION_SIZE
                cmp #GM_MI_NEW
                beq ACTION_NEW
ACTION_EXIT     lda #1
                sta bExit
                rts
ACTION_SIZE     ;shows size select dialog with
                ;bullet set correctly
                lda Mode
                sta bullet_mode
                jsr SelectMode
                rts
ACTION_NEW      lda CaretPos
                sta OldCaretPos                
                jsr Initialize
                jsr Shuffle
                jsr DrawBtnAtOldCaret
                jsr PrintNumbers
                jsr DrawCaret
                rts

HM_Actions      lda SelMenuItem
                cmp #HM_MI_HELP
                beq ACTION_HELP
                cmp #HM_MI_ABOUT
                beq ACTION_ABOUT
ACTION_HELP     jsr ShowHelpDlg
                rts
ACTION_ABOUT    jsr ShowInfoDlg
                rts

;=============================================
!zone Joystick
Joystick        jsr JoyDecoder
                bcs NoFire
                ;Fire pressed
                lda bFirePressed
                bne OnlyMove;fire was also pressed before
                lda #1;fire not pressed before
                sta bFirePressed
                sta bFire
                jmp OnlyMove
NoFire          ;Fire not pressed
                lda bFirePressed
                beq OnlyMove;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda #1
                sta bFireReleased
OnlyMove        lda dx
                ora dy
                bne move
                rts
move            lda #1
                sta bMoved
                lda dx
                asl
                clc
                adc VIC
                sta VIC
                sta VIC+2
                lda dx
                bpl right
                ;left
                bcc ovf_lr
                lda $d010
                and #%00000011
                bne change_y
                lda VIC
                cmp #24
                bcs change_y
                lda #24
                sta VIC
                sta VIC+2
                jmp change_y
right           bcs ovf_lr

                lda $d010
                and #%00000011
                beq change_y
                lda VIC
                cmp #88
                bcc change_y
                lda #87
                sta VIC
                sta VIC+2
                jmp change_y
                
                jmp change_y
ovf_lr          lda $d010
                eor #%00000011
                sta $d010
change_y        lda dy
                asl
                clc
                adc VIC+1
                sta VIC+1
                sta VIC+3
                cmp #50
                bcc correct_yup
                cmp #250
                bcs correct_ydown
                rts
correct_yup     lda #50
                sta VIC+1
                sta VIC+3
                rts
correct_ydown   lda #249
                sta VIC+1
                sta VIC+3
                rts


JoyDecoder      lda $dc00     ; get input from port 2 only
                ldy #0        ; this routine reads and decodes the
                ldx #0        ; joystick/firebutton input data in
                lsr           ; the accumulator. this least significant
                bcs djr0      ; 5 bits contain the switch closure
                dey           ; information. if a switch is closed then it
djr0            lsr           ; produces a zero bit. if a switch is open then
                bcs djr1      ; it produces a one bit. The joystick dir-
                iny           ; ections are right, left, forward, backward
djr1            lsr           ; bit3=right, bit2=left, bit1=backward,
                bcs djr2      ; bit0=forward and bit4=fire button.
                dex           ; at rts time dx and dy contain 2's compliment
djr2            lsr           ; direction numbers i.e. $ff=-1, $00=0, $01=1.
                bcs djr3      ; dx=1 (move right), dx=-1 (move left),
                inx           ; dx=0 (no x change). dy=-1 (move up screen),
djr3            lsr           ; dy=1 (move down screen), dy=0 (no y change).
                stx dx        ; the forward joystick position corresponds
                sty dy        ; to move up the screen and the backward
                rts           ; position to move down screen.
                              ;
                              ; at rts time the carry flag contains the fire
                              ; button state. if c=1 then button not pressed.
                              ; if c=0 then pressed.

!zone Mouse
potx     = sid+$19
poty     = sid+$1a

maxx     = 319 ;Screen Width
maxy     = 199 ;Screen Height
offsetx  = 24  ;Sprite left border edge
offsety  = 50  ;Sprite top  border edge
acc      = 127 ;accelaration
musposx  !word 160
musposy  !word 100
old_yPos !byte 0
old_xPos !byte 0

Mouse           jsr GetClicks
                jsr scanmovs
                jsr boundmus
                rts

;---------------------------------------
GetClicks       lda $dc01
                cmp #$ef
                bne noFire
                ;Fire pressed
                lda bFirePressed
                bne out_here;fire was also pressed before
                lda #1;fire not pressed before
                sta bFirePressed
                sta bFire
                rts
noFire          ;Fire not pressed
                lda bFirePressed
                beq out_here;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda #1
                sta bFireReleased
out_here        rts
;---------------------------------------

scanmovs        ;--- X Axis ---
                lda potx
oldpotx         ldy #0
                jsr movechk
                beq noxmove

                sty oldpotx+1

                clc
                adc musposx
                sta musposx
                txa            ;upper 8-bits
                adc musposx+1
                sta musposx+1
noxmove         ;--- Y Axis ---
                lda poty
oldpoty         ldy #0
                jsr movechk
                beq noymov

                sty oldpoty+1

                clc
                eor #$ff       ;Reverse Sign
                adc #1

                clc
                adc musposy
                sta musposy
                txa            ;Upper 8-bits
                eor #$ff       ;Reverse Sign
                adc musposy+1
                sta musposy+1
noymov          rts
;---------------------------------------
movechk         ;Y -> Old Pot Value
                ;A -> New Pot Value
                sty oldvalue+1
                tay
                sec

oldvalue        sbc #$ff
                and #%01111111
                cmp #%01000000
                bcs neg

                lsr a   ;remove noise bit
                beq nomove

                cmp #acc ;Acceleration Speed
                bcc *+3
                asl a   ;X2

                ldx #0
                cmp #0

                ;A > 0
                ;X = 0 (sign extension)
                ;Y = newvalue
                ;Z = 0

                rts

neg             ora #%10000000
                cmp #$ff
                beq nomove

                sec    ;Keep hi negative bit
                ror a  ;remove noise bit

                cmp #256-acc ;Acceleration Speed
                bcs *+3
                asl a       ;X2

                ldx #$ff

                ;A < 0
                ;X = $ff (sign extension)
                ;Y = newvalue
                ;Z = 0

                ;fallthrough

nomove          ;A = -
                ;X = -
                ;Y = -
                ;Z = 1
                rts

;---------------------------------------
boundmus        ldx musposx+1
                bmi zerox
                beq chky

                ldx #maxx-256
                cpx musposx
                bcs chky

                stx musposx
                bcc chky

zerox           ldx #0
                stx musposx
                stx musposx+1

chky            ldy musposy+1
                bmi zeroy
                beq loychk

                dec musposy+1
                ldy #maxy
                sty musposy
                bne movemus

loychk          ldy #maxy
                cpy musposy
                bcs movemus

                sty musposy
                bcc movemus

zeroy           ldy #0
                sty musposy
                sty musposy+1

movemus         lda xPos0
                sta old_xPos
                
                clc
                lda musposx
                adc #offsetx
                sta xPos0
                sta xPos1
                
                lda musposx+1
                adc #0
                beq clearxhi

                ;set x sprite pos high
                lda xposmsb
                ora #%00000011         
                bne *+7
         
clearxhi        ;set x sprite pos low
                lda xposmsb
                and #%11111100
                sta xposmsb
                
                lda xPos0
                cmp old_xPos
                beq make_ymove
                lda #1
                sta bMoved

make_ymove      lda yPos0
                sta old_yPos
                
                clc
                lda musposy
                adc #offsety
                sta yPos0
                sta yPos1

                cmp old_yPos
                beq no_ymove
                lda #1
                sta bMoved
no_ymove        rts

;---------------------------------------


!zone Preparations
Initialize      ;Get numbers in order
                lda #35
                ldx #34
init_loop       sta Numbers,x
                txa
                dex
                bpl init_loop
                ;Set game-relevant values
                ; WH = Mode+3
                ; WHSquared = WH^2
                ; WHTimes3 = WH * 3
                ; CaretPos = WH^2 - 1
                lda Mode
                clc
                adc #3
                sta WH
                sta factor
                jsr Mult
                sta WHSquared
                tax
                dex
                stx CaretPos
                lda #3
                sta factor
                lda WH
                jsr Mult
                sta WHTimes3
                rts

;Once called, never changes
SetGlobals      lda #CL_WHITE
                sta VIC+37; white as MC1 for sprites
                lda #CL_MIDGREY
                sta VIC+38; mid grey as MC2 for sprites
                
                ; FillMapsTable
                lda #<Map3
                sta MapsLo
                lda #>Map3
                sta MapsHi
                
                lda #<Map4
                sta MapsLo+1
                lda #>Map4
                sta MapsHi+1
                
                lda #<Map5
                sta MapsLo+2
                lda #>Map5
                sta MapsHi+2

                lda #<Map6
                sta MapsLo+3
                lda #>Map6
                sta MapsHi+3
                
                ;Install mouse pointer sprite                
                lda #SP_Mouse0
                sta SPRPTR_0
                lda #SP_Mouse1
                sta SPRPTR_1
                lda #CL_BLACK
                sta VIC+39
                lda #CL_WHITE
                sta VIC+40
                lda #100
                sta VIC
                sta VIC+2
                sta VIC+1
                sta VIC+3
                lda #%00000011
                sta VIC+21
                
                lda #15
                sta sid+24
                rts

!zone IRQ
RasterIRQ       lda $d019
                sta $d019
                
                lda #CL_LIGHTGREY
                sta $d021
                lda $02
                beq is_zero
                lda #0
                sta $02
                sta $d012
                jmp std_irq
is_zero         lda #1
                sta $02
                ldx Mode
                lda rasters,x
                sta $d012
                lda #CL_DARKBLUE
                sta $d021
                
                lda FireCounter
                cmp #joy_delay
                beq no_count
                inc FireCounter
no_count        jsr Tada
                lda MouseOn
                bne mouse
                jsr Joystick
                jmp std_irq
mouse           jsr Mouse
                jmp std_irq


InstallIRQ      sei                                ;Interrupts sperren

                lda #<RasterIRQ                    ;unsere Interrupt-Routine
                sta $0314                          ;in den IRQ-Vector eintragen
                lda #>RasterIRQ                    ;auch das MSB
                sta $0315

                lda #0
                sta $d012                          ;Raster-IRQ ausglösen bei 0
                lda $d011                          ;Zur Sicherheit auch noch
                and #%01111111                     ;das höchste Bit für den
                sta $d011                          ;gewünschten Raster-IRQ löschen 

                lda $d01a                          ;IRQs vom
                ora #%00000001                     ;VIC-II aktivieren
                sta $d01a

                lda #%01111111  ; disable CIA irq: Bit 7 sets the value, Bit 0...4 selects the bits to be set
                sta $dc0d
                lda $dc0d       ; acknowledge any pending CIA irq

                cli                                ;Interrupts wieder erlauben
                rts

RestoreIRQ      sei
                lda #$31
                sta $0314
                lda #$ea
                sta $0315
                
                lda $d01a
                and #%11111110
                sta $d01a
    
                lda #%11111111  ; enable CIA irq: Bit 7 sets the value, Bit 0...4 selects the bits to be set
                sta $dc0d
                lda $dc0d       ; acknowledge any pending CIA irq    
                cli
rts
                
!zone Graphics
;expects 3x3 map in FBFC
;draws map at index in a
Draw3x3Map      ;get x/y screen positions first
                ldx WH
                stx modulo
                ldx #3
                stx factor
                jsr Mod; oldA = x * WH + a
                stx Rect+1
                jsr Mult; 3 * a
                sta Rect
                lda Rect+1
                jsr Mult
                sta Rect+1
                ldx Mode
                lda WinPosX,x
                clc
                adc #2
                clc
                adc Rect
                sta Rect
                lda WinPosY,x
                clc
                adc #4
                clc
                adc Rect+1
                sta Rect+1; x/y screen positions in Rect
                lda #3
                sta MapWidth
                sta MapHeight
                ldx Rect+1
                lda ScrTabLo,x
                sta $fd
                lda ScrTabHi,x
                sta $fe
                ldy Rect
                jsr AddYtoFDFE
                jsr DrawMap
                rts

;copies rect in pointer $fb to Rect
CopyRect        ldy #3
copyloop        lda ($fb),y
                sta Rect,y
                dey
                bpl copyloop
                rts

;returns bool in res
;Sprite coord Rect ONLY MADE FROM 4 BYTES!!!
IsInRect        lda #0
                sta res
                lda $d010
                and #%00000011
                bne raus
                ;check Y
                lda VIC+1
                cmp Rect+1
                bcc raus
                cmp Rect+3
                bcs raus
                ;check X
                lda VIC
                cmp Rect
                bcc raus
                cmp Rect+2
                bcs raus
                lda #1
                sta res
raus            rts

SaveScreen      ldy #3
ss_outer        ldx #0
ss_inner        lda $0400,x ; bb ll hh
                sta $c400,x ; bb ll hh
                lda $d800,x ; bb ll hh
                sta $c800,x ; bb ll hh
                dex
                bne ss_inner
                inc ss_inner+2
                inc ss_inner+5
                inc ss_inner+8
                inc ss_inner+11
                dey
                bpl ss_outer
                lda #$04
                sta ss_inner+2
                lda #$c4
                sta ss_inner+5
                lda #$d8
                sta ss_inner+8
                lda #$c8
                sta ss_inner+11
                rts
                
RestoreScreen   ldy #3
rs_outer        ldx #0
rs_inner        lda $c400,x ; bb ll hh
                sta $0400,x ; bb ll hh
                lda $c800,x ; bb ll hh
                sta $d800,x ; bb ll hh
                dex
                bne rs_inner
                inc rs_inner+2
                inc rs_inner+5
                inc rs_inner+8
                inc rs_inner+11
                dey
                bpl rs_outer
                lda #$c4
                sta rs_inner+2
                lda #$04
                sta rs_inner+5
                lda #$c8
                sta rs_inner+8
                lda #$d8
                sta rs_inner+11
                rts
                
SetColors       lda #CL_LIGHTGREY
                sta $d021
                lda #CL_BLACK
                sta $d020
                lda #CL_LIGHTBLUE
                sta $d022
                lda #CL_MIDGREY
                sta $d023
                rts
                
MultiColorOn    lda $d016
                ora #$10
                sta $d016
                rts
                
MultiColorOff   lda $d016
                and #239
                sta $d016
                rts
                
InstallCharset  ; Char set at $2000
                lda $d018
                and #%11110001
                ora #%00001000
                sta $d018
                rts

ClearScreen     ldx #0
                ldy #8      ; farbe für später
clear_loop      lda #42     ; zeichen
                sta $0400,x
                sta $0500,x
                sta $0600,x
                sta $06e8,x ; nur bis 1000er-grenze
                tya         ; farbe
                sta $d800,x
                sta $d900,x
                sta $da00,x
                sta $dae8,x ; nur bis 1000er-grenze
                inx
                bne clear_loop
                rts
                
DrawWindow      ldx Mode
                lda MapsLo,x
                sta $fb
                lda MapsHi,x
                sta $fc
                lda WinPosLo,x
                sta $fd
                lda WinPosHi,x
                sta $fe
                lda WinDimX,x
                sta MapWidth
                lda WinDimY,x
                sta MapHeight
                jsr DrawMap
                rts           

;Expects:
; map pos in $fb
; map width in MapWidth
; map height in MapHeight
; upper left screen mem location in $fd
DrawMap         lda $fd
                pha
                lda $fe
                pha
                ldx MapHeight
                dex
                stx counter
outer_draw      lda MapWidth
                tay
                dey
draw_map        lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                tax
                lda $fe; add $d4 to $fe
                clc
                adc #$d4
                sta $fe
                
                lda Colors,x
                sta ($fd),y 
                
                lda $fe; subtract $d4 from $fe
                sec
                sbc #$d4
                sta $fe
                ;----------------------
                dey
                bpl draw_map
                
                ldy MapWidth
                jsr AddYtoFBFC
                ldy #40
                jsr AddYtoFDFE
                
                dec counter
                bpl outer_draw
                pla
                sta $fe
                pla
                sta $fd
                rts


!zone GeneralDialogRoutines
IsInDlgBtn      lda #<DlgBtnRect
                sta $fb
                lda #>DlgBtnRect
                sta $fc
                jsr CopyRect
                jsr IsInRect
                rts
                
;Dialog button ALWAYS in sprites 5 and 6!!!
ReleaseDlgBtn   lda #CL_WHITE
                sta col6
                lda #CL_MIDGREY
                sta col5
                rts
                
PushDlgBtn      lda #CL_WHITE
                sta col5
                lda #CL_DARKGREY
                sta col6
                rts

PrepareMap      ldy #11
                lda Mode
                cmp #2
                bcc mode01
                lda #131
                sta ($fb),y
                rts
mode01          lda #159
                sta ($fb),y
                rts

ShowInfoDlg     jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_INFO
                sta DialogMode
                lda #<InfoDlgMap
                sta $fb
                lda #>InfoDlgMap
                sta $fc
                jsr CompleteDlg
                rts

ShowWonDlg      jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_WON
                sta DialogMode
                lda #<WonDlgMap
                sta $fb
                lda #>WonDlgMap
                sta $fc
                jsr CompleteDlg
                rts

ShowHelpDlg     jsr SaveScreen
                lda #GM_DIALOG
                sta GameMode
                lda #DM_HELP
                sta DialogMode
                lda #<HelpDlgMap
                sta $fb
                lda #>HelpDlgMap
                sta $fc
                jsr CompleteDlg
                rts

CompleteDlg     lda #12
                sta MapWidth
                lda #8
                sta MapHeight
                ldx Mode
                lda WinPosLo,x
                sta $fd
                lda WinPosHi,x
                sta $fe
                ldy #204
                jsr AddYtoFDFE
                jsr PrepareMap
                jsr DrawMap
                
                ;Sprites
                lda #SP_FullBlueBox
                sta SPRPTR_2
                sta SPRPTR_3
                lda #SP_WhiteLine
                sta SPRPTR_4
                lda #SP_ButtonLower
                sta SPRPTR_5
                lda #SP_ButtonUpper
                sta SPRPTR_6
                lda #SP_WonDlgLeftBnd
                sta SPRPTR_7
                ;colors
                lda #CL_DARKBLUE
                sta col2
                sta col3
                lda #CL_WHITE
                sta col4
                sta col6
                sta col7
                lda #CL_MIDGREY
                sta col5
                ;positions
                ;-----------------X
                ldx Mode
                lda SprGameMenuPosX,x
                clc
                adc #24
                sta xPos7
                clc
                adc #2
                sta xPos2
                clc
                adc #42
                sta xPos3
                clc
                adc #20
                sta xPos5
                sta xPos6
                clc
                adc #6
                sta xPos4
                ;-----------------Y
                lda SprMenuPosY,x
                clc
                adc #24
                sta yPos4
                tax
                inx
                stx yPos2
                stx yPos3
                clc
                adc #46
                sta yPos5
                sta yPos6
                clc
                adc #10
                sta yPos7
                ;other attribs
                lda #%00001100
                sta VIC+29;stretch x
                lda #%00001100
                sta VIC+27;bkg priority
                lda #0
                sta VIC+23; no y stretch
                sta VIC+28; no MC sprites
                lda #%11111111
                sta VIC+21
                ;fill DlgBtnRect
                lda xPos6
                sta DlgBtnRect
                clc
                adc #24
                sta DlgBtnRect+2
                lda yPos6
                sta DlgBtnRect+1
                clc
                adc #11
                sta DlgBtnRect+3
                rts


!zone SizeDialogRoutines
ShowSizeDlg     lda #GM_DIALOG
                sta GameMode
                lda #DM_SIZE
                sta DialogMode
                lda #<SizeDlgMap
                sta $fb
                lda #>SizeDlgMap
                sta $fc
                lda #10
                sta MapWidth
                lda #7
                sta MapHeight
                ldx Mode
                lda WinPosLo,x
                sta $fd
                lda WinPosHi,x
                sta $fe
                ldy #121
                jsr AddYtoFDFE                
                jsr DrawMap
                jsr ShowSoundOnOff
                
                ;Sprites
                lda #SP_FullBlueBox
                sta SPRPTR_2
                sta SPRPTR_3
                lda #SP_WhiteLine
                sta SPRPTR_4
                lda #SP_ButtonLower
                sta SPRPTR_5
                lda #SP_ButtonUpper
                sta SPRPTR_6
                ;colors
                lda #CL_DARKBLUE
                sta col2
                sta col3
                lda #CL_WHITE
                sta col4
                sta col6
                lda #CL_MIDGREY
                sta col5
                ;positions
                ;-----------------X
                ldx Mode
                lda SprGameMenuPosX,x
                sta xPos2
                clc
                adc #28
                sta xPos3
                clc
                adc #20
                sta xPos5
                sta xPos6
                clc
                adc #6
                sta xPos4
                ;-----------------Y
                lda SprMenuPosY,x
                clc
                adc #9
                sta yPos2
                sta yPos3
                tax
                dex
                stx yPos4
                clc
                adc #14
                sta yPos5                
                sta yPos6
                ;other attribs
                lda #%00001100
                sta VIC+29;stretch x (MSB for selector)
                lda #%10001100
                sta VIC+27;bkg priority
                lda #0
                sta VIC+23; no y stretch
                sta VIC+28; no MC sprites
                lda #%01111111
                sta VIC+21
                ;fill DlgBtnRect
                lda xPos6
                sta DlgBtnRect
                clc
                adc #24
                sta DlgBtnRect+2
                lda yPos6
                sta DlgBtnRect+1
                clc
                adc #10
                sta DlgBtnRect+3
                rts
                
SelectBullet    ldx Mode
                lda VIC+1
                sec
                sbc SprMenuPosY,x
                sec
                sbc #24
                lsr
                lsr
                lsr
                sta bullet_mode
                sta SelectedMode
                jsr SelectMode
                rts

IsInBulletArea  ldx Mode
                lda SprGameMenuPosX,x
                clc
                adc #8
                sta Rect
                clc
                adc #24
                sta Rect+2
                lda SprMenuPosY,x
                clc
                adc #24
                sta Rect+1
                clc
                adc #32
                sta Rect+3
                jsr IsInRect
                rts

IsInSoundArea   ldx Mode
                lda SprGameMenuPosX,x
                clc
                adc #40
                sta Rect
                clc
                adc #32
                sta Rect+2
                lda SprMenuPosY,x
                clc
                adc #48
                sta Rect+1
                clc
                adc #8
                sta Rect+3
                jsr IsInRect
                rts

ShowSoundOnOff  ldx Mode
                lda WinPosY,x
                clc
                adc #8
                tay
                lda ScrTabLo,y
                sta $fb
                lda ScrTabHi,y
                sta $fc
                lda WinPosX,x
                clc
                adc #6
                tay
                lda #114
                clc
                adc SoundOn
                sta ($fb),y
                rts

UpdateOptDlgMap lda #114
                clc
                adc SoundOn
                sta SizeDlgMap + $37
                rts
                
SelectSound     lda SoundOn
                eor #%00000001
                sta SoundOn
                jsr ShowSoundOnOff
                jsr UpdateOptDlgMap
                rts
                
;expects mode in bullet_mode
SelectMode      jsr ShowSizeDlg
                lda #SP_BulletSelector
                sta SPRPTR_7
                lda #CL_DARKBLUE
                sta col7
                ;bullet sprite position:
                ;x: SprGameMenuPosX,x + 9
                ;y: SprMenuPosY,x + 25 + mode * 8
                ;bullet text pos
                ;x: WinPosX,x + 2
                ;y: WinPosY,x + 5 + mode
                ldx Mode
                lda SprGameMenuPosX,x
                clc
                adc #9
                sta xPos7
                lda SprMenuPosY,x
                clc
                adc #17
                ldy bullet_mode
yloop           clc
                adc #8
                dey
                bpl yloop
                sta yPos7
                lda #%11111111
                sta VIC+21
                ;make bullet white
                lda WinPosY,x
                clc
                adc #5
                clc
                adc bullet_mode
                tay
                lda ScrTabLo,y
                sta $fb
                lda ScrTabHi,y
                clc
                adc #$d4
                sta $fc
                lda WinPosX,x
                clc
                adc #2
                tay
                jsr AddYtoFBFC
                ldy #0
                lda #CL_WHITE
                sta ($fb),y
                rts
bullet_mode     !byte 0


!zone MenuRoutines
;returns bool in res
IsInGameMenu    ldx Mode
                lda SprGameMenuPosX,x
                sta Rect
                clc
                adc #48
                sta Rect+2
                lda SprMenuPosY,x
                clc
                adc #8
                sta Rect+1
                clc
                adc #24
                sta Rect+3
                jsr IsInRect
                rts

;returns bool in res                
IsInHelpMenu    ldx Mode
                lda SprGameMenuPosX,x
                clc
                adc #24
                sta Rect
                clc
                adc #40
                sta Rect+2
                lda SprMenuPosY,x
                clc
                adc #8
                sta Rect+1
                clc
                adc #16
                sta Rect+3
                jsr IsInRect
                rts
                
;returns bool in res
IsInMenuArea    ldx Mode
                lda SprGameMenuPosX,x
                sta Rect
                clc
                adc #48
                sta Rect+2
                lda SprMenuPosY,x
                sta Rect+1
                clc
                adc #8
                sta Rect+3
                jsr IsInRect
                rts

;Expects menu item id in SelMenuItem
HM_SelMenuItem  lda #SP_SelectBox
                sta SPRPTR_7
                ;------------------X
                ldx Mode
                lda SprHelpMenuPosX,x
                sta VIC+14
                ;------------------Y
                lda #7
                clc
                adc SprMenuPosY,x
                sta VIC+13
                lda SelMenuItem
                asl
                asl
                asl
                clc
                adc #8
                clc
                adc SprMenuPosY,x
                sta VIC+15
                ;color
                lda #CL_DARKBLUE
                sta VIC+46
                ;stretch
                lda #%10111000
                sta VIC+29; stretch X
                ;select box behind text
                lda #%10001000
                sta VIC+27
                lda #%11111011; sprite 2 not used
                sta VIC+21
                
                ;sel item->white, others->black
                jsr DrawHelpMenuMap
                ldy #84
                jsr SelectText
                rts

;Expects menu item id in SelMenuItem
GM_SelMenuItem  lda #SP_FullBlueBox
                sta SPRPTR_7
                ;------------------X
                ldx Mode
                lda SprGameMenuPosX,x
                sta VIC+14
                ;------------------Y
                lda SelMenuItem
                asl
                asl
                asl
                clc
                adc #8
                clc
                adc SprMenuPosY,x
                sta VIC+15
                ;color
                lda #CL_DARKBLUE
                sta VIC+46
                ;stretch
                lda #%10111000
                sta VIC+29; stretch X
                ;select box behind text
                lda #%10001000
                sta VIC+27
                lda #%10111011
                sta VIC+21
                
                ;sel item->white, others->black
                jsr DrawGameMenuMap
                ldy #81
                jsr SelectText
                rts
                
SelectText      ldx Mode
                lda WinPosLo,x
                sta $fb
                lda WinPosHi,x
                sta $fc
                ldx #$d4
                jsr AddYXtoFBFC
                ldx SelMenuItem
                inx
more40s         ldy #40
                jsr AddYtoFBFC
                dex
                bne more40s; found correct color pos
                lda #CL_WHITE
                ldy MapWidth
                dey
white_it        sta ($fb),y
                dey
                bpl white_it
                rts
                
ShowGameMenu    lda #GM_MENU
                sta GameMode
                lda #MM_GAME
                sta MenuMode
                jsr SaveScreen
                ;Sprites
                lda #SP_GM_BlueRect
                sta SPRPTR_3
                lda #SP_GM_Boundary
                sta SPRPTR_4
                sta SPRPTR_5
                ;positions
                ;-------------X
                ldx Mode
                ldy SprGameMenuPosX,x
                dey
                sty xPos3
                iny
                sty xPos5
                iny
                sty xPos4
                ;-------------Y
                ldy SprMenuPosY,x
                sty yPos3
                sty yPos5
                iny
                sty yPos4
                ;colors
                lda #CL_DARKBLUE
                sta col3
                lda #CL_BLACK
                sta col4
                lda #CL_MIDGREY
                sta col5
                ;MC, stretch
                lda #%00001000
                sta VIC+28; MC Sprite 3
                lda #%00111000
                sta VIC+29; stretch X Sprite 3-5
                lda #%00110000
                sta VIC+23; stretch Y Sprite 4-5
                ;blue box under text
                lda #%00001000
                sta VIC+27
                lda #%00111011
                sta VIC+21
                ;make "Game" white
                ldx Mode
                lda WinPosLo,x
                sta $fb
                lda WinPosHi,x
                sta $fc
                ldy #81
                ldx #$d4
                jsr AddYXtoFBFC; found correct color pos
                lda #CL_WHITE
                ldy #0
                sta ($fb),y
                iny
                sta ($fb),y
                iny
                sta ($fb),y
DrawGameMenuMap lda #<GameMenuMap
                sta $fb
                lda #>GameMenuMap
                sta $fc
                lda #6
                sta MapWidth
                lda #3
                sta MapHeight
                ldx Mode
                lda WinPosLo,x
                clc
                adc #121
                sta $fd
                lda WinPosHi,x
                adc #0
                sta $fe
                jsr DrawMap
                rts
                
ShowHelpMenu    lda #GM_MENU
                sta GameMode
                lda #MM_HELP
                sta MenuMode
                jsr SaveScreen
                lda #SP_HM_BlueRect
                sta SPRPTR_3
                lda #SP_HM_Boundary
                sta SPRPTR_4
                sta SPRPTR_5
                lda #SP_HM_LBoundary
                sta SPRPTR_6
                ;positions
                ;-------------------X
                ldx Mode
                ldy SprHelpMenuPosX,x;
                iny
                sty VIC+6
                sty VIC+8
                dey
                sty VIC+10
                dey
                sty VIC+12
                ;-------------------Y
                lda SprMenuPosY,x
                sta VIC+7
                sta VIC+11
                tay
                iny
                sty VIC+9
                clc
                adc #7
                sta VIC+13
                ;colors
                lda #CL_WHITE
                sta VIC+45
                lda #CL_DARKBLUE
                sta VIC+42
                lda #CL_BLACK
                sta VIC+43
                lda #CL_MIDGREY
                sta VIC+44
                ;MC, stretch
                lda #%00001000
                sta VIC+28; MC Sprite 3
                lda #%00111000
                sta VIC+29; stretch X Sprite 3-5
                lda #%00110000
                sta VIC+23; stretch Y Sprite 4-5
                ;blue box under text
                lda #%00001000
                sta VIC+27
                lda #%01111011
                sta VIC+21
                ;make "Help" white
                ldx Mode
                lda WinPosLo,x
                sta $fb
                lda WinPosHi,x
                sta $fc
                ldy #84
                ldx #$d4
                jsr AddYXtoFBFC; found correct color pos
                lda #CL_WHITE
                ldy #0
                sta ($fb),y
                iny
                sta ($fb),y
                iny
                sta ($fb),y
DrawHelpMenuMap lda #<HelpMenuMap
                sta $fb
                lda #>HelpMenuMap
                sta $fc
                lda #5
                sta MapWidth
                lda #2
                sta MapHeight
                ldx Mode
                lda WinPosLo,x
                clc
                adc #124
                sta $fd
                lda WinPosHi,x
                adc #0
                sta $fe
                jsr DrawMap
                rts
                
!zone Math
AddYXtoFBFC     tya
                clc
                adc $fb
                sta $fb
                txa
                adc $fc
                sta $fc
                rts
                
AddYtoFBFC      tya
                clc
                adc $fb
                sta $fb
                lda $fc
                adc #0
                sta $fc
                rts
                
AddYtoFDFE      tya
                clc
                adc $fd
                sta $fd
                lda $fe
                adc #0
                sta $fe
                rts

;SubtractYFromFDFE  
;                sty factor
;                lda $fd
;                sec
;                sbc factor
;                sta $fd
;                lda $fe
;                sbc #0
;                sta $fe
;                rts

;a <- a mod 3, x <- a div 3
Mod3            sec
                ldx #$ff
label           inx
                sbc #3
                bcs label
                adc #3
                rts

modulo          !byte 0
;a <- a mod [modulo], x <- a div [modulo]
Mod             sec
                ldx #$ff
mod_loop        inx
                sbc modulo
                bcs mod_loop
                adc modulo
                rts

factor          !byte 0
;a <- a * [factor]    (Keep a small!)
Mult            and #%11111111
                bne goon
                rts
goon            tax
                lda #0
sumup           clc
                adc factor
                dex
                bne sumup
                rts


!zone Game
PlayerHasWon    jsr ShowWonDlg
                lda #1
                sta TadaOn
                rts
                
IsWon           lda #0
                sta res
                ldx WHSquared
                dex
                ldy WHSquared
won_loop        tya
                cmp Numbers,x
                bne not_won
                dey
                dex
                bpl won_loop
                lda #1
                sta res
not_won         rts
                
MakeGameMove    lda CaretPos
                sta OldCaretPos
                jsr TryMove
                lda bHasMoved
                bne has_moved
                rts
has_moved       lda #0
                sta bHasMoved
                jsr DrawBtnAtOldCaret
                jsr PrintNumbers
                jsr DrawCaret
                jsr PushSound
                jsr IsWon
                lda res
                bne PlayerHasWon
                rts
                
TryMove         ;try move left
                ldx PressedBtnIndex
                inx
                cpx CaretPos
                beq MoveLeft
                ;try move right
                ldx CaretPos
                inx
                cpx PressedBtnIndex
                beq MoveRight
                ;try move up
                lda PressedBtnIndex
                clc
                adc WH
                cmp CaretPos
                beq MoveUp
                ;try move down
                lda CaretPos
                clc
                adc WH
                cmp PressedBtnIndex
                beq MoveDown
                rts 
                
;Expects value $fe--$01 in MoveDirection
MoveCaret       lda MoveDirection
                cmp #0
                beq MoveLeft
                cmp #255
                beq MoveRight
                cmp #1
                beq MoveUp
                cmp #254
                beq MoveDown
MoveLeft        lda WH
                sta modulo
                lda CaretPos
                jsr Mod;CaretPos mod WH->a
                bne MoveCaretLeft
                rts
MoveRight       lda WH
                sta modulo
                ldx CaretPos
                inx
                txa
                jsr Mod;CaretPos mod WH->a
                bne MoveCaretRight
                rts
MoveUp          lda CaretPos
                cmp WH
                bcs MoveCaretUp
                rts
MoveDown        lda WHSquared
                sec
                sbc WH
                tax
                dex
                cpx CaretPos
                bcs MoveCaretDown
                rts
MoveCaretLeft   ldx CaretPos
                txa
                tay
                dey
                sty CaretPos
                jsr SwapNumbers
                sta bHasMoved; will be a non-zero number
                rts
                
MoveCaretRight  ldx CaretPos
                txa
                tay
                iny
                sty CaretPos
                jsr SwapNumbers
                sta bHasMoved
                rts
                
MoveCaretUp     lda CaretPos
                tax
                sec
                sbc WH
                tay
                sty CaretPos
                jsr SwapNumbers
                sta bHasMoved
                rts
                
MoveCaretDown   lda CaretPos
                tax
                clc
                adc WH
                tay
                sty CaretPos
                jsr SwapNumbers
                sta bHasMoved
                rts

;expects caret pos in x and other index in y                
SwapNumbers     lda Numbers,y
                sta Numbers,x
                lda WHSquared
                sta Numbers,y
                rts

Shuffle         lda #10
                sta shuffle_counter+1
                lda #255
                sta shuffle_counter
once_more       ;generate random number
                lda $dc04  ;Low-Byte  von Timer A aus dem CIA-1
                eor $dc05  ;High-Byte von Timer A aus dem CIA-1
                eor $dd04  ;Low-Byte  von Timer A aus dem CIA-2
                adc $dd05  ;High-Byte von Timer A aus dem CIA-2
                eor $dd06  ;Low-Byte  von Timer B aus dem CIA-2
                eor $dd07  ;High-Byte von Timer B aus dem CIA-2
                ;take it mod 4
                ;jsr Mod4
                and #%00000011
                sec
                sbc #2
                sta $fb
                eor MoveDirection
                eor #%11111111
                beq once_more
                lda $fb
                sta MoveDirection; $fe -- $01
                jsr MoveCaret
                ;jsr PrintNumbers
                dec shuffle_counter
                bne once_more
                jsr PushSound
                dec shuffle_counter+1
                bne once_more
                rts
shuffle_counter !byte 0,0

!zone GameRepresentationInField
PrintNumbers    ;start position
                ldx Mode
                lda WinPosX,x
                clc
                adc #3
                tay
                lda WinPosY,x
                clc
                adc #5
                tax
                ;put it into FBFC
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                jsr AddYtoFBFC
                 
                ldx #0
rep_loop_out    ldy #0
rep_loop_in     cpx CaretPos
                beq no_draw
                lda Numbers,x
                clc
                adc #47
                sta ($fb),y
no_draw         inx
                iny
                iny
                iny
                cpy WHTimes3
                bcc rep_loop_in
                ldy #120
                jsr AddYtoFBFC
                cpx WHSquared
                bcc rep_loop_out
                rts
                
!zone MouseJoystickChoice
SCREEN_LOCATION = $0400
COLOR_LOCATION = $d800

InputChoice     lda #22
                sta $d018
                jsr $e544; clear screen
                
                ldy #0
                lda #1
do_colors       sta COLOR_LOCATION,y
                sta COLOR_LOCATION + 220,y
                iny
                cpy #220
                bne do_colors
                
new_choice      ldy #0
                lda MouseOn
                beq choice_joy

choice_mouse    lda ChoiceMouse,y
                sta SCREEN_LOCATION,y
                lda ChoiceMouse + 220,y
                sta SCREEN_LOCATION + 220,y
                iny
                cpy #220
                bne choice_mouse
                jmp input_get
                
choice_joy      lda ChoiceJoy,y
                sta SCREEN_LOCATION,y
                lda ChoiceJoy + 220,y
                sta SCREEN_LOCATION + 220,y
                iny
                cpy #220
                bne choice_joy
                
input_get       jsr $ffe4
                beq input_get
                cmp #13
                beq ok
                cmp #145
                beq up
                cmp #17
                beq down
                jmp input_get
up              lda MouseOn
                beq input_get
                lda #0
                sta MouseOn
                jmp new_choice
down            lda MouseOn
                bne input_get
                lda #1
                sta MouseOn
                jmp new_choice
ok              ;
                rts


!zone Sound
PushSound       lda SoundOn
                beq no_sound
                lda #0
                sta sid+4
                sta sid+6
                lda #6
                sta sid+$18
                lda #$1e
                sta sid+1
                lda #$70
                sta sid+5
                lda #$81
                sta sid+4
no_sound        rts

Tada            lda SoundOn
                beq no_sound
                lda TadaOn
                beq no_sound
                dec tada_counter0
                lda tada_counter0
                bne no_sound
                lda #3; here we act------
                sta tada_counter0
                dec tada_counter1
                lda tada_counter1
                beq ShutOffTada
                cmp #15
                beq PlayTada
                cmp #14
                beq TadaOff
                cmp #13
                beq PlayTada
                rts
                
PlayTada        lda #0
                sta sid+4
                sta sid+11
                lda #62
                sta sid
                lda #165
                sta sid+7
                lda #42
                sta sid+1
                lda #31
                sta sid+8
                lda #85
                sta sid+5
                sta sid+6
                sta sid+12
                sta sid+13
                lda #33
                sta sid+11
                sta sid+4
                rts

ShutOffTada     lda #16
                sta tada_counter1
                lda #0
                sta TadaOn
                
TadaOff         lda #0
                sta sid+4
                sta sid+11
                rts
                
tada_counter0   !byte 3
tada_counter1   !byte 16
                

!zone Data
TadaOn          !byte 0
SoundOn         !byte 1
MouseOn         !byte 0
CaretPos        !byte 0
OldCaretPos     !byte 0
MoveDirection   !byte 0; 0:left, 1:right, 2:up, 3:down
bHasMoved       !byte 0
WH              !byte 0;e.g., 3 if mode=0
WHSquared       !byte 0;e.g., 9 if mode=0
WHTimes3        !byte 0;e.g., 12 if mode=1
Mode            !byte 0; 0: 3x3, 1: 4x4, 2: 5x5, 3: 6x6
SelectedMode    !byte 0
exit_code       !byte 0
bExit           !byte 0; -will all
bMoved          !byte 0; -be only
bFire           !byte 0; -triggered
bFireReleased   !byte 0; -once
bModeChanged    !byte 0; -(exit codes)
bFirePressed    !byte 0; documents fire press at any time
bButtonPressed  !byte 0
PressedBtnX     !byte 0
PressedBtnY     !byte 0
PressedBtnIndX  !byte 0
PressedBtnIndY  !byte 0
PressedBtnRect  !byte 0,0,0,0
PressedBtnIndex !byte 0
PrsBtnNoCharInd !byte 0
NumberCharDefLo !byte 0
NumberCharDefHi !byte 0
DlgBtnRect      !byte 0,0,0,0
FireCounter     !byte joy_delay
dx              !byte 0 ;joystick
dy              !byte 0 ;directions
res             !byte 0 ;return value for various functions
Rect            !byte 0,0,0,0
counter         !byte 0
GameMode        !byte GM_NORMAL; 0: normal, 1: menu, 255: dialog
MenuMode        !byte 0
DialogMode      !byte 0; 0: size dlg, 1: won dlg
SelMenuItem     !byte 0
MapWidth        !byte 0
MapHeight       !byte 0
rasters         !byte 98,82,74,66
SprGameMenuPosX !byte 136,120,112,104
SprHelpMenuPosX !byte 160,144,136,128
SprMenuPosY     !byte 98,82,74,66
WinDimX         !byte 13,16,19,22
WinDimY         !byte 15,18,21,24
WinPosLo        !byte $ad,$5b,$32,$09
WinPosHi        !byte $04,$04,$04,$04
WinPosX         !byte 13,11,10,9
WinPosY         !byte 4,2,1,0
MapsLo          !byte 0,0,0,0
MapsHi          !byte 0,0,0,0
FieldPixelDim   !byte 72,96,120,143
Numbers         !byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
                !byte 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36

ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
;ColorTabHi      !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
;                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db


*=$2000
            !bin "chars.bin"

*=$2800
Colors      !bin "attribs.bin"; per char, thus 256 bytes
Map3        !bin "map3.bin"
Map4        !bin "map4.bin"
Map5        !bin "map5.bin"
Map6        !bin "map6.bin"
GameMenuMap !bin "GameMenuMap.bin"
HelpMenuMap !bin "HelpMenuMap.bin"
PushBtnMap  !bin "PushBtnMap.bin"
RelBtnMap   !bin "RelBtnMap.bin"
CaretMap    !bin "CaretMap.bin"
SizeDlgMap  !bin "SizeDialogMap.bin"
WonDlgMap   !bin "WonDialogMap.bin"
InfoDlgMap  !bin "InfoDialogMap.bin"
HelpDlgMap  !bin "HelpDialogMap.bin"
ChoiceJoy   !bin "ChoiceJoy.bin"
ChoiceMouse !bin "ChoiceMouse.bin"

!zone Sprites
*=$3400; Space for 16 sprites, four more in cassette buffer!
Mousepointer0Addr
!byte $c0,$00,$00,$a0,$00,$00,$90,$00
!byte $00,$88,$00,$00,$84,$00,$00,$82
!byte $00,$00,$8e,$00,$00,$a8,$00,$00
!byte $e4,$00,$00,$14,$00,$00,$12,$00
!byte $00,$12,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

Mousepointer1Addr
!byte $c0,$00,$00,$e0,$00,$00,$f0,$00
!byte $00,$f8,$00,$00,$fc,$00,$00,$fe
!byte $00,$00,$fe,$00,$00,$f8,$00,$00
!byte $fc,$00,$00,$1c,$00,$00,$1e,$00
!byte $00,$1e,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

GM_BlueRectAddr
!byte $aa,$a0,$00,$aa,$a0,$00,$aa,$a0
!byte $00,$aa,$a0,$00,$aa,$a0,$00,$aa
!byte $a0,$00,$aa,$a0,$00,$55,$55,$55
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$86

HM_BlueRectAddr
;Blaues Rechteck mit weißem Strich drunter
!byte $aa,$80,$00,$aa,$80,$00,$aa,$80
!byte $00,$aa,$80,$00,$aa,$80,$00,$aa
!byte $80,$00,$aa,$80,$00,$55,$55,$50
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$86

GameMenuBoundaryAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $00,$00,$01,$00,$00,$01,$00,$00
!byte $01,$00,$00,$01,$00,$00,$01,$00
!byte $00,$01,$00,$00,$01,$00,$00,$01
!byte $ff,$ff,$ff,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$0c

HelpMenuBoundaryAddr
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$08,$00
!byte $00,$08,$00,$00,$08,$00,$00,$08
!byte $00,$00,$08,$00,$00,$08,$00,$00
!byte $08,$00,$00,$08,$ff,$ff,$f8,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$0c

HelpMenuLeftBoundaryAddr
!byte $c0,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$80,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

SelectBoxAddr
!byte $ff,$ff,$f0,$ff,$ff,$f0,$ff,$ff
!byte $f0,$ff,$ff,$f0,$ff,$ff,$f0,$ff
!byte $ff,$f0,$ff,$ff,$f0,$ff,$ff,$f0
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$06

FullBlueBoxAddr
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$06

WhiteLineAddr
!byte $ff,$ff,$ff,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

ButtonLowerAddr
!byte $00,$00,$00,$00,$00,$02,$00,$00
!byte $02,$00,$00,$02,$00,$00,$02,$00
!byte $00,$02,$00,$00,$02,$00,$00,$02
!byte $00,$00,$02,$ff,$ff,$fe,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

ButtonUpperAddr
!byte $ff,$ff,$fe,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

BulletSelectorAddr
!byte $78,$00,$00,$fc,$00,$00,$fc,$00
!byte $00,$fc,$00,$00,$fc,$00,$00,$78
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

WonDlgLeftBndAddr
!byte $c0,$00,$00,$c0,$00,$00,$c0,$00
!byte $00,$c0,$00,$00,$c0,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

PushBtnBoundaryAddr
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$80,$00,$00,$80
!byte $00,$00,$80,$00,$00,$80,$00,$00
!byte $80,$00,$00,$80,$00,$00,$80,$00
!byte $00,$80,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$0c

; Just one left!