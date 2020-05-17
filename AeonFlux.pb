
ExamineDesktops()

; Create window.
Window = OpenWindow( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), "Aeon Flux", #PB_Window_BorderLess )
Canvas = CanvasGadget( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

;let's start with a single line of text and get that working
;then go multiline

Enumeration CursorMode
  #CursorModeBlock
  #CursorModeBar
  #CursorModeUnderline
EndEnumeration

Line.s = "Try This"
CursorPosition.i = 4
CursorMode.i = #CursorModeBlock

; Set up text buffer.
TextBufferLeftSize.i = 256
TextBufferRightSize.i = 256
TextBufferLeft.s = Space( TextBufferLeftSize )
TextBufferRight.s = Space( TextBufferRightSize )
*TextBufferLeft = @TextBufferLeft
*TextBufferRight = @TextBufferRight
TextBufferCursor.s = Space( 1 )
*TextBufferCursor = @TextBufferCursor

TextLeft.s = "Try "
TextRight.s = "his"

CopyMemory( @TextLeft, *TextBufferLeft, 4 * 2 )
CopyMemory( @TextRight, *TextBufferRight, 3 * 2 )

PokeC( *TextBufferLeft + 4 * 2, 0 )
PokeC( *TextBufferRight + 3 * 2, 0 )
PokeC( *TextBufferCursor, 'T' )

; Main loop.
Repeat
  
  Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
    
    ; Input.
    Select EventType()
      Case #PB_EventType_KeyDown
        Key = GetGadgetAttribute( Canvas, #PB_Canvas_Key )
        If Key = #PB_Shortcut_Escape
          Break
        EndIf
      Case #PB_EventType_Input
        Input = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
        Line = Line + Chr( Input )
    EndSelect
    
    ; Draw.
    If StartDrawing( CanvasOutput( Canvas ) )
      BackColor( #White )
      FrontColor( #Black )
      TextWidthLeft = TextWidth( TextBufferLeft )
      TextWidthRight = TextWidth( TextBufferRight )
      TextWidthCursor = TextWidth( TextBufferCursor )
      TextHeightLeft = TextHeight( TextBufferLeft )
      DrawText( 100, 100, TextBufferLeft )
      DrawText( 100 + TextWidthLeft + TextWidthCursor, 100, TextBufferRight )
      BackColor( #Black )
      FrontColor( #White )
      DrawText( 100 + TextWidthLeft, 100, TextBufferCursor )
      ;draw character at cursor position inverted?
      ;Box( 100 + TextWidthLeft, 100, TextHeightLeft, TextHeightLeft, RGBA( 0, 0, 0, 25 ) )
      StopDrawing()
    EndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow


;[ ] TODO Render cursor (block)
;[ ] TODO Move cursor with HJKL

;requirements:
;- memory buffer and string manipulation
;- windowing
;- text rendering
;- Mac and Windows
;- fully integrated IDE
;- native code executables
;- unicode support (preferably UTF8)
;- rich library
;- unit testing
;- ideal: hotreload

;PB doesn't have substring support but otherwise seems to be the only thing ticks *all* the boxes

;can mutate text in String
;but cannot create a substring without copying and cannot render a portion of a String only
;can truncate a string by writing a NUL character to memory

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 71
; FirstLine = 43
; EnableXP