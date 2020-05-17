
EnableExplicit

ExamineDesktops()

; Create window.
Define Window.i = OpenWindow( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), "Aeon Flux", #PB_Window_BorderLess )
Define Canvas.i = CanvasGadget( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

;let's start with a single line of text and get that working
;then go multiline

Enumeration CursorMode
  #CursorModeBlock
  #CursorModeBar
  #CursorModeUnderline
EndEnumeration

Define Line.s = "Try This"
Define CursorPositionInLine.i = 4
Define CursorMode.i = #CursorModeBlock

;store text in memory as UTF-8 and use the same single set of temp strings (UTF-16) for rendering?

; Set up text buffer.
Define TextBufferLeftSize.i = 256
Define TextBufferRightSize.i = 256
Define TextBufferLeft.s = Space( TextBufferLeftSize )
Define TextBufferRight.s = Space( TextBufferRightSize )
Define *TextBufferLeft = @TextBufferLeft
Define *TextBufferRight = @TextBufferRight
Define TextBufferCursor.s = Space( 1 )
Define *TextBufferCursor = @TextBufferCursor

Define TextLeft.s = "Try "
Define TextRight.s = "his"

CopyMemory( @TextLeft, *TextBufferLeft, 5 * 2 ) ; Include NUL.
CopyMemory( @TextRight, *TextBufferRight, 4 * 2 ) ; Include NUL.

PokeC( *TextBufferCursor, 'T' )

Procedure MoveCursorLeft()
  
  Shared CursorPositionInLine
  Shared *TextBufferRight
  Shared *TextBufferCursor
  Shared *TextBufferLeft
  
  ; Stop at first character.
  If CursorPositionInLine = 0
    ProcedureReturn
  EndIf
  
  ; Prepend current cursor character to right buffer.
  MoveMemory( *TextBufferRight, *TextBufferRight + 2, MemoryStringLength( *TextBufferRight ) * 2 + 2 ) ; Include NUL.
  PokeC( *TextBufferRight, PeekC( *TextBufferCursor ) )
  
  ; Cycle end of left buffer into cursor character.
  If CursorPositionInLine = 1
    PokeC( *TextBufferCursor, PeekC( *TextBufferLeft ) )
    PokeC( *TextBufferLeft, 0 )
  Else
    Define TextBufferLeftLengthInChars = MemoryStringLength( *TextBufferLeft )
    Define *LastCharOffset = *TextBufferLeft + ( TextBufferLeftLengthInChars - 1 ) * 2
    PokeC( *TextBufferCursor, PeekC( *LastCharOffset ) )
    PokeC( *LastCharOffset, 0 )
  EndIf
  CursorPositionInLine - 1
  
EndProcedure

Procedure MoveCursorRight()
  
  Shared Line
  Shared CursorPositionInLine
  Shared *TextBufferCursor
  Shared *TextBufferLeft
  Shared *TextBufferRight
  
  ; Stop at last character.
  If CursorPositionInLine = Len( Line ) - 1
    ProcedureReturn
  EndIf
  
  ; Append current cursor character to left buffer.
  Define TextBufferLeftLengthInChars = MemoryStringLength( *TextBufferLeft )
  PokeC( *TextBufferLeft + TextBufferLeftLengthInChars * 2, PeekC( *TextBufferCursor ) )
  PokeC( *TextBufferLeft + TextBufferLeftLengthInChars * 2 + 2, 0 )
  
  ; Cycle beginning of right buffer into cursor character.
  PokeC( *TextBufferCursor, PeekC( *TextBufferRight ) )
  MoveMemory( *TextBufferRight + 2, *TextBufferRight, MemoryStringLength( *TextBufferRight ) * 2 ) ; Include NUL.
  
  CursorPositionInLine + 1
  
EndProcedure

; Main loop.
Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
    
    ; Input.
    Select EventType()
      Case #PB_EventType_KeyDown
        Define Key = GetGadgetAttribute( Canvas, #PB_Canvas_Key )
        Select Key
          Case #PB_Shortcut_Escape
            End
          Case #PB_Shortcut_H
            MoveCursorLeft()
          Case #PB_Shortcut_L
            MoveCursorRight()
        EndSelect
      Case #PB_EventType_Input
        Define Input = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
        ;Line = Line + Chr( Input )
    EndSelect
    
    ; Draw.
    If StartDrawing( CanvasOutput( Canvas ) )

      Define TextWidthLeft = TextWidth( TextBufferLeft )
      Define TextWidthRight = TextWidth( TextBufferRight )
      Define TextWidthCursor = TextWidth( TextBufferCursor )
      Define TextHeightLeft = TextHeight( TextBufferLeft )
      
      ; Draw portion of line left and right to the cursor.
      BackColor( #White )
      FrontColor( #Black )
      DrawText( 100, 100, TextBufferLeft )
      DrawText( 100 + TextWidthLeft + TextWidthCursor, 100, TextBufferRight )
      
      ; Draw portion of line at cursor.
      BackColor( #Black )
      FrontColor( #White )
      DrawText( 100 + TextWidthLeft, 100, TextBufferCursor )
      
      StopDrawing()
      
    EndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow


;[X] TODO Render cursor (block)
;[ ] TODO Move cursor with H and L (vertical movement will have to wait until we have multiple lines)
;[ ] TODO Switch to insert mode
;[ ] TODO Insert characters

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
; CursorPosition = 120
; FirstLine = 96
; Folding = -
; EnableXP
; HideErrorLog