
EnableExplicit

#VectorRendering = 0

ExamineDesktops()

; Create window.
Define Window.i = OpenWindow( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), "Aeon Flux", #PB_Window_BorderLess )
Define Canvas.i = CanvasGadget( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

Define Font = LoadFont( #PB_Any, "Consolas", 16, #PB_Font_HighQuality )

;let's start with a single line of text and get that working
;then go multiline

Enumeration EditMode
  #NormalMode
  #InsertMode
EndEnumeration

Enumeration CursorMode
  #CursorModeBlock
  #CursorModeBar
  #CursorModeUnderline
EndEnumeration

Define Line.s = "Try This"
Define CursorPositionInLine.i = 4
Define CursorMode.i = #CursorModeBlock
Define EditMode.i = #NormalMode

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

;==============================================================================

Procedure SwitchToEditMode( Mode.i )
  Shared EditMode
  Shared CursorMode
  
  EditMode = Mode
  Select Mode
    Case #NormalMode
      CursorMode = #CursorModeBlock
    Case #InsertMode
      CursorMode = #CursorModeBar
  EndSelect
  
EndProcedure

;==============================================================================

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

;==============================================================================

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

;==============================================================================

; Main loop.
Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
    
    ; Input.
    Select EventType()
      Case #PB_EventType_KeyDown
        Define Key = GetGadgetAttribute( Canvas, #PB_Canvas_Key )
        Select EditMode
          Case #NormalMode
            Select Key
              Case #PB_Shortcut_Escape
                End
              Case #PB_Shortcut_H
                MoveCursorLeft()
              Case #PB_Shortcut_L
                MoveCursorRight()
              Case #PB_Shortcut_I
                SwitchToEditMode( #InsertMode )
            EndSelect
          Case #InsertMode
            Select Key
              Case #PB_Shortcut_Escape
                SwitchToEditMode( #NormalMode )
            EndSelect
        EndSelect
      Case #PB_EventType_Input
        Define Input = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
        ;Line = Line + Chr( Input )
    EndSelect
    
    ; Draw.
    CompilerIf #VectorRendering = 1
      
      ;flickers (should redraw only what's dirty)
      
      If StartDrawing( CanvasOutput( Canvas ) )
        ; Clear canvas. Seems like this is the only method to do so...
        Box( 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #White )
        StopDrawing()
      EndIf
      If StartVectorDrawing( CanvasVectorOutput( Canvas, #PB_Unit_Pixel ) )
      	
        VectorFont( FontID( Font ) )
  
        Define TextWidthLeft = VectorTextWidth( TextBufferLeft )
        Define TextWidthRight = VectorTextWidth( TextBufferRight )
        Define TextWidthCursor = VectorTextWidth( TextBufferCursor )
        
        ; Draw portion of line left and right to the cursor.
        VectorSourceColor( RGBA( 0, 0, 0, 255 ) )
        ;BackColor( #White )
        ;FrontColor( #Black )
        MovePathCursor( 100, 100 )
        DrawVectorText( TextBufferLeft )
        MovePathCursor( 100 + TextWidthLeft + TextWidthCursor, 100 )
        DrawVectorText( TextBufferRight )
        
        ; Draw portion of line at cursor.
        ;BackColor( #Black )
        ;FrontColor( #White )
        MovePathCursor( 100 + TextWidthLeft, 100 )
        DrawVectorText( TextBufferCursor )
        
        StopVectorDrawing()
        
      EndIf
    
    CompilerElse
    
      If StartDrawing( CanvasOutput( Canvas ) )
      	
        DrawingFont( FontID( Font ) )
        DrawingMode( #PB_2DDrawing_Default )
  
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
        If CursorMode = #CursorModeBlock
          BackColor( #Black )
          FrontColor( #White )
        EndIf
        DrawText( 100 + TextWidthLeft, 100, TextBufferCursor )
        If CursorMode = #CursorModeBar
          Box( 100 + TextWidthLeft, 100, 5, TextHeightLeft, #Black )
        EndIf
        
        StopDrawing()
        
      EndIf
    
    CompilerEndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow


;[X] TODO Render cursor (block)
;[X] TODO Move cursor with H and L (vertical movement will have to wait until we have multiple lines)
;[X] TODO Use fixed-width font
;[ ] TODO Switch to insert mode and back
;[ ] TODO Insert characters
;...
;[ ] Redraw only changed portion of the text (dirty rectangles)
;...
;[ ] TODO Switch to command mode

;requirements:
;- memory buffer and string manipulation
;- windowing
;- high-quality text rendering
;- Mac and Windows
;- fully integrated IDE
;- native code executables
;- unicode support (preferably UTF8)
;- rich library
;- unit testing
;- ideal: vector drawing
;- ideal: hotreload

;PB problems:
;- no aliased font rendering? -> ugly font (SOLVED: switched to vector drawing AND Windows high-DPI support)
;- no substring support (not a dealbreaker)

;can mutate text in String
;but cannot create a substring without copying and cannot render a portion of a String only
;can truncate a string by writing a NUL character to memory
; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 227
; FirstLine = 204
; Folding = -
; EnableXP
; HideErrorLog