; Aeon Flux
;
; Author: Rene Damm
; Started: 16-May-20

EnableExplicit

;Current Goal: multiple lines of text
;  (need to know where the lines are in the text) DONE
;  (need to be able to render individual lines)
;  (need to be able to navigate up and down)

XIncludeFile "Utils.pb"
XIncludeFile "GapBuffer.pb"         ; Memory.
XIncludeFile "TextMarker.pb"        ; Positions.
XIncludeFile "TextBuffer.pb"        ; Text.
XIncludeFile "TextEditor.pb"        ; Cursors, selections, undo.
XIncludeFile "Shell.pb"             ; Modes, commands.
XIncludeFile "TextEditorShell.pb"   ; Text editing commands.

; NOTE: Unit tests can be debugged by simply invoking them here.

;TextRenderer (dirty rects and render buffers)
;Editor (Shell?) (modes, command maps, macros, )    *all* edit operations must be representable as text strings
;Configuration
;Workspace (blobs)

;central event/message bus?

#VectorRendering = #False

ExamineDesktops()

Define CanvasWidth.i = DesktopWidth( 0 )
Define CanvasHeight.i = DesktopHeight( 0 )

Define TextColor.i = #Black
Define BackgroundColor.i = #White

; Create window.
Define Window.i = OpenWindow( #PB_Any, 0, 0, CanvasWidth, CanvasHeight, "Aeon Flux", #PB_Window_BorderLess )
Define Canvas.i = CanvasGadget( #PB_Any, 0, 0, CanvasWidth, CanvasHeight, #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

Define Font = LoadFont( #PB_Any, "Consolas", 16, #PB_Font_HighQuality )

Enumeration EditMode
  #NormalMode
  #InsertMode
EndEnumeration

Enumeration CursorMode
  #CursorModeBlock
  #CursorModeBar
  #CursorModeUnderline
EndEnumeration

;cursor may be spanning several characters
Define CursorPositionInLine.i = 0
Define CursorMode.i = #CursorModeBlock
Define EditMode.i = #NormalMode

;store text in memory as UTF-8 and use the same single set of temp strings (UTF-16) for rendering?

; Set up text buffer.
Define TextBufferLeftLength.i = 256
Define TextBufferRightLength.i = 256
Define TextBufferLeft.s = Space( TextBufferLeftLength )
Define TextBufferRight.s = Space( TextBufferRightLength )
Define *TextBufferLeft = @TextBufferLeft
Define *TextBufferRight = @TextBufferRight
Define TextBufferCursor.s = Space( 1 )
Define *TextBufferCursor = @TextBufferCursor

Define TextLengthLeft.i = 0
Define TextLengthRight.i = 0
Define TextLength.i = TextLengthLeft + 0 + TextLengthRight

PokeC( *TextBufferCursor, 0 )
PokeC( *TextBufferLeft, 0 )
PokeC( *TextBufferRight, 0 )

;==============================================================================

;do text rendering on thread? (and output as images)
;will eventually have to handle styling, annotations, etc

DeclareModule TextRenderBuffer
  
  Structure TextRenderBuffer
  EndStructure
  
EndDeclareModule

Module TextRenderBuffer
EndModule

DeclareModule DataModel
  
  ;root
  ;project
  ;workspace
  
  ;or all in separate modules?
  
EndDeclareModule

Module DataModel
EndModule

;==============================================================================


;TextCursor module? (block-select cursors and the like) or TextSelection?

;==============================================================================

Procedure MoveCursorLeft()
  
  Shared CursorPositionInLine
  Shared *TextBufferRight
  Shared *TextBufferCursor
  Shared *TextBufferLeft
  Shared TextLengthLeft
  Shared TextLengthRight
  
  ; Stop at first character.
  If CursorPositionInLine = 0
    ProcedureReturn
  EndIf
  
  ; Prepend current cursor character to right buffer.
  MoveMemory( *TextBufferRight, *TextBufferRight + 2, MemoryStringLength( *TextBufferRight ) * 2 + 2 ) ; Include NUL.
  PokeC( *TextBufferRight, PeekC( *TextBufferCursor ) )
  TextLengthRight + 1
  
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
  TextLengthLeft - 1
  CursorPositionInLine - 1
  
EndProcedure

;==============================================================================

Procedure MoveCursorRight()
  
  Shared TextLength
  Shared CursorPositionInLine
  Shared *TextBufferCursor
  Shared *TextBufferLeft
  Shared *TextBufferRight
  Shared TextLengthLeft
  Shared TextLengthRight
  
  ; Stop at last character.
  If CursorPositionInLine = TextLength - 1
    ProcedureReturn
  EndIf
  
  ; Append current cursor character to left buffer.
  Define TextBufferLeftLengthInChars = MemoryStringLength( *TextBufferLeft )
  PokeC( *TextBufferLeft + TextBufferLeftLengthInChars * 2, PeekC( *TextBufferCursor ) )
  PokeC( *TextBufferLeft + TextBufferLeftLengthInChars * 2 + 2, 0 )
  TextLengthLeft + 1
  
  ; Cycle beginning of right buffer into cursor character.
  PokeC( *TextBufferCursor, PeekC( *TextBufferRight ) )
  MoveMemory( *TextBufferRight + 2, *TextBufferRight, MemoryStringLength( *TextBufferRight ) * 2 ) ; Include NUL.]
  TextLengthRight - 1
  
  CursorPositionInLine + 1
  
EndProcedure

;==============================================================================

Procedure InsertCharacterAtCursor( Character.c )
  
  Shared TextBufferLeft
  Shared *TextBufferLeft
  Shared TextLengthLeft
  Shared TextBufferLeftLength
  Shared CursorPositionInLine
  Shared TextLength
  
  ; Increase buffer size, if necessary.
  If TextLengthLeft + 1 = TextBufferLeftLength
    Define NewLeftBufferLength = TextBufferLeftLength + 256
    Define NewLeftBuffer.s = Space( NewLeftBufferLength )
    CopyMemory( *TextBufferLeft, @NewLeftBuffer, TextLengthLeft * 2 ) ; Without NUL.
    TextBufferLeft = NewLeftBuffer
    *TextBufferLeft = @NewLeftBuffer
    TextBufferLeftLength = NewLeftBufferLength
  EndIf
  
  ; Append character to left buffer.
  Define *CharacterPtr = *TextBufferLeft + TextLengthLeft * 2
  PokeC( *CharacterPtr, Character )
  PokeC( *CharacterPtr + 2, 0 )
  
  CursorPositionInLine + 1
  TextLength + 1
  TextLengthLeft + 1
  
EndProcedure

;==============================================================================

Procedure DeleteCharacterBackwardsFromCursor()
  
  Shared *TextBufferLeft
  Shared TextLengthLeft
  Shared TextLength
  Shared CursorPositionInLine
  
  If TextLengthLeft = 0
    ProcedureReturn
  EndIf
  
  TextLengthLeft - 1
  PokeC( *TextBufferLeft + TextLengthLeft * 2, 0 )
  CursorPositionInLine - 1
  TextLength - 1
  
EndProcedure

;==============================================================================

Procedure SwitchToEditMode( Mode.i )
  
  Shared EditMode
  Shared CursorMode
  Shared CursorPositionInLine
  Shared TextLength
  
  EditMode = Mode
  Select Mode
    Case #NormalMode
      CursorMode = #CursorModeBlock
      ;TODO get rid of this behavior
      ; If we're at end of line, move left one position.
      If CursorPositionInLine = TextLength And TextLength > 0
        MoveCursorLeft()
      EndIf
    Case #InsertMode
      CursorMode = #CursorModeBar
  EndSelect
  
EndProcedure

;==============================================================================

Define EatNextCharacter.i = #False

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
                ; Suppress insertion of 'i' character.
                EatNextCharacter = #True
            EndSelect
          Case #InsertMode
            Select Key
              Case #PB_Shortcut_Back
                DeleteCharacterBackwardsFromCursor()
              Case #PB_Shortcut_Escape
                SwitchToEditMode( #NormalMode )
            EndSelect
        EndSelect
      Case #PB_EventType_Input
        If EditMode = #InsertMode And Not EatNextCharacter
          Define Input = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
          InsertCharacterAtCursor( Input )
        EndIf
        EatNextCharacter = #False
    EndSelect
    
    ; Draw.
    CompilerIf #VectorRendering = 1
      
      ;flickers (should redraw only what's dirty)
      ;vector drawing seems to be A LOT slower so nut sure I'll go further with this path
      
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
        
        ;keep a per-line dirty region
        
        ;REVIEW Drawing long strings seems to be super slow; this will ultimatly probably have to redraw as little as possible
        
        ; Clear line.
        Box( 100, 100, CanvasWidth, TextHeightLeft, BackgroundColor )
        
        ; Draw portion of line left and right to the cursor.
        BackColor( BackgroundColor )
        FrontColor( TextColor )
        DrawText( 100, 100, TextBufferLeft )
        DrawText( 100 + TextWidthLeft + TextWidthCursor, 100, TextBufferRight )
        
        ; Draw portion of line at cursor.
        If CursorMode = #CursorModeBlock
          BackColor( TextColor )
          FrontColor( BackgroundColor )
        EndIf
        DrawText( 100 + TextWidthLeft, 100, TextBufferCursor )
        If CursorMode = #CursorModeBar
          Box( 100 + TextWidthLeft, 100, 5, TextHeightLeft, TextColor )
        EndIf
        
        StopDrawing()
        
      EndIf
    
    CompilerEndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow

;[X] Can delete range in TextBuffer
;[ ] Can backspace in editor shell
;[ ] Refactor TextBuffer to have a write-head


;[X] TODO Render cursor (block)
;[X] TODO Move cursor with H and L (vertical movement will have to wait until we have multiple lines)
;[X] TODO Use fixed-width font
;[X] TODO Switch to insert mode and back
;[X] TODO Insert characters
;[X] TODO Start with empty buffer
;[X] TODO Backspace
;[ ] TODO Use gap buffer
;[ ] TODO Line markers
;[ ] TODO Add second line
;[ ] TODO Delete line
;[ ] TODO Save text
;[ ] TODO Restore text on startup
;...
;[ ] TODO Input run through command interpreter 
;...
;[ ] TODO Text is styled
;...
;[ ] TODO Move around node with keyboard
;[ ] TODO Move around node with mouse
;...
;[ ] TODO Run builder on program
;[ ] TODO Run runner on program
;...
;[ ] TODO Undo edit
;...
;[ ] TODO Redraw only changed portion of the text (dirty rectangles/lines)
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
;- no exceptions, asserts, or any other "normal" error handling support (only OnError for hard crashes)

;can mutate text in String
;but cannot create a substring without copying and cannot render a portion of a String only
;can truncate a string by writing a NUL character to memory
; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 21
; Folding = --
; EnableXP
; HideErrorLog