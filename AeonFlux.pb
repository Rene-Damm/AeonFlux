; Aeon Flux
;
; Author: Rene Damm
; Started: 16-May-20

EnableExplicit

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

DeclareModule Utils
  
  Macro QUOTE
    "
  EndMacro
  
  Macro DebugAssert( Expression )
    CompilerIf #PB_Compiler_Debugger
      If Not ( Expression )
        Debug "Assert: " + QUOTE Expression QUOTE
        Debug "Line " + Str( #PB_Compiler_Line ) + " in " + #PB_Compiler_File
        CallDebugger
      EndIf
    CompilerEndIf
  EndMacro
  
  Declare.q Min( A.q, B.q )
  Declare.q Max( A.q, B.q )
  Declare.q AlignToMultipleOf( Number.q, Alignment.q )
  
EndDeclareModule

Module Utils
  
  Procedure.q Min( A.q, B.q )
    If A < B
      ProcedureReturn A
    EndIf
    ProcedureReturn B
  EndProcedure
  
  Procedure.q Max( A.q, B.q )
    If A < B
      ProcedureReturn B
    EndIf
    ProcedureReturn A
  EndProcedure
  
  Procedure.q AlignToMultipleOf( Number.q, Alignment.q )
    ProcedureReturn Number + ( Alignment - Number % Alignment )
  EndProcedure
  
EndModule

;==============================================================================

DeclareModule GapBuffer
  
  ;............................................................................
  
  Structure GapBuffer
    
    StrideInBytes.b               ; Size of element in bytes.
    *LeftBuffer                   ; Memory block allocated for left buffer.
    *RightBuffer                  ; Memory block allocated for right buffer.
    *LeftBufferEnd                ; One past last element in left buffer.
    *RightBufferStart             ; First element in right buffer.
    LeftBufferSizeInBytes.q       ; Total size allocated for left buffer in bytes.
    RightBufferSizeInBytes.q      ; Total size allocated for right buffer in bytes.
    
  EndStructure
  
  ;............................................................................
  
  Declare   AllocateGapBuffer           ( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
  Declare   FreeGapBuffer               ( *Buffer.GapBuffer )
  Declare.q GetGapBufferPosition        ( *Buffer.GapBuffer )
  Declare.q GetGapBufferLength          ( *Buffer.GapBuffer )
  Declare   WriteIntoGapBuffer          ( *Buffer.GapBuffer, *Ptr, Count.q )
  Declare.q ReadFromGapBuffer           ( *Buffer.GapBuffer, *Ptr, Position.q, Count.q )
  Declare   MoveGapInGapBufferRelative  ( *Buffer.GapBuffer, Offset.q )
  Declare   MoveGapInGapBufferAbsolute  ( *Buffer.GapBuffer, Position.q )
  
EndDeclareModule

Module GapBuffer
  
  UseModule Utils
  
  ;............................................................................
  
  Procedure EnsureLeftBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define LeftSizeFilled = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    Define LeftCapacity = *Buffer\LeftBufferSizeInBytes - LeftSizeFilled
    
    If LeftCapacity < CapacityInBytes
      
      Define NewLeftBufferSize = AlignToMultipleOf( *Buffer\LeftBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewLeftBuffer = AllocateMemory( NewLeftBufferSize, #PB_Memory_NoClear )
      CopyMemory( *Buffer\LeftBuffer, *NewLeftBuffer, LeftSizeFilled )
      *Buffer\LeftBufferSizeInBytes = NewLeftBufferSize
      *Buffer\LeftBuffer = *NewLeftBuffer
      *Buffer\LeftBufferEnd = *NewLeftBuffer + LeftSizeFilled
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure EnsureRightBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define RightCapacity = *Buffer\RightBufferStart - *Buffer\RightBuffer
    
    If RightCapacity < CapacityInBytes
      
      Define RightSizeFilled = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
      Define NewRightBufferSize = AlignToMultipleOf( *Buffer\RightBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewRightBuffer = AllocateMemory( NewRightBufferSize, #PB_Memory_NoClear )
      Define *NewRightBufferStart = *NewRightBuffer + NewRightBufferSize - RightSizeFilled
      CopyMemory( *Buffer\RightBufferStart, *NewRightBufferStart, RightSizeFilled )
      *Buffer\RightBufferSizeInBytes = NewRightBufferSize
      *Buffer\RightBuffer = *NewRightBuffer
      *Buffer\RightBufferStart = *NewRightBufferStart
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure AllocateGapBuffer( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
    
    DebugAssert( SizeInBytes % StrideInBytes = 0 )
    
    ;REVIEW Defer allocation until we put something in the buffer?
    
    *Buffer\StrideInBytes = StrideInBytes
    *Buffer\LeftBuffer = AllocateMemory( SizeInBytes, #PB_Memory_NoClear )
    *Buffer\RightBuffer = AllocateMemory( SizeInBytes, #PB_Memory_NoClear )
    *Buffer\LeftBufferEnd = *Buffer\LeftBuffer
    *Buffer\RightBufferStart = *Buffer\RightBuffer + SizeInBytes
    *Buffer\LeftBufferSizeInBytes = SizeInBytes
    *Buffer\RightBufferSizeInBytes = SizeInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure FreeGapBuffer( *Buffer.GapBuffer )
    
    If *Buffer\LeftBuffer <> 0
      FreeMemory( *Buffer\LeftBuffer )
    EndIf
    
    If *Buffer\RightBuffer <> 0
      FreeMemory( *Buffer\RightBuffer )
    EndIf
    
    ClearStructure( *Buffer, GapBuffer )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferPosition( *Buffer.GapBuffer )
    
    ProcedureReturn ( *Buffer\LeftBufferEnd - *Buffer\LeftBuffer ) / *Buffer\StrideInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferLength( *Buffer.GapBuffer )
    
    Define LeftBufferSize = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    Define RightBufferSize = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
    
    ProcedureReturn ( LeftBufferSize + RightBufferSize ) / *Buffer\StrideInBytes
    
  EndProcedure

  ;............................................................................
  
  Procedure WriteIntoGapBuffer( *Buffer.GapBuffer, *Ptr, Count.q )
    
    DebugAssert( *Ptr <> 0 )
    DebugAssert( Count >= 0 )
    
    Define SizeOfElements = Count * *Buffer\StrideInBytes
    
    ; See if we need to grow the left buffer.
    EnsureLeftBufferCapacity( *Buffer, SizeOfElements )
    
    ; Copy elements.
    CopyMemory( *Ptr, *Buffer\LeftBufferEnd, SizeOfElements )
    *Buffer\LeftBufferEnd + SizeOfElements
    
  EndProcedure

  ;............................................................................
  
  Procedure.q ReadFromGapBuffer( *Buffer.GapBuffer, *Ptr, Position.q, Count.q )
    
    Define PositionInBytes.q = Position * *Buffer\StrideInBytes
    Define CountInBytes.q = Count * *Buffer\StrideInBytes
    Define *WritePosition = *Ptr
    
    ; Read left buffer portion.
    Define LeftBufferBytes = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    If PositionInBytes < LeftBufferBytes
      
      Define BytesToReadFromLeftBuffer.q = Min( CountInBytes, LeftBufferBytes - PositionInBytes )
      CopyMemory( *Buffer\LeftBuffer + PositionInBytes, *WritePosition, BytesToReadFromLeftBuffer )
      
      If BytesToReadFromLeftBuffer = CountInBytes
        ProcedureReturn BytesToReadFromLeftBuffer / *Buffer\StrideInBytes
      EndIf
      
      *WritePosition + BytesToReadFromLeftBuffer
      CountInBytes - BytesToReadFromLeftBuffer
      PositionInBytes + BytesToReadFromLeftBuffer
      
    EndIf
    
    ; Read right buffer portion.
    Define RightBufferBytes = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
    If PositionInBytes >= LeftBufferBytes And PositionInBytes < LeftBufferBytes + RightBufferBytes
      
      PositionInBytes - LeftBufferBytes
      Define BytesToReadFromRightBuffer.q = Min( CountInBytes, RightBufferBytes - PositionInBytes )
      CopyMemory( *Buffer\RightBufferStart + PositionInBytes, *WritePosition, BytesToReadFromRightBuffer )
      
      *WritePosition + BytesToReadFromRightBuffer
      
    EndIf
    
    ProcedureReturn ( *WritePosition - *Ptr ) / *Buffer\StrideInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure MoveGapInGapBufferRelative( *Buffer.GapBuffer, Offset.q )
    
    Define OffsetInBytes = Offset * *Buffer\StrideInBytes
    
    If Offset > 0
      
      DebugAssert( OffsetInBytes <= *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart )
      
      EnsureLeftBufferCapacity( *Buffer, OffsetInBytes )
      CopyMemory( *Buffer\RightBufferStart, *Buffer\LeftBufferEnd, OffsetInBytes )
      
      *Buffer\LeftBufferEnd + OffsetInBytes
      *Buffer\RightBufferStart + OffsetInBytes
      
    ElseIf Offset < 0
      
      OffsetInBytes * -1
      
      DebugAssert( OffsetInBytes <= *Buffer\LeftBufferEnd - *Buffer\LeftBuffer )
      
      EnsureRightBufferCapacity( *Buffer, OffsetInBytes )
      *Buffer\RightBufferStart - OffsetInBytes
      *Buffer\LeftBufferEnd - OffsetInBytes
      CopyMemory( *Buffer\LeftBufferEnd, *Buffer\RightBufferStart, OffsetInBytes )
      
    EndIf
    
  EndProcedure
    
  ;............................................................................
  
  Procedure MoveGapInGapBufferAbsolute( *Buffer.GapBuffer, Position.q )
    
    DebugAssert( Position >= 0 )
    
    Define CurrentPosition = GetGapBufferPosition( *Buffer )
    If CurrentPosition < Position
      MoveGapInGapBufferRelative( *Buffer, Position - CurrentPosition )
    ElseIf CurrentPosition > Position
      MoveGapInGapBufferRelative( *Buffer, - ( CurrentPosition - Position ) )
    EndIf
    
  EndProcedure
  
EndModule

ProcedureUnit CanCreateGapBuffer()

  UseModule GapBuffer
  
  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, 1 )
  
  Assert( Buffer\LeftBuffer <> 0 )
  Assert( Buffer\RightBuffer <> 0 )
  Assert( MemorySize( Buffer\LeftBuffer ) = 256 )
  Assert( MemorySize( Buffer\RightBuffer ) = 256 )
  Assert( Buffer\LeftBufferSizeInBytes = 256 )
  Assert( Buffer\RightBufferSizeInBytes = 256 )
  Assert( Buffer\StrideInBytes = 1 )
  Assert( Buffer\LeftBufferEnd = Buffer\LeftBuffer )
  Assert( Buffer\RightBufferStart = Buffer\RightBuffer + 256 )
  
  FreeGapBuffer( @Buffer )
  
  Assert( Buffer\LeftBuffer = 0 )
  Assert( Buffer\RightBuffer = 0 )
  Assert( Buffer\LeftBufferEnd = 0 )
  Assert( Buffer\RightBufferStart = 0 )
  Assert( Buffer\LeftBufferSizeInBytes = 0 )
  Assert( Buffer\RightBufferSizeInBytes = 0 )

EndProcedureUnit
  
ProcedureUnit CanInsertDataIntoGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Character ) )
  
  Assert( GetGapBufferLength( @Buffer ) = 0 )
  
  WriteIntoGapBuffer( @Buffer, @"Test", 5 ) ; Include NUL.
  
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  Assert( PeekS( Buffer\LeftBuffer ) = "Test" )

EndProcedureUnit
  
ProcedureUnit CanMoveGapInGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 4, SizeOf( Character ) )
  
  Assert( GetGapBufferPosition( @Buffer ) = 0 )
  
  WriteIntoGapBuffer( @Buffer, @"First", 5 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 5 )
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  
  MoveGapInGapBufferRelative( @Buffer, -2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 3 )
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  
  WriteIntoGapBuffer( @Buffer, @"me", 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 5 )
  Assert( GetGapBufferLength( @Buffer ) = 7 )
  Assert( PeekS( Buffer\LeftBuffer, 5 ) = "Firme" )
  Assert( PeekS( Buffer\RightBufferStart, 2 ) = "st" )
  
  MoveGapInGapBufferRelative( @Buffer, 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 7 )
  Assert( GetGapBufferLength( @Buffer ) = 7 )
  Assert( PeekS( Buffer\LeftBuffer, 7 ) = "Firmest" )
  
  WriteIntoGapBuffer( @Buffer, @"est", 3 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 10 )
  Assert( GetGapBufferLength( @Buffer ) = 10 )
  Assert( PeekS( Buffer\LeftBuffer, 10 ) = "Firmestest" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 2 )
  Assert( GetGapBufferLength( @Buffer ) = 10 )
  Assert( PeekS( Buffer\LeftBuffer, 2 ) = "Fi" )
  Assert( PeekS( Buffer\RightBufferStart, 8 ) = "rmestest" )

EndProcedureUnit
  
ProcedureUnit CanReadFromGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 4, SizeOf( Character ) )
  
  WriteIntoGapBuffer( @Buffer, @"First", 5 )
  WriteIntoGapBuffer( @Buffer, @"Second", 6 )
  
  Define String.s = Space( 6 )
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 8, 3 )
  
  Assert( String = "ondond" )
  
  ReadFromGapBuffer( @Buffer, @String, 4, 6 )
  
  Assert( String = "tSecon" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 5 )
  
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 1, 6 )
  
  UnitDebug( String )
  Assert( String = "irstSe" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 0 )
  
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 0, 6 )
  
  Assert( String = "FirstS" )

EndProcedureUnit

;==============================================================================

;move existing TextBuffer stuff here
;fill buffer from GapBuffer

DeclareModule TextRenderBuffer
  
  Structure TextRenderBuffer
  EndStructure
  
EndDeclareModule

Module TextRenderBuffer
EndModule

;==============================================================================

;newline positions and any other marker that needs to "stick" to text and remain in the same place

DeclareModule TextMarker
  ;gap marker positions are 64bit; negative is for left buffer, positive for right buffer
  
  
  Structure TextMarker
    Position.q
  EndStructure
  
EndDeclareModule

Module TextMarker
  
EndModule

;==============================================================================

; Central structure that holds a piece of text.
Structure TextBuffer
  *Text                   ; Raw text in UTF-8.
EndStructure

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
; CursorPosition = 278
; FirstLine = 258
; Folding = -----
; EnableXP
; HideErrorLog