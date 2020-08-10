EnableExplicit

XIncludeFile "GapBuffer.pb"
XIncludeFile "TextMarker.pb"

;;REVIEW: Have reference counts on buffers?
;;REVIEW: For very large files, consider having a tree of gap buffers

;;TODO: styling information

; Adds line tracking, markers, and styling to raw UTF-8 text kept in a gap buffer.
DeclareModule TextBuffer
  
  UseModule GapBuffer
  UseModule TextMarker
 
  ;............................................................................
  
  Structure TextBuffer
    
    Text.GapBuffer                ; The actual text *in UTF-8*.
    LinePositions.TextMarkerList  ; Positions of line *starts* (i.e. first character in line).
    
    ;;REVIEW: add storage location?
    ;;REVIEW: link all buffers into global list? (or optionally)
    
    ;implement job system
    ;have list of jobs tied to text buffer
    ;N reader jobs, max 1 writer job
    ;mutating buffer requires completing or canceling reader jobs
    
  EndStructure

  ;............................................................................

  Declare   CreateTextBuffer( *Buffer.TextBuffer )
  Declare   DestroyTextBuffer( *Buffer.TextBuffer )
  
  Declare.q GetTextBufferLength( *Buffer.TextBuffer )
  Declare.q GetTextBufferLineCount( *Buffer.TextBuffer )
  Declare.q GetTextBufferLineStart( *Buffer.TextBuffer, LineNumber.i )
  Declare.i GetTextBufferLineLength( *Buffer.TextBuffer, LineNumber.i )
  
  ;;REVIEW: should this rather have a write-head, too? would probably save us a bunch of checks whether we're at the right position
  
  Declare   WriteUTF8IntoTextBuffer( *Buffer.TextBuffer, Position.q, *Text, Count.q )
  Declare   WriteCharacterIntoTextBuffer( *Buffer.TextBuffer, Position.q, Character.c )
  Declare   WriteStringIntoTextBuffer( *Buffer.TextBuffer, Position.q, Text.s, Length.i = -1 )
  Declare.s ReadStringFromTextBuffer( *Buffer.TextBuffer, Position.q = 0, Count.q = -1 )
  Declare   DeleteRangeFromTextBuffer( *Buffer.TextBuffer, Position.q, Count.q )
  
  CompilerIf #False
  Declare   ReplaceUTF8InTextBuffer( *Buffer.TextBuffer, ... )
  Declare.q GetCurrentPositionInTextBuffer( *Buffer.TextBuffer )
  Declare.q ReadFromTextBuffer( *Buffer.TextBuffer, *Position.q, *Text, Count.q )
  Declare.q GetLineNumberInTextBuffer( *Buffer.TextBuffer, Position.q )
  CompilerEndIf

EndDeclareModule

Module TextBuffer
  
  UseModule GapBuffer
  UseModule TextMarker
  UseModule Utils
    
  ;............................................................................
  
  Procedure CreateTextBuffer( *Buffer.TextBuffer )
    
    InitializeGapBuffer( @*Buffer\Text, 1 ) ; We always store in UTF-8 even if in #PB_Unicode.
    InitializeTextMarkerList( @*Buffer\LinePositions )
    AddMarkerToTextMarkerList( @*Buffer\LinePositions, 0 )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyTextBuffer( *Buffer.TextBuffer )
    
    FreeGapBuffer( @*Buffer\Text )
    DestroyTextMarkerList( @*Buffer\LinePositions )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextBufferLength( *Buffer.TextBuffer )
    
    ProcedureReturn GetGapBufferLength( @*Buffer\Text )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextBufferLineCount( *Buffer.TextBuffer )

    DebugAssert( *Buffer <> #Null )
    ProcedureReturn GetTextMarkerListLength( @*Buffer\LinePositions )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextBufferLineStart( *Buffer.TextBuffer, LineNumber.i )
    
    DebugAssert( *Buffer <> #Null )
    DebugAssert( LineNumber > 0 )
    DebugAssert( LineNumber <= GetTextBufferLineCount( *Buffer ) )
    
    ProcedureReturn GetMarkerPosition( @*Buffer\LinePositions, LineNumber - 1 )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.i GetTextBufferLineLength( *Buffer.TextBuffer, LineNumber.i )
    
    DebugAssert( *Buffer <> #Null )
    DebugAssert( LineNumber > 0 )
    DebugAssert( LineNumber <= GetTextBufferLineCount( *Buffer ) )
    
    Define.q StartPositionOfLine = GetMarkerPosition( @*Buffer\LinePositions, LineNumber - 1 )
    Define.q StartPositionOfNextLine
    
    If LineNumber = GetGapBufferLength( @*Buffer\LinePositions )
      
      ; Last line.
      StartPositionOfNextLine = GetGapBufferLength( @*Buffer\Text ) + 1 ; Pretend there's a newline.
      If StartPositionOfNextLine = 1
        ProcedureReturn 0 ; Empty buffer.
      EndIf
      
    Else
      
      StartPositionOfNextLine = GetMarkerPosition( @*Buffer\LinePositions, LineNumber )
      
    EndIf
    
    ProcedureReturn StartPositionOfNextLine - StartPositionOfLine - 1 ; Don't include #LF in count.
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteUTF8IntoTextBuffer( *Buffer.TextBuffer, Position.q, *Text, Count.q )
    
    DebugAssert( *Text <> #Null )
    DebugAssert( Position >= 0 )
    DebugAssert( Count >= 0 )
    
    ; Reposition gap, if necessary.
    Define CurrentPosition.q = GetGapBufferPosition( @*Buffer\Text )
    If CurrentPosition <> Position
      MoveGapInGapBufferAbsolute( @*Buffer\Text, Position )
      ; Need to reposition linemarker gap, too, to ensure that we can insert new
      ; text without having to update any positions to the right of the current position.
      PositionTextMarkerListGap( @*Buffer\LinePositions, Position )
    EndIf
    
    ; Add text to buffer.
    WriteIntoGapBuffer( @*Buffer\Text, *Text, Count )
    
    ; Account for text we added in line marker positions.
    ShiftLastMarkerPosition( @*Buffer\LinePositions, Count )
    
    ; Add line markers, if necessary.
    For I = 0 To Count - 1
      Define.b Char = PeekB( *Text + I )
      If Char = #LF
        ; Add line marker pointing to first character *after* newline.
        AddMarkerToTextMarkerList( @*Buffer\LinePositions, Position + I + 1 )
      EndIf
    Next
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteCharacterIntoTextBuffer( *Buffer.TextBuffer, Position.q, Character.c )
    
    DebugAssert( *Buffer <> #Null )
    DebugAssert( Position >= 0 )
    
    CompilerIf #PB_Unicode
      
      If Character < 256
        ; Assumes little endian.
        WriteUTF8IntoTextBuffer( *Buffer, Position, @Character, 1 )
      Else
        NotImplemented( "Convert UTF-16 character to UTF-18" )
      EndIf
      
    CompilerElse
      
      WriteUTF8IntoTextBuffer( *Buffer, Position, @Character, 1 )
      
    CompilerEndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteStringIntoTextBuffer( *Buffer.TextBuffer, Position.q, Text.s, Length.i = -1 )
    
    DebugAssert( *Buffer <> #Null )
    
    CompilerIf #PB_Unicode
      
      If Length < 0
        Length = Len( Text )
      EndIf
            
      ;;OPTIMIZE: This is slow; any better way to do this?
      ;;          (Can we convert single characters and write them one by one?)
      
      ; Convert into UTF-8 into a temporary buffer.
      Define.b *TempBuffer = AllocateMemory( Length * 3 + 1 )
      PokeS( *TempBuffer, Text, Length, #PB_UTF8 )
      Define.q Count = MemoryStringLength( *TempBuffer, #PB_UTF8 | #PB_ByteLength )
      WriteUTF8IntoTextBuffer( *Buffer, Position, *TempBuffer, Count )
      FreeMemory( *TempBuffer )
      
    CompilerElse
      
      If Length < 0
        Length = StringByteLength( Text )
      EndIf
      WriteUTF8IntoTextBuffer( *Buffer, Position, @Text, Length )
      
    CompilerEndIf

    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s ReadStringFromTextBuffer( *Buffer.TextBuffer, Position.q = 0, Count.q = -1 )
    
    If Count < 0
      Count = GetGapBufferLength( *Buffer\Text )
    EndIf
    
    If Count = 0
      ProcedureReturn ""
    EndIf
    
    CompilerIf #PB_Unicode
      
      ;;OPTIMIZE: This can be done much faster
      Define *TempBuffer = AllocateMemory( Count )
      ReadFromGapBuffer( @*Buffer\Text, *TempBuffer, Position, Count )
      Define.s Result = PeekS( *TempBuffer, Count, #PB_UTF8 )
      FreeMemory( *TempBuffer )
      ProcedureReturn Result
      
    CompilerElse
      
      Define.s Result = Space( Count )
      ReadFromGapBuffer( @*Buffer\Text, @Result, Position, Count )
      ProcedureReturn Result
      
    CompilerEndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DeleteRangeFromTextBuffer( *Buffer.TextBuffer, Position.q, Count.q )
    
    DebugAssert( *Buffer <> #Null )
    DebugAssert( Position >= 0 )
    DebugAssert( Count >= 0 ) ; ATM negative range not implemented.
    
    ; Reposition gap, if necessary.
    Define CurrentPosition.q = GetGapBufferPosition( @*Buffer\Text )
    If CurrentPosition <> Position
      MoveGapInGapBufferAbsolute( @*Buffer\Text, Position )
      PositionTextMarkerListGap( @*Buffer\LinePositions, Position )
    EndIf
    
    ; Delete line markers, if necessary.
    Define.q EndPosition = Position + Count
    While GetMarkerPositionRightOfGap( @*Buffer\LinePositions ) < EndPosition
      EraseFromGapBuffer( @*Buffer\LinePositions, 1 )
    Wend
    
    ; Remove text.
    EraseFromGapBuffer( @*Buffer\Text, Count )
    
    ; Update/remove line markers.
    ShiftLastMarkerPosition( @*Buffer\LinePositions, - Count )
    
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanWriteIntoTextBuffer()

  UseModule TextBuffer

  Define *UTF8First = UTF8( "First" )

  Define.TextBuffer Buffer
  CreateTextBuffer( @Buffer )
  
  Assert( GetTextBufferLength( @Buffer ) = 0 )
  Assert( GetTextBufferLineCount( @Buffer ) = 1 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = "" )
  
  WriteStringIntoTextBuffer( @Buffer, 0, "First" )
  
  Assert( GetTextBufferLength( @Buffer ) = 5 )
  Assert( GetTextBufferLineCount( @Buffer ) = 1 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 5 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = "First" )
  
  WriteStringIntoTextBuffer( @Buffer, 5, ~"\nSecond" )
  
  Assert( GetTextBufferLength( @Buffer ) = 12 )
  Assert( GetTextBufferLineCount( @Buffer ) = 2 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 5 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineLength( @Buffer, 2 ) = 6 )
  Assert( GetTextBufferLineStart( @Buffer, 2 ) = 6 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"First\nSecond" )
  
  WriteStringIntoTextBuffer( @Buffer, 12, ~"\n" )
  
  Assert( GetTextBufferLength( @Buffer ) = 13 )
  Assert( GetTextBufferLineCount( @Buffer ) = 3 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 5 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineLength( @Buffer, 2 ) = 6 )
  Assert( GetTextBufferLineStart( @Buffer, 2 ) = 6 )
  Assert( GetTextBufferLineLength( @Buffer, 3 ) = 0 )
  Assert( GetTextBufferLineStart( @Buffer, 3 ) = 13 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"First\nSecond\n" )
  
  WriteCharacterIntoTextBuffer( @Buffer, 1, '\' )
    
  Assert( GetTextBufferLength( @Buffer ) = 14 )
  Assert( GetTextBufferLineCount( @Buffer ) = 3 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 6 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineLength( @Buffer, 2 ) = 6 )
  Assert( GetTextBufferLineStart( @Buffer, 2 ) = 7 )
  Assert( GetTextBufferLineLength( @Buffer, 3 ) = 0 )
  Assert( GetTextBufferLineStart( @Buffer, 3 ) = 14 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"F\\irst\nSecond\n" )
  
  DestroyTextBuffer( @Buffer )
  
  FreeMemory( *UTF8First )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanDeleteFromTextBuffer()

  UseModule TextBuffer

  Define.TextBuffer Buffer
  CreateTextBuffer( @Buffer )
  WriteStringIntoTextBuffer( @Buffer, 0, ~"Foo\nBarb" )
  
  DeleteRangeFromTextBuffer( @Buffer, 2, 1 )
  
  Assert( GetTextBufferLength( @Buffer ) = 7 )
  Assert( GetTextBufferLineCount( @Buffer ) = 2 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 2 )
  Assert( GetTextBufferLineStart( @Buffer, 2 ) = 3 )
  Assert( GetTextBufferLineLength( @Buffer, 2 ) = 4 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"Fo\nBarb" )
  
  DeleteRangeFromTextBuffer( @Buffer, 2, 2 )
  
  Assert( GetTextBufferLength( @Buffer ) = 5 )
  Assert( GetTextBufferLineCount( @Buffer ) = 1 )
  Assert( GetTextBufferLineStart( @Buffer, 1 ) = 0 )
  Assert( GetTextBufferLineLength( @Buffer, 1 ) = 5 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"Foarb" )
  
  DestroyTextBuffer( @Buffer )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 385
; FirstLine = 331
; Folding = ---
; Markers = 268,283
; EnableXP