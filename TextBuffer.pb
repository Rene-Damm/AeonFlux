EnableExplicit

XIncludeFile "GapBuffer.pb"
XIncludeFile "TextMarker.pb"

;;REVIEW: For very large files, consider having a tree of gap buffers

DeclareModule TextBuffer
  
  UseModule GapBuffer
  UseModule TextMarker
  
  ;where does the text cursor live?
  ;do I disassociate it with the gap buffer position and allow it to move freely in normal mode?
  
  ;............................................................................
  
  Structure TextBuffer
    Text.GapBuffer                ; The actual text *in UTF-8*.
    LinePositions.TextMarkerList  ; Positions of line starts.
    
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
  Declare   WriteUTF8IntoTextBuffer( *Buffer.TextBuffer, Position.q, *Text, Count.q )
  Declare   WriteStringIntoTextBuffer( *Buffer.TextBuffer, Position.q, Text.s, Length.i = -1 )
  Declare.s ReadStringFromTextBuffer( *Buffer.TextBuffer, Position.q = 0, Count.q = -1 )
  CompilerIf #False
  Declare   DeleteRangeFromTextBuffer( *Buffer.TextBuffer, Position.q, Count.q )
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
  
  Procedure RepositionGap( *Buffer.TextBuffer, NewPosition.q )
    
    DebugAssert( NewPosition >= 0 )
    
    Define CurrentPosition.q = GetGapBufferPosition( *Buffer\Text )
    If NewPosition > CurrentPosition
      
      ; Move gap in text buffer.
      MoveGapInGapBufferRelative( *Buffer\Text, NewPosition - CurrentPosition )
      
      ; Move gap in line marker buffer.
      
    ElseIf NewPosition < CurrentPosition
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure CreateTextBuffer( *Buffer.TextBuffer )
    InitializeGapBuffer( *Buffer\Text, 1 )
    InitializeTextMarkerList( *Buffer\LinePositions )
    AddMarkerToTextMarkerList( *Buffer\LinePositions, 0 )
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyTextBuffer( *Buffer.TextBuffer )
    FreeGapBuffer( *Buffer\Text )
    DestroyTextMarkerList( *Buffer\LinePositions )
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextBufferLength( *Buffer.TextBuffer )
    ProcedureReturn GetGapBufferLength( *Buffer\Text )
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextBufferLineCount( *Buffer.TextBuffer )
    ProcedureReturn GetTextMarkerListLength( *Buffer\LinePositions )
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteUTF8IntoTextBuffer( *Buffer.TextBuffer, Position.q, *Text, Count.q )
    
    DebugAssert( *Text <> 0 )
    DebugAssert( Count >= 0 )
    
    ; Reposition gap, if necessary.
    Define CurrentPosition.q = GetGapBufferPosition( *Buffer\Text )
    If CurrentPosition <> Position
      MoveGapInGapBufferAbsolute( *Buffer\Text, Position )
    EndIf
    
    ; Add text to buffer.
    WriteIntoGapBuffer( *Buffer\Text, *Text, Count )
    
    ; Add line markers, if necessary.
    For I = 0 To Count - 1
      Define.b Char = PeekB( *Text + I )
      If Char = 10 ; '\n'
        AddMarkerToTextMarkerList( *Buffer\LinePositions, Position + I )
      EndIf
    Next
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteStringIntoTextBuffer( *Buffer.TextBuffer, Position.q, Text.s, Length.i = -1 )
    
    CompilerIf #PB_Unicode
      
      If Length < 0
        Length = Len( Text )
      EndIf
            
      ;;OPTIMIZE: This is slow; any better way to do this?
      ;;          (Can we convert single characters and write them one by one?)
      
      ; Convert into UTF-8 into a temporary buffer.
      Define.b *TempBuffer = AllocateMemory( Length * 2 + 1 )
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
    
    CompilerIf #PB_Unicode
      
      ;;OPTIMIZE: This can be done much faster
      Define *TempBuffer = AllocateMemory( Count )
      ReadFromGapBuffer( *Buffer\Text, *TempBuffer, Position, Count )
      Define.s Result = PeekS( *TempBuffer, Count, #PB_UTF8 )
      FreeMemory( *TempBuffer )
      ProcedureReturn Result
      
    CompilerElse
      
      Define.s Result = Space( Count )
      ReadFromGapBuffer( *Buffer\Text, @Result, Position, Count )
      ProcedureReturn Result
      
    CompilerEndIf
    
  EndProcedure
  
EndModule

ProcedureUnit CanWriteIntoTextBuffer()

  UseModule TextBuffer

  Define *UTF8First = UTF8( "First" )

  Define.TextBuffer Buffer
  CreateTextBuffer( @Buffer )
  
  Assert( GetTextBufferLength( @Buffer ) = 0 )
  Assert( GetTextBufferLineCount( @Buffer ) = 1 )
  
  WriteStringIntoTextBuffer( @Buffer, 0, "First" )
  
  Assert( GetTextBufferLength( @Buffer ) = 5 )
  Assert( GetTextBufferLineCount( @Buffer ) = 1 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = "First" )
  
  WriteStringIntoTextBuffer( @Buffer, 5, ~"\nSecond" )
  
  Assert( GetTextBufferLength( @Buffer ) = 12 )
  Assert( GetTextBufferLineCount( @Buffer ) = 2 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"First\nSecond" )
  
  WriteStringIntoTextBuffer( @Buffer, 12, ~"\n" )
  
  Assert( GetTextBufferLength( @Buffer ) = 13 )
  Assert( GetTextBufferLineCount( @Buffer ) = 3 )
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"First\nSecond\n" )
  
  DestroyTextBuffer( @Buffer )
  
  FreeMemory( *UTF8First )
  
EndProcedureUnit  

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 5
; Folding = --
; EnableXP