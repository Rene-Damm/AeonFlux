EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "TextBuffer.pb"

;should this be renamed so that "TextEditor" can be an IEditor and implement commands? (or put that in TextEditorShell?)
;should the stuff here be merged into "TextBuffer"?

;allow restricting an editor to a limited view of a buffer?

;how does editing text relate to editing other pieces of information presented in the UI? shouldn't it be all one command/edit model?
;how is, for example, the running program represented and how is it interacted with? (both in terms of sending input to the running program as well as in terms of invoking commands for controlling it)

; Adds cursors, selections, and undo to a text buffer.
; Does not implement a specific text editing model (e.g. Vim) but rather only deals with raw text manipulation.
DeclareModule TextEditor
  
  UseModule TextBuffer
  UseModule TextMarker
  ;UseModule Undo
  
  ;............................................................................

  Structure TextEditor
    *Buffer.TextBuffer ;;REVIEW: should this be *owned* by the text editor?
    CursorPosition.q
    CursorColumn.q
    CursorLine.q
    SelectedColumns.i       ; Num chars selected horizontally from cursor position; can be negative.
    SelectedLines.i         ; Num lines selected vertically from cursor position; can be negative.
  EndStructure

  ;............................................................................

  Declare   CreateTextEditor( *Editor.TextEditor, *Buffer.TextBuffer )
  Declare   InsertUTF8IntoTextEditor( *Editor.TextEditor, *Ptr, Count.q )
  Declare   InsertCharacterIntoTextEditor( *Editor.TextEditor, Character.c )
  Declare   InsertStringIntoTextEditor( *Editor.TextEditor, Text.s )
  Declare.q GetCursorLineNumberFromTextEditor( *Editor.TextEditor )
  Declare.q GetCursorColumnNumberFromTextEditor( *Editor.TextEditor )
  
  CompilerIf #False
  Declare   MoveTextCursor( *Editor.TextEditor, NumColumns.i, NumLines.i = 0, StopAtLineBoundaries.i = #True )
  Declare   BeginTextSelection( *Editor.TextEditor )
  Declare   EndTextSelection( *Editor.TextEditor)
  Declare   BeginTextEdit( *Editor.TextEditor )
  Declare   EndTextEdit( *Editor.TextEditor )
  Declare.s GetFullTextFromTextEditorAsString( *Editor.TextEditor )
  Declare.s GetTextSelectedInTextEditorAsString( *Editor.TextEditor )
  CompilerEndIf

EndDeclareModule

Module TextEditor
  
  UseModule TextBuffer
  UseModule Utils
  
  ;............................................................................
  
  Procedure CreateTextEditor( *Editor.TextEditor, *Buffer.TextBuffer )
    
    DebugAssert( *Editor <> #Null )
    
    *Editor\Buffer = *Buffer
    *Editor\CursorPosition = 0
    *Editor\CursorLine = 1
    *Editor\CursorColumn = 1
    *Editor\SelectedColumns = 0
    *Editor\SelectedLines = 0
    
  EndProcedure
  
  ;............................................................................
  
  Procedure InsertUTF8IntoTextEditor( *Editor.TextEditor, *Ptr, Count.q )
    
    DebugAssert( *Editor <> #Null )
    
    If Count <= 0
      ProcedureReturn
    EndIf
    
    ; Find column number we will end up at.
    Define.q Position
    Define.q Column = Count + 1
    Define.i HasMultipleLines = #False
    Define.q Position = Count - 1
    Repeat
      If PeekB( *Ptr + Position ) = #LF
        Column - ( Position + 1 )
        HasMultipleLines = #True
        Break
      EndIf
      Position - 1
    Until Position < 0
    
    Define.q NumLinesBefore
    If HasMultipleLines
      NumLinesBefore = GetTextBufferLineCount( *Editor\Buffer )
    EndIf

    ; Insert text.
    WriteUTF8IntoTextBuffer( *Editor\Buffer, *Editor\CursorPosition, *Ptr, Count )
    
    ; Update cursor position.
    *Editor\CursorPosition + Count
    *Editor\CursorColumn = Column
    If HasMultipleLines
      *Editor\CursorLine + ( GetTextBufferLineCount( *Editor\Buffer ) - NumLinesBefore )
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure InsertCharacterIntoTextEditor( *Editor.TextEditor, Character.c )
    
    DebugAssert( *Editor <> #Null )
    
    CompilerIf #PB_Unicode
      
      If Character < 256
        ; Assumes little endian.
        InsertUTF8IntoTextEditor( *Editor, @Character, 1 )
      Else
        NotImplemented( "Converting UTF-16 character to UTF-8" )
      EndIf
      
    CompilerElse
      
      InsertUTF8IntoTextEditor( *Editor, @Character, 1 )
      
    CompilerEndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure InsertStringIntoTextEditor( *Editor.TextEditor, Text.s )
    
    DebugAssert( *Editor <> #Null )
    
    CompilerIf #PB_Unicode
      
      Define.q Length = Len( Text )
      Define.b *TempBuffer = AllocateMemory( Length * 3 + 1 )
      PokeS( *TempBuffer, Text, Length, #PB_UTF8 )
      Define.q Count = MemoryStringLength( *TempBuffer, #PB_UTF8 | #PB_ByteLength )
      InsertUTF8IntoTextEditor( *Editor, *TempBuffer, Count )
      FreeMemory( *TempBuffer )
      
    CompilerElse
      
      InsertUTF8IntoTextEditor( *Editor, @Text, StringByteLength( Text ) )
      
    CompilerEndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetCursorLineNumberFromTextEditor( *Editor.TextEditor )
    
    DebugAssert( *Editor <> #Null )
    
    ProcedureReturn *Editor\CursorLine
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetCursorColumnNumberFromTextEditor( *Editor.TextEditor )
    
    DebugAssert( *Editor <> #Null )
    
    ProcedureReturn *Editor\CursorColumn
    
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanInsertTextThroughTextEditor()

  UseModule TextEditor
  UseModule TextBuffer
  UseModule Utils
  
  Define.TextBuffer Buffer
  Define.TextEditor Editor
  
  CreateTextBuffer( @Buffer )
  CreateTextEditor( @Editor, @Buffer )
  
  Assert( ReadStringFromTextBuffer( @Buffer ) = "" )
  Assert( GetCursorLineNumberFromTextEditor( @Editor ) = 1 )
  Assert( GetCursorColumnNumberFromTextEditor( @Editor ) = 1 )
  
  InsertStringIntoTextEditor( @Editor, "Test" )
  
  Assert( ReadStringFromTextBuffer( @Buffer ) = "Test" )
  Assert( GetCursorLineNumberFromTextEditor( @Editor ) = 1 )
  Assert( GetCursorColumnNumberFromTextEditor( @Editor ) = 5 )
  
  InsertStringIntoTextEditor( @Editor, ~"\nFoobar\n" )
  
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"Test\nFoobar\n" )
  Assert( GetCursorLineNumberFromTextEditor( @Editor ) = 3 )
  Assert( GetCursorColumnNumberFromTextEditor( @Editor ) = 1 )
  
  InsertCharacterIntoTextEditor( @Editor, #LF )
  
  Assert( ReadStringFromTextBuffer( @Buffer ) = ~"Test\nFoobar\n\n" )
  Assert( GetCursorLineNumberFromTextEditor( @Editor ) = 4 )
  Assert( GetCursorColumnNumberFromTextEditor( @Editor ) = 1 )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 212
; FirstLine = 161
; Folding = --
; EnableXP