EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "TextEditor.pb"
XIncludeFile "Shell.pb"

; Adds a command model around TextEditor.
; Command model is very Vim-inspired.
DeclareModule TextEditorShell
  
  UseModule TextEditor
  UseModule TextBuffer
    
  ;............................................................................
  
  Enumeration TextEditorMode
    #NormalMode
    #InsertMode
  EndEnumeration
  
  ;............................................................................
  
  Structure TextEditorShell
    *Methods
    Mode.i
    Editor.TextEditor
    Buffer.TextBuffer
  EndStructure
    
  ;............................................................................

  Declare CreateTextEditorShell( *Editor.TextEditorShell )

EndDeclareModule

Module TextEditorShell
  
  UseModule Utils
  UseModule TextEditor
  UseModule TextBuffer
  UseModule Shell
  
  ;............................................................................
  
  Enumeration TextEditorCommands
    
    ; Mode commands.
    #TextEditorEnterInsertMode
    #TextEditorExitInsertMode
    
    ; Cursor motion commands.
    #TextEditorMoveLeft
    #TextEditorMoveRight
    #TextEditorMoveUp
    #TextEditorMoveDown
    
  EndEnumeration
  
  ;............................................................................
  
  Procedure CreateTextEditorShell( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    InitializeStructure( *Editor, TextEditorShell )
    
    *Editor\Methods = ?TextEditor_VTable
    *Editor\Mode = #NormalMode
    
    CreateTextBuffer( @*Editor\Buffer )
    CreateTextEditor( @*Editor\Editor, @*Editor\Buffer )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TextEditor_GetType( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    ProcedureReturn "TextEditor"

  EndProcedure
  
  ;............................................................................
  
  Procedure.s TextEditor_GetMode( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    Select *Editor\Mode
      Case #InsertMode
        ProcedureReturn "insert"
    EndSelect
    
    ProcedureReturn "normal"

  EndProcedure
  
  ;............................................................................
  
  Procedure.q TextEditor_GetCommands( *Editor.TextEditorShell, *OutCommands.Command )
    
    DebugAssert( *Editor <> #Null )
    
    PokeQ( *OutCommands, ?TextEditor_Commands )
    ProcedureReturn 6
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TextEditor_ExecuteCommand( *Editor.TextEditorShell, CommandId.i, Map Parameters.ParameterValue() )
    
    DebugAssert( *Editor <> #Null )
    
    Select CommandId
        
      Case #TextEditorEnterInsertMode
        *Editor\Mode = #InsertMode
        
      Case #TextEditorExitInsertMode
        *Editor\Mode = #NormalMode
        
      Case #TextEditorMoveLeft
        MoveCursorInTextEditor( *Editor\Editor, -1, 0 )
        
      Case #TextEditorMoveRight
        MoveCursorInTextEditor( *Editor\Editor, 1, 0 )
        
      Case #TextEditorMoveUp
        MoveCursorInTextEditor( *Editor\Editor, 0, -1 )
        
      Case #TextEditorMoveDown
        MoveCursorInTextEditor( *Editor\Editor, 0, 1 )
        
    EndSelect
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TextEditor_SendInput( *Editor.TextEditorShell, Input.s )
    
    DebugAssert( *Editor <> #Null )
    
    InsertStringIntoTextEditor( @*Editor\Editor, Input )
    
  EndProcedure
  
  ;............................................................................
  
  DataSection
    
    EditorData( TextEditor )
    
    TextEditor_Commands:
      CommandData( #TextEditorEnterInsertMode, "insert", "normal", "i", 0 )
      CommandData( #TextEditorExitInsertMode, "exit", "insert", "<ESC>", 0 )
      CommandData( #TextEditorMoveLeft, "move_left", "normal", "h", 0 )
      CommandData( #TextEditorMoveRight, "move_right", "normal", "l", 0 )
      CommandData( #TextEditorMoveUp, "move_up", "normal", "k", 0 )
      CommandData( #TextEditorMoveDown, "move_down", "normal", "j", 0 )
    
  EndDataSection
  
EndModule

;..............................................................................

ProcedureUnit CanCreateTextEditorInShell()

  UseModule Shell
  UseModule TextEditorShell
  UseModule TextBuffer
  UseModule TextEditor
  
  Define.Shell Shell
  CreateShell( @Shell )
  Define.IEditor *Editor = CreateEditor( @Shell, SizeOf( TextEditorShell ), @CreateTextEditorShell() )
  Define.TextEditorShell *TextEditor = *Editor
  
  Assert( *Editor\GetType() = "TextEditor" )
  Assert( *Editor\GetMode() = "normal" )
  Assert( GetCurrentEditor( @Shell ) = *Editor )
  
  SendShellInput( @Shell, "itext" )
  
  Assert( *Editor\GetMode() = "insert" )
  Assert( GetTextBufferLength( *TextEditor\Buffer ) = 4 )
  Assert( GetTextBufferLineCount( *TextEditor\Buffer ) = 1 )
  
  DestroyShell( @Shell )  

EndProcedureUnit

;..............................................................................

ProcedureUnit CanMoveTextEditorCursorInShell()

  UseModule Shell
  UseModule TextEditorShell
  UseModule TextBuffer
  UseModule TextEditor
  
  Define.Shell Shell
  CreateShell( @Shell )
  Define.IEditor *Editor = CreateEditor( @Shell, SizeOf( TextEditorShell ), @CreateTextEditorShell() )
  Define.TextEditorShell *TextEditor = *Editor
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 1 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 1 )
  
  SendShellInput( @Shell, ~"itext\ntxet<ESC>" )
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 2 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 5 )
  
  SendShellInput( @Shell, "hh" )
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 2 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 3 )
  
  SendShellInput( @Shell, "k" )
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 1 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 3 )
  
  SendShellInput( @Shell, "j" )
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 2 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 3 )
  
  SendShellInput( @Shell, "l" )
  
  Assert( GetCursorLineNumberFromTextEditor( @*TextEditor\Editor ) = 2 )
  Assert( GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor ) = 4 )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 213
; FirstLine = 175
; Folding = --
; EnableXP