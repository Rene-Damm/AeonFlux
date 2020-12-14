EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "TextEditor.pb"
XIncludeFile "Shell.pb"
XIncludeFile "Workspace.pb"

;;REVIEW: should all blob editors derive from the same base editor shell?

; Adds a command model around TextEditor.
; Command model is very Vim-inspired.
DeclareModule TextEditorShell
  
  UseModule TextEditor
  UseModule TextBuffer
  UseModule Workspace
  UseModule Shell
    
  ;............................................................................
  
  ; Extend EditorMode with text editor-specific modes.
  Enumeration EditorMode
    #InsertMode
  EndEnumeration
  
  ;............................................................................
  
  Structure TextEditorShell Extends Editor
    
    Editor.TextEditor
    Buffer.TextBuffer
    *Workspace.Workspace
    *TextBlob.Blob
    
  EndStructure
    
  ;............................................................................

  Declare   CreateTextEditorShell( *Editor.TextEditorShell )

EndDeclareModule

Module TextEditorShell
  
  UseModule Utils
  UseModule TextEditor
  UseModule TextBuffer
  UseModule Shell
  
  ;............................................................................
  
  ; Extend EditorCommands with text editor-specific commands.
  Enumeration EditorCommands
    
    ; Mode commands.
    #TextEditorEnterInsertMode
    #TextEditorExitInsertMode
    
    ; Cursor motion commands.
    #TextEditorMoveLeft
    #TextEditorMoveRight
    #TextEditorMoveUp
    #TextEditorMoveDown
    #TextEditorMoveNextLine
    
    ; Deletion commands.
    #TextEditorDeleteLeft
    
    ; Insertion commands.
    #TextEditorInsertLineBreak
    
    ; Workspace commands.
    
  EndEnumeration
  
  ;............................................................................
  
  Procedure CreateTextEditorShell( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    InitializeStructure( *Editor, TextEditorShell )
    
    *Editor\Methods = ?TextEditor_VTable
    
    CreateTextBuffer( @*Editor\Buffer )
    CreateTextEditor( @*Editor\Editor, @*Editor\Buffer )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TextEditor_GetType( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    ProcedureReturn "TextEditor"

  EndProcedure
  
  ;............................................................................
  
  Procedure.q TextEditor_GetCommands( *Editor.TextEditorShell, *OutCommands.Command )
    
    DebugAssert( *Editor <> #Null )
    
    PokeQ( *OutCommands, ?TextEditor_Commands )
    ProcedureReturn 9
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TextEditor_ExecuteCommand( *Editor.TextEditorShell, CommandId.i, Arguments.s )
    
    DebugAssert( *Editor <> #Null )
    
    Select CommandId
        
      Case #TextEditorEnterInsertMode
        *Editor\Mode = #InsertMode
        
      Case #TextEditorExitInsertMode
        *Editor\Mode = #NormalMode
        
      Case #TextEditorMoveLeft
        MoveCursorInTextEditor( @*Editor\Editor, -1, 0 )
        
      Case #TextEditorMoveRight
        MoveCursorInTextEditor( @*Editor\Editor, 1, 0 )
        
      Case #TextEditorMoveUp
        MoveCursorInTextEditor( @*Editor\Editor, 0, -1 )
        
      Case #TextEditorMoveDown
        MoveCursorInTextEditor( @*Editor\Editor, 0, 1 )
        
      Case #TextEditorMoveNextLine
        MoveCursorInTextEditor( @*Editor\Editor, 0, -1 )
        Define.q Column = GetCursorColumnNumberFromTextEditor( @*Editor\Editor )
        If Column <> 1
          MoveCursorInTextEditor( @*Editor\Editor, - ( Column - 1 ), 0 )
        EndIf
        
      Case #TextEditorDeleteLeft
        DeleteCharactersInTextEditor( @*Editor\Editor, -1 )
        
      Case #TextEditorInsertLineBreak
        InsertCharacterIntoTextEditor( @*Editor\Editor, #LF )
        
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
      CommandData( #TextEditorEnterInsertMode, "insert", #NormalMode, "i" )
      CommandData( #TextEditorExitInsertMode, "exit", #InsertMode, "<ESC>" )
      CommandData( #TextEditorMoveLeft, "move_left", #NormalMode, "h" )
      CommandData( #TextEditorMoveRight, "move_right", #NormalMode, "l" )
      CommandData( #TextEditorMoveUp, "move_up", #NormalMode, "k" )
      CommandData( #TextEditorMoveDown, "move_down", #NormalMode, "j" )
      CommandData( #TextEditorMoveNextLine, "move_next_line", #NormalMode, "<CR>" )
      CommandData( #TextEditorDeleteLeft, "delete_left", #InsertMode, "<BS>" )
      CommandData( #TextEditorInsertLineBreak, "insert_line_break", #InsertMode, "<CR>" )
    
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
  Assert( *TextEditor\Mode = #NormalMode )
  Assert( GetCurrentEditor( @Shell ) = *Editor )
  
  ; Insert text.
  SendShellInput( @Shell, "itext" )
  
  Assert( *TextEditor\Mode = #InsertMode )
  Assert( GetTextBufferLength( *TextEditor\Buffer ) = 4 )
  Assert( GetTextBufferLineCount( *TextEditor\Buffer ) = 1 )
  Assert( ReadStringFromTextBuffer( *TextEditor\Buffer ) = "text" )
  
  ; Delete text.
  SendShellInput( @Shell, "<BS><BS>" )
  
  Assert( *TextEditor\Mode = #InsertMode )
  Assert( GetTextBufferLength( *TextEditor\Buffer ) = 2 )
  Assert( GetTextBufferLineCount( *TextEditor\Buffer ) = 1 )
  Assert( ReadStringFromTextBuffer( *TextEditor\Buffer ) = "te" )
  
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

;..............................................................................

ProcedureUnit CanConnectTextBlobToTextEditorShell()

  UseModule Shell
  UseModule TextEditorShell
  UseModule TextBuffer
  UseModule TextEditor
  UseModule Workspace
  UseModule Root
  
  ;how is the editor connected to a workspace?
  
  ;command to load text
  ; - parameter for blob name or id
  ; - optional parameter for workspace name or id (if not supplied, uses current workspace)
  
  ;remember: saving is automatic! (no manual :w necessary as in Vim)
  
  Define.Shell Shell
  CreateShell( @Shell )
  Define.IEditor *Editor = CreateEditor( @Shell, SizeOf( TextEditorShell ), @CreateTextEditorShell() )
  Define.TextEditorShell *TextEditor = *Editor
  
  ExecuteShellCommand( @Shell, "edit", "testblob" )
  
  ;create text blob and connect it to text editor shell
  ;put text into shell (saving should happen automatically)
  ;modify the text

EndProcedureUnit

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 215
; FirstLine = 176
; Folding = --
; EnableXP