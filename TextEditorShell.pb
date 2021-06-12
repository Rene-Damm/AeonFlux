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
    Flags.i
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
  UseModule Workspace
  UseModule Root
  
  ;............................................................................
  
  EnumerationBinary TextEditorFlags
    #TextBufferDirty
  EndEnumeration
  
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
    #TextEditorEditBlob
    
  EndEnumeration
  
  ;............................................................................
  
  Procedure FlushTextBufferInternal( *Editor.TextEditorShell )
    
    If *Editor\Workspace = #Null
      ProcedureReturn
    EndIf
    
    DebugAssert( *Editor\TextBlob <> #Null )
    
    If Not ( *Editor\Flags & #TextBufferDirty )
      ProcedureReturn
    EndIf
    
    WriteTextBufferToFile( @*Editor\Buffer, *Editor\Workspace\Files, *Editor\TextBlob\FileHandle )
    *Editor\Flags & ~#TextBufferDirty
    
  EndProcedure
  
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
    ProcedureReturn 10
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TextEditor_ExecuteCommand( *Editor.TextEditorShell, CommandId.i, Arguments.s )
    
    DebugAssert( *Editor <> #Null )
    
    Select CommandId
        
      Case #TextEditorEnterInsertMode
        *Editor\Mode = #InsertMode
        
      Case #TextEditorExitInsertMode
        *Editor\Mode = #NormalMode
        FlushTextBufferInternal( *Editor )
        
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
        *Editor\Flags | #TextBufferDirty
        
      Case #TextEditorInsertLineBreak
        InsertCharacterIntoTextEditor( @*Editor\Editor, #LF )
        *Editor\Flags | #TextBufferDirty
        
      Case #TextEditorEditBlob
        Define.s BlobNameOrId = StringField( Arguments, 1, ~" \t" )
        Define.s WorkspaceNameOrId = StringField( Arguments, 2, ~" \t" )
        Define.Workspace *Workspace = Root\CurrentWorkspace
        If WorkspaceNameOrId <> ""
          NotImplemented( "Edit blob in workspace other than current" )
        EndIf
        Define *Blob = FindBlob( *Workspace, BlobNameOrId )
        ;;REVIEW: what should we do if the blob type isn't text? hex editing?
        If *Blob = #Null
          *Blob = CreateBlob( *Workspace, #TextBlob, BlobNameOrId )
        EndIf
        ;;TODO: load text from blob into buffer
        *Editor\TextBlob = *Blob
        *Editor\Workspace = *Workspace
        *Editor\Flags & ~#TextBufferDirty
        
    EndSelect
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TextEditor_SendInput( *Editor.TextEditorShell, Input.s )
    
    DebugAssert( *Editor <> #Null )
    
    InsertStringIntoTextEditor( @*Editor\Editor, Input )
    *Editor\Flags | #TextBufferDirty
    
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
      CommandData( #TextEditorEditBlob, "edit", #NormalMode, "" )
    
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
  UseModule Utils
  
  ResetStructure( @Root, Root )
  
  Define.IFileSystem *RootSystem = CreateVirtualFileSystem()
  Define.IDirectorySystem *WorkspaceLocation = CreateVirtualDirectorySystem()
  Define.IDirectorySystem *ProjectLocation = CreateVirtualDirectorySystem()
  
  LoadRoot( "Default", *RootSystem, *WorkspaceLocation, *ProjectLocation )
  
  Define.Workspace *Workspace = Root\CurrentWorkspace
  
  ; Remember: saving is automatic! (no manual :w necessary as in Vim)
  
  Define.Shell Shell
  CreateShell( @Shell )
  Define.IEditor *Editor = CreateEditor( @Shell, SizeOf( TextEditorShell ), @CreateTextEditorShell() )
  Define.TextEditorShell *TextEditor = *Editor
  
  SendShellInput( @Shell, ":edit testblob<RET>" )
  
  Assert( GetBlobCount( *Workspace ) = 1 )
  Define.Blob *TestBlob = FindBlob( *Workspace, "testblob" )
  Assert( *TestBlob <> #Null )
  Assert( *TestBlob\Name = "testblob" )
  Assert( *TestBlob\BlobType = #TextBlob )
  Assert( *Workspace\Files\FileExists( *TestBlob\Id + ".blob" ) )
  Assert( ReadTextFile( *Workspace\Files, *TestBlob\Id + ".blob" ) = "" )
  Assert( ReadStringFromTextBuffer( *TextEditor\Buffer ) = "" )
  Assert( *TextEditor\TextBlob = *TestBlob )
  Assert( *TextEditor\Workspace = *Workspace )
  
  SendShellInput( @Shell, "itest" )
  
  ; Should save only when we exit insert mode.
  Assert( ReadTextFile( *Workspace\Files, *TestBlob\Id + ".blob" ) = "" )
  Assert( ReadStringFromTextBuffer( *TextEditor\Buffer ) = "test" )
  
  SendShellInput( @Shell, "<ESC>" )
  
  Assert( ReadTextFile( *Workspace\Files, *TestBlob\Id + ".blob" ) = "test" )
  Assert( ReadStringFromTextBuffer( *TextEditor\Buffer ) = "test" )
  
  ;;TODO: modify the text and check it gets saved
  
  DestroyShell( @Shell )

EndProcedureUnit

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 366
; FirstLine = 312
; Folding = --
; EnableXP