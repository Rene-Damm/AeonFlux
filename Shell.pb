EnableExplicit

XIncludeFile "Utils.pb"

;per-mode command maps
;unmapped input is sent as text characters
;for control keys, unmapped input is sent as key codes
;each editor has a set of modes and an input mapping for  each

; or just hardcode everything?

; should the shell itself have its own commands? how are editors opened? should this be outside the domain of the shell?

; can one switch between editors through commands? (yes: each editor has some way of identification)

; for commands with parameters, interactive mode that puts together a requester to fill in parameter values

;;REVIEW: can modes be parameterized?
;;REVIEW: does undo need to be tied together here at this level?


; Dispatches commands to a dynamic set of editors.
; The shell has no visual aspect; it is purely a way of mutating discrete chunks of state through commands.
DeclareModule Shell
    
  ;............................................................................
  
  Structure CommandParameter
    Name.s
    Type.i
  EndStructure
  
  Structure Command
    Id.i
    Name.s
    Mode.s
    Binding.s
  EndStructure
  
  ;;REVIEW: why not just a common base structure Editor that has all the method pointers?
  
  ;;REVIEW: how is a visual aspect tied to this? at the workspace level?
  Interface IEditor
    GetType.s()
    GetMode.s()
    GetCommands( Array Commands.Command( 1 ) )
    ExecuteCommand( Id.i, Map Parameters() )
    ;SaveState.s() ;raw memory buffer instead?
    ;LoadState( State.s )
  EndInterface
  
  Structure Shell
    NumEditors.i
    *Editors.IEditor
    *CurrentEditor.IEditor
    
    ;map of macros
  EndStructure
  
  Prototype CreateEditorFn( *Ptr )
  
  ;............................................................................

  Declare   CreateShell( *Shell.Shell )
  Declare   DestroyShell( *Shell.Shell )
  Declare.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
  Declare   DestroyEditor( *Shell.Shell, Editor.IEditor )
  
  CompilerIf #False
  ;Declare   RegisterEditorType( Type.s, Size.i, *CreateFn.CreateEditorFn )
  Declare   ExecuteShellCommand( *Shell.Shell, Command.s )
  Declare   SendShellInput( *Shell.Shell, Input.s )
  Declare.i ListShellCommands( *Shell.Shell, Array Commands.s )
  Declare   AddEditorToShell( *Shell.Shell, Editor.IEditor )
  Declare   RemoveEditorFromShell( *Shell.Shell, *Editor.IEditor )
  Declare   StartRecordingMacro( *Shell.Shell, Name.s )
  Declare   StopRecordingMacro( *Shell.Shell )
  CompilerEndIf

EndDeclareModule

Module Shell
  
  UseModule Utils
  
  ;............................................................................
  
  Procedure CreateShell( *Shell.Shell )
    
    DebugAssert( *Shell <> #Null )
    
    InitializeStructure( *Shell, Shell )
    *Shell\CurrentEditor = -1
    *Shell\Editors = AllocateMemory( SizeOf( Quad ) * 64 )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyShell( *Shell.Shell )
    
    DebugAssert( *Shell <> #Null )
    
    ; Destroy editors.
    Define.i I
    For I = *Shell\NumEditors - 1 To 0 Step -1
      DestroyEditor( *Shell, PeekQ( *Shell\Editors + I ) )
    Next
    
    FreeMemory( *Shell\Editors )
    ClearStructure( *Shell, Shell )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( SizeInBytes > 0 )
    DebugAssert( SizeInBytes % 4 = 0 )
    DebugAssert( CreateFn <> #Null )
    
    Define.IEditor *Editor = AllocateMemory( SizeInBytes )
    *CreateFn( *Editor )
    
    ArrayAppendWithCapacity( @*Shell\Editors, @*Shell\NumEditors, @*Editor, SizeOf( Quad ) )
    
    ;;TODO: properly "make current"
    *Shell\CurrentEditor = *Editor
    
    ProcedureReturn *Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyEditor( *Shell.Shell, *Editor.IEditor )
    
    DebugAssert( *Shell <> #Null )
    
  EndProcedure
  
EndModule

;..............................................................................

DeclareModule TestEditor
  
  UseModule Shell
  
  ;............................................................................
  
  Structure TestEditor
    *Methods
  EndStructure
  
  ;............................................................................
  
  Declare CreateTestEditor( *Editor.TestEditor )
  
EndDeclareModule

Module TestEditor
  
  UseModule Shell
  
  ;............................................................................
  
  Procedure CreateTestEditor( *Editor.TestEditor )
    
    InitializeStructure( *Editor, TestEditor )
    *Editor\Methods = ?TestEditor_VTable
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TestEditor_GetType( *Editor.TestEditor )
    ProcedureReturn "TestEditor"
  EndProcedure
  
  ;............................................................................
  
  DataSection
    ; IEditor.
    TestEditor_VTable:
      Data.q @TestEditor_GetType()
  EndDataSection
  
EndModule

;..............................................................................

ProcedureUnit CanCreateShellWithMultipleEditors()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  
  Assert( Shell\NumEditors = 0 )
  Assert( Shell\CurrentEditor = -1 )
  
  Define.IEditor *Editor1 = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  Assert( Shell\NumEditors = 1 )
  Assert( Shell\CurrentEditor = *Editor1 )
  Assert( *Editor1\GetType() = "TestEditor" )
  
  DestroyShell( @Shell )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 159
; FirstLine = 131
; Folding = --
; EnableXP