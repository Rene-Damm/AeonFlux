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

; add way to repeat the same command (sequence) N times

;;REVIEW: can modes be parameterized?
;;REVIEW: does undo need to be tied together here at this level?
;;REVIEW: have a way to make commands contextual? (like <...> in Vim)
;;REVIEW: have a special "global" mode that is always active in addition to the specific current mode?

;;TODO: add repeat counts for command executions

;;TODO: macro recording and playback

; Dispatches commands to a dynamic set of editors.
; The shell has no visual aspect; it is purely a way of mutating discrete chunks of state through commands.
; All edit operations going through the shell can be represented as text strings.
DeclareModule Shell
  
  #GLOBAL_MODE = "global"
  
  ;............................................................................
  
  Macro EditorData( Name )
    Name#_VTable:
      Data.q @Name#_GetType()
      Data.q @Name#_GetMode()
      Data.q @Name#_GetCommands()
      Data.q @Name#_ExecuteCommand()
      Data.q @Name#_SendInput()
  EndMacro

  Macro CommandData( Id, Name, Mode, Binding, ParameterCount )
    Data.i Id
    Data.q @Name
    Data.q @Mode
    Data.q @Binding
    Data.i ParameterCount
  EndMacro
  
  Macro CommandParameterData( Name, Type )
    Data.s Name
    Data.i Type
  EndMacro
    
  ;............................................................................
  
  Structure ParameterValue
    Type.i
    String.s
    StructureUnion
      Quad.q
      Double.d
    EndStructureUnion
  EndStructure
  
  Structure CommandParameter
    Name.s
    Type.i
  EndStructure
  
  Structure Command
    Id.i
    Name.s
    Mode.s
    Binding.s 
    ParameterCount.i
    ; Followed by variable-size array of CommandParameter instances.
  EndStructure
  
  Structure Binding
    Input.s                 ; Input mapped to different input.
    *Command.Command        ; Input mapped to command.
    Map Bindings.Binding()  ; For stringed bindings such as "<C-M>i
  EndStructure
    
  ;;REVIEW: how is a visual aspect tied to this? at the workspace level?
  Interface IEditor
    GetType.s()
    GetMode.s()
    GetCommands.i( *OutCommands.Command )
    ExecuteCommand( Id.i, Map Parameters.ParameterValue() )
    SendInput( Input.s ) ;;TODO: send as raw UTF-8 buffer rather than string
    ;SaveState.s() ;raw memory buffer instead?
    ;LoadState( State.s )
  EndInterface
  
  Structure ModeRecord
    Name.s
    Map Commands.q()
    Map Bindings.Binding()
  EndStructure
    
  Structure EditorRecord
    *Editor.IEditor
    *EditorType.EditorType
    *CurrentMode.ModeRecord
  EndStructure
  
  Structure EditorType
    Name.i
    Map Modes.ModeRecord()
  EndStructure
  
  Structure Shell
    NumEditors.i
    CurrentEditor.i
    *Editors.EditorRecord
    Map EditorTypes.EditorType()
    Array Inputs.s( 1 )
    
    ;map of macros
  EndStructure
  
  Prototype CreateEditorFn( *Ptr )
  
  ;............................................................................

  Declare   CreateShell( *Shell.Shell )
  Declare   DestroyShell( *Shell.Shell )
  Declare.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
  Declare   DestroyEditor( *Shell.Shell, Editor.IEditor )
  Declare.q GetCurrentEditor( *Shell.Shell )
  Declare.q GetEditorFromShell( *Shell.Shell, Index.i )
  Declare   ExecuteShellCommand( *Shell.Shell, Command.s )
  Declare.i ListShellCommands( *Shell.Shell, Array Commands.s( 1 ), StartingWith.s = "" )
  Declare   SendShellInput( *Shell.Shell, Input.s )
  
  CompilerIf #False
  Declare   AddEditorToShell( *Shell.Shell, Editor.IEditor )
  Declare   RemoveEditorFromShell( *Shell.Shell, *Editor.IEditor )
  Declare   StartRecordingMacro( *Shell.Shell, Name.s )
  Declare   StopRecordingMacro( *Shell.Shell )
  CompilerEndIf

EndDeclareModule

Module Shell
  
  UseModule Utils
  
  Structure ShellInput
    *Command.Command
    Text.s
  EndStructure
    
  ;............................................................................
  
  Procedure CreateShell( *Shell.Shell )
    
    DebugAssert( *Shell <> #Null )
    
    InitializeStructure( *Shell, Shell )
    *Shell\CurrentEditor = -1
    *Shell\Editors = AllocateMemory( SizeOf( EditorRecord ) * 64 )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyShell( *Shell.Shell )
    
    DebugAssert( *Shell <> #Null )
    
    ; Destroy editors.
    While *Shell\NumEditors > 0
      Define.IEditor *Editor = GetEditorFromShell( *Shell, *Shell\NumEditors - 1 )
      DestroyEditor( *Shell, *Editor )
    Wend
    
    FreeMemory( *Shell\Editors )
    ClearStructure( *Shell, Shell )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( SizeInBytes > 0 )
    DebugAssert( *CreateFn <> #Null )
    
    ; Allocate.
    Define.IEditor *Editor = AllocateMemory( SizeInBytes )
    *CreateFn( *Editor )
    
    ; Read out commands and modes, if we haven't already.
    Define.s EditorTypeName = *Editor\GetType()
    Define.EditorType *EditorType = FindMapElement( *Shell\EditorTypes(), EditorTypeName )
    If *EditorType = #Null
      
      *EditorType = AddMapElement( *Shell\EditorTypes(), EditorTypeName )
      
      ; Retrieve command list from editor.
      Define.Command *Commands
      Define.i NumCommands = *Editor\GetCommands( @*Commands )
      
      ; Assemble mode map from list of commands.
      Define.Command *CurrentCommand = *Commands
      Dim Inputs.s( 10 )
      Define.i I
      For I = 0 To NumCommands - 1
        
        Define.s Mode = *CurrentCommand\Mode
        DebugAssert( Len( Mode ) > 0 )
        
        ; Get or add record for mode.
        Define.ModeRecord *ModeRecord = FindMapElement( *EditorType\Modes(), Mode )
        If *ModeRecord = #Null
          *ModeRecord = AddMapElement( *EditorType\Modes(), Mode )
          *ModeRecord\Name = Mode
        EndIf
        
        ; Add command.
        Define *Ptr = AddMapElement( *ModeRecord\Commands(), *CurrentCommand\Name )
        PokeQ( *Ptr, *CurrentCommand )
        
        ; Add binding, if any.
        Define.s Binding = *CurrentCommand\Binding
        If Len( Binding ) > 0
          
          ;;TODO: support stringed inputs such as <C-M>i
          
          If FindMapElement( *ModeRecord\Bindings(), Binding ) <> #Null
            ;;TODO: error
          EndIf
          
          Define.Binding *Element = AddMapElement( *ModeRecord\Bindings(), Binding )
          *Element\Command = *CurrentCommand
          
        EndIf
        
        *CurrentCommand + SizeOf( Command )
        
      Next
      
    EndIf
    
    ; Find command map for current mode.
    Define.s Mode = *Editor\GetMode()
    DebugAssert( Len( Mode ) > 0 )
    Define.ModeRecord *CurrentMode = FindMapElement( *EditorType\Modes(), Mode )
    DebugAssert( *CurrentMode <> #Null )
    
    ; Add record.
    Define.EditorRecord Record
    Record\Editor = *Editor
    Record\EditorType = *EditorType
    Record\CurrentMode = *CurrentMode
    Define.i Index = ArrayAppendWithCapacity( @*Shell\Editors, @*Shell\NumEditors, @Record, SizeOf( EditorRecord ) )
    
    ;;TODO: properly "make current"
    ; Make current.
    *Shell\CurrentEditor = Index
    
    ProcedureReturn *Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyEditor( *Shell.Shell, *Editor.IEditor )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( *Editor <> #Null )
    DebugAssert( *Shell\NumEditors > 0 )
    
    Define.q Index = 0
    Define.EditorRecord *Record = *Shell\Editors
    For Index = 0 To *Shell\NumEditors - 1
      If *Record\Editor = *Editor
        Break
      EndIf
      *Record + SizeOf( EditorRecord )
    Next
    
    ;;TODO: must give editor a shot at cleanup (so it can free whatever memory it has itself allocated)
    
    FreeMemory( *Editor )
    ArrayEraseAtWithCapacity( @*Shell\Editors, @*Shell\NumEditors, Index, SizeOf( EditorRecord ) )
    
    If *Shell\CurrentEditor = Index
      ;;TODO: switch to one of the other still existing editors (if any)
      *Shell\CurrentEditor = -1
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetCurrentEditor( *Shell.Shell )
    
    DebugAssert( *Shell <> #Null )
    
    If *Shell\CurrentEditor < 0
      ProcedureReturn #Null
    EndIf
    
    Define.EditorRecord *Record = *Shell\Editors + *Shell\CurrentEditor * SizeOf( EditorRecord )
    ProcedureReturn *Record\Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetEditorFromShell( *Shell.Shell, Index.i )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( Index >= 0 )
    
    If Index >= *Shell\NumEditors
      ProcedureReturn #Null
    EndIf
    
    Define.EditorRecord *Record = *Shell\Editors + Index * SizeOf( EditorRecord )
    ProcedureReturn *Record\Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure InternalExecuteShellCommand( *Editor.EditorRecord, *Command.Command )
    
    ;;TODO: get rid of temporary map allocation
    ; Execute.
    NewMap Parameters.ParameterValue()
    *Editor\Editor\ExecuteCommand( *Command\Id, Parameters.ParameterValue() )
    FreeMap( Parameters() )
    
    ; Check for mode switch.
    Define.s Mode = *Editor\Editor\GetMode()
    If Mode <> *Editor\CurrentMode\Name
      
      DebugAssert( Len( Mode ) > 0 )
      
      Define.ModeRecord *NewMode = FindMapElement( *Editor\EditorType\Modes(), Mode )
      
      DebugAssert( *NewMode <> #Null )
      *Editor\CurrentMode = *NewMode
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure ExecuteShellCommand( *Shell.Shell, Command.s )
    
    DebugAssert( *Shell <> #Null )
    
    If *Shell\CurrentEditor < 0
      ProcedureReturn
    EndIf
    
    Define.EditorRecord *Editor = *Shell\Editors + *Shell\CurrentEditor * SizeOf( EditorRecord )
    
    ; Look up command.
    Define *CommandPtrPtr = FindMapElement( *Editor\CurrentMode\Commands(), Command )
    If *CommandPtrPtr = #Null
      ;;TODO: diagnose error
      ProcedureReturn
    EndIf
    Define.Command *Command = PeekQ( *CommandPtrPtr )
   
    ; Execute.
    InternalExecuteShellCommand( *Editor, *Command )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.i ListShellCommands( *Shell.Shell, Array Commands.s( 1 ), StartingWith.s = "" ) ;;REVIEW: shouldn't this return command pointers rather than just names?
    
    DebugAssert( *Shell <> #Null )
    
    If *Shell\CurrentEditor < 0
      ProcedureReturn 0
    EndIf
    
    Define.i CommandCount = 0
    Define.i CommandsArraySize = ArraySize( Commands() )
    Define.i HaveStartingWith = #False
    If Len( StartingWith ) > 0
      HaveStartingWith = #True
    EndIf
    
    Define.EditorRecord *EditorRecord = *Shell\Editors + *Shell\CurrentEditor * SizeOf( EditorRecord )
    DebugAssert( *EditorRecord\CurrentMode <> #Null )
    
    ForEach *EditorRecord\CurrentMode\Commands()
      
      Define.Command *Command = *EditorRecord\CurrentMode\Commands()
      If HaveStartingWith And Not StartsWith( StartingWith, *Command\Name )
        Continue
      EndIf
      
      ; Enlarge array if necessary.
      If CommandCount = CommandsArraySize
        CommandsArraySize + 32
        ReDim Commands.s( CommandsArraySize )
      EndIf
      
      ; Add command.
      Commands( CommandCount ) = *Command\Name
      CommandCount + 1
      
    Next
    
    ProcedureReturn CommandCount
    
  EndProcedure
  
  ;............................................................................
  
  Procedure SendShellInput( *Shell.Shell, Input.s )
    
    DebugAssert( *Shell <> #Null )
    
    If *Shell\CurrentEditor < 0
      ProcedureReturn
    EndIf
    
    Define.EditorRecord *Editor = *Shell\Editors + *Shell\CurrentEditor * SizeOf( EditorRecord )
    Define.ModeRecord *Mode = *Editor\CurrentMode
    DebugAssert( *Mode <> #Null )
    
    Define *Ptr = @Input
    Define *StartPtr = *Ptr
    Define.i Length = Len( Input )
    Define.s Buffer
    Define.i BufferLength = 0
    Define.i BufferCapacity = 0
    Define.i InAngleBrackets = #False
    
    ; Parse input.
    While #True
      
      Define.c Char = PeekC( *Ptr )
      Define.Command *Command = #Null
      
      ;;TODO: deal with \< and \\
      
      If Char = '<'
        
        ; Start of angle bracket sequence.
        InAngleBrackets = #True
        ; If we have unflushed input, cause it to be flushed.
        If *StartPtr <> *Ptr
          Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, ( *Ptr - *StartPtr ) / SizeOf( Character ) )
        EndIf
        *StartPtr = *Ptr ; Include '<'
        
      ElseIf Char = '>' And InAngleBrackets
        
        ; End of angle bracket sequence.
        InAngleBrackets = #False
        Define.i SequenceLength = *Ptr - *StartPtr + 2 ; Include '>'
        If SequenceLength > 0
          
          Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, SequenceLength / SizeOf( Character ) )
          Define.Binding *Binding = FindMapElement( *Mode\Bindings(), Buffer )
          BufferLength = 0
          
          ;;TODO: support stringed input (such as <C-m>i)
          If *Binding <> #Null
            If MapSize( *Binding\Bindings() ) > 0
              NotImplemented( "stringed bindings" )
            EndIf
            *Command = *Binding\Command
          EndIf
          
          *StartPtr = *Ptr + SizeOf( Character )
          
        EndIf
        
      ElseIf Char = #NUL
        
        ; Flush input.
        If *StartPtr <> *Ptr
          Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, ( *Ptr - *StartPtr ) / SizeOf( Character ) )
        EndIf
        
      Else
        
        ; Check if current character is mapped to a command.
        Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *Ptr, 1 )
        Define.Binding *Binding = FindMapElement( *Mode\Bindings(), Buffer )
        BufferLength = 0
        
        If *Binding <> #Null
          ; Yes, it is. Flush prior input (if any) and execute command.
          If *StartPtr <> *Ptr
            Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, ( *Ptr - *StartPtr ) / SizeOf( Character ) )
          EndIf
          *StartPtr = *Ptr + SizeOf( Character )
          *Command = *Binding\Command
        EndIf
        
      EndIf
      
      ; Send input.
      If BufferLength > 0
        *Editor\Editor\SendInput( Buffer )
        BufferLength = 0
      EndIf
      
      ; Execute command.
      If *Command <> #Null
        InternalExecuteShellCommand( *Editor, *Command )
        *Mode = *Editor\CurrentMode ; May have changed.
      EndIf
        
      If Char = #NUL
        Break
      EndIf
     
      *Ptr + SizeOf( Character )
      
    Wend
    
    If InAngleBrackets
      ;;TODO: error
    EndIf
    
  EndProcedure
  
EndModule

;..............................................................................

DeclareModule TestEditor
  
  UseModule Shell
  
  ;............................................................................
  
  Enumeration TestEditorCommands
    #TestEditorCommandNone
    #TestEditorCommand1
    #TestEditorCommand2
    #TestEditorSwitchToMode1
    #TestEditorSwitchToDefaultMode
  EndEnumeration
  
  Structure RecordedCommand
    Id.i
    Map Parameters.ParameterValue()
  EndStructure
  
  Structure TestEditor
    *Methods
    CurrentMode.s
    List RecordedCommands.RecordedCommand()
    List RecordedInputs.s()
  EndStructure
  
  ;............................................................................
  
  Declare CreateTestEditor( *Editor.TestEditor )
  
EndDeclareModule

Module TestEditor
  
  UseModule Shell
  UseModule Utils

  ;............................................................................
  
  Procedure CreateTestEditor( *Editor.TestEditor )
    
    InitializeStructure( *Editor, TestEditor )
    *Editor\Methods = ?TestEditor_VTable
    *Editor\CurrentMode = "default"
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TestEditor_GetType( *Editor.TestEditor )
    ProcedureReturn "TestEditor"
  EndProcedure
  
  ;............................................................................

  Procedure.s TestEditor_GetMode( *Editor.TestEditor )
    DebugAssert( *Editor <> #Null )
    ProcedureReturn *Editor\CurrentMode
  EndProcedure
  
  ;............................................................................

  Procedure.i TestEditor_GetCommands( *Editor.TestEditor, *OutCommands.Command )
    DebugAssert( *OutCommands <> #Null )
    PokeQ( *OutCommands, ?TestEditor_Commands )
    ProcedureReturn 4
  EndProcedure
  
  ;............................................................................
  
  Procedure TestEditor_ExecuteCommand( *Editor.TestEditor, CommandId.i, Map Parameters.ParameterValue() )
    
    DebugAssert( *Editor <> #Null )
    
    Define.RecordedCommand *Record = AddElement( *Editor\RecordedCommands() )
    *Record\Id = CommandId
    CopyMap( Parameters(), *Record\Parameters() )
    
    Select CommandId
        
      Case #TestEditorSwitchToDefaultMode:
        *Editor\CurrentMode = "default"
        
      Case #TestEditorSwitchToMode1:
        *Editor\CurrentMode = "mode1"
        
    EndSelect
    
  EndProcedure
  
  ;............................................................................
  
  Procedure TestEditor_SendInput( *Editor.TestEditor, Input.s )
    DebugAssert( *Editor <> #Null )
    AddElement( *Editor\RecordedInputs() )
    *Editor\RecordedInputs() = Input
  EndProcedure
 
  ;............................................................................
  
  DataSection
    
    EditorData( TestEditor )
      
    TestEditor_Commands:
      CommandData( #TestEditorCommand1, "command1", "default", "<ESC>", 0 )
      CommandData( #TestEditorCommand2, "othercommand", "default", "i", 0 )
      CommandData( #TestEditorSwitchToMode1, "switchtomode1", "default", "p", 0 )
      CommandData( #TestEditorSwitchToDefaultMode, "switchtodefaultmode", "mode1", "<ESC>", 0 )
      
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
  Assert( GetCurrentEditor( @Shell ) = #Null )
  
  Define.IEditor *Editor1 = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  Assert( Shell\NumEditors = 1 )
  Assert( GetCurrentEditor ( @Shell ) = *Editor1 )
  Assert( *Editor1\GetType() = "TestEditor" )
  Assert( *Editor1\GetMode() = "default" )
  
  Define.IEditor *Editor2 = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  Assert( Shell\NumEditors = 2 )
  Assert( GetCurrentEditor( @Shell ) = *Editor2 )
  Assert( *Editor2\GetType() = "TestEditor" )
  Assert( *Editor2\GetMode() = "default" )
  
  ;;TODO: destroy *Editor2 and make sure *Editor1 becomes current
  
  DestroyShell( @Shell )
  
  Assert( Shell\NumEditors = 0 )
  Assert( Shell\Editors = #Null )

EndProcedureUnit

;..............................................................................

ProcedureUnit CanExecuteShellCommands()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  Define.TestEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  ExecuteShellCommand( @Shell, "command1" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  
  Define.RecordedCommand *Command = FirstElement( *Editor\RecordedCommands() )
  Assert( *Command\Id = #TestEditorCommand1 )
  
  DestroyShell( @Shell )

EndProcedureUnit

;;TODO: CanExecuteShellCommandsWithParameters
;;TODO: CanSwitchEditorModes

;..............................................................................

ProcedureUnit CanListAvailableCommands()

  UseModule Shell
  UseModule TestEditor
  UseModule Utils
  
  Define.Shell Shell
  CreateShell( @Shell )
  Define.IEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  Dim Commands.s( 0 )
  
  Define.i CommandCount = ListShellCommands( @Shell, Commands() )
  
  Assert( ArraySize( Commands() ) > 0 )
  Assert( CommandCount = 3 )
  Assert( FindStringInArray( Commands(), "command1" ) <> -1 )
  Assert( FindStringInArray( Commands(), "othercommand" ) <> -1 )
  Assert( FindStringInArray( Commands(), "switchtomode1" ) <> -1 )
  
  CommandCount = ListShellCommands( @Shell, Commands(), "com" )
  
  Assert( ArraySize( Commands() ) > 0 )
  Assert( CommandCount = 1 )
  Assert( Commands( 0 ) = "command1" )
  
  FreeArray( Commands() )
  DestroyShell( @Shell )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanSendInputToShell()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  Define.TestEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  SendShellInput( @Shell, "<ESC>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Define.RecordedCommand *Command = FirstElement( *Editor\RecordedCommands() )
  Assert( *Command\Id = #TestEditorCommand1 )
  
  DestroyShell( @Shell )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanSendMixedCommmandAndTextInputToShell()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  Define.TestEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  SendShellInput( @Shell, "itest" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Assert( ListSize( *Editor\RecordedInputs() ) = 1 )
  Assert( *Editor\RecordedCommands()\Id = #TestEditorCommand2 )
  Assert( MapSize( *Editor\RecordedCommands()\Parameters() ) = 0 )
  Assert( *Editor\RecordedInputs() = "test" )
  
  DestroyShell( @Shell )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanSwitchEditorModesInShell()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  Define.TestEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  SendShellInput( @Shell, "p" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Assert( *Editor\RecordedCommands()\Id = #TestEditorSwitchToMode1 )
  Assert( *Editor\CurrentMode = "mode1" )
  
  ClearList( *Editor\RecordedCommands() )
  
  SendShellInput( @Shell, "<ESC>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Assert( *Editor\RecordedCommands()\Id = #TestEditorSwitchToDefaultMode )
  Assert( *Editor\CurrentMode = "default" )
  
  DestroyShell( @Shell )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 30
; Folding = ----
; EnableXP