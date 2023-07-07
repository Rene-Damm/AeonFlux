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

;;REVIEW: how does this handle things like "yiw"? i.e. all the ways that Vim commands compose

;;TODO: add repeat counts for command executions

;;TODO: macro recording and playback

; Dispatches commands to a dynamic set of editors.
; The shell has no visual aspect; it is purely a way of mutating discrete chunks of state through commands.
; All edit operations going through the shell can be represented as text strings.
DeclareModule Shell
  
  ;............................................................................
  
  ;;TODO: ':' shortcut that opens command mode where input is recorded in command buffer and then executes command on enter
  ;;TODO: Vim-style short command names (e.g. "e" instead of "edit")
  
  Macro EditorData( Name )
    Name#_VTable:
      Data.i @Name#_GetType()
      Data.i @Name#_GetCommands()
      Data.i @Name#_ExecuteCommand()
      Data.i @Name#_SendInput()
  EndMacro

  Macro CommandData( Id, Name, Mode, Binding )
    Data.i Id
    Data.i Mode
    Data.i @Name
    Data.i @Binding
  EndMacro
    
  ;............................................................................
  
  Enumeration EditorCommands
    
    #EditorEnterCommandMode
    #EditorExitCommandMode
    #EditorAbortCommandMode
    
  EndEnumeration
  
  #EDITOR_COMMON_COMMANDS_COUNT = 3
  
  Enumeration EditorMode
    #NormalMode
    #CommandMode
  EndEnumeration
  
  Structure Command
    Id.i
    Mode.i
    Name.s
    Binding.s
    ;;TODO: Help.s
  EndStructure
  
  Structure Binding
    Input.s                 ; Input mapped to different input.
    *Command.Command        ; Input mapped to command.
    Map Bindings.Binding()  ; For stringed bindings such as "<C-M>i
  EndStructure
  
  Structure ModeRecord
    Map Commands.q()
    Map Bindings.Binding()
  EndStructure
    
  ;;REVIEW: how is a visual aspect tied to this? at the workspace level?
  Interface IEditor
    GetType.s()
    GetCommands.i( *OutCommands.Command )
    ExecuteCommand( Id.i, Arguments.s )
    SendInput( Input.s ) ;;TODO: send as raw UTF-8 buffer rather than string
    ;SaveState.s() ;raw memory buffer instead?
    ;LoadState( State.s )
  EndInterface
  
  Structure Editor
    *Methods
    *Type.EditorType
    Mode.i
    CommandInput.s
    CommandMode.i
  EndStructure
  
  Structure EditorType
    Name.i
    Array Modes.ModeRecord( 2 )
  EndStructure
  
  Structure Shell
    CurrentEditor.i
    NumEditors.q
    *Editors
    Map EditorTypes.EditorType()
    
    ;map of macros (or should those be collected in the editor type?)
  EndStructure
  
  Prototype CreateEditorFn( *Ptr )
  
  ;............................................................................

  Declare   CreateShell( *Shell.Shell )
  Declare   DestroyShell( *Shell.Shell )
  Declare.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
  Declare   DestroyEditor( *Shell.Shell, *Editor.IEditor )
  Declare.q GetCurrentEditor( *Shell.Shell )
  Declare.q GetEditorFromShell( *Shell.Shell, Index.i )
  Declare   ExecuteShellCommand( *Shell.Shell, Command.s, Arguments.s = "" )
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
    *Shell\Editors = AllocateMemory( SizeOf( Integer ) * 64 )
    
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
  
  Procedure AddEditorCommandsInternal( *Commands.Command, NumCommands.i, *EditorType.EditorType )
    
    ; Assemble mode map from list of commands.
    Define.Command *CurrentCommand = *Commands
    Define.i I
    For I = 0 To NumCommands - 1
      
      Define.i Mode = *CurrentCommand\Mode
      DebugAssert( Mode >= #NormalMode )
      
      ; Get or add record for mode.
      If ArraySize( *EditorType\Modes() ) <= Mode
        ReDim *EditorType\Modes( Mode + 1 )
      EndIf
      Define.ModeRecord *ModeRecord = @*EditorType\Modes( Mode )

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
      
  EndProcedure
  
  ;............................................................................
  
  Procedure.q CreateEditor( *Shell.Shell, SizeInBytes.i, *CreateFn.CreateEditorFn )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( SizeInBytes > 0 )
    DebugAssert( SizeInBytes > SizeOf( Editor ) )
    DebugAssert( *CreateFn <> #Null )
    
    ; Allocate.
    Define.Editor *Editor = AllocateMemory( SizeInBytes )
    *CreateFn( *Editor )
    
    ; Read out commands and modes, if we haven't already.
    Define.IEditor *EditorInterface = *Editor
    Define.s EditorTypeName = *EditorInterface\GetType()
    Define.EditorType *EditorType = FindMapElement( *Shell\EditorTypes(), EditorTypeName )
    If *EditorType = #Null
      
      *EditorType = AddMapElement( *Shell\EditorTypes(), EditorTypeName )
      
      ; Add common commands.
      AddEditorCommandsInternal( ?Editor_CommonCommands, #EDITOR_COMMON_COMMANDS_COUNT, *EditorType )
      
      ; Add commands specific to editor.
      Define.Command *Commands
      Define.i NumCommands = *EditorInterface\GetCommands( @*Commands )
      AddEditorCommandsInternal( *Commands, NumCommands, *EditorType )
      
    EndIf
    
    ; Add record.
    *Editor\Type = *EditorType
    *Editor\Mode = #NormalMode
    *Editor\CommandMode = #NormalMode
    Define.i Index = ArrayAppendWithCapacity( @*Shell\Editors, @*Shell\NumEditors, @*Editor, SizeOf( Integer ) )
    
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
    
    Define.i Index = ArrayIndexOf( *Shell\Editors, *Shell\NumEditors, @*Editor, SizeOf( Integer ) )
    DebugAssert( Index >= 0 )
    If Index < 0
      ProcedureReturn
    EndIf
    
    ;;TODO: must give editor a shot at cleanup (so it can free whatever memory it has itself allocated)
    
    FreeMemory( *Editor )
    ArrayEraseAtWithCapacity( @*Shell\Editors, @*Shell\NumEditors, Index, SizeOf( Integer ) )
    
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
    
    ProcedureReturn GetEditorFromShell( *Shell, *Shell\CurrentEditor )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetEditorFromShell( *Shell.Shell, Index.i )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( Index >= 0 )
    
    If Index >= *Shell\NumEditors
      ProcedureReturn #Null
    EndIf
    
    Define *Editor = PeekI( *Shell\Editors + Index * SizeOf( Integer ) )
    ProcedureReturn *Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q LookupShellCommandInternal( *Editor.Editor, Command.s )
    
    Define.ModeRecord *Mode = @*Editor\Type\Modes( *Editor\Mode )
    Define *CommandPtrPtr = FindMapElement( *Mode\Commands(), Command )
    If *CommandPtrPtr = #Null
      ; Try lower-case version.
      *CommandPtrPtr = FindMapElement( *Mode\Commands(), LCase( Command ) )
    EndIf
    If *CommandPtrPtr = #Null
      ;;TODO: diagnose error
      ProcedureReturn #Null
    EndIf
    Define.Command *Command = PeekI( *CommandPtrPtr )
    
    ProcedureReturn *Command
    
  EndProcedure
  
  ;............................................................................
  
  Procedure ExecuteShellCommandInternal( *Editor.Editor, *Command.Command, Arguments.s = "" )
    
    Define.i Id = *Command\Id
    If Id <= #EDITOR_COMMON_COMMANDS_COUNT
      
      ; Common commands.
      
      Select Id
          
        Case #EditorEnterCommandMode
          *Editor\CommandMode = *Editor\Mode
          *Editor\Mode = #CommandMode
          
        Case #EditorAbortCommandMode
          *Editor\Mode = *Editor\CommandMode
          *Editor\CommandInput = ""
          *Editor\CommandMode = #NormalMode
          
        Case #EditorExitCommandMode
          *Editor\Mode = *Editor\CommandMode
          *Editor\CommandMode = #NormalMode
          
          Define.s Input = *Editor\CommandInput
          Define *Ptr = @Input
          Define.s CommandName
          Define.s CommandArgs
          
          *Editor\CommandInput = ""
          
          While #True
            
            Define.c Char = PeekC( *Ptr )
            If Char = #NUL Or IsWhitespace( Char )
              
              CompilerIf #PB_Unicode
                CommandName = PeekS( @Input, ( *Ptr - @Input ) / SizeOf( Character ) )
              CompilerElse
                CommandName = PeekS( @Input, *Ptr - @Input, #PB_Ascii )
              CompilerEndIf
              
              ; Check for arguments.
              If Char <> #NUL
                ; Skip whitespace.
                While #True
                  *Ptr + SizeOf( Character )
                  Char = PeekC( *Ptr )
                  If Char = #NUL Or Not IsWhitespace( Char )
                    Break
                  EndIf
                Wend
                If Char <> #NUL
                  Define *ArgStart = *Ptr
                  ;;TODO: automatically trim whitespace at right end
                  While #True
                    *Ptr + SizeOf( Character )
                    Char = PeekC( *Ptr )
                    If Char = #NUL
                      Break
                    EndIf
                  Wend
                  CompilerIf #PB_Unicode
                    CommandArgs = PeekS( *ArgStart, ( *Ptr - *ArgStart ) / SizeOf( Character ) )
                  CompilerElse
                    CommandArgs = PeekS( *ArgStart, *Ptr - *ArgStart, #PB_Ascii )
                  CompilerEndIf
                EndIf
              EndIf
              
              Define.Command *Command = LookupShellCommandInternal( *Editor, CommandName )
              If *Command <> #Null
                ExecuteShellCommandInternal( *Editor, *Command, CommandArgs )
              EndIf
    
              Break
              
            Else
              
              *Ptr + SizeOf( Character )
              
            EndIf
            
          Wend
          
      EndSelect
      
    Else
      
      ; Execute editor-specific command.
      Define.IEditor *EditorInterface = *Editor
      *EditorInterface\ExecuteCommand( Id, Arguments )
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure ExecuteShellCommand( *Shell.Shell, Command.s, Arguments.s = "" )
    
    DebugAssert( *Shell <> #Null )
    DebugAssert( Len( Command ) > 0 )
    
    If *Shell\CurrentEditor < 0
      ProcedureReturn
    EndIf
    
    Define.Editor *Editor = GetCurrentEditor( *Shell )
    
    Define.Command *Command = LookupShellCommandInternal( *Editor, Command )
    If *Command <> #Null
      ExecuteShellCommandInternal( *Editor, *Command, Arguments )
    EndIf
    
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
    
    Define.Editor *Editor = GetCurrentEditor( *Shell )
    Define.ModeRecord *Mode = @*Editor\Type\Modes( *Editor\Mode )
    
    ForEach *Mode\Commands()
      
      Define.Command *Command = *Mode\Commands()
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
    
    Define.Editor *Editor = GetCurrentEditor( *Shell )
    Define.ModeRecord *Mode = @*Editor\Type\Modes( *Editor\Mode )
    
    Define *Ptr = @Input
    Define *StartPtr = *Ptr
    Define.i Length = Len( Input )
    Define.s Buffer
    Define.i BufferLength = 0
    Define.i BufferCapacity = 0
    Define.i InAngleBrackets = #False
    
    Macro FlushInput
      
      If BufferLength > 0
                
        If *Editor\Mode = #CommandMode
          
          *Editor\CommandInput + Buffer
          
        Else
          
          Define.IEditor *EditorInterface = *Editor
          *EditorInterface\SendInput( Buffer )
          
        EndIf
        
        BufferLength = 0
        
      EndIf
      
    EndMacro
    
    ;;FIXME: need to guarantee null terminator on Buffer
    
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
        Define.i SequenceLength = ( *Ptr - *StartPtr ) / SizeOf( Character ) + 1 ; Include '>'
        If SequenceLength > 0
          
          FlushInput
          
          Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, SequenceLength )
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
        
      ElseIf Not InAngleBrackets
        
        ; Check if current character is mapped to a command.
        Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *Ptr, 1 )
        Define.Binding *Binding = FindMapElement( *Mode\Bindings(), Buffer )
        BufferLength = 0
        
        If *Binding <> #Null
          ; Yes, it is. Flush prior input (if any) and execute command.
          If *StartPtr <> *Ptr
            Buffer = StringAppendChars( Buffer, @BufferLength, @BufferCapacity, *StartPtr, ( *Ptr - *StartPtr ) / SizeOf( Character ) - 1 )
          EndIf
          *StartPtr = *Ptr + SizeOf( Character )
          *Command = *Binding\Command
        EndIf
        
      EndIf
      
      ; Send input.
      FlushInput
      
      ; Execute command.
      If *Command <> #Null
        ExecuteShellCommandInternal( *Editor, *Command )
        *Mode = @*Editor\Type\Modes( *Editor\Mode ); May have changed.
      EndIf
        
      If Char = #NUL
        Break
      EndIf
     
      *Ptr + SizeOf( Character )
      
    Wend
    
    If InAngleBrackets
      ;;TODO: error
    EndIf
    
    UndefineMacro FlushInput
    
  EndProcedure
  
  ;............................................................................
  
  DataSection
    
    Editor_CommonCommands:
      CommandData( #EditorEnterCommandMode, "command", #NormalMode, ":" )
      CommandData( #EditorExitCommandMode, "exit", #CommandMode, "<RET>" )
      CommandData( #EditorAbortCommandMode, "abort", #CommandMode, "<ESC>" )
    
  EndDataSection
    
EndModule

;..............................................................................

DeclareModule TestEditor
  
  UseModule Shell
  
  ;............................................................................
  
  Enumeration EditorCommands
    #TestEditorCommandNone
    #TestEditorCommand1
    #TestEditorCommand2
    #TestEditorSwitchToMode1
    #TestEditorSwitchToNormalMode
  EndEnumeration
  
  Enumeration EditorMode
    #Mode1
  EndEnumeration
  
  Structure RecordedCommand
    Id.i
    Arguments.s
  EndStructure
  
  Structure TestEditor Extends Editor
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
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TestEditor_GetType( *Editor.TestEditor )
    ProcedureReturn "TestEditor"
  EndProcedure
  
  ;............................................................................

  Procedure.i TestEditor_GetCommands( *Editor.TestEditor, *OutCommands.Command )
    DebugAssert( *OutCommands <> #Null )
    PokeQ( *OutCommands, ?TestEditor_Commands )
    ProcedureReturn 4
  EndProcedure
  
  ;............................................................................
  
  Procedure TestEditor_ExecuteCommand( *Editor.TestEditor, CommandId.i, Arguments.s )
    
    DebugAssert( *Editor <> #Null )
    
    Define.RecordedCommand *Record = AddElement( *Editor\RecordedCommands() )
    *Record\Id = CommandId
    *Record\Arguments = Arguments
    
    Select CommandId
        
      Case #TestEditorSwitchToNormalMode:
        *Editor\Mode = #NormalMode
        
      Case #TestEditorSwitchToMode1:
        *Editor\Mode = #Mode1
        
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
      CommandData( #TestEditorCommand1, "command1", #NormalMode, "<ESC>" )
      CommandData( #TestEditorCommand2, "othercommand", #NormalMode, "i" )
      CommandData( #TestEditorSwitchToMode1, "switchtomode1", #NormalMode, "p" )
      CommandData( #TestEditorSwitchToNormalMode, "switchtodefaultmode", #Mode1, "<ESC>" )
      
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
  Define.TestEditor *TestEditor1 = *Editor1
  
  Assert( Shell\NumEditors = 1 )
  Assert( GetCurrentEditor ( @Shell ) = *Editor1 )
  Assert( *Editor1\GetType() = "TestEditor" )
  Assert( *TestEditor1\Mode = #NormalMode )
  
  Define.IEditor *Editor2 = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  Define.TestEditor *TestEditor2 = *Editor2
  
  Assert( Shell\NumEditors = 2 )
  Assert( GetCurrentEditor( @Shell ) = *Editor2 )
  Assert( *Editor2\GetType() = "TestEditor" )
  Assert( *TestEditor2\Mode = #NormalMode )
  
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
  
  ; Without parameter.
  ExecuteShellCommand( @Shell, "command1" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  
  Define.RecordedCommand *Command = FirstElement( *Editor\RecordedCommands() )
  Assert( *Command\Id = #TestEditorCommand1 )
  Assert( *Command\Arguments = "" )
  
  ClearList( *Editor\RecordedCommands() )
  
  ; With simple parameter.
  ExecuteShellCommand( @Shell, "Command1", "Args..." )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Define.RecordedCommand *Command = FirstElement( *Editor\RecordedCommands() )
  Assert( *Command\Id = #TestEditorCommand1 )
  Assert( *Command\Arguments = "Args..." )
  
  DestroyShell( @Shell )

EndProcedureUnit

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
  Assert( CommandCount = 4 )
  Assert( FindStringInArray( Commands(), "command" ) <> -1 )
  Assert( FindStringInArray( Commands(), "command1" ) <> -1 )
  Assert( FindStringInArray( Commands(), "othercommand" ) <> -1 )
  Assert( FindStringInArray( Commands(), "switchtomode1" ) <> -1 )
  
  CommandCount = ListShellCommands( @Shell, Commands(), "com" )
  
  Assert( ArraySize( Commands() ) > 0 )
  Assert( CommandCount = 2 )
  Assert( Commands( 0 ) = "command" )
  Assert( Commands( 1 ) = "command1" )
  
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

ProcedureUnit CanSendCommandInputToShell()

  UseModule Shell
  UseModule TestEditor

  Define.Shell Shell
  CreateShell( @Shell )
  Define.TestEditor *Editor = CreateEditor( @Shell, SizeOf( TestEditor ), @CreateTestEditor() )
  
  SendShellInput( @Shell, ":com" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 0 )  
  
  SendShellInput( @Shell, "mand1 args<RET>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Define.RecordedCommand *Command = FirstElement( *Editor\RecordedCommands() )
  Assert( *Command\Id = #TestEditorCommand1 )
  Assert( *Command\Arguments = "args" )
  
  ClearList( *Editor\RecordedCommands() )
  
  SendShellInput( @Shell, ":command1<ESC>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 0 )
  Assert( *Editor\Mode = #NormalMode )
  
  SendShellInput( @Shell, ":<RET>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 0 )
  Assert( *Editor\Mode = #NormalMode)
  
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
  Assert( *Editor\RecordedCommands()\Arguments = "" )
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
  Assert( *Editor\Mode = #Mode1 )
  
  ClearList( *Editor\RecordedCommands() )
  
  SendShellInput( @Shell, "<ESC>" )
  
  Assert( ListSize( *Editor\RecordedCommands() ) = 1 )
  Assert( *Editor\RecordedCommands()\Id = #TestEditorSwitchToNormalMode )
  Assert( *Editor\Mode = #NormalMode )
  
  DestroyShell( @Shell )

EndProcedureUnit

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 482
; FirstLine = 444
; Folding = -----
; Markers = 530,606,789
; EnableXP