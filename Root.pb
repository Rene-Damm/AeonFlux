EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "Project.pb"
XIncludeFile "Workspace.pb"

;root is a single file
;contains a list of workspaces
;contains a list of projects
;has one current workspace
;each workspace can be active or inactive in the root
;each workspace is connected to one or more projects

DeclareModule Root
  
  UseModule Project
  UseModule Workspace
  UseModule Utils
  
  ;............................................................................
  
  EnumerationBinary WorkspaceFlags
    #WorkspaceIsInactive
  EndEnumeration
  
  Structure WorkspaceRecord
    *Workspace.Workspace
    Flags.i
  EndStructure
  
  Structure Root
    
    Name.s
    
    FileHandle.q
    *FileSystem.IFileSystem
    
    ;should these be *lists* of locations?
    *DefaultWorkspaceLocation.IDirectorySystem
    *DefaultProjectLocation.IDirectorySystem
    
    *CurrentWorkspace.Workspace
    
    Map WorkspacesByName.q() ; Lowercased.
    Map ProjectsByName.q() ; Lowercased.
    
    WorkspaceCount.i
    *Workspaces.WorkspaceRecord
    
  EndStructure
  
  ;............................................................................
  
  Global Root.Root
  
  ;............................................................................
  
  Declare   LoadRoot( Name.s, *FileSystem.IFileSystem, *DefaultWorkspaceLocation.IDirectorySystem, *DefaultProjectLocation.IDirectorySystem )
  Declare.q CreateWorkspaceInRoot( Name.s = "" )
  Declare.q LoadWorkspaceIntoRoot( Name.s )
  Declare.q FindWorkspaceInRoot( Name.s )
  CompilerIf #False
  Declare   LoadRoot( Path.s );;file system?
  Declare   RemoveWorkspaceFromRoot( Path.s )
  Declare.q AddProjectToRoot( Path.s )
  Declare   RemoveProjectFromRoot( Path.s )
  Declare.q FindProjectInRoot( NameOrPath.s )
  Declare   SwitchCurrentWorkspace( ... )
  CompilerEndIf
  
EndDeclareModule

Module Root
  
  UseModule Utils
  UseModule Workspace
  
  ;............................................................................
  
  Procedure ResetRootInternal()
    
    If Root\FileHandle = 0
      ProcedureReturn
    EndIf
    
    Root\FileSystem\CloseFile( Root\FileHandle )
    If Root\Workspaces <> #Null
      Define.i Index
      For Index = 0 To Root\WorkspaceCount - 1
        Define.WorkspaceRecord *Record = Root\Workspaces + SizeOf( WorkspaceRecord ) * Index
        UnloadWorkspace( *Record\Workspace )
      Next
      FreeMemory( Root\Workspaces )
    EndIf
    
    ResetStructure( @Root, Root )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q AddWorkspaceInternal( *Workspace.Workspace )
    
    DebugAssert( *Workspace <> #Null )
    
    ; Add to dictionary.
    Define.s Name = *Workspace\Name
    DebugAssert( Len( Name ) > 0 )
    Define.s NameLowerCase = LCase( Name )
    Define.q *Element = AddMapElement( Root\WorkspacesByName(), NameLowerCase )
    PokeQ( *Element, *Workspace )
    
    ; Add to list.
    Define.WorkspaceRecord Record
    Record\Workspace = *Workspace
    Define.q Index = ArrayAppendWithCapacity( @Root\Workspaces, @Root\WorkspaceCount, @Record, SizeOf( WorkspaceRecord ) )
    
    ; If there's no current workspace, make this one current.
    If Root\CurrentWorkspace = #Null
      Root\CurrentWorkspace = *Workspace
    EndIf
    
    ProcedureReturn Index
    
  EndProcedure
  
  ;............................................................................
  
  Procedure SaveRootInternal()
    
    Define.i JSON = CreateJSON( #PB_Any )
    Define.i RootObject = SetJSONObject( JSONValue( JSON ) )
    Define.i WorkspacesArray = SetJSONArray( AddJSONMember( RootObject, "Workspaces" ) )
    Define.i ProjectsArray = SetJSONArray( AddJSONMember( RootObject, "Projects" ) )
    
    Define.i Index
    For Index = 0 To Root\WorkspaceCount - 1
      Define.WorkspaceRecord *Record = Root\Workspaces + Index * SizeOf( WorkspaceRecord )
      Define.Workspace *Workspace = *Record\Workspace
      Define.i WorkspaceObject = SetJSONObject( AddJSONElement( WorkspacesArray ) )
      SetJSONString( AddJSONMember( WorkspaceObject, "Name" ), *Record\Workspace\Name )
    Next
    
    SetJSONString( AddJSONMember( RootObject, "CurrentWorkspace" ), Root\CurrentWorkspace\Name )
    
    Define.s Text = ComposeJSON( JSON, #PB_JSON_PrettyPrint )
    FreeJSON( JSON )
    
    Root\FileSystem\WriteString( Root\FileHandle, Text )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure LoadRootInternal()
    
    Define.s Text = Root\FileSystem\ReadString( Root\FileHandle )
    Define.i JSON = ParseJSON( #PB_Any, Text, #PB_JSON_NoCase )
    
    If JSON = 0
      NotImplemented( "JSON parse error in root file" )
    EndIf
    
    Define.i RootObject = JSONValue( JSON )
    
    ; Workspaces.
    Define.i WorkspacesArray = GetJSONMember( RootObject, "Workspaces" )
    Define.i WorkspaceCount = JSONArraySize( WorkspacesArray )
    If WorkspaceCount > 0
      
      Root\Workspaces = AllocateMemory( SizeOf( WorkspaceRecord ) * WorkspaceCount, #PB_Memory_NoClear )
      Define.i Index
      For Index = 0 To WorkspaceCount - 1
        
        Define.i WorkspaceObject = GetJSONElement( WorkspacesArray, Index )
        Define.s WorkspaceName = GetJSONString( GetJSONMember( WorkspaceObject, "Name" ) )
        
        Define.IFileSystem *WorkspaceFS = Root\DefaultWorkspaceLocation\OpenDirectory( WorkspaceName )
        ;;TODO: diagnose if it fails to open
        If *WorkspaceFS <> #Null
          Define.Workspace *Workspace = AllocateStructure( Workspace )
          *Workspace\Name = WorkspaceName
          LoadWorkspace( *Workspace, *WorkspaceFS )
          AddWorkspaceInternal( *Workspace )
        EndIf
        
      Next
      
      Define.s CurrentWorkspaceName = GetJSONString( GetJSONMember( RootObject, "CurrentWorkspace" ) )
      Root\CurrentWorkspace = FindWorkspaceInRoot( CurrentWorkspaceName )
      
    EndIf
    
    FreeJSON( JSON )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure LoadRoot( Name.s, *FileSystem.IFileSystem, *DefaultWorkspaceLocation.IDirectorySystem, *DefaultProjectLocation.IDirectorySystem )
    
    DebugAssert( Len( Name ) > 0 )
    DebugAssert( *FileSystem <> #Null )
    
    ResetRootInternal()
    
    Root\FileSystem = *FileSystem
    Root\DefaultWorkspaceLocation = *DefaultWorkspaceLocation
    Root\DefaultProjectLocation = *DefaultProjectLocation
    
    Define.s FileName = Name + ".root"
    If *FileSystem\FileExists( FileName )
      
      Root\FileHandle = *FileSystem\OpenFile( FileName )
      LoadRootInternal()
      
    Else
      
      ; Create default contents.
      Root\FileHandle = *FileSystem\CreateFile( FileName )
      CreateWorkspaceInRoot()
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q CreateWorkspaceInRoot( Name.s = "" )
    
    ; If name is not set, generate one.
    If Len( Name ) = 0
      Define.i Number = 1
      While #True
        Name = "Workspace" + Str( Number )
        If Not FindWorkspaceInRoot( Name )
          Break
        EndIf
        Number + 1
      Wend
    EndIf
    
    ; If the given workspace already exists, this just loads it up.
    
    Define.IFileSystem *WorkspaceFS = Root\DefaultWorkspaceLocation\CreateDirectory( Name )
    Define.Workspace *Workspace = AllocateStructure( Workspace )
    *Workspace\Name = Name
    LoadWorkspace( *Workspace, *WorkspaceFS )
    AddWorkspaceInternal( *Workspace )
    SaveRootInternal()
    
    ProcedureReturn *Workspace
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q LoadWorkspaceIntoRoot( Name.s )
    
    DebugAssert( Len( Name ) > 0 )
    DebugAssert( FindWorkspaceInRoot( Name ) = #Null )
    
    Define.IFileSystem *WorkspaceFS = Root\DefaultWorkspaceLocation\OpenDirectory( Name )
    If *WorkspaceFS = #Null
      ProcedureReturn #Null
    EndIf
    
    Define.Workspace *Workspace = AllocateStructure( Workspace )
    *Workspace\Name = Name
    LoadWorkspace( *Workspace, *WorkspaceFS )
    AddWorkspaceInternal( *Workspace )
    SaveRootInternal()
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q FindWorkspaceInRoot( Name.s )
    
    DebugAssert( Len( Name ) > 0 )
    
    ;;REVIEW: is a dictionary for this really worth it?
    
    Define.s NameLowerCase = LCase( Name )
    Define.q *Element = FindMapElement( Root\WorkspacesByName(), NameLowerCase )
    If *Element = #Null
      ProcedureReturn #Null
    EndIf
    
    ProcedureReturn PeekQ( *Element )
    
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanCreateRoot()

  UseModule Root
  UseModule Workspace
  UseModule Utils
  
  ResetStructure( @Root, Root )
  
  Define.IFileSystem *RootSystem = CreateVirtualFileSystem()
  Define.IDirectorySystem *WorkspaceLocation = CreateVirtualDirectorySystem()
  Define.IDirectorySystem *ProjectLocation = CreateVirtualDirectorySystem()
  
  ; Load up root from non-existent state.
  LoadRoot( "Default", *RootSystem, *WorkspaceLocation, *ProjectLocation )
  
  ; Make sure the root got populated with default state.
  Assert( *RootSystem\FileExists( "Default.root" ) )
  Assert( FindString( ReadTextFile( *RootSystem, "Default.root" ), "Workspace1" ) <> 0 )
  Assert( Root\CurrentWorkspace <> #Null )
  Assert( Root\CurrentWorkspace\Name = "Workspace1" )
  Assert( Root\DefaultWorkspaceLocation = *WorkspaceLocation )
  Assert( Root\DefaultProjectLocation = *ProjectLocation )
  Assert( Root\WorkspaceCount = 1 )
  Assert( FindWorkspaceInRoot( "Workspace1" ) = Root\CurrentWorkspace )
  
  ; Create a new workspace.
  Define.Workspace *Workspace2 = CreateWorkspaceInRoot()
  Assert( FindString( ReadTextFile( *RootSystem, "Default.root" ), "Workspace1" ) <> 0 )
  Assert( FindString( ReadTextFile( *RootSystem, "Default.root" ), "Workspace2" ) <> 0 )
  Assert( *Workspace2 <> #Null )
  Assert( *Workspace2\Name = "Workspace2" )
  Assert( Root\CurrentWorkspace <> #Null )
  Assert( Root\CurrentWorkspace\Name = "Workspace1" )
  Assert( Root\WorkspaceCount = 2 )
  Assert( FindWorkspaceInRoot( "Workspace1" ) = Root\CurrentWorkspace )
  Assert( FindWorkspaceInRoot( "Workspace2" ) = *Workspace2 )
  
  ;;TODO: switch current workspace to Workspace2
  
  ; Load up another root from non-existent state.
  LoadRoot( "Other", *RootSystem, *WorkspaceLocation, *ProjectLocation )
  
  ; Make sure the root got populated with default state.
  Assert( *RootSystem\FileExists( "Other.root" ) )
  Assert( FindString( ReadTextFile( *RootSystem, "Other.root" ), "Workspace1" ) <> 0 )
  Assert( Root\CurrentWorkspace <> #Null )
  Assert( Root\CurrentWorkspace\Name = "Workspace1" )
  Assert( Root\DefaultWorkspaceLocation = *WorkspaceLocation )
  Assert( Root\DefaultProjectLocation = *ProjectLocation )
  Assert( Root\WorkspaceCount = 1 )
  Assert( FindWorkspaceInRoot( "Workspace1" ) = Root\CurrentWorkspace )
  
  ; Load first root back up.
  LoadRoot( "Default", *RootSystem, *WorkspaceLocation, *ProjectLocation )
  
  Assert( Root\CurrentWorkspace <> #Null )
  Assert( Root\CurrentWorkspace\Name = "Workspace1" )
  Assert( Root\WorkspaceCount = 2 )
  Assert( FindWorkspaceInRoot( "Workspace1" ) = Root\CurrentWorkspace )
  *Workspace2 = FindWorkspaceInRoot( "Workspace2" )
  Assert( *Workspace2\Name = "Workspace2" )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 191
; FirstLine = 150
; Folding = --
; EnableXP