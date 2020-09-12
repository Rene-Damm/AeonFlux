EnableExplicit

DeclareModule Workspace
  
  UseModule Utils
  UseModule Math
  
  ;............................................................................
  
  Enumeration BlobType
    #InvalidBlob
    #TextBlob
  EndEnumeration
  
  Enumeration BlobSubType
    
    #InvalidSubType
    
    ; Text subtypes.
    #TextTypeGeneric
    #TextTypeFlux
    
  EndEnumeration

  ;............................................................................
  
  ;information we need for a blob:
  ; - stable identifier (RO)
  ; - display name (RW)
  ; - type (RO), e.g. "text"
  ; - subtype (RW), e.g. "code"
  ; - transform (RW), i.e. position, rect, and scale
  ; - children (RW)
  
  Structure Blob
    
    BlobType.i
    ContentType.s
    Name.s
    Id.s
    Position.Vector2f
    Size.Vector2f
    Zoom.f
    FileHandle.q
    MetaFileHandle.q
    
    ;what about additional blob-specific stuff?
    
  EndStructure
  
  ; A workspace is a collection of blobs.
  Structure Workspace
    
    Name.s
    *Files.IFileSystem
    
    Map BlobsByName.q()
    Map BlobsById.q()
    
  EndStructure
  
  ;............................................................................
  
  Declare   LoadWorkspace( *Workspace.Workspace, *Files.IFileSystem )
  Declare   UnloadWorkspace( *Workspace.Workspace )
  
  Declare.q CreateBlob( *Workspace.Workspace, BlobType.i, Name.s )
  Declare.i GetBlobCount( *Workspace.Workspace )
  
  CompilerIf #False
  Declare.q OpenBlob( *Workspace.Workspace, Id.s )
  Declare   DeleteBlob( *Workspace.Workspace, *Blob.Blob )
  Declare   UpdateBlob( *Workspace.Workspace, *Blob.Blob )
  Declare.q FindBlobById( *Workspace.Workspace, Id.s )
  Declare.q FindBlobByName( *Workspace.Workspace, Name.s )
  CompilerEndIf
  
EndDeclareModule

Module Workspace
  
  UseModule Utils
  
  ;............................................................................
  
  Structure BlobMeta
    
    BlobType.s
    ContentType.s
    Name.s
    
  EndStructure
    
  ;............................................................................
  
  Procedure.s BlobTypeToString( BlobType.i )
    
    Select BlobType
        
      Case #TextBlob
        ProcedureReturn "Text"
        
    EndSelect
    
    ProcedureReturn "Unknown"
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.i StringToBlobType( BlobType.s )
    
    Select LCase( BlobType )
        
      Case "text"
        ProcedureReturn #TextBlob
        
    EndSelect
    
    ProcedureReturn #InvalidBlob
    
  EndProcedure
  
  ;............................................................................
  
  Procedure ReadMetaFile( *Blob.Blob, *Files.IFileSystem )
    
    DebugAssert( *Blob <> #Null )
    DebugAssert( Len( *Blob\Id ) > 0 )
    DebugAssert( *Blob\MetaFileHandle <> 0 )
    DebugAssert( *Files <> #Null )
    
    ; Read file into buffer.
    Define.s Path = *Blob\Id + ".meta"
    Define.i NumBytes = *Files\GetFileSize( Path )
    Define *Buffer = AllocateMemory( NumBytes )
    
    *Files\ReadFile( *Blob\MetaFileHandle, 0, NumBytes, *Buffer )
    
    ; Convert to string.
    Define.s Text = PeekS( *Buffer, NumBytes, #PB_UTF8 )
    FreeMemory( *Buffer )
    
    ; Parse JSON.
    Define.i JSON = ParseJSON( #PB_Any, Text, #PB_JSON_NoCase )
    Define.BlobMeta Meta
    ExtractJSONStructure( JSONValue( JSON ), @Meta, BlobMeta )
    FreeJSON( JSON )
    
    ; Copy values.
    *Blob\Name = Meta\Name
    *Blob\ContentType = Meta\ContentType
    *Blob\BlobType = StringToBlobType( Meta\BlobType )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteMetaFile( *Blob.Blob, *Files.IFileSystem )
    
    DebugAssert( *Blob <> #Null )
    DebugAssert( Len( *Blob\Id ) > 0 )
    DebugAssert( *Blob\MetaFileHandle <> 0 )
    DebugAssert( *Files <> #Null )
    
    Define.BlobMeta Meta
    
    Meta\BlobType = BlobTypeToString( *Blob\BlobType )
    Meta\ContentType = *Blob\ContentType
    Meta\Name = *Blob\Name
    
    Define.i JSON = CreateJSON( #PB_Any )
    InsertJSONStructure( JSONValue( JSON ), @Meta, BlobMeta )
    
    Define.i NumBytes = ExportJSONSize( JSON, #PB_JSON_PrettyPrint )
    Define *Buffer = AllocateMemory( NumBytes, #PB_Memory_NoClear )
    ExportJSON( JSON, *Buffer, NumBytes, #PB_JSON_PrettyPrint )
    FreeJSON( JSON )
        
    *Files\WriteFile( *Blob\MetaFileHandle, 0, NumBytes, *Buffer )
    FreeMemory( *Buffer )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure LoadWorkspace( *Workspace.Workspace, *Files.IFileSystem )
    
    DebugAssert( *Workspace <> #Null )
    DebugAssert( *Files <> #Null )
    
    *Workspace\Files = *Files
    
    Dim Files.s( 1 )
    Define.i NumMetaFiles = *Files\ListFiles( "", ".*\.meta", Files() )
    If NumMetaFiles > 0
      Define.i Index
      For Index = 0 To NumMetaFiles - 1
        
        Define.s MetaFilePath = Files( Index )
        Define.s Id = GetFilePart( MetaFilePath, #PB_FileSystem_NoExtension )
        Define.s BlobFilePath = Id + ".blob"
        
        Define.Blob *Blob = AllocateStructure( Blob )
        
        *Blob\Id = Id
        *Blob\MetaFileHandle = *Files\OpenFile( MetaFilePath )
        
        ReadMetaFile( *Blob, *Files )
        
        *Workspace\BlobsById( Id ) = *Blob
        *Workspace\BlobsByName( *Blob\Name ) = *Blob
        
      Next
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure UnloadWorkspace( *Workspace.Workspace )
    
    DebugAssert( *Workspace <> #Null )
    
    ; Free blobs.
    ForEach *Workspace\BlobsById()
      
      Define.Blob *Blob = *Workspace\BlobsById()
      
      *Workspace\Files\CloseFile( *Blob\FileHandle )
      *Workspace\Files\CloseFile( *Blob\MetaFileHandle )
      
      FreeStructure( *Blob )
      
    Next
    
    ResetStructure( *Workspace, Workspace )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q CreateBlob( *Workspace.Workspace, BlobType.i, Name.s )
    
    DebugAssert( *Workspace <> #Null )
    DebugAssert( BlobType > #InvalidBlob )
    DebugAssert( Len( Name ) > 0 )
    DebugAssert( FindMapElement( *Workspace\BlobsByName(), Name ) = #Null )
    
    Define.Blob *Blob = AllocateStructure( Blob )
    
    *Blob\BlobType = BlobType
    *Blob\Id = GenerateGUID()
    *Blob\Name = Name
    *Blob\FileHandle = *Workspace\Files\CreateFile( *Blob\Id + ".blob" )
    *Blob\MetaFileHandle = *Workspace\Files\CreateFile( *Blob\Id + ".meta" )
    
    *Workspace\BlobsById( *Blob\Id ) = *Blob
    *Workspace\BlobsByName( *Blob\Name ) = *Blob
    
    WriteMetaFile( *Blob, *Workspace\Files )
    
    ProcedureReturn *Blob
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.i GetBlobCount( *Workspace.Workspace )
    
    DebugAssert( *Workspace <> #Null )
    
    ProcedureReturn MapSize( *Workspace\BlobsById() )
    
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanSaveAndLoadWorkspace()

  UseModule Workspace
  UseModule Utils
  
  Define.Workspace *Workspace = AllocateStructure( Workspace )
  *Workspace\Name = "TestWorkspace"
  
  Define.IFileSystem *Files = CreateVirtualFileSystem()
  LoadWorkspace( *Workspace, *Files )
  
  Assert( GetBlobCount( *Workspace ) = 0 )
  
  Define.Blob *FirstBlob = CreateBlob( *Workspace, #TextBlob, "FirstBlob" )
  
  Assert( GetBlobCount( *Workspace ) = 1 )
  Assert( *FirstBlob <> #Null )
  Assert( *FirstBlob\Name = "FirstBlob" )
  Assert( *FirstBlob\Id <> "" )
  Assert( *FirstBlob\FileHandle <> 0 )
  Assert( *FirstBlob\MetaFileHandle <> 0 )
  Assert( *Files\FileExists( *FirstBlob\Id + ".blob" ) = #True )
  Assert( *Files\FileExists( *FirstBlob\Id + ".meta" ) = #True )
  
  Define.s FirstBlobId = *FirstBlob\Id
  
  UnloadWorkspace( *Workspace )
  
  Assert( GetBlobCount( *Workspace ) = 0 )
  Assert( *Files\FileExists( FirstBlobId + ".blob" ) = #True )
  Assert( *Files\FileExists( FirstBlobId + ".meta" ) = #True )
  
  LoadWorkspace( *Workspace, *Files )
  
  Assert( GetBlobCount( *Workspace ) = 1 )
  
  UnloadWorkspace( *Workspace )
  FreeStructure( *Workspace )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 58
; FirstLine = 24
; Folding = --
; EnableXP