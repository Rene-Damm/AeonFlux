EnableExplicit

DeclareModule Workspace
  
  ;............................................................................
  
  Enumeration BlobType
    #InvalidBlob
    #TextBlob
  EndEnumeration

  ;............................................................................
  
  Structure Blob
    BlobType.i
    MimeType.s
    Name.s
    Id.s
  EndStructure

  ;............................................................................
  
  Structure Workspace
    Path.s
    Map BlobsByName.Blob()
  EndStructure
  
  ;............................................................................
  
  Declare   LoadWorkspace( *Workspace.Workspace, Path.s )
  
EndDeclareModule

Module Workspace
  
  UseModule Utils
  
  ;............................................................................
  
  Procedure LoadWorkspace( *Workspace.Workspace, Path.s )
  EndProcedure
  
EndModule

;..............................................................................

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 29
; Folding = -
; EnableXP