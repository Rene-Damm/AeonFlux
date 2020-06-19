EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "TextEditor.pb"

; Adds a command model around TextEditor.
; Command model is very Vim-inspired.
DeclareModule TextEditorShell
  
  UseModule TextEditor
  UseModule TextBuffer
    
  ;............................................................................
  
  Structure TextEditorShell
    *Methods
    Editor.TextEditor
    Buffer.TextBuffer
  EndStructure
    
  ;............................................................................

  Declare.q CreateTextEditorShell( *Editor.TextEditorShell )

EndDeclareModule

Module TextEditorShell
  
  UseModule Utils
  UseModule TextEditor
  UseModule TextBuffer
  
  ;............................................................................
  
  Procedure.q CreateTextEditorShell( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    InitializeStructure( *Editor, TextEditorShell )
    *Editor\Methods = ?TextEditorShell_VTable
    
    CreateTextBuffer( @*Editor\Buffer )
    CreateTextEditor( @*Editor\Editor, @*Editor\Buffer )
    
    ProcedureReturn *Editor
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TextEditorShell_GetType( *Editor.TextEditorShell )
    
    DebugAssert( *Editor <> #Null )
    
    ProcedureReturn "TextEditor"

  EndProcedure
  
  ;............................................................................
  
  DataSection
    ; IEditor
    TextEditorShell_VTable:
      Data.q @TextEditorShell_GetType() 
  EndDataSection
  
EndModule

;..............................................................................

ProcedureUnit CanCreateTextEditorShell()

  UseModule TextEditorShell
  
  Define.TextEditorShell TextEditorShell
  Define.IEditor TextEditor = CreateTextEditorShell( @TextEditorShell )
  
  Assert( TextEditor\GetType() = "TextEditor" )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 23
; Folding = -
; EnableXP