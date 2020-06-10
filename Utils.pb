DeclareModule Utils
  
  #MIN_QUAD = -9223372036854775808
  #MAX_QUAD = 9223372036854775807
  
  Macro QUOTE
    "
  EndMacro
  
  Macro DebugAssert( Expression )
    CompilerIf #PB_Compiler_Debugger
      If Not ( Expression )
        Debug "Assert: " + QUOTE Expression QUOTE
        Debug "Line " + Str( #PB_Compiler_Line ) + " in " + #PB_Compiler_File
        CallDebugger
      EndIf
    CompilerEndIf
  EndMacro
  
  Declare.q Min               ( A.q, B.q )
  Declare.q Max               ( A.q, B.q )
  Declare.q AlignToMultipleOf ( Number.q, Alignment.q )
  
EndDeclareModule

Module Utils
  
  ;............................................................................
  
  Procedure.q Min( A.q, B.q )
    If A < B
      ProcedureReturn A
    EndIf
    ProcedureReturn B
  EndProcedure
  
  ;............................................................................

  
  Procedure.q Max( A.q, B.q )
    If A < B
      ProcedureReturn B
    EndIf
    ProcedureReturn A
  EndProcedure
  
  ;............................................................................

  Procedure.q AlignToMultipleOf( Number.q, Alignment.q )
    ProcedureReturn Number + ( Alignment - Number % Alignment )
  EndProcedure
  
EndModule
; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 2
; Folding = --
; EnableXP