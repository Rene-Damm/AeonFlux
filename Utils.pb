DeclareModule Utils
  
  #MIN_QUAD = -9223372036854775808
  #MAX_QUAD = 9223372036854775807
  #NEWLINE = 10
  
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
  
  Interface IFileSystem
    OpenFile.q( Path.s, Flags.i )
    Createfile.q( Path.s, Flags.i )
  EndInterface
  
  Prototype.i CompareFn               ( *Left, *Right )
  
  Declare.q Min                       ( A.q, B.q )
  Declare.q Max                       ( A.q, B.q )
  Declare.q AlignToMultipleOf         ( Number.q, Alignment.q )
  Declare.i CompareFnQ                ( *Left, *Right )
  Declare   NotImplemented            ( Message.s )
  
  Declare.q ArrayAppendWithCapacity   ( *Ptr, *Count, *Element, SizeOfElementsInBytes.i, Increment.i = 32 )
  
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
  
  
  ;............................................................................
  
  Procedure.i CompareFnQ( *Left, *Right )
    
    Define LeftValue.q = PeekQ( *Left )
    Define RightValue.q = PeekQ( *Right )
    
    If LeftValue < RightValue
      ProcedureReturn -1
    ElseIf LeftValue > RightValue
      ProcedureReturn 1
    Else
      ProcedureReturn 0
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure NotImplemented( Message.s )
    
    ShowDebugOutput()
    DebuggerError( "Not implemented: " + Message )
    End -1
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q ArrayAppendWithCapacity( *Ptr, *Count, *Element, SizeOfElementInBytes.i, Increment.i = 32 )
    
    DebugAssert( *Ptr <> #Null )
    DebugAssert( *Count <> #Null )
    DebugAssert( *Element <> #Null )
    DebugAssert( SizeOfElementInBytes > 0 )
    DebugAssert( Increment > 0 )
    
    Define *Array = PeekQ( *Ptr )
    Define.q Count
    
    If *Array = #Null
      *Array = AllocateMemory( Increment * SizeOfElementInBytes )
      Count = 0
      PokeQ( *Ptr, *Array )
    Else
      Define.q ArraySize = MemorySize( *Array )
      Count = PeekQ( *Count )
      If ( Count + 1 ) * SizeOfElementInBytes >= *Array + ArraySize
        Define *NewArray = AllocateMemory( ( ArraySize + Increment ) * SizeOfElementInBytes )
        CopyMemory( *Array, *NewArray, ArraySize * SizeOfElementInBytes )
        FreeMemory( *Array )
        *Array = *NewArray
        PokeQ( *Ptr, *Array )
      EndIf
    EndIf
    
    Define *To = *Array + Count * SizeOfElementInBytes
    CopyMemory( *Element, *To, SizeOfElementInBytes )
    PokeQ( *Count, Count + 1 )
    
    ProcedureReturn Count
    
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 37
; Folding = --
; EnableXP