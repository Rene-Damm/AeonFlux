DeclareModule Utils
  
  #MIN_QUAD = -9223372036854775808
  #MAX_QUAD = 9223372036854775807
  
  Macro QUOTE
    "
  EndMacro
  
  ;;FIXME: Doesn't actually work with strings :(
  Macro DebugAssert( Expression )
    CompilerIf #PB_Compiler_Debugger
      If Not ( Expression )
        Debug "Assert: " + QUOTE#Expression#QUOTE
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
  Declare.i StartsWith                ( Prefix.s, String.s )
  Declare   NotImplemented            ( Message.s )
  
  Declare.q ArrayAppendWithCapacity   ( *Ptr, *Count, *Element, SizeOfElementsInBytes.i, Increment.i = 32 )
  Declare   ArrayEraseAtWithCapacity  ( *Ptr, *Count, Index.q, SizeOfElementsInBytes.i )
  
  Declare.s StringAppendChars         ( Buffer.s, *BufferLength, *BufferCapacity, *Chars, NumChars.i )
  Declare.q FindStringInArray         ( Array Strings.s( 1 ), String.s )
  
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
    Define.q Remainder = Number % Alignment
    If Remainder = 0
      ProcedureReturn Number
    EndIf
    ProcedureReturn Number + ( Alignment - Remainder )
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
  
  Procedure.i StartsWith( Prefix.s, String.s )
    
    Define *PrefixPtr = @Prefix
    Define *StringPtr = @String
    
    While #True
      
      Define.c PrefixChar = PeekC( *PrefixPtr )
      If PrefixChar = #NUL
        ProcedureReturn #True
      EndIf
      
      Define.c StringChar = PeekC( *StringPtr )
      If PrefixChar <> StringChar
        ProcedureReturn #False
      EndIf
      
      *PrefixPtr + SizeOf( Character )
      *StringPtr + SizeOf( Character )
      
    Wend
    
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
  
  ;............................................................................
  
  Procedure ArrayEraseAtWithCapacity( *Ptr, *Count, Index.q, SizeOElementInBytes.i )
    
    Define *Array = PeekQ( *Ptr )
    Define.q Count = PeekQ( *Count )
    
    DebugAssert( *Array <> #Null )
    DebugAssert( Index >= 0 And Index < Count )
    
    If Index < Count - 1
      MoveMemory( *Array + ( Index + 1 ) * SizeOElementInBytes, *Array + Index * SizeOElementInBytes, ( Count - Index - 1 ) * SizeOElementInBytes )
    EndIf
    
    PokeQ( *Count, Count - 1 )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s StringAppendChars( Buffer.s, *BufferLength, *BufferCapacity, *Chars, NumChars.i )
    
    DebugAssert( *BufferLength <> #Null )
    DebugAssert( *BufferCapacity <> #Null )
    DebugAssert( *Chars <> #Null )
    
    Define.i Length = PeekI( *BufferLength )
    Define.i Capacity = PeekI( *BufferCapacity )
    
    ; Grow buffer, if necessary.
    If Length + NumChars + 1 > Capacity ; Account for terminator.
      Capacity + Max( NumChars + 1, 32 )
      Define.s NewBuffer = Space( Capacity )
      If Length > 0
        CopyMemory( @Buffer, @NewBuffer, Length * SizeOf( Character ) )
      EndIf
      PokeI( *BufferCapacity, Capacity )
      Buffer = NewBuffer
    EndIf
    
    ; Append characters.
    CopyMemory( *Chars, @Buffer + Length * SizeOf( Character ), NumChars * SizeOf( Character ) )
    PokeC( @Buffer + ( Length + NumChars ) * SizeOf( Character ), #NUL )
    PokeI( *BufferLength, Length + NumChars )
    
    ProcedureReturn Buffer
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q FindStringInArray( Array Strings.s( 1 ), String.s )
    
    Define.q Length = ArraySize( Strings() )
    Define.q StringLen = Len( String )
    Define.q Index
    For Index = 0 To Length - 1
      Define.s Element = Strings( Index )
      If FindString( Element, String, 0, #PB_String_NoCase ) And Len( Element ) = StringLen
        ProcedureReturn Index
      EndIf
    Next
    
    ProcedureReturn -1
    
  EndProcedure
  
EndModule

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 68
; FirstLine = 33
; Folding = ---
; EnableXP