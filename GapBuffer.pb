EnableExplicit

XIncludeFile "Utils.pb"

DeclareModule GapBuffer
  
  UseModule Utils
  
  ;............................................................................
  ; An array with a write head and the array split in two at the position
  ; of the write head.
  ; Can store arbitrary elements of fixed size.
  
  Structure GapBuffer
    
    StrideInBytes.b               ; Size of element in bytes.
    *LeftBuffer                   ; Memory block allocated for left buffer.
    *RightBuffer                  ; Memory block allocated for right buffer.
    *LeftBufferEnd                ; One past last element in left buffer.
    *RightBufferStart             ; First element in right buffer.
    LeftBufferSizeInBytes.q       ; Total size allocated for left buffer in bytes.
    RightBufferSizeInBytes.q      ; Total size allocated for right buffer in bytes.
    
  EndStructure
  
  ;............................................................................
  
  Declare   InitializeGapBuffer           ( *Buffer.GapBuffer, StrideInBytes.b )
  Declare   AllocateGapBuffer             ( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
  Declare   FreeGapBuffer                 ( *Buffer.GapBuffer )
  
  Declare.i IsGapBufferEmpty              ( *Buffer.GapBuffer )
  Declare.q GetGapBufferPosition          ( *Buffer.GapBuffer )
  Declare.q GetGapBufferLength            ( *Buffer.GapBuffer )
  Declare.q GetGapBufferLengthLeft        ( *Buffer.GapBuffer )
  Declare.q GetGapBufferLengthRight       ( *Buffer.GapBuffer )
  Declare.q GapBufferElementAt            ( *Buffer.GapBuffer, Position.q )
  
  Declare   WriteIntoGapBuffer            ( *Buffer.GapBuffer, *Ptr, Count.q )
  Declare.q ReadFromGapBuffer             ( *Buffer.GapBuffer, *Ptr, Position.q, Count.q )
  Declare   EraseFromGapBuffer            ( *Buffer.GapBuffer, Count.q )
  
  Declare   MoveGapInGapBufferRelative    ( *Buffer.GapBuffer, Offset.q )
  Declare   MoveGapInGapBufferAbsolute    ( *Buffer.GapBuffer, Position.q )
  
  Declare.q FindInGapBuffer               ( *Buffer.GapBuffer, *Ptr, Count.q, Position.q = 0 )
  Declare.q FindElementInOrderedGapBuffer ( *Buffer.GapBuffer, *Element, *Compare.CompareFn, Position.q = 0, Count.q = -1 )
  
EndDeclareModule

Module GapBuffer
  
  UseModule Utils
  
  ;............................................................................
  
  Procedure EnsureLeftBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    DebugAssert( CapacityInBytes >= 0 )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define LeftSizeFilled = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    Define LeftCapacity = *Buffer\LeftBufferSizeInBytes - LeftSizeFilled
    
    If LeftCapacity < CapacityInBytes
      
      Define NewLeftBufferSize = AlignToMultipleOf( *Buffer\LeftBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewLeftBuffer = AllocateMemory( NewLeftBufferSize, #PB_Memory_NoClear )
      If LeftSizeFilled > 0
        CopyMemory( *Buffer\LeftBuffer, *NewLeftBuffer, LeftSizeFilled )
      EndIf
      If *Buffer\LeftBuffer <> #Null
        FreeMemory( *Buffer\LeftBuffer )
      EndIf
      *Buffer\LeftBufferSizeInBytes = NewLeftBufferSize
      *Buffer\LeftBuffer = *NewLeftBuffer
      *Buffer\LeftBufferEnd = *NewLeftBuffer + LeftSizeFilled
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure EnsureRightBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    DebugAssert( CapacityInBytes >= 0 )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define RightCapacity = *Buffer\RightBufferStart - *Buffer\RightBuffer
    
    If RightCapacity < CapacityInBytes
      
      Define RightSizeFilled = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
      Define NewRightBufferSize = AlignToMultipleOf( *Buffer\RightBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewRightBuffer = AllocateMemory( NewRightBufferSize, #PB_Memory_NoClear )
      Define *NewRightBufferStart = *NewRightBuffer + NewRightBufferSize - RightSizeFilled
      If RightSizeFilled > 0
        CopyMemory( *Buffer\RightBufferStart, *NewRightBufferStart, RightSizeFilled )
      EndIf
      If *Buffer\RightBuffer <> #Null
        FreeMemory( *Buffer\RightBuffer )
      EndIf
      *Buffer\RightBufferSizeInBytes = NewRightBufferSize
      *Buffer\RightBuffer = *NewRightBuffer
      *Buffer\RightBufferStart = *NewRightBufferStart
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure InitializeGapBuffer( *Buffer.GapBuffer, StrideInBytes.b )
    
    DebugAssert( StrideInBytes > 0 )
    
    InitializeStructure( *Buffer, GapBuffer )
    *Buffer\StrideInBytes = StrideInBytes
    
  EndProcedure
    
  ;............................................................................
  
  Procedure AllocateGapBuffer( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
    
    DebugAssert( SizeInBytes % StrideInBytes = 0 )
    DebugAssert( StrideInBytes > 0 )
    
    *Buffer\StrideInBytes = StrideInBytes
    *Buffer\LeftBuffer = AllocateMemory( SizeInBytes, #PB_Memory_NoClear )
    *Buffer\RightBuffer = AllocateMemory( SizeInBytes, #PB_Memory_NoClear )
    *Buffer\LeftBufferEnd = *Buffer\LeftBuffer
    *Buffer\RightBufferStart = *Buffer\RightBuffer + SizeInBytes
    *Buffer\LeftBufferSizeInBytes = SizeInBytes
    *Buffer\RightBufferSizeInBytes = SizeInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure FreeGapBuffer( *Buffer.GapBuffer )
    
    If *Buffer\LeftBuffer <> 0
      FreeMemory( *Buffer\LeftBuffer )
    EndIf
    
    If *Buffer\RightBuffer <> 0
      FreeMemory( *Buffer\RightBuffer )
    EndIf
    
    ClearStructure( *Buffer, GapBuffer )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.i IsGapBufferEmpty( *Buffer.GapBuffer )
    
    If *Buffer\LeftBuffer = *Buffer\LeftBufferEnd And
       *Buffer\RightBufferStart = ( *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes )
      ProcedureReturn #True
    EndIf
    ProcedureReturn #False
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferPosition( *Buffer.GapBuffer )
    
    ProcedureReturn ( *Buffer\LeftBufferEnd - *Buffer\LeftBuffer ) / *Buffer\StrideInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferLength( *Buffer.GapBuffer )
    
    Define LeftBufferSize = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    Define RightBufferSize = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
    
    ProcedureReturn ( LeftBufferSize + RightBufferSize ) / *Buffer\StrideInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferLengthLeft( *Buffer.GapBuffer )
    ProcedureReturn ( *Buffer\LeftBufferEnd - *Buffer\LeftBuffer ) / *Buffer\StrideInBytes
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetGapBufferLengthRight( *Buffer.GapBuffer )
    ProcedureReturn ( *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart ) / *Buffer\StrideInBytes
  EndProcedure
  
  ;............................................................................
  ; Returns a pointer to the element at the given index.
  
  Procedure.q GapBufferElementAt( *Buffer.GapBuffer, Position.q )
    
    DebugAssert( Position >= 0 )
    DebugAssert( Position < GetGapBufferLength( *Buffer ) )
    
    Define.q NumLeft = GetGapBufferLengthLeft( *Buffer )
    If Position < NumLeft
      ProcedureReturn *Buffer\LeftBuffer + ( Position * *Buffer\StrideInBytes )
    Else
      Position - NumLeft
      ProcedureReturn *Buffer\RightBufferStart + ( Position * *Buffer\StrideInBytes )
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteIntoGapBuffer( *Buffer.GapBuffer, *Ptr, Count.q )
    
    DebugAssert( *Ptr <> 0 )
    DebugAssert( Count >= 0 )
    
    Define SizeOfElements = Count * *Buffer\StrideInBytes
    
    ; See if we need to grow the left buffer.
    EnsureLeftBufferCapacity( *Buffer, SizeOfElements )
    
    ; Copy elements.
    CopyMemory( *Ptr, *Buffer\LeftBufferEnd, SizeOfElements )
    *Buffer\LeftBufferEnd + SizeOfElements
    
  EndProcedure

  ;............................................................................
  
  Procedure.q ReadFromGapBuffer( *Buffer.GapBuffer, *Ptr, Position.q, Count.q )
    
    Define PositionInBytes.q = Position * *Buffer\StrideInBytes
    Define CountInBytes.q = Count * *Buffer\StrideInBytes
    Define *WritePosition = *Ptr
    
    ; Read left buffer portion.
    Define LeftBufferBytes = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    If PositionInBytes < LeftBufferBytes
      
      Define BytesToReadFromLeftBuffer.q = Min( CountInBytes, LeftBufferBytes - PositionInBytes )
      CopyMemory( *Buffer\LeftBuffer + PositionInBytes, *WritePosition, BytesToReadFromLeftBuffer )
      
      If BytesToReadFromLeftBuffer = CountInBytes
        ProcedureReturn BytesToReadFromLeftBuffer / *Buffer\StrideInBytes
      EndIf
      
      *WritePosition + BytesToReadFromLeftBuffer
      CountInBytes - BytesToReadFromLeftBuffer
      PositionInBytes + BytesToReadFromLeftBuffer
      
    EndIf
    
    ; Read right buffer portion.
    Define RightBufferBytes = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
    If PositionInBytes >= LeftBufferBytes And PositionInBytes < LeftBufferBytes + RightBufferBytes
      
      PositionInBytes - LeftBufferBytes
      Define BytesToReadFromRightBuffer.q = Min( CountInBytes, RightBufferBytes - PositionInBytes )
      CopyMemory( *Buffer\RightBufferStart + PositionInBytes, *WritePosition, BytesToReadFromRightBuffer )
      
      *WritePosition + BytesToReadFromRightBuffer
      
    EndIf
    
    ProcedureReturn ( *WritePosition - *Ptr ) / *Buffer\StrideInBytes
    
  EndProcedure
  
  ;............................................................................
  
  Procedure EraseFromGapBuffer( *Buffer.GapBuffer, Count.q )
    
    DebugAssert( *Buffer <> #Null )
    DebugAssert( Count > 0 Or GetGapBufferLengthLeft( *Buffer ) >= AbsQ( Count ) )
    DebugAssert( Count < 0 Or GetGapBufferLengthRight( *Buffer ) >= Count )
    
    If Count < 0
      *Buffer\LeftBufferEnd - Count * -1 * *Buffer\StrideInBytes
    ElseIf Count > 0
      *Buffer\RightBufferStart + Count * *Buffer\StrideInBytes
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure MoveGapInGapBufferRelative( *Buffer.GapBuffer, Offset.q )
    
    Define OffsetInBytes = Offset * *Buffer\StrideInBytes
    
    If Offset > 0
      
      ; Gap moves to the right. Move bytes from right to left buffer.
      
      DebugAssert( OffsetInBytes <= *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart )
      
      EnsureLeftBufferCapacity( *Buffer, OffsetInBytes )
      CopyMemory( *Buffer\RightBufferStart, *Buffer\LeftBufferEnd, OffsetInBytes )
      
      *Buffer\LeftBufferEnd + OffsetInBytes
      *Buffer\RightBufferStart + OffsetInBytes
      
    ElseIf Offset < 0
      
      ; Gap moves to the left. Move bytes from left to right buffer.
      
      OffsetInBytes * -1
      
      DebugAssert( OffsetInBytes <= *Buffer\LeftBufferEnd - *Buffer\LeftBuffer )
      
      EnsureRightBufferCapacity( *Buffer, OffsetInBytes )
      *Buffer\RightBufferStart - OffsetInBytes
      *Buffer\LeftBufferEnd - OffsetInBytes
      CopyMemory( *Buffer\LeftBufferEnd, *Buffer\RightBufferStart, OffsetInBytes )
      
    EndIf
    
  EndProcedure
    
  ;............................................................................
  
  Procedure MoveGapInGapBufferAbsolute( *Buffer.GapBuffer, Position.q )
    
    DebugAssert( Position >= 0 )
    
    Define CurrentPosition = GetGapBufferPosition( *Buffer )
    If CurrentPosition < Position
      MoveGapInGapBufferRelative( *Buffer, Position - CurrentPosition )
    ElseIf CurrentPosition > Position
      MoveGapInGapBufferRelative( *Buffer, - ( CurrentPosition - Position ) )
    EndIf
    
  EndProcedure
    
  ;............................................................................
  
  Procedure.q FindInGapBuffer( *Buffer.GapBuffer, *Ptr, Count.q, Position.q = 0 )
    
    ;;TODO
    NotImplemented( "FindInGapBuffer" )
    
  EndProcedure
    
  ;............................................................................
  
  Procedure.q FindElementInOrderedGapBuffer( *Buffer.GapBuffer, *Element, *Compare.CompareFn, Position.q = 0, Count.q = -1 )
    
    DebugAssert( Position >= 0 )
    DebugAssert( Position < GetGapBufferLength( *Buffer ) )
    DebugAssert( Count < 0 Or Count <= ( GetGapBufferLength( *Buffer ) - Position ) )
    
    Define *LeftBuffer = *Buffer\LeftBuffer
    Define *RightBuffer = *Buffer\RightBufferStart
    
    Define.q NumLeft = GetGapBufferLengthLeft( *Buffer )
    Define.q NumRight = GetGapBufferLengthRight( *Buffer )
    Define.q StrideInBytes = *Buffer\StrideInBytes
    
    ;;REVIEW: Is binary search really a good strategy? Seems like it quickly creates worst case cache access patterns...
    ; Binary search through buffer.
    
    Define.q LeftIndex = Position
    Define.q RightIndex
    Define.q GapIndex = NumLeft
    
    If Count >= 0
      RightIndex = Position + Count - 1
    Else
      RightIndex = NumLeft + NumRight - 1
    EndIf
    
    While LeftIndex <= RightIndex
      
      Define.q MidIndex = ( LeftIndex + RightIndex ) / 2
      
      Define *ElementPtr
      If MidIndex < GapIndex
        *ElementPtr = *LeftBuffer + MidIndex * StrideInBytes
      Else
        *ElementPtr = *RightBuffer + ( MidIndex - GapIndex ) * StrideInBytes
      EndIf
      
      Define.i Comparison = *Compare( *ElementPtr, *Element )
      
      If Comparison < 0
        LeftIndex = MidIndex + 1
      ElseIf Comparison > 0
        RightIndex = MidIndex - 1
      Else
        ProcedureReturn MidIndex
      EndIf
      
    Wend
    
    ProcedureReturn -1
    
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanCreateGapBuffer()

  UseModule GapBuffer
  
  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, 1 )
  
  Assert( Buffer\LeftBuffer <> 0 )
  Assert( Buffer\RightBuffer <> 0 )
  Assert( MemorySize( Buffer\LeftBuffer ) = 256 )
  Assert( MemorySize( Buffer\RightBuffer ) = 256 )
  Assert( Buffer\LeftBufferSizeInBytes = 256 )
  Assert( Buffer\RightBufferSizeInBytes = 256 )
  Assert( Buffer\StrideInBytes = 1 )
  Assert( Buffer\LeftBufferEnd = Buffer\LeftBuffer )
  Assert( Buffer\RightBufferStart = Buffer\RightBuffer + 256 )
  
  FreeGapBuffer( @Buffer )
  
  Assert( Buffer\LeftBuffer = 0 )
  Assert( Buffer\RightBuffer = 0 )
  Assert( Buffer\LeftBufferEnd = 0 )
  Assert( Buffer\RightBufferStart = 0 )
  Assert( Buffer\LeftBufferSizeInBytes = 0 )
  Assert( Buffer\RightBufferSizeInBytes = 0 )

EndProcedureUnit

;..............................................................................

ProcedureUnit CanInsertDataIntoGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Character ) )
  
  Assert( GetGapBufferLength( @Buffer ) = 0 )
  Assert( IsGapBufferEmpty( @Buffer ) = #True )
  
  WriteIntoGapBuffer( @Buffer, @"Test", 5 ) ; Include NUL.
  
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  Assert( PeekS( Buffer\LeftBuffer ) = "Test" )
  Assert( IsGapBufferEmpty( @Buffer ) = #False )
  
  FreeGapBuffer( @Buffer )

EndProcedureUnit

;..............................................................................

ProcedureUnit CanEraseDataFromGapBuffer()

  UseModule GapBuffer
  
  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Character ) )
  WriteIntoGapBuffer( @Buffer, @"TestFlub", 8 )
  MoveGapInGapBufferAbsolute( @Buffer, 4 )
  
  EraseFromGapBuffer( @Buffer, 2 )
  
  Assert( GetGapBufferLength( @Buffer ) = 6 )
  Assert( GetGapBufferPosition( @Buffer ) = 4 )
  
  Define.s First = Space( 6 )
  ReadFromGapBuffer( @Buffer, @First, 0, 6 )
  
  Assert( First = "Testub" )
  
  EraseFromGapBuffer( @Buffer, -2 )
  
  Assert( GetGapBufferLength( @Buffer ) = 4 )
  Assert( GetGapBufferPosition( @Buffer ) = 2 )
  
  Define.s Second = Space( 4 )
  ReadFromGapBuffer( @Buffer, @Second, 0, 4 )
  
  Assert( Second = "Teub" )
  
  FreeGapBuffer( @Buffer )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanMoveGapInGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 4, SizeOf( Character ) )
  
  Assert( GetGapBufferPosition( @Buffer ) = 0 )
  
  WriteIntoGapBuffer( @Buffer, @"First", 5 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 5 )
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  
  MoveGapInGapBufferRelative( @Buffer, -2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 3 )
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  
  WriteIntoGapBuffer( @Buffer, @"me", 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 5 )
  Assert( GetGapBufferLength( @Buffer ) = 7 )
  Assert( PeekS( Buffer\LeftBuffer, 5 ) = "Firme" )
  Assert( PeekS( Buffer\RightBufferStart, 2 ) = "st" )
  
  MoveGapInGapBufferRelative( @Buffer, 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 7 )
  Assert( GetGapBufferLength( @Buffer ) = 7 )
  Assert( PeekS( Buffer\LeftBuffer, 7 ) = "Firmest" )
  
  WriteIntoGapBuffer( @Buffer, @"est", 3 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 10 )
  Assert( GetGapBufferLength( @Buffer ) = 10 )
  Assert( PeekS( Buffer\LeftBuffer, 10 ) = "Firmestest" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 2 )
  
  Assert( GetGapBufferPosition( @Buffer ) = 2 )
  Assert( GetGapBufferLength( @Buffer ) = 10 )
  Assert( PeekS( Buffer\LeftBuffer, 2 ) = "Fi" )
  Assert( PeekS( Buffer\RightBufferStart, 8 ) = "rmestest" )

EndProcedureUnit

;..............................................................................

ProcedureUnit CanReadFromGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 4, SizeOf( Character ) )
  
  WriteIntoGapBuffer( @Buffer, @"First", 5 )
  WriteIntoGapBuffer( @Buffer, @"Second", 6 )
  
  Define String.s = Space( 6 )
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 8, 3 )
  
  Assert( String = "ondond" )
  
  ReadFromGapBuffer( @Buffer, @String, 4, 6 )
  
  Assert( String = "tSecon" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 5 )
  
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 1, 6 )
  
  Assert( String = "irstSe" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 0 )
  
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 0, 6 )
  
  Assert( String = "FirstS" )
  
  FreeGapBuffer( @Buffer )

EndProcedureUnit

;..............................................................................

CompilerIf #False ;;TODO
ProcedureUnit CanSearchInGapBuffer()

  UseModule GapBuffer
  
  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Character ) )
  
  WriteIntoGapBuffer( @Buffer, @"First", 5 )
  WriteIntoGapBuffer( @Buffer, @"Second", 6 )
  WriteIntoGapBuffer( @Buffer, @"Third", 5 )
  
  Assert( FindInGapBuffer( @Buffer, @"Fi", 2 ) = 0 )
  Assert( FindInGapBuffer( @Buffer, @"rst", 3 ) = 2 )
  Assert( FindInGapBuffer( @Buffer, @"cond", 4 ) = 7 )
  Assert( FindInGapBuffer( @Buffer, @"ird", 3 ) = 14 )
  Assert( FindInGapBuffer( @Buffer, @"none", 4 ) = -1 )
  Assert( FindInGapBuffer( @Buffer, @"i", 1, 5 ) = 13 )
  
  MoveGapInGapBufferAbsolute( @Buffer, 3 )
  
  Assert( FindInGapBuffer( @Buffer, @"Fi", 2 ) = 0 )
  Assert( FindInGapBuffer( @Buffer, @"rst", 3 ) = 2 )
  Assert( FindInGapBuffer( @Buffer, @"cond", 4 ) = 7 )
  Assert( FindInGapBuffer( @Buffer, @"ird", 3 ) = 14 )
  Assert( FindInGapBuffer( @Buffer, @"none", 4 ) = -1 )
  Assert( FindInGapBuffer( @Buffer, @"i", 1, 5 ) = 13 )
  
  FreeGapBuffer( @Buffer )
  
EndProcedureUnit
CompilerEndIf

;..............................................................................

ProcedureUnit CanGetPointerToElementInGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Quad ) )
  
  Define.q Value
  For Value = 0 To 10
    WriteIntoGapBuffer( @Buffer, @Value, 1 )
  Next
  
  Assert( PeekQ( GapBufferElementAt( @Buffer, 0 ) ) = 0 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 1 ) ) = 1 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 5 ) ) = 5 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 9 ) ) = 9 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 10 ) ) = 10 )
  
  MoveGapInGapBufferAbsolute( @Buffer, 5 )
  
  Assert( PeekQ( GapBufferElementAt( @Buffer, 0 ) ) = 0 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 1 ) ) = 1 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 4 ) ) = 4 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 5 ) ) = 5 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 9 ) ) = 9 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 10 ) ) = 10 )
  
  MoveGapInGapBufferAbsolute( @Buffer, 10 )
  
  Assert( PeekQ( GapBufferElementAt( @Buffer, 0 ) ) = 0 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 1 ) ) = 1 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 4 ) ) = 4 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 5 ) ) = 5 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 9 ) ) = 9 )
  Assert( PeekQ( GapBufferElementAt( @Buffer, 10 ) ) = 10 )
  
  FreeGapBuffer( @Buffer )
  
EndProcedureUnit

;..............................................................................

ProcedureUnit CanFindElementInOrderedGapBuffer()

  UseModule GapBuffer
  UseModule Utils
  
  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Quad ) )
  
  Define.q Value = 3
  
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = -1 )
  
  For Value = 0 To 10
    WriteIntoGapBuffer( @Buffer, @Value, 1 )
  Next
  
  Value = 3
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 3 )
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ(), 1, 5 ) = 3 )
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ(), 4, 5 ) = -1 )
  Value = 0
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 0 )
  Value = 10
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 10 )  
  Value = 100
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = -1 )
  Value = -1
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = -1 )
  
  MoveGapInGapBufferAbsolute( @Buffer, 4 )
  
  Value = 3
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 3 )
  Value = 4
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 4 )
  Value = 0
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 0 )
  Value = 10
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 10 )  
  Value = 100
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = -1 )
  Value = -1
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = -1 )
  
  MoveGapInGapBufferAbsolute( @Buffer, 0 )
  
  Value = 3
  Assert( FindElementInOrderedGapBuffer( @Buffer, @Value, @CompareFnQ() ) = 3 )
  
  FreeGapBuffer( @Buffer )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 11
; Folding = ----
; EnableXP