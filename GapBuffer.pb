DeclareModule GapBuffer
  
  ;............................................................................
  
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
  
  Declare   AllocateGapBuffer           ( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
  Declare   FreeGapBuffer               ( *Buffer.GapBuffer )
  Declare.q GetGapBufferPosition        ( *Buffer.GapBuffer )
  Declare.q GetGapBufferLength          ( *Buffer.GapBuffer )
  Declare   WriteIntoGapBuffer          ( *Buffer.GapBuffer, *Ptr, Count.q )
  Declare.q ReadFromGapBuffer           ( *Buffer.GapBuffer, *Ptr, Position.q, Count.q )
  Declare   MoveGapInGapBufferRelative  ( *Buffer.GapBuffer, Offset.q )
  Declare   MoveGapInGapBufferAbsolute  ( *Buffer.GapBuffer, Position.q )
  
EndDeclareModule

Module GapBuffer
  
  UseModule Utils
  
  ;............................................................................
  
  Procedure EnsureLeftBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define LeftSizeFilled = *Buffer\LeftBufferEnd - *Buffer\LeftBuffer
    Define LeftCapacity = *Buffer\LeftBufferSizeInBytes - LeftSizeFilled
    
    If LeftCapacity < CapacityInBytes
      
      Define NewLeftBufferSize = AlignToMultipleOf( *Buffer\LeftBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewLeftBuffer = AllocateMemory( NewLeftBufferSize, #PB_Memory_NoClear )
      CopyMemory( *Buffer\LeftBuffer, *NewLeftBuffer, LeftSizeFilled )
      *Buffer\LeftBufferSizeInBytes = NewLeftBufferSize
      *Buffer\LeftBuffer = *NewLeftBuffer
      *Buffer\LeftBufferEnd = *NewLeftBuffer + LeftSizeFilled
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure EnsureRightBufferCapacity( *Buffer.GapBuffer, CapacityInBytes.q )
    
    CapacityInBytes = AlignToMultipleOf( CapacityInBytes, *Buffer\StrideInBytes )
    
    Define RightCapacity = *Buffer\RightBufferStart - *Buffer\RightBuffer
    
    If RightCapacity < CapacityInBytes
      
      Define RightSizeFilled = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart
      Define NewRightBufferSize = AlignToMultipleOf( *Buffer\RightBufferSizeInBytes + Max( 1024, CapacityInBytes ), 1024 )
      Define *NewRightBuffer = AllocateMemory( NewRightBufferSize, #PB_Memory_NoClear )
      Define *NewRightBufferStart = *NewRightBuffer + NewRightBufferSize - RightSizeFilled
      CopyMemory( *Buffer\RightBufferStart, *NewRightBufferStart, RightSizeFilled )
      *Buffer\RightBufferSizeInBytes = NewRightBufferSize
      *Buffer\RightBuffer = *NewRightBuffer
      *Buffer\RightBufferStart = *NewRightBufferStart
      
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure AllocateGapBuffer( *Buffer.GapBuffer, SizeInBytes.q, StrideInBytes.b )
    
    DebugAssert( SizeInBytes % StrideInBytes = 0 )
    
    ;REVIEW Defer allocation until we put something in the buffer?
    
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
  
  Procedure MoveGapInGapBufferRelative( *Buffer.GapBuffer, Offset.q )
    
    Define OffsetInBytes = Offset * *Buffer\StrideInBytes
    
    If Offset > 0
      
      DebugAssert( OffsetInBytes <= *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes - *Buffer\RightBufferStart )
      
      EnsureLeftBufferCapacity( *Buffer, OffsetInBytes )
      CopyMemory( *Buffer\RightBufferStart, *Buffer\LeftBufferEnd, OffsetInBytes )
      
      *Buffer\LeftBufferEnd + OffsetInBytes
      *Buffer\RightBufferStart + OffsetInBytes
      
    ElseIf Offset < 0
      
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
  
EndModule

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
  
ProcedureUnit CanInsertDataIntoGapBuffer()

  UseModule GapBuffer

  Define Buffer.GapBuffer
  AllocateGapBuffer( @Buffer, 256, SizeOf( Character ) )
  
  Assert( GetGapBufferLength( @Buffer ) = 0 )
  
  WriteIntoGapBuffer( @Buffer, @"Test", 5 ) ; Include NUL.
  
  Assert( GetGapBufferLength( @Buffer ) = 5 )
  Assert( PeekS( Buffer\LeftBuffer ) = "Test" )

EndProcedureUnit
  
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
  
  UnitDebug( String )
  Assert( String = "irstSe" )
  
  MoveGapInGapBufferAbsolute( @Buffer, 0 )
  
  ReadFromGapBuffer( @Buffer, @String, 5, 6 )
  
  Assert( String = "Second" )
  
  ReadFromGapBuffer( @Buffer, @String, 0, 6 )
  
  Assert( String = "FirstS" )

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 374
; Folding = ---
; EnableXP