EnableExplicit

XIncludeFile "GapBuffer.pb"
XIncludeFile "Utils.pb"

; Text makers are stored in gap buffers.
; All positions stored in the left buffer are relative to the start of the side buffer (*LefBuffer).
; This makes these positions stable irrespective of the location of the gap.
; All positions stored in the right buffer are relative to the end of the right buffer (*RightBufferEnd).
; This makes these positions stable irrespective of the location of the gap.

; This storage scheme allows inserting into the gap without having to update *any* marker positions.
; The gap is only moved when *inserting* a new marker.

; Markers can either be accessed by position or with a stable accessor.

;;REVIEW: Also do dirty rects using markers?
;;REVIEW: Have ability for markers to be linked? (e.g. for folding-type information)

DeclareModule TextMarker
  
  UseModule GapBuffer
  
  ;only some markers need stable accessors; newlines don't
  
  ;............................................................................
  
  ;
  Structure TextMarker
    Index.q
  EndStructure
  
  Structure TextMarkerWithTableIndex Extends TextMarker
    TableIndex.q            ; -1 if no associated table index.
  EndStructure
  
  ; An ordered list of markers.
  ; Can have arbitrary many separate sets of markers.
  Structure TextMarkerList
    Buffer.GapBuffer
    EndPosition.q
  EndStructure
  
  ;............................................................................
  
  Declare   InitializeTextMarkerList( *List.TextMarkerList )
  Declare   DestroyTextMarkerList( *List.TextMarkerList )
  Declare.q GetTextMarkerListLength( *List.TextMarkerList )
  Declare   AddMarkerToTextMarkerList( *List.TextMarkerList, Position.q )
  Declare.q GetNextMarkerPosition( *List.TextMarkerList, Position.q )
  Declare.q GetPreviousMarkerPosition( *List.TextMarkerList, Position.q )
  Declare   ShiftLastMarkerPosition( *List.TextMarkerList, Amount.q )
  Declare.s TextMarkerListDebugString( *List.TextMarkerList )
  
EndDeclareModule

Module TextMarker
  
  UseModule GapBuffer
  UseModule Utils
  
  ;............................................................................
  
  Procedure.q GetPositionLeftOfGap( *List.TextMarkerList )
    Define.GapBuffer *Buffer = @*List\Buffer
    If *Buffer\LeftBufferEnd > *Buffer\LeftBuffer
      ProcedureReturn PeekQ( *Buffer\LeftBufferEnd - SizeOf( Quad ) )
    EndIf
    ProcedureReturn 0
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetPositionRightOfGap( *List.TextMarkerList )
    Define.GapBuffer *Buffer = @*List\Buffer
    If *Buffer\RightBufferStart < *Buffer\RightBuffer
      ProcedureReturn *List\EndPosition - PeekQ( *Buffer\RightBufferStart )
    EndIf
    ProcedureReturn #MAX_QUAD
  EndProcedure

  ;............................................................................
  
  Procedure InitializeTextMarkerList( *List.TextMarkerList )
    InitializeGapBuffer( *List\Buffer, SizeOf( Quad ) )
    *List\EndPosition = 0
  EndProcedure
  
  ;............................................................................
  
  Procedure DestroyTextMarkerList( *List.TextMarkerList )
    FreeGapBuffer( *List\Buffer )
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetTextMarkerListLength( *List.TextMarkerList )
    ProcedureReturn GetGapBufferLength( *List\Buffer )
  EndProcedure
  
  ;............................................................................
  
  Procedure AddMarkerToTextMarkerList( *List.TextMarkerList, Position.q )
    
    DebugAssert( Position >= 0 )
    
    Define *Buffer.GapBuffer = @*List\Buffer
    
    Define *LeftEnd = *Buffer\LeftBufferEnd
    Define *RightStart = *Buffer\RightBufferStart
    Define.q EndPosition = *List\EndPosition
    
    ; Move gap left, if necessary.
    Define.q ShiftLeftCount = 0
    While #True
      
      *LeftEnd - SizeOf( Quad )
      If *LeftEnd < *Buffer\LeftBuffer
        Break
      EndIf
      
      Define.q LeftPosition = PeekQ( *LeftEnd )
      If LeftPosition <= Position
        Break
      EndIf
      
      PokeQ( *LeftEnd, EndPosition - LeftPosition )
      ShiftLeftCount + 1
      
    Wend
      
    If ShiftLeftCount > 0
      MoveGapInGapBufferRelative( *Buffer, - ShiftLeftCount )
    Else
      
      ; Move gap right, if necessary.
      Define *RightEnd = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes
      If *RightStart < *RightEnd
        
        Define.q ShiftRightCount = 0
        
        While #True
          
          Define.q RightPosition = EndPosition - PeekQ( *RightStart )
          If RightPosition > Position
            Break
          EndIf
          
          PokeQ( *RightStart, RightPosition )
          ShiftRightCount + 1
      
          *RightStart + SizeOf( Quad )
          If *RightStart >= *RightEnd
            Break
          EndIf
          
        Wend
        
        If ShiftRightCount > 0
          MoveGapInGapBufferRelative( *Buffer, ShiftRightCount )
        EndIf

      EndIf
            
    EndIf
        
    ; Insert marker.
    ; NOTE: We always write into the left side buffer, so we don't
    ;       need to adjust the position here.
    WriteIntoGapBuffer( *List\Buffer, @Position, 1 )
    ;UnitDebug( Str( GetGapBufferPosition( *List\Buffer ) ) )
    If Position > *List\EndPosition
      *List\EndPosition = Position
    EndIf
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q GetNextMarkerPosition( *List.TextMarkerList, Position.q )
    
    DebugAssert( Position >= 0 )
    
    Define.GapBuffer *Buffer = @*List\Buffer
    
    ; Check left side.
    Define *LeftEnd = *Buffer\LeftBufferEnd - SizeOf( Quad )
    If *LeftEnd >= *Buffer\LeftBuffer And Position < PeekQ( *LeftEnd )
      
      While #True
        
        Define.q *NewLeftEnd = *LeftEnd - SizeOf( Quad )
        If *NewLeftEnd < *Buffer\LeftBuffer
          ProcedureReturn -1
        EndIf
        
        Define.q Value = PeekQ( *NewLeftEnd )
        If Value <= Position
          ProcedureReturn PeekQ( *LeftEnd )
        EndIf
        
        *LeftEnd = *NewLeftEnd
        
      Wend
      
    EndIf
    
    ; Check right side.
    Define *RightStart = *Buffer\RightBufferStart
    Define *RightEnd = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes
    Define.q EndPosition = *List\EndPosition
    If *RightStart < *RightEnd And ( EndPosition - PeekQ( *RightStart ) ) <= Position
      
      While #True
        
        *RightStart + SizeOf( Quad )
        If *RightStart >= *RightEnd
          ProcedureReturn -1
        EndIf
        
        Define.q Value = EndPosition - PeekQ( *RightStart )
        If Value > Position
          ProcedureReturn Value
        EndIf
       
      Wend
      
    EndIf
    
    ; Check if first position in right buffer is the marker we're looking for.
    ; Means gap is right around the position we're looking at.
    If *LeftEnd >= *Buffer\LeftBuffer And PeekQ( *LeftEnd ) <= Position And
       *RightStart < *RightEnd And ( EndPosition - PeekQ( *RightStart ) ) > Position
      ProcedureReturn EndPosition - PeekQ( *RightStart )
    EndIf
    
    ProcedureReturn -1
    
  EndProcedure
  
  ;............................................................................
    
  Procedure.q GetPreviousMarkerPosition( *List.TextMarkerList, Position.q )
    
    DebugAssert( Position >= 0 )
    
    Define.GapBuffer *Buffer = @*List\Buffer
    
    ; Check left side.
    Define *LeftEnd = *Buffer\LeftBufferEnd - SizeOf( Quad )
    If *LeftEnd >= *Buffer\LeftBuffer And PeekQ( *LeftEnd ) >= Position
      
      While #True
        
        *LeftEnd - SizeOf( Quad )
        If *LeftEnd < *Buffer\LeftBuffer
          ProcedureReturn -1
        EndIf
        
        Define.q Value = PeekQ( *LeftEnd )
        If Value < Position
          ProcedureReturn Value
        EndIf
        
      Wend
      
    EndIf
    
    ; Check right side.
    Define *RightStart = *Buffer\RightBufferStart
    Define *RightEnd = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes
    Define.q EndPosition = *List\EndPosition
    If *RightStart < *RightEnd And ( EndPosition - PeekQ( *RightStart ) ) < Position
      
      Define.q Value = -1
      While #True
        
        Define *NewRightStart = *RightStart + SizeOf( Quad )
        If *NewRightStart >= *RightEnd
          If PeekQ( *RightStart ) = Position
            ProcedureReturn -1
          EndIf
          ProcedureReturn Value
        EndIf
        
        Define.q NewValue = EndPosition - PeekQ( *NewRightStart )
        If NewValue > Position
          ProcedureReturn Value
        EndIf
        
        *RightStart = *NewRightStart
        Value = NewValue
        
      Wend
      
    EndIf
    
    ; Check if left position in left buffer is the marker we're looking for.
    ; Means gap is right around the position we're looking at.
    If *LeftEnd >= *Buffer\LeftBuffer And PeekQ( *LeftEnd ) < Position And
       ( *RightStart >= *RightEnd Or ( EndPosition - PeekQ( *RightStart ) ) >= Position )
      ProcedureReturn PeekQ( *LeftEnd )
    EndIf
    
    ProcedureReturn -1
    
  EndProcedure
  
  ;............................................................................
  
  ; As text is inserted or deleted, the end position of the marker list (relative
  ; to which everything on the right side of the buffer is stored) shifts and
  ; thus needs to be updated.
  Procedure ShiftLastMarkerPosition( *List.TextMarkerList, Amount.q )
    
    NotImplemented( "ShiftLastMarkerPosition" )
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.s TextMarkerListDebugString( *List.TextMarkerList )
    
    Define.s Result = "L("
    Define.GapBuffer *Buffer = @*List\Buffer
    
    Define *Left = *Buffer\LeftBuffer
    Define *LeftEnd = *Buffer\LeftBufferEnd
    While *Left < *LeftEnd
      
      Define.q Position = PeekQ( *Left )
      If *Left <> *Buffer\LeftBuffer
        Result + " "
      EndIf
      Result + Str( Position )
      
      *Left + SizeOf( Quad )
      
    Wend
    
    Result + ") R("
    Define *Right = *Buffer\RightBufferStart
    Define *RightEnd = *Buffer\RightBuffer + *Buffer\RightBufferSizeInBytes
    While *Right < *RightEnd
      
      Define.q Position = PeekQ( *Right )
      If *Right <> *Buffer\RightBufferStart
        Result + " "
      EndIf
      Result + Str( Position )
      
      *Right + SizeOf( Quad )
    Wend
    
    Result + ") " + Str( *List\EndPosition )
    
    ProcedureReturn Result
    
  EndProcedure

EndModule

;..............................................................................

ProcedureUnit CanAddMarkersToList()

  UseModule GapBuffer
  UseModule TextMarker

  Define MarkerList.TextMarkerList
  InitializeTextMarkerList( @MarkerList )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 0 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = -1 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )
  
  AddMarkerToTextMarkerList( @MarkerList, 0 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 1 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = 0 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )
  
  AddMarkerToTextMarkerList( @MarkerList, 10 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 2 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = 0 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = 10 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )  
  
  AddMarkerToTextMarkerList( @MarkerList, 5 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 3 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = -1 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = 5 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = 5 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )  
  
  AddMarkerToTextMarkerList( @MarkerList, 15 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 4 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = 15 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = 5 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = 5 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )
  
  ; Add *another* marker at position 10.
  AddMarkerToTextMarkerList( @MarkerList, 10 )
  
  ;;REVIEW: ATM consecutive markers at the same position are ignored; may not be the most useful behavior
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 5 )
  Assert( GetNextMarkerPosition( @MarkerList, 10 ) = 15 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 10 ) = 5 )
  Assert( GetNextMarkerPosition( @MarkerList, 0 ) = 5 )
  Assert( GetPreviousMarkerPosition( @MarkerList, 0 ) = -1 )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 17
; Folding = ---
; EnableXP