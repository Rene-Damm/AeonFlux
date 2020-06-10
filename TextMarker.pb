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
  Structure TextMarkerList
    Buffer.GapBuffer
    EndPosition.q
  EndStructure
  
  ;............................................................................
  
  Declare   InitializeTextMarkerList( *List.TextMarkerList )
  Declare   DestroyTextMarkerList( *List.TextMarkerList )
  Declare.q GetTextMarkerListLength( *List.TextMarkerList )
  Declare   AddMarkerToTextMarkerList( *List.TextMarkerList, Position.q )
  CompilerIf #False
  CompilerEndIf
  
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
    
    Define.q LeftPosition = GetPositionLeftOfGap( *List )
    Define.q RightPosition = GetPositionRightOfGap( *List )
    
    ; Move gap, if necessary.
    If Position < LeftPosition
      
      ; Move gap to left.
      
    ElseIf Position > RightPosition
      
      ; Move gap to right.
      
    EndIf
    
    ; Insert marker.
    WriteIntoGapBuffer( *List\Buffer, @Position, 1 )
    If RightPosition = #MAX_QUAD
      *List\EndPosition = *Position
    EndIf
    
  EndProcedure
  
EndModule

ProcedureUnit CanAddMarkersToList()

  UseModule TextMarker

  Define MarkerList.TextMarkerList
  InitializeTextMarkerList( @MarkerList )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 0 )
  
  AddMarkerToTextMarkerList( @MarkerList, 0 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 1 )
  
  AddMarkerToTextMarkerList( @MarkerList, 10 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 2 )
  
  AddMarkerToTextMarkerList( @MarkerList, 5 )
  
  Assert( GetTextMarkerListLength( @MarkerList ) = 3 )
  
EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 113
; FirstLine = 80
; Folding = --
; EnableXP