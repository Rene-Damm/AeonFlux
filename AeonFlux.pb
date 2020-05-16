
ExamineDesktops()

Window = OpenWindow( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), "Aeon Flux", #PB_Window_BorderLess )
Canvas = CanvasGadget( #PB_Any, 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

Text.s = ""

Repeat
  Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
    Select EventType()
      Case #PB_EventType_KeyDown
        Debug "Yeah"
      Case #PB_EventType_Input
        Input = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
        Text = Text + Chr( Input )
    EndSelect
    
    If StartDrawing( CanvasOutput( Canvas ) )
      DrawText( 100, 100, Text )
      StopDrawing()
    EndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow

; IDE Options = PureBasic 5.50 (Windows - x64)
; CursorPosition = 18
; EnableXP