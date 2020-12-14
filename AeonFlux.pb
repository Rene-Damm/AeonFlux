; Aeon Flux
;
; Author: Rene Damm
; Started: 16-May-20

EnableExplicit

XIncludeFile "Utils.pb"
XIncludeFile "Math.pb"
XIncludeFile "GapBuffer.pb"         ; Memory.
XIncludeFile "TextMarker.pb"        ; Positions.
XIncludeFile "TextBuffer.pb"        ; Text.
XIncludeFile "TextEditor.pb"        ; Cursors, selections, undo.
XIncludeFile "Workspace.pb"         ; Blob collections.
XIncludeFile "Project.pb"           ; Source files.
XIncludeFile "Root.pb"              ; 
XIncludeFile "Shell.pb"             ; Modes, commands.
XIncludeFile "TextEditorShell.pb"   ; Text editing commands.

; NOTE: Unit tests can be debugged by simply invoking them here.

;TextRenderer (dirty rects and render buffers)
;Configuration/settings

;central event/message bus?

#VectorRendering = #False

ExamineDesktops()

Define CanvasWidth.i = DesktopWidth( 0 )
Define CanvasHeight.i = DesktopHeight( 0 )

Define TextColor.i = #Black
Define BackgroundColor.i = #White

; Create window.
Define Window.i = OpenWindow( #PB_Any, 0, 0, CanvasWidth, CanvasHeight, "Aeon Flux", #PB_Window_BorderLess )
Define Canvas.i = CanvasGadget( #PB_Any, 0, 0, CanvasWidth, CanvasHeight, #PB_Canvas_Keyboard )
SetActiveGadget( Canvas )

Define Font = LoadFont( #PB_Any, "Consolas", 16, #PB_Font_HighQuality )

;store text in memory as UTF-8 and use the same single set of temp strings (UTF-16) for rendering?

;==============================================================================

;do text rendering on thread? (and output as images)
;will eventually have to handle styling, annotations, etc

DeclareModule TextRenderBuffer
  
  Structure TextRenderBuffer
  EndStructure
  
EndDeclareModule

Module TextRenderBuffer
EndModule

;==============================================================================

UseModule Utils
UseModule Workspace

Define.s TestDataDirectory = GetHomeDirectory() + "Dropbox" + #PS$ + "Workspaces" + #PS$ + "AeonFlux_PureBasic" + #PS$ + "_Test"

;later: add two directories, one for where projects live by default and one for where workspace live by default
Define.s ProjectPath = TestDataDirectory + #PS$ + "FirstProject" + #PS$
Define.s WorkspacePath = TestDataDirectory + #PS$ + "FirstWorkspace" + #PS$

Define.IFileSystem *WorkspaceFS = CreateLocalFileSystem( WorkspacePath )
Define.Workspace *Workspace = AllocateStructure( Workspace )
LoadWorkspace( *Workspace, *WorkspaceFS )

;==============================================================================

UseModule Shell
UseModule TextBuffer
UseModule TextEditor
UseModule TextEditorShell

; Create shell.
Define.Shell Shell
CreateShell( @Shell )

;;TODO: this needs to be connected to a text blob
; Create text editor.
Define.TextEditorShell *TextEditor = CreateEditor( @Shell, SizeOf( TextEditorShell ), @CreateTextEditorShell() )

;==============================================================================

; Main loop.
Repeat
  
  Define Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget
    
    ; Input.
    Select EventType()
        
      Case #PB_EventType_KeyDown
        Define Key = GetGadgetAttribute( Canvas, #PB_Canvas_Key )
        Select Key
          Case #PB_Shortcut_Escape
            SendShellInput( @Shell, "<ESC>" )
          Case #PB_Shortcut_Back
            SendShellInput( @Shell, "<BS>" )
          Case #PB_Shortcut_Return
            SendShellInput( @Shell, "<CR>" )
        EndSelect
        
      Case #PB_EventType_Input
        ;;TODO: modifier keys (control, shift, alt)
        Define Character = GetGadgetAttribute( Canvas, #PB_Canvas_Input )
        ;;TODO: avoid string conversion here
        SendShellInput( @Shell, Chr( Character ) )
        
    EndSelect
    
    ; Draw.
    CompilerIf #VectorRendering = 1
      
      ;flickers (should redraw only what's dirty)
      ;vector drawing seems to be A LOT slower so nut sure I'll go further with this path
      
      If StartDrawing( CanvasOutput( Canvas ) )
        ; Clear canvas. Seems like this is the only method to do so...
        Box( 0, 0, DesktopWidth( 0 ), DesktopHeight( 0 ), #White )
        StopDrawing()
      EndIf
      If StartVectorDrawing( CanvasVectorOutput( Canvas, #PB_Unit_Pixel ) )
      	
        VectorFont( FontID( Font ) )
  
        Define TextWidthLeft = VectorTextWidth( TextBufferLeft )
        Define TextWidthRight = VectorTextWidth( TextBufferRight )
        Define TextWidthCursor = VectorTextWidth( TextBufferCursor )
        
        ; Draw portion of line left and right to the cursor.
        VectorSourceColor( RGBA( 0, 0, 0, 255 ) )
        ;BackColor( #White )
        ;FrontColor( #Black )
        MovePathCursor( 100, 100 )
        DrawVectorText( TextBufferLeft )
        MovePathCursor( 100 + TextWidthLeft + TextWidthCursor, 100 )
        DrawVectorText( TextBufferRight )
        
        ; Draw portion of line at cursor.
        ;BackColor( #Black )
        ;FrontColor( #White )
        MovePathCursor( 100 + TextWidthLeft, 100 )
        DrawVectorText( TextBufferCursor )
        
        StopVectorDrawing()
        
      EndIf
    
    CompilerElse
    
      ;REVIEW Drawing long strings seems to be super slow; this will ultimatly probably have to redraw as little as possible
      
      If StartDrawing( CanvasOutput( Canvas ) )
      	
        DrawingFont( FontID( Font ) )
        DrawingMode( #PB_2DDrawing_Default )
        
        Define.i X = 100
        Define.i Y = 100
        ;;TODO: support selections
        Define.q CursorLineNumber = GetCursorLineNumberFromTextEditor( @*TextEditor\Editor )
        Define.q CursorColumnNumber = GetCursorColumnNumberFromTextEditor( @*TextEditor\Editor )
        Define.i NumLines = GetTextBufferLineCount( @*TextEditor\Buffer )
        Define.i LineNumber
        Box( X, Y, CanvasWidth, ( TextHeight( " " ) + 10 ) * ( NumLines + 1 ), BackgroundColor ) ; +1 lines is a hacky way to clear cursor after deleting last line
        For LineNumber = 1 To NumLines
          
          ;;TODO: replace this with a cached renderer
          
          Define.s Line = ReadLineFromTextBuffer( @*TextEditor\Buffer, LineNumber )
          Define.i LineLength = Len( Line )
          Define.i TextHeight
          If LineLength > 0
            TextHeight = TextHeight( Line )
          Else
            TextHeight = TextHeight( " " )
          EndIf
          
          BackColor( BackgroundColor )
          FrontColor( TextColor )
          
          If LineNumber = CursorLineNumber
            
            Define.i XOffset = 0
            Define.i Mode = *TextEditor\Mode
            
            ; Draw left part.
            If CursorColumnNumber > 1
              
              Define.s TextLeft = Left( Line, CursorColumnNumber - 1 )
              DrawText( X + XOffset, Y, TextLeft )
              XOffset + TextWidth( TextLeft )
              
            EndIf
            
            ; Draw cursor part.
            Define.s TextCursor
            If CursorColumnNumber <= LineLength
              TextCursor = Mid( Line, CursorColumnNumber, 1 ) ; Indexing starts at 1!
            Else
              TextCursor = " "
            EndIf
            If Mode = #NormalMode
              BackColor( TextColor )
              FrontColor( BackgroundColor )
            EndIf
            DrawText( X + XOffset, Y, TextCursor )
            If Mode = #InsertMode
              Box( X + XOffset, Y, 4, TextHeight, TextColor )
            EndIf
            XOffset + TextWidth( TextCursor )
            
            BackColor( BackgroundColor )
            FrontColor( TextColor )
            
            ; Draw right part.
            If CursorColumnNumber <= ( LineLength - 1 )
              
              Define.s TextRight = Right( Line, LineLength - CursorColumnNumber )
              DrawText( X + XOffset, Y, TextRight )
              
            EndIf
            
          Else
            
            DrawText( X, Y, Line )
            
          EndIf
          
          Y + TextHeight + 10
                    
        Next LineNumber        
        
        StopDrawing()
        
      EndIf
    
    CompilerEndIf
    
  EndIf
  
Until Event = #PB_Event_CloseWindow

;[X] Can delete range in TextBuffer
;[X] Can backspace in editor shell
;[X] Switch main over to using new text editing backend


;[X] TODO Render cursor (block)
;[X] TODO Move cursor with H and L (vertical movement will have to wait until we have multiple lines)
;[X] TODO Use fixed-width font
;[X] TODO Switch to insert mode and back
;[X] TODO Insert characters
;[X] TODO Start with empty buffer
;[X] TODO Backspace
;[X] TODO Use gap buffer
;[X] TODO Line markers
;[X] TODO Add second line
;[ ] TODO Delete line
;[ ] TODO Save text
;[ ] TODO Restore text on startup
;[ ] TODO Compose project

;...
;[ ] TODO Input run through command interpreter 
;...
;[ ] TODO Text is styled
;...
;[ ] TODO Move around node with keyboard
;[ ] TODO Move around node with mouse
;...
;[ ] TODO Run builder on program
;[ ] TODO Run runner on program
;...
;[ ] TODO Undo edit
;...
;[ ] TODO Redraw only changed portion of the text (dirty rectangles/lines)
;...
;[ ] TODO Switch to command mode

;requirements:
;- memory buffer and string manipulation
;- windowing
;- high-quality text rendering
;- Mac and Windows
;- fully integrated IDE
;- native code executables
;- unicode support (preferably UTF8)
;- rich library
;- unit testing
;- ideal: vector drawing
;- ideal: hotreload

;PB problems:
;- no aliased font rendering? -> ugly font (SOLVED: switched to vector drawing AND Windows high-DPI support)
;- no substring support (not a dealbreaker)
;- no exceptions, asserts, or any other "normal" error handling support (only OnError for hard crashes)

;can mutate text in String
;but cannot create a substring without copying and cannot render a portion of a String only
;can truncate a string by writing a NUL character to memory
; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 20
; Folding = -
; EnableXP
; HideErrorLog