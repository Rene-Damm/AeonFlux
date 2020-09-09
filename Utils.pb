EnableExplicit

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
    Destroy()
    OpenFile.q( Path.s, Flags.i = 0 )
    CreateFile.q( Path.s, Flags.i = 0 )
    CloseFile( Handle.q )
    ReadFile.q( Handle.q, Position.q, Size.q, *Buffer )
    WriteFile( Handle.q, Position.q, Size.q, *Buffer )
    TruncateFile( Handle.q, Position.q )
    FlushFileBuffers( Handle.q )
    DeleteFile.i( Path.s )
    GetFileSize.q( Path.s )
    ListFiles.i( Path.s, Pattern.s, Array Files.s( 1 ) )
    ListDirectories.i( Path.s, Pattern.s, Array Directories.s( 1 ) )
  EndInterface
  
  Prototype.i CompareFn               ( *Left, *Right )
  
  Declare.q Min                       ( A.q, B.q )
  Declare.q Max                       ( A.q, B.q )
  Declare.q AbsQ                      ( Number.q )
  Declare.q AlignToMultipleOf         ( Number.q, Alignment.q )
  Declare.i CompareFnQ                ( *Left, *Right )
  Declare.i StartsWith                ( Prefix.s, String.s )
  Declare.i StringEqual               ( String1.s, String2.s, NumChars.i )
  Declare   NotImplemented            ( Message.s )
  
  Declare.q ArrayAppendWithCapacity   ( *Ptr, *Count, *Element, SizeOfElementsInBytes.i, Increment.i = 32 )
  Declare   ArrayEraseAtWithCapacity  ( *Ptr, *Count, Index.q, SizeOfElementsInBytes.i )
  
  Declare.s StringAppendChars         ( Buffer.s, *BufferLength, *BufferCapacity, *Chars, NumChars.i )
  Declare.q FindStringInArray         ( Array Strings.s( 1 ), String.s )
  
  Declare.q CreateLocalFileSystem     ( RootPath.s )
  Declare.q CreateVirtualFileSystem   ()
  
  Declare.s ReadStringFromFile        ( *FileSystem.IFileSystem, FileHandle.q, Position.q = 0, Length = -1, Flags.i = 0 )
  Declare   WriteStringToFile         ( *FileSystem.IFileSystem, FileHandle.q, Position.q, String.s, Index.i = -1, NumChars = -1, Flags.i = 0 )
  
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
  
  Procedure.q AbsQ( Number.q )
    
    If Number >= 0
      ProcedureReturn Number
    EndIf
    
    ProcedureReturn Number * -1
    
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
  
  Procedure.i StringEqual( String1.s, String2.s, NumChars.i )
    
    If NumChars = 0
      ProcedureReturn #True
    EndIf
    
    Define.i Len1 = Len( String1 )
    If Len1 < NumChars
      ProcedureReturn #False
    EndIf
    
    Define.i Len2 = Len( String2 )
    If Len2 < NumChars
      ProcedureReturn #False
    EndIf
    
    Define *Ptr1 = @String1
    Define *Ptr2 = @String2
    
    Define.i Index
    For Index = 0 To NumChars - 1
      
      Define.c Char1 = PeekC( *Ptr1 + Index * SizeOf( Character ) )
      Define.c Char2 = PeekC( *Ptr2 + Index * SizeOf( Character ) )
      
      If Char1 <> Char2
        ProcedureReturn #False
      EndIf
      
    Next
    
    ProcedureReturn #True
    
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
  
  ;............................................................................
  
  Structure LocalFileSystem
    *Methods
    RootPath.s
  EndStructure
  
  Procedure.q CreateLocalFileSystem( RootPath.s )
    NotImplemented( "CreateVirtualFileSystem" )
  EndProcedure
  
  ;............................................................................
  
  EnumerationBinary
    #VirtualFile_Deleted
    #VirtualFile_IsOpen
  EndEnumeration
  
  Structure VirtualFile
    Flags.i
    Path.s
    PathLowerCase.s
    Size.q
    *Contents
  EndStructure
  
  Structure VirtualFileSystem
    *Methods
    Array Files.VirtualFile( 1 )
  EndStructure
  
  Procedure.q CreateVirtualFileSystem()
    
    Define.VirtualFileSystem *VFS = AllocateStructure( VirtualFileSystem )
    
    *VFS\Methods = ?VirtualFileSystem_VTable
    
    ; PureBasic always allocates arrays with a single element :(
    *VFS\Files( 0 )\Flags = #VirtualFile_Deleted
    
    ProcedureReturn *VFS
    
  EndProcedure
  
  Procedure VFS_Destroy( *VFS.VirtualFileSystem )
    
    FreeStructure( *VFS )
    
  EndProcedure
  
  Procedure.q VFS_OpenFile( *VFS.VirtualFileSystem, Path.s, Flags.i )
  EndProcedure
  
  Procedure.q VFS_CreateFile( *VFS.VirtualFileSystem, Path.s, Flags.i )
    
    DebugAssert( *VFS <> #Null )
    
    ;;OPTIMIZE: this can be optimized by keeping deleted files on a list (storing the index in the structure)
    ; Find unused slot.
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.i Index = 0
    For Index = 0 To NumFiles - 1
      If *VFS\Files( Index )\Flags & #VirtualFile_Deleted
        Break
      EndIf
    Next
    If Index = NumFiles
      ReDim *VFS\Files( NumFiles + 1 )
    EndIf
    
    *VFS\Files( Index )\Path = Path
    *VFS\Files( Index )\PathLowerCase = LCase( Path )
    *VFS\Files( Index )\Flags = #VirtualFile_IsOpen
    
    ProcedureReturn Index
    
  EndProcedure
  
  Procedure VFS_CloseFile( *VFS.VirtualFileSystem, Handle.q )
  EndProcedure
  
  Procedure.q VFS_ReadFile( *VFS.VirtualFileSystem, Handle.q, Position.q, Size.q, *Buffer )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle >= 0 )
    DebugAssert( Handle < ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    DebugAssert( *Buffer <> #Null )
    DebugAssert( *VFS\Files( Handle )\Flags & #VirtualFile_IsOpen )
    
    Define.q FileSize = *VFS\Files( Handle )\Size
    If Position + Size > FileSize
      Size = FileSize - Position
    EndIf
    
    Define *FileContents = *VFS\Files( Handle )\Contents
    CopyMemory( *FileContents + Position, *Buffer, Size )
    
    ProcedureReturn Size
    
  EndProcedure
  
  Procedure VFS_WriteFile( *VFS.VirtualFileSystem, Handle.q, Position.q, Size.q, *Buffer )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle >= 0 )
    DebugAssert( Handle < ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    DebugAssert( *Buffer <> #Null )
    DebugAssert( *VFS\Files( Handle )\Flags & #VirtualFile_IsOpen )
    
    ; Enlarge memory, if necessary.
    Define.q Capacity = 0
    Define *Contents = *VFS\Files( Handle )\Contents
    If *Contents <> #Null
      Capacity = MemorySize( *Contents )
    EndIf
    If Position + Size > Capacity
      Capacity = AlignToMultipleOf( Max( Capacity + 1024, Position + Size ), 1024 )
      *Contents = ReAllocateMemory( *Contents, Capacity, #PB_Memory_NoClear )
      *VFS\Files( Handle )\Contents = *Contents
    EndIf
    
    ; Copy.
    CopyMemory( *Buffer, *Contents + Position, Size )
    
    ; Update size.
    If *VFS\Files( Handle )\Size < Position + Size
      *VFS\Files( Handle )\Size = Position + Size
    EndIf
    
  EndProcedure
  
  Procedure VFS_TruncateFile( *VFS.VirtualFileSystem, Handle.q, Position.q )
    NotImplemented( "VFS_TruncateFile" )
  EndProcedure
  
  Procedure VFS_FlushFileBuffers( *VFS.VirtualFileSystem, Handle.q )
    ; Nothing to do.
  EndProcedure
  
  Procedure.i VFS_DeleteFile( *VFS.VirtualFileSystem, Path.s )
    
    DebugAssert( *VFS <> #Null )
    
    Define.s PathLowerCase = LCase( Path )
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.i Index
    For Index = 0 To NumFiles - 1
      If *VFS\Files( Index )\PathLowerCase = PathLowerCase
        ;;TODO: free memory buffer
        ClearStructure( *VFS\Files( Index ), VirtualFile )
        *VFS\Files( Index )\Flags = #VirtualFile_Deleted
        ProcedureReturn #True
      EndIf
    Next
    
    ProcedureReturn #False
    
  EndProcedure
  
  Procedure.q VFS_GetFileSize( *VFS.VirtualFileSystem, Path.s )
    
    DebugAssert( *VFS <> #Null )
    
    Define.s PathLowerCase = LCase( Path )
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.i Index
    For Index = 0 To NumFiles - 1
      
      If *VFS\Files( Index )\PathLowerCase = PathLowerCase
        ProcedureReturn *VFS\Files( Index )\Size
      EndIf
            
    Next
    
    ProcedureReturn -1
    
  EndProcedure
  
  Procedure.i VFS_ListFiles( *VFS.VirtualFileSystem, Path.s, Pattern.s, Array Files.s( 1 ) )
    
    DebugAssert( *VFS <> #Null )
    
    Define.i PathLen = Len( Path )
    Define.s PathLowerCase = LCase( Path )
    Define.i Count = 0
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.VirtualFile *File = @*VFS\Files()
    
    Define.i Index
    For Index = 0 To NumFiles - 1
      
      If *File\Flags & #VirtualFile_Deleted
        *File + SizeOf( VirtualFile )
        Continue
      EndIf
            
      ; Skip file if path doesn't match.
      Define.s FilePathLowerCase = *File\PathLowerCase
      Define.i LenFilePathLowerCase = Len( FilePathLowerCase )
      If PathLen > LenFilePathLowerCase Or Not StringEqual( PathLowerCase, FilePathLowerCase, PathLen ) Or
         ( LenFilePathLowerCase > PathLen And PathLen > 0 And PeekQ( @FilePathLowerCase + PathLen * SizeOf( Character ) ) <> #PS )
        *File + SizeOf( VirtualFile )
        Continue
      EndIf
      
      ;;TODO: check match against pattern
      
      ; Check capacity.
      If Capacity <= Count
        Capacity + 16
        ReDim Files( Capacity )
      EndIf
      
      ; Add file.
      Files( Count ) = *File\Path
      Count + 1
      
      *File + SizeOf( VirtualFile )
      
    Next
    
    ProcedureReturn Count
    
  EndProcedure
  
  Procedure.i VFS_ListDirectories( *VFS.VirtualFileSystem, Path.s, Pattern.s, Array Directories.s( 1 ) )
    NotImplemented( "VFS_ListDirectories" )
  EndProcedure
  
  DataSection
    
    VirtualFileSystem_VTable:
      Data.q @VFS_Destroy()
      Data.q @VFS_OpenFile()
      Data.q @VFS_CreateFile()
      Data.q @VFS_CloseFile()
      Data.q @VFS_ReadFile()
      Data.q @VFS_WriteFile()
      Data.q @VFS_TruncateFile()
      Data.q @VFS_FlushFileBuffers()
      Data.q @VFS_DeleteFile()
      Data.q @VFS_GetFileSize()
      Data.q @VFS_ListFiles()
      Data.q @VFS_ListDirectories()
    
  EndDataSection
  
  ;............................................................................
  
  Procedure.s ReadStringFromFile( *FileSystem.IFileSystem, FileHandle.q, Position.q = 0, Length = -1, Flags.i = 0 )
    NotImplemented( "ReadStringFromFile" )
  EndProcedure
  
  ;............................................................................
  
  Procedure WriteStringToFile( *FileSystem.IFileSystem, FileHandle.q, Position.q, String.s, Index.i = -1, NumChars = -1, Flags.i = 0 )
    NotImplemented( "WriteStringToFile" )
  EndProcedure
  
EndModule

;..............................................................................

ProcedureUnit CanCreateVirtualFileSystem()

  UseModule Utils

  Define.IFileSystem *VFS = CreateVirtualFileSystem()
  
  Dim Files.s( 1 )
  
  Assert( *VFS\ListFiles( "", "", Files() ) = 0 )
  Assert( *VFS\ListFiles( "DoesNotExist", "", Files() ) = 0 )
  
  Define.q First = *VFS\CreateFile( "first" )
  Define.q Second = *VFS\CreateFile( "second" )
  
  Assert( *VFS\ListFiles( "", "", Files() ) = 2 )
  
  Assert( ArraySize( Files() ) >= 2 )
  Assert( Files( 0 ) = "first" )
  Assert( Files( 1 ) = "second" )
  
  *VFS\WriteFile( First, 0, 5 * SizeOf( Character ), @"First" )
  *VFS\WriteFile( Second, 0, 6 * SizeOf( Character ), @"Second" )
  
  Assert( *VFS\GetFileSize( "first" ) = 5 * SizeOf( Character ) )
  Assert( *VFS\GetFileSize( "second" ) = 6 * SizeOf( Character ) )
  
  *VFS\WriteFile( First, 4 * SizeOf( Character ), 5 * SizeOf( Character ), @"Third" )
  
  Assert( *VFS\GetFileSize( "first" ) = 9 * SizeOf( Character ) )
  Assert( *VFS\GetFileSize( "second" ) = 6 * SizeOf( Character ) )
  
  Define.s FirstContents = Space( 9 )
  Define.s SecondContents = Space( 6 )
  
  Assert( *VFS\ReadFile( First, 0, 9 * SizeOf( Character ), @FirstContents ) = 9 * SizeOf( Character ) )
  Assert( *VFS\ReadFile( Second, 0, 6 * SizeOf( Character ), @SecondContents ) = 6 * SizeOf( Character ) )
  
  Assert( FirstContents = "FirsThird" )
  Assert( SecondContents = "Second" )
  
  Assert( *VFS\ReadFile( First, 2 * SizeOf( Character ), 2 * SizeOf( Character ), @FirstContents ) = 2 * SizeOf( Character ) )
  
  Assert( FirstContents = "rsrsThird" )
  
  Assert( *VFS\ReadFile( First, 6 * SizeOf( Character ), 5 * SizeOf( Character ), @FirstContents ) = 3 * SizeOf( Character ) )
  
  Assert( FirstContents = "irdsThird" )
  
  *VFS\DeleteFile( "first" )
  
  Assert( *VFS\GetFileSize( "first" ) = -1 )
  
  Assert( *VFS\ListFiles( "", "", Files() ) = 1 )
  Assert( Files( 0 ) = "second" )
  
  ;;TODO: close and reopen second file
  ;;TODO: create new file
  
  *VFS\Destroy()

EndProcedureUnit

; IDE Options = PureBasic 5.72 (Windows - x64)
; CursorPosition = 436
; FirstLine = 418
; Folding = ------
; EnableXP