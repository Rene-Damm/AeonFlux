EnableExplicit

DeclareModule Utils
  
  #MIN_QUAD = -9223372036854775808
  #MAX_QUAD = 9223372036854775807
  
  #SPACE = 32
  #NEWLINE = 10
  #RETURN = 13
  #EQUALS = 61
  
  ;............................................................................
  
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
  
  ;............................................................................

  ;;REVIEW: file systems probably need to be made thread-safe
  
  ;have an IFile??
  
  EnumerationBinary
    #FileTruncate     ; When writing, set file length to position plus length of bytes written.
  EndEnumeration
  
  ; File systems contain only files (no directories; these are represented as separate file systems).
  ; File paths can contain separators (for local file systems, that will make the files go into subdirectories).
  Interface IFileSystem
    Destroy()
    OpenFile.q( Path.s, Flags.i = 0 )
    CreateFile.q( Path.s, Flags.i = 0 )
    CloseFile( Handle.q )
    ReadFile.q( Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    WriteFile( Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    ReadString.s( Handle.q, Position.q = 0, Length.i = -1, Flags.i = 0 )
    WriteString( Handle.q, String.s, Position.q = 0, Flags.i = 0 )
    SetFileSize( Handle.q, Size.q )
    SizeFile.q( Handle.q )
    ;;REVIEW: give file mappings their own handle? or do away with UnmapFile and require MapFile() to be called each time and treat it like a cache?
    MapFile.q( Handle.q, Position.q = 0, Size.q = -1 )
    UnmapFile.q( Handle.q )
    FlushFileBuffers( Handle.q )
    DeleteFile.i( Path.s )
    GetFileSize.q( Path.s )
    FileExists.i( Path.s )
    ;;REVIEW: collapse Path argument into Pattern argument?
    ListFiles.i( Path.s, Pattern.s, Array Files.s( 1 ) )
  EndInterface
  
  Interface IDirectorySystem
    Destroy()
    CreateDirectory.q( Path.s ) ; Returns an IFileSystem.
    OpenDirectory.q( Path.s ) ; Returns an IFileSystem.
    DeleteDirectory( Path.s )
    DirectoryExists( Path.s )
    ListDirectories.i( Path.s, Pattern.s, Array Directories.s( 1 ) )
  EndInterface
  
  Prototype.q CreateDirectoryFn         ( Path.s )
  
  EnumerationBinary
    #JobDoesIO
  EndEnumeration
  
  Prototype   JobFn                     ( *Job )
  
  Structure Job
    Name.s
    Flags.i
    ;;TODO: dependencies
    *Data
    *JobThreadFunc.JobFn
    *MainThreadFunc.JobFn
    *CleanUpFunc.JobFn
  EndStructure
  
  Prototype.i CompareFn                 ( *Left, *Right )
  
  ;............................................................................
  
  Declare.q Min                         ( A.q, B.q )
  Declare.q Max                         ( A.q, B.q )
  Declare.q AbsQ                        ( Number.q )
  Declare.q AlignToMultipleOf           ( Number.q, Alignment.q )
  Declare.i CompareFnQ                  ( *Left, *Right )
  Declare.i StartsWith                  ( Prefix.s, String.s )
  Declare.i EndsWith                    ( Suffix.s, String.s )
  Declare.i StringEqual                 ( String1.s, String2.s, NumChars.i )
  Declare   NotImplemented              ( Message.s )
  Declare.i IsWhitespace                ( Character.c )
  
  Declare.q ArrayAppendWithCapacity     ( *Ptr, *Count, *Element, SizeOfElementsInBytes.i, Increment.i = 32 )
  Declare   ArrayEraseAtWithCapacity    ( *Ptr, *Count, Index.q, SizeOfElementsInBytes.i )
  Declare.q ArrayIndexOf                ( *Array, Count, *Element, SizeOfElementsInBytes.i )
  
  Declare.s StringAppendChars           ( Buffer.s, *BufferLength, *BufferCapacity, *Chars, NumChars.i )
  Declare.q FindStringInArray           ( Array Strings.s( 1 ), String.s )
  
  Declare.q CreateLocalFileSystem       ( RootPath.s )
  Declare.q CreateVirtualFileSystem     ()
  
  Declare.q CreateLocalDirectorySystem  ( Path.s )
  Declare.q CreateVirtualDirectorySystem()
  
  Declare.s ReadTextFile                ( *FileSystem.IFileSystem, Path.s )
  
  Declare.q RunJob                      ( *Job.Job )
  Declare   ExecuteMainThreadJobFuncs   ()
  
  Declare.s GenerateGUID                ()
  
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
  
  Procedure.i EndsWith( Suffix.s, String.s )
    
    Define.i SuffixLen = Len( Suffix )
    If SuffixLen = 0
      ProcedureReturn #True
    EndIf
    
    Define.i StringLen = Len( String )
    If SuffixLen > StringLen
      ProcedureReturn #False
    EndIf
    
    Define *SuffixPtr = @Suffix + ( SuffixLen - 1 ) * SizeOf( Character )
    Define *StringPtr = @String + ( StringLen - 1 ) * SizeOf( Character )
    
    While #True
      
      Define.c SuffixChar = PeekC( *SuffixPtr )
      Define.c StringChar = PeekC( *StringPtr )
      If SuffixChar <> StringChar
        ProcedureReturn #False
      EndIf
      
      SuffixLen - 1
      If SuffixLen = 0
        ProcedureReturn #True
      EndIf
      
      *SuffixPtr - SizeOf( Character )
      *StringPtr - SizeOf( Character )
      
    Wend
    
    ProcedureReturn #False
    
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
  
  Procedure.i IsWhitespace( Character.c )
    
    Select Character
      Case #TAB, #SPACE, #NEWLINE, #RETURN
        ProcedureReturn #True
    EndSelect
    
    ProcedureReturn #False
    
  EndProcedure
  
  ;............................................................................
  
  Procedure NotImplemented( Message.s )
    
    Debug( "Not implemented: " + Message )
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
  
  Procedure.q ArrayIndexOf( *Array, Count, *Element, SizeOfElementsInBytes.i )
    
    If *Array = #Null Or Count = 0
      ProcedureReturn -1
    EndIf
    
    Define.q Index
    Define *Ptr = *Array
    For Index = 0 To Count - 1
      If CompareMemory( *Ptr, *Element, SizeOfElementsInBytes )
        ProcedureReturn Index
      EndIf
      *Ptr + SizeOfElementsInBytes
    Next
    
    ProcedureReturn -1
    
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
  
  ;;TODO: support for read-only files
  
  #MAX_OPEN_HANDLES = 32
  
  Structure LocalFile
    Path.s ; In original casing.
    HandleIndex.i ; -1 if invalid.
    OpenCount.i
  EndStructure
  
  Structure LocalFileHandle
    Handle.i
    Timestamp.q
    *File.LocalFile
  EndStructure
  
  Structure LocalFileSystem
    
    *Methods
    RootPath.s ; Ends in path separator.
    
    Map Files.q() ; By path (lowercased).
    Array Handles.LocalFileHandle( #MAX_OPEN_HANDLES )
    
    ;;REVIEW: Put tree structure into a graph? (will keep file system structure available at all times)
    
  EndStructure
  
  CompilerIf #False
  Procedure.q MakeFileHandle( Version.i, Index.i )
    ProcedureReturn ( Version << 32 ) & FileNumber
  EndProcedure
  
  Procedure.i GetIndexFromFileHandle( FileHandle.q )
    ProcedureReturn FileHandle & $FFFFFFFF
  EndProcedure
  
  Procedure.i GetVersionFromFileHandle( FileHandle.q )
    ProcedureReturn FileHandle >> 32
  EndProcedure
  CompilerEndIf

  Procedure ClaimFileHandleIndex( *LFS.LocalFileSystem, *File.LocalFile )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( *File <> #Null )
    
    If *File\HandleIndex <> -1
      ProcedureReturn
    EndIf
    
    Define.i Index = 0
    Define.q LeastRecentlyUsedEntryTimestamp = 0
    Define.i IndexOfLeastRecentlyUsedEntry = -1
    
    ; Grab unused or least recently used entry.
    For Index = 0 To #MAX_OPEN_HANDLES - 1
      If *LFS\Handles( Index )\File = #Null
        ; Unused entry.
        Break
      EndIf
      
      ; Keep track of least recently used entry.
      Define.q Timestamp = *LFS\Handles( Index )\Timestamp
      If Timestamp < LeastRecentlyUsedEntryTimestamp
        IndexOfLeastRecentlyUsedEntry = Index
        LeastRecentlyUsedEntryTimestamp = Timestamp
      EndIf
    Next
    
    If Index > #MAX_OPEN_HANDLES - 1
      ; Close least recently used file.
      Index = IndexOfLeastRecentlyUsedEntry
      *LFS\Handles( Index )\File\HandleIndex = -1
      CloseFile( *LFS\Handles( Index )\Handle )
      *LFS\Handles( Index )\Handle = 0
    EndIf
    
    ; Open file.
    Define.s FullPath = *LFS\RootPath + *File\Path
    *LFS\Handles( Index )\Handle = OpenFile( #PB_Any, FullPath )
    *File\HandleIndex = Index
    
  EndProcedure
  
  Procedure.q CreateLocalFileSystem( RootPath.s )
    
    If Len( RootPath ) = 0
      RootPath = GetCurrentDirectory()
    EndIf
    
    If Not EndsWith( RootPath, #PS$ )
      RootPath + #PS$
    EndIf
        
    Define.LocalFileSystem *LFS = AllocateStructure( LocalFileSystem )
    
    *LFS\Methods = ?LocalFileSystem_VTable
    *LFS\RootPath = RootPath
        
    ProcedureReturn *LFS
    
  EndProcedure
  
  Procedure LFS_Destroy( *LFS.LocalFileSystem )
    
    ; Close files.
    Define.i NumFiles = ArraySize( *LFS\Handles() )
    If NumFiles > 0
      Define.i Index
      For Index = 0 To NumFiles - 1
        Define.i Handle = *LFS\Handles( Index )\Handle
        If Handle <> 0
          CloseFile( Handle )
        EndIf
      Next
    EndIf
    
    FreeStructure( *LFS )
    
  EndProcedure
  
  Procedure.q LFS_OpenFile( *LFS.LocalFileSystem, Path.s, Flags.i )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    ;;TODO: needs to protect against the file disappearing on disk after we added it to the map
    
    ; See if we already have it in the map.
    Define.s PathLowerCase = LCase( Path )
    Define.LocalFile *File = FindMapElement( *LFS\Files(), PathLowerCase )
    If *File = #Null
      
      ; No. Check if it exists. If so, add it.
      
      Define.s FullPath = *LFS\RootPath + Path
      If FileSize( FullPath ) < 0
        ProcedureReturn 0
      EndIf
      
      *File = AllocateStructure( LocalFile )
      *File\Path = Path
      *File\HandleIndex = -1
      
      *Entry = AddMapElement( *LFS\Files(), PathLowerCase )
      PokeQ( *Entry, *File )
      
    EndIf
    
    *File\OpenCount + 1
    ProcedureReturn *File
        
  EndProcedure
  
  Procedure.q LFS_CreateFile( *LFS.LocalFileSystem, Path.s, Flags.i )
    
    DebugAssert( *LFS <> #Null )
    
    ; Make sure it doesn't already exist.
    Define.s PathLowerCase = LCase( Path )
    Define.LocalFile *File = FindMapElement( *LFS\Files(), PathLowerCase )
    If *File = #Null
      
      *File = AllocateStructure( LocalFile )
      *File\Path = Path
      *File\HandleIndex = -1
      
      *Entry = AddMapElement( *LFS\Files(), PathLowerCase )
      PokeQ( *Entry, *File )
      
    EndIf
    
    ; Add it to the map.
    *File\OpenCount + 1
    
    ;;REVIEW: should this also create a file on disk right away?
    
    ProcedureReturn *File
    
  EndProcedure
  
  Procedure LFS_CloseFile( *LFS.LocalFileSystem, Handle.q )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    
    Define.LocalFile *File = Handle
    If *File\OpenCount > 0
      *File\OpenCount - 1
    EndIf
    If *File\OpenCount = 0
      If *File\HandleIndex <> -1
        CloseFile( *LFS\Handles( *File\HandleIndex )\Handle )
        *LFS\Handles( *File\HandleIndex )\File = #Null
        *LFS\Handles( *File\HandleIndex )\Handle = 0
      EndIf
      DeleteMapElement( *LFS\Files(), LCase( *File\Path ) )
      FreeStructure( *File )
    EndIf
    
  EndProcedure
  
  Procedure.q LFS_ReadFile( *LFS.LocalFileSystem, Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    DebugAssert( *Buffer <> #Null )
    
    Define.LocalFile *File = Handle
    DebugAssert( *File\OpenCount > 0 )
    
    ; Claim active handle, if we don't have one ATM.
    ClaimFileHandleIndex( *LFS, *File )
    
    ; Read.
    Define.q File = *LFS\Handles( *File\HandleIndex )\Handle
    FileSeek( File, Position, #PB_Absolute )
    ProcedureReturn ReadData( File, *Buffer, Size )
    
  EndProcedure
  
  Procedure LFS_WriteFile( *LFS.LocalFileSystem, Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    DebugAssert( *Buffer <> #Null )
    
    Define.LocalFile *File = Handle
    DebugAssert( *File\OpenCount > 0 )
    
    ; Claim active handle, if we don't have one ATM.
    ClaimFileHandleIndex( *LFS, *File )
    
    ; Write.
    Define.q File = *LFS\Handles( *File\HandleIndex )\Handle
    FileSeek( File, Position, #PB_Absolute )
    WriteData( File, *Buffer, Size )
    
    If Flags & #FileTruncate
      TruncateFile( File )
    EndIf
    
  EndProcedure
  
  Procedure.s LFS_ReadString( *LFS.LocalFileSystem, Handle.q, Position.q = 0, Length.q = -1, Flags.i = 0 )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    DebugAssert( Position >= 0 )
    DebugAssert( Length >= 0 Or Length = -1 )
    
    Define.LocalFile *File = Handle
    DebugAssert( *File\OpenCount > 0 )
    
    ; Claim active handle, if we don't have one ATM.
    ClaimFileHandleIndex( *LFS, *File )
    
    ; Read.
    Define.q File = *LFS\Handles( *File\HandleIndex )\Handle
    FileSeek( File, Position, #PB_Absolute )
    If Length < 0
      ProcedureReturn ReadString( File, #PB_UTF8 | #PB_File_IgnoreEOL )
    Else
      ProcedureReturn ReadString( File, #PB_UTF8 | #PB_File_IgnoreEOL, Length )
    EndIf
    
  EndProcedure
  
  Procedure LFS_WriteString( *LFS.LocalFileSystem, Handle.q, String.s, Position.q = 0, Flags.i = 0 )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    DebugAssert( Position >= 0 )
    
    Define.LocalFile *File = Handle
    DebugAssert( *File\OpenCount > 0 )
    
    ; Claim active handle, if we don't have one ATM.
    ClaimFileHandleIndex( *LFS, *File )
    
    ; Write.
    Define.q File = *LFS\Handles( *File\HandleIndex )\Handle
    FileSeek( File, Position, #PB_Absolute )
    WriteString( File, String, #PB_UTF8 )
    
    If Flags & #FileTruncate
      TruncateFile( File )
    EndIf
    
  EndProcedure
  
  Procedure LFS_SetFileSize( *LFS.LocalFileSystem, Handle.q, Size.q )
    
    DebugAssert( *LFS <> #Null )
    DebugAssert( Handle <> 0 )
    DebugAssert( Size >= 0 )
    
    Define.LocalFile *File = Handle
    DebugAssert( *File\OpenCount > 0 )
    
    ; Claim active handle, if we don't have one ATM.
    ClaimFileHandleIndex( *LFS, *File )
    
    ; Truncate.
    Define.q File = *LFS\Handles( *File\HandleIndex )\Handle
    FileSeek( File, Size, #PB_Absolute )
    TruncateFile( File )
    
  EndProcedure
  
  Procedure.q LFS_SizeFile( *LFS.LocalFileSystem, Handle.q )
    
    DebugAssert( *LFS <> #Null )
    NotImplemented( "LFS_SizeFile" )
    
  EndProcedure
  
  Procedure.q LFS_MapFile( *LFS.LocalFileSystem, Handle.q, Position.q = 0, Size.q = -1 )
    NotImplemented( "LFS_MapFile" )
  EndProcedure
  
  Procedure LFS_UnmapFile( *LFS.LocalFileSystem, Handle.q )
    NotImplemented( "LFS_UnmapFile" )
  EndProcedure
  
  Procedure LFS_FlushFileBuffers( *LFS.LocalFileSystem, Handle.q )
    ; Nothing to do.
  EndProcedure
  
  Procedure.i LFS_DeleteFile( *LFS.LocalFileSystem, Path.s )
    
    DebugAssert( *LFS <> #Null )
    NotImplemented( "LFS_DeleteFile" )
    
  EndProcedure
  
  Procedure.q LFS_GetFileSize( *LFS.LocalFileSystem, Path.s )
    
    DebugAssert( *LFS <> #Null )
    
    Define.s FullPath = *LFS\RootPath + Path
    ProcedureReturn FileSize( FullPath )
        
  EndProcedure
  
  Procedure.i LFS_FileExists( *LFS.LocalFileSystem, Path.s )
    
    DebugAssert( *LFS <> #Null )
    
    Define.s PathLowerCase = LCase( Path )
    Define.LocalFile *File = FindMapElement( *LFS\Files(), PathLowerCase )
    If *File <> #Null
      ProcedureReturn #True
    EndIf
          
    Define.s FullPath = *LFS\RootPath + Path
    If FileSize( FullPath ) >= 0
      ProcedureReturn #True
    EndIf    
    
    ProcedureReturn #False
        
  EndProcedure
  
  Procedure.i LFS_ListFiles( *LFS.LocalFileSystem, Path.s, Pattern.s, Array Files.s( 1 ) )
    
    DebugAssert( *LFS <> #Null )
    
    ; List contents of directory.
    Define.s FullPath = *LFS\RootPath + Path
    Define.q Directory = ExamineDirectory( #PB_Any, FullPath, "" )
    If Directory = 0
      ProcedureReturn 0
    EndIf
    
    ; Make sure path is terminated with a separator.
    Define.s PathWithSlash = Path
    If Not EndsWith( #PS$, Path ) And Len( Path ) > 0
      PathWithSlash + #PS$
    EndIf
    
    ; Compile regex, if given.
    Define.i Regex = #PB_Any
    If Len( Pattern ) > 0
      Regex = CreateRegularExpression( #PB_Any, Pattern, #PB_RegularExpression_NoCase )
    EndIf
    
    ; Go through directory entries.
    Define.i NumFound = 0
    Define.i Capacity = ArraySize( Files() )
    While NextDirectoryEntry( Directory )
      
      ; Skip anything that isn't a file.
      If DirectoryEntryType( Directory ) <> #PB_DirectoryEntry_File
        Continue
      EndIf
      
      ; Check pattern.
      Define.s FileName = DirectoryEntryName( Directory )
      If Regex <> #PB_Any And Not MatchRegularExpression( Regex, FileName )
        Continue
      EndIf
      
      ; Check capacity.
      If Capacity <= NumFound
        Capacity + 16
        ReDim Files( Capacity )
      EndIf
      
      ; Add file.
      Files( NumFound ) = PathWithSlash + FileName
      NumFound + 1
      
    Wend
    
    ; Clean up.
    FinishDirectory( Directory )
    If Regex <> #PB_Any
      FreeRegularExpression( Regex )
    EndIf
    
    ProcedureReturn NumFound
    
  EndProcedure
  
  DataSection
    
    LocalFileSystem_VTable:
      Data.q @LFS_Destroy()
      Data.q @LFS_OpenFile()
      Data.q @LFS_CreateFile()
      Data.q @LFS_CloseFile()
      Data.q @LFS_ReadFile()
      Data.q @LFS_WriteFile()
      Data.q @LFS_ReadString()
      Data.q @LFS_WriteString()
      Data.q @LFS_SetFileSize()
      Data.q @LFS_SizeFile()
      Data.q @LFS_MapFile()
      Data.q @LFS_UnmapFile()
      Data.q @LFS_FlushFileBuffers()
      Data.q @LFS_DeleteFile()
      Data.q @LFS_GetFileSize()
      Data.q @LFS_FileExists()
      Data.q @LFS_ListFiles()
    
  EndDataSection
    
  ;............................................................................
  
  Structure LocalDirectorySystem
    *Methods
    RootPath.s
    Map Directories.q()
  EndStructure
  
  Procedure.q CreateLocalDirectorySystem( Path.s )
    
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( Path, #PS$ )
      Path + #PS$
    EndIf
    
    Define.LocalDirectorySystem *LDS = AllocateStructure( LocalDirectorySystem )
    *LDS\Methods = ?LocalDirectorySystem_VTable
    *LDS\RootPath = Path
    
    ProcedureReturn *LDS
    
  EndProcedure
  
  Procedure LDS_Destroy( *LDS.LocalDirectorySystem )
    
    DebugAssert( *LDS <> #Null )
    ForEach *LDS\Directories()
      Define.IFileSystem *FileSystem =  *LDS\Directories()
      *FileSystem\Destroy()
    Next    
    FreeStructure( *LDS )
    
  EndProcedure
  
  Procedure.q LDS_CreateDirectory( *LDS.LocalDirectorySystem, Path.s )
    
    DebugAssert( *LDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( #PS$, Path )
      Path + #PS$
    EndIf
    
    Define.IFileSystem *FileSystem
    
    Define.s PathLowerCase = LCase( Path )
    Define.q *Element = FindMapElement( *LDS\Directories(), PathLowerCase )
    If *Element = #Null
      Define.s FullPath = *LDS\RootPath + Path
      If FileSize( FullPath ) <> -2 And CreateDirectory( FullPath ) = 0
        ProcedureReturn #Null
      EndIf 
      *FileSystem = CreateLocalFileSystem( FullPath )
      Define.q *Element = AddMapElement( *LDS\Directories(), PathLowerCase )
      PokeQ( *Element, *FileSystem )
    Else
      *FileSystem = PeekQ( *Element )
    EndIf
    
    ProcedureReturn *FileSystem    
    
  EndProcedure
  
  Procedure.q LDS_OpenDirectory( *LDS.LocalDirectorySystem, Path.s )
    
    DebugAssert( *LDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( #PS$, Path )
      Path + #PS$
    EndIf
    
    Define.IFileSystem *FileSystem
    
    Define.s PathLowerCase = LCase( Path )
    Define.q *Element = FindMapElement( *LDS\Directories(), PathLowerCase )
    If *Element = #Null
      Define.s FullPath = *LDS\RootPath + Path
      If FileSize( FullPath ) <> -2
        ProcedureReturn #Null
      EndIf 
      *FileSystem = CreateLocalFileSystem( FullPath )
      Define.q *Element = AddMapElement( *LDS\Directories(), PathLowerCase )
      PokeQ( *Element, *FileSystem )
    Else
      *FileSystem = PeekQ( *Element )
    EndIf
    
    ProcedureReturn *FileSystem   
    
  EndProcedure
  
  Procedure LDS_DeleteDirectory( *LDS.LocalDirectorySystem, Path.s )
    NotImplemented( "LDS_DeleteDirectory" )
  EndProcedure
  
  Procedure LDS_DirectoryExists( *LDS.LocalDirectorySystem, Path.s )
    
    DebugAssert( *LDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    NotImplemented( "LDS_DirectoryExists" )
    
  EndProcedure
  
  Procedure.i LDS_ListDirectories( *LDS.LocalDirectorySystem, Path.s, Pattern.s, Array Directories.s( 1 ) )
    
    DebugAssert( *LDS <> #Null )
    
    NotImplemented( "LDS_ListDirectories" )
    
  EndProcedure
  
  DataSection
    
    LocalDirectorySystem_VTable:
      Data.q @LDS_Destroy()
      Data.q @LDS_CreateDirectory()
      Data.q @LDS_OpenDirectory()
      Data.q @LDS_DeleteDirectory()
      Data.q @LDS_DirectoryExists()
      Data.q @LDS_ListDirectories()
    
  EndDataSection  
  
  ;............................................................................
  
  EnumerationBinary
    #VirtualFile_Deleted
  EndEnumeration
  
  Structure VirtualFile
    Flags.i
    OpenCount.i
    Path.s
    PathLowerCase.s
    Size.q
    *Contents
  EndStructure
  
  Structure VirtualFileSystem
    *Methods
    Array Files.VirtualFile( 1 ) ; Always having at least one element simplifies the code.
  EndStructure
  
  Procedure.q CreateVirtualFileSystem()
    
    Define.VirtualFileSystem *VFS = AllocateStructure( VirtualFileSystem )
    
    *VFS\Methods = ?VirtualFileSystem_VTable
    *VFS\Files( 0 )\Flags = #VirtualFile_Deleted
    
    ProcedureReturn *VFS
    
  EndProcedure
  
  Procedure VFS_Destroy( *VFS.VirtualFileSystem )
    
    FreeStructure( *VFS )
    
  EndProcedure
  
  Procedure.q VFS_OpenFile( *VFS.VirtualFileSystem, Path.s, Flags.i )
    
    DebugAssert( *VFS <> #Null )
    
    Define.s PathLowerCase = LCase( Path )
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.i Index
    For Index = 0 To NumFiles - 1
      
      If *VFS\Files( Index )\PathLowerCase = PathLowerCase
        *VFS\Files( Index )\OpenCount + 1
        ProcedureReturn Index + 1
      EndIf
            
    Next
    
    ProcedureReturn 0
    
  EndProcedure
  
  Procedure.q VFS_CreateFile( *VFS.VirtualFileSystem, Path.s, Flags.i )
    
    DebugAssert( *VFS <> #Null )
    
    ;;REVIEW: shouldn't this look for whether the file already exists?
    
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
    *VFS\Files( Index )\OpenCount = 1
    *VFS\Files( Index )\Flags = 0
    
    ProcedureReturn Index + 1
    
  EndProcedure
  
  Procedure VFS_CloseFile( *VFS.VirtualFileSystem, Handle.q )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    
    Define.i Index = Handle - 1
    *VFS\Files( Index )\OpenCount - 1
    
  EndProcedure
  
  Procedure.q VFS_ReadFile( *VFS.VirtualFileSystem, Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    DebugAssert( *Buffer <> #Null )
    
    Define.i Index = Handle - 1
    DebugAssert( *VFS\Files( Index )\OpenCount > 0 )
    
    Define.q FileSize = *VFS\Files( Index )\Size
    If Position + Size > FileSize
      Size = FileSize - Position
    EndIf
    
    Define *FileContents = *VFS\Files( Index )\Contents
    CopyMemory( *FileContents + Position, *Buffer, Size )
    
    ProcedureReturn Size
    
  EndProcedure
  
  Procedure VFS_WriteFile( *VFS.VirtualFileSystem, Handle.q, Position.q, Size.q, *Buffer, Flags.i = 0 )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    DebugAssert( Size >= 0 )
    
    Define.i Index = Handle - 1
    DebugAssert( *VFS\Files( Index )\OpenCount > 0 )
    
    ; We allow calling WriteFile with a NULL buffer if size is zero.
    ; Can be useful for truncating a file.
    If Size > 0
      DebugAssert( *Buffer <> #Null )
   
      ; Enlarge memory, if necessary.
      Define.q Capacity = 0
      Define *Contents = *VFS\Files( Index )\Contents
      If *Contents <> #Null
        Capacity = MemorySize( *Contents )
      EndIf
      If Position + Size > Capacity
        Capacity = AlignToMultipleOf( Max( Capacity + 1024, Position + Size ), 1024 )
        *Contents = ReAllocateMemory( *Contents, Capacity, #PB_Memory_NoClear )
        *VFS\Files( Index )\Contents = *Contents
      EndIf
      
      ; Copy.
      CopyMemory( *Buffer, *Contents + Position, Size )
      
    EndIf
    
    ; Update size.
    If *VFS\Files( Index )\Size < Position + Size Or Flags & #FileTruncate
      *VFS\Files( Index )\Size = Position + Size
    EndIf
    
  EndProcedure
  
  Procedure.s VFS_ReadString( *VFS.VirtualFileSystem, Handle.q, Position.q = 0, Length.i = -1, Flags.i = 0 )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    DebugAssert( Length >= 0 Or Length = -1 )
    
    Define.i Index = Handle - 1
    DebugAssert( *VFS\Files( Index )\OpenCount > 0 )
    DebugAssert( Position <= *VFS\Files( Index )\Size )
    DebugAssert( Position + Length <= *VFS\Files( Index )\Size )
    
    Define.q *Ptr = *VFS\Files( Index )\Contents
    
    Define.s String
    If Length < 0
      Define.i Size = *VFS\Files( Index )\Size - Position
      If Size <= 0
        String = ""
      Else
        String = PeekS( *Ptr + Position, Size, #PB_UTF8 | #PB_ByteLength )
      EndIf
    Else
      String = PeekS( *Ptr + Position, Length, #PB_UTF8 )
    EndIf
    
    ProcedureReturn String
    
  EndProcedure
  
  Procedure VFS_WriteString( *VFS.VirtualFileSystem, Handle.q, String.s, Position.q = 0, Flags.i = 0 )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    
    Define.q Size = StringByteLength( String, #PB_UTF8 )
    
    Define.i Index = Handle - 1
    DebugAssert( *VFS\Files( Index )\OpenCount > 0 )
    
    ; Enlarge memory, if necessary.
    Define.q Capacity = 0
    Define *Contents = *VFS\Files( Index )\Contents
    If *Contents <> #Null
      Capacity = MemorySize( *Contents )
    EndIf
    If Position + Size > Capacity
      Capacity = AlignToMultipleOf( Max( Capacity + 1024, Position + Size ), 1024 )
      *Contents = ReAllocateMemory( *Contents, Capacity, #PB_Memory_NoClear )
      *VFS\Files( Index )\Contents = *Contents
    EndIf
    
    ; Write.
    Define.q NumBytes = PokeS( *Contents + Position, String, Size, #PB_UTF8 | #PB_String_NoZero )
    DebugAssert( NumBytes = Size )
    
    ; Update size.
    If *VFS\Files( Index )\Size < Position + Size Or Flags & #FileTruncate
      *VFS\Files( Index )\Size = Position + Size
    EndIf
    
  EndProcedure
  
  Procedure VFS_SetFileSize( *VFS.VirtualFileSystem, Handle.q, Size.q )
    NotImplemented( "VFS_SetFileSize" )
  EndProcedure
  
  Procedure.q VFS_SizeFile( *VFS.VirtualFileSystem, Handle.q )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    
    Define.i Index = Handle - 1
    ProcedureReturn *VFS\Files( Index )\Size
    
  EndProcedure
  
  Procedure.q VFS_MapFile( *VFS.VirtualFileSystem, Handle.q, Position.q = 0, Size.q = -1 )
    
    DebugAssert( *VFS <> #Null )
    DebugAssert( Handle > 0 )
    DebugAssert( Handle <= ArraySize( *VFS\Files() ) )
    DebugAssert( Position >= 0 )
    
    ;;REVIEW: do we need to protected against resizing here? disallow resizing while a file is mapped?
    
    Define.i Index = Handle - 1
    DebugAssert( Position <= *VFS\Files( Index )\Size )
    Define.q *Ptr = *VFS\Files( Index )\Contents + Position
    DebugAssert( Size = -1 Or Position + Size <= *VFS\Files( Index )\Size )
    
    ProcedureReturn *Ptr
    
  EndProcedure
  
  Procedure VFS_UnmapFile( *VFS.VirtualFileSystem, Handle.q )
    ; Nothing to do.
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
  
  Procedure.i VFS_FileExists( *VFS.VirtualFileSystem, Path.s )
    
    DebugAssert( *VFS <> #Null )
    
    Define.s PathLowerCase = LCase( Path )
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.i Index
    For Index = 0 To NumFiles - 1
      
      Define.s Path = *VFS\Files( Index )\PathLowerCase
      If Path = PathLowerCase
        ProcedureReturn #True
      EndIf
      
    Next
    
    ProcedureReturn #False
    
  EndProcedure
  
  Procedure.i VFS_ListFiles( *VFS.VirtualFileSystem, Path.s, Pattern.s, Array Files.s( 1 ) )
    
    DebugAssert( *VFS <> #Null )
    
    Define.i Capacity = ArraySize( Files() )
    Define.i PathLen = Len( Path )
    Define.s PathLowerCase = LCase( Path )
    Define.i Count = 0
    Define.i NumFiles = ArraySize( *VFS\Files() )
    Define.VirtualFile *File = @*VFS\Files()
    Define.i Regex = #PB_Any
    If Len( Pattern ) > 0
      Regex = CreateRegularExpression( #PB_Any, Pattern, #PB_RegularExpression_NoCase )
    EndIf
    
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
      
      ; Check pattern.
      If Regex <> #PB_Any And Not MatchRegularExpression( Regex, FilePathLowerCase )
        *File + SizeOf( VirtualFile )
        Continue
      EndIf
      
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
    
    If Regex <> #PB_Any
      FreeRegularExpression( Regex )
    EndIf
    
    ProcedureReturn Count
    
  EndProcedure
  
  DataSection
    
    VirtualFileSystem_VTable:
      Data.q @VFS_Destroy()
      Data.q @VFS_OpenFile()
      Data.q @VFS_CreateFile()
      Data.q @VFS_CloseFile()
      Data.q @VFS_ReadFile()
      Data.q @VFS_WriteFile()
      Data.q @VFS_ReadString()
      Data.q @VFS_WriteString()
      Data.q @VFS_SetFileSize()
      Data.q @VFS_SizeFile()
      Data.q @VFS_MapFile()
      Data.q @VFS_UnmapFile()
      Data.q @VFS_FlushFileBuffers()
      Data.q @VFS_DeleteFile()
      Data.q @VFS_GetFileSize()
      Data.q @VFS_FileExists()
      Data.q @VFS_ListFiles()
    
  EndDataSection
  
  ;............................................................................
  
  Structure VirtualDirectorySystem
    *Methods
    Map Directories.q()
  EndStructure
  
  Procedure.q CreateVirtualDirectorySystem()
    
    Define.VirtualDirectorySystem *VDS = AllocateStructure( VirtualDirectorySystem )
    *VDS\Methods = ?VirtualDirectorySystem_VTable
    ProcedureReturn *VDS
    
  EndProcedure
  
  Procedure VDS_Destroy( *VDS.VirtualDirectorySystem )
    
    DebugAssert( *VDS <> #Null )
    ForEach *VDS\Directories()
      Define.IFileSystem *FileSystem =  *VDS\Directories()
      *FileSystem\Destroy()
    Next
    FreeStructure( *VDS )
    
  EndProcedure
  
  Procedure.q VDS_CreateDirectory( *VDS.VirtualDirectorySystem, Path.s )
    
    DebugAssert( *VDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( #PS$, Path )
      Path + #PS$
    EndIf
    
    Define.IFileSystem *FileSystem
    
    Define.s PathLowerCase = LCase( Path )
    Define.q *Element = FindMapElement( *VDS\Directories(), PathLowerCase )
    If *Element = #Null
      *FileSystem = CreateVirtualFileSystem()
      Define.q *Element = AddMapElement( *VDS\Directories(), PathLowerCase )
      PokeQ( *Element, *FileSystem )
    Else
      *FileSystem = PeekQ( *Element )
    EndIf
    
    ProcedureReturn *FileSystem
    
  EndProcedure
  
  Procedure.q VDS_OpenDirectory( *VDS.VirtualDirectorySystem, Path.s )
    
    DebugAssert( *VDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( #PS$, Path )
      Path + #PS$
    EndIf
    
    Define.IFileSystem *FileSystem = #Null
    
    Define.s PathLowerCase = LCase( Path )
    Define.q *Element = FindMapElement( *VDS\Directories(), PathLowerCase )
    If *Element <> #Null
      *FileSystem = PeekQ( *Element )
    EndIf
    
    ProcedureReturn *FileSystem
    
  EndProcedure
  
  Procedure VDS_DeleteDirectory( *VDS.VirtualDirectorySystem, Path.s )
    NotImplemented( "VDS_DeleteDirectory" )
  EndProcedure
  
  Procedure VDS_DirectoryExists( *VDS.VirtualDirectorySystem, Path.s )
    
    DebugAssert( *VDS <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    If Not EndsWith( #PS$, Path )
      Path + #PS$
    EndIf
    
    Define.s PathLowerCase = LCase( Path )
    If FindMapElement( *VDS\Directories(), PathLowerCase ) <> #Null
      ProcedureReturn #True
    EndIf
    
    ProcedureReturn #False
    
  EndProcedure
  
  Procedure.i VDS_ListDirectories( *VDS.VirtualDirectorySystem, Path.s, Pattern.s, Array Directories.s( 1 ) )
    
    DebugAssert( *VDS <> #Null )
    
    ProcedureReturn 0
    
  EndProcedure
  
  DataSection
    
    VirtualDirectorySystem_VTable:
      Data.q @VDS_Destroy()
      Data.q @VDS_CreateDirectory()
      Data.q @VDS_OpenDirectory()
      Data.q @VDS_DeleteDirectory()
      Data.q @VDS_DirectoryExists()
      Data.q @VDS_ListDirectories()
    
  EndDataSection
  
  ;............................................................................
  
  Procedure.s ReadTextFile( *FileSystem.IFileSystem, Path.s )
    
    DebugAssert( *FileSystem <> #Null )
    DebugAssert( Len( Path ) > 0 )
    
    Define.q Handle = *FileSystem\OpenFile( Path )
    If Handle = 0
      ProcedureReturn ""
    EndIf
    
    Define.s Contents = *FileSystem\ReadString( Handle )
    *FileSystem\CloseFile( Handle )
    
    ProcedureReturn Contents
    
  EndProcedure
  
  ;............................................................................
  
  Procedure.q RunJob( *Job.Job )
    NotImplemented( "RunJob" )
  EndProcedure
  
  ;............................................................................
  
  Procedure ExecuteMainThreadJobFuncs()
    NotImplemented( "ExecuteMainThreadJobFuncs" )
  EndProcedure
  
  ;............................................................................
    
  Procedure.s GenerateGUID()
    
    ; NOTE: Format needs to be the same across all platforms.
    
    CompilerIf #PB_Compiler_OS = #PB_OS_Windows
      
      Define.s{ 78 } Buffer
      Define.GUID GUID
      
      If CoCreateGuid_( @GUID ) = #S_OK
        Define.i NumChars = StringFromGUID2_( GUID, @Buffer, 76 )
        If NumChars = 39 ; 32 + 4 dashes + 2 curly braces + null terminator
          ProcedureReturn PeekS( @Buffer + 2, 36, #PB_Unicode ) ; Snip away curly brackets and null terminator.
        EndIf
      EndIf
      
    CompilerElseIf #PB_Compiler_OS = #PB_OS_MacOS
      
      Define.q UUIDRef = CocoaMessage( 0, CocoaMessage( 0, 0, "NSUUID alloc" ), "init" )
      If UUIDRef
        Define.q StringRef = CocoaMessage( 0, IDRef, "UUIDString" )
        Define.q UTF8 = CocoaMessage( 0, StringRef, "UTF8String" )
        Define.s Result = PeekS( UTF8, -1, #PB_UTF8 )
        CocoaMessage( 0, StringRef, "release" )
        CocoaMessage( 0, UUIDRef, "release" ) ;alloc
        CocoaMessage( 0, UUIDRef, "release" ) ;init
        ProcedureReturn #Result
      EndIf
      
    CompilerEndIf
    
  EndProcedure  
  
EndModule

;..............................................................................

ProcedureUnit CanCreateVirtualFileSystem()

  UseModule Utils

  Define.IFileSystem *VFS = CreateVirtualFileSystem()
  
  Dim Files.s( 1 )
  
  Assert( *VFS\ListFiles( "", "", Files() ) = 0 )
  Assert( *VFS\ListFiles( "DoesNotExist", "", Files() ) = 0 )
  Assert( *VFS\FileExists( "first" ) = #False )
  
  Define.q First = *VFS\CreateFile( "first" )
  Define.q Second = *VFS\CreateFile( "second" )
  
  Assert( First <> 0 )
  Assert( Second <> 0 )
  Assert( *VFS\FileExists( "first" ) = #True )
  Assert( *VFS\FileExists( "second" ) = #True )
  Assert( *VFS\FileExists( "DoesNotExist" ) = #False )
  
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
  Assert( *VFS\FileExists( "first" ) = #False )
  Assert( *VFS\FileExists( "second" ) = #True )
  
  Assert( *VFS\ListFiles( "", "", Files() ) = 1 )
  Assert( Files( 0 ) = "second" )
  
  *VFS\CloseFile( Second )
  Second = *VFS\OpenFile( "second" )
  
  Assert( Second <> 0 )
  
  ;;TODO: create new file
  
  *VFS\Destroy()

EndProcedureUnit

;..............................................................................

ProcedureUnit CanCreateVirtualDirectorySystem()

  UseModule Utils

  Define.IDirectorySystem *VDS = CreateVirtualDirectorySystem()
  
  Assert( *VDS <> #Null )
  
  Dim Directories.s( 0 )
  
  Assert( *VDS\ListDirectories( "", "", Directories() ) = 0 )
  Assert( ArraySize( Directories() ) = 0 )
  Assert( *VDS\DirectoryExists( "Directory1" ) = #False )
  
  Define.IFileSystem *Directory1 = *VDS\CreateDirectory( "Directory1" )
  
  Assert( *Directory1 <> #Null )
  Assert( *VDS\DirectoryExists( "Directory1" ) = #True )
  Assert( *VDS\DirectoryExists( "Directory2" ) = #False )
  
  Define.IFileSystem *Directory2 = *VDS\CreateDirectory( "Directory2" )
  
  Assert( *Directory2 <> #Null )
  Assert( *Directory1 <> *Directory2 )
  Assert( *VDS\DirectoryExists( "Directory1" ) = #True )
  Assert( *VDS\DirectoryExists( "Directory2" ) = #True )
  
  ; Calling CreateDirectory with existing directory should just open it.
  Define.IFileSystem *Directory2a = *VDS\CreateDirectory( "Directory2" )
  Assert( *Directory2a = *Directory2 )
  
  ;;TODO: ListDirectories
  ;;TODO: DeleteDirectory
  
  *VDS\Destroy()

EndProcedureUnit

;..............................................................................

ProcedureUnit CanRunJobs()

  ;;... continue here

EndProcedureUnit

; IDE Options = PureBasic 5.73 LTS (Windows - x64)
; CursorPosition = 787
; FirstLine = 758
; Folding = -------------
; Markers = 931,1433
; EnableXP