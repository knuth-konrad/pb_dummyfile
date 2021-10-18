'------------------------------------------------------------------------------
'Purpose  : Create one or more Random Data files of a specified size
'           Might be usefull For testing purposes.
'
'Prereq.  : -
'Note     : -
'
'   Author: Knuth Konrad 2007, 2017
'   Source: -
'  Changed: 10.02.2017
'           - Code reformatting
'           15.05.2017
'           - Application manifest added
'           - #Break on to prevent context menu mishap.
'           18.10.2021
'           - Rewrite file content creation / file creation in order to
'           work around Our of Memory errors
'------------------------------------------------------------------------------
#Compile Exe ".\DummyFile.exe"
#Option Version5
#Break On
#Dim All

#Link "baCmdLine.sll"
' #link "baUtil.PBlib"

#Debug Error Off
#Tools Off

%VERSION_MAJOR = 3
%VERSION_MINOR = 1
%VERSION_REVISION = 0

' Version Resource information
#Include ".\DummyFileRes.inc"
'------------------------------------------------------------------------------
'*** Constants ***
'------------------------------------------------------------------------------
' Default line length
%LINE_LENGTH = 80

' Write block size
%BLOCK_SIZE = 512 * 1024
'------------------------------------------------------------------------------
'*** Enumeration/TYPEs ***
'------------------------------------------------------------------------------
'------------------------------------------------------------------------------
'*** Declares ***
'------------------------------------------------------------------------------
#Include "sautilcc.inc" ' General console helpers
#Include "cs.inc"       ' Critical section

Declare Function FormatLoc(ByVal fextValue As Ext, ByVal sMask As String) As String
Declare Function FileExist(sFile As AsciiZ) As Long
Declare Sub ShowHelp()
'------------------------------------------------------------------------------
'*** Variabels ***
'------------------------------------------------------------------------------
Global gqudSize As Quad
Global gsContent As String
Global gsPath As String
Global gsFileExt As String
'==============================================================================

Function PBMain()
'------------------------------------------------------------------------------
'Purpose  : Programm startup method
'
'Prereq.  : -
'Parameter: -
'Returns  : -
'Note     : -
'
'   Author: Knuth Konrad
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------
   Local hFile As Long, lFiles As Long, i As Long
   Local sUnit, sCmd, sTemp, sTempfile As String
   Local qudTimes, qudOrgSize As Quad
   Local lRet, lChunks, lRemain, lCRLf, lLineLength As Long
   Local j As Long

   ' Application intro
   ConHeadline "DummyFile", %VERSION_MAJOR, %VERSION_MINOR, %VERSION_REVISION
   ConCopyright "2007-2021", $COMPANY_NAME
   Print ""

   sCmd = Command$

   Trace New ".\DummyFile.tra"
   Trace On

   Local o As IBACmdLine
   Local vnt As Variant

   Let o = Class "cBACmdLine"

   If IsFalse(o.Init(sCmd)) Then
      Print "Couldn't parse parameters: " & sCmd
      Print "Type DummyFile /? for help"
      Let o = Nothing
      Exit Function
   End If

   If Len(Trim$(Command$)) < 1 Or InStr(Command$, "/?") > 0 Then
      ShowHelp
      Exit Function
   End If

   ' *** Parse the parameters
   i = o.ValuesCount

   If i < 2 Then
      Print "Too few parameters: " & sCmd
      Print "Type DummyFile /? for help"
      Let o = Nothing
      Exit Function
   End If


   ' *** Mandatory parameters
   ' ** Number of files to create
   If IsTrue(o.HasParam("n")) Then
      sTemp = Variant$(o.GetValueByName("n"))
      lFiles = Val(Trim$(Remove$(sTemp, $Dq)))
   End If

   Dim hThdID(1 To lFiles) As Long


    ' ** Size and unit of size
   If IsTrue(o.HasParam("s")) Then
      sTemp = Variant$(o.GetValueByName("s"))
      sTemp = Trim$(Remove$(sTemp, $Dq))
   End If

   sUnit = LCase$(Right$(sTemp, 2))

   Select Case sUnit
   Case "kb"
       qudTimes = 1024
   Case "mb"
       qudTimes = 1024^2
   Case "gb"
       qudTimes = 1024^3
   Case Else
       qudTimes = 1
       sUnit = "byte"
   End Select

   qudOrgSize = Val(Trim$(sTemp))
   gqudSize = qudTimes * qudOrgSize

   ' Do the values of the mandatory parameters make sense?
   If lFiles < 1 Or gqudSize < 1 Then
      Print "Invalid parameter value(s): " & sCmd
      Print "Type DummyFile /? for help"
      Let o = Nothing
      Exit Function
   End If

   ' *** Optional parameters
   ' ** Location of file creation
   If IsTrue(o.HasParam("f")) Then
      sTemp = Variant$(o.GetValueByName("f"))
      sTemp = Trim$(Remove$(sTemp, $Dq))
   Else
      ' Current folder is the default location"
      sTemp = ".\"
   End If

   If IsFalse(DirExist(sTemp)) Then
      Print "Folder doesn't exsits: " & sTemp
      Let o = Nothing
      Exit Function
   End If

   gsPath = sTemp


   ' ** Create a text file with line breaks?
   If IsTrue(o.HasParam("lf")) Then
      lCRLF = 1
   Else
      lCRLF = 0
   End If

   ' ** Line length?
   If IsTrue(o.HasParam("ll")) Then
      sTemp = Variant$(o.GetValueByName("ll"))
      lLineLength = Val(Trim$(Remove$(sTemp, $Dq)))
   End If


   ' ** File extension?
   If IsTrue(o.HasParam("fe")) Then
      sTemp = Variant$(o.GetValueByName("fe"))
      sTemp = Trim$(Remove$(sTemp, $Dq))
      If Len(sTemp) > 0 Then
         gsFileExt = sTemp
      Else
         gsFileExt = "tmp"
      End If
   Else
      gsFileExt = "tmp"
   End If



   ' Determine if it's a relative or absolute path, i.e. .\MyFolder or C:\MyFolder and/or a UNC share
   Local sPathFull As String
   sPathFull = gsPath
   sPathFull = FullPathAndUNC(gsPath)

   ' Echo the CLI parameters
   Con.StdOut "# of files    : " & Format$(lFiles)
   Con.StdOut "File size     : " & Format$(qudOrgSize) & " " & sUnit
   Con.StdOut "File extension: " & gsFileExt
   Con.StdOut "Folder        : " & gsPath;
   ' If path is a relative path, display the full path also
   If LCase$(NormalizePath(gsPath)) = LCase$(NormalizePath(sPathFull)) Then
      Con.StdOut ""
   Else
      Con.StdOut " (" & sPathFull & ")"
   End If
   Con.StdOut "Add line feed : " & IIf$(IsTrue(lCRLF), "Yes", "No")
   If lLineLength > 0 Then
      Con.StdOut "Line length   : " & Format$(lLineLength)
   End If



   ' *** Start the file creation
   Print ""
   Print "Preparing file contents ..."

   '
   ' Call CreateRandomFileContent(gqudSize, gsContent, lCRLF, lLineLength)
   Call CreateRandomFileContent(%BLOCK_SIZE, gsContent, lCRLF, lLineLength)

   Print ""

   lChunks = lFiles \ %Maximum_Wait_Objects
   lRemain = lFiles Mod %Maximum_Wait_Objects

   Print "Preparing " & Format$(lFiles) & " thread(s) in " & Format$(lChunks + 1) & _
      " chunk(s) ..."

   Call InitCS()

   For j = 1 To lChunks

      For i = 1 To %Maximum_Wait_Objects
         Thread Create WriteTempFile(i) To hThdID(i)
      Next i

      lRet = WaitForMultipleObjects(%Maximum_Wait_Objects, ByVal VarPtr(hThdID(1)), %TRUE, %INFINITE)

      ' Free up the thread handles
      For i = 1 To %Maximum_Wait_Objects
         Thread Close hThdID(i) To lRet
      Next i

   Next j

   For i = 1 To lRemain
      Thread Create WriteTempFile(i) To hThdID(i)
   Next i

   lRet = WaitForMultipleObjects(lRemain, ByVal VarPtr(hThdID(1)), %TRUE, %INFINITE)

   ' Free thread handles
   For i = 1 To lRemain
      Thread Close hThdID(i) To lRet
   Next i

   Flush

   Print ""
   Print "Done!"

   Trace Off
   Trace Close

   Call DeleteCS()

   Profile ".\DummyFile.pro"

End Function
'==============================================================================

Sub ShowHelp()
'------------------------------------------------------------------------------
'Purpose  : Help screen
'
'Prereq.  : -
'Parameter: -
'Returns  : -
'Note     : -
'
'   Author: Knuth Konrad
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------

   Con.StdOut ""
   Con.StdOut "DummyFile lets you easily create a number of dummy files for various testing scenarios."
   Con.StdOut "The files are generated from random characters (plain text)."
   Con.StdOut "Per default, the file's contents is generated as one 'big blob'."
   Con.StdOut "However, you can specify to add line feeds (actually line feed plus carriage return)"
   Con.StdOut "to simulate 'proper text files'."
   Con.StdOut ""
   Con.StdOut "Usage:"
   Con.StdOut "DummyFile /n=<No. of files> /s=<file size> [/f=<folder to create files in>] [/lf] [/ll=<No. of characters per line>] [/fe=<file extension>]"
   Con.StdOut ""
   Con.StdOut "     e.g.: DummyFile /n=10 /s=12MB"
   Con.StdOut "              - Create 10 files (in the current folder) with a size of 12MB each, do not add line feed(s)."
   Con.StdOut "           DummyFile /n=10 /s=12MB /f=c:\temp"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, do not add line feed(s)."
   Con.StdOut "           DummyFile /n=10 /s=12MB /f=c:\temp /lf"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, add line feed(s). Line length defaults to 80 characters."
   Con.StdOut "           DummyFile /n=10 /s=12MB /f=c:\temp /lf /ll=72"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, add line feed(s). Line length should be 72 characters."
   Con.StdOut "           DummyFile /n=10 /s=12MB /f=c:\temp /lf /ll=72 /fe=txt"
   Con.StdOut "              - Create 10 files with the file extension 'txt' in the folder c:\temp with a size of 12MB each, add line feed(s)."
   Con.StdOut "                Line length should be 72 characters."
   Con.StdOut ""
   Con.StdOut "Parameters"
   Con.StdOut "----------"
   Con.StdOut "/n  - Number of files to create (mandatory)."
   Con.StdOut "/s  - File size of each file (mandatory)."
   Con.StdOut "/f  - Folder in which the file(s) should be created."
   Con.StdOut "/lf - Create files with line feeds (CrLf)."
   Con.StdOut "/ll - Line length (number of characters). Can only be used in conjunction with /lf."
   Con.StdOut "/fe - File extension. Defaults to 'tmp'."

   Con.StdOut ""
   Con.StdOut "Allowed file size units for parameter /s are:
   Con.StdOut "<empty> = Byte      e.g. DummyFile /n=1, /s=100"
   Con.StdOut "     kb = Kilobyte  e.g. DummyFile /n=1, /s=100kb"
   Con.StdOut "     mb = Megabyte  e.g. DummyFile /n=1, /s=100mb"
   Con.StdOut "     gb = Gigabyte  e.g. DummyFile /n=1, /s=100gb"
   Con.StdOut ""
   Con.StdOut "Please note: 1 KB = 1024 byte, 1 MB = 1024 KB etc."

End Sub
'==============================================================================

Thread Function WriteTempFile(ByVal lFile As Long) As Long
'------------------------------------------------------------------------------
'Purpose  : Create a dummy/temp. file
'
'Prereq.  : -
'Parameter: lFile - Thread handle
'Returns  : -
'Note     : -
'
'   Author: Knuth Konrad
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------
   Local i, hFile As Long
   Local sTempfile, sPath As String
   Local dwWriteCycles, dwRemainder As Dword

   sPath = NormalizePath(gsPath)

   hFile = FreeFile
   sTempfile = CreateTempFileName(ByCopy sPath, "sa", ByCopy gsFileExt)

   Call EnterCS
   Open sTempfile For Output As #hFile

   Print "Thread " & Format$(lFile) & ", creating file of " & FormatLoc(gqudSize, "#,") & " byte(s): " & sTempfile
   Call LeaveCS

   ' Write <x> times block until actual size

   dwWriteCycles = gqudSize \ %BLOCK_SIZE
   dwRemainder =  gqudSize - (dwWriteCycles * %BLOCK_SIZE)

   For i = 1 To dwWriteCycles
      Call EnterCS
      Print #hFile, gsContent;
      Call LeaveCS
      If i Mod 1024^1 = 0 Then
         Flush #hFile
      End If
   Next i

   If dwRemainder > 0 Then
      Call EnterCS
      Print #hFile, Left$(gsContent, dwRemainder);
      Call LeaveCS
   End If

   Close #hFile

   Print " - Thread " & Format$(lFile) & ", creation file of " & sTempfile & " finished."

End Function
'==============================================================================

Sub CreateRandomFileContent(ByVal qudSize As Quad, ByRef sResult As String, Optional ByVal lCRLF As Long, _
   Optional ByVal lLineLength As Long)
'------------------------------------------------------------------------------
'Purpose  : Generate the contents for the dummy files
'
'Prereq.  : -
'Parameter: qudSize     - Size of contents
'           lCRLF       - Simulate a "real" text file by adding CRLF after each line?
'           lLineLength - (character) length of a line
'Returns  : -
'Note     : lCRLF and lLineLength are used in conjunction
'
'   Author: Knuth Konrad
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------
   ' Local sResult As String
   Local s As Byte Ptr
   Local i, s1 As Quad
   Local dwLineCount As Dword
   Local curRow, curCol As Long

   Trace On
   Trace Print "-> Allocating space"

   ' Preallocate memory
   sResult = Space$(qudSize)
   s = StrPtr(sResult)
   Trace Print "<- Allocating space"

   ' Seed random
   Randomize qudSize

   Trace Print "-> Writing content"
   Con.Cell To curRow, curCol
   For i = 1 To qudSize
       @s = Rnd(48, 122)
       Incr s
       If i Mod 1024^2 = 0 Then
          Con.Locate curRow, 1
          Con.StdOut "Creating file contents: " & Format$(i, "#,") & " of " & Format$(qudSize, "#,") & " bytes";
       End If
   Next i
   Con.StdOut ""
   Trace Print "<- Writing content"

   ' Need a (text) file with actual lines?
   If IsTrue(lCRLF) Then

      If lLineLength = 0 Then lLineLength = %LINE_LENGTH

      s = StrPtr(sResult)
      s1 = s + qudSize
      dwLineCount = qudSize \ lLineLength

      For i = lLineLength To qudSize Step lLineLength

         Trace Print "s+i:   " & Format$(s + i)
         Trace Print "max s: " & Format$(s1)
         Trace Print "s:     " & Format$(s)
         Trace Print "i:     " & Format$(i)
         Trace Print "file:  " & Format$(qudSize)
         Trace Print String$(50, "-")

         s = s + lLineLength

         If s + 1 > s1 Then
            Exit For
         End If

         @s = 13  ' CR
         Incr s
         @s = 10  ' LF

      Next i

   End If

   ' CreateRandomFileContent = sResult

   Trace Print "- End of CreateRandomFileContent, Len(sResult): " & Format$(Len(sResult))
   Trace Off

   If Err Then
      Local lErr As Long
      lErr = Err
      Select Case lErr
      Case 7
         Con.StdOut "Hold on there - we ran out of memory while trying to create a file that big. Try reducing the file size a bit."
      Case Else
         Con.StdOut "An error occured while preparing the dummy file's content. Err " & Format$(Err) & ", " & Error$(Err)
      End Select
      ErrClear
   End If

End Sub
'==============================================================================

#If 0
Function FullPathAndUNC(ByVal sPath As String) As String
'------------------------------------------------------------------------------
'Purpose  : Resolves/expands a path from a relative path to an absolute path
'           and UNC path, if the drive is mapped
'
'Prereq.  : -
'Parameter: -
'Returns  : -
'Note     : -
'
'   Author: Knuth Konrad 30.01.2017
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------

   ' Determine if it's a relative or absolute path, i.e. .\MyFolder or C:\MyFolder
   Local szPathFull As AsciiZ * %Max_Path, sPathFull As String, lResult As Long
   sPathFull = sPath
   lResult = GetFullPathName(ByCopy sPath, %Max_Path, szPathFull, ByVal 0)
   If lResult <> 0 Then
      sPathFull = Left$(szPathFull, lResult)
   End If

   ' Now that we've got that sorted, resolve the UNC path, if any
   Local dwError As Dword
   FullPathAndUNC = UNCPathFromDriveLetter(sPathFull, dwError, 0)

End Function
'------------------------------------------------------------------------------

Function UNCPathFromDriveLetter(ByVal sPath As String, ByRef dwError As Dword, _
   Optional ByVal lDriveOnly As Long) As String
'------------------------------------------------------------------------------
'Purpose  : Returns a fully qualified UNC path location from a (mapped network)
'           drive letter/share
'
'Prereq.  : -
'Parameter: sPath       - Path to resolve
'           dwError     - ByRef(!), Returns the error code from the Win32 API, if any
'           lDriveOnly  - If True, return only the drive letter
'Returns  : -
'Note     : -
'
'   Author: Knuth Konrad 17.07.2013
'   Source: -
'  Changed: -
'------------------------------------------------------------------------------
' 32-bit declarations:
Local sTemp As String
Local szDrive As AsciiZ * 3, szRemoteName As AsciiZ * 1024
Local lSize, lStatus As Long

' The size used for the string buffer. Adjust this if you
' need a larger buffer.
Local lBUFFER_SIZE As Long
lBUFFER_SIZE = 1024

If Len(sPath) > 2 Then
   sTemp = Mid$(sPath, 3)
   szDrive = Left$(sPath, 2)
Else
   szDrive = sPath
End If

' Return the UNC path (\\Server\Share).
lStatus = WNetGetConnectionA(szDrive, szRemoteName, lBUFFER_SIZE)

' Verify that the WNetGetConnection() succeeded. WNetGetConnection()
' returns 0 (NO_ERROR) if it successfully retrieves the UNC path.
If lStatus = %NO_ERROR Then

   If IsTrue(lDriveOnly) Then

      ' Display the UNC path.
      UNCPathFromDriveLetter = Trim$(szRemoteName, Any $Nul & $WhiteSpace)

   Else

      UNCPathFromDriveLetter = Trim$(szRemoteName, Any $Nul & $WhiteSpace) & sTemp

   End If

Else

   ' Return the original filename/path unaltered
   UNCPathFromDriveLetter = sPath

End If

dwError = lStatus

End Function
'------------------------------------------------------------------------------

#EndIf
