'------------------------------------------------------------------------------
'Purpose  : Create one Or more Random Data files of a specified size
'           Might be usefull For testing purposes.
'
'Prereq.  : -
'Note     : -
'
'   Author: Knuth Konrad 2007
'   Source: -
'  Changed: 10.02.2017
'           - Code reformatting
'------------------------------------------------------------------------------
#Compile Exe ".\DummyFile.exe"
#Option Version5
#Break On
#Dim All

#Debug Error Off
#Tools Off

%VERSION_MAJOR = 2
%VERSION_MINOR = 0
%VERSION_REVISION = 2

' Version Resource information
#Include ".\DummyFileRes.inc"
'------------------------------------------------------------------------------
'*** Constants ***
'------------------------------------------------------------------------------
' Default line length
%LINE_LENGTH = 80
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
Declare Function CreateRandomFileContent(ByVal qudSize As Quad, Optional ByVal lCRLF As Long, _
   Optional ByVal lLineLength As Long) As String
'------------------------------------------------------------------------------
'*** Variabels ***
'------------------------------------------------------------------------------
Global gqudSize As Quad
Global gsContent As String
Global gsPath As String
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
   Local sUnit, sCommand, sCommand1, sCommand2, sCommand3, sCommand4, sCommand5, sTempfile As String
   Local qudTimes As Quad
   Local lRet, lChunks, lRemain, lCRLf, lLineLength As Long
   Local j As Long

   ' Application intro
   ConHeadline "DummyFile", %VERSION_MAJOR, %VERSION_MINOR, %VERSION_REVISION
   ConCopyright "2007-2017", $COMPANY_NAME
   Print ""

   Trace New ".\DummyFile.tra"
   Trace Off

   If (Len(Trim$(Command$)) < 1) Then
       ShowHelp
       Exit Function
   End If

   ' *** Parse the parameters
   sCommand = Command$

   ' *** Number of files to create
   sCommand1 = Parse$(sCommand, ",", 1)
   lFiles = Val(Trim$(sCommand1))

   Dim hThdID(1 To lFiles) As Long


    '*** Size and unit of size
   sCommand2 = Parse$(sCommand, ",", 2)

   sUnit = LCase$(Right$(sCommand2, 2))

   Select Case sUnit
   Case "kb"
       qudTimes = 1024
   Case "mb"
       qudTimes = 1024^2
   Case "gb"
       qudTimes = 1024^3
   Case Else
       qudTimes = 1
   End Select

   gqudSize = qudTimes * Val(Trim$(sCommand2))

   ' *** Location of file creation
   If ParseCount(sCommand, ",") > 2 Then
      gsPath = Parse$(sCommand, ",", 3)
      If Len(Trim$(gsPath)) < 1 Then
         gsPath = ".\"
      End If
   Else
      gsPath = ".\"
   End If

   ' *** Create a text file with line breaks?
   If ParseCount(sCommand, ",") > 3 Then
      If IsFalse(Val(Trim$(Parse$(sCommand, ",", 4)))) Then
         lCRLF = 0
      Else
         lCRLF = 1
      End If
   End If

   ' *** Line length?
   If ParseCount(sCommand, ",") > 4 Then
      lLineLength = Val(Trim$(Parse$(sCommand, ",", 5)))
   End If

   ' *** Start the file creation
   Print ""
   Print ""
   Print "Preparing file contents ..."

   gsContent = CreateRandomFileContent(gqudSize, lCRLF, lLineLength)

   Print ""
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

   Print ""
   Print "Done!"

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
   Con.StdOut "DummyFile lets you easily create a number of dummy files for various testing scenarios. The files are generated from random Con.StdOutable characters (plain text)."
   Con.StdOut "Per default, the file's contents is generated as one 'big blob'. However, you can specify to add line feeds (actually line feed plus carriage return)"
   Con.StdOut "to simulate 'proper text files'."
   Con.StdOut ""
   Con.StdOut "Usage:"
   Con.StdOut "DummyFile <No. of files>,<file size>,[<folder to create files in>][,<add line feed yes/no (1|0=default)>][,<line length (default = 80 characters)>]"
   Con.StdOut ""
   Con.StdOut "     i.e.: DummyFile 10, 12MB"
   Con.StdOut "              - Create 10 files (in the current folder) with a size of 12MB each, do not add line feed(s)."
   Con.StdOut "           DummyFile 10, 12MB, c:\temp"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, do not add line feed(s)."
   Con.StdOut "           DummyFile 10, 12MB, c:\temp, 1"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, add line feed(s). Line length defaults to 80 characters."
   Con.StdOut "           DummyFile 10, 12MB,, 1"
   Con.StdOut "              - Create 10 files in the current folder with a size of 12MB each, add line feed(s). Line length defaults to 80 characters."
   Con.StdOut "           DummyFile 10, 12MB, c:\temp, 1, 72"
   Con.StdOut "              - Create 10 files in the folder c:\temp with a size of 12MB each, add line feed(s). Line length should be 72 characters."
   Con.StdOut ""
   Con.StdOut "Allowed file size units: <empty> = Byte      i.e. DummyFile 1, 100"
   Con.StdOut "                              kb = Kilobyte  i.e. DummyFile 1, 100kb"
   Con.StdOut "                              mb = Megabyte  i.e. DummyFile 1, 100mb"
   Con.StdOut "                              gb = Gigabyte  i.e. DummyFile 1, 100gb"
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
   Local hFile As Long
   Local sTempfile, sPath As String

   Call EnterCS

   sPath = NormalizePath(gsPath)

   hFile = FreeFile
   sTempfile = CreateTempFileName(ByCopy sPath, "sa", "tmp")

   Open sTempfile For Output As #hFile

   Print "Thread " & Format$(lFile) & ", creating file of " & FormatLoc(gqudSize, "#,") & " byte(s): " & sTempfile

   Print #hFile, gsContent;
   Call LeaveCS

   Close #hFile

End Function
'==============================================================================

Function CreateRandomFileContent(ByVal qudSize As Quad, Optional ByVal lCRLF As Long, _
   Optional ByVal lLineLength As Long) As String
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
   Local sResult As String
   Local s As Byte Ptr
   Local i, s1 As Quad
   Local dwLineCount As Dword

   Trace On

   ' Preallocate memory
   sResult = Space$(qudSize)
   s = StrPtr(sResult)

   ' Seed random
   Randomize qudSize

   For i = 1 To qudSize
       @s = Rnd(48, 122)
       Incr s
   Next i

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

   CreateRandomFileContent = sResult

   Trace Off

End Function
'==============================================================================
