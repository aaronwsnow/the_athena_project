'Option Explicit On
'Option Strict Off
'Option Infer On
'Option Compare Binary

Imports System.Speech.Synthesis
Imports System.Speech.Recognition
Imports System.Threading
Imports System.Runtime.InteropServices
Imports System.Runtime.CompilerServices
Imports Matrix

Imports System.IO
Imports System.Reflection
Imports System.Windows.Threading
Imports System.Drawing.Text

Public Structure Language
    Public Const CSharp As String = "CSharp"
    Public Const VBCode As String = "VisualBasic"
    Public Const JScript As String = "JScript"
End Structure

Public Class Speech

    Private WithEvents SpeechSynth As New SpeechSynthesizer()
    Public Event Viseme(ByVal vis As Integer, ByVal duration As Double)
    Public Event SpeechProgress(ByVal charpos As Integer, ByVal word As String, ByVal percentComplete As Integer)
    Public Event TextProgress(ByVal charpos As Integer, ByVal word As String, ByVal percentComplete As Integer)
    Public Event IsFinished()

    Private Shared SpeechShared As New SpeechSynthesizer
    Public Shared ReadOnly Property Voices As List(Of String)
        Get
            Dim allVoices As New List(Of String)
            For Each vs In SpeechShared.GetInstalledVoices()
                allVoices.Add(vs.VoiceInfo.Name())
            Next
            Return allVoices
        End Get
    End Property

    Public Property CurrentVoice As String
        Get
            Return SpeechSynth.Voice().Name()
        End Get
        Set(value As String)
            SpeechSynth.SpeakAsyncCancelAll()
            SpeechSynth.SelectVoice(value)
        End Set
    End Property

    Public Property CurrentRate As Integer
        Get
            Return SpeechSynth.Rate()
        End Get
        Set(value As Integer)
            SpeechSynth.SpeakAsyncCancelAll()
            SpeechSynth.Rate() = value
        End Set
    End Property

    Private m_currentText As String = ""
    Public Property CurrentText() As String
        Get
            Return m_currentText
        End Get
        Set(value As String)
            m_currentText = Clean(value)
        End Set
    End Property

    Public Property CurrentPosition() As Integer = 0

    Public Property ReadLargeText() As Boolean = False

    Private Property CurrentPostionDifference() As Integer = 0

    Public Sub Speak()
        Dim Mid = Utility.Mid(CurrentText(), CurrentPosition() + 1, CurrentText().Length())
        CurrentPostionDifference() = CurrentText().Length() - Mid.Length()

        If ReadLargeText() = True Then
            Dim iarray = Utility.ToArray(Mid, ".")

            For Each line In iarray
                SpeechSynth.SpeakAsync(line)
            Next
        Else
            SpeechSynth.SpeakAsync(Mid)
        End If
    End Sub

    Public Sub Speak(ByVal text As String)
        CurrentText() = text
        Speak()
    End Sub

    Public Sub Speak(ByVal text As String, ByVal rate As Integer)
        CurrentText() = text
        CurrentRate() = rate
        Speak()
    End Sub

    Public Sub Speak(ByVal voice As String, ByVal text As String, ByVal rate As Integer)
        CurrentText() = text
        CurrentRate() = rate
        CurrentVoice() = voice
        Speak()
    End Sub

    Public Sub Speak(ByVal voice As Integer, ByVal text As String, ByVal rate As Integer)
        CurrentText() = text
        CurrentRate() = rate

        If voice < Voices().Count And voice > -1 Then
            CurrentVoice() = Voices(voice)
        End If

        Speak()
    End Sub

    Public Function IsSpeaking() As Boolean
        If SpeechSynth.State = SynthesizerState.Speaking Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub StopSpeech()
        SpeechSynth.Resume()
        SpeechSynth.SpeakAsyncCancelAll()
        Do Until IsSpeaking() = False : Loop
    End Sub

    Public Sub Reset()
        StopSpeech()
        CurrentPosition() = 0
    End Sub

    Public Sub Clear()
        StopSpeech()
        CurrentPosition() = 0
        CurrentText() = ""
    End Sub

    Private Function Clean(ByVal newText As String) As String
        Return Utility.Replace(newText, vbCrLf, "  ")
    End Function

    Private Sub VisemeReached(ByVal sender As Object, ByVal e As VisemeReachedEventArgs) Handles SpeechSynth.VisemeReached
        RaiseEvent Viseme(e.Viseme, e.Duration.TotalMilliseconds)
    End Sub

    Private Sub Speech_Progress(ByVal sender As Object, ByVal e As SpeakProgressEventArgs) Handles SpeechSynth.SpeakProgress
        Dim percentComplete = (e.CharacterPosition + e.CharacterCount) * 100 / CurrentText().Length()
        RaiseEvent SpeechProgress(e.CharacterPosition, e.CharacterCount, percentComplete)
    End Sub

    Private Sub Text_Progress(ByVal sender As Object, ByVal e As SpeakProgressEventArgs) Handles SpeechSynth.SpeakProgress
        Dim percentComplete = ((CurrentPosition() + e.CharacterPosition + e.CharacterCount) * 100 / CurrentText.Length())
        RaiseEvent TextProgress(CurrentPostionDifference() + e.CharacterPosition, e.CharacterCount, percentComplete)
    End Sub

    Private Sub SpeakCompleted(ByVal sender As Object, ByVal e As SpeakCompletedEventArgs) Handles SpeechSynth.SpeakCompleted
        RaiseEvent IsFinished()
    End Sub

    Public Sub ResumeSpeech()
        SpeechSynth.Resume()
    End Sub

    Public Sub PauseSpeech()
        SpeechSynth.Pause()
    End Sub

End Class

Public Class Recognition

    Private WithEvents recognizer As New SpeechRecognitionEngine(New Globalization.CultureInfo("en-US"))
    Public Event Print(ByVal word As String)
    Public Event Alternates(ByVal words As List(Of String))

    Public Property LocalGrammer() As New List(Of String)

    Public Sub Load(ByVal aGrammer As String())
        LocalGrammer.Clear()
        LocalGrammer.AddRange(aGrammer)

        recognizer.UnloadAllGrammars()

        Dim aChoices As New Choices()
        aChoices.Add(aGrammer)

        Dim newGrammer As New Grammar(aChoices.ToGrammarBuilder())
        recognizer.LoadGrammar(newGrammer)

        recognizer.RequestRecognizerUpdate()
        recognizer.SetInputToDefaultAudioDevice()
    End Sub
    Public Sub Load()
        recognizer.UnloadAllGrammars()
        recognizer.LoadGrammar(New DictationGrammar())
        recognizer.RequestRecognizerUpdate()
        recognizer.SetInputToDefaultAudioDevice()
    End Sub

    Public Sub [Start]()
        recognizer.RecognizeAsync(RecognizeMode.Multiple)
    End Sub
    Public Sub [Stop]()
        recognizer.RecognizeAsyncStop()
    End Sub

    Private Sub PrintWords(ByVal sender As Object, ByVal e As SpeechRecognizedEventArgs) Handles recognizer.SpeechRecognized
        RaiseEvent Print(e.Result.Text)
    End Sub
    Private Sub alternateWords(ByVal sender As Object, ByVal e As SpeechRecognitionRejectedEventArgs) Handles recognizer.SpeechRecognitionRejected
        Dim results As New List(Of String)

        For Each alt In e.Result.Alternates
            results.Add(alt.Text())
        Next
        RaiseEvent Alternates(results)
    End Sub

End Class

Public Class Taskman
    Private Shared Gate As New Matrix.Gate
    Public Shared Property Tasks() As New List(Of Thread)

#Region "Memory"
    'This is used for tasks to communicate publically

    Private Shared Property TaskMemory() As New TypeMemory
    Private Const DELIM As String = "@"
    Public Const EXIT_TAG As String = "<exit>"
    Public Const MESSAGE_TAG As String = "<message>"

    Public Shared WriteOnly Property Input(ByVal ID As String, ByVal variable As String)
        Set(value)
            TaskMemory.Index(ID, variable, Nothing) = value
        End Set
    End Property

    Public Shared ReadOnly Property Output(ByVal ID As String, ByVal variable As String, ByVal Defaults As Object) As Object
        Get
            Return TaskMemory.Index(ID, variable, Defaults)
        End Get
    End Property

    'a way of stopping a thread via a message
    Public Shared Sub StopTask(ByVal ID As String)
        TaskMemory.Index(ID, EXIT_TAG, False) = True
    End Sub
    Public Shared Sub StopTask()
        For Each task In Tasks
            TaskMemory.Index(task.Name(), EXIT_TAG, False) = True
        Next
    End Sub

    'read by task
    Public Shared ReadOnly Property IsExit(ByVal ID As String) As Boolean
        Get
            Return TaskMemory.Index(ID, EXIT_TAG, False)
        End Get
    End Property

    Public Shared Property Message(ByVal ID As String) As String
        Get
            Return TaskMemory.Index(ID, MESSAGE_TAG, Nothing)
        End Get
        Set(value As String)
            TaskMemory.Index(ID, MESSAGE_TAG, Nothing) = value
        End Set
    End Property

#End Region


    Public Shared Function IsRunning(ByVal ID As String) As Boolean
        For Each task In Tasks
            If Utility.Search(task.Name(), ID, Gate.eEQUALS) = True Then
                Return task.IsAlive()
            End If
        Next
        Return False
    End Function

    Public Shared Sub Add(ByVal ID As String, ByVal Funct As ThreadStart)
        Dim start As New Thread(Funct)
        start.Name() = ID
        start.IsBackground = True
        Tasks.Add(start)
    End Sub

    Public Shared Sub Remove(ByVal ID As String)
        Dim newList As New List(Of Thread)
        For Each task In Tasks
            If Utility.Search(task.Name(), ID, Gate.eEQUALS) = False Then
                newList.Add(task)
            End If
        Next

        Tasks() = newList
        TaskMemory.Remove(ID, Gate.eSTART + DELIM)
    End Sub
    Public Shared Sub Clear()
        Tasks.Clear()
        TaskMemory.Clear()
    End Sub

    Public Shared Sub Start(ByVal ID As String, ByVal Parims As Object)
        For Each task In Tasks
            If Utility.Search(task.Name(), ID, Gate.eEQUALS) = True Then
                task.Start(Parims)
            End If
        Next
    End Sub
    Public Shared Sub Start(ByVal ID As String, ByVal Parims As Object, ByVal timeLimit As Integer)
        For Each task In Tasks
            If Utility.Search(task.Name(), ID, Gate.eEQUALS) = True Then
                task.Start(Parims)
            End If
        Next

        Dim time As New Thread(Sub()
                                   Utility.Sleep(timeLimit)
                                   StopTask(ID)
                               End Sub)
        time.Start()

    End Sub

    Public Shared Sub Start(ByVal Parims As Object)
        For Each task In Tasks
            task.Start(Parims)
        Next
    End Sub
    Public Shared Sub Start(ByVal Parims As Object, ByVal timeLimit As Integer)
        For Each task In Tasks
            task.Start(Parims)
        Next

        Dim time As New Thread(Sub()
                                   Utility.Sleep(timeLimit)
                                   StopTask()
                               End Sub)
        time.Start()

    End Sub

    Public Shared Sub Start()
        For Each task In Tasks
            task.Start()
        Next
    End Sub

    Public Shared Sub Start(ByVal timeLimit As Integer)
        For Each task In Tasks
            task.Start()
        Next

        Dim time As New Thread(Sub()
                                   Utility.Sleep(timeLimit)
                                   StopTask()
                               End Sub)
        time.Start()

    End Sub

    Public Shared Sub [Stop](ByVal ID As String)
        For Each task In Tasks
            If Utility.Search(task.Name(), ID, Gate.eEQUALS) = True Then
                task.Abort()
            End If
        Next
    End Sub
    Public Shared Sub [Stop]()
        For Each task In Tasks
            task.Abort()
        Next
    End Sub

    Public Shared ReadOnly Property CurrentTasks() As List(Of String)
        Get
            Dim newList As New List(Of String)
            For Each task In Tasks
                newList.Add(task.Name())
            Next
            Return newList
        End Get
    End Property
    Public Shared ReadOnly Property TaskCount() As Integer
        Get
            Return Tasks.Count()
        End Get
    End Property


End Class

Public Class Bitmaps
    Implements IDisposable

    Public Property Bitmap() As System.Drawing.Bitmap
        Get
            Return Source
        End Get
        Set(value As System.Drawing.Bitmap)
            Source = value
        End Set
    End Property
    Public Property BitmapImage() As System.Windows.Media.Imaging.BitmapImage
        Get
            Return BitmapToBitmapImage(Source)
        End Get
        Set(value As System.Windows.Media.Imaging.BitmapImage)
            Source = BitmapImageToBitmap(value)
        End Set
    End Property

    Private Property Source() As System.Drawing.Bitmap = Nothing
    Public Property Iptr() = IntPtr.Zero
    Public Property bitmapData() As System.Drawing.Imaging.BitmapData = Nothing

    Public Property Pixels() As Byte()
    Public Property Depth() As Integer
    Public Property Width() As Integer
    Public Property Height() As Integer

    Private Property Steps() As Integer = 0
    Public Shared Property DefaultImageFormat() As System.Drawing.Imaging.ImageFormat = System.Drawing.Imaging.ImageFormat.Png

    Public Sub New(ByVal source As System.Windows.Media.Imaging.BitmapImage)
        Me.BitmapImage() = source
        LockBits()
    End Sub

    Public Sub New(ByVal source As System.Drawing.Bitmap)
        Me.Bitmap() = source
        LockBits()
    End Sub

    Public Sub LockBits()
        Try
            'Get width And height of bitmap
            Width = Source.Width
            Height = Source.Height

            'get total locked pixels count
            Dim PixelCount = Width * Height

            'Create rectangle to lock
            Dim Rect = New System.Drawing.Rectangle(0, 0, Width, Height)

            'get source bitmap pixel format size
            Depth = System.Drawing.Bitmap.GetPixelFormatSize(Source.PixelFormat)

            'Check if bpp (Bits Per Pixel) Is 8, 24, Or 32
            If (Depth <> 8 And Depth <> 24 And Depth <> 32) Then
                Throw New ArgumentException("Only 8, 24 and 32 bpp images are supported.")
            End If

            'Lock bitmap And return bitmap data
            bitmapData = Source.LockBits(Rect, System.Drawing.Imaging.ImageLockMode.ReadWrite, Source.PixelFormat)

            'create byte array to copy pixel values
            Steps = Depth / 8
            ReDim Pixels(PixelCount * Steps)

            Iptr = bitmapData.Scan0

            'Copy data from pointer to array
            System.Runtime.InteropServices.Marshal.Copy(Iptr, Pixels, 0, Pixels.Length - 1)

        Catch ex As Exception
            Throw ex
        End Try

    End Sub

    Public Sub UnlockBits()
        Try
            'Copy data from byte array to pointer
            System.Runtime.InteropServices.Marshal.Copy(Pixels, 0, Iptr, Pixels.Length - 1)

            'Unlock bitmap data
            Source.UnlockBits(bitmapData)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub


    Public Function GetPixel(ByVal x As Integer, ByVal y As Integer) As Color

        Dim clr As Color

        'Get color components count
        'Get start index of the specified pixel
        Dim i = ((y * Width) + x) * Steps

        If (i > Pixels.Length - Steps) Then
            Return Nothing
            'Throw New IndexOutOfRangeException()
        End If
        If (Depth = 32) Then 'For 32 bpp get Red, Green, Blue And Alpha
            Dim b = Pixels(i)
            Dim g = Pixels(i + 1)
            Dim r = Pixels(i + 2)
            Dim a = Pixels(i + 3) 'a
            clr = Color.FromArgb(a, r, g, b)
        End If
        If (Depth = 24) Then 'For 24 bpp get Red, Green And Blue
            Dim b = Pixels(i)
            Dim g = Pixels(i + 1)
            Dim r = Pixels(i + 2)
            clr = Color.FromArgb(255, r, g, b)
        End If

        If (Depth = 8) Then
            'For 8 bpp get color value (Red, Green And Blue values are the same)
            Dim c = Pixels(i)
            clr = Color.FromArgb(255, c, c, c)
        End If

        Return clr
    End Function

    Public Sub SetPixel(ByVal x As Integer, ByVal y As Integer, ByVal Color As Color)
        'Get color components count
        'Get start index of the specified pixel
        Dim i = ((y * Width) + x) * Steps

        If (i > Pixels.Length - Steps) Then
            Exit Sub
            'Throw New IndexOutOfRangeException()
        End If

        If (Depth = 32) Then 'For 32 bpp set Red, Green, Blue And Alpha
            Pixels(i) = Color.B
            Pixels(i + 1) = Color.G
            Pixels(i + 2) = Color.R
            Pixels(i + 3) = Color.A
        End If
        If (Depth = 24) Then 'For 24 bpp set Red, Green And Blue
            Pixels(i) = Color.B
            Pixels(i + 1) = Color.G
            Pixels(i + 2) = Color.R
        End If
        If (Depth = 8) Then 'For 8 bpp set color value (Red, Green And Blue values are the same)
            Pixels(i) = Color.B
        End If
    End Sub


#Region "Convert Image"

    Public Shared Function BitmapImageToBitmap(ByVal bmi As BitmapImage) As System.Drawing.Bitmap
        Dim ms = New System.IO.MemoryStream()

        Using ms
            Dim enc = New PngBitmapEncoder()
            enc.Frames.Add(BitmapFrame.Create(bmi))
            enc.Save(ms)
            Return New System.Drawing.Bitmap(ms)
        End Using
    End Function

    Public Shared Function BitmapToBitmapImage(ByVal bitmap As System.Drawing.Bitmap) As BitmapImage
        Dim ms = New System.IO.MemoryStream()

        Using ms
            bitmap.Save(ms, DefaultImageFormat())
            ms.Position = 0
            Dim bmi As New BitmapImage
            bmi.BeginInit()
            bmi.StreamSource = ms
            bmi.CacheOption = BitmapCacheOption.OnLoad
            bmi.EndInit()
            Return bmi
        End Using
    End Function


    Public Shared Function BitmapCopy(ByVal bitmap As System.Drawing.Bitmap) As System.Drawing.Bitmap
        Dim ms = New System.IO.MemoryStream()

        Using ms
            bitmap.Save(ms, DefaultImageFormat())
            Return New System.Drawing.Bitmap(ms)
        End Using
    End Function

    Public Shared Function BitmapImageCopy(ByVal bmi As BitmapImage) As BitmapImage
        Dim ms = New System.IO.MemoryStream()

        Using ms
            Dim enc = New PngBitmapEncoder()
            enc.Frames.Add(BitmapFrame.Create(bmi))
            enc.Save(ms)
            ms.Position = 0

            Dim newBmi As New BitmapImage
            newBmi.BeginInit()
            newBmi.StreamSource = ms
            newBmi.CacheOption = BitmapCacheOption.OnLoad
            newBmi.EndInit()
            Return newBmi
        End Using
    End Function

#End Region

#Region "Get Image"

    Public Shared Function GetBitmapFromFile(ByVal file As String) As System.Drawing.Bitmap
        Return System.Drawing.Image.FromFile(file)
    End Function

    Public Shared Function GetBitmapImageFromFile(ByVal file As String) As BitmapImage
        Return New BitmapImage(New Uri(file, UriKind.Absolute))
    End Function

    Public Shared Function GetBitmapImageFromStream(ByVal ms As System.IO.Stream) As System.Windows.Media.Imaging.BitmapImage
        Using ms
            Dim bmi As New BitmapImage
            bmi.BeginInit()
            bmi.StreamSource = ms
            bmi.CacheOption = BitmapCacheOption.OnLoad
            bmi.EndInit()
            Return bmi
        End Using
    End Function

    Public Shared Function GetBitmapFromStream(ByVal ms As System.IO.Stream) As System.Drawing.Bitmap
        Using ms
            Return System.Drawing.Image.FromStream(ms)
        End Using
    End Function

    Public Shared Function GetBytesFromBitmap(ByVal bitmap As System.Drawing.Bitmap) As Byte()
        Dim ms = New System.IO.MemoryStream()
        Using ms
            bitmap.Save(ms, DefaultImageFormat())
            Return ms.ToArray()
        End Using
    End Function

    Public Shared Function GetBitmapFromBytes(ByVal bitmaparray As Byte()) As System.Drawing.Bitmap
        Dim ms = New System.IO.MemoryStream(bitmaparray)
        Using ms
            Return System.Drawing.Image.FromStream(ms)
        End Using
    End Function

    Public Shared Function GetBytesFromBitmapImage(ByVal bmi As BitmapImage) As Byte()
        Dim ms = New System.IO.MemoryStream()
        Using ms
            Dim enc = New PngBitmapEncoder
            enc.Frames.Add(BitmapFrame.Create(bmi))
            enc.Save(ms)
            Return ms.ToArray()
        End Using
    End Function

    Public Shared Function GetBitmapImageFromBytes(ByVal bmiarray As Byte()) As BitmapImage
        Dim ms = New IO.MemoryStream(bmiarray)
        Using ms
            Dim bmi As New BitmapImage
            bmi.BeginInit()
            bmi.StreamSource = ms
            bmi.CacheOption = BitmapCacheOption.OnLoad
            bmi.EndInit()
            Return bmi
        End Using
    End Function

#End Region

#Region "Resize Image"

    Public Shared Function ResizeBitmapImageByMul(ByVal oldBitmapImage As BitmapImage, ByVal multiplier As Double) As BitmapImage
        Return BitmapToBitmapImage(ResizeBitmapByMul(BitmapImageToBitmap(oldBitmapImage), multiplier))
    End Function

    Public Shared Function ResizeBitmapImageByMul(ByVal oldBitmapImage As BitmapImage, ByVal multiplierX As Double, ByVal multiplierY As Double) As BitmapImage
        Return BitmapToBitmapImage(ResizeBitmapByMul(BitmapImageToBitmap(oldBitmapImage), multiplierX, multiplierY))
    End Function


    Public Shared Function ResizeBitmapImageByDpi(ByVal oldBitmapImage As BitmapImage, ByVal idealWidthHeight As Integer) As BitmapImage
        Return BitmapToBitmapImage(ResizeBitmapByDpi(BitmapImageToBitmap(oldBitmapImage), idealWidthHeight))
    End Function

    Public Shared Function ResizeBitmapImageByDpi(ByVal oldBitmapImage As BitmapImage, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As BitmapImage
        Return BitmapToBitmapImage(ResizeBitmapByDpi(BitmapImageToBitmap(oldBitmapImage), idealWidth, idealHeight))
    End Function


    Public Shared Function ResizeBitmapByMul(ByVal oldBitmap As System.Drawing.Bitmap, ByVal multiplier As Double) As System.Drawing.Bitmap
        Return ResizeBitmapByMul(oldBitmap, multiplier, multiplier)
    End Function

    Public Shared Function ResizeBitmapByMul(ByVal oldBitmap As System.Drawing.Bitmap, ByVal multiplierX As Double, ByVal multiplierY As Double) As System.Drawing.Bitmap

        If multiplierX = 0 And 1 <= multiplierY Then

            Return ResizeBitmapToHigherByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf 1 <= multiplierX And multiplierY = 0 Then

            Return ResizeBitmapToHigherByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf 1 > multiplierX And multiplierY = 0 Then

            Return ResizeBitmapToLowerByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf multiplierX = 0 And 1 > multiplierY Then

            Return ResizeBitmapToLowerByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf 1 <= multiplierX And 1 <= multiplierY Then

            Return ResizeBitmapToHigherByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf 1 > multiplierX And 1 > multiplierY Then

            Return ResizeBitmapToLowerByMul(oldBitmap, multiplierX, multiplierY)
        ElseIf 1 > multiplierX And 1 <= multiplierY Then

            Dim bm = ResizeBitmapToLowerByMul(oldBitmap, multiplierX, 1)
            Return ResizeBitmapToHigherByMul(bm, 1, multiplierY)
        ElseIf 1 <= multiplierX And 1 > multiplierY Then

            Dim bm = ResizeBitmapToLowerByMul(oldBitmap, 1, multiplierY)
            Return ResizeBitmapToHigherByMul(bm, multiplierX, 1)
        Else

            Return oldBitmap
        End If
    End Function


    Public Shared Function ResizeBitmapByDpi(ByVal oldBitmap As System.Drawing.Bitmap, ByVal idealWidthHeight As Integer) As System.Drawing.Bitmap
        Return ResizeBitmapByDpi(oldBitmap, idealWidthHeight, idealWidthHeight)
    End Function

    Public Shared Function ResizeBitmapByDpi(ByVal oldBitmap As System.Drawing.Bitmap, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As System.Drawing.Bitmap
        Dim width = oldBitmap.Width
        Dim height = oldBitmap.Height

        If idealWidth = 0 And height <= idealHeight Then

            Return ConvertBitmapToHigher(oldBitmap, idealWidth, idealHeight)
        ElseIf idealWidth = 0 And height > idealHeight Then

            Return ResizeBitmapToLower(oldBitmap, idealWidth, idealHeight)
        ElseIf width > idealWidth And idealHeight = 0 Then

            Return ResizeBitmapToLower(oldBitmap, idealWidth, idealHeight)
        ElseIf width <= idealWidth And idealHeight = 0 Then

            Return ConvertBitmapToHigher(oldBitmap, idealWidth, idealHeight)
        ElseIf width <= idealWidth And height <= idealHeight Then

            Return ConvertBitmapToHigher(oldBitmap, idealWidth, idealHeight)
        ElseIf width > idealWidth And height > idealHeight Then

            Return ResizeBitmapToLower(oldBitmap, idealWidth, idealHeight)
        ElseIf width > idealWidth And height <= idealHeight Then

            Dim bm = ResizeBitmapToLower(oldBitmap, idealWidth, 1)
            Return ConvertBitmapToHigher(bm, 1, idealHeight)
        ElseIf width <= idealWidth And height > idealHeight Then

            Dim bm = ResizeBitmapToLower(oldBitmap, 1, idealHeight)
            Return ConvertBitmapToHigher(bm, idealWidth, 1)
        Else

            Return oldBitmap
        End If
    End Function


    Private Shared Function ResizeBitmapToHigherByMul(ByVal oldBitmap As System.Drawing.Bitmap, ByVal multiplierX As Integer, ByVal multiplierY As Integer) As System.Drawing.Bitmap

        Dim width = 0
        Dim height = 0

        'This is set up so that a picture will maintain it's proportions if the 
        'idealValue Is higher than the actual picture width or height

        If 0 >= multiplierX And 1 < multiplierY Then

            multiplierX = multiplierY
            width = oldBitmap.Width * multiplierX
            height = oldBitmap.Height * multiplierY
        ElseIf 1 < multiplierX And 0 >= multiplierY Then

            multiplierY = multiplierX
            width = oldBitmap.Width * multiplierX
            height = oldBitmap.Height * multiplierY
        ElseIf 1 < multiplierX And 1 < multiplierY Then

            width = oldBitmap.Width * multiplierX
            height = oldBitmap.Height * multiplierY
        ElseIf 1 >= multiplierX And 1 >= multiplierY Then
            Return oldBitmap
        End If

        Dim newBitmap As New System.Drawing.Bitmap(width, height, oldBitmap.PixelFormat)

        Dim old_lockBitmap = New Bitmaps(oldBitmap)
        Dim new_lockBitmap = New Bitmaps(newBitmap)

        Using old_lockBitmap
            Using new_lockBitmap

                Dim currentX = 0
                Dim currentY = 0

                For y = 0 To old_lockBitmap.Height - 1
                    For x = 0 To old_lockBitmap.Width - 1

                        Dim color = old_lockBitmap.GetPixel(x, y)
                        currentX = x * multiplierX
                        currentY = y * multiplierY

                        For nY = currentY To currentY + multiplierY - 1
                            For nX = currentX To currentX + multiplierX - 1
                                new_lockBitmap.SetPixel(nX, nY, color)
                            Next
                        Next
                    Next
                Next
            End Using
        End Using


        Return new_lockBitmap.Bitmap
    End Function

    Private Shared Function ConvertBitmapToHigher(ByVal oldBitmap As System.Drawing.Bitmap, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As System.Drawing.Bitmap
        Dim width = 0
        Dim height = 0

        If idealWidth = 0 Then
            width = 0
        Else
            width = idealWidth / oldBitmap.Width
        End If

        If idealHeight = 0 Then
            height = 0
        Else
            height = idealHeight / oldBitmap.Height
        End If

        Return ResizeBitmapToHigherByMul(oldBitmap, width, height)
    End Function

    Private Shared Function ResizeBitmapToLower(ByVal oldBitmap As System.Drawing.Bitmap, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As System.Drawing.Bitmap

        Dim ideal_skipX = 0
        Dim ideal_skipY = 0

        '1000 Original
        '10 Ideal
        'take 1 pixel every 100 and you will end up with 10 pixels
        '1000/10 = 100 ideal_skip

        'This is set up so that a picture will maintain it's proportions if the 
        'idealValue Is lower than the actual picture width or height

        If oldBitmap.Width <= idealWidth And oldBitmap.Height > idealHeight Then
            ideal_skipY = Math.Round(oldBitmap.Height / idealHeight)
            ideal_skipX = Math.Round(oldBitmap.Width / idealHeight)
        ElseIf oldBitmap.Width > idealWidth And oldBitmap.Height <= idealHeight Then
            ideal_skipX = Math.Round(oldBitmap.Width / idealWidth)
            ideal_skipY = Math.Round(oldBitmap.Height / idealWidth)
        ElseIf oldBitmap.Width > idealWidth And oldBitmap.Height > idealHeight Then
            ideal_skipX = Math.Round(oldBitmap.Width / idealWidth)
            ideal_skipY = Math.Round(oldBitmap.Height / idealHeight)
        ElseIf oldBitmap.Width <= idealWidth And oldBitmap.Height <= idealHeight Then
            Return oldBitmap
        End If


        'Find suitable width and height closest to idealWidth and idealHeight
        Dim newWidth = 0
        Dim newHeight = 0
        For i = 0 To oldBitmap.Width - 1
            If i Mod (ideal_skipX) = 0 Then newWidth += 1
        Next
        For i = 0 To oldBitmap.Height - 1
            If i Mod (ideal_skipY) = 0 Then newHeight += 1
        Next

        Dim newBitmap As New System.Drawing.Bitmap(newWidth, newHeight, oldBitmap.PixelFormat)

        Dim old_lockBitmap = New Bitmaps(oldBitmap)
        Dim new_lockBitmap = New Bitmaps(newBitmap)

        Using old_lockBitmap
            Using new_lockBitmap

                Dim currentX = 0
                Dim currentY = 0

                For y = 0 To old_lockBitmap.Height - 1
                    If (y Mod (ideal_skipY) = 0) Then

                        For x = 0 To old_lockBitmap.Width - 1
                            If (x Mod (ideal_skipX) = 0) Then
                                Dim color = old_lockBitmap.GetPixel(x, y)
                                new_lockBitmap.SetPixel(currentX, currentY, color)
                                currentX += 1
                            End If
                        Next

                        currentX = 0
                        currentY += 1

                    End If
                Next

            End Using
        End Using

        Return new_lockBitmap.Bitmap
    End Function

    Private Shared Function ResizeBitmapToLowerByMul(ByVal oldBitmap As System.Drawing.Bitmap, ByVal multiplierX As Double, ByVal multiplierY As Double) As System.Drawing.Bitmap
        Return ResizeBitmapToLower(oldBitmap, oldBitmap.Width * multiplierX, oldBitmap.Height * multiplierY)
    End Function


    Public Shared Function CreateResizedImage(ByVal source As ImageSource, ByVal width As Integer, ByVal height As Integer, ByVal margin As Integer) As ImageSource

        If width = 0 Then
            width = source.Width * height / source.Height
        ElseIf height = 0 Then
            height = source.Height * width / source.Width
        End If

        Dim rect = New Rect(margin, margin, width - margin * 2, height - margin * 2)

        Dim group = New DrawingGroup()
        RenderOptions.SetBitmapScalingMode(group, BitmapScalingMode.NearestNeighbor)
        RenderOptions.SetEdgeMode(group, EdgeMode.Aliased)

        group.Children.Add(New ImageDrawing(source, rect))

        Dim DrawingVisual = New DrawingVisual()
        Dim drawingContext = DrawingVisual.RenderOpen()
        Using drawingContext
            drawingContext.DrawDrawing(group)
        End Using

        Dim resizedImage = New RenderTargetBitmap(
        width, height,         ' Resized dimensions
        96, 96,                ' Default DPI values
        PixelFormats.Default)  ' Default pixel format
        resizedImage.Render(DrawingVisual)

        Dim ms = New System.IO.MemoryStream()

        Using ms
            Dim enc = New PngBitmapEncoder()
            enc.Frames.Add(BitmapFrame.Create(resizedImage))
            enc.Save(ms)

            ms.Position = 0
            Dim bmi As New BitmapImage
            bmi.BeginInit()
            bmi.StreamSource = ms
            bmi.CacheOption = BitmapCacheOption.OnLoad
            bmi.EndInit()
            Return bmi
        End Using

    End Function

    Public Shared Function CreateTextImage(ByVal source As ImageSource, ByVal text As String, ByVal location As Point, ByVal size As Integer, ByVal brush As Brush, ByVal typeFace As Typeface) As ImageSource

        Dim rect = New Rect(0, 0, source.Width, source.Height)

        Dim group = New DrawingGroup()
        RenderOptions.SetBitmapScalingMode(group, BitmapScalingMode.NearestNeighbor)
        RenderOptions.SetEdgeMode(group, EdgeMode.Aliased)

        Dim DrawingVisual = New DrawingVisual()
        Dim drawingContext = DrawingVisual.RenderOpen()
        Using drawingContext

            Dim text1 As New FormattedText(text,
                                           Globalization.CultureInfo.InvariantCulture,
                                           FlowDirection.LeftToRight,
                                           typeFace,
                                           size,
                                           brush)

            drawingContext.DrawImage(source, rect)
            drawingContext.DrawText(text1, location)
        End Using

        Dim resizedImage = New RenderTargetBitmap(
        source.Width, source.Height,         ' Resized dimensions
        96, 96,                ' Default DPI values
        PixelFormats.Default)  ' Default pixel format
        resizedImage.Render(DrawingVisual)

        Dim ms = New System.IO.MemoryStream()

        Using ms
            Dim enc = New PngBitmapEncoder()
            enc.Frames.Add(BitmapFrame.Create(resizedImage))
            enc.Save(ms)

            ms.Position = 0
            Dim bmi As New BitmapImage
            bmi.BeginInit()
            bmi.StreamSource = ms
            bmi.CacheOption = BitmapCacheOption.OnLoad
            bmi.EndInit()
            Return bmi
        End Using

    End Function


    Public Shared Function CreateResizedImageBytes(ByVal source As ImageSource, ByVal width As Integer, ByVal height As Integer, ByVal margin As Integer) As Byte()

        If width = 0 Then
            width = source.Width * height / source.Height
        ElseIf height = 0 Then
            height = source.Height * width / source.Width
        End If

        Dim rect = New Rect(margin, margin, width - margin * 2, height - margin * 2)

        Dim group = New DrawingGroup()
        RenderOptions.SetBitmapScalingMode(group, BitmapScalingMode.NearestNeighbor)
        RenderOptions.SetEdgeMode(group, EdgeMode.Aliased)

        group.Children.Add(New ImageDrawing(source, rect))

        Dim DrawingVisual = New DrawingVisual()
        Dim drawingContext = DrawingVisual.RenderOpen()
        Using drawingContext
            drawingContext.DrawDrawing(group)
        End Using

        Dim resizedImage = New RenderTargetBitmap(
        width, height,         ' Resized dimensions
        96, 96,                ' Default DPI values
        PixelFormats.Default)  ' Default pixel format
        resizedImage.Render(DrawingVisual)

        Dim ms = New System.IO.MemoryStream()

        Using ms
            Dim enc = New PngBitmapEncoder()
            enc.Frames.Add(BitmapFrame.Create(resizedImage))
            enc.Save(ms)
            Return ms.ToArray
        End Using

    End Function


    Public Shared Sub FlipBitmap(ByRef bitmap As System.Drawing.Bitmap, ByRef direction As System.Drawing.RotateFlipType)
        bitmap.RotateFlip(direction)
    End Sub

    Public Shared Sub FlipBitmapImage(ByRef bmi As BitmapImage, ByRef direction As System.Drawing.RotateFlipType)
        Dim bm = BitmapImageToBitmap(bmi)
        FlipBitmap(bm, direction)
        bmi = BitmapToBitmapImage(bm)
    End Sub


    Public Shared Function FlipBitmapImage(ByVal bmi As BitmapImage, ByVal arg1 As Integer, ByVal arg2 As Integer, ByVal arg3 As Integer, ByVal arg4 As Integer) As BitmapImage
        'Haven't gotten it to work yet
        Return New TransformedBitmap(bmi.Clone, New ScaleTransform(arg1, arg2, arg3, arg4)).Source

        'Dim tempImage As New TransformedBitmap
        'tempImage.BeginInit()
        'tempImage.Source = bmi
        'tempImage.Transform = New ScaleTransform(arg1, arg2, arg3, arg4) 'Dim transform = New RotateTransform(180)
        'tempImage.EndInit()
        'Return tempImage.Source
    End Function




#End Region

#Region "Create Image"

    Public Shared Function CreateBitmapImage(ByVal width As Integer, ByVal height As Integer) As BitmapImage
        Return BitmapToBitmapImage(CreateBitmap(width, height))
    End Function

    Public Shared Function CreateBitmapImage(ByVal width As Integer, ByVal height As Integer, ByVal color As Color) As BitmapImage
        Return BitmapToBitmapImage(CreateBitmap(width, height, color))
    End Function

    Public Shared Function CreateBitmapImage(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color) As BitmapImage
        Return BitmapToBitmapImage(CreateBitmap(width, height, color))
    End Function

    Public Shared Function CreateBitmapImage(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As BitmapImage
        Return BitmapToBitmapImage(CreateBitmap(width, height, color, idealWidth, idealHeight))
    End Function

    Public Shared Function CreateBitmapImage(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color, ByVal idealWidth As Integer, ByVal idealHeight As Integer, ByVal repeatPattern As Boolean) As BitmapImage
        Return BitmapToBitmapImage(CreateBitmap(width, height, color, idealWidth, idealHeight, repeatPattern))
    End Function



    Public Shared Function CreateBitmap(ByVal width As Integer, ByVal height As Integer) As System.Drawing.Bitmap
        Return New System.Drawing.Bitmap(width, height)
    End Function

    Public Shared Function CreateBitmap(ByVal width As Integer, ByVal height As Integer, ByVal color As Color) As System.Drawing.Bitmap
        Dim newBitLock As New Bitmaps(New System.Drawing.Bitmap(width, height))
        Using newBitLock
            For y = 0 To height - 1
                For x = 0 To width - 1
                    newBitLock.SetPixel(x, y, color)
                Next
            Next
        End Using

        Return newBitLock.Bitmap
    End Function

    Public Shared Function CreateBitmap(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color) As System.Drawing.Bitmap
        Dim newBitLock As New Bitmaps(New System.Drawing.Bitmap(width, height))
        Using newBitLock
            For y = height - 1 To 0 Step -1
                For x = width - 1 To 0 Step -1
                    newBitLock.SetPixel(x, y, color(y, x))
                Next
            Next
        End Using

        Return newBitLock.Bitmap
    End Function

    Public Shared Function CreateBitmap(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color, ByVal idealWidth As Integer, ByVal idealHeight As Integer) As System.Drawing.Bitmap
        Dim newBitLock As New Bitmaps(New System.Drawing.Bitmap(width, height))

        Dim dem_1 = color.GetUpperBound(0)
        Dim dem_2 = color.GetUpperBound(1)

        Using newBitLock
            For y = height - 1 To 0 Step -1
                For x = width - 1 To 0 Step -1

                    If x <= dem_1 And y <= dem_2 Then
                        newBitLock.SetPixel(x, y, color(y, x))
                    Else
                        newBitLock.SetPixel(x, y, Colors.Blue)
                    End If

                Next
            Next
        End Using

        Return ResizeBitmapByDpi(newBitLock.Bitmap, idealWidth, idealHeight)
    End Function

    Public Shared Function CreateBitmap(ByVal width As Integer, ByVal height As Integer, ByVal color(,) As Color, ByVal idealWidth As Integer, ByVal idealHeight As Integer, ByVal repeatPattern As Boolean) As System.Drawing.Bitmap

        If repeatPattern = False Then
            Return CreateBitmap(width, height, color, idealWidth, idealHeight)
        End If

        Dim newBitLock As New Bitmaps(New System.Drawing.Bitmap(width, height))

        Dim dem_1 = color.GetUpperBound(0)
        Dim dem_2 = color.GetUpperBound(1)

        Dim cntX = 0
        Dim cntY = 0

        Using newBitLock
            For y = height - 1 To 0 Step -1

                If cntY < dem_2 Then
                    cntY += 1
                Else
                    cntY = 0
                End If

                For x = width - 1 To 0 Step -1

                    If cntX < dem_1 Then
                        cntX += 1
                    Else
                        cntX = 0
                    End If

                    newBitLock.SetPixel(x, y, color(cntY, cntX))
                Next
            Next
        End Using

        Return ResizeBitmapByDpi(newBitLock.Bitmap, idealWidth, idealHeight)
    End Function

#End Region


    Public Sub Dispose() Implements IDisposable.Dispose
        UnlockBits()
    End Sub

End Class

Public Class Keyboards
    Private Shared Gate As New Matrix.Gate
    'AddHandler Listener.KeyDown, AddressOf KListener_KeyDown
    'OR!
    'WithEvents Listener As New Keyboards.KeyboardListener()

    'Private Sub KListener_KeyDown(sender As Object, args As Keyboards.RawKeyEventArgs)
    'MessageBox.Show(args.Key)
    'End Sub

    'Private Sub KListener_KeyUp(sender As Object, args As Keyboards.RawKeyEventArgs)
    'End Sub

    'Private Sub Application_Exit(sender As Object, e As ExitEventArgs) Handles MyBase.Unloaded
    'Listener.Dispose()
    'End Sub

    Public Shared Property Log() As String = ""

    Public Shared Property CurrentKeys() As New List(Of Input.Key)

    Public Shared ReadOnly Property IsKeyPressed() As Boolean
        Get
            If CurrentKeys.Count > 0 Then
                Return True
            Else
                Return False
            End If
        End Get
    End Property


    Public Shared Function Search(ByVal keys As Input.Key(), ByVal eGate As Integer) As Boolean
        Select Case eGate
            Case Gate.eEQUALS
                Return compare(keys)
            Case Gate.eAND
                Return and_gate(keys)
            Case Gate.eOR
                Return or_gate(keys)
            Case Else
                Return False
        End Select
    End Function

    Private Shared Function compare(ByVal keyCompare As Input.Key()) As Boolean
        Dim CopyList As New List(Of Key)
        CopyList.AddRange(CurrentKeys())

        If CopyList.Count() = keyCompare.Count() Then
            For Each Entry In CopyList
                If count(CopyList.ToArray, Entry) <> count(keyCompare, Entry) Then
                    Return False
                End If
            Next
        Else
            Return False
        End If

        Return True
    End Function
    Private Shared Function count(ByVal keyList As Input.Key(), ByVal Item As Input.Key) As Integer
        Dim Cnt As Integer = 0

        For Each Entry In keyList
            If Entry = Item Then
                Cnt += 1
            End If
        Next

        Return Cnt
    End Function
    Private Shared Function or_gate(ByVal keyContain As Input.Key()) As Boolean
        For Each key In keyContain
            If CurrentKeys.Contains(key) = True Then Return True
        Next

        Return False
    End Function
    Private Shared Function and_gate(ByVal keyContain As Input.Key()) As Boolean
        For Each key In keyContain
            If CurrentKeys.Contains(key) = False Then Return False
        Next

        Return True
    End Function


    Public Class KeyboardListener
        Implements IDisposable

        Public Sub New()
            ' We have to store the HookCallback, so that it is not garbage collected runtime
            hookedLowLevelKeyboardProc = DirectCast(AddressOf LowLevelKeyboardProc, InterceptKeys.LowLevelKeyboardProc)

            ' Set the hook
            hookId = InterceptKeys.SetHook(hookedLowLevelKeyboardProc)

            ' Assign the asynchronous callback event
            hookedKeyboardCallbackAsync = New KeyboardCallbackAsync(AddressOf KeyboardListener_KeyboardCallbackAsync)
        End Sub


        Protected Overrides Sub Finalize()
            Try
                Dispose()
            Finally
                MyBase.Finalize()
            End Try
        End Sub

        Public Event KeyDown As RawKeyEventHandler
        Public Event KeyUp As RawKeyEventHandler
        Public Event KeyDownOnce As RawKeyEventHandler
        Public Event KeyUpOnce As RawKeyEventHandler

        Private Shared Sub KeysDown(sender As System.Object, e As RawKeyEventArgs) Handles Me.KeyDown
            Try
                Log() = Log() & e.sKey
            Catch
            End Try
        End Sub

#Region "Inner workings"

        Private hookId As IntPtr = IntPtr.Zero
        Private Delegate Sub KeyboardCallbackAsync(keyEvent As InterceptKeys.KeyEvent, vkCode As Integer)

        <MethodImpl(MethodImplOptions.NoInlining)>
        Private Function LowLevelKeyboardProc(nCode As Integer, wParam As UIntPtr, lParam As IntPtr) As IntPtr
            If nCode >= 0 Then
                If wParam.ToUInt32() = CInt(InterceptKeys.KeyEvent.WM_KEYDOWN) OrElse wParam.ToUInt32() = CInt(InterceptKeys.KeyEvent.WM_KEYUP) OrElse wParam.ToUInt32() = CInt(InterceptKeys.KeyEvent.WM_SYSKEYDOWN) OrElse wParam.ToUInt32() = CInt(InterceptKeys.KeyEvent.WM_SYSKEYUP) Then
                    hookedKeyboardCallbackAsync.BeginInvoke(CType(wParam.ToUInt32(), InterceptKeys.KeyEvent), Marshal.ReadInt32(lParam), Nothing, Nothing)
                End If
            End If

            Return InterceptKeys.CallNextHookEx(hookId, nCode, wParam, lParam)
        End Function


        Private hookedKeyboardCallbackAsync As KeyboardCallbackAsync
        Private hookedLowLevelKeyboardProc As InterceptKeys.LowLevelKeyboardProc


        Private Sub KeyboardListener_KeyboardCallbackAsync(keyEvent As InterceptKeys.KeyEvent, vkCode As Integer)
            Dim keys = System.Windows.Input.KeyInterop.KeyFromVirtualKey(vkCode)

            Select Case keyEvent
                ' KeyDown events
                Case InterceptKeys.KeyEvent.WM_KEYDOWN, InterceptKeys.KeyEvent.WM_SYSKEYDOWN

                    If CurrentKeys().Contains(keys) = False Then
                        CurrentKeys().Add(keys)
                        RaiseEvent KeyDownOnce(Me, New RawKeyEventArgs(keys))
                        If CurrentKeys.Count() > 10 Then CurrentKeys.RemoveAt(0)
                    End If

                    RaiseEvent KeyDown(Me, New RawKeyEventArgs(keys))

                    Exit Select
                ' KeyUp events
                Case InterceptKeys.KeyEvent.WM_KEYUP, InterceptKeys.KeyEvent.WM_SYSKEYUP
                    RaiseEvent KeyUp(Me, New RawKeyEventArgs(keys))

                    If CurrentKeys().Contains(keys) = True Then
                        RaiseEvent KeyUpOnce(Me, New RawKeyEventArgs(keys))
                        Try
                            CurrentKeys().Remove(keys)
                        Catch : End Try
                    End If

                    Exit Select
                Case Else

                    Exit Select
            End Select
        End Sub

#End Region


        Public Sub Dispose() Implements IDisposable.Dispose
            InterceptKeys.UnhookWindowsHookEx(hookId)
        End Sub

    End Class

    Public Class RawKeyEventArgs
        Inherits EventArgs

        Public Key As Key
        Public sKey As String

        Public Sub New(iKey As Key)
            Me.sKey = Feed(iKey)
            Me.Key = iKey
        End Sub

        Private Function IsShift()
            If CurrentKeys.Contains(Input.Key.LeftShift) Or CurrentKeys.Contains(Input.Key.RightShift) Then
                Return True
            Else
                Return False
            End If
        End Function

        Private Function Feed(ByVal e As Input.Key) As String

            Select Case e
                Case 44 To 69
                    If Console.CapsLock = True Or IsShift() = True Then
                        Return e.ToString
                    Else
                        Return e.ToString.ToLower
                    End If
                Case 34 To 43
                    If IsShift() = True Then
                        Select Case e.ToString
                            Case "D1" : Return "!"
                            Case "D2" : Return "@"
                            Case "D3" : Return "#"
                            Case "D4" : Return "$"
                            Case "D5" : Return "%"
                            Case "D6" : Return "^"
                            Case "D7" : Return "&"
                            Case "D8" : Return "*"
                            Case "D9" : Return "("
                            Case "D0" : Return ")"
                        End Select
                    Else
                        Return e.ToString.Replace("D", Nothing)
                    End If
                Case 74 To 83
                    Return e.ToString.Replace("NumPad", Nothing)
                Case 84 To 89 '86
                    Select Case e.ToString
                        Case "Divide" : Return "/"
                        Case "Multiply" : Return "*"
                        Case "Subtract" : Return "-"
                        Case "Add" : Return "+"
                        Case "Decimal" : Return "."
                    End Select
                Case 18
                    Return " "
                Case 140 To 152 '147, 148, 

                    If IsShift() = True Then
                        Select Case e.ToString
                            Case "OemMinus" : Return "_"
                            Case "OemPlus" : Return "+"
                            Case "OemOpenBrackets" : Return "{"
                            Case "Oem6" : Return "}"
                            Case "Oem5" : Return "|"
                            Case "Oem1" : Return ":"
                            Case "OemQuotes" : Return """"
                            Case "OemComma" : Return "<"
                            Case "OemPeriod" : Return ">"
                            Case "OemQuestion" : Return "?"
                            Case "Oem3" : Return "~"
                        End Select
                    Else
                        Select Case e.ToString
                            Case "OemMinus" : Return "-"
                            Case "OemPlus" : Return "="
                            Case "OemOpenBrackets" : Return "["
                            Case "Oem6" : Return "]"
                            Case "Oem5" : Return "\"
                            Case "Oem1" : Return ";"
                            Case "OemQuotes" : Return "'"
                            Case "OemComma" : Return ","
                            Case "OemPeriod" : Return "."
                            Case "OemQuestion" : Return "/"
                            Case "Oem3" : Return "`"
                        End Select
                    End If
                Case Input.Key.Return
                    Return Environment.NewLine
                Case Else
                    If e.ToString.Length = 1 Then
                        Return e.ToString
                    Else
                        Return "<" & e.ToString & ">"
                    End If
            End Select
            Return Nothing
        End Function

    End Class

    Public Delegate Sub RawKeyEventHandler(sender As Object, args As RawKeyEventArgs)

    Friend NotInheritable Class InterceptKeys
        Private Sub New()
        End Sub

        Public Delegate Function LowLevelKeyboardProc(nCode As Integer, wParam As UIntPtr, lParam As IntPtr) As IntPtr
        Public Shared WH_KEYBOARD_LL As Integer = 13

        Public Enum KeyEvent As Integer
            WM_KEYDOWN = 256
            WM_KEYUP = 257
            WM_SYSKEYUP = 261
            WM_SYSKEYDOWN = 260
        End Enum

        Public Shared Function SetHook(proc As LowLevelKeyboardProc) As IntPtr
            Using curProcess As Process = Process.GetCurrentProcess()
                Using curModule As ProcessModule = curProcess.MainModule
                    Return SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle(curModule.ModuleName), 0)
                End Using
            End Using
        End Function


        <DllImport("user32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
        Public Shared Function SetWindowsHookEx(idHook As Integer, lpfn As LowLevelKeyboardProc, hMod As IntPtr, dwThreadId As UInteger) As IntPtr
        End Function

        <DllImport("user32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
        Public Shared Function UnhookWindowsHookEx(hhk As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
        End Function

        <DllImport("user32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
        Public Shared Function CallNextHookEx(hhk As IntPtr, nCode As Integer, wParam As UIntPtr, lParam As IntPtr) As IntPtr
        End Function

        <DllImport("kernel32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
        Public Shared Function GetModuleHandle(lpModuleName As String) As IntPtr
        End Function

    End Class

End Class

Public Class MouseInfo

    Public Shared Sub Load()
        GetAsyncKeyState(1)
    End Sub

    <DllImport("user32.dll")>
    Private Shared Function GetCursorPos(ByRef pt As Win32Point) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <StructLayout(LayoutKind.Sequential)>
    Friend Structure Win32Point
        Public X As Int32
        Public Y As Int32
    End Structure

    Public Shared Function GetMousePosition() As Point
        Dim w32Mouse As New Win32Point()
        GetCursorPos(w32Mouse)
        Return New Point(w32Mouse.X, w32Mouse.Y)
    End Function

    Public Shared Function GetKeyState(ByVal virtualKeyCode As UInt16) As Boolean
        If MouseInfo.GetAsyncKeyState(virtualKeyCode) <> 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    <DllImport("user32.dll")>
    Public Shared Function GetAsyncKeyState(virtualKeyCode As UInt16) As Short
    End Function

    <DllImport("user32.dll")>
    Public Shared Function GetSystemMetrics(virtualKeyCode As UInt16) As Short
    End Function

    'https://support.microsoft.com/en-us/kb/88922


    Public Shared ReadOnly Property IsLeftMousePressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_LBUTTON)
        End Get
    End Property

    Public Shared ReadOnly Property IsRightMousePressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_RBUTTON)
        End Get
    End Property

    Public Shared ReadOnly Property IsMiddleMousePressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_MBUTTON)
        End Get
    End Property

    Public Shared ReadOnly Property IsRightPressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_RIGHT)
        End Get
    End Property

    Public Shared ReadOnly Property IsLeftPressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_LEFT)
        End Get
    End Property

    Public Shared ReadOnly Property IsUpPressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_UP)
        End Get
    End Property

    Public Shared ReadOnly Property IsDownPressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_DOWN)
        End Get
    End Property

    Public Shared ReadOnly Property IsSpacebarPressed() As Boolean
        Get
            Return GetKeyState(KeyStates.VK_SPACE)
        End Get
    End Property

    Public Event MouseEventPressed()
    WithEvents CheckMouse_Timer As New TimeDispatcher(10)
    Private Sub CheckMouse_Event() Handles CheckMouse_Timer.Tick

        If IsRightMousePressed() = True Then
            RaiseEvent MouseEventPressed()
        End If
        If IsLeftMousePressed() = True Then
            RaiseEvent MouseEventPressed()
        End If
        If IsMiddleMousePressed() = True Then
            RaiseEvent MouseEventPressed()
        End If

    End Sub

    Public Sub MouseMonitorStart()
        CheckMouse_Timer.Start()
    End Sub
    Public Sub MouseMonitorStop()
        CheckMouse_Timer.Stop()
    End Sub


    Public Shared ReadOnly Property ScreenSize() As Size = New Size(SystemParameters.PrimaryScreenWidth,
                                                                    SystemParameters.PrimaryScreenHeight)

    Public Shared Function Monitor() As Integer
        Dim m_pos = GetMousePosition()
        Dim screen = ScreenSize()

        For i = 1 To 10
            If m_pos.X < (i * screen.Width) Then
                Return i
            End If
        Next

        Return 1
    End Function

    Public Shared WriteOnly Property MouseCurser() As System.Windows.Input.Cursor
        Set(ByVal value As System.Windows.Input.Cursor)
            System.Windows.Input.Mouse.OverrideCursor = value
        End Set
    End Property

    Public Shared ReadOnly Property GetCurrentAssemblies(ByVal Optional removeCurrentAssembly As Boolean = False) As List(Of String)
        Get
            Dim assemblies As New List(Of String)
            For Each assem In AppDomain.CurrentDomain.GetAssemblies()
                assemblies.Add(Utility.Remove(assem.CodeBase, "file:///"))
            Next

            'removes currently running .exe file address from the list
            If removeCurrentAssembly = True Then
                assemblies.Remove(Utility.Remove(Application.ResourceAssembly.CodeBase, "file:///"))
            End If

            Return assemblies
        End Get
    End Property


    Public Property InWindow() As Boolean = False
    Public Event EnterWindow()
    Public Event LeaveWindow()

    Private Property X As Double = 0
    Private Property Y As Double = 0
    Private Property Width() As Double = 0
    Private Property Height() As Double = 0

    Public Sub WindowSize(ByVal X As Double, ByVal Y As Double, ByVal width As Double, ByVal height As Double)
        Me.X() = X
        Me.Y() = Y
        Me.Width() = X + width
        Me.Height() = Y + height
    End Sub

    Public Sub WindowSize(ByVal position As Point, ByVal size As Size)
        Me.X() = position.X
        Me.Y() = position.Y
        Me.Width() = X + size.Width
        Me.Height() = Y + size.Height
    End Sub


    Public Sub WindowMonitorStart()
        CheckWindow_Timer.Start()
    End Sub

    Public Sub WindowMonitorStop()
        CheckWindow_Timer.Stop()
    End Sub

    WithEvents CheckWindow_Timer As New TimeDispatcher(10)
    Private Sub CheckWindow_Event() Handles CheckWindow_Timer.Tick
        Dim pos = GetMousePosition()

        If Utility.InRange(pos.X, Me.X, Width(), False) = True Then
            If Utility.InRange(pos.Y, Me.Y, Height(), False) = True Then
                InWindow() = True
                RaiseEvent EnterWindow()
            Else
                InWindow() = False
                RaiseEvent LeaveWindow()
            End If
        Else
            InWindow() = False
            RaiseEvent LeaveWindow()
        End If

    End Sub

End Class

Public Enum KeyStates As UShort
    VK_LBUTTON = 1 'Left mouse button
    VK_RBUTTON = 2 'Right mouse button
    VK_CANCEL = 3 'Control-break processing
    VK_MBUTTON = 4 'Middle mouse button On a three-button mouse
    '05 - 07    Undefined
    VK_BACK = 8 'BACKSPACE key
    VK_TAB = 9 'TAB key
    '0A0B	Undefined
    VK_CLEAR = 12 'CLEAR key
    VK_RETURN = 13 'ENTER key
    '0E0F	Undefined
    VK_SHIFT = 16 'SHIFT key
    VK_CONTROL = 17 'CTRL key
    VK_MENU = 18 'ALT key
    VK_PAUSE = 19 'PAUSE key
    VK_CAPITAL = 20 'CAPS LOCK key
    '1519    Reserved for Kanji systems
    '1A	Undefined
    VK_ESCAPE = 27 'ESC key
    '1C1F	Reserved for Kanji systems
    VK_SPACE = 32 'SPACEBAR
    VK_PRIOR = 33 'PAGE UP key
    VK_NEXT = 34 'PAGE DOWN key
    VK_END = 35 'END key
    VK_HOME = 36 'HOME key
    VK_LEFT = 37 'LEFT ARROW key
    VK_UP = 38 'UP ARROW key
    VK_RIGHT = 39 'RIGHT ARROW key
    VK_DOWN = 40 'DOWN ARROW key
    VK_SELECT = 41 'SELECT key
    '2A	Specific to original equipment manufacturer
    VK_EXECUTE = 43 'EXECUTE key
    VK_SNAPSHOT = 44    'PRINT SCREEN key
    VK_INSERT = 45  'INS key
    VK_DELETE = 46  'DEL key
    VK_HELP = 47    'HELP key
    '3A40	Undefined
    VK_LWIN = 91 'Left Windows key on a Microsoft Natural Keyboard
    VK_RWIN = 92 'Right Windows key on a Microsoft Natural Keyboard
    VK_APPS = 93 'Applications key on a Microsoft Natural Keyboard
    '5E5F	Undefined
    VK_NUMPAD0 = 96 'Numeric keypad 0 key
    VK_NUMPAD1 = 97 'Numeric keypad 1 key
    VK_NUMPAD2 = 98 'Numeric keypad 2 key
    VK_NUMPAD3 = 99 'Numeric keypad 3 key
    VK_NUMPAD4 = 100 'Numeric keypad 4 key
    VK_NUMPAD5 = 101 'Numeric keypad 5 key
    VK_NUMPAD6 = 102 'Numeric keypad 6 key
    VK_NUMPAD7 = 103 'Numeric keypad 7 key
    VK_NUMPAD8 = 104 'Numeric keypad 8 key
    VK_NUMPAD9 = 105 'Numeric keypad 9 key
    VK_MULTIPLY = 106 'Multiply key
    VK_ADD = 107 'Add key
    VK_SEPARATOR = 108 'Separator key
    VK_SUBTRACT = 109 'Subtract key
    VK_DECIMAL = 110 'Decimal key
    VK_DIVIDE = 111 'Divide key
    VK_F1 = 112 'F1 key
    VK_F2 = 113 'F2 key
    VK_F3 = 114 'F3 key
    VK_F4 = 115 'F4 key
    VK_F5 = 116 'F5 key
    VK_F6 = 117 'F6 key
    VK_F7 = 118 'F7 key
    VK_F8 = 119 'F8 key
    VK_F9 = 120 'F9 key
    VK_F10 = 121 'F10 key
    VK_F11 = 122 'F11 key
    VK_F12 = 123 'F12 key
    VK_F13 = 124 'F13 key
    VK_F14 = 125 'F14 key
    VK_F15 = 126 'F15 key
    VK_F16 = 127 'F16 key
    VK_F17 = 128 'F17 key
    VK_F18 = 129 'F18 key
    VK_F19 = 130 'F19 key
    VK_F20 = 131 'F20 key
    VK_F21 = 132 'F21 key
    VK_F22 = 133 'F22 key
    '(PPC only) Key used to lock device.
    VK_F23 = 134 'F23 key
    VK_F24 = 135 'F24 key
    '888F	Unassigned
    VK_NUMLOCK = 144 'NUM LOCK key
    VK_SCROLL = 145 'SCROLL LOCK key
    VK_LSHIFT = 160 'Left SHIFT
    VK_RSHIFT = 161 'Right SHIFT
    VK_LCONTROL = 162 'Left CTRL
    VK_RCONTROL = 163 'Right CTRL
    VK_LMENU = 164 'Left ALT
    VK_RMENU = 165 'Right ALT
    'BA-C0	Specific to original equipment manufacturer; reserved. See following tables.
    'C1-DA	Unassigned
    'DB-E2	Specific to original equipment manufacturer; reserved. See following tables.
    'E3 – E4	Specific To original equipment manufacturer
    'E5	Unassigned
    'E6	Specific To original equipment manufacturer
    VK_PACKET = 231 'Used To pass Unicode characters As If they were keystrokes. If VK_PACKET Is used With SendInput, Then the Unicode character To be delivered should be placed into the lower 16 bits Of the scan code. If a keyboard message Is removed from the message queue And the virtual key Is VK_PACKET, Then the Unicode character will be the upper 16 bits Of the lparam.
    'E8	Unassigned
    'E9-F5	Specific to original equipment manufacturer
    VK_ATTN = 246 'ATTN key
    VK_CRSEL = 247 'CRSEL key
    VK_EXSEL = 248 'EXSEL key
    VK_EREOF = 249 'Erase EOF key
    VK_PLAY = 250 'PLAY key
    VK_ZOOM = 251 'ZOOM key
    VK_NONAME = 252 'Reserved For future use
    VK_PA1 = 253 'PA1 key
    VK_OEM_CLEAR = 254 'CLEAR key
    VK_KEYLOCK = 255 '3874 'Key used To lock device
    VK_OEM_SCROLL = 145 'None
    VK_OEM_1 = 186 '";:" For US
    VK_OEM_PLUS = 187 '"+" any country/region
    VK_OEM_COMMA = 188 '"," any country/region
    VK_OEM_MINUS = 189 '"-" any country/region
    VK_OEM_PERIOD = 190 '"." any country/region
    VK_OEM_2 = 191 '"/?" For US
    VK_OEM_3 = 192 '"`~" For US
    VK_OEM_4 = 219 '"[{" For US
    VK_OEM_5 = 220 '"\|" For US
    VK_OEM_6 = 221 '"]}" For US
    VK_OEM_7 = 222 '"'"" for US
    VK_OEM_8 = 223 'None
    VK_OEM_AX = 225 'AX key On Japanese AX keyboard
    VK_OEM_102 = 226 '"<>" Or "\|" On RT 102-key keyboard
End Enum

Public Class GenerikButton

    Public WithEvents Source As New Grid
    Public WithEvents BorderSource As New Border
    Public WithEvents TextSource As New TextBlock

    Public Property Name() As String = ""

    Private Sub Initialize() Handles Source.Initialized
        TextSource.TextAlignment = TextAlignment.Center
        TextSource.HorizontalAlignment = HorizontalAlignment.Center
        TextSource.VerticalAlignment = VerticalAlignment.Center

        BorderSource.CornerRadius = New CornerRadius(3)

        BorderSource.Child = TextSource
        Source.Children.Add(BorderSource)

        Font(NormalFont())
    End Sub


    Public Sub ResetSizeToText()
        Dim t_size = Local.GetTextSize(Text(), NormalFont())

        If Source.Width <= 0 Or Double.IsNaN(Source.Width) = True Then
            Dim textMargin = TextSource.Margin.Left + TextSource.Margin.Right
            Dim border = BorderSource.BorderThickness.Left + BorderSource.BorderThickness.Right
            Source.Width = t_size.Width + textMargin + border
        End If

        If Source.Height <= 0 Or Double.IsNaN(Source.Height) = True Then
            Dim textMargin = TextSource.Margin.Top + TextSource.Margin.Bottom
            Dim border = BorderSource.BorderThickness.Top + BorderSource.BorderThickness.Bottom
            Source.Height = t_size.Height + textMargin + border
        End If
    End Sub

    Sub New()
    End Sub
    Sub New(ByVal width As Double, ByVal height As Double)

        If width > 0 Then
            Source.Width = width
        End If

        If height > 0 Then
            Source.Height = height
        End If

    End Sub
    Sub New(ByVal location As Point, ByVal width As Double, ByVal height As Double)

        If width > 0 Then
            Source.Width = width
        End If

        If height > 0 Then
            Source.Height = height
        End If

        Source.Margin = New Thickness(location.X, location.Y, 0, 0)
    End Sub

    Public Property Width() As Double
        Get
            Return Source.Width
        End Get
        Set(ByVal value As Double)
            Source.Width = value
        End Set
    End Property
    Public Property Height() As Double
        Get
            Return Source.Height
        End Get
        Set(ByVal value As Double)
            Source.Height = value
        End Set
    End Property

    Public ReadOnly Property ActualWidth()
        Get
            Dim sourceMargin = Source.Margin.Left + Source.Margin.Right
            Return Width() + sourceMargin
        End Get
    End Property
    Public ReadOnly Property ActualHeight()
        Get
            Dim sourceMargin = Source.Margin.Top + Source.Margin.Bottom
            Return Height() + sourceMargin
        End Get
    End Property

    Public ReadOnly Property Position() As Point
        Get
            Return Source.PointToScreen(New Point(0, 0))
        End Get
    End Property

    Public Property Location() As Point
        Get
            Return New Point(Source.Margin.Left, Source.Margin.Top)
        End Get
        Set(ByVal value As Point)
            Source.Margin = New Thickness(value.X, value.Y, 0, 0)
        End Set
    End Property

    Public Sub Buffer(ByVal left As Double, ByVal top As Double, ByVal right As Double, ByVal bottom As Double)
        Source.Margin = New Thickness(left, top, right, bottom)
    End Sub
    Public Sub Buffer(ByVal thick As Thickness)
        Source.Margin = thick
    End Sub

    Public Sub TextBuffer(ByVal left As Double, ByVal top As Double, ByVal right As Double, ByVal bottom As Double)
        TextSource.Margin = New Thickness(left, top, right, bottom)
    End Sub
    Public Sub TextBuffer(ByVal thick As Thickness)
        TextSource.Margin = thick
    End Sub

    Public Sub Border(ByVal color As Brush, ByVal thickness As Thickness)
        BorderSource.BorderBrush = color
        BorderSource.BorderThickness = thickness
    End Sub

    Public Property Text() As String
        Get
            Return TextSource.Text
        End Get
        Set(value As String)
            TextSource.Text = value
        End Set
    End Property

#Region "Animation"

    'multi animation seqences
    Public Property Animation() As New List(Of List(Of ImageBrush))
    Private Property Frame() As Integer = 0
    Public Property Index() As Integer = 0

    Public Sub Add(ByVal addX As Double, ByVal addY As Double)
        Source.Margin = New Thickness(Source.Margin.Left + addX, Source.Margin.Top + addY, 0, 0)
    End Sub

    Public Sub Render()
        If Animation(Index).Count <= Frame() Then Frame() = 0
        Source.Background() = Animation(Index)(Frame)
        Frame() += 1
    End Sub

#End Region

    Private m_NormalFont As New Fonts
    Public Property NormalFont() As Fonts
        Get
            Return m_NormalFont
        End Get
        Set(value As Fonts)
            If IsHovered() = False Then
                Font(value)
            End If
            m_NormalFont = value
        End Set
    End Property

    Private m_HoverFont As New Fonts
    Public Property HoverFont() As Fonts
        Get
            Return m_HoverFont
        End Get
        Set(value As Fonts)
            If IsHovered() = True Then
                Font(value)
            End If
            m_HoverFont = value
        End Set
    End Property

    Private m_Background As Brush
    Public Property Background() As Brush
        Get
            Return m_Background
        End Get
        Set(value As Brush)
            m_Background = value
            BorderSource.Background() = value
        End Set
    End Property

    Public Property Foreground() As Brush
        Get
            Return TextSource.Foreground()
        End Get
        Set(value As Brush)
            TextSource.Foreground() = value
        End Set
    End Property


    Public Property HoveredBackground() As Brush

    Private Sub Font(ByVal family As FontFamily, ByVal size As Double, ByVal style As FontStyle, ByVal weight As FontWeight, ByVal stretch As FontStretch, ByVal brush As Brush)
        TextSource.FontFamily() = family
        TextSource.FontSize() = size
        TextSource.FontStyle() = style
        TextSource.FontWeight() = weight
        TextSource.FontStretch() = stretch
        TextSource.Foreground() = brush
    End Sub

    Private Sub Font(ByVal newFont As Fonts)
        TextSource.FontFamily() = newFont.Family()
        TextSource.FontSize() = newFont.Size()
        TextSource.FontStyle() = newFont.Style()
        TextSource.FontWeight() = newFont.Weight()
        TextSource.FontStretch() = newFont.Stretch()
        TextSource.Foreground() = newFont.Color()
    End Sub

#Region "Events"

    'Use if you want to have an animation for a hovered state then you
    'should suspend the autohover feature
    Public Property SuspendAutoHover() As Boolean = False

    Public Event HoverEnter(ByVal sender As GenerikButton)
    Public Event HoverLeave(ByVal sender As GenerikButton)
    Public Event LeftClick(ByVal sender As GenerikButton)

    Public Property IsHovered() As Boolean = False
    Public Property IsLeftDown() As Boolean = False

    Private Property IsHoverEnterTrigger() As Boolean = False
    Public ReadOnly Property IsHoverEnterOnce() As Boolean
        Get
            If IsHoverEnterTrigger() = True Then
                IsHoverEnterTrigger() = False
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Public Property IsHoverLeaveTrigger() As Boolean = False
    Public ReadOnly Property IsHoverLeaveOnce() As Boolean
        Get
            If IsHoverLeaveTrigger() = True Then
                IsHoverLeaveTrigger() = False
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Public Property IsLeftDownTrigger() As Boolean = False
    Public ReadOnly Property IsLeftDownOnce() As Boolean
        Get
            If IsLeftDownTrigger() = True Then
                IsLeftDownTrigger() = False
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Public Property IsLeftUpTrigger() As Boolean = False
    Public ReadOnly Property IsLeftUpOnce() As Boolean
        Get
            If IsLeftUpTrigger() = True Then
                IsLeftUpTrigger() = False
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Private Sub Hover_MouseEntered() Handles Source.MouseEnter

        IsHovered() = True
        IsHoverEnterTrigger() = True
        RaiseEvent HoverEnter(Me)

        If SuspendAutoHover() = False Then
            If IsNothing(HoverFont) = False Then
                Font(HoverFont)
            End If

            If IsNothing(HoveredBackground) = False Then
                BorderSource.Background() = HoveredBackground()
            End If
        End If
    End Sub
    Private Sub Hover_MouseLeave() Handles Source.MouseLeave
        IsHovered() = False
        IsHoverLeaveTrigger() = True

        RaiseEvent HoverLeave(Me)

        If SuspendAutoHover() = False Then
            If IsNothing(NormalFont) = False Then
                Font(NormalFont)
            End If

            If IsNothing(Background) = False Then
                BorderSource.Background() = Background()
            End If
        End If
    End Sub


    Private Sub Left_Down() Handles Source.PreviewMouseLeftButtonDown
        IsLeftDown() = True
        IsLeftDownTrigger() = True
        RaiseEvent LeftClick(Me)
    End Sub
    Private Sub Left_Up() Handles Source.PreviewMouseLeftButtonUp
        IsLeftUpTrigger() = True
        IsLeftDown() = False
    End Sub

#End Region

    Public Property HorizontalAlignment() As HorizontalAlignment
        Get
            Return Source.HorizontalAlignment
        End Get
        Set(value As HorizontalAlignment)
            Source.HorizontalAlignment = value
        End Set
    End Property

    Public Property VerticalAlignment() As VerticalAlignment
        Get
            Return Source.VerticalAlignment
        End Get
        Set(value As VerticalAlignment)
            Source.VerticalAlignment = value
        End Set
    End Property

End Class

Public Class ManageGenerikButtons
    Public Property ButtonList() As New List(Of GenerikButton)
    Private Property SourcePanel As New StackPanel
    Private Property SourceBorder As New Border
    Public Property Source As New Grid

    Public Property HoverFont() As New Fonts
    Public Property NormalFont() As New Fonts
    Public Property HoveredBackground() As Brush
    Public Property ButtonBackground() As Brush
    Public Property Background() As Brush
    Public Property Buffer() As New Thickness(5)
    Public Property Spacing() As New Thickness(5)
    Public Property TextHorizontalAlignment() As HorizontalAlignment
    Public Property TextVerticalAlignment() As VerticalAlignment

    Public Property ButtonSize() As New Size(0, 0)
    Public ReadOnly Property ButtonCount() As Integer
        Get
            Return ButtonInfo().Count()
        End Get
    End Property

    Public Property ButtonInfo() As String() = {"B1|one", "B2|two", "B3|three"}

    Private Property ButtonBorderColor() As Brush
    Private Property ButtonBorderThickness() As New Thickness(0)
    Public Sub ButtonBorder(ByVal color As Brush, ByVal thick As Double)
        ButtonBorderThickness = New Thickness(thick)
        ButtonBorderColor = color
    End Sub
    Public Sub ButtonBorder(ByVal color As Brush, ByVal thick As Thickness)
        ButtonBorderThickness() = thick
        ButtonBorderColor = color
    End Sub

    Private Property BorderColor() As Brush
    Private Property BorderThickness() As New Thickness(0)
    Public Sub Border(ByVal color As Brush, ByVal thick As Thickness)
        BorderThickness = thick
        BorderColor = color
    End Sub

    Public Sub New()
        SourcePanel.Orientation = Orientation.Horizontal
    End Sub

    Public Sub New(ByVal names As String())
        SourcePanel.Orientation = Orientation.Horizontal
        ButtonInfo() = names
        Load()
    End Sub

    Public Property Visibility() As Visibility
        Get
            Return SourcePanel.Visibility
        End Get
        Set(value As Visibility)
            SourcePanel.Visibility = value
        End Set
    End Property

    Private Property ActualButtonWidth() As Double = 0
    Private Property ActualButtonHeight() As Double = 0

    Public Sub Load()
        ButtonList.Clear()
        SourcePanel.Children.Clear()
        Source.Children.Clear()

        For Each name In ButtonInfo()
            Dim newNames = Utility.ToList(name, "|")

            If newNames.Count = 2 Then
                Dim newEntity As New GenerikButton(ButtonSize.Width(), ButtonSize.Height())

                newEntity.Name() = newNames(0)
                newEntity.Text() = newNames(1)
                newEntity.NormalFont() = NormalFont()
                newEntity.HoverFont() = HoverFont()
                newEntity.Background() = ButtonBackground()
                newEntity.Border(ButtonBorderColor, ButtonBorderThickness)
                newEntity.HoveredBackground() = HoveredBackground()
                newEntity.TextBuffer(Buffer())
                newEntity.Buffer(Spacing())
                newEntity.VerticalAlignment() = TextVerticalAlignment()
                newEntity.HorizontalAlignment() = TextHorizontalAlignment()
                newEntity.ResetSizeToText()

                findSize(newEntity)

                AddHandler newEntity.LeftClick, AddressOf LeftClick_Check
                AddHandler newEntity.HoverEnter, AddressOf HoverEnter_Check
                AddHandler newEntity.HoverLeave, AddressOf HoverLeave_Check

                ButtonList.Add(newEntity)

                SourcePanel.Background = Background()
                SourcePanel.Children.Add(newEntity.Source())
            End If
        Next


        SourceBorder.BorderBrush = BorderColor()
        SourceBorder.BorderThickness = BorderThickness()
        SourceBorder.Child = SourcePanel
        Source.Children.Add(SourceBorder)

    End Sub

    Private Sub findSize(ByRef button As GenerikButton)

        If SourcePanel.Orientation = Orientation.Horizontal Then
            ActualButtonWidth() = ActualButtonWidth() + button.ActualWidth

            If button.ActualHeight() > ActualButtonHeight() Then
                ActualButtonHeight() = button.ActualHeight()
            End If

        Else
            ActualButtonHeight() = ActualButtonHeight() + button.ActualHeight

            If button.ActualWidth() > ActualButtonWidth() Then
                ActualButtonWidth() = button.ActualWidth()
            End If

        End If
    End Sub


    Public Event LeftClick(ByVal button As GenerikButton)
    Public Event HoverEnter(ByVal button As GenerikButton)
    Public Event HoverLeave(ByVal button As GenerikButton)


    Private Sub LeftClick_Check()
        For Each button In ButtonList
            If button.IsLeftDownOnce() = True Then
                RaiseEvent LeftClick(button)
            End If
        Next
    End Sub

    Private Sub HoverEnter_Check()
        For Each button In ButtonList
            If button.IsHoverEnterOnce() = True Then
                RaiseEvent HoverEnter(button)
            End If
        Next
    End Sub

    Private Sub HoverLeave_Check()
        For Each button In ButtonList
            If button.IsHoverLeaveOnce() = True Then
                RaiseEvent HoverLeave(button)
            End If
        Next
    End Sub

    Public ReadOnly Property Position() As Point
        Get
            Return Source.PointToScreen(New Point(0, 0))
        End Get
    End Property

    Public Property HorizontalAlignment() As HorizontalAlignment
        Get
            Return Source.HorizontalAlignment
        End Get
        Set(value As HorizontalAlignment)
            Source.HorizontalAlignment = value
        End Set
    End Property

    Public Property VerticalAlignment() As VerticalAlignment
        Get
            Return Source.VerticalAlignment
        End Get
        Set(value As VerticalAlignment)
            Source.VerticalAlignment = value
        End Set
    End Property

    Public Property Orientation() As Orientation
        Get
            Return SourcePanel.Orientation
        End Get
        Set(value As Orientation)
            SourcePanel.Orientation = value
        End Set
    End Property

    Public ReadOnly Property Size As Size
        Get
            Dim borderWidth = SourceBorder.BorderThickness.Right + SourceBorder.BorderThickness.Left
            Dim borderHeight = SourceBorder.BorderThickness.Top + SourceBorder.BorderThickness.Bottom
            Return New Size(ActualButtonWidth() + borderWidth, ActualButtonHeight() + borderHeight)
        End Get
    End Property

End Class

Public Class Fonts
    Public Sub New()
        Me.Family() = New FontFamily("Arial")
        Me.Size() = 12
        Me.Style() = FontStyles.Normal
        Me.Weight() = FontWeights.Normal
        Me.Stretch() = FontStretches.Normal
        Me.Color() = Brushes.Black
    End Sub
    Public Sub New(ByVal family As FontFamily, ByVal size As Double, ByVal color As Brush)
        Me.Family() = family
        Me.Size() = size
        Me.Color() = color
    End Sub

    Public Sub New(ByVal family As FontFamily, ByVal size As Double, ByVal style As FontStyle, ByVal weight As FontWeight, ByVal stretch As FontStretch, ByVal color As Brush)
        Me.Family() = family
        Me.Size() = size
        Me.Style() = style
        Me.Weight() = weight
        Me.Stretch() = stretch
        Me.Color() = color
    End Sub

    Public Property Family() As FontFamily = New FontFamily("Arial")
    Public Property Size() As Double = 12
    Public Property Style() As FontStyle = FontStyles.Normal
    Public Property Weight() As FontWeight = FontWeights.Normal
    Public Property Stretch() As FontStretch = FontStretches.Normal
    Public Property Color() As Brush = Brushes.Black

End Class

Public Class Scripts

    Private Debug As New Matrix.Debug
    Public Const DEF_PROGRAM_NAME As String = Schema.DEF_ATHENA_NAME
    Const DEF_SOURCE_EXT As String = Ext.VB
    Const DEF_DLL_FILE As String = "dll" & Ext.IO
    Const DEF_PLUG As String = "#PLUGIN"
    Const DEF_AREA As String = "<AREA:"
    Const DEF_CLOSE As String = ">"

    Public Sub New()
        DebugThread.WorkerSupportsCancellation() = True
    End Sub

    Public Property CodeSource() As String = ""
    Public Property PluginSource As String = ""
    Public Property ProgramSource() As String = ""

    Public ReadOnly Property IsRunning() As Boolean
        Get
            Return DebugThread.IsBusy()
        End Get
    End Property

    Public Property Code() As String = ""
    Public Property Dlls() As List(Of String)
    Public Property Errors() As String = ""
    Public Property IsCompileError() As Boolean = False
    Public Property Messages() As String = ""

    Public Event IsError(ByVal errors As String)

    Public Sub Print(ByVal location As String)
        Utility.TextWriter(location, Code(), False)
    End Sub

    Public Sub Close()
        Utility.ExitProgram(DEF_PROGRAM_NAME)
    End Sub

    Private Function Combine() As Boolean

        If Utility.FileExists(CodeSource()) = True Then

            Dim files = Utility.ZipReader(CodeSource())

            Dim text = ""
            For Each file In files.Items
                If Utility.Search(file.Name(), DEF_SOURCE_EXT, Gate.eEND) = True Then
                    text = text & file.GetString()
                End If

                If Utility.Search(file.Name(), DEF_DLL_FILE, Gate.eEND) = True Then
                    Dlls() = Utility.ToList(file.GetString(), vbCrLf)
                End If
            Next

            text = Load_Plugins(text)

            Load_Dlls()

            Code() = text
            Return True
        Else
            Errors() = Debug.eERROR & "Source file could not be found: '" & CodeSource() & "'"
            RaiseEvent IsError(Errors())
            Return False
        End If
    End Function

    Public Function Load_Plugins(ByVal code As String) As String

        For Each file In Utility.GetAllFiles(PluginSource())
            If Utility.Search(file, Schema.Current_Plugins(), Gate.eEQUALS) = True Then

                Dim text = Utility.TextReader(file)

                For Each plugin In Utility.ToList(text, DEF_PLUG)
                    If Utility.IsEmpty(plugin) = False Then
                        Dim name = Utility.Trim(Utility.GetMidArg(plugin, DEF_AREA, DEF_CLOSE, Gate.eEQUALS))

                        If Utility.IsEmpty(name) = False Then
                            plugin = name & Utility.MultiStrings(vbCrLf, 3) & plugin
                            code = Utility.Replace(code, name, plugin)
                        End If
                    End If
                Next

            End If
        Next

        Return code
    End Function


    Private Sub Load_Dlls()

        Dim currentAsmb = MouseInfo.GetCurrentAssemblies()
        Dim currentDlls As New List(Of String)(Dlls())

        For Each asmb In currentAsmb

            'Getting dll locations from the mother program
            'I'm filtering them for a lighter weight
            If Utility.Search(asmb, {"Matrix.Dll",
                                      "PresentationFramework.dll",
                                      "WindowsBase.dll",
                                      "System.Core.dll",
                                      "System.dll",
                                      "PresentationCore.dll",
                                      "Microsoft.VisualBasic.dll"}, Gate.eEQUALS) = True Then


                'Trying to avoid copies of Dlls added from the Dlls.IO file
                For i = 0 To currentDlls.Count - 1
                    If Utility.Search(asmb, currentDlls(i), Gate.eEND) = True Then
                        Dlls(i) = asmb
                    End If
                Next

                Dlls.Add(asmb)

            End If
        Next
    End Sub

    Public Sub Compile()
        Errors() = False

        If Combine() = False Then Exit Sub

        Dim results = Utility.Compiler(Code(), ProgramSource() & DEF_PROGRAM_NAME, Language.VBCode, Dlls(), "/optimize")
        If Utility.Search(results, Debug.eERROR, Gate.eSEARCH) = True Then
            Errors() = results
            IsCompileError() = True
            RaiseEvent IsError("Problems Compiling Code: " & vbCrLf & Errors())
        Else
            IsCompileError() = False
        End If
    End Sub

    Private attempts As Integer = 0
    Public Sub Run()
        If IsCompileError() = False Then
            Stops()

            Try
                If DebugThread.IsBusy() = False Then
                    DebugThread.RunWorkerAsync()
                End If
            Catch
                Errors() = Debug.eERROR & "Could not run program: '" & ProgramSource() & DEF_PROGRAM_NAME & "'"
                RaiseEvent IsError(Errors())
            End Try
        End If
    End Sub

    Public Sub Stops()
        DebugThread.CancelAsync()
        Utility.ExitProgram(DEF_PROGRAM_NAME)
        Do Until DebugThread.IsBusy() = False : Loop
    End Sub

    WithEvents DebugThread As New ComponentModel.BackgroundWorker
    Private Sub Debugging_DoWork() Handles DebugThread.DoWork
        Messages() = Utility.OpenProgram(ProgramSource() & DEF_PROGRAM_NAME, Nothing, False)
    End Sub
    Private Sub Debugging_Completed() Handles DebugThread.RunWorkerCompleted
        If Utility.Search(Messages(), Debug.eERROR, Gate.eSTART) = True Then
            Errors() = Messages()
            Messages() = ""
            RaiseEvent IsError("Problems Running Code: " & vbCrLf & Errors())
        End If
    End Sub


End Class

Public Class DelayDispatcher

    Public Sub New()
    End Sub

    Public Sub New(ByVal delay As ULong)
        Me.Interval() = 1
        Me.Delay() = delay
    End Sub

    Public Sub New(ByVal delay As ULong, ByVal interval As Double)
        Me.Interval() = interval
        Me.Delay() = delay
    End Sub

    WithEvents LocalTimer As New Threading.DispatcherTimer()
    Public Event Elapsed()
    Public Property IsWorking() As Boolean = False

    Public Sub Start()
        Count() = 0
        LocalTimer.Start()
    End Sub

    Public Sub Start(ByVal delay As ULong)
        Me.Delay() = delay
        Me.Interval() = 1
        Start()
    End Sub

    Public Sub [Stop]()
        LocalTimer.Stop()
    End Sub


    Public Property Delay() As ULong = 0
    Public Property Interval() As Integer
        Get
            Return LocalTimer.Interval.Milliseconds
        End Get
        Set(value As Integer)
            LocalTimer.Interval = TimeSpan.FromMilliseconds(value)
        End Set
    End Property

    Private Property Count() As ULong = 0

    Private Sub Delay_Time() Handles LocalTimer.Tick

        If Count >= Delay() Then
            IsWorking() = False
            Count = 0
            RaiseEvent Elapsed()
            LocalTimer.Stop()
        End If

        Count += 1
    End Sub


End Class

Public Class TimeDispatcher

    Public Sub New()
    End Sub

    Public Sub New(ByVal interval As ULong)
        Me.Interval() = interval
        IsSecondTimer() = False
    End Sub

    Public Sub New(ByVal day As Integer, ByVal hour As Integer, ByVal minute As Integer, ByVal second As Integer)
        'Used in the timer
        Me.Days() = day
        Me.Hours() = hour
        Me.Minutes() = minute
        Me.Seconds() = second
        Me.Interval() = 1000
        IsSecondTimer() = True
    End Sub

    WithEvents LocalTimer As New Threading.DispatcherTimer()

    Public Property Days() As Integer = 0
    Public Property Hours() As Integer = 0
    Public Property Minutes() As Integer = 0
    Public Property Seconds() As Integer = 0

    Public Event Tick()
    Public Property IsWorking() As Boolean = False
    Public Property IsSecondTimer() As Boolean = False

    Public Sub Start()
        IsWorking() = True
        cDays = 0
        cHours = 0
        cMinutes = 0
        cSeconds = 0
        LocalTimer.Start()
    End Sub

    Public Sub [Stop]()
        LocalTimer.Stop()
        IsWorking() = False
    End Sub

    Public Sub Sticky(ByVal interval As Double)
        Me.Interval() = interval
        If IsWorking() = False Then Start()
        Do Until IsWorking() = False
            Utility.Sleep(interval)
        Loop
    End Sub

    Public Property Interval() As Integer
        Get
            Return LocalTimer.Interval.Milliseconds
        End Get
        Set(value As Integer)
            LocalTimer.Interval = TimeSpan.FromMilliseconds(value)
        End Set
    End Property

    Private Property cSeconds() As Integer = 0
    Private Property cMinutes() As Integer = 0
    Private Property cHours() As Integer = 0
    Private Property cDays() As Integer = 0

    Private Sub Delay_Time() Handles LocalTimer.Tick

        If IsSecondTimer() = True Then

            If Me.Days() = cDays Then
                If Me.Hours() = cHours Then
                    If Me.Minutes() = cMinutes Then
                        If Me.Seconds() = cSeconds Then
                            cDays = 0
                            cHours = 0
                            cMinutes = 0
                            cSeconds = 0
                            RaiseEvent Tick()
                        End If
                    End If
                End If
            End If

            cSeconds += 1

            If cHours = 24 Then
                cDays += 1
                cHours = 0
            End If

            If cMinutes = 60 Then
                cHours += 1
                cMinutes = 0
            End If

            If cSeconds = 60 Then
                cMinutes += 1
                cSeconds = 0
            End If

        Else
            RaiseEvent Tick()
        End If

    End Sub

End Class

Public Class Schema

    Public Const DEF_UIPROGRAM_NAME As String = "UI.exe"
    Public Const DEF_ATHENA_NAME As String = "athena.exe"
    Public Const DEF_LAUNCHER_NAME As String = "Launcher.exe"

    Public Class Avatar
        Public Const FOLDER_TEXTURE As String = "textures"
        Public Const FOLDER_SOUNDS As String = "audio"
        Public Const FOLDER_PROFILES As String = "profiles"
        Public Const FOLDER_SCRIPTS As String = "scripts"
        Public Const FILE_INDEX As String = "index.io"
    End Class



#Region "Globals"

    Public Shared Event UpdateSize(ByVal newSize As Size)
    Public Shared Sub RaiseUpdateSizeEvent(ByVal newSize As Size)
        RaiseEvent UpdateSize(newSize)
    End Sub

    Public Shared Property Global_Postion() As Point
    Public Shared Property Global_Size() As Size

    'GLOBAL
    Public Shared Property Global_Font() As FontFamily = LocalResources.GetFont()
    Public Shared Property Global_FontColor() As Brush = Brushes.White
    Public Shared Property Global_FontBackColor() As Brush = Brushes.Pink
    Public Shared Property Global_HoverColor() As Brush = Brushes.Red
    Public Shared Property Global_FontSize() As Double = 18
    Public Shared Property Global_Window_Color() As Brush = Brushes.Black
    Public Shared Property Global_Border_Color() As Brush = Brushes.Red
    Public Shared Property Global_Border_Size() As Double = 3

    'PROMPT
    Public Shared Property Prompt_Ai_FontColor() As Brush = Brushes.Purple
    Public Shared Property Prompt_User_FontColor() As Brush = Brushes.White
    Public Shared Property Prompt_Error_FontColor() As Brush = Brushes.Red

    'STATUS
    Public Shared Property Status_FontColor() As Brush = Brushes.Green

    Public Shared ReadOnly Property Hover_Fonts() As Fonts
        Get
            Return New Fonts(Global_Font(), Global_FontSize(), Global_HoverColor())
        End Get
    End Property

    Public Shared ReadOnly Property Normal_Fonts() As Fonts
        Get
            Return New Fonts(Global_Font(), Global_FontSize(), Global_FontColor())
        End Get
    End Property

    'SCRIPT_EDITOR
    Public Shared Property Script_Font() As String = "Consolas"
    Public Shared Property Script_FontSize() As Single = 12

    Public Shared Property Script_FontColor() As System.Drawing.Color = System.Drawing.Color.Black
    Public Shared Property Script_BackColor() As System.Drawing.Color = System.Drawing.Color.White

    Public Shared Property Script_NumberColor() As System.Drawing.Brush = System.Drawing.Brushes.Magenta
    Public Shared Property Script_StringColor() As System.Drawing.Brush = System.Drawing.Brushes.Brown
    Public Shared Property Script_CommentColor() As System.Drawing.Brush = System.Drawing.Brushes.Green
    Public Shared Property Script_KeyWordColor() As System.Drawing.Brush = System.Drawing.Brushes.Blue
    Public Shared Property Script_AttributeColor() As System.Drawing.Brush = System.Drawing.Brushes.Gray
    Public Shared Property Script_ClassColor() As System.Drawing.Brush = System.Drawing.Brushes.Purple

#End Region

#Region "Defaults"


    Public Shared Property Current_Package() As String = "standard"

    Private Shared m_Current_Brain As String = "standard" & Ext.BRAIN
    Public Shared Property Current_Brain(Optional ByVal nameOnly As Boolean = False) As String
        Get
            If nameOnly = False Then
                Return Folders.Brains() & m_Current_Brain
            Else
                Return m_Current_Brain
            End If
        End Get
        Set(value As String)
            m_Current_Brain = value
        End Set
    End Property

    Private Shared m_Current_Avartar As String = "standard" & Ext.AVATAR
    Public Shared Property Current_Avatar(Optional ByVal nameOnly As Boolean = False) As String
        Get
            If nameOnly = False Then
                Return Folders.Avatars & m_Current_Avartar
            Else
                Return m_Current_Avartar
            End If
        End Get
        Set(value As String)
            m_Current_Avartar = value
        End Set
    End Property


    Public Shared Property Current_Script() As String = "standard"

    Public Shared Property Current_Canvas_DPI() As Integer = 512
    Public Shared Property Current_Avatar_DPI() As Integer = 256

    Public Shared Property Current_Profile() As String = "Standard"
    Public Shared Property Profiles() As New List(Of String)
    Public Shared Property Current_Ai() As String = "Athena"

    Public Shared Property Current_Plugins() As New List(Of String)

    Private Shared m_Current_Cluster As String = "standard" & Ext.DATA
    Public Shared Property Current_Cluster(Optional ByVal nameOnly As Boolean = False) As String
        Get
            If nameOnly = False Then
                Return Folders.Databases & m_Current_Cluster
            Else
                Return m_Current_Cluster
            End If
        End Get
        Set(value As String)
            m_Current_Cluster = value
        End Set
    End Property

    Private Shared m_Current_Word As String = "standard" & Ext.WORD
    Public Shared Property Current_Word(Optional ByVal nameOnly As Boolean = False) As String
        Get
            If nameOnly = False Then
                Return Folders.Word & m_Current_Word
            Else
                Return m_Current_Word
            End If
        End Get
        Set(value As String)
            m_Current_Word = value
        End Set
    End Property

    Public Shared Property Current_Voice() As Integer = 0
    Public Shared Property Current_Voice_Speed() As Integer = 0

#End Region

#Region "StoreToXML"

    Private Shared XmlSrc As New XmlSource
    Private Const ADDRESS_L1 As String = "local"
    Private Const ADDRESS_GENERAL As String = "general"
    Private Const DELIM As String = "[@]"

    Private Shared Property Memory(ByVal sub_address As String, ByVal name As String) As String
        Get
            Return Utility.Trim(XmlSrc.InnerText({ADDRESS_L1, sub_address, name}))
        End Get
        Set(value As String)
            value = Utility.Trim(value)
            Dim address As String() = {ADDRESS_L1, sub_address, name}
            If XmlSrc.DoesNodeExist(address) = True Then
                XmlSrc.InnerText(address, False) = value
            Else
                XmlSrc.Create(address, value)
            End If
        End Set
    End Property

    Public Shared Sub Load(Optional profile As String = "")

        XmlSrc.Loadfile(Files.Save())


        If profile = "" Then
            Current_Profile() = Utility.IsEmpty(Memory(ADDRESS_GENERAL, "profile"), "Profile")
            Profiles() = Utility.ToList(Memory(ADDRESS_GENERAL, "profiles"), DELIM)
            If Profiles.Count = 0 Then Profiles.Add("Profile")
        Else
            Current_Profile() = profile
        End If


        m_Current_Avartar = Utility.IsEmpty(Memory(Current_Profile(), "avatar"), "standard" & Ext.AVATAR)
        m_Current_Brain = Utility.IsEmpty(Memory(Current_Profile(), "brain"), "standard" & Ext.BRAIN)
        m_Current_Cluster = Utility.IsEmpty(Memory(Current_Profile(), "cluster"), "standard" & Ext.DATA)
        m_Current_Word = Utility.IsEmpty(Memory(Current_Profile(), "word"), "standard" & Ext.WORD)
        Current_Package() = Utility.IsEmpty(Memory(Current_Profile(), "package"), "standard")
        Current_Ai() = Utility.IsEmpty(Memory(Current_Profile(), "ainame"), "Athena")
        Current_Script() = Utility.IsEmpty(Memory(Current_Profile(), "script"), "standard")
        Current_Voice() = Utility.ToInt(Memory(Current_Profile(), "voice"), 0)
        Current_Voice_Speed() = Utility.ToInt(Memory(Current_Profile(), "voicespeed"), 0)
        Current_Canvas_DPI() = Utility.ToInt(Memory(Current_Profile(), "canvasdpi"), 512)
        Current_Avatar_DPI() = Utility.ToInt(Memory(Current_Profile(), "avatardpi"), 256)
        Current_Plugins() = Utility.ToList(Memory(Current_Profile(), "plugins"), DELIM)

    End Sub



    Public Shared Sub Save()

        Memory(Current_Profile(), "avatar") = m_Current_Avartar
        Memory(Current_Profile(), "brain") = m_Current_Brain
        Memory(Current_Profile(), "cluster") = m_Current_Cluster
        Memory(Current_Profile(), "word") = m_Current_Word
        Memory(Current_Profile(), "package") = Current_Package()
        Memory(Current_Profile(), "script") = Current_Script()
        Memory(Current_Profile(), "ainame") = Current_Ai()
        Memory(Current_Profile(), "voice") = Utility.ToStr(Current_Voice())
        Memory(Current_Profile(), "voicespeed") = Utility.ToStr(Current_Voice_Speed())
        Memory(Current_Profile(), "canvasdpi") = Utility.ToStr(Current_Canvas_DPI())
        Memory(Current_Profile(), "avatardpi") = Utility.ToStr(Current_Avatar_DPI())
        Memory(Current_Profile(), "plugins") = Utility.Join(Current_Plugins(), DELIM)

        Memory(ADDRESS_GENERAL, "profiles") = Utility.Join(Profiles(), DELIM)
        Memory(ADDRESS_GENERAL, "profile") = Current_Profile()

        XmlSrc.Save(Files.Save())
    End Sub

    Public Shared Sub Delete(Optional profile As String = "")
        If Utility.IsEmpty(profile) = True Then
            XmlSrc.Delete({ADDRESS_L1, Current_Profile()})
        Else
            XmlSrc.Delete({ADDRESS_L1, profile})
        End If
    End Sub

    Public Shared Sub Rename(ByVal newProfile As String)
        XmlSrc.Rename({ADDRESS_L1, Current_Profile()}, newProfile)
    End Sub

#End Region

#Region "SetStyles"

    Public Shared Sub SetStyle(ByRef lables As Label)
        lables.Foreground = Global_FontColor()
        lables.FontFamily = Global_Font()
        lables.FontSize = Global_FontSize()
        lables.Background = Brushes.Transparent
    End Sub

    Public Shared Sub SetStyle(ByRef inputs As TextBox)
        inputs.Foreground = Global_FontColor()
        inputs.FontFamily = Global_Font()
        inputs.FontSize = Global_FontSize()
        inputs.Background = Global_FontBackColor()
    End Sub

    Public Shared Sub SetStyle(ByRef combos As ComboBox)
        combos.Foreground = Global_Window_Color()
        combos.FontFamily = Global_Font()
        combos.FontSize = Global_FontSize()
        combos.Background = Global_FontBackColor()
    End Sub

    Public Shared Sub SetStyle(ByRef borders As Border)
        borders.BorderThickness = New Thickness(Global_Border_Size())
        borders.BorderBrush = Global_Border_Color()
    End Sub

    Public Shared Sub SetStyle(ByRef treeView As TreeViewItem)
        treeView.FontFamily = Global_Font()
        treeView.FontSize = Global_FontSize()
        treeView.Foreground = Global_FontColor()
        treeView.Background = Brushes.Transparent
    End Sub

    Public Shared Sub SetStyle(ByRef listView As ListView)
        listView.FontFamily = Global_Font()
        listView.FontSize = Global_FontSize()
        listView.Foreground = Global_FontColor()
        listView.Background = Brushes.Transparent
    End Sub

#End Region

End Class

Public Class Local


    Public Shared Function FindCenter(ByVal formLocation As Point, ByVal formSize As Size, ByVal dialogSize As Size) As Point
        'This is a way of opening a dialog form in the center of the main form
        'without using Me.StartPosition = FormStartPosition.CenterParent
        Dim centerPoint As New Point

        Dim half_form_X = formSize.Width / 2
        Dim half_form_Y = formSize.Height / 2

        Dim half_Diag_X = dialogSize.Width / 2
        Dim half_Diag_Y = dialogSize.Height / 2

        centerPoint.X = formLocation.X + half_form_X - half_Diag_X
        centerPoint.Y = formLocation.Y + half_form_Y - half_Diag_Y

        Return centerPoint
    End Function

    Public Shared Function FindCenter(ByVal formLocation As Point, ByVal formSize As Size) As Point
        'This is a way of opening a dialog form in the center of the main form
        'without using Me.StartPosition = FormStartPosition.CenterParent
        Dim centerPoint As New Point

        Dim half_form_X = formSize.Width / 2
        Dim half_form_Y = formSize.Height / 2

        centerPoint.X = formLocation.X + half_form_X
        centerPoint.Y = formLocation.Y + half_form_Y

        Return centerPoint
    End Function

    Public Shared Function FindCenter(ByVal form As Control, ByVal Dialog As Control) As Point
        'This is a way of opening a dialog form in the center of the main form
        'without using Me.StartPosition = FormStartPosition.CenterParent
        Dim centerPoint As New Point

        Dim half_form_X = form.Width / 2
        Dim half_form_Y = form.Height / 2

        Dim half_Diag_X = Dialog.Width / 2
        Dim half_Diag_Y = Dialog.Height / 2

        centerPoint.X = form.Margin.Left + (half_form_X - half_Diag_X)
        centerPoint.Y = form.Margin.Top + (half_form_Y - half_Diag_Y)

        Return centerPoint
    End Function


    Public Shared Function GetTextSize(strText As String, ByVal eSize As Double, ByVal family As String) As Size
        Dim ft = New FormattedText(strText, Globalization.CultureInfo.CurrentCulture, FlowDirection.LeftToRight, New Typeface(family), eSize, Brushes.Black)
        Return New Size(ft.WidthIncludingTrailingWhitespace, ft.Height)
    End Function

    Public Shared Function GetTextSize(strText As String, ByVal eFont As Fonts) As Size
        Dim ft = New FormattedText(strText, Globalization.CultureInfo.CurrentCulture, FlowDirection.LeftToRight, New Typeface(eFont.Family().Source), eFont.Size(), eFont.Color())
        Return New Size(ft.WidthIncludingTrailingWhitespace, ft.Height)
    End Function

    Public Shared ReadOnly Property FontFamilies() As List(Of String)
        Get
            Dim newList As New List(Of String)
            For Each Family In Media.Fonts.SystemFontFamilies
                newList.Add(Family.Source)
            Next
            Return newList
        End Get
    End Property

    Public Shared Function IsFont(ByVal fontName As String) As Boolean
        For Each Family In FontFamilies()
            If Utility.Search(Family, fontName, Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Shared Function GetLocalFonts() As List(Of String)
        Dim newList As New List(Of String)
        Dim fontCol As New PrivateFontCollection

        For Each file In Utility.GetAllFiles(Folders.Data(), "*.ttf", Gate.eEND)
            fontCol.AddFontFile(file)
            newList.Add(fontCol.Families(0).Name)
        Next
        Return newList
    End Function

    Public Shared Function ToFontStyle(ByVal strFontStyle As String) As FontStyle
        If Utility.Search(strFontStyle, "Normal", Gate.eEQUALS) = True Then Return FontStyles.Normal
        If Utility.Search(strFontStyle, "Italic", Gate.eEQUALS) = True Then Return FontStyles.Italic
        If Utility.Search(strFontStyle, "Oblique", Gate.eEQUALS) = True Then Return FontStyles.Oblique
        Return FontStyles.Normal
    End Function

    Public Shared Function ToFontWeight(ByVal strFontWeight As String) As FontWeight
        If Utility.Search(strFontWeight, "Black", Gate.eEQUALS) = True Then Return FontWeights.Black
        If Utility.Search(strFontWeight, "Bold", Gate.eEQUALS) = True Then Return FontWeights.Bold
        If Utility.Search(strFontWeight, "DemiBold", Gate.eEQUALS) = True Then Return FontWeights.DemiBold
        If Utility.Search(strFontWeight, "ExtraBlack", Gate.eEQUALS) = True Then Return FontWeights.ExtraBlack
        If Utility.Search(strFontWeight, "ExtraBold", Gate.eEQUALS) = True Then Return FontWeights.ExtraBold
        If Utility.Search(strFontWeight, "ExtraLight", Gate.eEQUALS) = True Then Return FontWeights.ExtraLight
        If Utility.Search(strFontWeight, "Heavy", Gate.eEQUALS) = True Then Return FontWeights.Heavy
        If Utility.Search(strFontWeight, "Light", Gate.eEQUALS) = True Then Return FontWeights.Light
        If Utility.Search(strFontWeight, "Medium", Gate.eEQUALS) = True Then Return FontWeights.Medium
        If Utility.Search(strFontWeight, "Normal", Gate.eEQUALS) = True Then Return FontWeights.Normal
        If Utility.Search(strFontWeight, "Regular", Gate.eEQUALS) = True Then Return FontWeights.Regular
        If Utility.Search(strFontWeight, "SemiBold", Gate.eEQUALS) = True Then Return FontWeights.SemiBold
        If Utility.Search(strFontWeight, "Thin", Gate.eEQUALS) = True Then Return FontWeights.Thin
        If Utility.Search(strFontWeight, "UltraBlack", Gate.eEQUALS) = True Then Return FontWeights.UltraBlack
        If Utility.Search(strFontWeight, "UltraBold", Gate.eEQUALS) = True Then Return FontWeights.UltraBold
        If Utility.Search(strFontWeight, "UltraLight", Gate.eEQUALS) = True Then Return FontWeights.UltraLight
        Return FontWeights.Regular
    End Function

    Public Shared Function ToFontFamily(ByVal strFontFamily As String) As FontFamily
        Return New FontFamily(strFontFamily)
    End Function

    Public Shared Function ToFontStretch(ByVal strFontStretch As String) As FontStretch
        If Utility.Search(strFontStretch, "Condensed", Gate.eEQUALS) = True Then Return FontStretches.Condensed
        If Utility.Search(strFontStretch, "Expanded", Gate.eEQUALS) = True Then Return FontStretches.Expanded
        If Utility.Search(strFontStretch, "ExtraCondensed", Gate.eEQUALS) = True Then Return FontStretches.ExtraCondensed
        If Utility.Search(strFontStretch, "ExtraExpanded", Gate.eEQUALS) = True Then Return FontStretches.ExtraExpanded
        If Utility.Search(strFontStretch, "Medium", Gate.eEQUALS) = True Then Return FontStretches.Medium
        If Utility.Search(strFontStretch, "Normal", Gate.eEQUALS) = True Then Return FontStretches.Normal
        If Utility.Search(strFontStretch, "SemiCondensed", Gate.eEQUALS) = True Then Return FontStretches.SemiCondensed
        If Utility.Search(strFontStretch, "SemiExpanded", Gate.eEQUALS) = True Then Return FontStretches.SemiExpanded
        If Utility.Search(strFontStretch, "UltraCondensed", Gate.eEQUALS) = True Then Return FontStretches.UltraCondensed
        If Utility.Search(strFontStretch, "UltraExpanded", Gate.eEQUALS) = True Then Return FontStretches.UltraExpanded
        Return FontStretches.Normal
    End Function

    Public Shared Function ToFontSize(ByVal strFontSize As String) As Integer
        Return CInt(strFontSize)
    End Function

    Public Shared Function IsPoint(ByVal strX As String, ByVal strY As String) As Boolean
        If IsNumeric(strX) = True And IsNumeric(strY) = True Then
            Return True
        End If
        Return False
    End Function

    Public Shared Function ToPoint(ByVal strX As String, ByVal strY As String) As Point
        Return New Point(Utility.ToInt(strX), Utility.ToInt(strY))
    End Function

    Public Shared Function IntToColor(ByVal strArgb As String()) As Color
        Utility.IsNull(strArgb)
        If strArgb.Length = 4 Then
            Return Color.FromArgb(CByte(strArgb(0)), CByte(strArgb(1)), CByte(strArgb(2)), CByte(strArgb(3)))
        End If
        Return Colors.Black
    End Function

    Public Shared Function IntToBrush(ByVal strArgb As String()) As Brush
        Return New SolidColorBrush(IntToColor(strArgb))
    End Function

    Public Shared Function BrushToColor(ByVal brushs As Brush) As Color
        Return brushs.GetValue(SolidColorBrush.ColorProperty)
    End Function

    Public Shared Function ColorToBrush(ByVal colors As Color) As Brush
        Return New SolidColorBrush(colors)
    End Function

    Public Shared Function ToBrush(ByVal colors As System.Drawing.Color) As Brush
        Return New SolidColorBrush(Color.FromArgb(colors.A, colors.R, colors.G, colors.B))
    End Function

    Public Shared Function ToColor(ByVal brushs As Brush) As System.Drawing.Color
        Dim colorProp = brushs.GetValue(SolidColorBrush.ColorProperty)
        Return System.Drawing.Color.FromArgb(colorProp.A, colorProp.R, colorProp.G, colorProp.B)
    End Function

    Public Shared Sub Debug(ByVal message As String)
        Diagnostics.Debug.WriteLine(message)
    End Sub
    Public Shared Sub Debug(ByVal message As String, ByVal caps As String)
        Diagnostics.Debug.WriteLine(caps)
        Diagnostics.Debug.WriteLine(message)
        Diagnostics.Debug.WriteLine(caps)
    End Sub

End Class

Public Class EmbededResource

    Private Shared thisExe As System.Reflection.Assembly = System.Reflection.Assembly.GetExecutingAssembly()

    Public Shared Function Resource(ByVal file As String) As System.IO.Stream
        Return thisExe.GetManifestResourceStream(file)
    End Function

    Public Shared Function Resources() As List(Of String)
        Return New List(Of String)(thisExe.GetManifestResourceNames())
    End Function


    Public Shared Function BitmapImageResource(ByVal file As String, ByVal width As Double, ByVal height As Double) As BitmapImage
        Dim bmi = Bitmaps.GetBitmapImageFromStream(Resource(file))
        Return Bitmaps.CreateResizedImage(bmi, width, height, 0)
    End Function

    Public Shared Function ImageBrushResource(ByVal file As String, ByVal width As Double, ByVal height As Double) As ImageBrush
        Return New ImageBrush(BitmapImageResource(file, width, height))
    End Function

    Public Shared Function ImageBrushResources(ByVal files As String(), ByVal width As Double, ByVal height As Double) As List(Of ImageBrush)
        Dim newList As New List(Of ImageBrush)

        For Each file In files
            newList.Add(ImageBrushResource(file, width, height))
        Next

        Return newList
    End Function


End Class

Public Class LocalResources

    Public Shared Function GetIcon() As ImageSource
        Return Bitmaps.GetBitmapImageFromFile(Folders.Data() & "athena.ico")
    End Function

    Public Shared Function GetSplash() As ImageSource
        Return Bitmaps.GetBitmapImageFromFile(Folders.Data() & "splash.png")
    End Function

    Public Shared Function GetLauncherText() As String
        Return Utility.TextReader(Folders.Data() & "launcher.txt")
    End Function

    Public Shared Function GetFont() As FontFamily
        Return New FontFamily(Folders.Data() & "#LCD Solid")
    End Function

End Class

