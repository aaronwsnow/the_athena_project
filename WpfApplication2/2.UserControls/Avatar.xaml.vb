Imports System.Windows.Media.Media3D
Imports Matrix
Imports MatrixUtil

Public Class AvatarX

    Private Property Veiwport_Main() As New Viewport3D
    Private Property Grid_Main() As New Grid
    Private Property TextBoxDebug() As New TextBox
    Public Event NotifyByScript(ByVal names As List(Of String), ByVal args As List(Of List(Of String)))

    Property m_ShowDebug As Boolean = False
    Public Property ShowDebug() As Boolean
        Get
            Return m_ShowDebug
        End Get
        Set(value As Boolean)
            m_ShowDebug = value
            If value = False Then

                TextBoxDebug.Dispatcher.Invoke(Sub()
                                                   DebugTimer.Stop()
                                                   FrameTimer.Stop()
                                                   TextBoxDebug.Visibility = Visibility.Hidden
                                               End Sub)
            Else
                TextBoxDebug.Dispatcher.Invoke(Sub()
                                                   DebugTimer.Start()
                                                   FrameTimer.Start()
                                                   TextBoxDebug.Visibility = Visibility.Visible
                                               End Sub)
            End If
        End Set
    End Property

    Public Property ShowAvatar() As Boolean = True
    Public Property ShowCanvas() As Boolean = True
    Public Property SuspendScript() As Boolean = False
    Public WriteOnly Property Suspend() As Boolean
        Set(value As Boolean)
            If value = True Then
                SuspendScript() = True
                ShowCanvas() = False
                ShowAvatar() = False
            Else
                SuspendScript() = False
                ShowCanvas() = True
                ShowAvatar() = True
            End If
        End Set
    End Property

    Private Sub Initialize() Handles Me.Initialized

        Veiwport_Main.VerticalAlignment = VerticalAlignment.Stretch
        Veiwport_Main.HorizontalAlignment = HorizontalAlignment.Stretch


        Grid_Main.VerticalAlignment = VerticalAlignment.Stretch
        Grid_Main.HorizontalAlignment = HorizontalAlignment.Stretch
        Grid_Main.ShowGridLines = False
        Grid_Main.Children.Add(Veiwport_Main)


        TextBoxDebug.TextWrapping = TextWrapping.NoWrap
        TextBoxDebug.Visibility = Visibility.Hidden
        TextBoxDebug.HorizontalAlignment = HorizontalAlignment.Center
        TextBoxDebug.VerticalAlignment = VerticalAlignment.Center
        TextBoxDebug.Background = Brushes.Transparent
        TextBoxDebug.BorderThickness = New Thickness(4)
        Grid_Main.Children.Add(TextBoxDebug)


        Me.Content = Grid_Main

        'UseLayoutRounding = True
        'SnapsToDevicePixels = True

    End Sub

    Private Sub Loads() Handles Me.Loaded
        MouseInfo.Load()
    End Sub

#Region "VARIABLES"

    'hopefully 16 milliseconds intervals will translate to about 60.2(60) frames a second 
    WithEvents LoopTimer As New TimeDispatcher(10)
    WithEvents Talk As New Speech


    Public Const DEF_FACE As String = "face"
    Public Const DEF_TOP As String = "top"
    Public Const DEF_RIGHT As String = "right"
    Public Const DEF_LEFT As String = "left"
    Public Const DEF_BOTTOM As String = "bottom"
    Public Const DEF_BACK As String = "back"
    Public Const DEF_CANVAS As String = "canvas"
    Public Const DEF_ANIMATION As String = "standard"
    Public Const DEF_SCRIPT As String = "standard"
    Public Const DEF_VISEME As String = "vis"

    Public Const DEF_NAME_DELIM As String = "."
    Public Const DEF_ADDRESS_DELIM As String = "/"
    Public Const DEF_PROPERTY_DELIM As String = ":"
    Public Const DEF_RANDOM_DELIM As String = ","
    Public Const DEF_ANIMATE_DELIM As String = vbCrLf
    Public Const DEF_LINE_DELIM As String = "#"
    Public Const DEF_ARGS_DELIM As String = ","


    Private OriginPosition() As Double = New Double() {0, 0, 0}
    Private OriginRotation() As Double = New Double() {0, 0, 0}
    Private VPosition() As Double = New Double() {0, 0, 0}
    Private VRotation() As Double = New Double() {0, 0, 0}
    Private VRandRotation() As Double = New Double() {0, 0, 0}
    Private VRandPosition() As Double = New Double() {0, 0, 0}

    Private VPositionStart() As Double = New Double() {Nothing, Nothing, Nothing}
    Private VRotationStart() As Double = New Double() {Nothing, Nothing, Nothing}
    Private Property Position_Count() As Integer = 0

    Public Property Origin() As Point3D
        Get
            Return Cube.Origin()
        End Get
        Set(value As Point3D)
            Cube.Origin() = value
        End Set
    End Property

    Private Property Script_Cmd() As New List(Of String)
    Private Property Script_Args As New List(Of List(Of String))
    Private Property Script_Hold As Double
    Private Property Script_Next() As Boolean = False

    Public Property CurrentScript() As String = DEF_SCRIPT
    Public Shared Property CanvasSize() As Size

    Private Property Commands() As New Textures.Script
    Public Property IsScriptRunning() As Boolean = False

    Private Property ScriptIndex() As Integer = 1
    Private Property ScriptLine() As Integer = 0

    Private m_Face As String = DEF_ANIMATION
    Public Property Face() As String
        Get
            Return m_Face
        End Get
        Set(value As String)
            m_Face = value
            Face_Frame() = 1
            Face_Step() = 0
            Face_Chance() = 1
            Face_Images().Clear()
            Face_Anime() = Textures.GetAnimations(value, DEF_FACE)
        End Set
    End Property
    Private Property Face_Frame() As Integer = 1
    Private Property Face_Step() As Integer = 0
    Private Property Face_Chance() As Integer = 1
    Private Property Face_Anime() As New Textures.Animations
    Private Property Face_Images() As New List(Of List(Of BitmapImage))

    Private m_Back As String = DEF_ANIMATION
    Public Property Back() As String
        Get
            Return m_Back
        End Get
        Set(value As String)
            m_Back = value
            Back_Frame() = 1
            Back_Step() = 0
            Back_Chance() = 1
            Back_Images().Clear()
            Back_Anime() = Textures.GetAnimations(value, DEF_BACK)
        End Set
    End Property
    Private Property Back_Frame() As Integer = 1
    Private Property Back_Step() As Integer = 0
    Private Property Back_Chance() As Integer = 1
    Private Property Back_Anime() As New Textures.Animations
    Private Property Back_Images() As New List(Of List(Of BitmapImage))

    Private m_Top As String = DEF_ANIMATION
    Public Property Top() As String
        Get
            Return m_Top
        End Get
        Set(value As String)
            m_Top = value
            Top_Frame() = 1
            Top_Step() = 0
            Top_Chance() = 1
            Top_Images().Clear()
            Top_Anime() = Textures.GetAnimations(value, DEF_TOP)
        End Set
    End Property
    Private Property Top_Frame() As Integer = 1
    Private Property Top_Step() As Integer = 0
    Private Property Top_Chance() As Integer = 1
    Private Property Top_Anime() As New Textures.Animations
    Private Property Top_Images() As New List(Of List(Of BitmapImage))

    Private m_Bottom As String = DEF_ANIMATION
    Public Property Bottom() As String
        Get
            Return m_Bottom
        End Get
        Set(value As String)
            m_Bottom = value
            Bottom_Frame() = 1
            Bottom_Step() = 0
            Bottom_Chance() = 1
            Bottom_Images().Clear()
            Bottom_Anime() = Textures.GetAnimations(value, DEF_BOTTOM)
        End Set
    End Property
    Private Property Bottom_Frame() As Integer = 1
    Private Property Bottom_Step() As Integer = 0
    Private Property Bottom_Chance() As Integer = 1
    Private Property Bottom_Anime() As New Textures.Animations
    Private Property Bottom_Images() As New List(Of List(Of BitmapImage))

    Private Shared m_Right As String = DEF_ANIMATION
    Public Shared Property Right() As String
        Get
            Return m_Right
        End Get
        Set(value As String)
            m_Right = value
            Right_Frame() = 1
            Right_Step() = 0
            Right_Chance() = 1
            Right_Images().Clear()
            Right_Anime() = Textures.GetAnimations(value, DEF_RIGHT)
        End Set
    End Property
    Private Shared Property Right_Frame() As Integer = 1
    Private Shared Property Right_Step() As Integer = 0
    Private Shared Property Right_Chance() As Integer = 1
    Private Shared Property Right_Anime() As New Textures.Animations
    Private Shared Property Right_Images() As New List(Of List(Of BitmapImage))

    Private m_Left As String = DEF_ANIMATION
    Public Property Left() As String
        Get
            Return m_Left
        End Get
        Set(value As String)
            m_Left = value
            Left_Frame() = 1
            Left_Step() = 0
            Left_Chance() = 1
            Left_Images().Clear()
            Left_Anime() = Textures.GetAnimations(value, DEF_LEFT)
        End Set
    End Property
    Private Property Left_Frame() As Integer = 1
    Private Property Left_Step() As Integer = 0
    Private Property Left_Chance() As Integer = 1
    Private Property Left_Anime() As New Textures.Animations
    Private Property Left_Images() As New List(Of List(Of BitmapImage))

    Private m_Canvas As String = DEF_ANIMATION
    Public Property Canvas() As String
        Get
            Return m_Canvas
        End Get
        Set(value As String)
            m_Canvas = value
            Canvas_Frame() = 1
            Canvas_Step() = 0
            Canvas_Chance() = 1
            Canvas_Images().Clear()
            Canvas_Anime() = Textures.GetAnimations(value, DEF_CANVAS)
        End Set
    End Property
    Private Property Canvas_Frame() As Integer = 1
    Private Property Canvas_Step() As Integer = 0
    Private Property Canvas_Chance() As Integer = 1
    Private Property Canvas_Anime() As New Textures.Animations
    Private Property Canvas_Images() As New List(Of List(Of BitmapImage))


#End Region


#Region "LOADING"

    WithEvents LocalTextures As New Textures
    Public Event HasLoaded()

    Public Property IsAvatarLoaded() As Boolean = False
    Private Property LoadingThread() As Threading.Thread
    Public Property CurrentAvatar() As String = ""

    Private Sub TexturesLoaded() Handles LocalTextures.IsLoaded
        IsAvatarLoaded() = False
        ResetCharater()
        Script()
        IsAvatarLoaded() = True
        RaiseEvent HasLoaded()
    End Sub

    Public Sub RefreshAvatar()
        LoadingThread = New System.Threading.Thread(AddressOf LoadAvatarZip)
        LoadingThread.IsBackground = False
        LoadingThread.Start()
    End Sub


    Public Sub Load(ByVal avatar As String)
        CurrentAvatar() = avatar
        Grid_Main.Background() = New ImageBrush(Textures.DefaultLoading())

        RefreshAvatar()

        LoadingCube()
        ResetCharater()

        LoopTimer.Start()
        KeyBoardTimer.Start()
    End Sub

    Private Sub LoadAvatarZip()
        If Textures.IsLoading() = False Then
            Textures.Load(CurrentAvatar())
        End If
    End Sub


    Private Sub LoadingCube()
        If Cube.IsLoaded() = False Then
            LoadCubeTextures()
            LoadCubeMesh()
            LoadLighting()
            LoadCamera()
            Cube.IsLoaded() = True
        End If
    End Sub


    Private Sub LoadCubeTextures()
        Cube.Textures.Face() = Textures.DefaultImage()
        Cube.Textures.Back() = Textures.DefaultImage()
        Cube.Textures.Top() = Textures.DefaultImage()
        Cube.Textures.Bottom() = Textures.DefaultImage()
        Cube.Textures.Right() = Textures.DefaultImage()
        Cube.Textures.Left() = Textures.DefaultImage()
    End Sub

    Private Sub LoadCubeMesh()

        Dim size As Double = Cube.DEFAULT_SIZE 'Cube Size
        Dim P1 = New Point3D(-size, size, size)
        Dim P2 = New Point3D(size, size, size)
        Dim P3 = New Point3D(-size, -size, size)
        Dim P4 = New Point3D(size, -size, size)
        Dim P5 = New Point3D(-size, size, -size)
        Dim P6 = New Point3D(size, size, -size)
        Dim P7 = New Point3D(-size, -size, -size)
        Dim P8 = New Point3D(size, -size, -size)

        'face 
        Cube.Sides.Face() = createSides(P1, P2, P3, P4)
        'right
        Cube.Sides.Right() = createSides(P2, P6, P4, P8)
        'back
        Cube.Sides.Back() = createSides(P6, P5, P8, P7)
        'left
        Cube.Sides.Left() = createSides(P5, P1, P7, P3)
        'top
        Cube.Sides.Top() = createSides(P5, P6, P1, P2)
        'bottem
        Cube.Sides.Bottom() = createSides(P3, P4, P7, P8)

    End Sub

    Private Sub LoadLighting()
        Dim myDirectionalLight As New DirectionalLight()
        myDirectionalLight.Color = Colors.White
        myDirectionalLight.Direction = New Vector3D(-0.61, -0.5, -0.61)
        Cube.Lighting = myDirectionalLight
    End Sub

    Private Sub LoadCamera()
        Dim myPCamera As New PerspectiveCamera()
        myPCamera.Position = New Point3D(0, 0, 5)
        myPCamera.LookDirection = New Vector3D(0, 0, -1)
        myPCamera.UpDirection = New Vector3D(0, 1, 0)
        myPCamera.FieldOfView = 60
        Veiwport_Main.Camera = myPCamera
    End Sub

    Private Function createSides(ByVal p1 As Point3D, ByVal p2 As Point3D, ByVal p3 As Point3D, ByVal p4 As Point3D) As GeometryModel3D
        ' The geometry specifes the shape of the 3D plane. In this sample, a flat sheet is created.
        Dim myMeshGeometry3D As New MeshGeometry3D()

        'Create a collection of vertex positions for the MeshGeometry3D. 
        Dim myPositionCollection As New Point3DCollection()
        myPositionCollection.Add(p3)
        myPositionCollection.Add(p4)
        myPositionCollection.Add(p2)
        myPositionCollection.Add(p2)
        myPositionCollection.Add(p1)
        myPositionCollection.Add(p3)
        myMeshGeometry3D.Positions = myPositionCollection


        'Create a collection of normal vectors for the MeshGeometry3D.
        Dim myNormalCollection As New Vector3DCollection()
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myNormalCollection.Add(New Vector3D(0, 0, 1))
        myMeshGeometry3D.Normals = myNormalCollection


        'Create a collection of texture coordinates for the MeshGeometry3D.
        Dim myTextureCoordinatesCollection As New PointCollection()
        myTextureCoordinatesCollection.Add(New Point(0, 0))
        myTextureCoordinatesCollection.Add(New Point(1, 0))
        myTextureCoordinatesCollection.Add(New Point(1, 1))
        myTextureCoordinatesCollection.Add(New Point(1, 1))
        myTextureCoordinatesCollection.Add(New Point(0, 1))
        myTextureCoordinatesCollection.Add(New Point(0, 0))
        myMeshGeometry3D.TextureCoordinates = myTextureCoordinatesCollection


        'Create a collection of triangle indices for the MeshGeometry3D.
        Dim myTriangleIndicesCollection As New Int32Collection()
        myTriangleIndicesCollection.Add(0)
        myTriangleIndicesCollection.Add(1)
        myTriangleIndicesCollection.Add(2)
        myTriangleIndicesCollection.Add(3)
        myTriangleIndicesCollection.Add(4)
        myTriangleIndicesCollection.Add(5)
        myMeshGeometry3D.TriangleIndices = myTriangleIndicesCollection

        Dim myGeometryModel As New GeometryModel3D()
        myGeometryModel.Geometry = myMeshGeometry3D

        Return myGeometryModel
    End Function


#End Region


#Region "DEBUG"

    Private Sub Frames()
        If ShowDebug() = True Then
            Frames_Count() += 1
        End If
    End Sub

    Private Property Current_FrameRate() As Integer = 0
    Private Property Frames_Count() As Integer = 0
    WithEvents FrameTimer As New TimeDispatcher(1000)
    Private Sub FrameTimer_Tick() Handles FrameTimer.Tick
        Try
            Current_FrameRate() = Frames_Count()
            Frames_Count() = 0
        Catch : End Try
    End Sub


    WithEvents DebugTimer As New TimeDispatcher(100)
    Private Sub DebugTimer_Tick() Handles DebugTimer.Tick
        Try
            UpdateDebug()
        Catch : End Try
    End Sub

    Private Sub UpdateDebug()

        If ShowDebug() = True Then
            TextBoxDebug.FontFamily() = Schema.Global_Font()
            TextBoxDebug.Foreground() = Schema.Global_FontColor()
            TextBoxDebug.FontSize() = Schema.Global_FontSize()
            TextBoxDebug.BorderBrush = Schema.Global_Border_Color()

            Dim pos = Cube.CurrentPosition()
            Dim posText = "<Avatar Position> X=" & Math.Round(pos.X, 2) & ", Y=" & Math.Round(pos.Y, 2) & ", Z=" & Math.Round(pos.Z, 2)

            Dim mousePos = Grid_Main.PointFromScreen(MouseInfo.GetMousePosition)

            Dim X = Math.Round(mousePos.X / Grid_Main.ActualWidth() * 100, 2)
            Dim Y = Math.Round(mousePos.Y / Grid_Main.ActualHeight() * 100, 2)

            If X > 100 Then X = 100
            If Y > 100 Then Y = 100

            Dim mouseText = "<Mouse> X=" & X & ", Y=" & Y

            Dim frameText = "<Frame Rate> " & Current_FrameRate()

            TextBoxDebug.Text = frameText & vbCrLf & posText & vbCrLf & mouseText
        End If

    End Sub

#End Region


#Region "GAME LOOP"


    Public Sub GameLoop() Handles LoopTimer.Tick
        Try
            Game()
            Frames()
        Catch : End Try
    End Sub


    Private Sub Game()
        Cube.Reset()

        If IsAvatarLoaded() = True Then

            If SuspendScript() = False Then
                If IsScriptRunning() = True Then
                    UpdateScript()
                Else
                    Idle()
                End If
            End If

            If ShowAvatar() = False Then
                InvisibleCubeTextures()
            Else
                UpdateCubeTextures()
                UpdatePositions()
            End If

            If ShowCanvas() = False Then
                InvisibleBackgroundTexture()
            Else
                UpdateBackGroundTexture()
            End If

            UpdateModals()
        End If
    End Sub

    Private Sub InvisibleCubeTextures()
        Cube.Textures.Face() = Textures.InvisibleImage()
        Cube.Textures.Back() = Textures.InvisibleImage()
        Cube.Textures.Top() = Textures.InvisibleImage()
        Cube.Textures.Bottom() = Textures.InvisibleImage()
        Cube.Textures.Right() = Textures.InvisibleImage()
        Cube.Textures.Left() = Textures.InvisibleImage()
    End Sub

    Private Sub InvisibleBackgroundTexture()
        Grid_Main.Background() = New ImageBrush(Textures.InvisibleImage())
    End Sub

    Private Sub UpdateModals()
        Cube.BindLightingToModel()
        Cube.BindTextureToSide()
        Cube.BindSidesToModel()
        Cube.BindTranslationsToModel()
        Veiwport_Main.Children.Clear()
        BindModelToViewport()
    End Sub

    Private Sub UpdatePositions()
        Cube.Rotate(VRotation(0), {1, 0, 0})
        Cube.Rotate(VRotation(1), {0, 1, 0})
        Cube.Rotate(VRotation(2), {0, 0, 1})
        Cube.Position(VPosition)
    End Sub

    Private Sub Idle()
        RotationsByRandom(30, 30, 20, 0.8, 0.1)
        PositionsByRandom(0.5, 0.5, 0.5, 0.1, 0.1)


        'Home(1)

        'Positions(0, 0, 0, 1, 0.1)

        'Rotations(0, 270, 0, 1, 0.1)

        'Sides(3, 4)

        'Rotation(-45, 0, 0)
        'Rotations(-50, 50, 50)
        'Rotations(100, 100, 10, 1, 0.1)
        'RotationsRnd(400, 400, 400, 4, 0.5)
        'Rotations(0, -460, 0, 5000, 0.9, True)
        'RotationsRnd(400, 400, 400, 4, 0.5, True)

        'Positions(100, 0, -500, 500, 0.001)

        'Position(0, 0, 0)

        'Shake(1, 1)
        'SideSwipe(1.5, 0.1, 0.5)
        'Yes_Move(15, 4, 1)
        'No_Move(15, 4, 1)
        'UpSwipe(100, 4, 0)
    End Sub

    Private Sub UpdateScript()

        If Script_Next = True Then
            If Command(Script_Cmd(), Script_Args(), Script_Hold()) = True Then
                Script_Next() = False
            End If
        Else
            If ScriptIndex() = 0 Then IsScriptRunning() = False

            Dim Command = Commands.GetCommand(ScriptIndex())
            Script_Cmd() = Command.Name()
            Script_Args() = Command.Arguments()
            Script_Hold() = Command.Hold()
            ScriptIndex() = Command.GoToIndex()
            Script_Next() = True
        End If

    End Sub


    Private Sub UpdateCubeTextures()
        UpdateFaceTexture()
        UpdateBackTexture()
        UpdateTopTexture()
        UpdateBottomTexture()
        UpdateRightTexture()
        UpdateLeftTexture()
    End Sub


    Private Sub ResetCharater()
        Face() = DEF_ANIMATION
        Top() = DEF_ANIMATION
        Bottom() = DEF_ANIMATION
        Right() = DEF_ANIMATION
        Left() = DEF_ANIMATION
        Back() = DEF_ANIMATION
        Canvas() = DEF_ANIMATION
    End Sub


    Private Sub UpdateFaceTexture()

        Dim A = Face_Anime()

        If Face_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Face_Images.Add(Lst1)
            Next
        End If

        If Face_Step() > A.Speed() Then Face_Step() = 0

        If A.ChanceSpeed() = Face_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Face_Chance() = 1
        Else
            Face_Chance() += 1
        End If

        If Face_Step() = A.Speed() Then

            Cube.Textures.Face() = Face_Images(A.CurrentSet())(Face_Frame() - 1)

            If A.DoesLoop() = True Then
                If Face_Frame() >= A.Count() Then
                    Face_Frame() = 1
                    Face_Step() = 0
                Else
                    Face_Frame() += 1
                End If
            Else
                If Face_Frame() < A.Count() Then
                    Face_Frame() += 1
                End If
            End If

            If Face_Step() > 0 Then Face_Step() = 0

        Else
            Face_Step() += 1
        End If

    End Sub

    Private Sub UpdateBackTexture()

        Dim A = Back_Anime()

        If Back_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Back_Images.Add(Lst1)
            Next
        End If

        If Back_Step() > A.Speed() Then Back_Step() = 0

        If A.ChanceSpeed() = Back_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Back_Chance() = 1
        Else
            Back_Chance() += 1
        End If

        If Back_Step() = A.Speed() Then

            Cube.Textures.Back() = Back_Images(A.CurrentSet())(Back_Frame() - 1)

            If A.DoesLoop() = True Then
                If Back_Frame() >= A.Count() Then
                    Back_Frame() = 1
                    Back_Step() = 0
                Else
                    Back_Frame() += 1
                End If
            Else
                If Back_Frame() < A.Count() Then
                    Back_Frame() += 1
                End If
            End If

            Back_Step() = 0
        Else
            Back_Step() += 1
        End If

    End Sub

    Private Sub UpdateTopTexture()

        Dim A = Top_Anime()

        If Top_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Top_Images.Add(Lst1)
            Next
        End If

        If Top_Step() > A.Speed() Then Top_Step() = 0

        If A.ChanceSpeed() = Top_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Top_Chance() = 1
        Else
            Top_Chance() += 1
        End If

        If Top_Step() = A.Speed() Then

            Cube.Textures.Top() = Top_Images(A.CurrentSet())(Top_Frame() - 1)

            If A.DoesLoop() = True Then
                If Top_Frame() >= A.Count() Then
                    Top_Frame() = 1
                    Top_Step() = 0
                Else
                    Top_Frame() += 1
                End If
            Else
                If Top_Frame() < A.Count() Then
                    Top_Frame() += 1
                End If
            End If

            Top_Step() = 0
        Else
            Top_Step() += 1
        End If

    End Sub

    Private Sub UpdateBottomTexture()

        Dim A = Bottom_Anime()

        If Bottom_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Bottom_Images.Add(Lst1)
            Next
        End If

        If Bottom_Step() > A.Speed() Then Bottom_Step() = 0

        If A.ChanceSpeed() = Bottom_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Bottom_Chance() = 1
        Else
            Bottom_Chance() += 1
        End If

        If Bottom_Step() = A.Speed() Then

            Cube.Textures.Bottom() = Bottom_Images(A.CurrentSet())(Bottom_Frame() - 1)

            If A.DoesLoop() = True Then
                If Bottom_Frame() >= A.Count() Then
                    Bottom_Frame() = 1
                    Bottom_Step() = 0
                Else
                    Bottom_Frame() += 1
                End If
            Else
                If Bottom_Frame() < A.Count() Then
                    Bottom_Frame() += 1
                End If
            End If

            Bottom_Step() = 0
        Else
            Bottom_Step() += 1
        End If

    End Sub

    Private Sub UpdateRightTexture()

        Dim A = Right_Anime()

        If Right_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Right_Images.Add(Lst1)
            Next
        End If

        If Right_Step() > A.Speed() Then Right_Step() = 0

        If A.ChanceSpeed() = Right_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Right_Chance() = 1
        Else
            Right_Chance() += 1
        End If

        If Right_Step() = A.Speed() Then

            Cube.Textures.Right() = Right_Images(A.CurrentSet())(Right_Frame() - 1)

            If A.DoesLoop() = True Then
                If Right_Frame() >= A.Count() Then
                    Right_Frame() = 1
                    Right_Step() = 0
                Else
                    Right_Frame() += 1
                End If
            Else
                If Right_Frame() < A.Count() Then
                    Right_Frame() += 1
                End If
            End If

            Right_Step() = 0
        Else
            Right_Step() += 1
        End If


    End Sub

    Private Sub UpdateLeftTexture()

        Dim A = Left_Anime()

        If Left_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Left_Images.Add(Lst1)
            Next
        End If

        If Left_Step() > A.Speed() Then Left_Step() = 0

        If A.ChanceSpeed() = Left_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Left_Chance() = 1
        Else
            Left_Chance() += 1
        End If

        If Left_Step() = A.Speed() Then

            Cube.Textures.Left() = Left_Images(A.CurrentSet())(Left_Frame() - 1)

            If A.DoesLoop() = True Then
                If Left_Frame() >= A.Count() Then
                    Left_Frame() = 1
                    Left_Step() = 0
                Else
                    Left_Frame() += 1
                End If
            Else
                If Left_Frame() < A.Count() Then
                    Left_Frame() += 1
                End If
            End If

            Left_Step() = 0
        Else
            Left_Step() += 1
        End If

    End Sub


    Public Property CurrentProfile() As String = ""
    Public Event ProfileChange(ByVal profile As Textures.Profile)

    Private Sub UpdateBackGroundTexture()

        Dim A = Canvas_Anime()

        If CurrentProfile() <> A.Profile() Then
            CurrentProfile() = A.Profile()
            RaiseEvent ProfileChange(Textures.GetProfile(A.Profile()))
        End If

        If Canvas_Images.Count() = 0 Then
            For Each i In A.IndexSets()
                Dim Lst1 As New List(Of BitmapImage)

                For c = 1 To i.Count
                    Lst1.Add(Nothing)
                Next

                For Each j In i
                    Lst1.Item(j.Index() - 1) = j.ImageFromStream()
                Next

                Canvas_Images.Add(Lst1)
            Next
        End If

        If Canvas_Step() > A.Speed() Then Canvas_Step() = 0

        If A.ChanceSpeed() = Canvas_Chance() Then
            A.CurrentSet() = Utility.Rand(0, A.SetCount() - 1)
            Canvas_Chance() = 1
        Else
            Canvas_Chance() += 1
        End If

        If Canvas_Step() = A.Speed() Then

            Grid_Main.Background() = New ImageBrush(Canvas_Images(A.CurrentSet())(Canvas_Frame() - 1))

            If A.DoesLoop() = True Then
                If Canvas_Frame() >= A.Count() Then
                    Canvas_Frame() = 1
                    Canvas_Step() = 0
                Else
                    Canvas_Frame() += 1
                End If
            Else
                If Canvas_Frame() < A.Count() Then
                    Canvas_Frame() += 1
                End If
            End If

            Canvas_Step() = 0
        Else
            Canvas_Step() += 1
        End If

    End Sub

    Private Sub BindModelToViewport()
        Dim myModelVisual3D As New ModelVisual3D()
        myModelVisual3D.Content = Cube.Model
        Veiwport_Main.Children.Add(myModelVisual3D)
    End Sub

#End Region


#Region "KEYBOARD"

    Public Shared Property IsActivated() As Boolean = False
    WithEvents Listener As New Keyboards.KeyboardListener()
    WithEvents KeyBoardTimer As New TimeDispatcher(10)

    Private Sub Application_Exit() Handles MyBase.Unloaded
        Listener.Dispose()
    End Sub

    Private Sub KeyLoop() Handles KeyBoardTimer.Tick

        If IsActivated() = True Then

            If Keyboards.IsKeyPressed() = True Then
                If Keyboards.Search({Key.Escape}, Gate.eOR) = True Then
                    Utility.ExitProgram()
                End If

                If Keyboards.Search({Key.LeftShift, Key.RightShift}, Gate.eOR) = True Then
                    SuspendScript() = True

                    If Keyboards.Search({Key.Up}, Gate.eOR) = True Then
                        RotationByDirection(-1, 0, 0)
                    ElseIf Keyboards.Search({Key.Down}, Gate.eOR) = True Then
                        RotationByDirection(1, 0, 0)
                    End If

                    If Keyboards.Search({Key.Left}, Gate.eOR) = True Then
                        RotationByDirection(0, -1, 0)
                    ElseIf Keyboards.Search({Key.Right}, Gate.eOR) = True Then
                        RotationByDirection(0, 1, 0)
                    End If

                End If

                If Keyboards.Search({Key.LeftCtrl, Key.RightCtrl}, Gate.eOR) = True Then
                    SuspendScript() = True

                    If Keyboards.Search({Key.Left}, Gate.eOR) = True Then
                        RotationByDirection(0, 0, 1)
                    ElseIf Keyboards.Search({Key.Right}, Gate.eOR) = True Then
                        RotationByDirection(0, 0, -1)
                    End If

                    If Keyboards.Search({Key.Up}, Gate.eOR) = True Then
                        PositionByDirection(0, 0, -0.02)
                    ElseIf Keyboards.Search({Key.Down}, Gate.eOR) = True Then
                        PositionByDirection(0, 0, 0.02)
                    End If

                End If

                If Keyboards.Search({Key.Space}, Gate.eOR) = True Then
                    SuspendScript() = True

                    If Keyboards.Search({Key.Up}, Gate.eOR) = True Then
                        PositionByDirection(0, 0.02, 0)
                    ElseIf Keyboards.Search({Key.Down}, Gate.eOR) = True Then
                        PositionByDirection(0, -0.02, 0)
                    End If

                    If Keyboards.Search({Key.Left}, Gate.eOR) = True Then
                        PositionByDirection(-0.02, 0, 0)
                    ElseIf Keyboards.Search({Key.Right}, Gate.eOR) = True Then
                        PositionByDirection(0.02, 0, 0)
                    End If

                End If

                If Keyboards.Search({Key.Home}, Gate.eOR) = True Then
                    Home()
                    IsScriptRunning() = False
                End If

            Else
                SuspendScript() = False
            End If

        End If
    End Sub

    Private Sub KeyTest_KeyDown(ByVal sender As Object, e As Keyboards.RawKeyEventArgs) Handles Listener.KeyDownOnce

        If IsActivated() = True Then

            If Keyboards.Search({Key.LeftCtrl, Key.RightCtrl}, Gate.eOR) = True Then

                If Keyboards.Search({Key.T}, Gate.eOR) = True Then
                    Test()
                End If

                If Keyboards.Search({Key.R}, Gate.eOR) = True Then
                    RefreshAvatar()
                End If

                If Keyboards.Search({Key.D}, Gate.eOR) = True Then
                    If ShowDebug() = True Then
                        ShowDebug() = False
                    Else
                        ShowDebug() = True
                    End If
                End If

                If Keyboards.Search({Key.S}, Gate.eOR) = True Then
                    ScriptStop()
                    SpeechStop()
                End If

            End If

        End If
    End Sub

    Public Sub Test()

        'Face() = "sad"
        'Local.Debug(Speak("Hello, how are you today Aaron Wesley Snow."))

        'Script("standard")

        'Textures.PlaySound("jew")

        'Local.Debug("test")
    End Sub


#End Region


#Region "TOOLS"


    Private Sub RotationsByRandom(ByVal max_X As Double, ByVal max_Y As Double, ByVal max_Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal)

        If VRotationStart(0) = Nothing Then VRotationStart(0) = VRotation(0)
        If VRotationStart(1) = Nothing Then VRotationStart(1) = VRotation(1)
        If VRotationStart(2) = Nothing Then VRotationStart(2) = VRotation(2)

        Dim X = VRandRotation(0)  'X
        Dim Y = VRandRotation(1)  'Y
        Dim Z = VRandRotation(2)  'Z

        Dim GOAL_REACHED As Boolean = True

        If inDegree(VRotation(0), X, SPEED, SPEED, True) = False Then

            If inDegree(VRotation(0), X, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(0), VRotation(0), SPEED, X, SINE, -1)
                VRotation(0) = addDegree(VRotation(0), -newSpeed) 'X
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(0), VRotation(0), SPEED, X, SINE, 1)
                VRotation(0) = addDegree(VRotation(0), newSpeed) 'X
            End If

            GOAL_REACHED = False
        End If

        If inDegree(VRotation(1), Y, SPEED, SPEED, True) = False Then

            If inDegree(VRotation(1), Y, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(1), VRotation(1), SPEED, Y, SINE, -1)
                VRotation(1) = addDegree(VRotation(1), -newSpeed) 'X
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(1), VRotation(1), SPEED, Y, SINE, 1)
                VRotation(1) = addDegree(VRotation(1), newSpeed) 'X
            End If

            GOAL_REACHED = False
        End If

        If inDegree(VRotation(2), Z, SPEED, SPEED, True) = False Then

            If inDegree(VRotation(2), Z, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(2), VRotation(2), SPEED, Z, SINE, -1)
                VRotation(2) = addDegree(VRotation(2), -newSpeed) 'X
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(2), VRotation(2), SPEED, Z, SINE, 1)
                VRotation(2) = addDegree(VRotation(2), newSpeed) 'X
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            VRotationStart(0) = VRotation(0)
            VRotationStart(1) = VRotation(1)
            VRotationStart(2) = VRotation(2)

            Dim rndX = Utility.RandDouble(-max_Y, max_Y) / 2 'Y
            Dim rndY = Utility.RandDouble(-max_X, max_X) / 2 'X
            Dim rndZ = Utility.RandDouble(-max_Z, max_Z) / 2 'Z

            If rndX < 0 Then rndX = 360 + rndX
            If rndY < 0 Then rndY = 360 + rndY
            If rndZ < 0 Then rndZ = 360 + rndZ

            rndX = addDegree(OriginRotation(0), rndX)
            rndY = addDegree(OriginRotation(1), rndY)
            rndZ = addDegree(OriginRotation(2), rndZ)

            VRandRotation(0) = rndX 'Y
            VRandRotation(1) = rndY 'X
            VRandRotation(2) = rndZ 'Z
        End If

    End Sub

    Private Sub RotationsByRandom_MultipleTurns(ByVal max_X As Double, ByVal max_Y As Double, ByVal max_Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal)

        If VRotationStart(0) = Nothing Then VRotationStart(0) = VRotation(0)
        If VRotationStart(1) = Nothing Then VRotationStart(1) = VRotation(1)
        If VRotationStart(2) = Nothing Then VRotationStart(2) = VRotation(2)

        Dim X = VRandRotation(0)  'X
        Dim Y = VRandRotation(1)  'Y
        Dim Z = VRandRotation(2)  'Z

        Dim GOAL_REACHED As Boolean = True

        If VRotation(0) < (X - SPEED) Or VRotation(0) > (X + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(0), VRotation(0), SPEED, X, SINE)
            If VRotation(0) < X Then
                VRotation(0) += newSpeed 'X
            Else
                VRotation(0) -= newSpeed 'X
            End If

            GOAL_REACHED = False
        End If

        If VRotation(1) < (Y - SPEED) Or VRotation(1) > (Y + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(1), VRotation(1), SPEED, Y, SINE)

            If VRotation(1) < Y Then
                VRotation(1) += newSpeed 'Y
            Else
                VRotation(1) -= newSpeed 'Y
            End If

            GOAL_REACHED = False
        End If

        If VRotation(2) < (Z - SPEED) Or VRotation(2) > (Z + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(2), VRotation(2), SPEED, Z, SINE)
            If VRotation(2) < Z Then
                VRotation(2) += newSpeed 'Z
            Else
                VRotation(2) -= newSpeed 'Z
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            VRotationStart(0) = VRotation(0)
            VRotationStart(1) = VRotation(1)
            VRotationStart(2) = VRotation(2)

            Dim rndX = Utility.RandDouble(-max_Y, max_Y) / 2 'Y
            Dim rndY = Utility.RandDouble(-max_X, max_X) / 2 'X
            Dim rndZ = Utility.RandDouble(-max_Z, max_Z) / 2  'Z

            If rndX < 0 Then rndX = 360 + rndX
            If rndY < 0 Then rndY = 360 + rndY
            If rndZ < 0 Then rndZ = 360 + rndZ

            rndX = addDegree(OriginRotation(0), rndX)
            rndY = addDegree(OriginRotation(1), rndY)
            rndZ = addDegree(OriginRotation(2), rndZ)

            VRandRotation(0) = rndX 'Y
            VRandRotation(1) = rndY 'X
            VRandRotation(2) = rndZ 'Z
        End If

    End Sub


    Private Function Rotations(ByVal X As Double, ByVal Y As Double, ByVal Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal) As Boolean

        If VRotationStart(0) = Nothing Then VRotationStart(0) = VRotation(0)
        If VRotationStart(1) = Nothing Then VRotationStart(1) = VRotation(1)
        If VRotationStart(2) = Nothing Then VRotationStart(2) = VRotation(2)

        Dim GOAL_REACHED As Boolean = True

        If inDegree(VRotation(0), X, SPEED, SPEED, True) = False Then


            If inDegree(VRotation(0), X, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(0), VRotation(0), SPEED, X, SINE, -1)
                VRotation(0) = addDegree(VRotation(0), -newSpeed) 'X
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(0), VRotation(0), SPEED, X, SINE, 1)
                VRotation(0) = addDegree(VRotation(0), newSpeed) 'X
            End If

            GOAL_REACHED = False
        End If

        If inDegree(VRotation(1), Y, SPEED, SPEED, True) = False Then


            If inDegree(VRotation(1), Y, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(1), VRotation(1), SPEED, Y, SINE, -1)
                VRotation(1) = addDegree(VRotation(1), -newSpeed) 'Y
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(1), VRotation(1), SPEED, Y, SINE, 1)
                VRotation(1) = addDegree(VRotation(1), newSpeed) 'Y
            End If

            GOAL_REACHED = False
        End If

        If inDegree(VRotation(2), Z, SPEED, SPEED, True) = False Then


            If inDegree(VRotation(2), Z, 0, 180, False) = True Then
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(2), VRotation(2), SPEED, Z, SINE, -1)
                VRotation(2) = addDegree(VRotation(2), -newSpeed) 'Z
            Else
                Dim newSpeed = SpeedBetweenDegrees(VRotationStart(2), VRotation(2), SPEED, Z, SINE, 1)
                VRotation(2) = addDegree(VRotation(2), newSpeed) 'Z
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            Rotation(VRotation)
        End If

        Return GOAL_REACHED
    End Function

    Private Function Rotations_MultipleTurns(ByVal X As Double, ByVal Y As Double, ByVal Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal) As Boolean

        If VRotationStart(0) = Nothing Then VRotationStart(0) = VRotation(0)
        If VRotationStart(1) = Nothing Then VRotationStart(1) = VRotation(1)
        If VRotationStart(2) = Nothing Then VRotationStart(2) = VRotation(2)

        Dim GOAL_REACHED As Boolean = True

        If VRotation(0) < (X - SPEED) Or VRotation(0) > (X + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(0), VRotation(0), SPEED, X, SINE)
            If VRotation(0) < X Then
                VRotation(0) += newSpeed 'X
            Else
                VRotation(0) -= newSpeed 'X
            End If

            GOAL_REACHED = False
        End If

        If VRotation(1) < (Y - SPEED) Or VRotation(1) > (Y + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(1), VRotation(1), SPEED, Y, SINE)

            If VRotation(1) < Y Then
                VRotation(1) += newSpeed 'Y
            Else
                VRotation(1) -= newSpeed 'Y
            End If

            GOAL_REACHED = False
        End If

        If VRotation(2) < (Z - SPEED) Or VRotation(2) > (Z + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VRotationStart(2), VRotation(2), SPEED, Z, SINE)
            If VRotation(2) < Z Then
                VRotation(2) += newSpeed 'Z
            Else
                VRotation(2) -= newSpeed 'Z
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            Rotation(VRotation)
        End If

        Return GOAL_REACHED
    End Function



    Private Sub RotationByDirection(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
        VRotation(0) = addDegree(VRotation(0), X)
        VRotation(1) = addDegree(VRotation(1), Y)
        VRotation(2) = addDegree(VRotation(2), Z)

        Rotation(VRotation(0), VRotation(1), VRotation(2))
    End Sub

    Private Sub Rotation(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)

        If X >= 360 Or Y >= 360 Or Z >= 360 Then
            X = X Mod 360
            Y = Y Mod 360
            Z = Z Mod 360

            If X < 0 Then X = 360 + X
            If Y < 0 Then Y = 360 + Y
            If Z < 0 Then Z = 360 + Z
        End If

        VRotation(0) = X 'X-axis
        VRotation(1) = Y 'Y-axis
        VRotation(2) = Z 'Z-axis

        OriginRotation(0) = X 'X-axis
        OriginRotation(1) = Y 'Y-axis
        OriginRotation(2) = Z 'Z-axis

        VRandRotation(0) = X 'X-axis
        VRandRotation(1) = Y 'Y-axis
        VRandRotation(2) = Z 'Z-axis

        VRotationStart(0) = X 'X-axis
        VRotationStart(1) = Y 'X-axis
        VRotationStart(2) = Z 'Z-axis
    End Sub

    Private Sub Rotation(ByVal rotate As Double())
        Rotation(rotate(0), rotate(1), rotate(2))
    End Sub



    Private Sub PositionsByRandom(ByVal max_X As Double, ByVal max_Y As Double, ByVal max_Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal)
        SPEED = SPEED / 40

        If VPositionStart(0) = Nothing Then VPositionStart(0) = VPosition(0)
        If VPositionStart(1) = Nothing Then VPositionStart(1) = VPosition(1)
        If VPositionStart(2) = Nothing Then VPositionStart(2) = VPosition(2)

        Dim X = VRandPosition(0)  'X
        Dim Y = VRandPosition(1)  'Y
        Dim Z = VRandPosition(2)  'Z

        Dim GOAL_REACHED As Boolean = True

        If VPosition(0) < (X - SPEED) Or VPosition(0) > (X + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(0), VPosition(0), SPEED, X, SINE)
            If VPosition(0) < X Then
                VPosition(0) += newSpeed 'X
            Else
                VPosition(0) -= newSpeed 'X
            End If

            GOAL_REACHED = False
        End If

        If VPosition(1) < (Y - SPEED) Or VPosition(1) > (Y + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(1), VPosition(1), SPEED, Y, SINE)
            If VPosition(1) < Y Then
                VPosition(1) += newSpeed 'Y
            Else
                VPosition(1) -= newSpeed 'Y
            End If

            GOAL_REACHED = False
        End If

        If VPosition(2) < (Z - SPEED) Or VPosition(2) > (Z + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(2), VPosition(2), SPEED, Z, SINE)
            If VPosition(2) < Z Then
                VPosition(2) += newSpeed 'Z
            Else
                VPosition(2) -= newSpeed 'Z
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            VPositionStart(0) = VPosition(0)
            VPositionStart(1) = VPosition(1)
            VPositionStart(2) = VPosition(2)

            Dim rndX = Utility.RandDouble(-max_X, max_X)
            Dim rndY = Utility.RandDouble(-max_Y, max_Y)
            Dim rndZ = Utility.RandDouble(-max_Z, max_Z)

            rndX = OriginPosition(0) + rndX
            rndY = OriginPosition(1) + rndY
            rndZ = OriginPosition(2) + rndZ

            VRandPosition(0) = rndX 'X
            VRandPosition(1) = rndY 'Y
            VRandPosition(2) = rndZ 'Z
        End If

    End Sub

    Private Function Positions(ByVal X As Double, ByVal Y As Double, ByVal Z As Double, ByVal SPEED As Double, ByVal SINE As Decimal) As Boolean
        Dim GOAL_REACHED As Boolean = True
        SPEED = SPEED / 40

        If VPositionStart(0) = Nothing Then VPositionStart(0) = VPosition(0)
        If VPositionStart(1) = Nothing Then VPositionStart(1) = VPosition(1)
        If VPositionStart(2) = Nothing Then VPositionStart(2) = VPosition(2)


        If VPosition(0) < (X - SPEED) Or VPosition(0) > (X + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(0), VPosition(0), SPEED, X, SINE)

            If VPosition(0) < X Then
                VPosition(0) += newSpeed 'X
            Else
                VPosition(0) -= newSpeed 'X
            End If

            GOAL_REACHED = False
        End If

        If VPosition(1) < (Y - SPEED) Or VPosition(1) > (Y + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(1), VPosition(1), SPEED, Y, SINE)

            If VPosition(1) < Y Then
                VPosition(1) += newSpeed 'Y
            Else
                VPosition(1) -= newSpeed 'Y
            End If

            GOAL_REACHED = False
        End If

        If VPosition(2) < (Z - SPEED) Or VPosition(2) > (Z + SPEED) Then
            Dim newSpeed = SpeedBetweenPoints(VPositionStart(2), VPosition(2), SPEED, Z, SINE)

            If VPosition(2) < Z Then
                VPosition(2) += newSpeed 'Z
            Else
                VPosition(2) -= newSpeed 'Z
            End If

            GOAL_REACHED = False
        End If

        If GOAL_REACHED = True Then
            Position(VPosition)
        End If

        Return GOAL_REACHED
    End Function

    Private Sub Position(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
        VPosition(0) = X
        VPosition(1) = Y
        VPosition(2) = Z

        OriginPosition(0) = X
        OriginPosition(1) = Y
        OriginPosition(2) = Z

        VRandPosition(0) = X
        VRandPosition(1) = Y
        VRandPosition(2) = Z

        VPositionStart(0) = X
        VPositionStart(1) = Y
        VPositionStart(2) = Z
    End Sub

    Private Sub PositionByDirection(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
        VPosition(0) += X
        VPosition(1) += Y
        VPosition(2) += Z

        Position(VPosition(0), VPosition(1), VPosition(2))
    End Sub

    Private Sub Position(ByVal location As Double())
        Position(location(0), location(1), location(2))
    End Sub



    Private Function Hold(ByVal TICKS As Integer, ByRef intVar As Integer) As Boolean
        Dim GOAL_REACHED As Boolean = True

        '-1 will make this perminent
        If TICKS < 0 Then Return False
        If TICKS = 0 Then Return True

        If TICKS >= intVar Then
            intVar += 1
            GOAL_REACHED = False
        Else
            intVar = 0
        End If

        Return GOAL_REACHED
    End Function

    Private Function HoldRnd(ByVal min_Tick As Double, ByVal max_Tick As Double, ByRef intVar As Integer) As Boolean
        Dim GOAL_REACHED As Boolean = True

        Dim rnd = Utility.Rand(min_Tick, max_Tick)

        If rnd >= intVar Then
            intVar += 1
            GOAL_REACHED = False
        Else
            intVar = 0
        End If

        Return GOAL_REACHED
    End Function

    Private Function HoldRnd(ByVal TICKS As Double, ByVal min As Double, ByVal max As Double, ByRef intVar As Integer) As Integer

        If Hold(TICKS, intVar) = True Or intVar = 0 Then
            intVar = Utility.Rand(min, max)
        End If

        Return intVar
    End Function

    Private Function HoldRndDbl(ByVal TICKS As Double, ByVal min As Double, ByVal max As Double, ByRef intVar As Integer) As Double

        If Hold(TICKS, intVar) = True Or intVar = 0 Then
            intVar = Utility.RandDouble(min, max)
        End If

        Return intVar
    End Function

    Private Function Circles(ByVal Frequency As Double, ByVal Amplitude As Double, ByRef intVar As Integer) As Double
        intVar += Frequency
        Return (Math.Sin(intVar)) * Amplitude
    End Function

    Private Function SpeedBetweenPoints(ByVal startPOS As Double, ByVal currentPOS As Double, ByVal currentSpeed As Double, ByVal endPOS As Double, ByVal minSpeed As Decimal) As Double

        Dim newSpeed = currentSpeed
        Dim totalDist = endPOS - startPOS
        Dim position = Math.Abs(currentPOS - startPOS)
        Dim midWay = Math.Abs(totalDist / 2)

        If startPOS = endPOS Then Return currentSpeed

        If position >= midWay Then
            'speed here must decrease
            Dim count = position - midWay
            Dim percentage = (1 - (count / midWay)) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        Else
            'speed here must increase
            Dim percentage = (position / midWay) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        End If

        Return newSpeed
    End Function

    Private Function SpeedBetweenDegrees(ByVal startPOS As Double, ByVal currentPOS As Double, ByVal currentSpeed As Double, ByVal endPOS As Double, ByVal minSpeed As Decimal, ByVal direction As Integer) As Double

        Dim newSpeed = currentSpeed
        Dim totalDist As Double = 0.0
        Dim position As Double = 0.0

        ''direction' sets which direction to go on a cicle to find the end point, -1 or 1

        If direction = -1 Then
            totalDist = addDegree(startPOS, -endPOS)
            position = addDegree(startPOS, -currentPOS)
        Else
            totalDist = addDegree(endPOS, -startPOS)
            position = addDegree(currentPOS, -startPOS)
        End If

        Dim midWay = Math.Abs(totalDist / 2)
        If startPOS = endPOS Then Return currentSpeed

        If position >= midWay Then
            'speed here must decrease
            Dim count = position - midWay
            Dim percentage = (1 - (count / midWay)) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        Else
            'speed here must increase
            Dim percentage = (position / midWay) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        End If

        Return newSpeed
    End Function

    Private Function SpeedBetweenDegrees(ByVal startPOS As Double, ByVal currentPOS As Double, ByVal currentSpeed As Double, ByVal endPOS As Double, ByVal minSpeed As Decimal) As Double
        'UNUSED
        'It shows finds the shortest distance between two points on a circle and chooses that route

        Dim newSpeed = currentSpeed
        Dim totalDist As Double = 0.0
        Dim position As Double = 0.0

        Dim dir1 = addDegree(startPOS, -endPOS)
        Dim dir2 = addDegree(endPOS, -startPOS)

        If dir1 < dir2 Then
            totalDist = dir1
            position = addDegree(startPOS, -currentPOS)
        Else
            totalDist = dir2
            position = addDegree(currentPOS, -startPOS)
        End If

        Dim midWay = Math.Abs(totalDist / 2)
        If startPOS = endPOS Then Return currentSpeed

        If position >= midWay Then
            'speed here must decrease
            Dim count = position - midWay
            Dim percentage = (1 - (count / midWay)) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        Else
            'speed here must increase
            Dim percentage = (position / midWay) ^ 1.1

            If percentage > minSpeed Then
                newSpeed = percentage * currentSpeed
            Else
                newSpeed = minSpeed * currentSpeed
            End If
        End If

        Return newSpeed
    End Function



    Private Function IsNegative(ByVal int As Double) As Boolean
        If int < 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Function IsDecimal(ByVal int As Double) As Boolean
        int = Math.Abs(int)
        If int < 1 And int > 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Sub resetDegree()
        VRotation(0) = VRotation(0) Mod 360
        VRotation(1) = VRotation(1) Mod 360
        VRotation(2) = VRotation(2) Mod 360
    End Sub

    Private Function addDegree(ByVal degrees As Double, ByVal degreeToAdd As Double) As Double
        Dim results = 0.0
        degrees = degrees Mod 360
        degreeToAdd = degreeToAdd Mod 360
        results = degrees + degreeToAdd
        If results < 0 Then results = 360 + results
        Return results Mod 360
    End Function

    Private Function inDegree(ByVal degreeToCheck As Double, ByVal degree As Double, ByVal lowTol As Double, ByVal highTol As Double, ByVal inclussive As Boolean) As Boolean

        degreeToCheck = degreeToCheck Mod 360
        degree = degree Mod 360

        Dim highDegree = addDegree(degree, highTol)
        Dim lowDegree = addDegree(degree, -lowTol)

        If degreeToCheck = degree Then Return True

        If inclussive = True Then

            If 0 >= (degree - lowTol) Then
                If degreeToCheck <= 360 And degreeToCheck >= lowDegree Then
                    Return True
                End If

                If degreeToCheck > 0 And degreeToCheck < degree Then
                    Return True
                End If
            End If

            If 360 <= (degree + highTol) Then

                If degreeToCheck >= 0 And degreeToCheck <= highDegree Then
                    Return True
                End If

                If degreeToCheck < 360 And degreeToCheck > degree Then
                    Return True
                End If

            End If
        Else

            If 0 > (degree - lowTol) Then
                If degreeToCheck <= 360 And degreeToCheck >= lowDegree Then
                    Return True
                End If

                If degreeToCheck > 0 And degreeToCheck < degree Then
                    Return True
                End If
            End If

            If 360 < (degree + highTol) Then

                If degreeToCheck >= 0 And degreeToCheck <= highDegree Then
                    Return True
                End If

                If degreeToCheck < 360 And degreeToCheck > degree Then
                    Return True
                End If

            End If

        End If

        If degreeToCheck <= highDegree And degreeToCheck >= lowDegree Then
            Return True
        Else
            Return False
        End If

    End Function


#End Region


#Region "SCRIPTS"

    Public Sub ScriptStop()
        IsScriptRunning() = False
        ScriptIndex() = 1
        ScriptLine() = 0
        Script_Next() = False
        cmd_hold = 0
    End Sub

    Public Sub Script()
        ScriptStop()
        SpeechStop()

        CurrentScript() = Utility.RemoveExtension(CurrentScript())
        Commands() = Textures.GetScript(CurrentScript())

        IsScriptRunning() = True
    End Sub

    Public Sub Script(ByVal name As String)
        CurrentScript() = Utility.RemoveExtension(name)

        ScriptStop()
        SpeechStop()

        Commands() = Textures.GetScript(CurrentScript())

        IsScriptRunning() = True
    End Sub

    Public Sub Script(ByVal code As List(Of String))

        ScriptStop()
        SpeechStop()

        Commands() = Textures.LoadScript(code)

        IsScriptRunning() = True
    End Sub


    Private Function Dbl(ByVal db As String) As Double
        Return Utility.ToDbl(db, 0)
    End Function

    Private Function Dec(ByVal dc As String) As Decimal
        Return Utility.ToDec(dc, 0)
    End Function

    Private Function ToInt(ByVal ints As String) As Double
        Return Utility.ToInt(ints, 0)
    End Function

    Private Function ToBool(ByVal bools As String) As Boolean
        Return Utility.ToBool(bools)
    End Function

    Private Property cmd_hold As Integer = 0
    Private Property names_list As New List(Of String)

    Private Function Command(ByVal names As List(Of String), ByVal args As List(Of List(Of String)), ByVal holds As Integer) As Boolean
        'command functions that are perpetual will only stop when the hold function is set to true
        'non perpetual functions will continue until fullfilled
        If IsNothing(names) = True Then Return True
        If IsNothing(args) = True Then Return True
        If IsNothing(holds) = True Then Return True

        names_list.Clear()
        names_list.AddRange(names)
        Dim index As Integer = 0

        'trimming the args array
        Dim newList As New List(Of List(Of String))
        For Each argl In args
            newList.Add(Utility.Trim(argl))
        Next
        args = newList


        index = Utility.IndexOf(names_list, "notify", True, Gate.eEQUALS)
        If index > -1 Then
            'if notify is found then all names and args that are found on that line is passed to notification
            RaiseEvent NotifyByScript(names_list, args)
            names_list.RemoveAt(index)
        End If

        index = Utility.IndexOf(names_list, "lookat", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 3 Then
                Rotation(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "look", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                If Rotations(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4))) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "rotate", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                If Rotations_MultipleTurns(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4))) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "twirl", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 3 Then
                RotationByDirection(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "randomlook", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                RotationsByRandom(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "randomfarlook", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                RotationsByRandom_MultipleTurns(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "travelto", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 3 Then
                Position(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "travel", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                If Positions(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4))) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "randomtravel", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 5 Then
                PositionsByRandom(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)), Dec(args(index)(4)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "randomhold", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                If HoldRnd(Dbl(args(index)(0)), Dbl(args(index)(1)), cmd_hold) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "home", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                If args(index)(0) = "null" Then
                    Home()
                    names_list.RemoveAt(index)
                Else
                    If Home(Dbl(args(index)(0))) = True Then
                        names_list.RemoveAt(index)
                    End If
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "yesmove", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                Yes_Move(Dbl(args(index)(0)), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "nomove", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                No_Move(Dbl(args(index)(0)), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "sideswipe", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                Side_Swipe(Dbl(args(index)(0)), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "upswipe", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                Up_Swipe(Dbl(args(index)(0)), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "shake", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                Shake(Dbl(args(index)(0)), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "showside", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 2 Then
                Sides(args(index)(0), Dbl(args(index)(1)))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "sound", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 7 Then
                PlaySound(args(index)(0), ToBool(args(index)(1)), Dbl(args(index)(2)), Dbl(args(index)(3)),
                                         Dbl(args(index)(4)), Dbl(args(index)(5)), ToBool(args(index)(6)))

                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "speak", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                If Speak(args(index)(0), False) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

            If args(index).Count = 3 Then
                If Speak(args(index)(0), ToInt(args(index)(1)), ToInt(args(index)(2)), False) = True Then
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "voice", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                If Speech.Voices.Contains(args(index)(0)) = True Then
                    Talk.CurrentVoice() = args(index)(0)
                End If

                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "voicerate", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Talk.CurrentRate() = Dbl(args(index)(0))
                names_list.RemoveAt(index)
            End If

        End If

        index = Utility.IndexOf(names_list, "stopscript", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                If args(index)(0) = "null" Then
                    ScriptStop()
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "stopspeech", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                If args(index)(0) = "null" Then
                    SpeechStop()
                    names_list.RemoveAt(index)
                End If
            End If

        End If

        index = Utility.IndexOf(names_list, "face", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Face() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "back", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Back() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "top", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Top() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "bottom", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Bottom() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "right", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Right() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "left", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Left() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        index = Utility.IndexOf(names_list, "background", True, Gate.eEQUALS)
        If index > -1 Then

            If args(index).Count = 1 Then
                Canvas() = args(index)(0)
                names_list.RemoveAt(index)
            End If
        End If

        If names_list.Count() = 0 Then
            ResetSpeech()
            Return Hold(holds, cmd_hold)
        End If

        Return False
    End Function



    Private Function Home(ByVal speed As Double) As Boolean
        If Rotations(0, 0, 0, speed, 0.1) = True And Positions(0, 0, 0, speed, 0.1) = True Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub Home()
        Position(0, 0, 0)
        Rotation(0, 0, 0)
    End Sub

    Private Sub Side_Swipe(ByVal width As Double, ByVal speed As Double)
        Select Case ScriptLine()
            Case 0
                If Positions(-width, 0, 0, speed, 0.1) = True Then ScriptLine() = 1
            Case 1
                If Positions(width, 0, 0, speed, 0.1) = True Then ScriptLine() = 0
        End Select
    End Sub

    Private Sub Up_Swipe(ByVal hieght As Integer, ByVal speed As Double)
        Select Case ScriptLine()
            Case 0
                If Positions(0, -hieght, 0, speed, 0.1) = True Then ScriptLine() = 1
            Case 1
                If Positions(0, hieght, 0, speed, 0.1) = True Then ScriptLine() = 0
        End Select
    End Sub

    Private Sub Yes_Move(ByVal height As Double, ByVal speed As Double)
        Select Case ScriptLine()
            Case 0
                If Rotations(360 - height, 0, 0, speed, 0.1) = True Then ScriptLine() = 1
            Case 1
                If Rotations(height, 0, 0, speed, 0.1) = True Then ScriptLine() = 0
        End Select
    End Sub

    Private Sub No_Move(ByVal width As Double, ByVal speed As Double)
        Select Case ScriptLine()
            Case 0
                If Rotations(360, 360 - width, 360, speed, 0.1) = True Then ScriptLine() = 1
            Case 1
                If Rotations(0, width, 0, speed, 0.1) = True Then ScriptLine() = 0
        End Select
    End Sub

    Private Property shakeVar As Integer = 0
    Private Sub Shake(ByVal speed As Double, ByVal width As Double)
        If Hold(speed, shakeVar) = True Then
            Dim numb1 = Utility.RandDouble(-width, width)
            Dim numb2 = Utility.RandDouble(-width, width)
            Dim numb3 = Utility.RandDouble(-width, width)
            Rotation(numb1, numb2, numb3)
        End If
    End Sub

    Private Function Sides(ByVal sidetoFace As String, ByVal speed As Double) As Boolean

        Select Case sidetoFace
            Case "face"
                Return Rotations(0, 0, 0, speed, 0.1)
            Case "top"
                Return Rotations(90, 0, 0, speed, 0.1)
            Case "bottom"
                Return Rotations(270, 0, 0, speed, 0.1)
            Case "left"
                Return Rotations(0, 90, 0, speed, 0.1)
            Case "right"
                Return Rotations(0, 270, 0, speed, 0.1)
            Case "back"
                Return Rotations(0, 180, 0, speed, 0.1)
        End Select

        Return True
    End Function


#End Region


#Region "SPEECH"

    Private Property Current_Face() As String = ""

    Private Sub ResetSpeech()
        SpeakOnce() = False
        FinishedSpeaking() = False
    End Sub

    Public Sub Speak(ByVal text As String)
        If IsSpeaking() = False Then Current_Face() = Face()
        Talk.Speak(text)
    End Sub

    Public Sub Speak(ByVal text As String, ByVal voice As Integer, ByVal rate As Integer)
        If IsSpeaking() = False Then Current_Face() = Face()
        Talk.Speak(voice, text, rate)
    End Sub

    Public Function Speak(ByVal text As String, ByVal voice As Integer, ByVal rate As Integer, ByVal resetspeakonce As Boolean) As Boolean
        'for use with scripting

        If IsSpeaking() = False And SpeakOnce() = False Then
            If resetspeakonce = False Then SpeakOnce() = True
            Current_Face() = Face()
            Talk.Speak(voice, text, rate)
        End If

        If FinishedSpeaking() = True And SpeakOnce() = True Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Property FinishedSpeaking() As Boolean = False
    Private Property SpeakOnce() As Boolean = False
    Private Property SpeechPass() As Boolean = True

    Public Function Speak(ByVal text As String, ByVal resetspeakonce As Boolean) As Boolean
        'for use with scripting

        If IsSpeaking() = False And SpeakOnce() = False Then
            If resetspeakonce = False Then SpeakOnce() = True
            Current_Face() = Face()
            Talk.Speak(text)
        End If

        If FinishedSpeaking() = True And SpeakOnce() = True Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function IsSpeaking() As Boolean
        Return Talk.IsSpeaking()
    End Function

    Public Property ReadLargeText() As Boolean
        Get
            Return Talk.ReadLargeText()
        End Get
        Set(value As Boolean)
            Talk.ReadLargeText() = value
        End Set
    End Property

    Public Sub SpeechStop()
        If Talk.IsSpeaking() = True Then
            Talk.StopSpeech()
            Face() = Current_Face()
            SpeakOnce() = False
            SpeechPass() = False
        End If
    End Sub

    Private Sub Visemes(ByVal vis As Object, ByVal dur As Double) Handles Talk.Viseme

        Select Case vis
            Case 0 'silence              
                Face() = DEF_VISEME + "1"
            Case 1 'ae, ax, ah, (hut, at)
                Face() = DEF_VISEME + "2"
            Case 2 'aa, (odd)
                Face() = DEF_VISEME + "3"
            Case 3 'ao, (ought)
                Face() = DEF_VISEME + "4"
            Case 4 'ey, eh, uh (ed, hood, ate)
                Face() = DEF_VISEME + "5"
            Case 5 'er (hurt)
                Face() = DEF_VISEME + "6"
            Case 6 'y, iy, ih, ix (eat, it, yield)
                Face() = DEF_VISEME + "7"
            Case 7 'w, uw (two, we)
                Face() = DEF_VISEME + "8"
            Case 8 'ow (oat)
                Face() = DEF_VISEME + "9"
            Case 9 'aw (cow)
                Face() = DEF_VISEME + "10"
            Case 10 'oy (toy)
                Face() = DEF_VISEME + "11"
            Case 11 'ay (hide)
                Face() = DEF_VISEME + "12"
            Case 12 'h (he)
                Face() = DEF_VISEME + "13"
            Case 13 'r (read)
                Face() = DEF_VISEME + "14"
            Case 14 'l (lee)
                Face() = DEF_VISEME + "15"
            Case 15 's, z (zee, sea)
                Face() = DEF_VISEME + "16"
            Case 16 'sh, ch, jh, zh (cheese, gee, seizure, she)
                Face() = DEF_VISEME + "17"
            Case 17 'th, dh, (thee, theta)
                Face() = DEF_VISEME + "18"
            Case 18 'f, v (vee, fee)
                Face() = DEF_VISEME + "19"
            Case 19 'd, t, n (knee, dee, tea)
                Face() = DEF_VISEME + "20"
            Case 20 'k, g, ng (ping, green, key)
                Face() = DEF_VISEME + "21"
            Case 21 'p, b, m (me, be, pee)
                Face() = DEF_VISEME + "22"
        End Select

    End Sub

    Private Sub Finished() Handles Talk.IsFinished
        Face() = Current_Face()

        If SpeechPass() = True Then
            FinishedSpeaking() = True
        Else
            FinishedSpeaking() = False
            SpeechPass() = True
        End If
    End Sub

    Public Sub PlaySound(ByVal name As String, ByVal isloop As Boolean, ByVal postion As Double, ByVal volume As Double, ByVal balance As Double, ByVal speed As Double, ByVal instance As Boolean)
        Textures.PlaySound(name, isloop, postion, volume, balance, speed, instance)
    End Sub

#End Region


#Region "AVATAR"

    Public Class Cube
        'This is the Cube Avatar
        Public Const DEFAULT_SIZE As Double = 1

        Public Shared Property Model() As New Model3DGroup()
        Public Shared Property Lighting() As New DirectionalLight()
        Public Shared Property IsLoaded() As Boolean = False

        Private Shared ReadOnly Property DefaultImage() As BitmapImage
            Get
                Return Bitmaps.CreateBitmapImage(4, 4, {{Colors.Black, Colors.White}, {Colors.White, Colors.Black}}, 32, 32, True)
            End Get
        End Property

        Public Class Textures
            Public Shared Property Face() As BitmapImage = DefaultImage()
            Public Shared Property Back() As BitmapImage = DefaultImage()
            Public Shared Property Top() As BitmapImage = DefaultImage()
            Public Shared Property Bottom() As BitmapImage = DefaultImage()
            Public Shared Property Right() As BitmapImage = DefaultImage()
            Public Shared Property Left() As BitmapImage = DefaultImage()
        End Class

        Public Class Sides
            Public Shared Property Face() As GeometryModel3D
            Public Shared Property Back() As GeometryModel3D
            Public Shared Property Top() As GeometryModel3D
            Public Shared Property Bottom() As GeometryModel3D
            Public Shared Property Right() As GeometryModel3D
            Public Shared Property Left() As GeometryModel3D
        End Class


        Public Shared Property Origin() As New Point3D(0, 0, 0)
        Public Shared Property CurrentPosition() As Point3D = Origin()


        Private Shared Property TranslationGroup() As New Transform3DGroup()

        Public Shared Sub Rotate(ByVal rotation As Double, ByVal direction As Double())
            Dim Rot_RotateTransform3D As New RotateTransform3D()
            Dim Rot_AxisAngleRotation3d As New AxisAngleRotation3D()
            Rot_AxisAngleRotation3d.Axis = New Vector3D(direction(0), direction(1), direction(2))
            Rot_AxisAngleRotation3d.Angle = rotation
            Rot_RotateTransform3D.Rotation = Rot_AxisAngleRotation3d
            TranslationGroup().Children.Add(Rot_RotateTransform3D)
        End Sub

        Public Shared Sub Position(ByVal direction As Double())
            Dim transform = New TranslateTransform3D(New Vector3D(Origin().X + direction(0), Origin().Y + direction(1), Origin().Z + direction(2)))
            CurrentPosition() = New Point3D(Origin().X + direction(0), Origin().Y + direction(1), Origin().Z + direction(2))
            TranslationGroup().Children.Add(transform)
        End Sub

        Public Shared Sub Reset()
            Model() = New Model3DGroup()
            TranslationGroup() = New Transform3DGroup()
        End Sub

        Public Shared Sub BindTextureToSide()
            BindTexture(Cube.Sides.Face(), Cube.Textures.Face())
            BindTexture(Cube.Sides.Back(), Cube.Textures.Back())
            BindTexture(Cube.Sides.Top(), Cube.Textures.Top())
            BindTexture(Cube.Sides.Bottom(), Cube.Textures.Bottom())
            BindTexture(Cube.Sides.Right(), Cube.Textures.Right())
            BindTexture(Cube.Sides.Left(), Cube.Textures.Left())
        End Sub

        Private Shared Sub BindTexture(ByRef GModel As GeometryModel3D, ByRef texture As BitmapImage)
            Dim Texture_Material As New DiffuseMaterial
            Dim Texture_iBrush As New ImageBrush

            Texture_iBrush.ImageSource = texture
            Texture_Material.Brush = Texture_iBrush
            GModel.Material() = Texture_Material
        End Sub



        Public Shared Sub BindLightingToModel()
            Model.Children.Add(Lighting())
        End Sub

        Public Shared Sub BindSidesToModel()
            Model.Children.Add(Sides.Face())
            Model.Children.Add(Sides.Back())
            Model.Children.Add(Sides.Top())
            Model.Children.Add(Sides.Bottom())
            Model.Children.Add(Sides.Right())
            Model.Children.Add(Sides.Left())
        End Sub

        Public Shared Sub BindTranslationsToModel()
            Model.Transform = TranslationGroup()
        End Sub


    End Class

    Public Class Textures

        'This loads all animations found in the Avatar zip file into a memory structure in order to be easily accessed

        'The index.txt file found the Avatar zip file declares:
        'animation's name,
        'optional profile(only for canvas)
        'wether the animation loops or not,
        'the speed of the frames (a '1' value will set the tick speed to be at its maximum)
        'the speed (count) of the frames before a new subset animation is chosen by random (a '0' value will not allow 
        'switches between different animations, Default is the first animation)
        'the file names contained in any/all folders for each animation set
        'a comma separates the different animation sets

        'face:smiling:300:true:1:16:face.smile,face.sad
        'side:animation_name:doesloop:speed-skips:chance-skips:file_root_name1,file_root_name1,ect.
        '
        'face:smiling:300:true:1:16:face.smile,face.sad
        'side:animation_name:profile_name(*only for canvas):doesloop:speed-skips:chance-skips:file_root_name1,file_root_name1,ect.
        '
        'it doesn't matter what folders are in the textures folder, the program only looks for the file name minus the index (face.blond).1
        'the larger the speed the slower the animation will be
        'when choosing random animations, try to pick a chance-speed that will occure when the animation sequence is finishing
        '(a 16 frame animation with a speed of 10 milisecond skips could have a chance-skip at 160. It will allow 10 cycles before another chance will come up)

        'Script
        'index, name, args, hold, goto
        '#1:home:null:0:2
        '#2:nomove:15,4:300:3
        '#3:home:null:0:4
        '#4:{speak:how are you doing today}
        '   {twirl-5,-5,-5}:0:5
        '#5:yesmove:15,4:300:0


        'Profile
        'null Is no change
        'origin:0,0,0
        'input:0,0,0,0
        'prompt:0,0,0,0
        'menu:0,0,0,0
        'window:null,null,null,null
        'font:Arial
        'color:123,244,11,43

        'NOTES
        'if anime profile changes then internal profile should change, but not until it changes
        'it will send an event change, with new profile data, to the UI

        Public Shared Event IsLoaded()
        Public Shared Property IsLoading() As Boolean = False
        Public Shared Property Loaded() As Boolean = False

        Public Shared Property SetList() As New List(Of Sets)
        Public Shared Property AnimationsList() As New List(Of Animations)
        Public Shared Property SoundsList() As New List(Of Sounds)
        Public Shared Property ProfileList() As New List(Of Profile)
        Public Shared Property ScriptList() As New List(Of Script)

        Public Shared ReadOnly Property DefaultImage() As BitmapImage = Bitmaps.CreateBitmapImage(256, 256, Colors.Transparent)
        Public Shared ReadOnly Property DefaultLoading() As BitmapImage = Bitmaps.CreateBitmapImage(256, 256, Colors.Transparent)
        Public Shared ReadOnly Property InvisibleImage() As BitmapImage = Bitmaps.CreateBitmapImage(256, 256, Colors.Transparent)

        Public Shared Property LocalResources() As New Matrix.TypeMemory


        Public Const NULL As String = "null"
        Public Const TYPE_BYTE As String = "byte"
        Public Const TYPE_BMI As String = "bmi"

        Public Shared ReadOnly Property CurrentCanvasDPI() As Integer
            Get
                Return Schema.Current_Canvas_DPI()
            End Get
        End Property

        Public Shared ReadOnly Property CurrentAvatarDPI() As Integer
            Get
                Return Schema.Current_Avatar_DPI()
            End Get
        End Property

        Public Shared Sub Load(ByVal zipFile As String)
            IsLoading() = True

            Dim tempkey = Utility.RandKey(5)
            Dim fullpath = Utility.Path({Folders.Temp(), tempkey})

            Utility.FileCopy(zipFile, fullpath)

            LocalResources() = Utility.ZipStreamReader(fullpath)

            Clear()

            LoadSets()
            LoadAnimations()
            LoadSounds()
            LoadProfiles()
            LoadScripts()

            RaiseEvent IsLoaded()
            IsLoading() = False
            Loaded() = True
        End Sub

        Private Shared Sub LoadSets()
            'Loading pictures into Sets
            'side.name.index.png
            Dim newSets As New List(Of Sets)
            Dim newType As New TypeMemory

            For Each Item In LocalResources.Items

                If Utility.Search(Item.Index, {Schema.Avatar.FOLDER_TEXTURE, Ext.PNG}, Gate.eAND) = True Then

                    Dim bmi As BitmapImage

                    Try
                        bmi = Bitmaps.GetBitmapImageFromStream(Item.Entry)
                    Catch
                        bmi = DefaultImage()
                    End Try

                    Dim clean_name = Utility.GetFileName(Item.Index, True)
                    Dim name = Utility.ToArray(clean_name, DEF_NAME_DELIM)

                    'rotateImages
                    If Utility.Search(GetIndex(name, 0), {DEF_FACE, DEF_BACK}, Gate.eEQUALS) = True Then
                        Bitmaps.FlipBitmapImage(bmi, System.Drawing.RotateFlipType.RotateNoneFlipY)
                    End If

                    If Utility.Search(GetIndex(name, 0), {DEF_RIGHT, DEF_LEFT}, Gate.eEQUALS) = True Then
                        Bitmaps.FlipBitmapImage(bmi, System.Drawing.RotateFlipType.RotateNoneFlipXY)
                    End If

                    If name.Count() = 3 Then
                        newType.Add(Item.Index, TYPE_BMI, bmi)
                        newSets.Add(New Sets(Item.Index, GetIndex(name, 0), GetIndex(name, 1), Utility.ToInt(GetIndex(name, 2))))
                    End If

                Else
                    newType.Add(Item)
                End If
            Next

            LocalResources() = newType
            SetList() = newSets
        End Sub

        Private Shared Sub LoadAnimations()
            'Loading Sets in Animations using index.txt file

            Dim newAnimations As New List(Of Animations)
            Dim text As String = ""

            'reading index file
            For Each Item In LocalResources.Items
                If Utility.Search(Item.Index, Schema.Avatar.FILE_INDEX, Gate.eSEARCH) = True Then
                    text = Utility.StreamReader(Item.Entry)
                End If
            Next

            Dim iarray = Utility.ToArray(text, DEF_ANIMATE_DELIM)

            For Each ln In iarray

                Dim properties = Utility.ToArray(ln, DEF_PROPERTY_DELIM)

                If properties.Count() = 6 Then

                    Dim Side = GetIndex(properties, 0)
                    Dim Name = GetIndex(properties, 1)
                    Dim Profile = "null"
                    Dim Height = 256 'picture height resolution in pixels of all the pictures in that animation 
                    Dim DoesLoop = GetIndex(properties, 2)
                    Dim Speed = GetIndex(properties, 3)
                    Dim ChanceSpeed = GetIndex(properties, 4)
                    Dim file = GetIndex(properties, 5)
                    Dim files = Utility.ToArray(file, DEF_RANDOM_DELIM)

                    If Utility.Search(Side, DEF_CANVAS, Gate.eEQUALS) = True Then
                        Height = CurrentCanvasDPI()
                    Else
                        Height = CurrentAvatarDPI()
                    End If

                    If Utility.IsEmpty(Side) = False And Utility.IsEmpty(Name) = False And Utility.IsEmpty(Profile) = False And Utility.IsInt(Speed) = True And
                       Utility.IsBool(DoesLoop) = True And Utility.IsInt(ChanceSpeed) = True And Utility.IsEmpty(files) = False And Utility.IsInt(Height) = True Then

                        newAnimations.Add(New Animations(Side, Name, Profile, Utility.ToInt(Speed), Utility.ToBool(DoesLoop), Utility.ToInt(ChanceSpeed), GetSets(files, Utility.ToInt(Height))))
                    End If

                ElseIf properties.Count() = 7 Then

                    Dim Side = GetIndex(properties, 0)
                    Dim Name = GetIndex(properties, 1)
                    Dim Profile = GetIndex(properties, 2)
                    Dim Height = 256 'picture height resolution in pixels of all the pictures in that animation 
                    Dim DoesLoop = GetIndex(properties, 3)
                    Dim Speed = GetIndex(properties, 4)
                    Dim ChanceSpeed = GetIndex(properties, 5)
                    Dim file = GetIndex(properties, 6)
                    Dim files = Utility.ToArray(file, DEF_RANDOM_DELIM)

                    If Utility.Search(Side, DEF_CANVAS, Gate.eEQUALS) = True Then
                        Height = CurrentCanvasDPI()
                    Else
                        Height = CurrentAvatarDPI()
                    End If

                    If Utility.IsEmpty(Side) = False And Utility.IsEmpty(Name) = False And Utility.IsEmpty(Profile) = False And Utility.IsInt(Speed) = True And
                       Utility.IsBool(DoesLoop) = True And Utility.IsInt(ChanceSpeed) = True And Utility.IsEmpty(files) = False And Utility.IsInt(Height) = True Then

                        newAnimations.Add(New Animations(Side, Name, Profile, Utility.ToInt(Speed), Utility.ToBool(DoesLoop), Utility.ToInt(ChanceSpeed), GetSets(files, Utility.ToInt(Height))))
                    End If

                End If
            Next

            AnimationsList() = newAnimations
        End Sub

        Private Shared Function GetSets(ByVal files As String(), ByVal height As Integer) As List(Of List(Of Sets))
            Dim newSetList As New List(Of List(Of Sets))

            For Each file In files
                If Utility.IsEmpty(file) = False Then
                    newSetList.Add(GetSet(file, height))
                End If
            Next

            Return newSetList
        End Function

        Private Shared Property IsCanvasSizeFilled() As Boolean = False
        Private Shared Function GetSet(ByVal file As String, ByVal height As Integer) As List(Of Sets)
            Dim newSetList As New List(Of Sets)
            Dim file_array = Utility.ToArray(file, DEF_NAME_DELIM)
            Dim side = GetIndex(file_array, 0)
            Dim name = GetIndex(file_array, 1)

            If file_array.Count = 2 Then

                'Getting all pictures from certain folders addresses 
                For Each set1 In SetList()

                    If Utility.Search(side, set1.Side(), Gate.eEQUALS) = True And
                        Utility.Search(name, set1.Name(), Gate.eEQUALS) = True Then

                        Dim img = set1.Image()

                        If img IsNot Nothing Then
                            Dim bmi = Bitmaps.CreateResizedImage(img, 0, height, 0)
                            set1.Stream() = Bitmaps.GetBytesFromBitmapImage(bmi)

                            'used to find canvas image size
                            'used to set ui dimensions if none is set in profiles
                            If IsCanvasSizeFilled() = False Then
                                If Utility.Search(set1.Side(), DEF_CANVAS, Gate.eEQUALS) = True Then
                                    CanvasSize() = New Size(bmi.Width, bmi.Height)
                                    IsCanvasSizeFilled() = True
                                End If
                            End If

                        End If

                        newSetList.Add(set1)
                    End If
                Next

            End If
            Return newSetList
        End Function

        Public Shared Sub Clear()
            SetList().Clear()
            AnimationsList().Clear()
            SoundsList.Clear()
            ProfileList().Clear()
            ScriptList.Clear()
            Loaded() = False
        End Sub


        Public Shared Function GetAnimations(ByVal name As String, ByVal side As String) As Animations

            For Each anime In AnimationsList()
                If Utility.Search(anime.Name(), name, Gate.eEQUALS) = True Then
                    If Utility.Search(anime.Side(), side, Gate.eEQUALS) = True Then
                        Return anime
                    End If
                End If
            Next

            Return New Animations
        End Function

        Public Shared Function GetImages(ByVal name As String, ByVal side As String) As List(Of List(Of Sets))

            For Each anime In AnimationsList()
                If Utility.Search(anime.Name(), name, Gate.eEQUALS) = True Then
                    If Utility.Search(anime.Side(), side, Gate.eEQUALS) = True Then
                        Return anime.IndexSets()
                    End If
                End If
            Next

            Return New List(Of List(Of Sets))
        End Function

        'Sounds
        Private Shared Sub LoadSounds()
            Dim newType As New Matrix.TypeMemory

            For Each Item In LocalResources.Items
                If Utility.Search(Item.Index, {Schema.Avatar.FOLDER_SOUNDS, Ext.WAV}, Gate.eAND) = True Then
                    Dim name = Utility.GetFileName(Item.Index, True)
                    SoundsList.Add(New Sounds(name, Item.Entry))
                Else
                    newType.Add(Item)
                End If
            Next

            LocalResources() = newType
        End Sub

        Public Shared Sub PlaySound(ByVal name As String, ByVal isloop As Boolean, ByVal postion As Double, ByVal volume As Double, ByVal balance As Double, ByVal speed As Double, ByVal instance As Boolean)
            For Each Item In SoundsList()
                If Utility.Search(Item.Address(), name, Gate.eEQUALS) = True Then
                    Audio.Play(name, TryCast(Item.Sound(), IO.MemoryStream), isloop, postion, volume, balance, speed, instance)
                    Exit Sub
                End If
            Next
        End Sub


        'Profiles
        Private Shared Sub LoadProfiles()
            Dim newType As New Matrix.TypeMemory

            For Each Item In LocalResources.Items

                If Utility.Search(Item.Index, {Schema.Avatar.FOLDER_PROFILES, Ext.PRO}, Gate.eAND) = True Then
                    Dim newPro As New Profile
                    newPro.Name() = Utility.GetFileName(Item.Index, True)

                    Dim text = Utility.StreamReader(Item.Entry)
                    Dim iarray = Utility.ToArray(text, DEF_ANIMATE_DELIM)


                    For Each line In iarray
                        Dim props = Utility.ToArray(line, DEF_PROPERTY_DELIM)
                        props = Utility.Trim(props)

                        If props.Count() = 2 Then
                            Dim CMD = Trim(props(0))
                            Dim ARG = Trim(props(1))
                            Dim args = Utility.ToArray(ARG, DEF_ARGS_DELIM)
                            args = Utility.Trim(args)

                            If Utility.Search(CMD, Profile.DEF_ORIGIN, Gate.eEQUALS) = True Then
                                If args.Count() = 3 Then
                                    newPro.Origin = New Point3D(ToDbl(args(0)), ToDbl(args(1)), ToDbl(args(2)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_INPUT, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Input_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Input_Size() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_PROMPT, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Prompt_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Prompt_Size() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_STATUS, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Status_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Status_Size() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_MENU, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Menu_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Menu_Size() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GRIP, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Grip_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Grip_Size() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_WINDOW, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Window_Location() = New Point(ToDbl(args(0)), ToDbl(args(1)))
                                    newPro.Window_ActualSize() = New Size(ToDbl(args(2)), ToDbl(args(3)))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_FONTFAMILY, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.Global_FontFamily() = args(0)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_FONTCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Global_FontColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_WINDOWCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Window_Color() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_AICOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Prompt_AiColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_USERCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Prompt_UserColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_HOVERCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Global_HoverColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_FONTBACKCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Global_FontBackColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_BORDERCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Global_BorderColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_STATUSCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Status_FontColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_ERRORCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Prompt_ErrorColor() = ToColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_GLOBAL_FONTSIZE, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.Global_FontSize() = ToDbl(args(0))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SHOWAVATAR, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.ShowAvatar() = ToBool(args(0))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SHOWCANVAS, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.ShowCanvas() = ToBool(args(0))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SUSPENDSCRIPT, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.SuspendScript() = ToBool(args(0))
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SUSPEND, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.Suspend() = ToBool(args(0))
                                End If
                            End If


                            'script editor
                            If Utility.Search(CMD, Profile.DEF_SCRIPT_ATTRIBUTECOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_AttributeColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_BACKCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_BackColor() = ToDrawingColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_FONTCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_FontColor() = ToDrawingColor(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_CLASSCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_ClassColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_COMMENTCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_CommentColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_KEYWORDCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_KeyWordColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_NUMBERCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_NumberColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_STRINGCOLOR, Gate.eEQUALS) = True Then
                                If args.Count() = 4 Then
                                    newPro.Script_StringColor() = ToDrawingBrush(args)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_FONT, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.Script_Font() = args(0)
                                End If
                            End If

                            If Utility.Search(CMD, Profile.DEF_SCRIPT_FONTSIZE, Gate.eEQUALS) = True Then
                                If args.Count() = 1 Then
                                    newPro.Script_FontSize() = ToSingle(args(0))
                                End If
                            End If
                        End If
                    Next

                    ProfileList.Add(newPro)

                Else
                    newType.Add(Item)
                End If


            Next

            LocalResources() = newType
        End Sub

        Public Shared Function GetProfile(ByVal name As String) As Profile

            For Each pro In ProfileList()
                If Utility.Search(pro.Name(), name, Gate.eEQUALS) = True Then
                    Return pro
                End If
            Next

            Return New Profile
        End Function


        'Scripts
        Public Shared Sub LoadScripts()
            Dim newType As New Matrix.TypeMemory

            For Each Item In LocalResources.Items

                If Utility.Search(Item.Index, {Schema.Avatar.FOLDER_SCRIPTS, Ext.CUB}, Gate.eAND) = True Then
                    Dim script_name = Utility.GetFileName(Item.Index(), True)
                    Dim text = Utility.StreamReader(Item.Entry)

                    Dim newScript = LoadScript(Utility.ToList(text, DEF_LINE_DELIM))
                    newScript.Name() = script_name
                    ScriptList.Add(newScript)
                Else
                    newType.Add(Item)
                End If
            Next

            LocalResources() = newType
        End Sub

        Public Shared Function LoadScript(ByVal code As List(Of String)) As Script
            Dim newScript As New Script

            For Each line In code

                Dim com_list As New List(Of String)
                Dim arg_list As New List(Of List(Of String))
                Dim m_line = Utility.ToArray(line, DEF_PROPERTY_DELIM)

                If Utility.Search(line, {"{", "}"}, Gate.eOR) = True And m_line.Count() > 5 Then
                    Dim get_args = Utility.GetArgArray(line, "{", "}", Gate.eEQUALS)

                    line = Utility.Remove(line, get_args)
                    Dim new_line = Utility.ToArray(line, DEF_PROPERTY_DELIM)


                    Dim index = ToInt(new_line(0))
                    Dim hold = ToDbl(new_line(2))
                    Dim gotoline = ToInt(new_line(3))

                    For Each arg In get_args
                        Dim arg_array = Utility.ToArray(arg, DEF_PROPERTY_DELIM)
                        com_list.Add(arg_array(0))

                        Dim arguments = Utility.ToList(arg_array(1), DEF_ARGS_DELIM)
                        arg_list.Add(arguments)
                    Next

                    newScript.Add(index, com_list, arg_list, hold, gotoline)

                Else
                    'This is for single code lines

                    If m_line.Count() = 5 Then

                        Dim index = Utility.ToInt(m_line(0), 0)
                        Dim command = Utility.Trim(m_line(1))
                        Dim arguments = Utility.ToList(m_line(2), DEF_ARGS_DELIM)
                        Dim hold = Utility.ToDbl(m_line(3), 0)
                        Dim gotoline = Utility.ToInt(m_line(4), 0)

                        com_list.Add(command)
                        arg_list.Add(arguments)

                        newScript.Add(index, com_list, arg_list, hold, gotoline)

                    End If

                End If

            Next

            Return newScript
        End Function

        Public Shared Function GetScript(ByVal name As String) As Script
            For Each scrpt In ScriptList()
                If Utility.Search(scrpt.Name, name, Gate.eEQUALS) = True Then
                    Return scrpt
                End If
            Next
            Return New Script
        End Function


        'Utils
        Private Shared Function ToDbl(ByVal toDbls As String) As Double
            Return Utility.ToDbl(toDbls, 0)
        End Function

        Private Shared Function ToSingle(ByVal toSings As String) As Double
            Return Utility.ToSingle(toSings, 0)
        End Function

        Private Shared Function ToByte(ByVal toDbls As String) As Double
            Return Utility.ToByte(toDbls, 0)
        End Function

        Private Shared Function ToInt(ByVal toInts As String) As Integer
            Return Utility.ToInt(toInts, 0)
        End Function

        Private Shared Function ToBool(ByVal toBools As String) As Boolean
            Return Utility.ToBool(toBools)
        End Function

        Private Shared Function ToColor(ByVal args As String()) As Color
            Return Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3)))
        End Function
        Private Shared Function ToDrawingColor(ByVal args As String()) As System.Drawing.Color
            Return System.Drawing.Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3)))
        End Function

        Private Shared Function ToBrush(ByVal args As String()) As Brush
            Return New SolidColorBrush(Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3))))
        End Function
        Private Shared Function ToDrawingBrush(ByVal args As String()) As System.Drawing.Brush
            Return New System.Drawing.SolidBrush(System.Drawing.Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3))))
        End Function


        Private Shared Function Trim(ByVal strg As String) As String
            Return Utility.Trim(strg)
        End Function
        Private Shared Function GetIndex(ByVal array As String(), ByVal index As Integer) As String
            Return Utility.Trim(Utility.GetIndex(array, index, ""))
        End Function

        Public Class Animations

            Public Sub New()
            End Sub

            Public Sub New(ByVal side As String, ByVal name As String, ByVal profile As String, ByVal speed As Integer, ByVal doesloop As Boolean, ByVal chanceSpeed As Integer, ByVal indexes As List(Of List(Of Sets)))
                Me.Side() = side
                Me.Name() = name
                Me.Profile() = profile
                Me.Speed() = speed
                Me.DoesLoop() = doesloop
                Me.ChanceSpeed() = chanceSpeed
                Me.IndexSets = indexes
            End Sub


            Public Property Side() As String = ""
            Public Property Name() As String = ""
            Public Property Profile() As String = ""
            Public Property DoesLoop() As Boolean = False
            Public Property Speed() As Integer = 0

            Public ReadOnly Property Count() As Integer
                Get
                    If Utility.InRange(CurrentSet(), 0, IndexSets().Count() - 1, True) = True Then
                        Return IndexSets()(CurrentSet()).Count()
                    Else
                        Return 0
                    End If
                End Get
            End Property

            Public Property ChanceSpeed() As Integer = 0

            Private m_CurrentSet As Integer = 0
            Public Property CurrentSet() As Integer
                Get
                    Return m_CurrentSet
                End Get
                Set(value As Integer)
                    If value >= IndexSets.Count() Then value = IndexSets.Count() - 1
                    If value < 0 Then value = 0

                    m_CurrentSet = value
                End Set
            End Property

            Public ReadOnly Property SetCount() As Integer
                Get
                    Return IndexSets().Count()
                End Get
            End Property

            Public Property IndexSets() As New List(Of List(Of Sets))


            Public Function GetStream(ByVal index As Integer) As Byte()

                If Utility.InRange(CurrentSet(), 0, IndexSets().Count() - 1, True) = True Then
                    For Each set1 In IndexSets()(CurrentSet())
                        If set1.Index() = index Then
                            Return set1.Stream()
                        End If
                    Next
                End If

                Return Bitmaps.GetBytesFromBitmapImage(DefaultImage())
            End Function

            Public Function GetOriginalImage(ByVal index As Integer) As BitmapImage

                If Utility.InRange(CurrentSet(), 0, IndexSets().Count() - 1, True) = True Then
                    For Each set1 In IndexSets()(CurrentSet())
                        If set1.Index() = index Then
                            Return set1.ImageFromStream
                        End If
                    Next
                End If

                Return DefaultImage()
            End Function

            Public Function GetImageFromStream(ByVal index As Integer) As BitmapImage

                If Utility.InRange(CurrentSet(), 0, IndexSets().Count() - 1, True) = True Then
                    For Each set1 In IndexSets()(CurrentSet())
                        If set1.Index() = index Then
                            Return set1.ImageFromStream()
                        End If
                    Next
                End If

                Return DefaultImage()
            End Function


        End Class

        Public Class Sets

            Public Sub New()
            End Sub
            Public Sub New(ByVal address As String, ByVal side As String, ByVal name As String, ByVal index As Integer)
                Me.Index() = index
                Me.Name() = name
                Me.Side() = side
                Me.Address() = address
            End Sub

            Public Property Address() As String = ""
            Public Property Name() As String = ""
            Public Property Side() As String = ""
            Public Property Index() As Integer = 0
            Public ReadOnly Property Image() As BitmapImage
                Get
                    Return TryCast(LocalResources().Index(Address(), TYPE_BMI, Nothing), BitmapImage)
                End Get
            End Property

            Public Property Stream() As Byte()
                Get
                    Return TryCast(LocalResources().Index(Address(), TYPE_BYTE, Nothing), Byte())
                End Get
                Set(value As Byte())
                    LocalResources().Index(Address(), TYPE_BMI, Nothing) = value
                    LocalResources().Tag(Address()) = TYPE_BYTE
                End Set
            End Property

            Public ReadOnly Property ImageFromStream() As BitmapImage
                Get
                    Return Bitmaps.GetBitmapImageFromBytes(Stream())
                End Get
            End Property

        End Class

        Public Class Sounds
            Sub New()
            End Sub
            Sub New(ByVal address As String, ByVal sound As Object)
                Me.Address() = address
                Me.Sound() = sound
            End Sub
            Public Property Address() As String
            Public Property Sound() As Object
        End Class

        Public Class Profile
            Public Const DEF_ORIGIN As String = "origin"
            Public Const DEF_SHOWAVATAR As String = "showavatar"
            Public Const DEF_SHOWCANVAS As String = "showcanvas"
            Public Const DEF_SUSPENDSCRIPT As String = "suspendscript"
            Public Const DEF_SUSPEND As String = "suspend"

            Public Const DEF_INPUT As String = "input"
            Public Const DEF_PROMPT As String = "prompt"
            Public Const DEF_STATUS As String = "status"
            Public Const DEF_MENU As String = "menu"
            Public Const DEF_GRIP As String = "grip"
            Public Const DEF_WINDOW As String = "window"

            Public Const DEF_GLOBAL_FONTFAMILY As String = "font"
            Public Const DEF_GLOBAL_FONTSIZE As String = "fontsize"
            Public Const DEF_GLOBAL_BORDERCOLOR As String = "bordercolor"
            Public Const DEF_GLOBAL_FONTCOLOR As String = "fontcolor"
            Public Const DEF_GLOBAL_HOVERCOLOR As String = "hovercolor"
            Public Const DEF_GLOBAL_FONTBACKCOLOR As String = "fontbackcolor"
            Public Const DEF_WINDOWCOLOR As String = "windowcolor"
            Public Const DEF_AICOLOR As String = "aicolor"
            Public Const DEF_USERCOLOR As String = "usercolor"
            Public Const DEF_STATUSCOLOR As String = "statuscolor"
            Public Const DEF_ERRORCOLOR As String = "errorcolor"

            Public Const DEF_SCRIPT_FONT As String = "scriptfont"
            Public Const DEF_SCRIPT_FONTSIZE As String = "scriptfontsize"
            Public Const DEF_SCRIPT_FONTCOLOR As String = "scriptfontcolor"
            Public Const DEF_SCRIPT_BACKCOLOR As String = "scriptbackcolor"
            Public Const DEF_SCRIPT_NUMBERCOLOR As String = "scriptnumbercolor"
            Public Const DEF_SCRIPT_STRINGCOLOR As String = "scriptstringcolor"
            Public Const DEF_SCRIPT_COMMENTCOLOR As String = "scriptcommentcolor"
            Public Const DEF_SCRIPT_KEYWORDCOLOR As String = "scriptkeywordcolor"
            Public Const DEF_SCRIPT_ATTRIBUTECOLOR As String = "scriptattributecolor"
            Public Const DEF_SCRIPT_CLASSCOLOR As String = "scriptclasscolor"

            Public Property Name() As String = ""
            Public Property Origin() As New Point3D(0, 0, 0)
            Public Property ShowAvatar() As Boolean = True
            Public Property ShowCanvas() As Boolean = True
            Public Property SuspendScript() As Boolean = False
            Public Property Suspend() As Boolean = False

            Public Property Input_Location() As New Point(-1, -1)
            Public Property Input_Size() As New Size(0, 0)

            Public Property Prompt_Location() As New Point(-1, -1)
            Public Property Prompt_Size() As New Size(0, 0)

            Public Property Status_Location() As New Point(-1, -1)
            Public Property Status_Size() As New Size(0, 0)

            Public Property Menu_Location() As New Point(-1, -1)
            Public Property Menu_Size() As New Size(0, 0)

            Public Property Grip_Location() As New Point(-1, -1)
            Public Property Grip_Size() As New Size(0, 0)

            Public Property Window_Location() As New Point(-1, -1)
            Public Property Window_ActualSize() As New Size(0, 0)

            Public Property Global_FontFamily() As String = ""
            Public Property Global_FontSize() As Double = 0

            Public Property Global_BorderColor() As Color = Colors.White()
            Public Property Global_FontColor() As Color = Colors.White()
            Public Property Global_HoverColor() As Color = Colors.Red()
            Public Property Global_FontBackColor() As Color = Colors.Pink()
            Public Property Window_Color() As Color = Colors.Black()
            Public Property Prompt_AiColor() As Color = Colors.Blue()
            Public Property Prompt_UserColor() As Color = Colors.White()
            Public Property Prompt_ErrorColor() As Color = Colors.Red()

            Public Property Status_FontColor() As Color = Colors.Green()

            'scripts
            Public Property Script_Font() As String = "Consolas"
            Public Property Script_FontSize() As Single = 12

            Public Property Script_FontColor() As System.Drawing.Color = System.Drawing.Color.Black
            Public Property Script_BackColor() As System.Drawing.Color = System.Drawing.Color.White

            Public Property Script_NumberColor() As System.Drawing.Brush = System.Drawing.Brushes.Magenta
            Public Property Script_StringColor() As System.Drawing.Brush = System.Drawing.Brushes.Brown
            Public Property Script_CommentColor() As System.Drawing.Brush = System.Drawing.Brushes.Green
            Public Property Script_KeyWordColor() As System.Drawing.Brush = System.Drawing.Brushes.Blue
            Public Property Script_AttributeColor() As System.Drawing.Brush = System.Drawing.Brushes.Gray
            Public Property Script_ClassColor() As System.Drawing.Brush = System.Drawing.Brushes.Purple


        End Class

        Public Class Script
            Public Property Name() As String = ""
            Public Property Commands() As New List(Of Command)

            Public Sub New()
            End Sub

            Public Sub New(ByVal name As String, ByVal commands As List(Of Command))
                Me.Name = name
                Me.Commands = commands
            End Sub

            Public Sub Add(ByVal index As Integer, ByVal name As List(Of String), ByVal args As List(Of List(Of String)), ByVal hold As Double, ByVal gotoIndex As Integer)
                If DoesIndexExist(index) = False Then
                    Commands().Add(New Command(index, name, args, hold, gotoIndex))
                End If
            End Sub

            Public Function GetCommand(ByVal index As Integer) As Command
                For Each com In Commands
                    If com.Index = index Then
                        Return com
                    End If
                Next
                Return New Command
            End Function

            Public Function DoesIndexExist(ByVal index As Integer) As Boolean
                For Each com In Commands
                    If com.Index = index Then Return True
                Next
                Return False
            End Function

        End Class

        Public Class Command
            Public Sub New()
            End Sub
            Public Sub New(ByVal index As Integer, ByVal name As List(Of String), ByVal args As List(Of List(Of String)), ByVal hold As Double, ByVal gotoIndex As Integer)
                Me.Index = index
                Me.Name = name
                Me.Arguments = args
                Me.Hold = hold
                Me.GoToIndex = gotoIndex
            End Sub

            Public Property Index() As Integer = 0
            Public Property Name() As List(Of String)
            Public Property Arguments() As New List(Of List(Of String))
            Public Property Hold() As Double = 0
            Public Property GoToIndex() As Integer = 0
        End Class


    End Class

#End Region


End Class

