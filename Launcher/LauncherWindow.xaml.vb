Imports Matrix
Imports MatrixUtil

Class LauncherWindow

    WithEvents RunTimer As New TimeDispatcher(1000)

    Private Sub Initialize() Handles Me.Initialized

        Dim center = Local.FindCenter(New Point(0, 0), MouseInfo.ScreenSize(), New Size(Width, Height))
        LaunchWindow.Left = center.X
        LaunchWindow.Top = center.Y

        LaunchWindow.MaxHeight = LaunchWindow.Height
        LaunchWindow.MinHeight = LaunchWindow.Height
        LaunchWindow.MaxWidth = LaunchWindow.Width
        LaunchWindow.MinWidth = LaunchWindow.Width

        Button_Launch.TextBuffer(0, 0, 0, 0)
        Button_Launch.Buffer(3, 3, 3, 3)
        Button_Options.TextBuffer(0, 0, 0, 0)
        Button_Options.Buffer(3, 3, 3, 3)

        Schema.Global_Border_Color() = Brushes.Blue
        Schema.Global_Border_Size() = 3
        Schema.Global_Font() = LocalResources.GetFont()
        Schema.Global_FontColor() = Brushes.Black
        Schema.Global_FontBackColor() = Brushes.LightYellow
        Schema.Global_FontSize() = 13
        Schema.Global_HoverColor() = Brushes.White
        Schema.Global_Window_Color() = Brushes.LightBlue

        Schema.SetStyle(Main_Text)

        Main_Text.Text = LocalResources.GetLauncherText()

    End Sub

    Private Sub Load() Handles Me.Loaded

        If Utility.IsProgramRunning(Schema.DEF_UIPROGRAM_NAME) = False Then
            Launch()
            IsRunning() = False
        Else
            ExitLaunch()
            IsRunning() = True
        End If

        RunTimer.Start()
    End Sub

    Private Sub Exiting() Handles Me.Closed
        OptionEditor_Window.Close()
    End Sub


#Region "Options"
    Private OptionEditor_Window As New OptionEditor

    Private Sub Button_Options_Event() Handles Button_Options.LeftClick

        OptionEditor_Window.Close()
        OptionEditor_Window = New OptionEditor

        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), New Size(OptionEditor_Window.Width, OptionEditor_Window.Height))

        OptionEditor_Window.Top() = center.Y
        OptionEditor_Window.Left() = center.X
        OptionEditor_Window.Show()

    End Sub




#End Region




#Region "Launch"

    Private Property IsRunning() As Boolean = False
    Private Property TriggeredOn() As Boolean = False
    Private Property TriggeredOff() As Boolean = False

    Private Sub RunDetect_Timer() Handles RunTimer.Tick
        If Utility.IsProgramRunning(Schema.DEF_UIPROGRAM_NAME) = False Then

            If TriggeredOn() = False Then
                Launch()
                TriggeredOn() = True
                TriggeredOff() = False
            End If

            IsRunning() = False
        Else

            If TriggeredOff() = False Then
                ExitLaunch()
                TriggeredOff() = True
                TriggeredOn() = False
            End If

            IsRunning() = True
        End If
    End Sub


    Private Sub Button_Launch_Event() Handles Button_Launch.LeftClick

        If Launch_Thread.IsBusy = False And Exit_Thread.IsBusy = False Then
            If IsRunning() = False Then
                Launch_Thread.RunWorkerAsync()
            Else
                Exit_Thread.RunWorkerAsync()
            End If
        End If

    End Sub


    WithEvents Launch_Thread As New ComponentModel.BackgroundWorker
    Private Sub Launch_DoWork() Handles Launch_Thread.DoWork
        Utility.OpenProgram(Folders.Directory() & Schema.DEF_UIPROGRAM_NAME)
    End Sub
    Private Sub Launch_Completed() Handles Launch_Thread.RunWorkerCompleted
        ExitLaunch()
    End Sub


    WithEvents Exit_Thread As New ComponentModel.BackgroundWorker
    Private Sub Exit_DoWork() Handles Exit_Thread.DoWork
        Utility.ExitProgram(Schema.DEF_UIPROGRAM_NAME)
        Utility.ExitProgram(Schema.DEF_ATHENA_NAME)
    End Sub
    Private Sub Exit_Completed() Handles Exit_Thread.RunWorkerCompleted
        Launch()
    End Sub


    Private Sub Launch()
        Button_Launch.Text() = "Launch"
        Main_Grid.Background = Brushes.Black
        Button_Launch.Background = Brushes.Green
        Button_Launch.Border(Brushes.White, New Thickness(10))
        Button_Launch.NormalFont() = New Fonts(Schema.Global_Font, 42, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)
        Button_Launch.HoverFont() = New Fonts(Schema.Global_Font, 52, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)

        Button_Options.Text() = "Options"
        Button_Options.Background = Brushes.LightBlue
        Button_Options.Border(Brushes.White, New Thickness(10))
        Button_Options.NormalFont() = New Fonts(Schema.Global_Font, 42, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)
        Button_Options.HoverFont() = New Fonts(Schema.Global_Font, 52, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)
    End Sub

    Private Sub ExitLaunch()
        Button_Launch.Text() = "STOP!"
        Button_Launch.Background = Brushes.Red
        Button_Launch.Border(Brushes.White, New Thickness(10))
        Button_Launch.NormalFont() = New Fonts(Schema.Global_Font, 42, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.White)
        Button_Launch.HoverFont() = New Fonts(Schema.Global_Font, 52, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.White)

        Button_Options.Text() = "Options"
        Button_Options.Background = Brushes.LightBlue
        Button_Options.Border(Brushes.White, New Thickness(10))
        Button_Options.NormalFont() = New Fonts(Schema.Global_Font, 42, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)
        Button_Options.HoverFont() = New Fonts(Schema.Global_Font, 52, FontStyles.Normal, FontWeights.Bold, FontStretches.Normal, Brushes.Black)
    End Sub

#End Region

End Class
