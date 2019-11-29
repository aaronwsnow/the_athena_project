Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

Class Home_Page


    WithEvents Scale_Menu As New ManageGenerikButtons()
    WithEvents Exit_Menu As New ManageGenerikButtons()



#Region "Loading"

    Private MainSplash As New LoadSplash

    Private Sub Initialize() Handles Home_Page_Window.Initialized

        Dim center = Local.FindCenter(New Point(0, 0), MouseInfo.ScreenSize(), New Size(Home_Page_Window.Width, Home_Page_Window.Height))
        Home_Page_Window.Left = center.X
        Home_Page_Window.Top = center.Y

        Main_Grid.Visibility = Visibility.Hidden
        MainSplash.Show()

        Exit_Menu.Buffer() = New Thickness(2)
        Exit_Menu.Spacing() = New Thickness(3, 3, 3, 3)
        Exit_Menu.ButtonSize() = New Size(0, 0)
        Exit_Menu.ButtonInfo() = {"min|[_]", "max|[O]", "exit|[X]"}
        Exit_Menu.Load()

        Exit_Menu.HorizontalAlignment = HorizontalAlignment.Right
        Exit_Menu.VerticalAlignment = VerticalAlignment.Top
        Main_Grid.Children.Add(Exit_Menu.Source)


        Scale_Menu.Buffer() = New Thickness(2)
        Scale_Menu.Spacing() = New Thickness(3, 3, 3, 3)
        Scale_Menu.ButtonSize() = New Size(0, 0)
        Scale_Menu.ButtonInfo() = {"scale|[S]"}
        Scale_Menu.Load()

        Scale_Menu.HorizontalAlignment = HorizontalAlignment.Right
        Scale_Menu.VerticalAlignment = VerticalAlignment.Bottom
        Main_Grid.Children.Add(Scale_Menu.Source)
    End Sub

    Private Sub Load() Handles Home_Page_Window.Loaded
        MouseInfo.Load()
        Folders.MakeDirectories()
        Folders.CleanTemp()

        Store_Postition()
        Store_Size()

        CodeManage.Close()
        Schema.Load()

        AvatarX.CurrentScript() = Schema.Current_Script()
        AvatarX.Load(Schema.Current_Avatar())

    End Sub

    Private Sub IsClosing() Handles Home_Page_Window.Closed
        CodeManage.Close()
    End Sub


    Private Sub Has_Loaded() Handles AvatarX.HasLoaded
        PauseDelay.Start(0)
    End Sub

    WithEvents PauseDelay As New DelayDispatcher()
    WithEvents DisplayDelay As New DelayDispatcher()

    Private Sub PauseDelay_Event() Handles PauseDelay.Elapsed
        AvatarX.Suspend() = True

        Refresh_Exit_Panel()
        Refresh_Globals()

        DisplayDelay.Start(100)
    End Sub
    Private Sub DisplayDelay_Event() Handles DisplayDelay.Elapsed

        Main_Grid.Visibility = Visibility.Visible
        MainSplash.Close()
        AvatarX.Suspend() = False

        WriteMemory.Location() = Files.Output()
        WriteMemory.Reset()

        ReadMemory.Location() = Files.Input()
        ReadMemory.Reset()
        Read_Timer.Start()
    End Sub


#End Region


#Region "Profile"

    Private Sub Refresh_Exit_Panel()

        Exit_Menu.Background = Schema.Global_Window_Color
        Exit_Menu.HoveredBackground = Schema.Global_Window_Color
        Exit_Menu.HoverFont = Schema.Hover_Fonts()
        Exit_Menu.NormalFont = Schema.Normal_Fonts()
        Exit_Menu.Border(Schema.Global_FontColor, New Thickness(1))
        Exit_Menu.ButtonBorder(Brushes.Transparent, New Thickness(0))
        Exit_Menu.Load()

        Scale_Menu.Background = Schema.Global_Window_Color
        Scale_Menu.HoveredBackground = Schema.Global_Window_Color
        Scale_Menu.HoverFont = Schema.Hover_Fonts()
        Scale_Menu.NormalFont = Schema.Normal_Fonts()
        Scale_Menu.Border(Schema.Global_FontColor, New Thickness(1))
        Scale_Menu.ButtonBorder(Brushes.Transparent, New Thickness(0))
        Scale_Menu.Load()

    End Sub

    Private Sub Refresh_Globals()

        PromptX.BorderBrush() = Brushes.Transparent()
        StatusX.BorderBrush() = Brushes.Transparent()
        InputX.BorderBrush() = Brushes.Transparent()
        MenuX.BorderBrush() = Brushes.Transparent()
        GripX.BorderBrush() = Brushes.Transparent()

        InputX.Fonts() = Schema.Normal_Fonts()

        MenuX.NormalFont() = Schema.Normal_Fonts()
        MenuX.HoverFont() = Schema.Hover_Fonts()

    End Sub

    Private Property InitialStatusMessage() As Boolean = True
    Private Property CurrentProfile() As New AvatarX.Textures.Profile
    Private Sub Profile(ByVal e As AvatarX.Textures.Profile) Handles AvatarX.ProfileChange
        CurrentProfile() = e

        Visible() = False
        AvatarX.Suspend() = True

        'Size and Locations
        If e.Window_Location.X >= 0 And e.Window_Location.Y >= 0 Then
            Home_Page_Window.Left = e.Window_Location.X
            Home_Page_Window.Top = e.Window_Location.Y
            Schema.Global_Postion() = e.Window_Location()
        End If
        If e.Window_ActualSize.Width = 0 And e.Window_ActualSize.Height = 0 Then
            Home_Page_Window.Width = AvatarX.CanvasSize().Width
            Home_Page_Window.Height = AvatarX.CanvasSize().Height
            Schema.Global_Size() = AvatarX.CanvasSize()
        ElseIf e.Window_ActualSize.Width > 0 And e.Window_ActualSize.Height > 0 Then
            Home_Page_Window.Width = e.Window_ActualSize.Width()
            Home_Page_Window.Height = e.Window_ActualSize.Height
            Schema.Global_Size() = e.Window_ActualSize()
        End If


        ResizeControls(AvatarX.ActualWidth(), AvatarX.ActualHeight())

        'Fonts and Colors
        If IsNothing(e.Window_Color()) = False Then
            Schema.Global_Window_Color() = New SolidColorBrush(e.Window_Color())
        End If

        If Utility.IsEmpty(e.Global_FontFamily()) = False Then
            If Local.IsFont(e.Global_FontFamily()) = True Then
                Schema.Global_Font() = New FontFamily(e.Global_FontFamily())
            Else
                Schema.Global_Font() = New FontFamily(Folders.Data() & "#" & e.Global_FontFamily())
            End If
        End If

        If e.Global_FontSize() > 0 Then
            Schema.Global_FontSize() = e.Global_FontSize()
        End If

        If IsNothing(e.Global_FontColor()) = False Then
            Schema.Global_FontColor() = New SolidColorBrush(e.Global_FontColor())
        End If

        If IsNothing(e.Global_HoverColor()) = False Then
            Schema.Global_HoverColor() = New SolidColorBrush(e.Global_HoverColor())
        End If

        If IsNothing(e.Global_FontBackColor()) = False Then
            Schema.Global_FontBackColor() = New SolidColorBrush(e.Global_FontBackColor())
        End If

        If IsNothing(e.Prompt_AiColor()) = False Then
            Schema.Prompt_Ai_FontColor() = New SolidColorBrush(e.Prompt_AiColor())
        End If

        If IsNothing(e.Prompt_UserColor()) = False Then
            Schema.Prompt_User_FontColor() = New SolidColorBrush(e.Prompt_UserColor())
        End If

        If IsNothing(e.Global_BorderColor()) = False Then
            Schema.Global_Border_Color() = New SolidColorBrush(e.Global_BorderColor())
        End If

        If IsNothing(e.Status_FontColor()) = False Then
            Schema.Status_FontColor() = New SolidColorBrush(e.Status_FontColor())
        End If

        If IsNothing(e.Prompt_ErrorColor()) = False Then
            Schema.Prompt_Error_FontColor() = New SolidColorBrush(e.Prompt_ErrorColor())
        End If


        'script
        If IsNothing(e.Script_AttributeColor()) = False Then
            Schema.Script_AttributeColor() = e.Script_AttributeColor()
        End If
        If IsNothing(e.Script_ClassColor()) = False Then
            Schema.Script_ClassColor() = e.Script_ClassColor()
        End If
        If IsNothing(e.Script_CommentColor()) = False Then
            Schema.Script_CommentColor() = e.Script_CommentColor()
        End If
        If IsNothing(e.Script_KeyWordColor()) = False Then
            Schema.Script_KeyWordColor() = e.Script_KeyWordColor()
        End If
        If IsNothing(e.Script_NumberColor()) = False Then
            Schema.Script_NumberColor() = e.Script_NumberColor()
        End If
        If IsNothing(e.Script_StringColor()) = False Then
            Schema.Script_StringColor() = e.Script_StringColor()
        End If


        If IsNothing(e.Script_Font()) = False Then
            Schema.Script_Font() = e.Script_Font()
        End If
        If IsNothing(e.Script_FontSize()) = False Then
            Schema.Script_FontSize() = e.Script_FontSize()
        End If
        If IsNothing(e.Script_FontColor()) = False Then
            Schema.Script_FontColor() = e.Script_FontColor()
        End If
        If IsNothing(e.Script_BackColor()) = False Then
            Schema.Script_BackColor() = e.Script_BackColor()
        End If


        'Origin
        If IsNothing(e.Origin()) = False Then
            AvatarX.Origin() = e.Origin()
        End If

        If IsNothing(e.ShowAvatar()) = False Then
            AvatarX.ShowAvatar() = e.ShowAvatar()
        End If

        If IsNothing(e.SuspendScript()) = False Then
            AvatarX.SuspendScript() = e.SuspendScript()
        End If

        If IsNothing(e.Suspend()) = False Then
            AvatarX.Suspend() = e.Suspend()
        End If

        If IsNothing(e.ShowCanvas()) = False Then
            AvatarX.ShowCanvas() = e.ShowCanvas()
            Visible() = e.ShowCanvas()
        End If

        Refresh_Exit_Panel()
        Refresh_Globals()

        If InitialStatusMessage() = True Then
            InitialStatusMessage() = False
            Status_PROMPT("Type """ & "Please wake up!" & """ to initiate Athena. Type """ & "Please go to sleep." &
                          """ to put Athena to sleep. Type """ & "What can I say?" & """ to receive a list of commands.")
        End If

    End Sub



    Private Sub NotifyByScripts(ByVal names As List(Of String), ByVal args As List(Of List(Of String))) Handles AvatarX.NotifyByScript
        'this allows for the script to modify variables in the main program
        'it can fire once per script-line in the 'notify' command is given

        Dim index As Integer = 0

        index = names.IndexOf("somecommand")
        If index > -1 Then

            If args(index).Count = 3 Then
                'Rotation(Dbl(args(index)(0)), Dbl(args(index)(1)), Dbl(args(index)(2)))
            End If

        End If


    End Sub

    Private Sub ResizeControls(ByVal Width As Double, ByVal Height As Double)

        Dim e = CurrentProfile()

        If e.Input_Location.X >= 0 And e.Input_Location.Y >= 0 And
            e.Input_Size.Width > 0 And e.Input_Size.Height > 0 Then

            If e.Input_Size.Width > e.Input_Location.X And e.Input_Size.Height > e.Input_Location.Y Then

                Dim left = (e.Input_Location.X / 100) * Width
                Dim top = (e.Input_Location.Y / 100) * Height
                Dim newWidth = ((e.Input_Size.Width - e.Input_Location.X) / 100) * Width
                Dim newHeight = ((e.Input_Size.Height - e.Input_Location.Y) / 100) * Height

                InputX.Margin = New Thickness(left, top, 0, 0)
                InputX.Width = newWidth
                InputX.Height = newHeight

            End If
        End If

        If e.Prompt_Location.X >= 0 And e.Prompt_Location.Y >= 0 And
            e.Prompt_Size.Width > 0 And e.Prompt_Size.Height > 0 Then

            If e.Prompt_Size.Width > e.Prompt_Location.X And e.Prompt_Size.Height > e.Prompt_Location.Y Then

                Dim left = (e.Prompt_Location.X / 100) * Width
                Dim top = (e.Prompt_Location.Y / 100) * Height
                Dim newWidth = ((e.Prompt_Size.Width - e.Prompt_Location.X) / 100) * Width
                Dim newHeight = ((e.Prompt_Size.Height - e.Prompt_Location.Y) / 100) * Height

                PromptX.Margin = New Thickness(left, top, 0, 0)
                PromptX.Width = newWidth
                PromptX.Height = newHeight
            End If

        End If

        If e.Status_Location.X >= 0 And e.Status_Location.Y >= 0 And
            e.Status_Size.Width > 0 And e.Status_Size.Height > 0 Then

            If e.Status_Size.Width > e.Status_Location.X And e.Status_Size.Height > e.Status_Location.Y Then

                Dim left = (e.Status_Location.X / 100) * Width
                Dim top = (e.Status_Location.Y / 100) * Height
                Dim newWidth = ((e.Status_Size.Width - e.Status_Location.X) / 100) * Width
                Dim newHeight = ((e.Status_Size.Height - e.Status_Location.Y) / 100) * Height

                StatusX.Margin = New Thickness(left, top, 0, 0)
                StatusX.Width = newWidth
                StatusX.Height = newHeight
            End If

        End If

        If e.Menu_Location.X >= 0 And e.Menu_Location.Y >= 0 And
             e.Menu_Size.Width > 0 And e.Menu_Size.Height > 0 Then

            If e.Menu_Size.Width > e.Menu_Location.X And e.Menu_Size.Height > e.Menu_Location.Y Then

                Dim left = (e.Menu_Location.X / 100) * Width
                Dim top = (e.Menu_Location.Y / 100) * Height
                Dim newWidth = ((e.Menu_Size.Width - e.Menu_Location.X) / 100) * Width
                Dim newHeight = ((e.Menu_Size.Height - e.Menu_Location.Y) / 100) * Height

                MenuX.Margin = New Thickness(left, top, 0, 0)
                MenuX.Width = newWidth
                MenuX.Height = newHeight
            End If
        End If

        If e.Grip_Location.X >= 0 And e.Grip_Location.Y >= 0 And
              e.Grip_Size.Width > 0 And e.Grip_Size.Height > 0 Then

            If e.Grip_Size.Width > e.Grip_Location.X And e.Grip_Size.Height > e.Grip_Location.Y Then

                Dim left = (e.Grip_Location.X / 100) * Width
                Dim top = (e.Grip_Location.Y / 100) * Height
                Dim newWidth = ((e.Grip_Size.Width - e.Grip_Location.X) / 100) * Width
                Dim newHeight = ((e.Grip_Size.Height - e.Grip_Location.Y) / 100) * Height

                GripX.Margin = New Thickness(left, top, 0, 0)
                GripX.Width = newWidth
                GripX.Height = newHeight
            End If
        End If


    End Sub

#End Region


#Region "Input"

    Private Property Brain_Commands() As New List(Of String)

    Private Shared Property ReadMemory() As New Memory
    WithEvents Read_Timer As New TimeDispatcher(100)

    Private Sub Reader() Handles Read_Timer.Tick
        ReadMemory.Load()

        If ReadMemory.Ready() = True Then

            Dim response = ReadMemory().Response()
            Dim book = ReadMemory().Book()
            Dim errors = ReadMemory().Errors()
            Dim script = ReadMemory().Script()
            Dim status = ReadMemory().Status()
            Dim commands = ReadMemory().Commands()
            Dim code = ReadMemory().Code()

            'Send to avatar
            If Utility.IsEmpty(errors) = False Then
                Prompt_ERROR(errors)
                Speak(Utility.RandArray(DEF_ERRORMESSAGE, ""))
            Else

                If Utility.IsEmpty(code) = False Then
                    AvatarX.Script(code)
                End If

                If Utility.IsEmpty(script) = False Then
                    AvatarX.Script(script)
                End If

                If Utility.IsEmpty(status) = False Then
                    Status_PROMPT(status)
                End If

                If Utility.IsEmpty(response) = False Then
                    Prompt_AI(response)
                    Speak(response)
                End If

                If Utility.IsEmpty(book) = False Then
                    Dim text = Utility.TextReader(book)
                    Prompt_AI(text)
                    Speak(text)
                End If

                If Utility.IsEmpty(commands) = False Then
                    Brain_Commands().Clear()
                    Brain_Commands().AddRange(commands)
                End If

            End If

            ReadMemory.Reset()
        End If
    End Sub


    Private Shared Property WriteMemory() As New Memory
    Private Property InputHistory() As New List(Of String)
    Private Property InputIndex() As Integer = 0
    Private Property KeySpace As Boolean = False
    Private Property CommandResults() As String = ""
    Private Property CurrentInput() As String = ""

    Private Sub InputRecieve(ByVal sender As Object, ByVal e As KeyEventArgs) Handles InputX.PreviewKeyDown

        If e.Key = Key.Up Then
            If KeySpace = True Then
                InputIndex() += 1
                KeySpace = False
            End If

            InputIndex() -= 1

            If Utility.InRange(InputHistory(), InputIndex()) = True Then
                InputX.Text = InputHistory(InputIndex())
            Else
                InputIndex() += 1
            End If
        End If

        If e.Key = Key.Down Then
            InputIndex() += 1

            If Utility.InRange(InputHistory(), InputIndex()) = True Then
                InputX.Text = InputHistory(InputIndex())
            Else
                InputX.Text = ""
                InputIndex() -= 1
                KeySpace = True
            End If
        End If

        If e.Key = Key.Return Then

            CurrentInput() = Utility.Trim(InputX.Text())

            'History storage for input
            InputHistory.Add(CurrentInput())
            If InputHistory.Count() > 100 Then
                InputHistory.RemoveAt(0)
            End If

            InputIndex() = InputHistory.Count()
            KeySpace = False

            If Utility.IsEmpty(CurrentInput()) = False Then
                Prompt_USER(CurrentInput())

                'Process commands
                CommandResults() = ProcessCommands(CurrentInput())

                If CommandResults() = DEFAULT_SKIP Then
                    Clean_Timer.Start(1)
                    Exit Sub
                End If

                If CommandResults() <> DEFAULT_NOTHING Then
                    Prompt_AI(CommandResults())
                    Speak(CommandResults())
                Else

                    '*****
                    'sending info
                    WriteMemory.Clear()
                    WriteMemory.UserInput() = CurrentInput()
                    WriteMemory.Ready() = True
                    WriteMemory.Save()
                    '*****
                End If

                Clean_Timer.Start(1)

            End If

        End If

    End Sub

    WithEvents CodeManage As New Scripts
    Private Shared Property CodeRun() As Boolean = False
    WithEvents Code_Thread As New ComponentModel.BackgroundWorker
    Private Sub Code_DoWork() Handles Code_Thread.DoWork

        If Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = False Then
            If CodeManage().IsRunning() = False Then

                CodeManage().CodeSource() = Schema.Current_Brain()
                CodeManage().PluginSource() = Folders.Plugins()
                CodeManage().ProgramSource() = Folders.Directory()
                CodeManage().Compile()
                CodeManage.Print(Files.BrainDbg())

                If CodeRun() = True Then
                    CodeManage().Run()
                End If
            End If
        End If

    End Sub

    Private Sub CodeManage_IsErrorEvent() Handles CodeManage.IsError
        If Utility.IsEmpty(CodeManage.Errors()) = False Then
            Dim errors As String = CodeManage.Errors()
            PromptX.Dispatcher.BeginInvoke(Sub()
                                               Prompt_ERROR(errors)
                                               Speak(Utility.RandArray(DEF_ERRORMESSAGE, ""))
                                           End Sub)
            CodeManage.Errors() = ""
        End If
    End Sub


    Const DEF_STATUS As String = "<STATUS> "
    Const DEF_AI As String = "<AI> "
    Const DEF_USER As String = "<USER> "
    Const DEF_ERROR As String = "<ERROR> "
    Property DEF_ERRORMESSAGE As String() = {"An error has occured.", "I'm having an error.", "There was some kind of error."}

    WithEvents Clean_Timer As New DelayDispatcher()
    Private Sub Clean_Input() Handles Clean_Timer.Elapsed
        InputX.Dispatcher.BeginInvoke(Sub() InputX.Clear())
    End Sub

    Private Sub Prompt_ERROR(ByVal input As String)
        input = DEF_ERROR & input & vbCrLf
        PromptX.Send(input, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Error_FontColor))
    End Sub
    Private Sub Prompt_ERROR(ByVal input As List(Of String))
        Dim out = DEF_ERROR & Utility.Join(input, vbCrLf) & vbCrLf
        PromptX.Send(out, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Error_FontColor))
    End Sub

    Private Sub Status_PROMPT(ByVal input As String)
        StatusX.Send(input, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Status_FontColor))
    End Sub
    Private Sub Status_PROMPT(ByVal input As List(Of String))
        StatusX.Send(Utility.Join(input, vbCrLf), New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Status_FontColor))
    End Sub

    Private Sub Prompt_CLEAN(ByVal input As String)
        input = input & vbCrLf
        PromptX.Send(input, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Ai_FontColor))
    End Sub
    Private Sub Prompt_CLEAN(ByVal input As List(Of String))
        Dim out = Utility.Join(input, vbCrLf) & vbCrLf
        PromptX.Send(out, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Ai_FontColor))
    End Sub


    Private Sub Prompt_AI(ByVal input As String)
        input = DEF_AI & input & vbCrLf
        input = PromptArgs(input)
        PromptX.Send(input, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Ai_FontColor))
    End Sub
    Private Sub Prompt_AI(ByVal input As List(Of String))
        Dim out = DEF_AI & Utility.Join(input, vbCrLf) & vbCrLf
        out = PromptArgs(out)
        PromptX.Send(out, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_Ai_FontColor))
    End Sub


    Private Sub Prompt_USER(ByVal input As String)
        input = DEF_USER & input & vbCrLf
        PromptX.Send(input, New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Prompt_User_FontColor()))
    End Sub

    Private Sub Speak(ByVal input As String)
        input = Utility.RemoveByArgs(input, "(", ")", Gate.eEQUALS)

        input = SpeakArgs(input)
        AvatarX.SpeechStop()
        AvatarX.ReadLargeText() = False
        AvatarX.Speak(input, Schema.Current_Voice, Schema.Current_Voice_Speed)
    End Sub
    Private Sub Speak(ByVal input As List(Of String))
        AvatarX.SpeechStop()
        AvatarX.ReadLargeText() = False

        For Each res In input
            res = SpeakArgs(res)
            AvatarX.Speak(res, Schema.Current_Voice, Schema.Current_Voice_Speed)
        Next
    End Sub

    Private Function SpeakArgs(ByVal input As String) As String
        Dim args = Utility.GetArgArray(input, "(", ")", Gate.eEQUALS)

        For Each arg In args
            Dim tospeak = Utility.GetMidArg(arg, Utility.QUOTE, Utility.QUOTE, Gate.eEQUALS)
            input = Utility.Replace(input, "(" & arg & ")", tospeak)
        Next
        Return input
    End Function
    Private Function PromptArgs(ByVal input As String) As String
        Dim args = Utility.GetArgArray(input, "(", ")", Gate.eEQUALS)

        For Each arg In args
            Dim todisplay = Utility.Trim(Utility.RemoveByArgs(arg, Utility.QUOTE, Utility.QUOTE, Gate.eEQUALS))
            input = Utility.Replace(input, "(" & arg & ")", todisplay)
        Next
        Return input
    End Function

    Private Sub Compiles()
        CodeRun() = False
        Code_Thread.RunWorkerAsync()
    End Sub


    Private Sub Starts()
        WriteMemory.Clear()
        WriteMemory.UserInput() = CurrentInput()
        WriteMemory.Package() = Schema.Current_Package()
        WriteMemory.Cluster() = Schema.Current_Cluster()
        WriteMemory.Profile() = Schema.Current_Profile()
        WriteMemory.Avatar() = Schema.Current_Ai()
        WriteMemory.Ready() = True
        WriteMemory.Save()

        CodeRun() = True
        Code_Thread.RunWorkerAsync()
    End Sub

    Private Sub Restarts()
        WriteMemory.Clear()
        WriteMemory.UserInput() = "a/exit"
        WriteMemory.Ready() = True
        WriteMemory.Save()

        Do Until Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = False
            Utility.Sleep(100)
        Loop

        If Code_Thread.IsBusy() = True Then Code_Thread.CancelAsync()

        Do Until Code_Thread.IsBusy() = False
            Utility.Sleep(100)
        Loop

        CodeRun() = True
        Code_Thread.RunWorkerAsync()
    End Sub


    Private Sub Checks()

    End Sub

    Const DEFAULT_SKIP As String = "<skip>"
    Const DEFAULT_NOTHING As String = "<nothing>"

    Private Function ProcessCommands(ByVal cmd As String) As String

        Dim commands As New List(Of String)

        If Utility.Search(cmd, "Please Wake up!", Gate.eEQUALS) = True Then
            cmd = "u/start"
        End If

        If Utility.Search(cmd, "Please go to sleep.", Gate.eEQUALS) = True Then
            cmd = "u/sleep"
        End If

        If Utility.Search(cmd, "What can I say?", Gate.eEQUALS) = True Then
            cmd = "u/commands"
        End If

        If Utility.Search(cmd, {"u\*", "u/*"}, Gate.eEQUALS) = True Then
            cmd = Utility.Remove(cmd, {"u\", "u/"})

            Dim args = Utility.Trim(Utility.ToArray(cmd, ":"))

            commands.Add("u/start <runs brain script>")
            If Utility.Search(cmd, "start", Gate.eEQUALS) = True Then
                If Code_Thread.IsBusy() = False And
                    Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = False Then
                    Starts()
                    Return "I am now waking."
                Else
                    Return "I am already awake."
                End If

                Return DEFAULT_SKIP
            End If

            commands.Add("u/restart <restarts brain script>")
            If Utility.Search(cmd, "restart", Gate.eEQUALS) = True Then

                Restarts()
                Return "Now attempting to restart."
            End If

            'allows athena to save data (safest method)
            commands.Add("u/sleep <saves and stops brain script>")
            If Utility.Search(cmd, "sleep", Gate.eEQUALS) = True Then

                If Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = True Then
                    CurrentInput() = "a/exit"

                    Dim out = "I will try to sleep."
                    Prompt_AI(out)
                    Speak(out)

                    Return DEFAULT_NOTHING
                Else
                    Return "I am already asleep."
                End If

                Return DEFAULT_SKIP
            End If

            commands.Add("u/shutdown <stops brain script without save>")
            If Utility.Search(cmd, "shutdown", Gate.eEQUALS) = True Then

                If Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = True Then
                    If Code_Thread.IsBusy() = True Then Code_Thread.CancelAsync()
                    Utility.ExitProgram(Scripts.DEF_PROGRAM_NAME)

                    Return "Now shutting down brain."
                Else
                    Return "Brain is already shutdown."
                End If

                Return DEFAULT_SKIP
            End If

            commands.Add("u/compile <compiles and runs brain>")
            If Utility.Search(cmd, "compile", Gate.eEQUALS) = True Then
                If Code_Thread.IsBusy() = False And
                    Utility.IsProgramRunning(Scripts.DEF_PROGRAM_NAME) = False Then
                    Compiles()
                    Prompt_CLEAN("AI is compiled.")
                Else
                    Prompt_CLEAN("AI is currently running.")
                End If

                Return DEFAULT_SKIP
            End If

            commands.Add("u/debug <debugging mode>")
            If Utility.Search(cmd, "debug", Gate.eSTART) = True Then
                If AvatarX.IsActivated() = False Then
                    AvatarX.IsActivated() = True
                    Active_Debug()
                    Return "Debugging mode is now active."
                Else
                    AvatarX.IsActivated() = False
                    Deactive_Debug()
                    Return "Debugging mode is now deactivated."
                End If
            End If

            commands.Add("u/face: <animation-to-run>")
            If Utility.Search(cmd, "face:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Face() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/back: <animation-to-run>")
            If Utility.Search(cmd, "back:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Back() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/right: <animation-to-run>")
            If Utility.Search(cmd, "right:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Right() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/left: <animation-to-run>")
            If Utility.Search(cmd, "left:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Left() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/top: <animation-to-run>")
            If Utility.Search(cmd, "top:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Top() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/bottom: <animation-to-run>")
            If Utility.Search(cmd, "bottom:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Bottom() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/canvas: <animation-to-run>")
            If Utility.Search(cmd, "canvas:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Canvas() = args(1)
                    Return Utility.Concat({"Searching animations for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/script: <script-to-run>")
            If Utility.Search(cmd, "script:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    AvatarX.Script(args(1))
                    Return Utility.Concat({"Searching scripts for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/play: <sound-to-play>")
            If Utility.Search(cmd, "play:", Gate.eSTART) = True Then
                If args.Length() = 2 Then

                    Audio.Close()
                    For Each file In Utility.GetAllFiles(Folders.Sounds())
                        Dim name = Utility.GetFileName(file, True)
                        If Utility.Search(name, args(1), Gate.eEQUALS) = True Then
                            Audio.Play(name, file, False, 0, 100, 0, 100, False)
                            Exit For
                        End If
                    Next

                    Return Utility.Concat({"Searching for", args(1), "."}, True)
                End If

            End If

            commands.Add("u/playlist <gets a list of sounds>")
            If Utility.Search(cmd, "playlist", Gate.eSTART) = True Then

                Dim all_files = Utility.GetAllFiles(Folders.Sounds())

                For Each file In all_files
                    file = Utility.GetFileName(file, True)
                    Prompt_CLEAN(file)
                Next

                Return "Here is a list of all the sounds I have"
            End If

            commands.Add("u/stop <stops everything>")
            If Utility.Search(cmd, "stop", Gate.eSTART) = True Then
                AvatarX.ScriptStop()
                AvatarX.SpeechStop()
                Audio.Close()
                AvatarX.Home()
                Return "Stopping all activity."
            End If

            commands.Add("u/home <returns avatar to home position>")
            If Utility.Search(cmd, "home", Gate.eSTART) = True Then
                AvatarX.Home()
                Return DEFAULT_SKIP
            End If

            commands.Add("repeat: <line-to-repeat>")
            If Utility.Search(cmd, "repeat:", Gate.eSTART) = True Then
                If args.Length() = 2 Then
                    Prompt_AI(args(1))
                    Speak(args(1))
                End If

                Return DEFAULT_SKIP
            End If

            commands.Add("u/exit <exits ui program>")
            If Utility.Search(cmd, "exit", Gate.eSTART) = True Then
                Closing_Timer.Start(500)
                Return "Program is now Shutting Down."
            End If

            commands.Add("u/clear <clears prompt/status>")
            If Utility.Search(cmd, "clear", Gate.eSTART) = True Then
                PromptX.Clear()
                StatusX.Clear()
                Return DEFAULT_SKIP
            End If

            commands.Add("u/copy <copies prompt>")
            If Utility.Search(cmd, "copy", Gate.eSTART) = True Then
                PromptX.Copy()
                Return DEFAULT_SKIP
            End If

            commands.Add("u/read: <book-to-read>")
            If Utility.Search(cmd, "read:", Gate.eSTART) = True Then

                If args.Length() = 2 Then

                    For Each file In Utility.GetAllFiles(Folders.Books())
                        Dim name = Utility.GetFileName(file, True)
                        If Utility.Search(name, args(1), Gate.eEQUALS) = True Then
                            Dim text = Utility.TextReader(file)
                            Prompt_CLEAN(text)
                            Speak(text)
                            Exit For
                        End If
                    Next

                    Return DEFAULT_SKIP
                End If
            End If

            commands.Add("u/booklist <gets a list of books>")
            If Utility.Search(cmd, "booklist", Gate.eSTART) = True Then

                Dim all_files = Utility.GetAllFiles(Folders.Books())

                For Each file In all_files
                    file = Utility.GetFileName(file, True)
                    Prompt_CLEAN(file)
                Next

                Return "Here is a list of all the books I have"
            End If

            'commands originating from brain script
            commands.Add("u/scriptcmds <available brain commands>")
            If Utility.Search(cmd, "scriptcmds", Gate.eSTART) = True Then

                If commands.Count() = 0 Then
                    Prompt_CLEAN("No script-commands avaliable.")
                Else
                    Prompt_CLEAN(Utility.Join(Brain_Commands(), vbCrLf))
                End If
                Return DEFAULT_SKIP
            End If

            'Keep this as last command!!!
            commands.Add("u/commands <a list of ui commands>")
            If Utility.Search(cmd, "commands", Gate.eSTART) = True Then
                Prompt_CLEAN(Utility.Join(commands, vbCrLf))
                Return DEFAULT_SKIP
            End If


            Return "Command not found!"
        End If

        Return DEFAULT_NOTHING
    End Function

    WithEvents Closing_Timer As New DelayDispatcher()
    Private Sub Closing_Delay() Handles Closing_Timer.Elapsed
        Utility.ExitProgram()
    End Sub

#End Region


#Region "Utils"

    Private Sub Active_Debug()
        PromptX.BorderBrush() = Schema.Global_Border_Color()
        StatusX.BorderBrush() = Schema.Global_Border_Color()
        InputX.BorderBrush() = Schema.Global_Border_Color()
        MenuX.BorderBrush() = Schema.Global_Border_Color()
        GripX.BorderBrush() = Schema.Global_Border_Color()
        AvatarX.ShowDebug() = True
    End Sub

    Private Sub Deactive_Debug()
        PromptX.BorderBrush() = Brushes.Transparent()
        StatusX.BorderBrush() = Brushes.Transparent()
        InputX.BorderBrush() = Brushes.Transparent()
        MenuX.BorderBrush() = Brushes.Transparent()
        GripX.BorderBrush() = Brushes.Transparent()
        AvatarX.ShowDebug() = False
    End Sub

    Private Sub LostActivated() Handles Home_Page_Window.Deactivated
        AvatarX.IsActivated() = False
        Deactive_Debug()
    End Sub


    Private Sub Store_Postition()
        Schema.Global_Postion() = New Point(Home_Page_Window.Left, Home_Page_Window.Top)
    End Sub

    Private Sub Store_Size()
        Schema.Global_Size() = New Size(Home_Page_Window.Width, Home_Page_Window.Height)
    End Sub

    Private WriteOnly Property Visible() As Boolean
        Set(value As Boolean)
            If value = True Then
                MenuX.Visibility = Visibility.Visible
                InputX.Visibility = Visibility.Visible
                PromptX.Visibility = Visibility.Visible
                StatusX.Visibility = Visibility.Visible
                AvatarX.SuspendScript() = False
                AvatarX.ShowAvatar() = True
            Else
                MenuX.Visibility = Visibility.Hidden
                InputX.Visibility = Visibility.Hidden
                PromptX.Visibility = Visibility.Hidden
                StatusX.Visibility = Visibility.Hidden
                AvatarX.SuspendScript() = True
                AvatarX.ShowAvatar() = False
            End If
        End Set
    End Property

#End Region


#Region "Window Basics"

    Private Sub ExitMenu_Event(ByVal button As GenerikButton) Handles Exit_Menu.LeftClick

        If button.Name() = "min" Then
            Home_Page_Window.WindowState = WindowState.Minimized
        End If

        If button.Name() = "max" Then
            Max_Event()
        End If

        If button.Name() = "exit" Then
            Utility.ExitProgram()
        End If

    End Sub


    Private Property PreviousSize() As Size
    Private Property PreviousPos() As Point
    Private Property IsMaxed() As Boolean = False
    Private Sub Max_Event()
        If IsMaxed = False Then
            IsMaxed() = True
            PreviousSize() = New Size(Home_Page_Window.ActualWidth, Home_Page_Window.ActualHeight)
            PreviousPos() = New Point(Home_Page_Window.Left, Home_Page_Window.Top)
            Home_Page_Window.WindowState = WindowState.Maximized
            UpdateSize_Event(MouseInfo.ScreenSize())
        Else
            Home_Page_Window.WindowState = WindowState.Normal
            Home_Page_Window.Left = PreviousPos.X
            Home_Page_Window.Top = PreviousPos.Y
            Home_Page_Window.Width = PreviousSize.Width
            Home_Page_Window.Height = PreviousSize.Height

            UpdateSize_Event(PreviousSize())
            IsMaxed() = False
        End If
    End Sub


    Private Property move_offset() As Point
    Private Sub Move_Event() Handles GripX.MouseLeftButtonDown
        If IsMaxed() = True Then
            Max_Event()
        Else
            Dim pnt = MouseInfo.GetMousePosition()
            move_offset = New Point(pnt.X - Home_Page_Window.Left, pnt.Y - Home_Page_Window.Top)
            move_loop.Start()
        End If
    End Sub

    WithEvents move_loop As New TimeDispatcher(10)
    Private Sub moving_now() Handles move_loop.Tick

        Dim pnt = MouseInfo.GetMousePosition()

        Home_Page_Window.Top = pnt.Y - move_offset.Y
        Home_Page_Window.Left = pnt.X - move_offset.X
        Store_Postition()

        If MouseInfo.GetAsyncKeyState(KeyStates.VK_LBUTTON) = 0 Then
            move_loop.Stop()
        End If

    End Sub

    Private WithEvents Schema_Event As New Schema
    Private Sub UpdateSize_Event(ByVal wsize As Size) Handles Schema_Event.UpdateSize
        Dim currentWidth = Home_Page_Window.Width
        Dim currentHeight = Home_Page_Window.Height

        Dim diffW = (wsize.Width - currentWidth) / 2
        Dim diffH = (wsize.Height - currentHeight) / 2

        Dim X = Home_Page_Window.Left - diffW
        Dim Y = Home_Page_Window.Top - diffH

        Home_Page_Window.Left = X
        Home_Page_Window.Top = Y

        Home_Page_Window.Width = wsize.Width
        Home_Page_Window.Height = wsize.Height
        ResizeControls(AvatarX.ActualWidth(), AvatarX.ActualHeight())

        Store_Size()
        Store_Postition()
    End Sub

    WithEvents ScaleDialog As New ScaleDialog
    Private Sub Scale_Event(ByVal button As GenerikButton) Handles Scale_Menu.LeftClick

        If button.Name() = "scale" Then

            If IsMaxed() = True Then
                Home_Page_Window.WindowState = WindowState.Normal
                UpdateSize_Event(PreviousSize())
                IsMaxed() = False
            End If

            ScaleDialog = New ScaleDialog
            Dim center = Local.FindCenter(New Point(Home_Page_Window.Left, Home_Page_Window.Top), Schema.Global_Size, New Size(ScaleDialog.Width, ScaleDialog.Height))
            ScaleDialog.Location() = center
            ScaleDialog.Show()

        End If
    End Sub

    Private Sub Scale_Hover_Enter() Handles ScaleDialog.Closed
        Visible() = True
    End Sub

    Private Sub Scale_Hover_Exit() Handles ScaleDialog.Opened
        Visible() = False
    End Sub

#End Region



End Class


