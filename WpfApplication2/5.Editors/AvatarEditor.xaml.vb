Imports System.Windows.Media.Media3D
Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

Public Class AvatarEditor


    Private Sub Initialize() Handles Me.Initialized

        Me.Background() = Schema.Global_Window_Color()
        FastScript_Editor.Font(Schema.Normal_Fonts)

        Main_TreeView.BorderBrush = Schema.Global_Border_Color

        Schema.SetStyle(Border_Index_Edit)

        Border_Texture_Menu.BorderBrush = Schema.Global_Border_Color
        Border_Texture_Menu.BorderThickness = New Thickness(5)

        Border_CurrentFile.BorderBrush = Schema.Global_Border_Color
        Border_CurrentFile.BorderThickness = New Thickness(5, 0, 5, 5)

        SP_Index_Select.BorderBrush = Schema.Global_Border_Color
        SP_Index_Select.BorderThickness = New Thickness(0)

        Button_Audio_Add.TextBuffer(30, 30, 30, 30)
        Button_Audio_Delete.TextBuffer(30, 30, 30, 30)
        Button_Audio_Play.TextBuffer(30, 30, 30, 30)
        Button_Audio_Stop.TextBuffer(30, 30, 30, 30)
        Button_Audio_Pause.TextBuffer(30, 30, 30, 30)

        Schema.SetStyle(LB_Index_Animation)
        Schema.SetStyle(LB_Index_ChanceTickSkip)
        Schema.SetStyle(LB_Index_Images)
        Schema.SetStyle(LB_Index_ImageSelect)
        Schema.SetStyle(LB_Index_IsLoop)
        Schema.SetStyle(LB_Index_Profile)
        Schema.SetStyle(LB_Index_Side)
        Schema.SetStyle(LB_Index_TickSkip)

        Schema.SetStyle(LB_Script_AttributeColor)
        Schema.SetStyle(LB_Script_BackColor)
        Schema.SetStyle(LB_Script_ClassColor)
        Schema.SetStyle(LB_Script_CommentColor)
        Schema.SetStyle(LB_Script_Font)
        Schema.SetStyle(LB_Script_FontColor)
        Schema.SetStyle(LB_Script_FontSize)
        Schema.SetStyle(LB_Script_KeyWordColor)
        Schema.SetStyle(LB_Script_NumberColor)
        Schema.SetStyle(LB_Script_StringColor)

        Schema.SetStyle(LB_Profile_AiColor)
        Schema.SetStyle(LB_Profile_BorderColor)
        Schema.SetStyle(LB_Profile_ErrorColor)
        Schema.SetStyle(LB_Profile_Font)
        Schema.SetStyle(LB_Profile_FontColor)
        Schema.SetStyle(LB_Profile_FontSize)
        Schema.SetStyle(LB_Profile_Grip)
        Schema.SetStyle(LB_Profile_HoverColor)
        Schema.SetStyle(LB_Profile_FontbackColor)
        Schema.SetStyle(LB_Profile_Input)
        Schema.SetStyle(LB_Profile_Menu)
        Schema.SetStyle(LB_Profile_Origin)
        Schema.SetStyle(LB_Profile_Prompt)
        Schema.SetStyle(LB_Profile_Status)
        Schema.SetStyle(LB_Profile_ShowAvatar)
        Schema.SetStyle(LB_Profile_ShowCanvas)
        Schema.SetStyle(LB_Profile_StatusColor)
        Schema.SetStyle(LB_Profile_Suspend)
        Schema.SetStyle(LB_Profile_SuspendScript)
        Schema.SetStyle(LB_Profile_UserColor)
        Schema.SetStyle(LB_Profile_Window)
        Schema.SetStyle(LB_Profile_WindowColor)
        Schema.SetStyle(LB_CurrentFile)
        Schema.SetStyle(LB_CurrentFileTitle)
        Schema.SetStyle(LB_Texture_Frame)
        Schema.SetStyle(LB_Texture_FrameIndex)
        Schema.SetStyle(LB_Index_Select)

        Schema.SetStyle(TB_Index_Animation)
        Schema.SetStyle(TB_Index_ChanceTickSkip)
        Schema.SetStyle(TB_Index_Images)
        Schema.SetStyle(TB_Index_Profile)
        Schema.SetStyle(TB_Index_TickSkip)


        SP_Index_Side.Items.Add("face")
        SP_Index_Side.Items.Add("back")
        SP_Index_Side.Items.Add("right")
        SP_Index_Side.Items.Add("left")
        SP_Index_Side.Items.Add("top")
        SP_Index_Side.Items.Add("bottom")
        SP_Index_Side.Items.Add("canvas")
        SP_Index_Side.Index() = 0

        SP_Profile_Font.Clear()
        SP_Profile_Font.Items().AddRange(Local.GetLocalFonts)
        SP_Profile_Font.Items().AddRange(Local.FontFamilies)

        SP_Script_Font.Clear()
        SP_Script_Font.Items().AddRange(Local.FontFamilies)

        Main_Menu.ButtonInfo() = {"File|File", "Save|Save"}

        FastScript_Editor.ShowHighlight() = False
        FastScript_Editor.ShowPopup() = False
    End Sub

    Private Sub Load() Handles Me.Loaded
        Load_TreeView()
    End Sub

    Private Sub UnLoads() Handles Me.Unloaded
        ColorPicker.Close()
    End Sub


    Private Shared Property AvatarFolder() As String = Folders.Avatars()
    Private Shared Property CurrentScript() As String = ""
    Private Shared Property CurrentProfile() As String = ""
    Private Shared Property CurrentAvatar() As String = ""
    Private Shared Property CurrentAudio() As String = ""
    Private Shared Property CurrentAnimationFolder() As String = ""

    Private Shared ReadOnly Property CurrentAddress() As String
        Get
            Return AvatarFolder() & CurrentAvatar()
        End Get
    End Property

    Private Property IsIndexOpen() As Boolean = False
    Private Property IsAudioOpen() As Boolean = False
    Private Property IsProfileOpen() As Boolean = False
    Private Property IsScriptOpen() As Boolean = False
    Private Property IsTextureOpen() As Boolean = False


    Private Sub Main_Splitter_MouseEnter_Event() Handles Main_Splitter.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub Main_Splitter_MouseLeave_Event() Handles Main_Splitter.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub

    Private Sub Unload() Handles Me.Unloaded
        Audio.Close()
        Sub_Menu.Close()
    End Sub

    WithEvents Sub_Menu As New MenuDialog
    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick
        Dim makebutton = False

        Sub_Menu = New MenuDialog()
        Sub_Menu.Orientation = Orientation.Vertical
        Sub_Menu.TextHorizontalAlignment = HorizontalAlignment.Left
        Sub_Menu.Buffer() = New Thickness(10)
        Sub_Menu.ButtonBorder(Brushes.Transparent, New Thickness(0))

        If button.Name() = "Save" Then
            If IsIndexOpen() = True Then
                Index_Save()
            ElseIf IsAudioOpen() = True Then

            ElseIf IsProfileOpen() = True Then
                Profile_Save()
            ElseIf IsScriptOpen() = True Then
                Utility.ZipWriter(CurrentAddress(), {Schema.Avatar.FOLDER_SCRIPTS, CurrentScript()}, FastScript_Editor.Text())
            ElseIf IsTextureOpen() = True Then

            End If

            Message.Show(Me, "Save Successful!")
            Exit Sub
        End If

        If button.Name() = "File" Then
            Sub_Menu.ButtonInfo() = {"OpenFolder|Open Folder", "Refresh|Refresh"}
            makebutton = True
        End If

        If makebutton = True Then
            Sub_Menu.Load()
            Dim loc = Main_Menu.PointToScreen(New Point(0, 0))
            Sub_Menu.Location() = New Point(button.Position.X, loc.Y + Main_Menu.Size.Height)
            Sub_Menu.Show()
        End If
    End Sub

    Private Sub Sub_Menu_Event(ByVal button As GenerikButton) Handles Sub_Menu.LeftClick
        Sub_Menu.Close()

        If button.Name() = "OpenFolder" Then
            OpenFolder()
            Exit Sub
        End If

        If button.Name() = "Refresh" Then
            Load_TreeView()
            Exit Sub
        End If

    End Sub


    Private Sub OpenFolder()
        Utility.OpenFolder(Folders.Avatars())
    End Sub



#Region "Audio"

    Private Sub Audio_Load()

    End Sub

    Private Sub Audio_Play_Event() Handles Button_Audio_Play.LeftClick
        If Utility.IsEmpty(CurrentAudio()) = False Then

            If Audio.IsPaused(CurrentAudio()) = False Then
                If Audio.IsPlaying(CurrentAudio()) = False Then
                    If LoadAudio_Thread.IsBusy = False Then
                        LoadAudio_Thread.RunWorkerAsync()
                    End If
                End If
            Else
                Audio.Play()
            End If

        End If
    End Sub

    Private Shared Property LoadAudio_Stream() As IO.Stream
    WithEvents LoadAudio_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadAudio_DoWork() Handles LoadAudio_Thread.DoWork
        LoadAudio_Stream() = Utility.ZipStreamReader(AvatarFolder() & CurrentAvatar(), {Schema.Avatar.FOLDER_SOUNDS, CurrentAudio()})
    End Sub
    Private Sub LoadAudio_Completed() Handles LoadAudio_Thread.RunWorkerCompleted
        Audio.Play(CurrentAudio(), LoadAudio_Stream(), True, 0, 100, 0, 0, False)
    End Sub


    Private Sub Audio_Stop_Event() Handles Button_Audio_Stop.LeftClick
        Audio.Stop()
        Audio.Close()
    End Sub

    Private Sub Audio_Pause_Event() Handles Button_Audio_Pause.LeftClick
        Audio.Pause()
    End Sub

    Private Sub Audio_Add_Event() Handles Button_Audio_Add.LeftClick

        FolderPicker.InitialDirectory() = Folders.Athena()
        FolderPicker.Filter() = {"*" & Ext.WAV}

        Dim result = FolderPicker.Show()

        If result = FolderPicker.OK Then

            If Utility.IsEmpty(CurrentAvatar()) = False Then

                If Utility.DoesZipEntryExist(CurrentAddress(), {Schema.Avatar.FOLDER_SOUNDS, FolderPicker.SafeFileName()}) = False Then

                    Dim stream = Utility.FileReader(FolderPicker.FullFileName())
                    Utility.ZipWriter(CurrentAddress(), {Schema.Avatar.FOLDER_SOUNDS, FolderPicker.SafeFileName()}, stream, True)

                End If
            End If
        End If

        Audio_Load()
    End Sub

    Private Sub Audio_Delete_Event() Handles Button_Audio_Delete.LeftClick

        If Utility.IsEmpty(CurrentAudio()) = False Then

            Dim results = Message.Show(Me, "Are you sure you want to delete """ & CurrentAudio() & """ ?")
            If results = Message.OK Then

                Utility.DeleteZipEntry(AvatarFolder() & CurrentAvatar(), {Schema.Avatar.FOLDER_SOUNDS, CurrentAudio()})

            End If
        End If

        Audio_Load()
    End Sub

#End Region


#Region "Index"

    Private Sub Index_Load()
        If LoadIndex_Thread.IsBusy = False Then
            LoadIndex_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Property LoadIndex_Document() As New List(Of String)
    WithEvents LoadIndex_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadIndex_DoWork() Handles LoadIndex_Thread.DoWork
        Dim text = Utility.ZipReader(CurrentAddress(), Schema.Avatar.FILE_INDEX)
        LoadIndex_Document() = Utility.ToList(text, vbCrLf)
    End Sub
    Private Sub LoadIndex_Completed() Handles LoadIndex_Thread.RunWorkerCompleted

        SP_Index_Select.Clear()
        For Each line In LoadIndex_Document()
            Dim count = Utility.CharCount(line, AvatarX.DEF_PROPERTY_DELIM)

            If count = 5 Or count = 6 Then
                SP_Index_Select.Items.Add(line)
            End If
        Next

        SP_Index_Select.Index() = 0

        If LoadIndexImages_Thread.IsBusy = False Then
            LoadIndexImages_Thread.RunWorkerAsync()
        End If
    End Sub


    Private Property LoadIndex_Images() As New List(Of String)
    WithEvents LoadIndexImages_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadIndexImages_DoWork() Handles LoadIndexImages_Thread.DoWork
        Dim images = Utility.ZipGetFiles(CurrentAddress(), Schema.Avatar.FOLDER_TEXTURE, Ext.PNG, Gate.eEND)
        Dim filter As New List(Of String)

        For Each img In images
            img = Utility.GetFileName(img, False)
            Dim imgs = Utility.ToArray(img, AvatarX.DEF_NAME_DELIM)
            img = GetIndex(imgs, 0) & AvatarX.DEF_NAME_DELIM & GetIndex(imgs, 1)

            If Utility.Search(filter, img, Gate.eEQUALS) = False Then
                filter.Add(img)
                LoadIndex_Images().Add(img)
            End If
        Next
    End Sub
    Private Sub LoadIndexImages_Completed() Handles LoadIndexImages_Thread.RunWorkerCompleted
        SP_Index_ImagesSelect.Clear()

        For Each img In LoadIndex_Images()
            SP_Index_ImagesSelect.Items.Add(img)
        Next

        Index_Clear()
    End Sub


    Private Sub Index_Save()
        Index_Text() = ""
        For Each line In SP_Index_Select.Items
            Index_Text() &= line & vbCrLf
        Next

        If SaveIndex_Thread.IsBusy = False Then
            SaveIndex_Thread.RunWorkerAsync()
        End If

    End Sub

    Private Shared Property Index_Text() As String = ""
    WithEvents SaveIndex_Thread As New ComponentModel.BackgroundWorker
    Private Sub SaveIndex_DoWork() Handles SaveIndex_Thread.DoWork
        If Utility.IsEmpty(Index_Text()) = False Then
            Utility.ZipWriter(CurrentAddress(), Schema.Avatar.FILE_INDEX, Index_Text())
        End If
    End Sub
    Private Sub SaveIndex_Completed() Handles SaveIndex_Thread.RunWorkerCompleted
    End Sub


    Private Sub SP_Index_Select_Event() Handles SP_Index_Select.SelectedSpin
        Dim line = SP_Index_Select.Text()
        Dim properties = Utility.ToArray(line, AvatarX.DEF_PROPERTY_DELIM)

        If properties.Length() = 6 Then

            SP_Index_Side.Text() = GetIndex(properties, 0) 'Side
            TB_Index_Animation.Text() = GetIndex(properties, 1) 'Animation
            TB_Index_Profile.Text() = "null"
            Button_Index_IsLoop.Text() = GetIndex(properties, 2) 'Loop
            TB_Index_TickSkip.Text() = GetIndex(properties, 3) 'Speed
            TB_Index_ChanceTickSkip.Text() = GetIndex(properties, 4) 'Chance
            TB_Index_Images.Text() = GetIndex(properties, 5) 'files

        ElseIf properties.Length() = 7 Then

            SP_Index_Side.Text() = GetIndex(properties, 0) 'Side
            TB_Index_Animation.Text() = GetIndex(properties, 1) 'Animation
            TB_Index_Profile.Text() = GetIndex(properties, 2) 'Profile
            Button_Index_IsLoop.Text() = GetIndex(properties, 3) 'Loop
            TB_Index_TickSkip.Text() = GetIndex(properties, 4) 'Speed
            TB_Index_ChanceTickSkip.Text() = GetIndex(properties, 5) 'Chance
            TB_Index_Images.Text() = GetIndex(properties, 6) 'files

        End If


    End Sub

    Private Sub Numbers_Only(ByVal sender As TextBox, ByVal e As TextChangedEventArgs) Handles TB_Index_TickSkip.TextChanged,
                                                                                              TB_Index_ChanceTickSkip.TextChanged

        Dim i = sender.SelectionStart
        If Utility.IsInt(sender.Text) = False Then
            sender.Text = Utility.IntOnly(sender.Text)
            If i > 0 Then sender.SelectionStart = i - 1
        End If

    End Sub


    Private Function GetIndex(ByVal array As String(), ByVal index As Integer) As String
        Return Utility.Trim(Utility.GetIndex(array, index, ""))
    End Function

    Private Sub SP_Index_ImagesSelect_Event() Handles SP_Index_ImagesSelect.SelectedSpin
        Dim text = SP_Index_ImagesSelect.Text()

        If Utility.IsEmpty(TB_Index_Images.Text()) = False Then
            TB_Index_Images.Text() &= "," & text
        Else
            TB_Index_Images.Text() = text
        End If

    End Sub

    Private Sub Button_Index_Add_Event() Handles Button_Index_Add.LeftClick

        Dim Side = Utility.Trim(SP_Index_Side.Text())
        Dim Animation = Utility.Trim(TB_Index_Animation.Text())
        Dim Profile = Utility.Trim(TB_Index_Profile.Text())
        Dim IsLoop = Utility.Trim(Button_Index_IsLoop.Text())
        Dim TickSkip = Utility.Trim(TB_Index_TickSkip.Text())
        Dim Chance = Utility.Trim(TB_Index_ChanceTickSkip.Text())
        Dim Images = Utility.Trim(TB_Index_Images.Text())


        If Utility.IsEmpty(Side) = False And
           Utility.IsEmpty(Animation) = False And
           Utility.IsEmpty(IsLoop) = False And
           Utility.IsEmpty(TickSkip) = False And
           Utility.IsEmpty(Chance) = False And
           Utility.IsEmpty(Images) = False Then

            If Utility.IsEmpty(Profile) = False And Utility.Search(Profile, "null", Gate.eEQUALS) = False Then
                Dim line = Utility.Join({Side, Animation, Profile, IsLoop, TickSkip, Chance, Images}, AvatarX.DEF_PROPERTY_DELIM)
                SP_Index_Select.Items.Add(line)
            Else
                Dim line = Utility.Join({Side, Animation, IsLoop, TickSkip, Chance, Images}, AvatarX.DEF_PROPERTY_DELIM)
                SP_Index_Select.Items.Add(line)
            End If

        End If

    End Sub

    Private Sub Button_Index_Delete_Event() Handles Button_Index_Delete.LeftClick

        If Utility.IsEmpty(SP_Index_Select.Text()) = False Then
            SP_Index_Select.Remove(SP_Index_Select.Text, Gate.eEQUALS)
        End If

    End Sub

    Private Sub Buttom_Index_Edit_Event() Handles Button_Index_Edit.LeftClick
        If Utility.IsEmpty(SP_Index_Select.Text()) = False Then

            Dim Side = Utility.Trim(SP_Index_Side.Text())
            Dim Animation = Utility.Trim(TB_Index_Animation.Text())
            Dim Profile = Utility.Trim(TB_Index_Profile.Text())
            Dim IsLoop = Utility.Trim(Button_Index_IsLoop.Text())
            Dim TickSkip = Utility.Trim(TB_Index_TickSkip.Text())
            Dim Chance = Utility.Trim(TB_Index_ChanceTickSkip.Text())
            Dim Images = Utility.Trim(TB_Index_Images.Text())


            If Utility.IsEmpty(Side) = False And
               Utility.IsEmpty(Animation) = False And
               Utility.IsEmpty(IsLoop) = False And
               Utility.IsEmpty(TickSkip) = False And
               Utility.IsEmpty(Chance) = False And
               Utility.IsEmpty(Images) = False Then

                If Utility.IsEmpty(Profile) = False And Utility.Search(Profile, "null", Gate.eEQUALS) = False Then
                    Dim line = Utility.Join({Side, Animation, Profile, IsLoop, TickSkip, Chance, Images}, AvatarX.DEF_PROPERTY_DELIM)
                    SP_Index_Select.Text = line
                Else
                    Dim line = Utility.Join({Side, Animation, IsLoop, TickSkip, Chance, Images}, AvatarX.DEF_PROPERTY_DELIM)
                    SP_Index_Select.Text = line
                End If

            End If
        End If
    End Sub

    Private Sub Button_Index_IsLoop_Event() Handles Button_Index_IsLoop.LeftClick
        If Utility.Search(Button_Index_IsLoop().Text(), "true", Gate.eEQUALS) = True Then
            Button_Index_IsLoop.Text() = "false"
        Else
            Button_Index_IsLoop.Text() = "true"
        End If
    End Sub

    Private Sub Button_Index_Clear_Event() Handles Button_Index_Clear.LeftClick
        Index_Clear()
    End Sub

    Private Sub Index_Clear()
        SP_Index_Select.Index() = 0
        TB_Index_Animation.Text() = AvatarX.DEF_ANIMATION
        TB_Index_Profile.Text() = "null"
        Button_Index_IsLoop.Text() = "true"
        TB_Index_TickSkip.Text() = "1"
        TB_Index_ChanceTickSkip.Text() = "0"
        TB_Index_Images.Text() = ""
        SP_Index_ImagesSelect.Index() = 0
    End Sub

#End Region


#Region "Profile"

    Private Sub Profile_Load()
        If LoadProfile_Thread.IsBusy = False Then
            LoadProfile_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Shared Property LoadProfile_Document() As New List(Of String)
    WithEvents LoadProfile_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadProfile_DoWork() Handles LoadProfile_Thread.DoWork
        Dim text = Utility.ZipReader(CurrentAddress(), {Schema.Avatar.FOLDER_PROFILES, CurrentProfile()})
        LoadProfile_Document() = Utility.ToList(text, AvatarX.DEF_ANIMATE_DELIM)
    End Sub
    Private Sub LoadProfile_Completed() Handles LoadProfile_Thread.RunWorkerCompleted

        For Each line In LoadProfile_Document()
            Dim props = Utility.ToArray(line, AvatarX.DEF_PROPERTY_DELIM)
            props = Utility.Trim(props)

            If props.Count() = 2 Then
                Dim CMD = Trim(props(0))
                Dim ARG = Trim(props(1))
                Dim args = Utility.ToArray(ARG, AvatarX.DEF_ARGS_DELIM)
                args = Utility.Trim(args)

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_ORIGIN, Gate.eEQUALS) = True Then
                    If args.Count() = 3 Then
                        IN_Profile_Origin.Return_X = ToDbl(args(0))
                        IN_Profile_Origin.Return_Y() = ToDbl(args(1))
                        IN_Profile_Origin.Return_Z() = ToDbl(args(2))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_INPUT, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Input.Return_X() = ToDbl(args(0))
                        IN_Profile_Input.Return_Y() = ToDbl(args(1))
                        IN_Profile_Input.Return_W() = ToDbl(args(2))
                        IN_Profile_Input.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_PROMPT, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Prompt.Return_X() = ToDbl(args(0))
                        IN_Profile_Prompt.Return_Y() = ToDbl(args(1))
                        IN_Profile_Prompt.Return_W() = ToDbl(args(2))
                        IN_Profile_Prompt.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_STATUS, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Status.Return_X() = ToDbl(args(0))
                        IN_Profile_Status.Return_Y() = ToDbl(args(1))
                        IN_Profile_Status.Return_W() = ToDbl(args(2))
                        IN_Profile_Status.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_MENU, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Menu.Return_X() = ToDbl(args(0))
                        IN_Profile_Menu.Return_Y() = ToDbl(args(1))
                        IN_Profile_Menu.Return_W() = ToDbl(args(2))
                        IN_Profile_Menu.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GRIP, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Grip.Return_X() = ToDbl(args(0))
                        IN_Profile_Grip.Return_Y() = ToDbl(args(1))
                        IN_Profile_Grip.Return_W() = ToDbl(args(2))
                        IN_Profile_Grip.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_WINDOW, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        IN_Profile_Window.Return_X() = ToDbl(args(0))
                        IN_Profile_Window.Return_Y() = ToDbl(args(1))
                        IN_Profile_Window.Return_W() = ToDbl(args(2))
                        IN_Profile_Window.Return_H() = ToDbl(args(3))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_FONTFAMILY, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        SP_Profile_Font.Text() = args(0)
                    End If

                End If

                'Colors
                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_FONTCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_FontColor.Background = ToBrush(args)
                        FontColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_FONTBACKCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_FontBackColor.Background = ToBrush(args)
                        FontBackColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_WINDOWCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_WindowColor.Background = ToBrush(args)
                        WindowColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_AICOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_AiColor.Background = ToBrush(args)
                        AiColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_USERCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_UserColor.Background = ToBrush(args)
                        UserColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_HOVERCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_HoverColor.Background = ToBrush(args)
                        HoverColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_BORDERCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_BorderColor.Background = ToBrush(args)
                        BorderColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_STATUSCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_StatusColor.Background = ToBrush(args)
                        StatusColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_ERRORCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Profile_ErrorColor.Background = ToBrush(args)
                        ErrorColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_GLOBAL_FONTSIZE, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        SP_Profile_FontSize.Base() = ToDbl(args(0))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SHOWAVATAR, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        Button_Profile_ShowAvatar().Text() = args(0)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SHOWCANVAS, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        Button_Profile_ShowCanvas().Text() = args(0)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SUSPENDSCRIPT, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        Button_Profile_SuspendScript().Text() = args(0)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SUSPEND, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        Button_Profile_Suspend().Text() = args(0)
                    End If
                End If

                'script
                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_ATTRIBUTECOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_AttributeColor.Background = ToBrush(args)
                        AttributeColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_BACKCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_BackColor.Background = ToBrush(args)
                        ScriptBackColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_CLASSCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_ClassColor.Background = ToBrush(args)
                        ClassColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_COMMENTCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_CommentColor.Background = ToBrush(args)
                        CommentColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_FONTCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_FontColor.Background = ToBrush(args)
                        ScriptFontColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_KEYWORDCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_KeyWordColor.Background = ToBrush(args)
                        KeywordColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_NUMBERCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_NumberColor.Background = ToBrush(args)
                        NumberColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_STRINGCOLOR, Gate.eEQUALS) = True Then
                    If args.Count() = 4 Then
                        Button_Script_StringColor.Background = ToBrush(args)
                        StringColor() = ToColor(args)
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_FONTSIZE, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        SP_Script_FontSize.Base() = ToDbl(args(0))
                    End If
                End If

                If Utility.Search(CMD, AvatarX.Textures.Profile.DEF_SCRIPT_FONT, Gate.eEQUALS) = True Then
                    If args.Count() = 1 Then
                        SP_Script_Font.Text() = args(0)
                    End If
                End If



            End If
        Next

    End Sub


    Private Shared Property SaveProfile_Document() As New List(Of String)
    Private Sub Profile_Save()
        Dim ARGD As String = AvatarX.DEF_ARGS_DELIM
        Dim PROPD As String = AvatarX.DEF_PROPERTY_DELIM


        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_ORIGIN, PROPD, IN_Profile_Origin.Return_X,
                                    ARGD, IN_Profile_Origin.Return_Y, ARGD, IN_Profile_Origin.Return_Z}))

        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SHOWAVATAR, PROPD, Button_Profile_ShowAvatar.Text}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SHOWCANVAS, PROPD, Button_Profile_ShowCanvas.Text}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SUSPENDSCRIPT, PROPD, Button_Profile_SuspendScript.Text}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SUSPEND, PROPD, Button_Profile_Suspend.Text}))


        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_INPUT, PROPD, IN_Profile_Input.Return_X, ARGD, IN_Profile_Input.Return_Y,
                                    ARGD, IN_Profile_Input.Return_W, ARGD, IN_Profile_Input.Return_H}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_PROMPT, PROPD, IN_Profile_Prompt.Return_X, ARGD, IN_Profile_Prompt.Return_Y,
                                    ARGD, IN_Profile_Prompt.Return_W, ARGD, IN_Profile_Prompt.Return_H}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_STATUS, PROPD, IN_Profile_Status.Return_X, ARGD, IN_Profile_Status.Return_Y,
                                    ARGD, IN_Profile_Status.Return_W, ARGD, IN_Profile_Status.Return_H}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_MENU, PROPD, IN_Profile_Menu.Return_X, ARGD, IN_Profile_Menu.Return_Y,
                                    ARGD, IN_Profile_Menu.Return_W, ARGD, IN_Profile_Menu.Return_H}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GRIP, PROPD, IN_Profile_Grip.Return_X, ARGD, IN_Profile_Grip.Return_Y,
                                    ARGD, IN_Profile_Grip.Return_W, ARGD, IN_Profile_Grip.Return_H}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_WINDOW, PROPD, IN_Profile_Window.Return_X, ARGD, IN_Profile_Window.Return_Y,
                                    ARGD, IN_Profile_Window.Return_W, ARGD, IN_Profile_Window.Return_H}))


        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_FONTFAMILY, PROPD, SP_Profile_Font.Text}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_FONTSIZE, PROPD, SP_Profile_FontSize.Text}))


        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_FONTCOLOR, PROPD, FontColor.A, ARGD, FontColor.R,
                                    ARGD, FontColor.G, ARGD, FontColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_HOVERCOLOR, PROPD, HoverColor.A, ARGD, HoverColor.R,
                                   ARGD, HoverColor.G, ARGD, FontColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_FONTBACKCOLOR, PROPD, FontBackColor.A, ARGD, FontBackColor.R,
                                    ARGD, FontBackColor.G, ARGD, FontBackColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_WINDOWCOLOR, PROPD, WindowColor.A, ARGD, WindowColor.R,
                                    ARGD, WindowColor.G, ARGD, WindowColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_AICOLOR, PROPD, AiColor.A, ARGD, AiColor.R,
                                    ARGD, AiColor.G, ARGD, AiColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_USERCOLOR, PROPD, UserColor.A, ARGD, UserColor.R,
                                    ARGD, UserColor.G, ARGD, UserColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_GLOBAL_BORDERCOLOR, PROPD, BorderColor.A, ARGD, BorderColor.R,
                                    ARGD, BorderColor.G, ARGD, BorderColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_STATUSCOLOR, PROPD, StatusColor.A, ARGD, StatusColor.R,
                                    ARGD, StatusColor.G, ARGD, StatusColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_ERRORCOLOR, PROPD, ErrorColor.A, ARGD, ErrorColor.R,
                                    ARGD, ErrorColor.G, ARGD, ErrorColor.B}))



        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_ATTRIBUTECOLOR, PROPD, AttributeColor.A, ARGD, AttributeColor.R,
                                    ARGD, AttributeColor.G, ARGD, AttributeColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_BACKCOLOR, PROPD, ScriptBackColor.A, ARGD, ScriptBackColor.R,
                                    ARGD, ScriptBackColor.G, ARGD, ScriptBackColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_CLASSCOLOR, PROPD, ClassColor.A, ARGD, ClassColor.R,
                                    ARGD, ClassColor.G, ARGD, ClassColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_COMMENTCOLOR, PROPD, CommentColor.A, ARGD, CommentColor.R,
                                    ARGD, CommentColor.G, ARGD, CommentColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_KEYWORDCOLOR, PROPD, KeywordColor.A, ARGD, KeywordColor.R,
                                    ARGD, KeywordColor.G, ARGD, KeywordColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_NUMBERCOLOR, PROPD, NumberColor.A, ARGD, NumberColor.R,
                                    ARGD, NumberColor.G, ARGD, NumberColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_STRINGCOLOR, PROPD, StringColor.A, ARGD, StringColor.R,
                                    ARGD, StringColor.G, ARGD, StringColor.B}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_FONTCOLOR, PROPD, ScriptFontColor.A, ARGD, ScriptFontColor.R,
                                    ARGD, ScriptFontColor.G, ARGD, ScriptFontColor.B}))

        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_FONT, PROPD, SP_Script_Font.Text}))
        SaveProfile_Document.Add(Utility.Concat({AvatarX.Textures.Profile.DEF_SCRIPT_FONTSIZE, PROPD, SP_Script_FontSize.Text}))


        If SaveProfile_Thread.IsBusy = False Then
            SaveProfile_Thread.RunWorkerAsync()
        End If

    End Sub

    WithEvents SaveProfile_Thread As New ComponentModel.BackgroundWorker
    Private Sub SaveProfile_DoWork() Handles SaveProfile_Thread.DoWork
        Utility.ZipWriter(CurrentAddress(), {Schema.Avatar.FOLDER_PROFILES, CurrentProfile()}, Utility.Join(SaveProfile_Document, vbCrLf))
    End Sub
    Private Sub SaveProfile_Completed() Handles SaveProfile_Thread.RunWorkerCompleted
    End Sub


    Private Shared Function ToDbl(ByVal toDbls As String) As Double
        Return Utility.ToDbl(toDbls, 0)
    End Function

    Private Shared Function ToByte(ByVal toDbls As String) As Double
        Return Utility.ToByte(toDbls, 0)
    End Function

    Private Shared Function ToBool(ByVal toBools As String) As Boolean
        Return Utility.ToBool(toBools)
    End Function

    Private Shared Function ToBrush(ByVal args As String()) As Brush
        Return New SolidColorBrush(Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3))))
    End Function

    Private Shared Function ToColor(ByVal args As String()) As Color
        Return Color.FromArgb(ToByte(args(0)), ToByte(args(1)), ToByte(args(2)), ToByte(args(3)))
    End Function



    Private Sub Button_Profile_ShowAvatar_Event() Handles Button_Profile_ShowAvatar.LeftClick
        If Utility.Search(Button_Profile_ShowAvatar().Text(), "true", Gate.eEQUALS) = True Then
            Button_Profile_ShowAvatar.Text() = "false"
        Else
            Button_Profile_ShowAvatar.Text() = "true"
        End If
    End Sub

    Private Sub Button_Profile_ShowCanvas_Event() Handles Button_Profile_ShowCanvas.LeftClick
        If Utility.Search(Button_Profile_ShowCanvas().Text(), "true", Gate.eEQUALS) = True Then
            Button_Profile_ShowCanvas.Text() = "false"
        Else
            Button_Profile_ShowCanvas.Text() = "true"
        End If
    End Sub

    Private Sub Button_Profile_Suspend_Event() Handles Button_Profile_Suspend.LeftClick
        If Utility.Search(Button_Profile_Suspend().Text(), "true", Gate.eEQUALS) = True Then
            Button_Profile_Suspend.Text() = "false"
        Else
            Button_Profile_Suspend.Text() = "true"
        End If
    End Sub

    Private Sub Button_Profile_SuspendScript_Event() Handles Button_Profile_SuspendScript.LeftClick
        If Utility.Search(Button_Profile_SuspendScript().Text(), "true", Gate.eEQUALS) = True Then
            Button_Profile_SuspendScript.Text() = "false"
        Else
            Button_Profile_SuspendScript.Text() = "true"
        End If
    End Sub

    Private Property AiColor() As Color = Colors.Black
    Private Sub Button_Profile_AiColor_Event() Handles Button_Profile_AiColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_AiColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            AiColor() = ColorPicker.CurrentColor()
            Button_Profile_AiColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property BorderColor() As Color
    Private Sub Button_Profile_BorderColor_Event() Handles Button_Profile_BorderColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_BorderColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            BorderColor() = ColorPicker.CurrentColor()
            Button_Profile_BorderColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property ErrorColor() As Color
    Private Sub Button_Profile_ErrorColor_Event() Handles Button_Profile_ErrorColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_ErrorColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            ErrorColor() = ColorPicker.CurrentColor()
            Button_Profile_ErrorColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property FontColor() As Color
    Private Sub Button_Profile_FontColor_Event() Handles Button_Profile_FontColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_FontColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            FontColor() = ColorPicker.CurrentColor()
            Button_Profile_FontColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property HoverColor() As Color
    Private Sub Button_Profile_HoverColor_Event() Handles Button_Profile_HoverColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_HoverColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            HoverColor() = ColorPicker.CurrentColor()
            Button_Profile_HoverColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property FontBackColor() As Color
    Private Sub Button_Profile_FontBackColor_Event() Handles Button_Profile_FontBackColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_FontBackColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            FontBackColor() = ColorPicker.CurrentColor()
            Button_Profile_FontBackColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub


    Private Property StatusColor() As Color
    Private Sub Button_Profile_StatusColor_Event() Handles Button_Profile_StatusColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_StatusColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            StatusColor() = ColorPicker.CurrentColor()
            Button_Profile_StatusColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property UserColor() As Color
    Private Sub Button_Profile_UserColor_Event() Handles Button_Profile_UserColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_UserColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            UserColor() = ColorPicker.CurrentColor()
            Button_Profile_UserColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property WindowColor() As Color
    Private Sub Button_Profile_WindowColor_Event() Handles Button_Profile_WindowColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Profile_WindowColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            WindowColor() = ColorPicker.CurrentColor()
            Button_Profile_WindowColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub



    Private Property ScriptFontColor() As Color = Colors.Black
    Private Sub Button_Script_FontColor_Event() Handles Button_Script_FontColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_FontColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            ScriptFontColor() = ColorPicker.CurrentColor()
            Button_Script_FontColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property ScriptBackColor() As Color
    Private Sub Button_Script_BackColor_Event() Handles Button_Script_BackColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_BackColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            ScriptBackColor() = ColorPicker.CurrentColor()
            Button_Script_BackColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property NumberColor() As Color
    Private Sub Button_Script_NumberColor_Event() Handles Button_Script_NumberColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_NumberColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            NumberColor() = ColorPicker.CurrentColor()
            Button_Script_NumberColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property StringColor() As Color
    Private Sub Button_Script_StringColor_Event() Handles Button_Script_StringColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_StringColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            StringColor() = ColorPicker.CurrentColor()
            Button_Script_StringColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property CommentColor() As Color
    Private Sub Button_Script_CommentColor_Event() Handles Button_Script_CommentColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_CommentColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            CommentColor() = ColorPicker.CurrentColor()
            Button_Script_CommentColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property KeywordColor() As Color
    Private Sub Button_Script_KeyWordColor_Event() Handles Button_Script_KeyWordColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_KeyWordColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            KeywordColor() = ColorPicker.CurrentColor()
            Button_Script_KeyWordColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property AttributeColor() As Color
    Private Sub Button_Script_AttributeColor_Event() Handles Button_Script_AttributeColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_AttributeColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            AttributeColor() = ColorPicker.CurrentColor()
            Button_Script_AttributeColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub

    Private Property ClassColor() As Color
    Private Sub Button_Script_ClassColor_Event() Handles Button_Script_ClassColor.LeftClick
        ColorPicker.Size() = New Size(400, 300)
        Dim center = Local.FindCenter(New Point(Left, Top), New Size(Width, Height), ColorPicker.Size())
        ColorPicker.Location = center
        ColorPicker.InitialBrush = Button_Script_ClassColor.Background

        Dim out = ColorPicker.Show()

        If out = ColorPicker.OK Then
            ClassColor() = ColorPicker.CurrentColor()
            Button_Script_ClassColor.Background = ColorPicker.CurrentBrush()
        End If
    End Sub




#End Region


#Region "Texture"


    Private Shared Property GeneralMemory() As TypeMemory = AvatarX.Textures.LocalResources()
    Private Shared Property LocalMemory() As New TypeMemory
    Private Shared Property CurrentTextureFile() As String = ""

    Private Sub Texture_Load()
        LocalMemory().Clear()
        ImageIndex() = 1
        CurrentTextureFile() = ""

        For Each Item In AvatarX.Textures.LocalResources().Items
            If Utility.Search(Item.Index, Utility.Join({Schema.Avatar.FOLDER_TEXTURE, CurrentAnimationFolder()}, AvatarX.DEF_ADDRESS_DELIM) &
                              AvatarX.DEF_ADDRESS_DELIM, Gate.eSTART) = True Then
                LocalMemory().Items.Add(Item)
            End If
        Next

        GImages()
    End Sub


    Private Shared Property CurrentImage() As System.Drawing.Bitmap = Bitmaps.CreateBitmap(256, 256, Colors.Red)
    WithEvents GetImage_Thread As New ComponentModel.BackgroundWorker
    Private Sub GetImage_DoWork() Handles GetImage_Thread.DoWork

        For Each Item In LocalMemory().Items
            Dim file_name = Utility.GetFileName(Item.Index, False)
            Dim file_array = Utility.ToArray(file_name, AvatarX.DEF_NAME_DELIM)
            Dim face = Utility.GetIndex(file_array, 0, AvatarX.DEF_FACE)
            Dim iIndex = Utility.GetIndex(file_array, 2, "0")

            If Utility.Search(iIndex, Utility.ToStr(ImageIndex()), Gate.eEQUALS) = True Then

                Dim bmi = Bitmaps.GetBitmapFromBytes(Item.Entry)

                'rotateImages
                If Utility.Search(face, {AvatarX.DEF_FACE, AvatarX.DEF_BACK}, Gate.eEQUALS) = True Then
                    Bitmaps.FlipBitmap(bmi, System.Drawing.RotateFlipType.RotateNoneFlipY)
                End If

                If Utility.Search(face, {AvatarX.DEF_RIGHT, AvatarX.DEF_LEFT}, Gate.eEQUALS) = True Then
                    Bitmaps.FlipBitmap(bmi, System.Drawing.RotateFlipType.RotateNoneFlipXY)
                End If

                CurrentTextureFile() = file_name
                CurrentImage() = bmi
                Exit For
            End If
        Next
    End Sub
    Private Sub GetImage_Completed() Handles GetImage_Thread.RunWorkerCompleted
        LB_Texture_FrameIndex.Content = ImageIndex()
        LB_CurrentFile.Content() = CurrentTextureFile()
        IMG_Canvas.Source = Bitmaps.BitmapToBitmapImage(CurrentImage())
    End Sub


    Private Shared Property ImageIndex() As Integer = 1
    Private Sub Button_Texture_Forward_Event() Handles Button_Texture_Forward.LeftClick
        ImageIndex() += 1
        If ImageIndex() < 1 Then ImageIndex() = LocalMemory.Count()
        If ImageIndex() > LocalMemory.Count() Then ImageIndex() = 1

        GImages()
    End Sub

    Private Sub Button_Texture_Backward_Event() Handles Button_Texture_Backward.LeftClick
        ImageIndex() -= 1
        If ImageIndex() < 1 Then ImageIndex() = LocalMemory.Count()
        If ImageIndex() > LocalMemory.Count() Then ImageIndex() = 1

        GImages()
    End Sub

    Private Sub GImages()

        If GetImage_Thread.IsBusy() = False Then
            GetImage_Thread.RunWorkerAsync()
        End If
    End Sub

#End Region


#Region "Script"

    Private Sub Script_Load()
        If ScriptLoad_Thread.IsBusy = False Then
            ScriptLoad_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Shared Property ScriptLoadText() As String = ""
    WithEvents ScriptLoad_Thread As New ComponentModel.BackgroundWorker
    Private Sub ScriptLoad_DoWork() Handles ScriptLoad_Thread.DoWork
        ScriptLoadText() = Utility.ZipReader(CurrentAddress(), {Schema.Avatar.FOLDER_SCRIPTS, CurrentScript()})
    End Sub
    Private Sub ScriptLoad_Completed() Handles ScriptLoad_Thread.RunWorkerCompleted
        FastScript_Editor.Dispatcher.BeginInvoke(Sub() FastScript_Editor.Text() = ScriptLoadText()
                                             )
    End Sub

#End Region


    Private Sub Select_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Object)) Handles Main_TreeView.SelectedItemChanged

        If e.NewValue IsNot Nothing Then
            Main_Scroll.ScrollToTop()
            LB_CurrentFile.Content() = ""

            Dim header As String = ""
            Dim avatar As String = ""
            Dim files_1 As String = ""
            Dim files_2 As String = ""

            Try
                header = TryCast(e.NewValue.Parent.Parent.Parent, TreeViewItem).Header
                avatar = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                files_1 = TryCast(e.NewValue.Parent, TreeViewItem).Header
                files_2 = TryCast(e.NewValue, TreeViewItem).Header
            Catch

                Try
                    header = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                    avatar = TryCast(e.NewValue.Parent, TreeViewItem).Header
                    files_1 = TryCast(e.NewValue, TreeViewItem).Header
                Catch

                    Try
                        header = TryCast(e.NewValue.Parent, TreeViewItem).Header
                        avatar = TryCast(e.NewValue, TreeViewItem).Header
                    Catch
                    End Try
                End Try
            End Try

            CurrentAvatar() = avatar

            If Utility.Search(files_1, Schema.Avatar.FILE_INDEX, Gate.eEQUALS) = True Then
                Grid_Audio.Visibility = Visibility.Collapsed
                Grid_Index.Visibility = Visibility.Visible
                Grid_Profile.Visibility = Visibility.Collapsed
                Grid_Script.Visibility = Visibility.Collapsed
                Grid_Texture.Visibility = Visibility.Collapsed
                Border_Texture_Menu.Visibility = Visibility.Collapsed
                LB_CurrentFile.Content() = Schema.Avatar.FILE_INDEX
                Index_Load()

                IsIndexOpen() = True
                IsAudioOpen() = False
                IsProfileOpen() = False
                IsScriptOpen() = False
                IsTextureOpen() = False
            End If

            If Utility.Search(files_1, Schema.Avatar.FOLDER_SOUNDS, Gate.eEQUALS) = True Then
                Grid_Audio.Visibility = Visibility.Visible
                Grid_Index.Visibility = Visibility.Collapsed
                Grid_Profile.Visibility = Visibility.Collapsed
                Grid_Script.Visibility = Visibility.Collapsed
                Grid_Texture.Visibility = Visibility.Collapsed
                Border_Texture_Menu.Visibility = Visibility.Collapsed

                If Utility.Search(files_2, Ext.WAV, Gate.eEND) = True Then
                    CurrentAudio() = files_2
                    LB_CurrentFile.Content() = files_2
                    Audio_Load()

                    IsIndexOpen() = False
                    IsAudioOpen() = True
                    IsProfileOpen() = False
                    IsScriptOpen() = False
                    IsTextureOpen() = False
                End If
            End If

            If Utility.Search(files_1, Schema.Avatar.FOLDER_PROFILES, Gate.eEQUALS) = True Then
                Grid_Audio.Visibility = Visibility.Collapsed
                Grid_Index.Visibility = Visibility.Collapsed
                Grid_Profile.Visibility = Visibility.Visible
                Grid_Script.Visibility = Visibility.Collapsed
                Grid_Texture.Visibility = Visibility.Collapsed
                Border_Texture_Menu.Visibility = Visibility.Collapsed

                If Utility.Search(files_2, Ext.PRO, Gate.eEND) = True Then
                    CurrentProfile() = files_2
                    LB_CurrentFile.Content() = files_2
                    Profile_Load()

                    IsIndexOpen() = False
                    IsAudioOpen() = False
                    IsProfileOpen() = True
                    IsScriptOpen() = False
                    IsTextureOpen() = False
                End If
            End If

            If Utility.Search(files_1, Schema.Avatar.FOLDER_SCRIPTS, Gate.eEQUALS) = True Then
                Grid_Audio.Visibility = Visibility.Collapsed
                Grid_Index.Visibility = Visibility.Collapsed
                Grid_Profile.Visibility = Visibility.Collapsed
                Grid_Script.Visibility = Visibility.Visible
                Grid_Texture.Visibility = Visibility.Collapsed
                Border_Texture_Menu.Visibility = Visibility.Collapsed

                If Utility.Search(files_2, Ext.CUB, Gate.eEND) = True Then
                    CurrentScript() = files_2
                    LB_CurrentFile.Content() = files_2
                    Script_Load()

                    IsIndexOpen() = False
                    IsAudioOpen() = False
                    IsProfileOpen() = False
                    IsScriptOpen() = True
                    IsTextureOpen() = False
                End If
            End If

            If Utility.Search(files_1, Schema.Avatar.FOLDER_TEXTURE, Gate.eEQUALS) = True Then
                Grid_Audio.Visibility = Visibility.Collapsed
                Grid_Index.Visibility = Visibility.Collapsed
                Grid_Profile.Visibility = Visibility.Collapsed
                Grid_Script.Visibility = Visibility.Collapsed
                Grid_Texture.Visibility = Visibility.Visible
                Border_Texture_Menu.Visibility = Visibility.Visible

                If Utility.IsEmpty(files_2) = False Then
                    CurrentAnimationFolder() = files_2
                    Texture_Load()

                    IsIndexOpen() = False
                    IsAudioOpen() = False
                    IsProfileOpen() = False
                    IsScriptOpen() = False
                    IsTextureOpen() = True
                End If
            End If
        End If

    End Sub


    Private Sub Load_TreeView()
        If Load_TreeView_Thread.IsBusy = False Then
            Load_TreeView_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Sub Load_TreeView_InThread()
        Main_TreeView.Items.Clear()

        Dim Main_Head As New TreeViewItem
        Main_Head.Header = "Avatars"

        Schema.SetStyle(Main_Head)
        Main_Head.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head)


        Dim files = Utility.GetAllFiles(AvatarFolder())

        For Each file_1 In files

            If Utility.Search(file_1, {"*" & Ext.AVATAR, "*" & Ext.ZIP}, Gate.eEQUALS) = True Then

                Dim main_folders As New TreeViewItem
                main_folders.Header = Utility.GetFileName(file_1, False)
                Schema.SetStyle(main_folders)
                main_folders.IsExpanded = True
                Main_Head.Items.Add(main_folders)


                Dim sub_folders = Utility.ZipGetFolders(file_1, {})
                For Each folders In sub_folders

                    Dim folder_set As New TreeViewItem
                    folder_set.Header = folders
                    Schema.SetStyle(folder_set)
                    folder_set.IsExpanded = False
                    main_folders.Items.Add(folder_set)


                    If Utility.Search(folders, Schema.Avatar.FILE_INDEX, Gate.eEQUALS) = False Then
                        If Utility.Search(folders, Schema.Avatar.FOLDER_TEXTURE, Gate.eEQUALS) = False Then

                            Dim sub_files = Utility.ZipGetFiles(file_1, {folders})
                            For Each file_2 In sub_files
                                Dim file_set As New TreeViewItem
                                file_set.Header = file_2
                                Schema.SetStyle(file_set)
                                file_set.IsExpanded = False
                                folder_set.Items.Add(file_set)
                            Next


                        Else

                            Dim sub_files = Utility.ZipGetFolders(file_1, {folders})
                            For Each file_2 In sub_files
                                Dim file_set As New TreeViewItem
                                file_set.Header = file_2
                                Schema.SetStyle(file_set)
                                file_set.IsExpanded = False
                                folder_set.Items.Add(file_set)
                            Next

                        End If

                    End If

                Next

            End If
        Next

    End Sub

    WithEvents Load_TreeView_Thread As New ComponentModel.BackgroundWorker
    Private Sub Load_TreeView_DoWork() Handles Load_TreeView_Thread.DoWork
        Main_TreeView.Dispatcher.BeginInvoke(Sub() Load_TreeView_InThread())
    End Sub
    Private Sub Load_TreeView_Completed() Handles Load_TreeView_Thread.RunWorkerCompleted
    End Sub



End Class
