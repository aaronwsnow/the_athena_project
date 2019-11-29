Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

Public Class OptionEditor

    Private Sub Initialize() Handles Me.Initialized
        Me.Icon() = LocalResources.GetIcon()

        Main_Grid.Background = Schema.Global_Window_Color()

        Schema.SetStyle(LB_Avatar)
        Schema.SetStyle(LB_Brain)
        Schema.SetStyle(LB_Cluster)
        Schema.SetStyle(LB_Word)
        Schema.SetStyle(LB_Defaults)
        Schema.SetStyle(LB_Package)
        Schema.SetStyle(LB_Plugins)
        Schema.SetStyle(LB_Profiles)
        Schema.SetStyle(LB_Profile)
        Schema.SetStyle(LB_Ai)
        Schema.SetStyle(LB_Voice)
        Schema.SetStyle(LB_VoiceSpeed)
        Schema.SetStyle(LB_Script)
        Schema.SetStyle(LB_AvatarDPI)
        Schema.SetStyle(LB_CanvasDPI)

        Schema.SetStyle(TB_Profile)
        Schema.SetStyle(TB_Ai)
        Schema.SetStyle(Border_Options)

        PS_Plugins.Fonts() = Schema.Normal_Fonts()

    End Sub


    Private Sub Load() Handles Me.Loaded


        For Each folder In Utility.GetAllFolders(Folders.Athena(), False)
            folder = Utility.GetFolderName(folder)
            SP_Package.Items.Add(folder)
        Next

        For Each file In Utility.GetAllFiles(Folders.Avatars())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.AVATAR}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Avatar.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Brains())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.BRAIN}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Brain.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Databases())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.DATA}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Cluster.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Word())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.WORD}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Word.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Plugins())
            If Utility.Search(file, {"*" & Ext.TEXT, "*" & Ext.PLUG}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                PS_Plugins.PluginList().Add(file)
            End If
        Next

        For Each voice In Speech.Voices
            SP_Voices.Items.Add(voice)
        Next

        'Schema.Current_Profile() is defaulted to 'Standard'
        Schema.Load()

        PS_Plugins.PluginsToCheck() = Schema.Current_Plugins()
        PS_Plugins.Load()

        For Each pro In Schema.Profiles()
            SP_Profiles.Items.Add(pro)
        Next

        Dim scripts = Utility.ZipGetFiles(Schema.Current_Avatar(), {Schema.Avatar.FOLDER_SCRIPTS},
                                          "*" & Ext.CUB, Gate.eEND)
        SP_Script.Items.Add("no script")
        For Each scr In scripts
            SP_Script.Items.Add(scr)
        Next


        TB_Profile.Text() = Schema.Current_Profile()
        SP_Profiles.SelectText() = Schema.Current_Profile()
        TB_Ai.Text() = Schema.Current_Ai()

        SP_Package.SelectText() = Schema.Current_Package()
        SP_Avatar.SelectText() = Schema.Current_Avatar(True)
        SP_Brain.SelectText() = Schema.Current_Brain(True)
        SP_Cluster.SelectText() = Schema.Current_Cluster(True)
        SP_Word.SelectText() = Schema.Current_Word(True)

        SP_Script.SelectText() = Schema.Current_Script()
        SP_Voices.Index() = Schema.Current_Voice()
        SP_VoiceSpeed.SelectText() = Schema.Current_Voice_Speed()
        SP_AvatarDPI.Base() = Schema.Current_Avatar_DPI()
        SP_CanvasDPI.Base() = Schema.Current_Canvas_DPI()


    End Sub

    Private Sub Unloading() Handles Me.Unloaded
        Dim out = Message.Show(Me, "Do you want to Save?")

        If out = Message.OK Then
            Schema.Save()
        End If
    End Sub


    Private Sub CB_Profiles_Event() Handles SP_Profiles.Spins
        If Utility.IsEmpty(SP_Profiles.Text) = False Then

            Dim newProfile = SP_Profiles.Text
            Schema.Current_Profile() = newProfile
            Schema.Load(newProfile)
            TB_Profile.Text() = newProfile

            TB_Ai.Text() = Schema.Current_Ai()
            SP_Package.SelectText() = Schema.Current_Package()
            SP_Avatar.SelectText() = Schema.Current_Avatar(True)
            SP_Brain.SelectText() = Schema.Current_Brain(True)
            SP_Cluster.SelectText() = Schema.Current_Cluster(True)
            SP_Word.SelectText() = Schema.Current_Word(True)
            SP_Script.SelectText() = Schema.Current_Script()
            SP_Voices.Index() = Schema.Current_Voice()
            SP_VoiceSpeed.SelectText() = Schema.Current_Voice_Speed()
            SP_CanvasDPI.Base() = Schema.Current_Canvas_DPI()
            SP_AvatarDPI.Base() = Schema.Current_Avatar_DPI()

            PS_Plugins.PluginsToCheck() = Schema.Current_Plugins()
            PS_Plugins.Load()
        End If
    End Sub

    Private Sub SP_Avatar_Event() Handles SP_Avatar.Spins
        If Utility.IsEmpty(SP_Avatar.Text) = False Then
            Schema.Current_Avatar = SP_Avatar.Text

            SP_Script.Clear()
            Dim scripts = Utility.ZipGetFiles(Schema.Current_Avatar(), {Schema.Avatar.FOLDER_SCRIPTS},
                                              "*" & Ext.CUB, Gate.eEND)
            SP_Script.Items.Add("no script")
            For Each scr In scripts
                SP_Script.Items.Add(scr)
            Next
        End If
    End Sub

    Private Sub SP_Package_Event() Handles SP_Package.Spins

        If Utility.IsEmpty(SP_Package.Text) = False Then
            Schema.Current_Package() = SP_Package.Text
            Folders.Package() = SP_Package.Text
        End If

        SP_Brain.Clear()
        SP_Avatar.Clear()
        SP_Cluster.Clear()
        SP_Word.Clear()
        PS_Plugins.Clear()

        For Each file In Utility.GetAllFiles(Folders.Avatars())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.AVATAR}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Avatar.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Brains())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.BRAIN}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Brain.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Databases())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.DATA}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Cluster.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Word())
            If Utility.Search(file, {"*" & Ext.ZIP, "*" & Ext.WORD}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                SP_Word.Items.Add(file)
            End If
        Next

        For Each file In Utility.GetAllFiles(Folders.Plugins())
            If Utility.Search(file, {"*" & Ext.TEXT, "*" & Ext.PLUG}, Gate.eEQUALS) = True Then
                file = Utility.GetFileName(file, False)
                PS_Plugins.PluginList().Add(file)
            End If
        Next

        PS_Plugins.PluginsToCheck() = Schema.Current_Plugins()
        PS_Plugins.Load()

        SP_Avatar.SelectText(Gate.eSEARCH) = "standard"
        SP_Brain.SelectText(Gate.eSEARCH) = "standard"
        SP_Cluster.SelectText(Gate.eSEARCH) = "standard"
        SP_Word.SelectText(Gate.eSEARCH) = "standard"

    End Sub

    Private Sub SP_Brain_Event() Handles SP_Brain.Spins
        If Utility.IsEmpty(SP_Brain.Text) = False Then
            Schema.Current_Brain = SP_Brain.Text
        End If
    End Sub

    Private Sub SP_Cluster_Event() Handles SP_Cluster.Spins
        If Utility.IsEmpty(SP_Cluster.Text) = False Then
            Schema.Current_Cluster() = SP_Cluster.Text
        End If
    End Sub

    Private Sub SP_Word_Event() Handles SP_Word.Spins
        If Utility.IsEmpty(SP_Word.Text) = False Then
            Schema.Current_Word() = SP_Word.Text
        End If
    End Sub

    Private Sub PS_Plugins_Event() Handles PS_Plugins.IsChecked
        Schema.Current_Plugins() = PS_Plugins.CheckedList()
    End Sub

    Private Sub SP_Script_Event() Handles SP_Script.Spins
        If Utility.IsEmpty(SP_Script.Text) = False Then
            Schema.Current_Script() = SP_Script.Text
        End If
    End Sub

    Private Sub SP_Voices_Event() Handles SP_Voices.Spins
        If Utility.IsEmpty(SP_Voices.Text) = False Then
            Schema.Current_Voice() = SP_Voices.Index()
        End If
    End Sub

    Private Sub SP_VoiceSelect_Event() Handles SP_VoiceSpeed.Spins
        Schema.Current_Voice_Speed() = SP_VoiceSpeed.Text()
    End Sub

    Private Sub Button_Add_Event() Handles Button_Add.LeftClick
        Dim profile = TB_Profile.Text()

        If Utility.IsEmpty(profile) = True Then
            Message.Show(Me, "Please enter a new profile name.")
        End If
        If SP_Profiles.Contains(profile, Gate.eEQUALS) = True Then
            Message.Show(Me, "This profile already exists.")
            Exit Sub
        End If

        Schema.Current_Profile() = profile
        SP_Profiles.Add(profile)
        SP_Profiles.Text = profile
        Schema.Profiles.Add(profile)
    End Sub

    Private Sub Button_Rename_Event() Handles Button_Rename.LeftClick
        Dim newProfile = SP_Profiles.Text()

        Schema.Profiles.Remove(Schema.Current_Profile())
        Schema.Profiles.Add(newProfile)
        SP_Profiles.Remove(Schema.Current_Profile(), Gate.eEQUALS)
        SP_Profiles.Add(newProfile)
        SP_Profiles.Text() = newProfile

        Schema.Rename(newProfile)
        Schema.Current_Profile() = newProfile

        Schema.Save()
        Message.Show(Me, "Successful Rename!")
    End Sub

    Private Sub Button_Delete_Event() Handles Button_Delete.LeftClick
        If Utility.Search(TB_Profile.Text(), "Standard", Gate.eEQUALS) = False Then

            Dim ProfileDelete = Utility.Trim(TB_Profile.Text())

            SP_Profiles.Remove(ProfileDelete, Gate.eEQUALS)
            Schema.Profiles.Remove(ProfileDelete)

            Schema.Current_Profile() = "Standard"
            Schema.Load("Standard")
            TB_Profile.Text() = "Standard"
            SP_Profiles.Text() = "Standard"

            TB_Ai.Text() = Schema.Current_Ai()
            SP_Package.SelectText() = Schema.Current_Package()
            SP_Avatar.SelectText() = Schema.Current_Avatar(True)
            SP_Brain.SelectText() = Schema.Current_Brain(True)
            SP_Cluster.SelectText() = Schema.Current_Cluster(True)
            SP_Word.SelectText() = Schema.Current_Word(True)
            SP_Script.SelectText() = Schema.Current_Script()
            SP_Voices.Index() = Schema.Current_Voice()
            SP_VoiceSpeed.SelectText() = Schema.Current_Voice_Speed()
            SP_CanvasDPI.Base() = Schema.Current_Canvas_DPI()
            SP_AvatarDPI.Base() = Schema.Current_Avatar_DPI()

            PS_Plugins.PluginsToCheck() = Schema.Current_Plugins()
            PS_Plugins.Load()

            Schema.Delete(ProfileDelete)
            Schema.Save()
        Else
            Message.Show(Me, "Cannot delete this Profile.")
        End If
    End Sub

    Private Sub SP_CanvasDPI_Event() Handles SP_CanvasDPI.Spins
        If Utility.IsEmpty(SP_CanvasDPI.Text) = False Then
            Schema.Current_Canvas_DPI() = Utility.ToInt(SP_CanvasDPI.Text())
        End If
    End Sub

    Private Sub SP_AvatarDPI_Event() Handles SP_AvatarDPI.Spins
        If Utility.IsEmpty(SP_AvatarDPI.Text) = False Then
            Schema.Current_Avatar_DPI() = Utility.ToInt(SP_AvatarDPI.Text())
        End If
    End Sub

    Private Sub Button_Save_Event() Handles Button_Save.LeftClick
        Schema.Current_Ai() = Utility.Trim(TB_Ai.Text())
        Schema.Save()
        Message.Show(Me, "Save Successful!")
    End Sub

    Private Sub Button_OpenFolder_Event() Handles Button_OpenFolder.LeftClick
        Utility.OpenFolder(Folders.Athena())
    End Sub

End Class
