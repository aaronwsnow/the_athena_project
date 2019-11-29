Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

Public Class ScriptEditor

    Const DEF_BRAIN As String = "Brains"
    Const DEF_PLUGIN As String = "Plugins"
    Const DEF_DEBUG As String = "Debug"
    Const DEF_NEWBRAIN As String = "(new)Brain"
    Const DEF_NEWPLUG As String = "(new)Plugin"
    Const DEF_SCRIPT As String = "(new)Script"

    Private Property IsPlugin() As Boolean = False

    Private Sub Initialize() Handles Me.Initialized

        FastScript_Editor.Font(Schema.Normal_Fonts)
        Main_Grid.Background() = Schema.Global_Window_Color

        Main_TreeView.BorderBrush = Schema.Global_Border_Color

        Schema.SetStyle(LB_CurrentBrain)
        Schema.SetStyle(LB_CurrentScript)
        Schema.SetStyle(LB_CurrentPlugin)
        Schema.SetStyle(TB_CurrentBrain)
        Schema.SetStyle(TB_CurrentScript)
        Schema.SetStyle(TB_CurrentPlugin)

        Border_File.BorderBrush = Schema.Global_Border_Color()

        Main_Menu.ButtonInfo() = {"File|File", "Brain|Brain", "Script|Script", "Plugin|Plugin", "Bookmark|Bookmark", "Save|Save"}

        FastScript_Editor.ScriptLanguage() = FastColoredTextBoxNS.Language.Custom

    End Sub

    Private Sub Load() Handles Me.Loaded
        Load_TreeView()
    End Sub

    Private Sub Main_Splitter_MouseEnter_Event() Handles Main_Splitter.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub Main_Splitter_MouseLeave_Event() Handles Main_Splitter.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub

    Private Property BrainFolder() As String = Folders.Brains()

    Private Property PluginsFolder() As String = Folders.Plugins()

    Private Sub Load_TreeView()
        If Load_TreeView_Thread.IsBusy = False Then
            Load_TreeView_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Sub Load_TreeView_InThread()
        Main_TreeView.Items.Clear()

        Dim Main_Head_Brain As New TreeViewItem
        Main_Head_Brain.Header = DEF_BRAIN

        Schema.SetStyle(Main_Head_Brain)
        Main_Head_Brain.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head_Brain)


        Dim BrainFiles = Utility.GetAllFiles(BrainFolder())

        For Each file In BrainFiles

            If Utility.Search(file, {"*" & Ext.BRAIN, "*" & Ext.ZIP}, Gate.eEQUALS) = True Then

                Dim newItem As New TreeViewItem
                newItem.Header = Utility.GetFileName(file, False)
                Schema.SetStyle(newItem)
                newItem.IsExpanded = False
                Main_Head_Brain.Items.Add(newItem)

                Dim itemNames = Utility.ZipGetEntries(file, True)

                For Each item In itemNames
                    If Utility.Search(item, {"*" & Ext.VB, "*" & Ext.IO}, Gate.eEQUALS) = True Then
                        Dim newCode As New TreeViewItem
                        newCode.Header = item
                        Schema.SetStyle(newCode)
                        newItem.Items.Add(newCode)
                    End If
                Next

            End If
        Next


        Dim Main_Head_Plugins As New TreeViewItem
        Main_Head_Plugins.Header = DEF_PLUGIN

        Schema.SetStyle(Main_Head_Plugins)
        Main_Head_Plugins.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head_Plugins)


        Dim Main_Head_Debug As New TreeViewItem
        Main_Head_Debug.Header = DEF_DEBUG

        Schema.SetStyle(Main_Head_Debug)
        Main_Head_Debug.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head_Debug)


        Dim ScriptFiles = Utility.GetAllFiles(PluginsFolder())

        For Each file In ScriptFiles

            If Utility.Search(file, Ext.PLUG, Gate.eEND) = True Then

                Dim newItem As New TreeViewItem
                newItem.Header = Utility.GetFileName(file, False)
                Schema.SetStyle(newItem)
                newItem.IsExpanded = True
                Main_Head_Plugins.Items.Add(newItem)

            End If

            If Utility.Search(file, Ext.DBG, Gate.eEND) = True Then

                Dim newItem As New TreeViewItem
                newItem.Header = Utility.GetFileName(file, False)
                Schema.SetStyle(newItem)
                newItem.IsExpanded = True
                Main_Head_Debug.Items.Add(newItem)

            End If

        Next
    End Sub

    WithEvents Load_TreeView_Thread As New ComponentModel.BackgroundWorker
    Private Sub Load_TreeView_DoWork() Handles Load_TreeView_Thread.DoWork
        Main_TreeView.Dispatcher.BeginInvoke(Sub() Load_TreeView_InThread())
    End Sub
    Private Sub Load_TreeView_Completed() Handles Load_TreeView_Thread.RunWorkerCompleted
    End Sub


    Private Property OriginalScript() As String = ""
    Private Property OriginalBrain() As String = ""
    Private Property OriginalPlugin() As String = ""


    Private Sub Select_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Object)) Handles Main_TreeView.SelectedItemChanged
        If e.NewValue IsNot Nothing Then
            Dim header = ""
            Dim script = ""
            Dim brain = ""

            Try
                header = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                brain = TryCast(e.NewValue.Parent, TreeViewItem).Header
                script = TryCast(e.NewValue, TreeViewItem).Header
            Catch
                Try
                    header = TryCast(e.NewValue.Parent, TreeViewItem).Header
                    brain = TryCast(e.NewValue, TreeViewItem).Header
                Catch
                    Try
                        header = TryCast(e.NewValue, TreeViewItem).Header
                    Catch : End Try
                End Try
            End Try

            If Utility.Search(header, DEF_BRAIN, True, Gate.eEQUALS) = True Then
                IsPlugin() = False

                LB_CurrentBrain.Visibility = Visibility.Visible
                TB_CurrentBrain.Visibility = Visibility.Visible
                LB_CurrentPlugin.Visibility = Visibility.Hidden
                TB_CurrentPlugin.Visibility = Visibility.Hidden
                LB_CurrentScript.Visibility = Visibility.Visible
                TB_CurrentScript.Visibility = Visibility.Visible

                TB_CurrentBrain.Text() = brain
                OriginalBrain() = brain

                TB_CurrentScript.Text() = ""
                OriginalScript() = ""

                TB_CurrentPlugin.Text() = ""
                OriginalPlugin() = ""

                If Utility.IsEmpty(script) = False Then
                    TB_CurrentScript.Text() = script
                    OriginalScript() = script

                    If LoadScript_Thread.IsBusy = False Then
                        Load_Location() = BrainFolder() & brain
                        Load_Script() = script
                        Load_Mode() = 2
                        LoadScript_Thread.RunWorkerAsync()
                    End If
                End If

            End If

            If Utility.Search(header, {DEF_PLUGIN, DEF_DEBUG}, True, Gate.eEQUALS) = True Then
                IsPlugin() = True

                LB_CurrentBrain.Visibility = Visibility.Hidden
                TB_CurrentBrain.Visibility = Visibility.Hidden
                LB_CurrentPlugin.Visibility = Visibility.Visible
                TB_CurrentPlugin.Visibility = Visibility.Visible
                LB_CurrentScript.Visibility = Visibility.Hidden
                TB_CurrentScript.Visibility = Visibility.Hidden


                TB_CurrentBrain.Text() = ""
                OriginalBrain() = ""

                TB_CurrentScript.Text() = ""
                OriginalScript() = ""

                TB_CurrentPlugin.Text() = brain
                OriginalPlugin() = brain

                If LoadScript_Thread.IsBusy = False Then
                    Load_Location() = PluginsFolder() & brain
                    Load_Mode() = 1
                    LoadScript_Thread.RunWorkerAsync()
                End If
            End If

        End If
    End Sub

    Private Shared Property Load_Location() As String = ""
    Private Shared Property Load_Script() As String = ""
    Private Shared Property Load_Mode() As Integer = 1
    Private Shared Property Load_Content() As String = ""
    WithEvents LoadScript_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadScript_DoWork() Handles LoadScript_Thread.DoWork
        If Load_Mode() = 1 Then
            Load_Content() = Utility.TextReader(Load_Location())
        ElseIf Load_Mode() = 2 Then
            Load_Content() = Utility.ZipReader(Load_Location(), Load_Script())
        End If
    End Sub
    Private Sub Load_Script_Completed() Handles LoadScript_Thread.RunWorkerCompleted
        FastScript_Editor.Dispatcher.BeginInvoke(Sub()
                                                     FastScript_Editor.Text() = Load_Content()
                                                 End Sub)
    End Sub


    Private Sub Unload() Handles Me.Unloaded
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
            Save()
            Exit Sub
        End If

        If button.Name() = "File" Then
            Sub_Menu.ButtonInfo() = {"OpenBrainFolder|Brain Folder", "OpenPluginFolder|Plugin Folder", "Refresh|Refresh", "FindScript|Find", "ReplaceScript|Replace", "GoToScript|GoTo"}
            makebutton = True
        End If

        If button.Name() = "Brain" Then
            Sub_Menu.ButtonInfo() = {"NewBrain|New Brain", "SaveAs|SaveAs Brain", "DeleteBrain|Delete", "CopyBrain|Copy"}
            makebutton = True
        End If

        If button.Name() = "Script" Then
            Sub_Menu.ButtonInfo() = {"NewScript|New Script", "DeleteScript|Delete", "CopyScript|Copy"}
            makebutton = True
        End If

        If button.Name() = "Plugin" Then
            Sub_Menu.ButtonInfo() = {"NewPlugin|New Plugin", "DeletePlugin|Delete", "CopyPlugin|Copy"}
            makebutton = True
        End If

        If button.Name() = "Bookmark" Then
            Sub_Menu.ButtonInfo() = {"AddBookmark|Add", "RemoveBookmark|Remove", "NextBookmark|Next", "PreviousBookmark|Previous"}
            makebutton = True
            FastScript_Editor.IsEnabled() = False
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

        'Brain
        If button.Name() = "NewBrain" Then
            NewBrain()
            Exit Sub
        End If
        If button.Name() = "SaveAs" Then
            SaveAs()
            Exit Sub
        End If
        If button.Name() = "DeleteBrain" Then
            DeleteBrain()
            Exit Sub
        End If
        If button.Name() = "CopyBrain" Then
            CopyBrain()
            Exit Sub
        End If
        'Script
        If button.Name() = "NewScript" Then
            NewScript()
            Exit Sub
        End If
        If button.Name() = "DeleteScript" Then
            DeleteScript()
            Exit Sub
        End If
        If button.Name() = "CopyScript" Then
            CopyScript()
            Exit Sub
        End If
        'Plugin
        If button.Name() = "NewPlugin" Then
            NewPlugin()
            Exit Sub
        End If
        If button.Name() = "DeletePlugin" Then
            DeletePlugin()
            Exit Sub
        End If
        If button.Name() = "CopyPlugin" Then
            CopyPlugin()
            Exit Sub
        End If
        'Folders
        If button.Name() = "OpenBrainFolder" Then
            OpenBrainFolder()
            Exit Sub
        End If
        If button.Name() = "OpenPluginFolder" Then
            OpenPluginFolder()
            Exit Sub
        End If
        If button.Name() = "Refresh" Then
            Refresh()
            Exit Sub
        End If
        If button.Name() = "FindScript" Then
            FindScript()
            Exit Sub
        End If
        If button.Name() = "ReplaceScript" Then
            ReplaceScript()
            Exit Sub
        End If
        If button.Name() = "GoToScript" Then
            GoToScript()
            Exit Sub
        End If
        'Bookmarks
        If button.Name() = "AddBookmark" Then
            AddBookmark()
            Bookmark_Timer.Start(10)
            Exit Sub
        End If
        If button.Name() = "RemoveBookmark" Then
            RemoveBookmark()
            Bookmark_Timer.Start(10)
            Exit Sub
        End If
        If button.Name() = "NextBookmark" Then
            GotoNextBookmark()
            Bookmark_Timer.Start(10)
            Exit Sub
        End If
        If button.Name() = "PreviousBookmark" Then
            GotoPreviousBookmark()
            Bookmark_Timer.Start(10)
            Exit Sub
        End If

    End Sub


    WithEvents Bookmark_Timer As New DelayDispatcher()
    Private Sub Bookmark_Delay() Handles Bookmark_Timer.Elapsed
        FastScript_Editor.IsEnabled() = True
    End Sub

    Private Sub AddBookmark()
        FastScript_Editor.AddBookmark()
    End Sub
    Private Sub RemoveBookmark()
        FastScript_Editor.RemoveBookmark()
    End Sub
    Private Sub GotoNextBookmark()
        FastScript_Editor.GotoNextBookmark()
    End Sub
    Private Sub GotoPreviousBookmark()
        FastScript_Editor.GotoPreviousBoomark()
    End Sub

    Private Sub FindScript()
        FastScript_Editor.ShowFindDialog()
    End Sub
    Private Sub ReplaceScript()
        FastScript_Editor.ShowReplaceDialog()
    End Sub
    Private Sub GoToScript()
        FastScript_Editor.ShowGoToDialog()
    End Sub



    Private Sub Refresh()
        Load_TreeView()

        If IsPlugin() = False Then
            FastScript_Editor.Text() = Utility.ZipReader(BrainFolder() & OriginalBrain(), OriginalScript())
        Else
            FastScript_Editor.Text() = Utility.TextReader(PluginsFolder() & OriginalPlugin())
        End If
    End Sub

    Private Sub Save()

        If IsPlugin() = False Then

            If Utility.IsEmpty(TB_CurrentBrain.Text()) = False Then
                If Utility.IsEmpty(TB_CurrentScript.Text()) = False Then

                    If Utility.Search(TB_CurrentBrain.Text(), Ext.ZIP, Gate.eEND) = False Then
                        TB_CurrentBrain.Text() = Utility.RemoveExtension(TB_CurrentBrain.Text()) & Ext.BRAIN
                    End If
                    If Utility.Search(TB_CurrentScript.Text(), {"*" & Ext.VB, "*" & Ext.IO}, Gate.eEQUALS) = False Then
                        TB_CurrentScript.Text() = Utility.RemoveExtension(TB_CurrentScript.Text()) & Ext.VB
                    End If


                    'allows for rename, not copy
                    If Utility.Search(OriginalBrain(), TB_CurrentBrain.Text(), Gate.eEQUALS) = False Then
                        Utility.FileRename(BrainFolder() & OriginalBrain(), TB_CurrentBrain.Text())
                    End If

                    If Utility.Search(OriginalScript(), TB_CurrentScript.Text(), Gate.eEQUALS) = False Then
                        Utility.DeleteZipEntry(BrainFolder() & TB_CurrentBrain.Text(), OriginalScript())
                    End If

                    Utility.ZipWriter(BrainFolder() & TB_CurrentBrain.Text(), TB_CurrentScript.Text(), FastScript_Editor.Text())
                    OriginalBrain() = TB_CurrentBrain.Text()
                    OriginalScript() = TB_CurrentScript.Text()

                    Load_TreeView()
                    Message.Show(Me, "Save Successful!")
                End If
            Else
                SaveAs()
            End If

        Else

            If Utility.Search(TB_CurrentPlugin.Text(), Ext.DBG, Gate.eEND) = True Then
                Message.Show(Me, "This file is a Read-Only Debug file. For use in debugging code.")
            Else

                TB_CurrentPlugin.Text() = Utility.RemoveExtension(TB_CurrentPlugin.Text()) & Ext.PLUG

                'allows for rename, not copy
                If Utility.Search(OriginalPlugin(), TB_CurrentPlugin.Text(), Gate.eEQUALS) = False Then
                    Utility.FileRename(PluginsFolder() & OriginalPlugin(), TB_CurrentPlugin.Text())
                End If

                Utility.TextWriter(PluginsFolder() & TB_CurrentPlugin.Text(), FastScript_Editor.Text(), False)
                OriginalPlugin() = TB_CurrentPlugin.Text()

                Load_TreeView()
                Message.Show(Me, "Save Successful!")
            End If



        End If



    End Sub

    Private Sub SaveAs()

        FolderPicker.Filter() = {Ext.ZIP, Ext.BRAIN}
        FolderPicker.Extension() = Ext.BRAIN
        FolderPicker.InitialFile() = DEF_BRAIN
        FolderPicker.InitialDirectory() = BrainFolder()

        Dim result = FolderPicker.Show()

        If result = FolderPicker.OK Then

            Utility.ZipWriter(FolderPicker.FullFileName(), TB_CurrentScript.Text(), FastScript_Editor.Text())
            TB_CurrentBrain.Text() = FolderPicker.SafeFileName()
            OriginalBrain() = TB_CurrentBrain.Text()
            OriginalScript() = TB_CurrentScript.Text()

            Load_TreeView()
            Message.Show(Me, "Save Successful!")
        End If
    End Sub


    Private Sub NewBrain()
        FastScript_Editor.Text() = ""
        Dim newScript = TB_CurrentScript.Text()
        Dim newBrains = TB_CurrentBrain.Text()
        Dim newSExtens = ""
        Dim newBExtens = ""

        If Utility.IsEmpty(newScript) = True Then
            newScript = DEF_SCRIPT
        End If

        If Utility.Search(newScript, Ext.VB, Gate.eEND) = True Then
            newSExtens = Ext.VB
        ElseIf Utility.Search(newScript, Ext.IO, Gate.eEND) = True Then
            newSExtens = Ext.IO
        Else
            newSExtens = Ext.VB
        End If

        newScript = Utility.RemoveExtension(newScript)

        '*********

        If Utility.IsEmpty(newBrains) = True Then
            newBrains = DEF_NEWBRAIN
        End If

        If Utility.Search(newBrains, Ext.ZIP, Gate.eEND) = True Then
            newBExtens = Ext.ZIP
        Else
            newBExtens = Ext.BRAIN
        End If

        newBrains = Utility.RemoveExtension(newBrains)

        '*********
        'get new name
        Dim count = 1
        Dim newCopy = newBrains
        newBrains = newBrains & newBExtens
        Do Until Utility.FileExists(BrainFolder() & newBrains) = False
            newBrains = newCopy & "(" & count & ")" & newBExtens
            count += 1
        Loop

        newScript = newScript & newSExtens

        TB_CurrentBrain.Text() = newBrains
        OriginalBrain() = newBrains

        TB_CurrentScript.Text() = newScript
        OriginalScript() = newScript

        Utility.ZipWriter(BrainFolder() & newBrains, newScript, FastScript_Editor.Text())
        Load_TreeView()

    End Sub

    Private Sub NewScript()

        If Utility.IsEmpty(TB_CurrentBrain.Text()) = False Then

            FastScript_Editor.Text() = ""
            Dim newScripts = TB_CurrentScript.Text()
            Dim newExtens = ""

            If Utility.Search(TB_CurrentBrain.Text(), Ext.ZIP, Gate.eEND) = False Then
                TB_CurrentBrain.Text() = Utility.RemoveExtension(TB_CurrentBrain.Text()) & Ext.BRAIN
            End If

            '*********

            If Utility.IsEmpty(newScripts) = True Then
                newScripts = DEF_SCRIPT
            End If

            If Utility.Search(newScripts, Ext.VB, Gate.eEND) = True Then
                newExtens = Ext.VB
            ElseIf Utility.Search(newScripts, Ext.IO, Gate.eEND) = True Then
                newExtens = Ext.IO
            Else
                newExtens = Ext.VB
            End If

            newScripts = Utility.RemoveExtension(newScripts)


            '*********
            'get new name
            Dim count = 1
            Dim newCopy = newScripts
            newScripts = newScripts & newExtens
            Do Until Utility.DoesZipEntryExist(BrainFolder() & TB_CurrentBrain.Text(), newScripts) = False
                newScripts = newCopy & "(" & count & ")" & newExtens
                count += 1
            Loop

            TB_CurrentScript.Text() = newScripts
            OriginalScript() = newScripts

            Utility.ZipWriter(BrainFolder() & TB_CurrentBrain.Text(), newScripts, FastScript_Editor.Text())
            Load_TreeView()
        Else
            Message.Show(Me, "Select a Brain before adding a new Script.")
        End If

    End Sub

    Private Sub NewPlugin()

        FastScript_Editor.Text() = ""
        Dim newPlugin = TB_CurrentPlugin.Text()
        Dim newExtens = Ext.PLUG

        '*********

        If Utility.IsEmpty(newPlugin) = True Then
            newPlugin = DEF_SCRIPT
        End If

        newPlugin = Utility.RemoveExtension(newPlugin)

        '*********
        'get new name
        Dim count = 1
        Dim newCopy = newPlugin
        newPlugin = newPlugin & newExtens
        Do Until Utility.FileExists(PluginsFolder() & newPlugin) = False
            newPlugin = newCopy & "(" & count & ")" & newExtens
            count += 1
        Loop

        TB_CurrentPlugin.Text() = newPlugin
        OriginalPlugin() = newPlugin

        Utility.TextWriter(PluginsFolder() & newPlugin, FastScript_Editor.Text(), False)
        Load_TreeView()

    End Sub


    Public Sub DeleteBrain()
        If Utility.IsEmpty(TB_CurrentBrain.Text()) = False Then

            Dim results = Message.Show(Me, "Are you sure you want to delete """ & TB_CurrentBrain.Text() & """ Brain" & "?")

            If results = Message.OK Then
                Utility.FileDelete(BrainFolder() & TB_CurrentBrain.Text())
                OriginalScript() = ""
                OriginalBrain() = ""
                TB_CurrentBrain.Text() = ""
                TB_CurrentScript.Text() = ""
                FastScript_Editor.Text() = ""
                Load_TreeView()
            End If

        End If
    End Sub

    Public Sub DeleteScript()
        If Utility.IsEmpty(TB_CurrentScript.Text()) = False Then

            Dim results = Message.Show(Me, "Are you sure you want to delete """ & TB_CurrentScript.Text() & """ Script" & "?")

            If results = Message.OK Then
                Utility.DeleteZipEntry(BrainFolder() & TB_CurrentBrain.Text(), TB_CurrentScript.Text())
                FastScript_Editor.Text() = ""
                OriginalScript() = ""
                TB_CurrentScript.Text() = ""
                Load_TreeView()
            End If

        End If
    End Sub

    Public Sub DeletePlugin()
        If Utility.IsEmpty(TB_CurrentPlugin.Text()) = False Then

            Dim results = Message.Show(Me, "Are you sure you want to delete """ & TB_CurrentPlugin.Text() & """ Plugin" & "?")

            If results = Message.OK Then
                Utility.FileDelete(PluginsFolder() & TB_CurrentPlugin.Text())
                FastScript_Editor.Text() = ""
                OriginalPlugin() = ""
                TB_CurrentPlugin.Text() = ""
                Load_TreeView()
            End If

        End If
    End Sub


    Public Sub CopyBrain()

        Dim newBrains = TB_CurrentBrain.Text()
        Dim newExtens = ""

        If Utility.IsEmpty(newBrains) = True Then
            newBrains = DEF_NEWBRAIN
        End If

        If Utility.Search(newBrains, Ext.ZIP, Gate.eEND) = True Then
            newExtens = Ext.ZIP
        Else
            newExtens = Ext.BRAIN
        End If

        newBrains = Utility.RemoveExtension(newBrains)

        '*********
        'get new name
        Dim count = 1
        Dim newCopy = newBrains
        newBrains = newBrains & newExtens
        Do Until Utility.FileExists(BrainFolder() & newBrains) = False
            newBrains = newCopy & "-Copy(" & count & ")" & newExtens
            count += 1

            If count = 50 Then Exit Sub
        Loop

        Utility.FileCopy(BrainFolder() & OriginalBrain(), BrainFolder() & newBrains)
        Load_TreeView()
    End Sub

    Public Sub CopyScript()

        If Utility.IsEmpty(TB_CurrentBrain.Text()) = False Then

            Dim newScripts = TB_CurrentScript.Text()
            Dim newExtens = ""

            If Utility.Search(TB_CurrentBrain.Text(), Ext.ZIP, Gate.eEND) = False Then
                TB_CurrentBrain.Text() = Utility.RemoveExtension(TB_CurrentBrain.Text()) & Ext.BRAIN
            End If


            '*********

            If Utility.IsEmpty(newScripts) = True Then
                newScripts = DEF_SCRIPT
            End If

            If Utility.Search(newScripts, Ext.VB, Gate.eEND) = True Then
                newExtens = Ext.VB
            ElseIf Utility.Search(newScripts, Ext.IO, Gate.eEND) = True Then
                newExtens = Ext.IO
            Else
                newExtens = Ext.VB
            End If

            newScripts = Utility.RemoveExtension(newScripts)


            '*********
            'get new name
            Dim count = 1
            Dim newCopy = newScripts
            newScripts = newScripts & newExtens
            Do Until Utility.DoesZipEntryExist(BrainFolder() & TB_CurrentBrain.Text(), newScripts) = False
                newScripts = newCopy & "-Copy(" & count & ")" & newExtens
                count += 1

                If count = 50 Then Exit Sub
            Loop

            Utility.CopyZipEntry(BrainFolder() & TB_CurrentBrain.Text(), OriginalScript(), newScripts)
            Load_TreeView()
        Else
            Message.Show(Me, "Select a Brain before copying a Script.")
        End If

    End Sub

    Public Sub CopyPlugin()


        Dim newPlugin = TB_CurrentScript.Text()
        Dim newExtens = Ext.PLUG


        '*********

        If Utility.IsEmpty(newPlugin) = True Then
            newPlugin = DEF_SCRIPT
        End If

        newPlugin = Utility.RemoveExtension(newPlugin)

        '*********
        'get new name
        Dim count = 1
        Dim newCopy = newPlugin
        newPlugin = newPlugin & newExtens
        Do Until Utility.FileExists(PluginsFolder() & newPlugin) = False
            newPlugin = newCopy & "-Copy(" & count & ")" & newExtens
            count += 1

            If count = 1000 Then Exit Sub
        Loop

        Utility.FileCopy(PluginsFolder() & OriginalPlugin(), PluginsFolder() & newPlugin)
        Load_TreeView()

    End Sub


    Private Sub OpenBrainFolder()
        Utility.OpenFolder(BrainFolder())
    End Sub
    Private Sub OpenPluginFolder()
        Utility.OpenFolder(PluginsFolder())
    End Sub

End Class
