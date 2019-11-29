Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

'refresh treeview
'save node shows two messages
'new story needed


Public Class DialogEditor
    Private Shared Property Story As New Story
    Private Property OldDetection As New Story.Detection
    Const DEF_TAG_EXAMPLE As String = "#search:or"
    Const DEF_NOTSAVED As String = "Please Save the Story before continuing."
    Private ReadOnly Property DEF_CONVERS_EXAMPLE As String() = {"(User)Hello", "(Ai)Hi there. What do you want?"}
    Private ReadOnly Property DEF_METATAGS_EXAMPLE As String() = {"#info:", "#tier:-1", "#detect:", "#search:or", "#skips:", "#table:", "#leads:", "#avatar:any", "#type:story"}
    Public Const DEF_NEWSTORY As String = "newStory"


    Private Sub Initialize() Handles Me.Initialized

        Main_Grid.Background = Schema.Global_Window_Color
        Main_TreeView.BorderBrush = Schema.Global_Border_Color
        Load_TreeView()

        Main_Menu.ButtonInfo() = {"File|File", "Edit|Edit", "Window|Window", "Save|Save", " | ", "Back|Back", "Forward|Forward"}

        Schema.SetStyle(Label_PageInfo)
        Schema.SetStyle(Label_Depth)
        Schema.SetStyle(Label_Select)
        Schema.SetStyle(Label_Detect)
        Schema.SetStyle(Label_iTags)
        Schema.SetStyle(Label_Name)
        Schema.SetStyle(Label_Header)
        Schema.SetStyle(Label_Navigate)
        Schema.SetStyle(Label_oNote)
        Schema.SetStyle(Label_oTags)
        Schema.SetStyle(Label_Rate)
        Schema.SetStyle(Label_Test_Index)
        Schema.SetStyle(Label_Test_Input)
        Schema.SetStyle(Label_Test_iTags)
        Schema.SetStyle(Label_Test_oTags)
        Schema.SetStyle(Label_Test_Output)
        Schema.SetStyle(Label_Test_Rate)
        Schema.SetStyle(Label_iNote)
        Schema.SetStyle(Label_Input)
        Schema.SetStyle(Label_Output)
        Schema.SetStyle(Label_UpdateGoTo)
        Schema.SetStyle(Label_AddGoto)
        Schema.SetStyle(Label_CurrentPage)
        Schema.SetStyle(Label_PageTags)
        Schema.SetStyle(Label_AllPages)
        Schema.SetStyle(Label_NavPages)
        Schema.SetStyle(Label_JumpGoto)
        Schema.SetStyle(Label_Inherit)
        Schema.SetStyle(Label_CopyToPages)

        Schema.SetStyle(TextBox_SearchNote)
        Schema.SetStyle(TextBox_Depth)
        Schema.SetStyle(TextBox_Detect)
        Schema.SetStyle(TextBox_UpdateGoTo)
        Schema.SetStyle(TextBox_iNote)
        Schema.SetStyle(TextBox_iTags)
        Schema.SetStyle(TextBox_Name)
        Schema.SetStyle(TextBox_Meta)
        Schema.SetStyle(TextBox_oNote)
        Schema.SetStyle(TextBox_oTags)
        Schema.SetStyle(TextBox_Rate)
        Schema.SetStyle(TextBox_Test_Index)
        Schema.SetStyle(TextBox_Test_Input)
        Schema.SetStyle(TextBox_Test_iTags)
        Schema.SetStyle(TextBox_Test_oTags)
        Schema.SetStyle(TextBox_Test_Output)
        Schema.SetStyle(TextBox_Test_Rate)
        Schema.SetStyle(TextBox_CurrentPage)
        Schema.SetStyle(TextBox_AddGoto)
        Schema.SetStyle(TextBox_PageTags)
        Schema.SetStyle(TextBox_JumpGoto)
        Schema.SetStyle(TextBox_Inherit)

        Schema.SetStyle(Border_SearchNote)
        Border_SearchNote.Background = Schema.Global_FontBackColor

        Schema.SetStyle(Border_Info)
        Schema.SetStyle(Border_PageInfo)
        Schema.SetStyle(Border_Selector)
        Schema.SetStyle(Border_Test_Input)
        Schema.SetStyle(Border_Test_Output)
        Schema.SetStyle(Border_Response)
        Schema.SetStyle(Border_Input)
        Schema.SetStyle(Border_Converse)
        Schema.SetStyle(Border_XML)


        Editor_Window.Visibility = Visibility.Visible
        Parser_Window.Visibility = Visibility.Collapsed
        Tester_Window.Visibility = Visibility.Collapsed
        XML_Window.Visibility = Visibility.Collapsed

        ScriptEdit_Parser.Font(Schema.Normal_Fonts)
        ScriptEdit_XML.Font(Schema.Normal_Fonts)

        Canvas.SetZIndex(Main_Menu, 1)

        ScriptEdit_Parser.ShowPopup() = False
        ScriptEdit_Parser.ShowHighlight() = False

        ScriptEdit_XML.ScriptLanguage() = FastColoredTextBoxNS.Language.XML
        ScriptEdit_XML.ShowPopup() = False

    End Sub

    Private Sub Load() Handles Me.Loaded
        Load_Story()
    End Sub

    Private Property SaveReminder() As Boolean = False
    Private Sub Unload() Handles Me.Unloaded
        Sub_Menu.Close()

        If SaveReminder() = True Then
            Dim out = Message.Show(Me, "Do you wish to Save your recent Story.")
            If out = Message.OK Then
                Save_Story()
            End If
        End If
    End Sub

    WithEvents Sub_Menu As New MenuDialog
    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick
        Dim makebutton = False

        Sub_Menu = New MenuDialog()
        Sub_Menu.Orientation = Orientation.Vertical
        Sub_Menu.TextHorizontalAlignment = HorizontalAlignment.Left
        Sub_Menu.Buffer() = New Thickness(10)
        Sub_Menu.ButtonBorder(Brushes.Transparent, New Thickness(0))


        If button.Name() = "File" Then
            Sub_Menu.ButtonInfo() = {"OpenFolder|Open Folder", "NewStory|New Story", "RefreshFiles|Refresh", "DeleteStory|Delete", "SaveAsStory|SaveAs"}
            makebutton = True
        End If

        If button.Name() = "Edit" Then
            Sub_Menu.ButtonInfo() = {"NewPage|New Page", "DeletePage|Delete Page", "RefreshPage|Refresh Page", "ClearPage|Clear Page", "ResetStory|Reset Story", "ClearStory|Clear Story"}
            makebutton = True
        End If

        If button.Name() = "Window" Then
            Sub_Menu.ButtonInfo() = {"Editor|Show Editor", "Parser|Show Parser", "Tester|Show Tester", "XML_Editor|Show XML"}
            makebutton = True
        End If

        If button.Name() = "Save" Then
            Save_Story()
            Exit Sub
        End If

        If button.Name() = "Back" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Backward_Event()
            End If
            Exit Sub
        End If

        If button.Name() = "Forward" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Forward_Event()
            End If
            Exit Sub
        End If

        If makebutton = True Then
            Sub_Menu.Load()
            Dim loc = Main_Menu.PointToScreen(New Point(0, 0))
            Sub_Menu.Location() = New Point(button.Position.X, loc.Y + Main_Menu.Size.Height)
            Sub_Menu.IsOpen() = True
        End If

    End Sub

    Private Sub Sub_Menu_Event(ByVal button As GenerikButton) Handles Sub_Menu.LeftClick
        Sub_Menu.Close()

        If button.Name() = "OpenFolder" Then
            OpenFolder()
        End If
        If button.Name() = "NewStory" Then
            New_Story()
        End If
        If button.Name() = "RefreshFiles" Then
            Load_TreeView()
        End If
        If button.Name() = "DeleteStory" Then
            If Utility.IsEmpty(Story.Source()) = False Then
                Delete_Story()
                Exit Sub
            End If
        End If
        If button.Name() = "SaveAsStory" Then
            SaveAs_Story()
        End If

        If button.Name() = "Editor" Then
            Editor_Window.Visibility = Visibility.Visible
            Parser_Window.Visibility = Visibility.Collapsed
            Tester_Window.Visibility = Visibility.Collapsed
            XML_Window.Visibility = Visibility.Collapsed
        End If
        If button.Name() = "Parser" Then
            Parser_Window.Visibility = Visibility.Visible
            Editor_Window.Visibility = Visibility.Collapsed
            Tester_Window.Visibility = Visibility.Collapsed
            XML_Window.Visibility = Visibility.Collapsed
            ScriptEdit_Parser.Text() = Utility.Join(DEF_CONVERS_EXAMPLE, vbCrLf)
        End If
        If button.Name() = "Tester" Then
            Tester_Window.Visibility = Visibility.Visible
            Editor_Window.Visibility = Visibility.Collapsed
            Parser_Window.Visibility = Visibility.Collapsed
            XML_Window.Visibility = Visibility.Collapsed
        End If
        If button.Name() = "XML_Editor" Then
            Tester_Window.Visibility = Visibility.Collapsed
            Editor_Window.Visibility = Visibility.Collapsed
            Parser_Window.Visibility = Visibility.Collapsed
            XML_Window.Visibility = Visibility.Visible

            RefreshXML()
        End If


        If button.Name() = "NewPage" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                New_Page()
            End If
            Exit Sub
        End If
        If button.Name() = "DeletePage" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Delete_Page()
            End If
            Exit Sub
        End If
        If button.Name() = "RefreshPage" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Refresh_Page()
            End If
            Exit Sub
        End If
        If button.Name() = "ClearPage" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Clear_Page()
            End If
            Exit Sub
        End If
        If button.Name() = "ResetStory" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Reset_Story()
            End If
            Exit Sub
        End If
        If button.Name() = "ClearStory" Then
            If Utility.IsEmpty(Story.Source()) = True Then
                Message.Show(Me, DEF_NOTSAVED)
            Else
                Clear_Story()
            End If
            Exit Sub
        End If

    End Sub


    Private Sub OpenFolder()
        Utility.OpenFolder(NodeFolder())
    End Sub

    Private Property Skip_AllPages() As Boolean = False
    Public Sub Refresh() Handles Button_RefreshPage.LeftClick
        Clear_Page()

        If Skip_AllPages() = False Then
            For Each page In Story.Chapters.Pages()
                SP_AllPages.Add(page.Name())
            Next
            SP_AllPages.Index() = 0
        End If

        For Each page In Story.Chapters.Pages()
            SP_CopyToPages.Add(page.Name())
        Next
        SP_CopyToPages.Index() = 0

        For Each page In Story.GetGoTos()
            SP_NavPages.Add(page)
        Next
        SP_NavPages.Index() = 0

        TextBox_Name.Text = Utility.IsEmpty(Story.Name(), DEF_NEWSTORY)
        TextBox_Meta.Text = Utility.IsEmpty(Story.Header(), Utility.Join(DEF_METATAGS_EXAMPLE, vbCrLf))

        TextBox_CurrentPage.Text = Story.GetPage()
        TextBox_iNote.Text = Utility.Join(Story.GetNote(), Story.DEF_DELIM)
        TextBox_PageTags.Text = Story.GetTags()
        TextBox_JumpGoto.Text = Story.GetJump()
        TextBox_Inherit.Text = Utility.Join(Story.GetInheritence(), Story.DEF_DELIM)

        For Each nav In Story.GetNavigation
            SP_Choices.Add(Utility.Join(nav.Detect, Story.DEF_DELIM))
        Next
        SP_Choices.Index() = 0

        TextBox_Depth.Text = " " & Story.CurrentDepth() & " "

    End Sub

    Private Sub Load_Story()
        Refresh()
        SP_AllPages.Index() = 0
        SP_CopyToPages.Index() = 0
        SP_NavPages.Index() = 0
        SaveReminder() = False
    End Sub

    Private Sub Select_Page() Handles SP_AllPages.SelectedSpin
        Story.Load(SP_AllPages.Text())
        Story.PreviousReset()
        Skip_AllPages() = True
        Refresh()
        Skip_AllPages() = False
    End Sub

    Private Sub Select_CopyToPages() Handles SP_CopyToPages.SelectedSpin
        Dim findpage = Story.Chapters.Find(SP_CopyToPages.Text())
        Dim out = Message.Show(Me, "You are about to Update the currently selected page's Navigation using the """ & SP_CopyToPages.Text() & """ page's Navigation.")
        If out = Message.OK Then
            Story.Chapters.Update(TextBox_CurrentPage.Text(), Nothing, Nothing, Nothing, Nothing, findpage.Navigate())
            Refresh()
        End If
    End Sub

    Private Sub Select_NavPage() Handles SP_NavPages.SelectedSpin
        If Story.Load(SP_NavPages.Text) = True Then
            Refresh()
        End If
    End Sub

    Private Sub SP_Choices_SelectionChanged() Handles SP_Choices.SelectedSpin
        If Utility.IsEmpty(SP_Choices.Text) = False Then
            Dim selected = Story.GetNavigation(SP_Choices.Index)

            OldDetection() = selected

            If selected IsNot Nothing Then
                TextBox_Detect.Text = Utility.Join(selected.Detect, Story.DEF_DELIM)
                TextBox_Rate.Text = " " & selected.Rate.ToString & " "
                TextBox_iTags.Text = selected.Tags
                TextBox_UpdateGoTo.Text = selected.GoTo

                Dim nextPage = Story.Chapters.Find(selected.GoTo)
                TextBox_oNote.Text = Utility.Join(nextPage.Note, Story.DEF_DELIM)
                TextBox_oTags.Text = nextPage.Tags
            End If
        End If
    End Sub

    Private Sub Clear_Page()
        TextBox_Detect.Text = Story.DEFAULT_NAME(0)
        TextBox_Rate.Text = " 0 "
        TextBox_iTags.Text = DEF_TAG_EXAMPLE
        TextBox_UpdateGoTo.Text = ""
        TextBox_AddGoto.Text = Story.RandomPageName()
        TextBox_oNote.Text = ""
        TextBox_oTags.Text = ""

        If Skip_AllPages() = False Then SP_AllPages.Clear()
        SP_CopyToPages.Clear()
        SP_NavPages.Clear()
        SP_Choices.Clear()

        TextBox_Test_Input.Text() = ""
        TextBox_Test_Output.Text() = ""
        TextBox_Test_iTags.Text() = ""
        TextBox_Test_oTags.Text() = ""
        TextBox_Test_Rate.Text = " -1 "
        TextBox_Test_Index.Text = " -1 "

        SaveReminder() = True
    End Sub

    Private Sub New_Page() Handles Button_NewPage.LeftClick

        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(TextBox_AddGoto.Text) = True Then
            Message.Show(Me, "The '(Add/New) Goto Page' Inputbox is Empty.")
            Exit Sub
        End If

        Story.Chapters.Create(TextBox_AddGoto.Text(), {""}, "", "", {""}, New List(Of Story.Detection))

        If Utility.IsEmpty(TextBox_AddGoto.Text) = False Then
            If Story.Load(TextBox_AddGoto.Text) = True Then
                SaveReminder() = True
                Refresh()
            End If
        End If

    End Sub

    Private Sub New_Story()
        Clear_Story()
        TextBox_Name.Text() = DEF_NEWSTORY
    End Sub

    Private Sub Delete_Story()
        Dim name = Story.Name()

        Dim out = Message.Show(Me, "Are you sure want to Delete """ & Story.Source() & """ ?")

        If out = Message.OK Then
            Utility.FileDelete(Story.Source())

            Clear_Story()
            Refresh_Page()
            Load_TreeView()
            SaveReminder() = False
        End If

    End Sub

    Private Sub SearchNotes() Handles Button_SearchNote.LeftClick
        Dim pages = Story.SearchNotes(TextBox_SearchNote.Text(), Gate.eSEARCH)
        SP_AllPages.Clear()

        For Each page In pages
            SP_AllPages.Add(page.Name())
        Next

        SP_AllPages.Index() = 0
    End Sub

#Region "Save"

    Private Sub Save_Story()
        If Utility.IsEmpty(Story.Source()) = False Then
            Story.Header() = Utility.Trim(TextBox_Meta.Text())

            If SaveStory_Thread.IsBusy() = False Then
                SaveStory_Thread.RunWorkerAsync()
            End If

            SaveReminder() = False
            Message.Show(Me, "Save Successful!")
        Else
            SaveAs_Story()
        End If
    End Sub
    WithEvents SaveStory_Thread As New ComponentModel.BackgroundWorker
    Private Sub SaveStory_DoWork() Handles SaveStory_Thread.DoWork
        Story.Save()
    End Sub
    Private Sub SaveStory_Completed() Handles SaveStory_Thread.RunWorkerCompleted
    End Sub

    Private Sub SaveAs_Story()

        FolderPicker.Filter() = {Ext.XML}
        FolderPicker.Extension() = Ext.XML
        FolderPicker.InitialFile() = TextBox_Name.Text() & Ext.XML
        FolderPicker.InitialDirectory() = Folders.Dialogs()

        Dim result = FolderPicker.Show()

        If result = FolderPicker.OK Then
            Dim filename = FolderPicker.FullFileName
            Story.Source() = filename
            Story.Header() = Utility.Trim(TextBox_Meta.Text())

            If SaveAsStory_Thread.IsBusy() = False Then
                SaveAsStory_Thread.RunWorkerAsync()
            End If

            TextBox_Name.Text() = Story.Name()
            CurrentFile() = FolderPicker.SafeFileName()
            SaveReminder() = False
            Message.Show(Me, "Save Successful!")
        End If

    End Sub
    WithEvents SaveAsStory_Thread As New ComponentModel.BackgroundWorker
    Private Sub SaveAsStory_DoWork() Handles SaveAsStory_Thread.DoWork
        Story.Save()
        Story.Load()
    End Sub
    Private Sub SaveAsStory_Completed() Handles SaveAsStory_Thread.RunWorkerCompleted
        Refresh()
        Load_TreeView()
    End Sub

#End Region


    Private Sub Adding_Node() Handles Button_Add.LeftClick

        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(TextBox_Detect.Text) = True Then
            Message.Show(Me, "The 'Detect' Inputbox is Empty.")
            Exit Sub
        End If

        If Utility.IsEmpty(TextBox_AddGoto.Text) = True Then
            Message.Show(Me, "The '(Add) Goto Page' Inputbox is Empty.")
            Exit Sub
        End If

        Dim nav As New Story.Detection
        nav.Detect = Utility.Trim(Utility.ToArray(TextBox_Detect.Text, Story.DEF_DELIM))
        nav.GoTo = TextBox_AddGoto.Text
        nav.Tags = TextBox_iTags.Text
        nav.Rate = Utility.ToInt(TextBox_Rate.Text, 0)

        If Utility.IsEmpty(TextBox_oNote.Text) = False Then
            Story.Previous() = nav
            Story.Chapters.Create(TextBox_AddGoto.Text, Utility.Trim(Utility.ToArray(TextBox_oNote.Text, Story.DEF_DELIM)), TextBox_JumpGoto.Text(), TextBox_oTags.Text, Utility.Trim(Utility.ToArray(TextBox_Inherit.Text, Story.DEF_DELIM)), Nothing)
        Else
            'this is used so that you can GoTo a node page without modifying the Note of that page
            Story.Previous() = nav
            Story.Chapters.Create(TextBox_AddGoto.Text, {""}, TextBox_JumpGoto.Text(), TextBox_oTags.Text, Utility.Trim(Utility.ToArray(TextBox_Inherit.Text, Story.DEF_DELIM)), New List(Of Story.Detection))
        End If


        Story.Chapters.AddNavigate(TextBox_CurrentPage.Text, nav)
        Story.RefreshPage()

        Refresh()
        SaveReminder() = True

    End Sub

    Private Sub GoTo_Page() Handles Button_GoTo.LeftClick
        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(TextBox_UpdateGoTo.Text) = False Then
            If Story.Load(TextBox_UpdateGoTo.Text) = True Then
                Refresh()
            End If
        End If
    End Sub


    Private Sub Skip_Event() Handles Button_Skip.LeftClick
        'this selects the current displayed selection and goes it's page
        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(SP_Choices.Text) = False Then
            Dim selected = Story.GetNavigation(SP_Choices.Index)

            OldDetection() = selected

            If selected IsNot Nothing Then

                If Utility.IsEmpty(selected.GoTo) = False Then
                    If Story.Load(selected.GoTo) = True Then
                        Refresh()
                    End If
                End If

            End If
        End If

    End Sub

    Private Sub Jump_Event() Handles Button_Jump.LeftClick
        'this selects the current displayed selection and goes it's page
        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(SP_Choices.Text) = False Then
            Dim selected = Story.GetNavigation(SP_Choices.Index)

            OldDetection() = selected

            If selected IsNot Nothing Then

                If Utility.IsEmpty(selected.GoTo) = False Then
                    If Story.Load(selected.GoTo) = True Then

                        Story.Jump()
                        Refresh()
                    End If
                End If

            End If
        End If

    End Sub


    Private Sub Delete_Page()
        Story.RemovePage(False)
        Refresh()
        SaveReminder() = True
    End Sub


    Private Sub Reset_Story() Handles Button_Reset.LeftClick
        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If ResetStory_Thread.IsBusy = False Then
            ResetStory_Thread.RunWorkerAsync()
        End If

    End Sub
    WithEvents ResetStory_Thread As New ComponentModel.BackgroundWorker
    Private Sub ResetStory_DoWork() Handles ResetStory_Thread.DoWork
        Story.Load()
    End Sub
    Private Sub ResetStory_Completed() Handles ResetStory_Thread.RunWorkerCompleted
        Refresh()
    End Sub


    Private Sub Refresh_Page()
        Story.RefreshPage()
        Refresh()
    End Sub

    Private Sub Update_Page() Handles Button_UpdatePage.LeftClick
        Dim name = Utility.Trim(TextBox_CurrentPage.Text())
        Dim oldname = Story.CurrentPage().Name()
        If Utility.Search(oldname, name, Gate.eEQUALS) = False Then
            Story.Chapters.Rename(oldname, name)
        End If

        Story.Chapters.Update(name, Utility.ToArray(TextBox_iNote.Text(), Story.DEF_DELIM), TextBox_JumpGoto.Text(), TextBox_PageTags.Text(), Utility.ToArray(TextBox_Inherit.Text(), Story.DEF_DELIM), Nothing)
    End Sub


    Private Sub UpdateNavigate_Event() Handles Button_UpdateNavigate.LeftClick

        'this updates the Detect and GoTo page of a Navagate, including the GoTo Page's Note/Tag

        If Utility.IsEmpty(Story.Source()) = True Then
            Message.Show(Me, DEF_NOTSAVED)
            Exit Sub
        End If

        If Utility.IsEmpty(TextBox_CurrentPage.Text) = False Then

            If Utility.IsEmpty(TextBox_Detect.Text) = True Then
                Message.Show(Me, "The 'Detect' Inputbox is Empty.")
                Exit Sub
            End If

            If Utility.IsEmpty(TextBox_UpdateGoTo.Text) = True Then
                Message.Show(Me, "The '(Update) Goto Page' Inputbox is Empty.")
                Exit Sub
            End If


            Dim newDetection As New Story.Detection
            newDetection.Detect = Utility.Trim(Utility.ToArray(TextBox_Detect.Text, Story.DEF_DELIM))
            newDetection.GoTo = TextBox_UpdateGoTo.Text
            newDetection.Tags = TextBox_iTags.Text
            newDetection.Rate = Utility.ToInt(TextBox_Rate.Text, 0)

            Story.Chapters.UpdateNavigate(TextBox_CurrentPage.Text, OldDetection, newDetection)

            OldDetection.Copy(newDetection)

            If Utility.IsEmpty(TextBox_oNote.Text) = False Then
                Story.Chapters.Update(TextBox_UpdateGoTo.Text, Utility.Trim(Utility.ToArray(TextBox_oNote.Text, Story.DEF_DELIM)), TextBox_JumpGoto.Text(), TextBox_oTags.Text, Utility.Trim(Utility.ToArray(TextBox_Inherit.Text, Story.DEF_DELIM)), Nothing)
            Else
                Story.Chapters.Update(TextBox_UpdateGoTo.Text, Nothing, TextBox_JumpGoto.Text(), TextBox_oTags.Text, Utility.Trim(Utility.ToArray(TextBox_Inherit.Text, Story.DEF_DELIM)), Nothing)
            End If

            Story.RefreshPage()
            Refresh()
            SaveReminder() = True
        End If
    End Sub

    Private Sub Button_Loop_Event() Handles Button_Loop.LeftClick
        TextBox_AddGoto.Text = Story.GetPage()
    End Sub

    Private Sub Button_Remove_Event() Handles Button_Remove.LeftClick

        Dim out = Message.Show(Me, "Are you sure you want to remove this choice?")

        If out = Message.OK Then
            Dim detect = Utility.Trim(Utility.ToArray(TextBox_Detect.Text(), Story.DEF_DELIM))
            Dim gotos = TextBox_UpdateGoTo.Text()
            Dim rate = Utility.ToInt(TextBox_Rate.Text(), 0)
            Dim tags = TextBox_iTags.Text()
            Dim newTags As New Tags(tags)

            Story.CurrentPage.RemoveDetection(detect, gotos, rate, newTags, False)
            SP_Choices.RemoveCurrent()
        End If

    End Sub

    Private Sub Forward_Event()
        If Story.ForwardPage() = False Then
            Refresh()
        End If
    End Sub

    Private Sub Backward_Event()
        If Story.BackPage() = False Then
            Refresh()
        End If
    End Sub

    Private Sub Clear_Story()
        Story.Clear()
        Story.Load()
        ScriptEdit_XML.Clear()
        Refresh()
        SaveReminder() = True
    End Sub

    Private Sub Random_Event() Handles Button_Random.LeftClick
        TextBox_AddGoto.Text = Story.RandomPageName()
    End Sub


#Region "Test"

    Private Sub Explore_Event() Handles Button_Send.LeftClick

        Dim input As String = Utility.Trim(TextBox_Test_Input.Text)
        If input = "" Then input = Nothing

        Dim rate As Integer? = Nothing
        Dim sRate As String = Utility.Trim(TextBox_Test_Rate.Text)
        If Utility.IsInt(sRate) = True And sRate <> "-1" Then
            rate = Utility.ToInt(sRate)
        End If

        Dim index As Integer? = Nothing
        Dim sIndex As String = Utility.Trim(TextBox_Test_Index.Text)
        If Utility.IsInt(sIndex) = True And sIndex <> "-1" Then
            index = Utility.ToInt(sIndex)
        End If

        Dim tags As Tags = Nothing
        Dim sTags As String = Utility.Trim(TextBox_Test_iTags.Text)
        If Utility.IsEmpty(sTags) = False Then
            tags = New Tags(sTags)
        End If

        Story.Explorer(input, rate, index, tags)
        Dim results = Story.Commit()

        If results = True Then
            Story.Jump()
            Refresh()
            TextBox_Test_Output.Text = Story.GetRandomNote()
            TextBox_Test_oTags.Text = Story.GetTags
        Else
            TextBox_Test_Output.Text = ""
            TextBox_Test_oTags.Text = ""
        End If

        TextBox_Test_Input.Clear()

    End Sub
    Private Sub TextBox_Input_Rate_Type(ByVal sender As Object, ByVal e As TextChangedEventArgs) Handles TextBox_Rate.TextChanged
        If Utility.IsInt(TextBox_Rate.Text) = False Then
            Dim newChr = ""
            Dim cnt = 0
            For Each Chrs In TextBox_Rate.Text.ToArray
                If IsNumeric(Chrs) = True Then
                    newChr &= Chrs
                End If
                If Chrs = "-" And cnt = 0 Then
                    newChr &= Chrs
                End If
                cnt += 1
            Next

            TextBox_Rate.Text = " " & newChr & " "
            TextBox_Rate.SelectionStart() = TextBox_Rate.Text.Length + 2
        End If
    End Sub
    Private Sub TextBox_Test_Input_Rate_Type(ByVal sender As Object, ByVal e As TextChangedEventArgs) Handles TextBox_Test_Rate.TextChanged
        If Utility.IsInt(TextBox_Test_Rate.Text) = False Then
            Dim newChr = ""
            Dim cnt = 0
            For Each Chrs In TextBox_Test_Rate.Text.ToArray
                If IsNumeric(Chrs) = True Then
                    newChr &= Chrs
                End If
                If Chrs = "-" And cnt = 0 Then
                    newChr &= Chrs
                End If
                cnt += 1
            Next

            TextBox_Test_Rate.Text = " " & newChr & " "
            TextBox_Test_Rate.SelectionStart() = TextBox_Test_Rate.Text.Length + 2
        End If
    End Sub
    Private Sub TextBox_Test_Input_Index_Type(ByVal sender As Object, ByVal e As TextChangedEventArgs) Handles TextBox_Test_Index.TextChanged
        If Utility.IsInt(TextBox_Test_Index.Text) = False Then
            Dim newChr = ""
            Dim cnt = 0
            For Each Chrs In TextBox_Test_Index.Text.ToArray
                If IsNumeric(Chrs) = True Then
                    newChr &= Chrs
                End If
                If Chrs = "-" And cnt = 0 Then
                    newChr &= Chrs
                End If
                cnt += 1
            Next

            TextBox_Test_Index.Text = " " & newChr & " "
            TextBox_Test_Index.SelectionStart() = TextBox_Test_Index.Text.Length + 2
        End If
    End Sub

#End Region


#Region "Convers Parse"

    Private Sub Parse_Event() Handles Button_Parse.LeftClick
        Parse_Converse_Content() = ScriptEdit_Parser.Text()

        If ParseConverse_Thread.IsBusy = False Then
            ParseConverse_Thread.RunWorkerAsync()
        End If
    End Sub
    Private Sub ClearParse_Event() Handles Button_ClearParse.LeftClick
        ScriptEdit_Parser.Clear()
    End Sub
    Private Sub LoadParse_Event() Handles Button_LoadParse.LeftClick

        FolderPicker.Filter() = {Ext.TEXT}
        FolderPicker.InitialDirectory() = Folders.Athena()

        Dim result = FolderPicker.Show()

        If result = FolderPicker.OK Then
            Load_Converse_Location() = FolderPicker.FullFileName()

            If LoadConverse_Thread.IsBusy = False Then
                LoadConverse_Thread.RunWorkerAsync()
            End If

        End If
    End Sub

    Private Shared Property Load_Converse_Content() As String = ""
    Private Shared Property Load_Converse_Location() As String = ""
    WithEvents LoadConverse_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadConverse_DoWork() Handles LoadConverse_Thread.DoWork
        Load_Converse_Content() = Utility.TextReader(Load_Converse_Location())
    End Sub
    Private Sub LoadConverse_Completed() Handles LoadConverse_Thread.RunWorkerCompleted
        ScriptEdit_Parser.Dispatcher.BeginInvoke(Sub() ScriptEdit_Parser.Text() = Load_Converse_Content())
    End Sub


    Private Shared Property Parse_Converse_Content() As String = ""
    WithEvents ParseConverse_Thread As New ComponentModel.BackgroundWorker
    Private Sub ParseConverse_DoWork() Handles ParseConverse_Thread.DoWork
        Story.Parse(Parse_Converse_Content(), vbCrLf)
        Story.Load()
    End Sub
    Private Sub ParseConverse_Completed() Handles ParseConverse_Thread.RunWorkerCompleted
        Refresh()
    End Sub

#End Region


#Region "XML"

    Private Sub ClearXML() Handles Button_ClearXML.LeftClick
        ScriptEdit_XML.Clear()
    End Sub
    Private Sub SaveXML() Handles Button_SaveXML.LeftClick
        If Utility.IsEmpty(ScriptEdit_XML.Text()) = False Then
            Utility.TextWriter(NodeFolder() & CurrentFile(), ScriptEdit_XML.Text(), False)
            Message.Show(Me, "Save Successful!")
        End If
    End Sub
    Private Sub RefreshXML() Handles Button_RefreshXML.LeftClick
        If LoadXml_Thread.IsBusy = False Then
            LoadXml_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Shared Property Load_XmlContent() As String = ""
    WithEvents LoadXml_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadXml_DoWork() Handles LoadXml_Thread.DoWork
        If Utility.IsEmpty(CurrentFile()) = False Then
            If Utility.FileExists(NodeFolder() & CurrentFile()) = True Then
                Load_XmlContent() = Utility.TextReader(NodeFolder() & CurrentFile())
            End If
        End If
    End Sub
    Private Sub LoadXml_Completed() Handles LoadXml_Thread.RunWorkerCompleted
        ScriptEdit_XML.Dispatcher.BeginInvoke(Sub() ScriptEdit_XML.Text() = Load_XmlContent())
    End Sub

    Private Sub FindXml() Handles Button_FindXML.LeftClick
        ScriptEdit_XML.ShowFindDialog()
    End Sub
    Private Sub ReplaceXml() Handles Button_ReplaceXML.LeftClick
        ScriptEdit_XML.ShowReplaceDialog()
    End Sub
    Private Sub GoToXml() Handles Button_GoToXML.LeftClick
        ScriptEdit_XML.ShowGoToDialog()
    End Sub

#End Region


    Private Sub Main_Splitter_MouseEnter_Event() Handles Main_Splitter.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub Main_Splitter_MouseLeave_Event() Handles Main_Splitter.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub

    Private Shared Property NodeFolder() As String = Folders.Dialogs()
    Private Shared Property CurrentFile() As String = ""

    Private Sub Load_TreeView()
        If Load_TreeView_Thread.IsBusy = False Then
            Load_TreeView_Thread.RunWorkerAsync()
        End If
    End Sub

    Private Sub Load_TreeView_InThread()

        'database/table/column
        Main_TreeView.Items.Clear()

        Dim Main_Head As New TreeViewItem
        Main_Head.Header = "Dialogs"

        Schema.SetStyle(Main_Head)
        Main_Head.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head)

        Dim files = Utility.GetAllFiles(NodeFolder())

        For Each file In files

            If Utility.Search(file, "*" & Ext.XML, Gate.eEND) = True Then

                Dim connection As New TreeViewItem
                connection.Header() = Utility.GetFileName(file, False)
                Schema.SetStyle(connection)
                connection.IsExpanded = True
                Main_Head.Items.Add(connection)

            End If
        Next
    End Sub

    WithEvents Load_TreeView_Thread As New ComponentModel.BackgroundWorker
    Private Sub Load_TreeView_DoWork() Handles Load_TreeView_Thread.DoWork
        Main_TreeView.Dispatcher.BeginInvoke(Sub() Load_TreeView_InThread())
    End Sub
    Private Sub Load_TreeView_Completed() Handles Load_TreeView_Thread.RunWorkerCompleted
    End Sub

    Private Sub Select_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Object)) Handles Main_TreeView.SelectedItemChanged

        If e.NewValue IsNot Nothing Then

            Dim file As String = ""

            Try
                file = TryCast(e.NewValue, TreeViewItem).Header
            Catch
            End Try

            If Utility.IsEmpty(file) = False Then
                Story.Source() = NodeFolder() & file

                If LoadStory_Thread.IsBusy = False Then
                    LoadStory_Thread.RunWorkerAsync()
                End If

                CurrentFile() = file
                SaveReminder() = False
            End If

        End If

    End Sub

    WithEvents LoadStory_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadStory_DoWork() Handles LoadStory_Thread.DoWork
        Story.Loadfile()
    End Sub
    Private Sub LoadStory_Completed() Handles LoadStory_Thread.RunWorkerCompleted
        RefreshXML()
        Refresh()
        SP_AllPages.Index() = 0
        SP_NavPages.Index() = 0
        SP_CopyToPages.Index() = 0
    End Sub


End Class
