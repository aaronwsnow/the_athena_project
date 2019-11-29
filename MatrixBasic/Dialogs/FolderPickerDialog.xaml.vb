Imports Matrix
Imports MatrixUtil

Public Class FolderPickerDialog
    Const DEF_FOLDER As String = "..."

    Private Sub Initialize() Handles Me.Initialized
        Me.Icon() = LocalResources.GetIcon()

        Schema.SetStyle(File_Border)
        Schema.SetStyle(Main_Border)
        Schema.SetStyle(Scroll_Border)
        Schema.SetStyle(CurrentDirectory_Border)
        Schema.SetStyle(TB_CurrentDirectory)
        TB_CurrentDirectory.Background = Schema.Global_Window_Color()
        Schema.SetStyle(TB_File)
        TB_File.Background = Schema.Global_Window_Color()
        Schema.SetStyle(Main_ListView)

        Main_Grid.Background = Schema.Global_Window_Color()
        Main_Menu.ButtonInfo() = {UP & "|" & UP, OK & "|" & OK, CANCEL & "|" & CANCEL}
        Main_Menu.HorizontalAlignment = HorizontalAlignment.Right
        Main_Menu.Border(Schema.Global_Border_Color, New Thickness(0))

    End Sub

    Private Sub OnLoad() Handles Me.Loaded
        If Utility.FolderExists(InitialDirectory()) = False Then
            InitialDirectory() = "C:\"
        End If

        If Utility.IsPathRoot(InitialDirectory()) = True Then
            TB_CurrentDirectory.Text() = InitialDirectory()
        Else
            TB_CurrentDirectory.Text() = Utility.GetPathRoot(InitialDirectory()) & "..\" & Utility.GetFolderName(InitialDirectory()) & "\"
        End If

        CurrentDirectory() = InitialDirectory()
        TB_File.Text() = InitialFile()

        If NoFiles() = True Then
            File_Border.Visibility = Visibility.Collapsed
        End If

        Load_ListBox()
    End Sub

    Public Property InitialDirectory() As String = "C:/"
    Public Property InitialFile() As String = ""
    Private Property CurrentDirectory() As String = ""
    Public Property Filter() As String() = {""}
    Public Property Extension() As String = ""

    Private m_NoFiles As Boolean = False
    Public Property NoFiles() As Boolean
        Get
            Return m_NoFiles
        End Get
        Set(value As Boolean)
            If value = True Then
                File_Border.Visibility = Visibility.Collapsed
            Else
                File_Border.Visibility = Visibility.Visible
            End If
            m_NoFiles = value
        End Set
    End Property


    Public Const UP As String = "UP"
    Public Const OK As String = "OK"
    Public Const CANCEL As String = "CANCEL"

    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick
        If button.Name() = UP Then

            If Utility.IsPathRoot(CurrentDirectory()) = False Then

                Dim parent = Utility.GetParentFolder(CurrentDirectory())

                If Utility.IsPathRoot(parent) = True Then
                    CurrentDirectory() = parent
                Else
                    CurrentDirectory() = parent & "\"
                End If

                Load_ListBox()
            End If

            If Utility.IsPathRoot(CurrentDirectory()) = True Then
                TB_CurrentDirectory.Text() = CurrentDirectory()
            Else
                TB_CurrentDirectory.Text() = Utility.GetPathRoot(CurrentDirectory()) & "..\" & Utility.GetFolderName(CurrentDirectory()) & "\"
            End If

        ElseIf button.Name() = OK Then
            Dim filename = Utility.Trim(Utility.RemoveExtension(TB_File.Text()) & Extension())

            Returns() = button.Name()
            ReturnDirectory() = CurrentDirectory()
            SafeFileName() = filename
            FullFileName() = CurrentDirectory() & filename
            Close()
        Else
            Close()
        End If

    End Sub

    Public Property Returns() As String = ""
    Public Property ReturnDirectory() As String = ""
    Public Property SafeFileName() As String = ""
    Public Property FullFileName() As String = ""

    Private Property IsFile() As Boolean = False
    Private Sub ListView_Select(ByVal sender As Object, ByVal e As SelectionChangedEventArgs) Handles Main_ListView.SelectionChanged
        Dim text As String = Main_ListView.SelectedItem

        If text IsNot Nothing Then
            If Utility.Search(text, DEF_FOLDER, Gate.eEND) = True Then
                text = Utility.Remove(text, DEF_FOLDER)
                TB_File.Text() = ""
                CurrentDirectory() = CurrentDirectory() & text & "\"
                IsFile() = False
            Else
                TB_File.Text() = text
                IsFile() = True
            End If

        End If
    End Sub

    Private Sub Load_ListBox()
        If Utility.FolderExists(CurrentDirectory()) = True Then
            Main_ListView.Items.Clear()
            For Each folder In Utility.GetAllFolders(CurrentDirectory(), False)
                Main_ListView.Items.Add(Utility.GetFolderName(folder) & DEF_FOLDER)
            Next

            If NoFiles() = False Then
                For Each file In Utility.GetAllFiles(CurrentDirectory())
                    If Utility.Search(file, Filter(), Gate.eEQUALS) = True Then
                        Main_ListView.Items.Add(Utility.GetFileName(file, False))
                    End If
                Next
            End If
        Else
            Message.Show(Me, {"File does not Exist:", CurrentDirectory()})
        End If
    End Sub

    Private Sub Selected_ListBoxItem() Handles Main_ListView.MouseDoubleClick
        If IsFile() = False Then
            If Utility.IsPathRoot(CurrentDirectory()) = True Then
                TB_CurrentDirectory.Text() = CurrentDirectory()
            Else
                TB_CurrentDirectory.Text() = Utility.GetPathRoot(CurrentDirectory()) & "..\" & Utility.GetFolderName(CurrentDirectory()) & "\"
            End If

            Load_ListBox()
        End If
    End Sub

    Private Sub FileDirectory_Enter_Event() Handles TB_File.GotFocus
        TB_File.Clear()
    End Sub

    Private Sub CurrentDirectory_Enter_Event() Handles TB_CurrentDirectory.GotFocus
        TB_CurrentDirectory.Text() = CurrentDirectory()
    End Sub
    Private Sub CurrentDirectory_Exit_Event() Handles TB_CurrentDirectory.LostFocus
        If Utility.IsPathRoot(CurrentDirectory()) = True Then
            TB_CurrentDirectory.Text() = CurrentDirectory()
        Else
            TB_CurrentDirectory.Text() = Utility.GetPathRoot(CurrentDirectory()) & "..\" & Utility.GetFolderName(CurrentDirectory()) & "\"
        End If

        Load_ListBox()
    End Sub

    Private Sub CurrentDirectory_TextChanged_Event() Handles TB_CurrentDirectory.TextChanged
        If TB_CurrentDirectory.IsFocused = True Then
            CurrentDirectory() = Utility.Trim(TB_CurrentDirectory.Text)
        End If
    End Sub

End Class

Public Class FolderPicker
    Private Shared Pick As New FolderPickerDialog

    Public Const OK As String = FolderPickerDialog.OK
    Public Const CANCEL As String = FolderPickerDialog.CANCEL

    Public Shared Function Show(ByVal x As Double, ByVal y As Double) As String
        Location() = New Point(x, y)
        Show()
        Return Pick.Returns()
    End Function
    Public Shared Function Show() As String
        Pick = New FolderPickerDialog
        Pick.InitialDirectory() = InitialDirectory()
        Pick.InitialFile() = InitialFile()
        Pick.Filter() = Filter
        Pick.NoFiles() = NoFiles()
        Pick.Extension() = Extension()

        If Location() <> New Point(-1, -1) Then
            Pick.Left = Location.X
            Pick.Top = Location.Y
        Else
            Dim center = Local.FindCenter(Schema.Global_Postion(), Schema.Global_Size(), Size())
            Pick.Left = center.X
            Pick.Top = center.Y
        End If

        Pick.Width = Size.Width
        Pick.Height = Size.Height

        Pick.ShowDialog()
        Return Pick.Returns()
    End Function

    Public Shared Property InitialDirectory() As String = ""
    Public Shared Property InitialFile() As String = ""
    Public Shared Property Extension() As String = ""
    Public Shared ReadOnly Property ReturnDirectory() As String
        Get
            Return Pick.ReturnDirectory()
        End Get
    End Property
    Public Shared ReadOnly Property SafeFileName() As String
        Get
            Return Pick.SafeFileName()
        End Get
    End Property
    Public Shared ReadOnly Property FullFileName() As String
        Get
            Return Pick.FullFileName()
        End Get
    End Property
    Public Shared Property Filter() As String() = {""}
    Public Shared Property NoFiles() As Boolean = False

    Public Shared Property Location() As New Point(-1, -1)
    Public Shared Property Size() As New Size(600, 400)

End Class
