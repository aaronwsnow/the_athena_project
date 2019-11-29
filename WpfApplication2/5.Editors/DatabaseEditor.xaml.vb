Imports System.Threading
Imports Matrix
Imports MatrixUtil
Imports MatrixBasic

Public Class DatabaseEditor

    Const DEF_ADDRESS_DELIM As String = " > "
    Const DEF_NEW_CLUSTER As String = "(New)Cluster"
    Const DEF_NEW_DATABASE As String = "(New)Database"
    Const DEF_NEW_TABLE As String = "(New)Table"

    Private Sub Initialize() Handles Me.Initialized

        Main_DataGrid.FontFamily = Schema.Global_Font()
        Main_DataGrid.FontSize = Schema.Global_FontSize()
        Main_DataGrid.Foreground = Schema.Global_FontColor()
        Main_DataGrid.BorderBrush = Schema.Global_Border_Color()
        Main_DataGrid.Background = Schema.Global_Window_Color()

        Main_DataGrid.ButtonBackground() = Schema.Global_FontBackColor()
        Main_DataGrid.ButtonBorder(Schema.Global_Border_Color, New Thickness(Schema.Global_Border_Size()))
        Main_DataGrid.ButtonNormalFont() = Schema.Normal_Fonts()
        Main_DataGrid.ButtonHoverFont() = Schema.Hover_Fonts()

        Main_Grid.Background() = Schema.Global_Window_Color
        Main_TreeView.BorderBrush = Schema.Global_Border_Color
        Main_Menu.ButtonInfo() = {"File|File", "Commit|Commit", "Save|Save"}

        Main_PBar.Foreground() = Schema.Global_Border_Color()
        Main_PBar.Background() = Schema.Global_FontColor()

    End Sub

    Private Shared Property CurrentCluster() As New Data
    Private Shared Property CurrentDatabase() As String = ""
    Private Shared Property CurrentTable() As String = ""
    Private Shared Property CurrentConnection() As String = ""
    Private Shared Property DatabaseFolder() As String = Folders.Databases()

    Private Shared Property ClusterList() As New List(Of Data)
    Private Shared ReadOnly Property CopyClusterList() As List(Of Data)
        Get
            Dim CopyList As New List(Of Data)
            CopyList.AddRange(ClusterList())
            Return CopyList
        End Get
    End Property


    Private Sub Main_Splitter_MouseEnter_Event() Handles Main_Splitter.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub Main_Splitter_MouseLeave_Event() Handles Main_Splitter.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub

    Private Sub Load() Handles Me.Loaded
        ReLoad_DB() = True
        Load_TreeView()
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

        If button.Name() = "Commit" Then
            If Main_DataGrid.IsSearch() = True Then
                Search()
            ElseIf Main_DataGrid.IsUpdate() = True Then
                Update()
            ElseIf Main_DataGrid.IsCopy() = True Then
                Copy()
            ElseIf Main_DataGrid.IsDelete() = True Then
                Delete()
            ElseIf Main_DataGrid.IsNew() = True Then
                NewFile()
            End If
            Exit Sub
        End If

        If button.Name() = "Save" Then
            Save()
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
            Refresh()
            Exit Sub
        End If

    End Sub


    Private Shared Property SearchColumn() As String = ""
    Private Shared Property SearchRow() As String = ""

    Private Sub SelectedAddress_Event(ByVal e As String) Handles Main_DataGrid.SelectedAddress
        'Loads the table with found table address

        Dim add = Utility.ToArray(e, DEF_ADDRESS_DELIM)

        CurrentDatabase() = Utility.GetIndex(add, 0, "")
        CurrentTable() = Utility.GetIndex(add, 1, "")

        SearchColumn() = Utility.GetIndex(add, 2, "")
        SearchRow() = Utility.GetIndex(add, 3, "")

        LoadTable()
    End Sub

    Private Function Cluster_Address() As String
        Return DatabaseFolder() & CurrentConnection()
    End Function


#Region "Save"

    Private Sub Update()
        Main_PBar.IsIndeterminate = False

        'update to database
        If UpdateData_Thread.IsBusy() = False Then
            Main_PBar.IsIndeterminate = True
            PreGridToDatabase()
            UpdateData_Thread.RunWorkerAsync()
        End If

    End Sub

    Private Sub Save()
        Main_PBar.IsIndeterminate = False

        If HardSave_Thread.IsBusy() = False Then
            Main_PBar.IsIndeterminate = True
            HardSave_Thread.RunWorkerAsync()
        End If

    End Sub

    WithEvents HardSave_Thread As New ComponentModel.BackgroundWorker
    Private Sub HardSave_DoWork() Handles HardSave_Thread.DoWork

        'if we are loading one table at a time then we need to save only those tables
        'this means that we need to physically delete old tables and replace them
        'it also means we need to keep up with all clusters, databases, and tables we rename

        For Each CL In CopyClusterList()
            CL.RemoveDeletedFromZip()

            For Each DB In CL.DatabasesList
                For Each TB In CL.TableList(DB)
                    If CL.DoesAnyExistInTable(DB, TB) = True Then
                        CL.SaveTableToZip(DB, TB)
                    End If
                Next
            Next

            CL.Clean()
            UpdateCluster(CL)
        Next

    End Sub
    Private Sub HardSave_Completed() Handles HardSave_Thread.RunWorkerCompleted
        Main_PBar.IsIndeterminate = False
        Message.Show(Me, "Save Successful!")
    End Sub

#End Region


#Region "Region"

    Private Sub Search()

        Dim find = Main_DataGrid.SearchInput()
        Dim stype = Main_DataGrid.SearchType()

        If Main_DataGrid.IsCluster() = True Then
            Search_Mode() = 0
        ElseIf Main_DataGrid.IsDatabase() = True Then
            Search_Mode() = 1
        ElseIf Main_DataGrid.IsTable() = True Then
            Search_Mode() = 2
        End If

        Main_PBar.IsIndeterminate = False

        If Search_Thread.IsBusy() = False Then
            Main_PBar.IsIndeterminate = True
            Search_Thread.RunWorkerAsync()
        End If

    End Sub

    Private Shared Property Search_Addresses() As New List(Of String)
    Private Shared Property Search_Mode() As Integer = 0
    Private Shared Property Search_Find() As String = ""
    Private Shared Property Search_SType() As Gate = Gate.eSEARCH
    WithEvents Search_Thread As New ComponentModel.BackgroundWorker
    Private Sub Search_DoWork() Handles Search_Thread.DoWork
        Dim results As New List(Of Data.Cell)

        Select Case Search_Mode
            Case 0
                LoadClusterByPartials()
                results = CurrentCluster().SearchDatabases(Search_Find(), Search_SType())
            Case 1
                LoadDatabaseByPartials(CurrentDatabase())
                results = CurrentCluster().SearchByDatabase(CurrentDatabase(), Search_Find(), Search_SType())
            Case 2
                LoadTableByPartials(CurrentDatabase(), CurrentTable())
                results = CurrentCluster().SearchByTable(CurrentDatabase(), CurrentTable(), Search_Find(), Search_SType())
        End Select

        Dim addresses As New List(Of String)
        For Each cell In results
            addresses.Add(cell.Database() & DEF_ADDRESS_DELIM & cell.Table & DEF_ADDRESS_DELIM & cell.Column & DEF_ADDRESS_DELIM & cell.Row)
        Next

        Search_Addresses() = addresses
    End Sub
    Private Sub Search_Completed() Handles Search_Thread.RunWorkerCompleted
        Main_DataGrid.Dispatcher.BeginInvoke(Sub() Main_DataGrid.SearchAddress() = Search_Addresses())
        Main_PBar.IsIndeterminate = False
    End Sub

#End Region


#Region "Copy"

    Private Sub LoadClusterByPartials()

        If CurrentCluster.DoesAnyExist() = False Then
            CurrentCluster.LoadClusterDirectZip()
            Exit Sub
        End If

        Dim db_count = 0
        Dim databases = CurrentCluster.DatabasesList()
        For Each database In databases
            If CurrentCluster.DoesAnyExistInDatabase(database) = False Then
                CurrentCluster.LoadDatabaseDirectZip(database)
                db_count += 1
            End If
        Next

        If databases.Count() = db_count Then Exit Sub

        For Each database In CurrentCluster.DatabasesList()
            For Each table In CurrentCluster.TableList(database)
                If CurrentCluster.DoesAnyExistInTable(database, table) = False Then
                    CurrentCluster.LoadTableDirectZip(database, table)
                End If
            Next
        Next

    End Sub
    Private Sub LoadDatabaseByPartials(ByVal database As String)

        If CurrentCluster.DoesAnyExistInDatabase(database) = False Then
            CurrentCluster.LoadDatabaseDirectZip(database)
            Exit Sub
        End If

        For Each table In CurrentCluster.TableList(database)
            If CurrentCluster.DoesAnyExistInTable(database, table) = False Then
                CurrentCluster.LoadTableDirectZip(database, table)
            End If
        Next

    End Sub
    Private Sub LoadTableByPartials(ByVal database As String, ByVal table As String)

        If CurrentCluster.DoesAnyExistInTable(database, table) = False Then
            CurrentCluster.LoadTableDirectZip(database, table)
        End If

    End Sub

    Private Shared Property CopyMode() As Integer = 0

    WithEvents Copy_Thread As New ComponentModel.BackgroundWorker
    Private Sub Copy_DoWork() Handles Copy_Thread.DoWork
        Dim newCluster As New Data

        Select Case CopyMode()
            Case 0
                'CurrentCluster.CopyZip(DatabaseFolder() & new_CurrentConnection) 'Physical Copy!
                'Loads an empty/part empty cluster which we intend to copy
                LoadClusterByPartials()

                newCluster = CurrentCluster.Copy()
                If Utility.IsEmpty(new_CurrentConnection) = True Then new_CurrentConnection = DEF_NEW_CLUSTER
                new_CurrentConnection = ChooseClusterName(new_CurrentConnection, "-Copy")
                NewCluster.Connection() = DatabaseFolder() & new_CurrentConnection

                ClusterList.Add(newCluster)
            Case 1

                If Utility.IsEmpty(new_CurrentConnection) = True Then new_CurrentConnection = DEF_NEW_CLUSTER
                If Utility.IsEmpty(new_CurrentDatabase) = True Then new_CurrentDatabase = DEF_NEW_DATABASE

                'Loads an empty/part empty database which we intend to copy
                LoadDatabaseByPartials(old_CurrentDatabase)

                If Utility.Search(old_CurrentConnection, new_CurrentConnection, Gate.eEQUALS) = False Then

                    If DoesClusterExist(new_CurrentConnection) = False Then
                        newCluster.Connection() = DatabaseFolder() & new_CurrentConnection

                        Dim old_Cells = CurrentCluster.GetDatabase(old_CurrentDatabase)
                        Dim new_Cells As New List(Of Data.Cell)
                        For Each cell In old_Cells
                            cell.Database() = new_CurrentDatabase
                            new_Cells.Add(cell)
                        Next

                        newCluster.SetCells(new_Cells)
                        ClusterList.Add(newCluster)
                    Else

                        Dim old_Cluster = GetCluster(new_CurrentConnection)
                        new_CurrentDatabase = ChooseDatabaseName(old_Cluster, new_CurrentDatabase, "-Copy")
                        old_Cluster.Copy(old_CurrentDatabase, new_CurrentDatabase)
                        UpdateCluster(old_Cluster)
                    End If

                Else
                    new_CurrentDatabase = ChooseDatabaseName(CurrentCluster(), new_CurrentDatabase, "-Copy")
                    CurrentCluster().Copy(old_CurrentDatabase, new_CurrentDatabase)
                    UpdateCurrentCluster()
                End If

            Case 2

                If Utility.IsEmpty(new_CurrentConnection) = True Then new_CurrentConnection = DEF_NEW_CLUSTER
                If Utility.IsEmpty(new_CurrentDatabase) = True Then new_CurrentDatabase = DEF_NEW_DATABASE
                If Utility.IsEmpty(new_CurrentTable) = True Then new_CurrentTable = DEF_NEW_TABLE

                'Loads an empty table in a database which we intend to copy
                LoadTableByPartials(old_CurrentDatabase, old_CurrentTable)

                If Utility.Search(old_CurrentConnection, new_CurrentConnection, Gate.eEQUALS) = False Then

                    If DoesClusterExist(new_CurrentConnection) = False Then
                        newCluster.Connection() = DatabaseFolder() & new_CurrentConnection

                        Dim old_Cells = CurrentCluster.GetTable(old_CurrentDatabase, old_CurrentTable)
                        Dim new_Cells As New List(Of Data.Cell)
                        For Each cell In old_Cells
                            cell.Database() = new_CurrentDatabase
                            cell.Table() = new_CurrentTable
                            new_Cells.Add(cell)
                        Next

                        newCluster.SetCells(new_Cells)
                        ClusterList.Add(newCluster)
                    Else

                        Dim old_Cluster = GetCluster(new_CurrentConnection)
                        new_CurrentTable = ChooseTableName(CurrentCluster(), new_CurrentDatabase, new_CurrentTable, "-Copy")
                        old_Cluster.Copy(old_CurrentDatabase, old_CurrentTable, new_CurrentDatabase, new_CurrentTable)
                        UpdateCluster(old_Cluster)
                    End If

                Else
                    new_CurrentTable = ChooseTableName(CurrentCluster(), new_CurrentDatabase, new_CurrentTable, "-Copy")
                    CurrentCluster().Copy(old_CurrentDatabase, old_CurrentTable, new_CurrentDatabase, new_CurrentTable)
                    UpdateCurrentCluster()
                End If

        End Select


    End Sub

    Private Sub Copy_Completed() Handles Copy_Thread.RunWorkerCompleted
        Load_TreeView()
        Main_PBar.IsIndeterminate = False
        Message.Show(Me, "Copy Successful!")
    End Sub

    Private Sub Copy()

        old_CurrentConnection = CurrentConnection()
        new_CurrentConnection = Main_DataGrid.ClusterName()

        old_CurrentDatabase = CurrentDatabase()
        new_CurrentDatabase = Main_DataGrid.DatabaseName()

        old_CurrentTable = CurrentTable()
        new_CurrentTable = Main_DataGrid.TableName()


        If Main_DataGrid.IsCluster = True Then
            CopyMode() = 0
        ElseIf Main_DataGrid.IsDatabase = True Then
            CopyMode() = 1
        ElseIf Main_DataGrid.IsTable = True Then
            CopyMode() = 2
        End If

        Main_PBar.IsIndeterminate = False

        If Copy_Thread.IsBusy() = False Then
            Main_PBar.IsIndeterminate = True
            Copy_Thread.RunWorkerAsync()
        End If
    End Sub

#End Region


#Region "New"

    Private Sub NewFile()

        If Main_DataGrid.IsCluster() = True Then
            NewCluster(False)
        ElseIf Main_DataGrid.IsDatabase() = True Then
            NewDatabase(False)
        ElseIf Main_DataGrid.IsTable() = True Then
            NewTable()
        End If

    End Sub
    Private Sub NewCluster(ByVal grouped As Boolean)

        Dim new_Name = Main_DataGrid.ClusterName()
        new_Name = ChooseClusterName(new_Name, "-New")

        CurrentCluster() = New Data
        CurrentCluster().Connection() = DatabaseFolder() & new_Name
        ClusterList().Add(CurrentCluster())

        CurrentConnection() = new_Name
        CurrentDatabase() = ""
        CurrentTable() = ""

        If grouped = False Then
            Main_DataGrid.Clear()
            Main_DataGrid.ClusterName() = CurrentConnection()
            Load_TreeView()
        End If

    End Sub
    Private Sub NewDatabase(ByVal grouped As Boolean)

        If Utility.IsEmpty(CurrentConnection()) = True Or
            Utility.Search(CurrentConnection(), Main_DataGrid.ClusterName(), Gate.eEQUALS) = False Then
            NewCluster(True)
        End If

        Dim new_Name = Main_DataGrid.DatabaseName()
        new_Name = ChooseDatabaseName(CurrentCluster(), new_Name, "-New")

        CurrentCluster().AddDatabase(new_Name)
        UpdateCurrentCluster()
        CurrentDatabase() = new_Name
        CurrentTable() = ""

        If grouped = False Then
            Main_DataGrid.Clear()
            Main_DataGrid.ClusterName() = CurrentConnection()
            Main_DataGrid.DatabaseName() = CurrentDatabase()
            Load_TreeView()
        End If
    End Sub
    Private Sub NewTable()

        If Utility.IsEmpty(CurrentConnection()) = True Or
            Utility.Search(CurrentConnection(), Main_DataGrid.ClusterName(), Gate.eEQUALS) = False Then
            NewCluster(True)
        End If

        If Utility.IsEmpty(CurrentDatabase()) = True Or
            Utility.Search(CurrentDatabase(), Main_DataGrid.DatabaseName(), Gate.eEQUALS) = False Then
            NewDatabase(True)
        End If

        Dim new_Name = Main_DataGrid.TableName()
        new_Name = ChooseTableName(CurrentCluster(), CurrentDatabase(), new_Name, "-New")

        CurrentCluster().AddTable(CurrentDatabase(), new_Name)
        UpdateCurrentCluster()
        CurrentTable() = new_Name

        Main_DataGrid.Clear()
        Main_DataGrid.ClusterName() = CurrentConnection()
        Main_DataGrid.DatabaseName() = CurrentDatabase()
        Main_DataGrid.TableName() = CurrentTable()
        Load_TreeView()
    End Sub


    Private Function ChooseClusterName(ByVal name As String, ByVal post As String) As String
        Dim newExtens = ""

        If Utility.IsEmpty(name) = True Then
            name = DEF_NEW_CLUSTER
        End If

        If Utility.Search(name, Ext.ZIP, Gate.eEND) = True Then
            newExtens = Ext.ZIP
        Else
            newExtens = Ext.DATA
        End If

        name = Utility.RemoveExtension(name)

        Dim count = 1
        Dim newCopy = name
        name = name & newExtens
        Do Until DoesClusterExist(name) = False
            name = newCopy & "(" & count & ")" & post & newExtens
            count += 1
        Loop

        Return name
    End Function
    Private Function ChooseDatabaseName(ByVal cluster As Data, ByVal name As String, ByVal post As String) As String
        If Utility.IsEmpty(name) = True Then
            name = DEF_NEW_DATABASE
        End If

        Dim count = 1
        Dim newCopy = name
        Do Until cluster.DoesDatabaseExistByName(name) = False
            name = newCopy & "(" & count & ")" & post
            count += 1
        Loop

        Return name
    End Function
    Private Function ChooseTableName(ByVal cluster As Data, ByVal database As String, ByVal name As String, ByVal post As String) As String
        If Utility.IsEmpty(name) = True Then
            name = DEF_NEW_TABLE
        End If

        Dim count = 1
        Dim newCopy = name
        Do Until cluster.DoesTableExistByName(database, name) = False
            name = newCopy & "(" & count & ")" & post
            count += 1
        Loop

        Return name
    End Function

#End Region



#Region "Delete"
    Private Sub Delete()

        If Main_DataGrid.IsCluster() = True Then
            DeleteCluster()
        ElseIf Main_DataGrid.IsDatabase() = True Then
            DeleteDatabase()
        ElseIf Main_DataGrid.IsTable() = True Then
            DeleteTable()
        End If

    End Sub
    Private Sub DeleteCluster()
        If Utility.IsEmpty(CurrentConnection()) = False Then
            Dim results = Message.Show(Me, "Are you sure you want to delete """ & CurrentConnection() & """ cluster" & "?")

            If results = Message.OK Then
                Utility.FileDelete(DatabaseFolder() & CurrentConnection())
                RemoveCurrentCluster()

                Load_TreeView()
                Main_DataGrid.Clear()
                CurrentConnection() = ""
                CurrentDatabase() = ""
                CurrentTable() = ""
            End If
        Else
            Message.Show(Me, "You need to select a cluster before it can be deleted.")
        End If
    End Sub
    Public Sub DeleteDatabase()

        If Utility.IsEmpty(CurrentDatabase()) = False Then
            Dim results = Message.Show(Me, "Are you sure you want to delete """ & CurrentDatabase() & """ database" & "?")

            If results = Message.OK Then
                CurrentCluster().RemoveDatabase(CurrentDatabase())
                UpdateCurrentCluster()
                Load_TreeView()
                Main_DataGrid.Clear()
                Main_DataGrid.ClusterName() = CurrentConnection()
                CurrentDatabase() = ""
                CurrentTable() = ""
            End If
        Else
            Message.Show(Me, "You need to select a database before it can be deleted.")
        End If
    End Sub
    Public Sub DeleteTable()
        If Utility.IsEmpty(CurrentTable()) = False Then
            Dim results = Message.Show(Me, "Are you sure you want to delete """ & CurrentTable() & """ table" & "?")

            If results = Message.OK Then
                CurrentCluster().RemoveTable(CurrentDatabase(), CurrentTable())
                UpdateCurrentCluster()
                Load_TreeView()
                Main_DataGrid.Clear()
                Main_DataGrid.ClusterName() = CurrentConnection()
                Main_DataGrid.DatabaseName() = CurrentDatabase()
                CurrentTable() = ""
            End If
        Else
            Message.Show(Me, "You need to select a table before it can be deleted.")
        End If
    End Sub


#End Region


    Private Sub OpenFolder()
        Utility.OpenFolder(DatabaseFolder())
    End Sub

    Public Sub Refresh()
        Load_TreeView()
        LoadTable()
    End Sub


#Region "Update"

    WithEvents UpdateData_Thread As New ComponentModel.BackgroundWorker
    Private Sub UpdateData_DoWork() Handles UpdateData_Thread.DoWork
        MidGridToDatabase()
    End Sub
    Private Sub UpdateData_Completed() Handles UpdateData_Thread.RunWorkerCompleted
        PostGridToDatabase()
        UpdateCurrentCluster()
        Load_TreeView()
        Main_PBar.IsIndeterminate = False
        Message.Show(Me, "Update Successful!")
    End Sub


    Private Shared Property GridLocalColumns() As New List(Of String)
    Private Shared Property GridLocalRows() As New List(Of String)
    Private Shared Property GridLocalTable() As New List(Of List(Of String))
    Private Shared Property GridDataCells() As New List(Of Data.Cell)

    Private Shared Property old_CurrentConnection() As String
    Private Shared Property old_CurrentDatabase() As String
    Private Shared Property old_CurrentTable() As String

    Private Shared Property new_CurrentConnection() As String
    Private Shared Property new_CurrentDatabase() As String
    Private Shared Property new_CurrentTable() As String


    Public Sub PreGridToDatabase()
        GridDataCells.Clear()
        GridLocalColumns() = Main_DataGrid.LocalColumns()
        GridLocalRows() = Main_DataGrid.LocalRows()
        GridLocalTable() = Main_DataGrid.LocalTable()

        old_CurrentConnection() = CurrentConnection()
        old_CurrentDatabase() = CurrentDatabase()
        old_CurrentTable() = CurrentTable()

        CurrentConnection() = Main_DataGrid.ClusterName()
        CurrentDatabase() = Main_DataGrid.DatabaseName()
        CurrentTable() = Main_DataGrid.TableName()
    End Sub
    Public Sub MidGridToDatabase()

        If Utility.IsEmpty(CurrentConnection()) = False And
            Utility.IsEmpty(CurrentDatabase()) = False And
            Utility.IsEmpty(CurrentTable()) = False Then

            If GridLocalColumns.Count() = 0 And GridLocalRows.Count() = 0 Then
                GridDataCells().Add(New Data.Cell(CurrentDatabase(), CurrentTable()))

            ElseIf GridLocalColumns.Count() > 0 And GridLocalRows.Count() = 0 Then
                For Each col In GridLocalColumns
                    GridDataCells().Add(New Data.Cell(CurrentDatabase(), CurrentTable(), col, ""))
                Next

            ElseIf GridLocalColumns.Count() = 0 And GridLocalRows.Count() > 0 Then
                For Each row In GridLocalRows
                    GridDataCells().Add(New Data.Cell(CurrentDatabase(), CurrentTable(), "", row))
                Next

            ElseIf GridLocalColumns.Count() > 0 And GridLocalRows.Count() > 0 Then
                For i = 0 To GridLocalColumns.Count() - 1
                    For j = 0 To GridLocalRows.Count() - 1
                        Dim column = GridLocalColumns(i)
                        Dim row = GridLocalRows(j)

                        If Utility.IsEmpty(column) = False And Utility.IsEmpty(row) = False Then
                            Dim cell = Utility.GetIndex(GridLocalTable(), i, j, "")
                            GridDataCells().Add(New Data.Cell(CurrentDatabase(), CurrentTable(), column, row, cell))
                        End If
                    Next
                Next
            End If
        End If
    End Sub
    Public Sub PostGridToDatabase()
        'RENAME AND/OR UPDATE
        If Utility.IsEmpty(CurrentConnection()) = False And
            Utility.IsEmpty(CurrentDatabase()) = False And
            Utility.IsEmpty(CurrentTable()) = False Then

            If Utility.Search(old_CurrentConnection, CurrentConnection(), Gate.eEQUALS) = False Then
                CurrentCluster().RenameZip(CurrentConnection()) 'Physical Rename!
                CurrentCluster().Connection() = DatabaseFolder() & CurrentConnection()
            End If

            CurrentCluster().SetTable(old_CurrentDatabase, old_CurrentTable, GridDataCells())
        End If
    End Sub

#End Region


#Region "Load"

    Private Sub Select_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Object)) Handles Main_TreeView.SelectedItemChanged

        If e.NewValue IsNot Nothing Then

            Dim header As String = ""
            Dim connection As String = ""
            Dim database As String = ""
            Dim table As String = ""
            Dim column As String = ""

            Try
                header = TryCast(e.NewValue.Parent.Parent.Parent.Parent, TreeViewItem).Header
                connection = TryCast(e.NewValue.Parent.Parent.Parent, TreeViewItem).Header
                database = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                table = TryCast(e.NewValue.Parent, TreeViewItem).Header
                column = TryCast(e.NewValue, TreeViewItem).Header
            Catch

                Try
                    header = TryCast(e.NewValue.Parent.Parent.Parent, TreeViewItem).Header
                    connection = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                    database = TryCast(e.NewValue.Parent, TreeViewItem).Header
                    table = TryCast(e.NewValue, TreeViewItem).Header
                Catch

                    Try
                        header = TryCast(e.NewValue.Parent.Parent, TreeViewItem).Header
                        connection = TryCast(e.NewValue.Parent, TreeViewItem).Header
                        database = TryCast(e.NewValue, TreeViewItem).Header
                    Catch

                        Try
                            header = TryCast(e.NewValue.Parent, TreeViewItem).Header
                            connection = TryCast(e.NewValue, TreeViewItem).Header
                        Catch
                        End Try
                    End Try
                End Try
            End Try

            CurrentConnection() = connection
            CurrentDatabase() = database
            CurrentTable() = table

            SetCurrentCluster(connection)

            Main_DataGrid.Clear()
            Main_DataGrid.ClusterName() = connection
            Main_DataGrid.DatabaseName() = database
            Main_DataGrid.TableName() = table

            If Utility.IsEmpty(table) = False Then
                Load_Database_Name() = connection
                Load_Database_Thread.RunWorkerAsync()
                Main_PBar.IsIndeterminate = True
            End If

        End If

    End Sub

    WithEvents Load_Database_Thread As New ComponentModel.BackgroundWorker
    Private Shared Property Load_Database_Name() As String = ""
    Private Sub Load_Database_DoWork() Handles Load_Database_Thread.DoWork

        Dim newCluster As New Data
        Dim newClusterList As New List(Of Data)

        'returns database by name
        'updates database in List

        If Utility.IsEmpty(Load_Database_Name()) = False Then

            For Each DB In ClusterList()
                If Utility.Search(DB.Name(), Load_Database_Name(), Gate.eEQUALS) = True Then

                    If DB.DoesAnyExistInTable(CurrentDatabase(), CurrentTable()) = False Then
                        DB.Connection() = DatabaseFolder() & Load_Database_Name()
                        DB.RemoveTable(CurrentDatabase(), CurrentTable())
                        DB.LoadTableDirectZip(CurrentDatabase(), CurrentTable())
                    End If

                    newCluster = DB
                End If

                newClusterList.Add(DB)
            Next

            ClusterList() = newClusterList
        End If

        CurrentCluster() = newCluster
    End Sub
    Private Sub Load_Database_Completed() Handles Load_Database_Thread.RunWorkerCompleted
        LoadTable()
    End Sub

    Public Sub DatabaseToGrid()

        Main_DataGrid.Dispatcher.BeginInvoke(Sub()
                                                 Main_DataGrid.LocalTable() = LocalTable()
                                                 Main_DataGrid.LocalColumns() = LocalColumns()
                                                 Main_DataGrid.LocalRows() = LocalRows()
                                                 Main_DataGrid.ClusterName() = CurrentConnection()
                                                 Main_DataGrid.DatabaseName() = CurrentDatabase()
                                                 Main_DataGrid.TableName() = CurrentTable()
                                                 Main_DataGrid.LoadCells()
                                                 Main_DataGrid.IsDataUpdated() = False
                                             End Sub)
    End Sub

    Private Sub LoadTable()
        LoadTable_Thread.WorkerSupportsCancellation() = True
        LoadTable_Thread.CancelAsync()

        If Utility.IsEmpty(CurrentConnection()) = False Then
            CurrentCluster().Connection() = Cluster_Address()

            If Utility.IsEmpty(CurrentDatabase()) = False Then
                If Utility.IsEmpty(CurrentTable()) = False Then

                    Do Until LoadTable_Thread.IsBusy() = False : Loop
                    If LoadTable_Thread.IsBusy() = False Then
                        LoadTable_Thread.RunWorkerAsync()
                    End If
                End If
            End If
        End If

    End Sub

    Private Shared Property LocalColumns() As New List(Of String)
    Private Shared Property LocalRows() As New List(Of String)
    Private Shared Property LocalTable() As New List(Of List(Of String))
    WithEvents LoadTable_Thread As New ComponentModel.BackgroundWorker
    Private Sub LoadTable_DoWork() Handles LoadTable_Thread.DoWork
        LocalTable.Clear()
        LocalColumns.Clear()
        LocalRows.Clear()

        LocalColumns() = CurrentCluster().ColumnList(CurrentDatabase(), CurrentTable())
        LocalRows() = CurrentCluster().RowList(CurrentDatabase(), CurrentTable())

        'Keep this Just-in-Case (It's slow but accurate)
        If False Then
            For Each col In LocalColumns
                If LoadTable_Thread.CancellationPending() = True Then
                    Exit Sub
                End If

                Dim rowList As New List(Of String)
                For Each row In LocalRows
                    rowList.Add(Utility.GetIndex(CurrentCluster().Entries(CurrentDatabase(), CurrentTable(), col, row), 0, ""))
                Next

                LocalTable.Add(rowList)
            Next
        End If
        '*****

        For Each row In LocalColumns
            If LoadTable_Thread.CancellationPending() = True Then
                Exit Sub
            End If

            LocalTable.Add(CurrentCluster().EntriesByColumn(CurrentDatabase(), CurrentTable(), row))
        Next

    End Sub
    Private Sub LoadTable_Completed() Handles LoadTable_Thread.RunWorkerCompleted
        If LoadTable_Thread.CancellationPending() = True Then
            Exit Sub
        End If

        DatabaseToGrid()

        If Utility.IsEmpty(SearchColumn()) = False Then
            If Utility.IsEmpty(SearchRow()) = False Then
                Main_DataGrid.SetFocus(SearchColumn(), SearchRow())
                SearchColumn() = ""
                SearchRow() = ""
            End If
        End If

        Main_PBar.IsIndeterminate = False
    End Sub

#End Region


#Region "Cluster Tools"

    Private Shared Sub UpdateCurrentCluster()
        Dim newClusterList As New List(Of Data)

        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), CurrentCluster().Name(), Gate.eEQUALS) = True Then
                newClusterList.Add(CurrentCluster())
            Else
                newClusterList.Add(DB)
            End If
        Next

        ClusterList() = newClusterList
    End Sub
    Private Shared Sub UpdateCluster(ByVal cluster As Data)
        Dim newClusterList As New List(Of Data)

        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), cluster.Name(), Gate.eEQUALS) = True Then
                newClusterList.Add(cluster)
            Else
                newClusterList.Add(DB)
            End If
        Next

        ClusterList() = newClusterList
    End Sub

    Private Shared Sub RemoveCurrentCluster()
        Dim newClusterList As New List(Of Data)

        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), CurrentCluster().Name(), Gate.eEQUALS) = False Then
                newClusterList.Add(DB)
            End If
        Next

        ClusterList() = newClusterList
    End Sub
    Private Shared Sub RemoveCluster(ByVal name As String)
        Dim newClusterList As New List(Of Data)

        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), name, Gate.eEQUALS) = False Then
                newClusterList.Add(DB)
            End If
        Next

        ClusterList() = newClusterList
    End Sub

    Private Shared Function DoesClusterExist(ByVal name As String) As Boolean
        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), name, Gate.eEQUALS) = True Then
                Return True
            End If
        Next
        Return False
    End Function

    Private Shared Sub SetCurrentCluster(ByVal name As String)
        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), name, Gate.eEQUALS) = True Then
                CurrentCluster() = DB
            End If
        Next
    End Sub

    Private Shared Function GetCluster(ByVal name As String) As Data
        For Each DB In ClusterList()
            If Utility.Search(DB.Name(), name, Gate.eEQUALS) = True Then
                Return DB
            End If
        Next

        Return Nothing
    End Function

#End Region


#Region "Load Tree"

    Private Property ReLoad_DB() As Boolean = True

    Private Sub Load_TreeView()
        Main_PBar.IsIndeterminate = False

        If TreeView_Thread.IsBusy = False Then
            Main_PBar.IsIndeterminate = True
            TreeView_Thread.RunWorkerAsync()
        End If
    End Sub

    WithEvents TreeView_Thread As New ComponentModel.BackgroundWorker
    Private Sub Load_TreeViewThread_DoWork() Handles TreeView_Thread.DoWork
        If ReLoad_DB() = True Then
            ClusterList.Clear()

            Dim files = Utility.GetAllFiles(DatabaseFolder())
            For Each file In files

                If Utility.Search(file, {"*" & Ext.DATA, "*" & Ext.ZIP}, Gate.eEQUALS) = True Then
                    Dim Local_DB As New Data
                    Local_DB.Connection() = file
                    Local_DB.LoadNamesFromZip()
                    ClusterList.Add(Local_DB)
                End If

            Next

            ReLoad_DB() = False
        End If

        Main_TreeView.Dispatcher.BeginInvoke(Sub() Load_TreeView_InThread())

    End Sub

    Private Sub Load_TreeView_InThread()

        'database/table/column

        Main_TreeView.Items.Clear()

        Dim Main_Head As New TreeViewItem
        Main_Head.Header = "Clusters"

        Schema.SetStyle(Main_Head)
        Main_Head.IsExpanded = True
        Main_TreeView.Items.Add(Main_Head)

        For Each lDatabase In ClusterList()

            Dim connection As New TreeViewItem
            Dim name = lDatabase.Name()
            connection.Header() = name
            Schema.SetStyle(connection)
            connection.IsExpanded = True
            Main_Head.Items.Add(connection)

            For Each DB In lDatabase.DatabasesList()

                Dim database As New TreeViewItem
                database.Header() = DB
                Schema.SetStyle(database)
                database.IsExpanded = True
                connection.Items.Add(database)

                For Each tb In lDatabase.TableList(DB)

                    Dim table As New TreeViewItem
                    table.Header() = tb
                    Schema.SetStyle(table)
                    table.IsExpanded = False
                    database.Items.Add(table)

                    For Each col In lDatabase.ColumnList(DB, tb)

                        Dim column As New TreeViewItem
                        column.Header() = Utility.RemoveExtension(col)
                        Schema.SetStyle(column)
                        column.IsExpanded = False
                        table.Items.Add(column)

                    Next
                Next
            Next
        Next

        Main_PBar.IsIndeterminate = False

    End Sub

    Private Sub Load_TreeViewThread_Completed() Handles TreeView_Thread.RunWorkerCompleted

    End Sub

#End Region

End Class
