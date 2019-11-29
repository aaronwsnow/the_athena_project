Imports Matrix
Imports MatrixBasic
Imports MatrixUtil

Public Class DatabaseEdit

    Private Sub Initialize() Handles Me.Initialized

        Scroll_Columns.Value = 0
        Scroll_Columns.SmallChange = 1
        Scroll_Columns.LargeChange = 5
        Scroll_Columns.Minimum = 0

        Scroll_Rows.Value = 0
        Scroll_Rows.SmallChange = 1
        Scroll_Rows.LargeChange = 10
        Scroll_Rows.Minimum = 0

        Mode_Button.TextBuffer(3, 3, 3, 3)
        Mode_Button.Buffer(0, 0, 0, 0)

        Scope_Button.TextBuffer(3, 3, 3, 3)
        Scope_Button.Buffer(0, 0, 0, 0)

        Dim b_text = "------"
        BT_x0y0_Header.Text() = "----"
        BT_x1y0_Header.Text() = b_text
        BT_x2y0_Header.Text() = b_text
        BT_x3y0_Header.Text() = b_text
        BT_x4y0_Header.Text() = b_text
        BT_x5y0_Header.Text() = b_text


        SP_SearchType.Items.Add("search")
        SP_SearchType.Items.Add("equals")
        SP_SearchType.Items.Add("not")
        SP_SearchType.Items.Add("start")
        SP_SearchType.Items.Add("end")
        'SP_SearchType.Items.Add("and")
        'SP_SearchType.Items.Add("or")
        'SP_SearchType.Items.Add("nand")
        'SP_SearchType.Items.Add("nor")
        'SP_SearchType.Items.Add("seq")
        'SP_SearchType.Items.Add("xnor")
        'SP_SearchType.Items.Add("xor")

        SP_SearchType.Text() = "search"

    End Sub

    Private Sub Unload() Handles Me.Unloaded
        Sub_Menu.Close()
    End Sub

    WithEvents Sub_Menu As New MenuDialog
    Private Property EditColumnIndex() As Integer = 0

    Private Sub Button_Click_Event(ByVal sender As ButtonX) Handles BT_x0y0_Header.LeftClick,
                                                                 BT_x1y0_Header.LeftClick,
                                                                 BT_x2y0_Header.LeftClick,
                                                                 BT_x3y0_Header.LeftClick,
                                                                 BT_x4y0_Header.LeftClick,
                                                                 BT_x5y0_Header.LeftClick



        Sub_Menu = New MenuDialog()
        Sub_Menu.Orientation = Orientation.Vertical
        Sub_Menu.TextHorizontalAlignment = HorizontalAlignment.Left
        Sub_Menu.Buffer() = New Thickness(10)
        Sub_Menu.ButtonBorder(Brushes.Transparent, New Thickness(0))

        If sender Is BT_x0y0_Header Then
            Sub_Menu.ButtonInfo() = {"Delete|Delete", "Count|Count", "Text|Text"}
        Else
            Sub_Menu.ButtonInfo() = {"Delete|Delete", "Copy|Copy", "Paste|Paste", "Text|Text"}
        End If

        Sub_Menu.Load()


        Dim loc As New Point(0, 0)
        Dim M_width = Sub_Menu.Size.Width() / 2

        If sender Is BT_x0y0_Header Then
            loc = BT_x0y0_Header.Postion()
            Dim H_width = BT_x0y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 0
        ElseIf sender Is BT_x1y0_Header Then
            loc = BT_x1y0_Header.Postion()
            Dim H_width = BT_x1y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 1
        ElseIf sender Is BT_x2y0_Header Then
            loc = BT_x2y0_Header.Postion()
            Dim H_width = BT_x2y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 2
        ElseIf sender Is BT_x3y0_Header Then
            loc = BT_x3y0_Header.Postion()
            Dim H_width = BT_x3y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 3
        ElseIf sender Is BT_x4y0_Header Then
            loc = BT_x4y0_Header.Postion()
            Dim H_width = BT_x4y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 4
        ElseIf sender Is BT_x5y0_Header Then
            loc = BT_x5y0_Header.Postion()
            Dim H_width = BT_x5y0_Header.ActualWidth() / 2
            Dim x = loc.X + (H_width - M_width)
            Sub_Menu.Location() = New Point(x, loc.Y)
            EditColumnIndex() = 5
        End If

        Sub_Menu.Show()

    End Sub


    Private Property CopyColumn() As New List(Of String)
    Private Property CopyHeader() As String = ""
    Private Property IsCopied() As Boolean = False

    Private Sub Sub_Menu_Event(ByVal button As GenerikButton) Handles Sub_Menu.LeftClick
        Sub_Menu.Close()

        If button.Name() = "Count" Then
            Dim pnt = MaxGridSize()

            For i = 1 To Utility.ToInt(pnt.Y)
                Utility.SetIndex(LocalRows(), i - 1, i.ToString)
            Next

            LoadCells(0)
        End If

        If button.Name() = "Delete" Then
            Select Case EditColumnIndex
                Case 0
                    LocalRows.Clear()
                    LoadCells(0)
                Case 1
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex(), New List(Of String))
                    LoadCells(1)
                Case 2
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 1, New List(Of String))
                    LoadCells(2)
                Case 3
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 2, New List(Of String))
                    LoadCells(3)
                Case 4
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 3, New List(Of String))
                    LoadCells(4)
                Case 5
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 4, New List(Of String))
                    LoadCells(5)
            End Select

            GoTo copy
        End If
        If button.Name() = "Copy" Then
copy:
            CopyColumn.Clear()
            Select Case EditColumnIndex
                Case 0
                    CopyColumn() = LocalRows()
                Case 1
                    CopyColumn() = Utility.GetIndex(LocalTable(), ColumnIndex(), New List(Of String))
                    CopyHeader() = Utility.GetIndex(LocalColumns(), ColumnIndex(), "")
                Case 2
                    CopyColumn() = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, New List(Of String))
                    CopyHeader() = Utility.GetIndex(LocalColumns(), ColumnIndex() + 1, "")
                Case 3
                    CopyColumn() = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, New List(Of String))
                    CopyHeader() = Utility.GetIndex(LocalColumns(), ColumnIndex() + 2, "")
                Case 4
                    CopyColumn() = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, New List(Of String))
                    CopyHeader() = Utility.GetIndex(LocalColumns(), ColumnIndex() + 3, "")
                Case 5
                    CopyColumn() = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, New List(Of String))
                    CopyHeader() = Utility.GetIndex(LocalColumns(), ColumnIndex() + 4, "")
            End Select

            IsCopied() = True
        End If
        If button.Name() = "Paste" And IsCopied() = True Then
            Select Case EditColumnIndex
                Case 0
                    LocalRows() = CopyColumn()
                    LoadCells(0)
                Case 1
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex(), CopyColumn())
                    LocalColumns() = Utility.SetIndex(LocalColumns(), ColumnIndex(), CopyHeader())
                    LoadCells(1)
                Case 2
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 1, CopyColumn())
                    LocalColumns() = Utility.SetIndex(LocalColumns(), ColumnIndex() + 1, CopyHeader())
                    LoadCells(2)
                Case 3
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 2, CopyColumn())
                    LocalColumns() = Utility.SetIndex(LocalColumns(), ColumnIndex() + 2, CopyHeader())
                    LoadCells(3)
                Case 4
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 3, CopyColumn())
                    LocalColumns() = Utility.SetIndex(LocalColumns(), ColumnIndex() + 3, CopyHeader())
                    LoadCells(4)
                Case 5
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 4, CopyColumn())
                    LocalColumns() = Utility.SetIndex(LocalColumns(), ColumnIndex() + 4, CopyHeader())
                    LoadCells(5)
            End Select

        End If

        If button.Name() = "Text" Then

            Dim text = Clipboard.GetText()
            Dim clip_array = Utility.ToList(text, vbCrLf)


            Select Case EditColumnIndex
                Case 0
                    LocalRows() = CopyColumn()
                    LoadCells(0)
                Case 1
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex(), clip_array)
                    LoadCells(1)
                Case 2
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 1, clip_array)
                    LoadCells(2)
                Case 3
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 2, clip_array)
                    LoadCells(3)
                Case 4
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 3, clip_array)
                    LoadCells(4)
                Case 5
                    LocalTable() = Utility.SetIndex(LocalTable(), ColumnIndex() + 4, clip_array)
                    LoadCells(5)
            End Select

            Scroll_Rows.Maximum = MaxGridSize.Y + 5
            Scroll_Columns.Maximum = MaxGridSize.X + 5
        End If

    End Sub

    Private Function MaxGridSize() As Point
        Dim pnt As New Point

        Dim table_W = LocalTable.Count() 'max table columns
        Dim columns_W = LocalColumns.Count()
        Dim table_H = 0 'max table rows
        Dim rows_H = LocalRows.Count()

        For Each col In LocalTable()
            Dim cnt = col.Count()
            If table_H < cnt Then
                table_H = cnt
            End If
        Next

        If table_W > columns_W Then
            pnt.X = table_W
        Else
            pnt.X = columns_W
        End If

        If table_H > rows_H Then
            pnt.Y = table_H
        Else
            pnt.Y = rows_H
        End If

        Return pnt
    End Function


    Private Sub ModeButtons_Event() Handles Mode_Button.LeftClick

        Dim Copy = "Copy"
        Dim Update = "Update"
        Dim Delete = "Delete"
        Dim Search = "Search"
        Dim [New] = "New"

        If Mode_Button.Text = Update Then
            Mode_Button.Text = Delete

            m_IsUpdate = False
            m_IsCopy = False
            m_IsSearch = False
            m_IsDelete = True
            m_IsNew = False

        ElseIf Mode_Button.Text = Delete Then
            Mode_Button.Text = Copy

            m_IsUpdate = False
            m_IsCopy = True
            m_IsSearch = False
            m_IsDelete = False
            m_IsNew = False

        ElseIf Mode_Button.Text = Copy Then
            Mode_Button.Text = [New]

            m_IsUpdate = False
            m_IsCopy = False
            m_IsSearch = False
            m_IsDelete = False
            m_IsNew = True

        ElseIf Mode_Button.Text = [New] Then
            Mode_Button.Text = Search

            m_IsUpdate = False
            m_IsCopy = False
            m_IsSearch = True
            m_IsDelete = False
            m_IsNew = False

            Grid_Search_Bar.Visibility = Visibility.Visible

        ElseIf Mode_Button.Text = Search Then
            Mode_Button.Text = Update

            CellBackgroundBrush() = Background()
            CellForegroundBrush() = Foreground()

            Grid_Search_Bar.Visibility = Visibility.Collapsed
            TB_SearchInput.Text() = ""
            SP_SearchAddress.Clear()

            m_IsUpdate = True
            m_IsCopy = False
            m_IsSearch = False
            m_IsDelete = False
            m_IsNew = False

        End If

    End Sub

    Private Sub ScopeButtons_Event() Handles Scope_Button.LeftClick

        Dim Cluster = "Cluster"
        Dim Database = "Database"
        Dim Table = "Table"

        If Scope_Button.Text = Table Then
            Scope_Button.Text = Cluster

            m_IsCluster = True
            m_IsDatabase = False
            m_IsTable = False

        ElseIf Scope_Button.Text = Cluster Then
            Scope_Button.Text = Database

            m_IsCluster = False
            m_IsDatabase = True
            m_IsTable = False

        ElseIf Scope_Button.Text = Database Then
            Scope_Button.Text = Table

            m_IsCluster = False
            m_IsDatabase = False
            m_IsTable = True

        End If

    End Sub

    Public Sub ButtonBorder(ByVal color As Brush, ByVal thickness As Thickness)
        BT_x0y0_Header.Border(color, thickness)
        BT_x1y0_Header.Border(color, thickness)
        BT_x2y0_Header.Border(color, thickness)
        BT_x3y0_Header.Border(color, thickness)
        BT_x4y0_Header.Border(color, thickness)
        BT_x5y0_Header.Border(color, thickness)
    End Sub
    Public WriteOnly Property ButtonNormalFont() As Fonts
        Set(value As Fonts)
            Mode_Button.NormalFont() = value
            Scope_Button.NormalFont() = value
            BT_x0y0_Header.NormalFont() = value
            BT_x1y0_Header.NormalFont() = value
            BT_x2y0_Header.NormalFont() = value
            BT_x3y0_Header.NormalFont() = value
            BT_x4y0_Header.NormalFont() = value
            BT_x5y0_Header.NormalFont() = value
        End Set
    End Property
    Public WriteOnly Property ButtonHoverFont() As Fonts
        Set(value As Fonts)
            Mode_Button.HoverFont() = value
            Scope_Button.HoverFont() = value
            BT_x0y0_Header.HoverFont() = value
            BT_x1y0_Header.HoverFont() = value
            BT_x2y0_Header.HoverFont() = value
            BT_x3y0_Header.HoverFont() = value
            BT_x4y0_Header.HoverFont() = value
            BT_x5y0_Header.HoverFont() = value
        End Set
    End Property
    Public WriteOnly Property ButtonBackground() As Brush
        Set(value As Brush)
            Mode_Button.Background = value
            Scope_Button.Background = value
            SP_SearchAddress.Background() = value
            SP_SearchType.Background() = value
            BT_x0y0_Header.Background = value
            BT_x1y0_Header.Background = value
            BT_x2y0_Header.Background = value
            BT_x3y0_Header.Background = value
            BT_x4y0_Header.Background = value
            BT_x5y0_Header.Background = value
        End Set
    End Property


    Public Property CellBorderThickness() As Thickness
        Get
            Return TB_x0y1.BorderThickness
        End Get
        Set(value As Thickness)

            TB_x0y1.BorderThickness = value
            TB_x0y2.BorderThickness = value
            TB_x0y3.BorderThickness = value
            TB_x0y4.BorderThickness = value
            TB_x0y5.BorderThickness = value
            TB_x0y6.BorderThickness = value
            TB_x0y7.BorderThickness = value
            TB_x0y8.BorderThickness = value
            TB_x0y9.BorderThickness = value

            TB_x1y0_Header.BorderThickness = value
            TB_x1y1.BorderThickness = value
            TB_x1y2.BorderThickness = value
            TB_x1y3.BorderThickness = value
            TB_x1y4.BorderThickness = value
            TB_x1y5.BorderThickness = value
            TB_x1y6.BorderThickness = value
            TB_x1y7.BorderThickness = value
            TB_x1y8.BorderThickness = value
            TB_x1y9.BorderThickness = value

            TB_x2y0_Header.BorderThickness = value
            TB_x2y1.BorderThickness = value
            TB_x2y2.BorderThickness = value
            TB_x2y3.BorderThickness = value
            TB_x2y4.BorderThickness = value
            TB_x2y5.BorderThickness = value
            TB_x2y6.BorderThickness = value
            TB_x2y7.BorderThickness = value
            TB_x2y8.BorderThickness = value
            TB_x2y9.BorderThickness = value

            TB_x3y0_Header.BorderThickness = value
            TB_x3y1.BorderThickness = value
            TB_x3y2.BorderThickness = value
            TB_x3y3.BorderThickness = value
            TB_x3y4.BorderThickness = value
            TB_x3y5.BorderThickness = value
            TB_x3y6.BorderThickness = value
            TB_x3y7.BorderThickness = value
            TB_x3y8.BorderThickness = value
            TB_x3y9.BorderThickness = value

            TB_x4y0_Header.BorderThickness = value
            TB_x4y1.BorderThickness = value
            TB_x4y2.BorderThickness = value
            TB_x4y3.BorderThickness = value
            TB_x4y4.BorderThickness = value
            TB_x4y5.BorderThickness = value
            TB_x4y6.BorderThickness = value
            TB_x4y7.BorderThickness = value
            TB_x4y8.BorderThickness = value
            TB_x4y9.BorderThickness = value

            TB_x5y0_Header.BorderThickness = value
            TB_x5y1.BorderThickness = value
            TB_x5y2.BorderThickness = value
            TB_x5y3.BorderThickness = value
            TB_x5y4.BorderThickness = value
            TB_x5y5.BorderThickness = value
            TB_x5y6.BorderThickness = value
            TB_x5y7.BorderThickness = value
            TB_x5y8.BorderThickness = value
            TB_x5y9.BorderThickness = value
        End Set
    End Property
    Public Property CellBorderBrush() As Brush
        Get
            Return TB_x0y1.BorderBrush
        End Get
        Set(value As Brush)

            TB_x0y1.BorderBrush = value
            TB_x0y2.BorderBrush = value
            TB_x0y3.BorderBrush = value
            TB_x0y4.BorderBrush = value
            TB_x0y5.BorderBrush = value
            TB_x0y6.BorderBrush = value
            TB_x0y7.BorderBrush = value
            TB_x0y8.BorderBrush = value
            TB_x0y9.BorderBrush = value

            TB_x1y0_Header.BorderBrush = value
            TB_x1y1.BorderBrush = value
            TB_x1y2.BorderBrush = value
            TB_x1y3.BorderBrush = value
            TB_x1y4.BorderBrush = value
            TB_x1y5.BorderBrush = value
            TB_x1y6.BorderBrush = value
            TB_x1y7.BorderBrush = value
            TB_x1y8.BorderBrush = value
            TB_x1y9.BorderBrush = value

            TB_x2y0_Header.BorderBrush = value
            TB_x2y1.BorderBrush = value
            TB_x2y2.BorderBrush = value
            TB_x2y3.BorderBrush = value
            TB_x2y4.BorderBrush = value
            TB_x2y5.BorderBrush = value
            TB_x2y6.BorderBrush = value
            TB_x2y7.BorderBrush = value
            TB_x2y8.BorderBrush = value
            TB_x2y9.BorderBrush = value

            TB_x3y0_Header.BorderBrush = value
            TB_x3y1.BorderBrush = value
            TB_x3y2.BorderBrush = value
            TB_x3y3.BorderBrush = value
            TB_x3y4.BorderBrush = value
            TB_x3y5.BorderBrush = value
            TB_x3y6.BorderBrush = value
            TB_x3y7.BorderBrush = value
            TB_x3y8.BorderBrush = value
            TB_x3y9.BorderBrush = value

            TB_x4y0_Header.BorderBrush = value
            TB_x4y1.BorderBrush = value
            TB_x4y2.BorderBrush = value
            TB_x4y3.BorderBrush = value
            TB_x4y4.BorderBrush = value
            TB_x4y5.BorderBrush = value
            TB_x4y6.BorderBrush = value
            TB_x4y7.BorderBrush = value
            TB_x4y8.BorderBrush = value
            TB_x4y9.BorderBrush = value

            TB_x5y0_Header.BorderBrush = value
            TB_x5y1.BorderBrush = value
            TB_x5y2.BorderBrush = value
            TB_x5y3.BorderBrush = value
            TB_x5y4.BorderBrush = value
            TB_x5y5.BorderBrush = value
            TB_x5y6.BorderBrush = value
            TB_x5y7.BorderBrush = value
            TB_x5y8.BorderBrush = value
            TB_x5y9.BorderBrush = value
        End Set
    End Property
    Public Property CellForegroundBrush() As Brush
        Get
            Return TB_x0y1.Foreground
        End Get
        Set(value As Brush)

            TB_x0y1.Foreground = value
            TB_x0y2.Foreground = value
            TB_x0y3.Foreground = value
            TB_x0y4.Foreground = value
            TB_x0y5.Foreground = value
            TB_x0y6.Foreground = value
            TB_x0y7.Foreground = value
            TB_x0y8.Foreground = value
            TB_x0y9.Foreground = value

            TB_x1y0_Header.Foreground = value
            TB_x1y1.Foreground = value
            TB_x1y2.Foreground = value
            TB_x1y3.Foreground = value
            TB_x1y4.Foreground = value
            TB_x1y5.Foreground = value
            TB_x1y6.Foreground = value
            TB_x1y7.Foreground = value
            TB_x1y8.Foreground = value
            TB_x1y9.Foreground = value

            TB_x2y0_Header.Foreground = value
            TB_x2y1.Foreground = value
            TB_x2y2.Foreground = value
            TB_x2y3.Foreground = value
            TB_x2y4.Foreground = value
            TB_x2y5.Foreground = value
            TB_x2y6.Foreground = value
            TB_x2y7.Foreground = value
            TB_x2y8.Foreground = value
            TB_x2y9.Foreground = value

            TB_x3y0_Header.Foreground = value
            TB_x3y1.Foreground = value
            TB_x3y2.Foreground = value
            TB_x3y3.Foreground = value
            TB_x3y4.Foreground = value
            TB_x3y5.Foreground = value
            TB_x3y6.Foreground = value
            TB_x3y7.Foreground = value
            TB_x3y8.Foreground = value
            TB_x3y9.Foreground = value

            TB_x4y0_Header.Foreground = value
            TB_x4y1.Foreground = value
            TB_x4y2.Foreground = value
            TB_x4y3.Foreground = value
            TB_x4y4.Foreground = value
            TB_x4y5.Foreground = value
            TB_x4y6.Foreground = value
            TB_x4y7.Foreground = value
            TB_x4y8.Foreground = value
            TB_x4y9.Foreground = value

            TB_x5y0_Header.Foreground = value
            TB_x5y1.Foreground = value
            TB_x5y2.Foreground = value
            TB_x5y3.Foreground = value
            TB_x5y4.Foreground = value
            TB_x5y5.Foreground = value
            TB_x5y6.Foreground = value
            TB_x5y7.Foreground = value
            TB_x5y8.Foreground = value
            TB_x5y9.Foreground = value
        End Set
    End Property
    Public Property CellFontFamily() As FontFamily
        Get
            Return TB_x0y1.FontFamily
        End Get
        Set(value As FontFamily)

            TB_x0y1.FontFamily = value
            TB_x0y2.FontFamily = value
            TB_x0y3.FontFamily = value
            TB_x0y4.FontFamily = value
            TB_x0y5.FontFamily = value
            TB_x0y6.FontFamily = value
            TB_x0y7.FontFamily = value
            TB_x0y8.FontFamily = value
            TB_x0y9.FontFamily = value

            TB_x1y0_Header.FontFamily = value
            TB_x1y1.FontFamily = value
            TB_x1y2.FontFamily = value
            TB_x1y3.FontFamily = value
            TB_x1y4.FontFamily = value
            TB_x1y5.FontFamily = value
            TB_x1y6.FontFamily = value
            TB_x1y7.FontFamily = value
            TB_x1y8.FontFamily = value
            TB_x1y9.FontFamily = value

            TB_x2y0_Header.FontFamily = value
            TB_x2y1.FontFamily = value
            TB_x2y2.FontFamily = value
            TB_x2y3.FontFamily = value
            TB_x2y4.FontFamily = value
            TB_x2y5.FontFamily = value
            TB_x2y6.FontFamily = value
            TB_x2y7.FontFamily = value
            TB_x2y8.FontFamily = value
            TB_x2y9.FontFamily = value

            TB_x3y0_Header.FontFamily = value
            TB_x3y1.FontFamily = value
            TB_x3y2.FontFamily = value
            TB_x3y3.FontFamily = value
            TB_x3y4.FontFamily = value
            TB_x3y5.FontFamily = value
            TB_x3y6.FontFamily = value
            TB_x3y7.FontFamily = value
            TB_x3y8.FontFamily = value
            TB_x3y9.FontFamily = value

            TB_x4y0_Header.FontFamily = value
            TB_x4y1.FontFamily = value
            TB_x4y2.FontFamily = value
            TB_x4y3.FontFamily = value
            TB_x4y4.FontFamily = value
            TB_x4y5.FontFamily = value
            TB_x4y6.FontFamily = value
            TB_x4y7.FontFamily = value
            TB_x4y8.FontFamily = value
            TB_x4y9.FontFamily = value

            TB_x5y0_Header.FontFamily = value
            TB_x5y1.FontFamily = value
            TB_x5y2.FontFamily = value
            TB_x5y3.FontFamily = value
            TB_x5y4.FontFamily = value
            TB_x5y5.FontFamily = value
            TB_x5y6.FontFamily = value
            TB_x5y7.FontFamily = value
            TB_x5y8.FontFamily = value
            TB_x5y9.FontFamily = value
        End Set
    End Property
    Public Property CellFontSize() As Double
        Get
            Return TB_x0y1.FontSize()
        End Get
        Set(value As Double)

            TB_x0y1.FontSize = value
            TB_x0y2.FontSize = value
            TB_x0y3.FontSize = value
            TB_x0y4.FontSize = value
            TB_x0y5.FontSize = value
            TB_x0y6.FontSize = value
            TB_x0y7.FontSize = value
            TB_x0y8.FontSize = value
            TB_x0y9.FontSize = value

            TB_x1y0_Header.FontSize = value
            TB_x1y1.FontSize = value
            TB_x1y2.FontSize = value
            TB_x1y3.FontSize = value
            TB_x1y4.FontSize = value
            TB_x1y5.FontSize = value
            TB_x1y6.FontSize = value
            TB_x1y7.FontSize = value
            TB_x1y8.FontSize = value
            TB_x1y9.FontSize = value

            TB_x2y0_Header.FontSize = value
            TB_x2y1.FontSize = value
            TB_x2y2.FontSize = value
            TB_x2y3.FontSize = value
            TB_x2y4.FontSize = value
            TB_x2y5.FontSize = value
            TB_x2y6.FontSize = value
            TB_x2y7.FontSize = value
            TB_x2y8.FontSize = value
            TB_x2y9.FontSize = value

            TB_x3y0_Header.FontSize = value
            TB_x3y1.FontSize = value
            TB_x3y2.FontSize = value
            TB_x3y3.FontSize = value
            TB_x3y4.FontSize = value
            TB_x3y5.FontSize = value
            TB_x3y6.FontSize = value
            TB_x3y7.FontSize = value
            TB_x3y8.FontSize = value
            TB_x3y9.FontSize = value

            TB_x4y0_Header.FontSize = value
            TB_x4y1.FontSize = value
            TB_x4y2.FontSize = value
            TB_x4y3.FontSize = value
            TB_x4y4.FontSize = value
            TB_x4y5.FontSize = value
            TB_x4y6.FontSize = value
            TB_x4y7.FontSize = value
            TB_x4y8.FontSize = value
            TB_x4y9.FontSize = value

            TB_x5y0_Header.FontSize = value
            TB_x5y1.FontSize = value
            TB_x5y2.FontSize = value
            TB_x5y3.FontSize = value
            TB_x5y4.FontSize = value
            TB_x5y5.FontSize = value
            TB_x5y6.FontSize = value
            TB_x5y7.FontSize = value
            TB_x5y8.FontSize = value
            TB_x5y9.FontSize = value
        End Set
    End Property
    Public Property CellBackgroundBrush() As Brush
        Get
            Return TB_x0y0_Corner.Background
        End Get
        Set(value As Brush)
            TB_x0y0_Corner.Background = value
            TB_x0y1.Background = value
            TB_x0y2.Background = value
            TB_x0y3.Background = value
            TB_x0y4.Background = value
            TB_x0y5.Background = value
            TB_x0y6.Background = value
            TB_x0y7.Background = value
            TB_x0y8.Background = value
            TB_x0y9.Background = value

            TB_x1y0_Header.Background = value
            TB_x1y1.Background = value
            TB_x1y2.Background = value
            TB_x1y3.Background = value
            TB_x1y4.Background = value
            TB_x1y5.Background = value
            TB_x1y6.Background = value
            TB_x1y7.Background = value
            TB_x1y8.Background = value
            TB_x1y9.Background = value

            TB_x2y0_Header.Background = value
            TB_x2y1.Background = value
            TB_x2y2.Background = value
            TB_x2y3.Background = value
            TB_x2y4.Background = value
            TB_x2y5.Background = value
            TB_x2y6.Background = value
            TB_x2y7.Background = value
            TB_x2y8.Background = value
            TB_x2y9.Background = value

            TB_x3y0_Header.Background = value
            TB_x3y1.Background = value
            TB_x3y2.Background = value
            TB_x3y3.Background = value
            TB_x3y4.Background = value
            TB_x3y5.Background = value
            TB_x3y6.Background = value
            TB_x3y7.Background = value
            TB_x3y8.Background = value
            TB_x3y9.Background = value

            TB_x4y0_Header.Background = value
            TB_x4y1.Background = value
            TB_x4y2.Background = value
            TB_x4y3.Background = value
            TB_x4y4.Background = value
            TB_x4y5.Background = value
            TB_x4y6.Background = value
            TB_x4y7.Background = value
            TB_x4y8.Background = value
            TB_x4y9.Background = value

            TB_x5y0_Header.Background = value
            TB_x5y1.Background = value
            TB_x5y2.Background = value
            TB_x5y3.Background = value
            TB_x5y4.Background = value
            TB_x5y5.Background = value
            TB_x5y6.Background = value
            TB_x5y7.Background = value
            TB_x5y8.Background = value
            TB_x5y9.Background = value
        End Set
    End Property

    Private m_BorderBrush As Brush = Brushes.Red
    Public Shadows Property BorderBrush() As Brush
        Get
            Return m_BorderBrush
        End Get
        Set(ByVal value As Brush)
            m_BorderBrush = value
            CellBorderBrush() = value
            TB_Editor.BorderBrush = value
            LB_Cluster.BorderBrush = value
            Border_Cluster.BorderBrush = value
            LB_Database.BorderBrush = value
            Border_Database.BorderBrush = value
            LB_Table.BorderBrush = value
            Border_Table.BorderBrush = value

            LB_Mode.BorderBrush = value
            LB_Scope.BorderBrush = value

            Border_LB_SearchInput.BorderBrush = value
            LB_SearchInput.BorderBrush = value
            Border_TB_SearchInput.BorderBrush = value
            TB_SearchInput.BorderBrush = value
            Border_LB_SearchType.BorderBrush = value
            LB_SearchType.BorderBrush = value
            Border_LB_SearchAddress.BorderBrush = value
            LB_SearchAddress.BorderBrush = value

            SP_SearchAddress.BorderOuter(value, New Thickness(3, 0, 3, 3))
            SP_SearchType.BorderOuter(value, New Thickness(3, 3, 3, 3))
            Mode_Button.Border(value, New Thickness(3, 0, 3, 3))
            Scope_Button.Border(value, New Thickness(3, 0, 3, 3))

        End Set
    End Property

    Private m_Foreground As Brush = Brushes.Black
    Public Shadows Property Foreground() As Brush
        Get
            Return m_Foreground
        End Get
        Set(ByVal value As Brush)
            m_Foreground = value
            CellForegroundBrush() = value

            LB_Cluster.Foreground = value
            LB_Database.Foreground = value
            LB_Table.Foreground = value
            LB_Mode.Foreground = value
            LB_Scope.Foreground = value

            LB_SearchInput.Foreground = value
            LB_SearchType.Foreground = value
            LB_SearchAddress.Foreground = value
            TB_SearchInput.Foreground = value

            TB_Editor.Foreground = value
            TB_Cluster.Foreground = value
            TB_Database.Foreground = value
            TB_Table.Foreground = value
        End Set
    End Property

    Private m_Background As Brush = Brushes.White
    Public Shadows Property Background() As Brush
        Get
            Return m_Background
        End Get
        Set(ByVal value As Brush)
            m_Background = value

            CellBackgroundBrush() = value
            TB_Editor.Background = value

            LB_Cluster.Background = value
            LB_Database.Background = value
            LB_Table.Background = value
            LB_Mode.Background = value
            LB_Scope.Background = value

            Border_LB_SearchInput.Background = value
            LB_SearchInput.Background = value
            Border_LB_SearchType.Background = value
            LB_SearchType.Background = value
            Border_LB_SearchAddress.Background = value
            LB_SearchAddress.Background = value

            Border_TB_SearchInput.Background = value
            TB_SearchInput.Background = value

            TB_Cluster.Background = value
            TB_Database.Background = value
            TB_Table.Background = value
            Border_Database.Background = value
            Border_Table.Background = value
            Border_Cluster.Background = value

            Main_Grid.Background = value
            Main_Splitter.Background = value
            Scroll_Columns.Background = value
            Scroll_Rows.Background = value
        End Set
    End Property


    Public Shadows WriteOnly Property FontFamily() As FontFamily
        Set(ByVal value As FontFamily)
            CellFontFamily() = value
            TB_Editor.FontFamily = value
            LB_Cluster.FontFamily = value
            TB_Cluster.FontFamily = value
            LB_Database.FontFamily = value
            TB_Database.FontFamily = value
            LB_Table.FontFamily = value
            TB_Table.FontFamily = value

            LB_Mode.FontFamily = value
            LB_Scope.FontFamily = value
            LB_SearchInput.FontFamily = value
            TB_SearchInput.FontFamily = value
            LB_SearchType.FontFamily = value
            LB_SearchAddress.FontFamily = value

        End Set
    End Property
    Public Shadows WriteOnly Property FontSize() As Double
        Set(ByVal value As Double)
            CellFontSize() = value
            TB_Editor.FontSize = value
            LB_Cluster.FontSize = value
            TB_Cluster.FontSize = value
            LB_Database.FontSize = value
            TB_Database.FontSize = value
            LB_Table.FontSize = value
            TB_Table.FontSize = value

            LB_Mode.FontSize = value
            LB_Scope.FontSize = value
            LB_SearchInput.FontSize = value
            TB_SearchInput.FontSize = value
            LB_SearchType.FontSize = value
            LB_SearchAddress.FontSize = value

        End Set
    End Property


    Private Sub Main_Splitter_MouseEnter_Event() Handles Main_Splitter.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub Main_Splitter_MouseLeave_Event() Handles Main_Splitter.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub

    Public Property LocalTable() As New List(Of List(Of String))
    Public Property LocalColumns() As New List(Of String)
    Public Property LocalRows() As New List(Of String)
    Public Property ColumnIndex() As Integer = 0
    Public Property RowIndex() As Integer = 0
    Public Property IsDataUpdated() As Boolean = True

    Property m_IsUpdate As Boolean = True
    Public ReadOnly Property IsUpdate() As Boolean
        Get
            Return m_IsUpdate
        End Get
    End Property

    Property m_IsDelete As Boolean = False
    Public ReadOnly Property IsDelete() As Boolean
        Get
            Return m_IsDelete
        End Get
    End Property

    Property m_IsNew As Boolean = False
    Public ReadOnly Property IsNew() As Boolean
        Get
            Return m_IsNew
        End Get
    End Property

    Property m_IsCopy As Boolean = False
    Public ReadOnly Property IsCopy() As Boolean
        Get
            Return m_IsCopy
        End Get
    End Property

    Property m_IsSearch As Boolean = False
    Public ReadOnly Property IsSearch() As Boolean
        Get
            Return m_IsSearch
        End Get
    End Property

    Property m_IsCluster As Boolean = True
    Public ReadOnly Property IsCluster() As Boolean
        Get
            Return m_IsCluster
        End Get
    End Property

    Property m_IsDatabase As Boolean = False
    Public ReadOnly Property IsDatabase() As Boolean
        Get
            Return m_IsDatabase
        End Get
    End Property

    Property m_IsTable As Boolean = False
    Public ReadOnly Property IsTable() As Boolean
        Get
            Return m_IsTable
        End Get
    End Property

    Public Property SearchAddress() As List(Of String)
        Get
            Return SP_SearchAddress.Items
        End Get
        Set(value As List(Of String))
            CellBackgroundBrush() = Background()
            CellForegroundBrush() = Foreground()

            SP_SearchAddress.Items() = value
            SP_SearchAddress.Index() = 0
        End Set
    End Property

    Public Property ClusterName() As String
        Get
            Return Utility.Trim(TB_Cluster.Text())
        End Get
        Set(value As String)
            TB_Cluster.Text() = value
        End Set
    End Property

    Public Property DatabaseName() As String
        Get
            Return Utility.Trim(TB_Database.Text())
        End Get
        Set(value As String)
            TB_Database.Text() = value
        End Set
    End Property

    Public Property TableName() As String
        Get
            Return Utility.Trim(TB_Table.Text())
        End Get
        Set(value As String)
            TB_Table.Text() = value
        End Set
    End Property

    Public ReadOnly Property SearchType() As Gate
        Get
            Select Case SP_SearchType.Text
                Case "and"
                    Return Gate.eAND
                Case "end"
                    Return Gate.eEND
                Case "equals"
                    Return Gate.eEQUALS
                Case "nand"
                    Return Gate.eNAND
                Case "nor"
                    Return Gate.eNOR
                Case "not"
                    Return Gate.eNOT
                Case "or"
                    Return Gate.eOR
                Case "search"
                    Return Gate.eSEARCH
                Case "seq"
                    Return Gate.eSEQ
                Case "start"
                    Return Gate.eSTART
                Case "xnor"
                    Return Gate.eXNOR
                Case "xor"
                    Return Gate.eXOR
                Case Else
                    Return Gate.eSEARCH
            End Select

        End Get
    End Property

    Public ReadOnly Property SearchInput() As String
        Get
            Return TB_SearchInput.Text()
        End Get
    End Property

    Private Property SafeToStore() As Boolean = False

    Public Event SelectedAddress(ByVal address As String)
    Private Sub SelectedAddress_Event() Handles SP_SearchAddress.SelectedSpin

        If Utility.IsEmpty(SP_SearchAddress.Text) = False Then
            RaiseEvent SelectedAddress(SP_SearchAddress.Text)
        End If

    End Sub

    Private Property CellFocus() As New Point(0, 0)
    Public Sub SetFocus(ByVal column As String, ByVal row As String)

        Dim index As New Point(0, 0)
        For i = 0 To LocalColumns.Count - 1
            If Utility.Search(LocalColumns(i), column, Gate.eEQUALS) = True Then
                index.X = i
                Exit For
            End If
        Next

        Dim r_index = 0
        For i = 0 To LocalRows.Count - 1
            If Utility.Search(LocalRows(i), row, Gate.eEQUALS) = True Then
                index.Y = i
                Exit For
            End If
        Next

        CellFocus() = index
        ResetFocus()
    End Sub

    Private Sub ResetFocus()

        CellBackgroundBrush() = Background()
        CellForegroundBrush() = Foreground()

        Select Case CellFocus().X
            Case ColumnIndex()
                Select Case CellFocus().Y
                    Case RowIndex()
                        TB_x1y1.Focus()
                        TB_x1y1.Foreground() = Background()
                        TB_x1y1.Background() = Foreground()
                    Case RowIndex() + 1
                        TB_x1y2.Focus()
                        TB_x1y2.Foreground() = Background()
                        TB_x1y2.Background() = Foreground()
                    Case RowIndex() + 2
                        TB_x1y3.Focus()
                        TB_x1y3.Foreground() = Background()
                        TB_x1y3.Background() = Foreground()
                    Case RowIndex() + 3
                        TB_x1y4.Focus()
                        TB_x1y4.Foreground() = Background()
                        TB_x1y4.Background() = Foreground()
                    Case RowIndex() + 4
                        TB_x1y5.Focus()
                        TB_x1y5.Foreground() = Background()
                        TB_x1y5.Background() = Foreground()
                    Case RowIndex() + 5
                        TB_x1y6.Focus()
                        TB_x1y6.Foreground() = Background()
                        TB_x1y6.Background() = Foreground()
                    Case RowIndex() + 6
                        TB_x1y7.Focus()
                        TB_x1y7.Foreground() = Background()
                        TB_x1y7.Background() = Foreground()
                    Case RowIndex() + 7
                        TB_x1y8.Focus()
                        TB_x1y8.Foreground() = Background()
                        TB_x1y8.Background() = Foreground()
                    Case RowIndex() + 8
                        TB_x1y9.Focus()
                        TB_x1y9.Foreground() = Background()
                        TB_x1y9.Background() = Foreground()
                    Case Else
                        TB_x0y0_Corner.Focus()
                End Select
            Case ColumnIndex() + 1
                Select Case CellFocus().Y
                    Case RowIndex()
                        TB_x2y1.Focus()
                        TB_x2y1.Foreground() = Background()
                        TB_x2y1.Background() = Foreground()
                    Case RowIndex() + 1
                        TB_x2y2.Focus()
                        TB_x2y2.Foreground() = Background()
                        TB_x2y2.Background() = Foreground()
                    Case RowIndex() + 2
                        TB_x2y3.Focus()
                        TB_x2y3.Foreground() = Background()
                        TB_x2y3.Background() = Foreground()
                    Case RowIndex() + 3
                        TB_x2y4.Focus()
                        TB_x2y4.Foreground() = Background()
                        TB_x2y4.Background() = Foreground()
                    Case RowIndex() + 4
                        TB_x2y5.Focus()
                        TB_x2y5.Foreground() = Background()
                        TB_x2y5.Background() = Foreground()
                    Case RowIndex() + 5
                        TB_x2y6.Focus()
                        TB_x2y6.Foreground() = Background()
                        TB_x2y6.Background() = Foreground()
                    Case RowIndex() + 6
                        TB_x2y7.Focus()
                        TB_x2y7.Foreground() = Background()
                        TB_x2y7.Background() = Foreground()
                    Case RowIndex() + 7
                        TB_x2y8.Focus()
                        TB_x2y8.Foreground() = Background()
                        TB_x2y8.Background() = Foreground()
                    Case RowIndex() + 8
                        TB_x2y9.Focus()
                        TB_x2y9.Foreground() = Background()
                        TB_x2y9.Background() = Foreground()
                    Case Else
                        TB_x0y0_Corner.Focus()
                End Select
            Case ColumnIndex() + 2
                Select Case CellFocus().Y
                    Case RowIndex()
                        TB_x3y1.Focus()
                        TB_x3y1.Foreground() = Background()
                        TB_x3y1.Background() = Foreground()
                    Case RowIndex() + 1
                        TB_x3y2.Focus()
                        TB_x3y2.Foreground() = Background()
                        TB_x3y2.Background() = Foreground()
                    Case RowIndex() + 2
                        TB_x3y3.Focus()
                        TB_x3y3.Foreground() = Background()
                        TB_x3y3.Background() = Foreground()
                    Case RowIndex() + 3
                        TB_x3y4.Focus()
                        TB_x3y4.Foreground() = Background()
                        TB_x3y4.Background() = Foreground()
                    Case RowIndex() + 4
                        TB_x3y5.Focus()
                        TB_x3y5.Foreground() = Background()
                        TB_x3y5.Background() = Foreground()
                    Case RowIndex() + 5
                        TB_x3y6.Focus()
                        TB_x3y6.Foreground() = Background()
                        TB_x3y6.Background() = Foreground()
                    Case RowIndex() + 6
                        TB_x3y7.Focus()
                        TB_x3y7.Foreground() = Background()
                        TB_x3y7.Background() = Foreground()
                    Case RowIndex() + 7
                        TB_x3y8.Focus()
                        TB_x3y8.Foreground() = Background()
                        TB_x3y8.Background() = Foreground()
                    Case RowIndex() + 8
                        TB_x3y9.Focus()
                        TB_x3y9.Foreground() = Background()
                        TB_x3y9.Background() = Foreground()
                    Case Else
                        TB_x0y0_Corner.Focus()
                End Select
            Case ColumnIndex() + 3
                Select Case CellFocus().Y
                    Case RowIndex()
                        TB_x4y1.Focus()
                        TB_x4y1.Foreground() = Background()
                        TB_x4y1.Background() = Foreground()
                    Case RowIndex() + 1
                        TB_x4y2.Focus()
                        TB_x4y2.Foreground() = Background()
                        TB_x4y2.Background() = Foreground()
                    Case RowIndex() + 2
                        TB_x4y3.Focus()
                        TB_x4y3.Foreground() = Background()
                        TB_x4y3.Background() = Foreground()
                    Case RowIndex() + 3
                        TB_x4y4.Focus()
                        TB_x4y4.Foreground() = Background()
                        TB_x4y4.Background() = Foreground()
                    Case RowIndex() + 4
                        TB_x4y5.Focus()
                        TB_x4y5.Foreground() = Background()
                        TB_x4y5.Background() = Foreground()
                    Case RowIndex() + 5
                        TB_x4y6.Focus()
                        TB_x4y6.Foreground() = Background()
                        TB_x4y6.Background() = Foreground()
                    Case RowIndex() + 6
                        TB_x4y7.Focus()
                        TB_x4y7.Foreground() = Background()
                        TB_x4y7.Background() = Foreground()
                    Case RowIndex() + 7
                        TB_x4y8.Focus()
                        TB_x4y8.Foreground() = Background()
                        TB_x4y8.Background() = Foreground()
                    Case RowIndex() + 8
                        TB_x4y9.Focus()
                        TB_x4y9.Foreground() = Background()
                        TB_x4y9.Background() = Foreground()
                    Case Else
                        TB_x0y0_Corner.Focus()
                End Select
            Case ColumnIndex() + 4
                Select Case CellFocus().Y
                    Case RowIndex()
                        TB_x5y1.Focus()
                        TB_x5y1.Foreground() = Background()
                        TB_x5y1.Background() = Foreground()
                    Case RowIndex() + 1
                        TB_x5y2.Focus()
                        TB_x5y2.Foreground() = Background()
                        TB_x5y2.Background() = Foreground()
                    Case RowIndex() + 2
                        TB_x5y3.Focus()
                        TB_x5y3.Foreground() = Background()
                        TB_x5y3.Background() = Foreground()
                    Case RowIndex() + 3
                        TB_x5y4.Focus()
                        TB_x5y4.Foreground() = Background()
                        TB_x5y4.Background() = Foreground()
                    Case RowIndex() + 4
                        TB_x5y5.Focus()
                        TB_x5y5.Foreground() = Background()
                        TB_x5y5.Background() = Foreground()
                    Case RowIndex() + 5
                        TB_x5y6.Focus()
                        TB_x5y6.Foreground() = Background()
                        TB_x5y6.Background() = Foreground()
                    Case RowIndex() + 6
                        TB_x5y7.Focus()
                        TB_x5y7.Foreground() = Background()
                        TB_x5y7.Background() = Foreground()
                    Case RowIndex() + 7
                        TB_x5y8.Focus()
                        TB_x5y8.Foreground() = Background()
                        TB_x5y8.Background() = Foreground()
                    Case RowIndex() + 8
                        TB_x5y9.Focus()
                        TB_x5y9.Foreground() = Background()
                        TB_x5y9.Background() = Foreground()
                    Case Else
                        TB_x0y0_Corner.Focus()
                End Select
            Case Else
                TB_x0y0_Corner.Focus()
        End Select

    End Sub


    Private Sub Scroll_Rows_Event(ByVal sender As Object, ByVal e As Primitives.ScrollEventArgs) Handles Scroll_Rows.Scroll
        RowIndex() = e.NewValue
        SafeToStore() = False
        LoadCells(-1)

        If IsSearch() = True Then
            ResetFocus()
        Else
            TB_x0y0_Corner.Focus()
        End If

    End Sub

    Private Sub Scroll_Columns_Event(ByVal sender As Object, ByVal e As Primitives.ScrollEventArgs) Handles Scroll_Columns.Scroll
        ColumnIndex() = e.NewValue
        SafeToStore() = False
        LoadCells(-1)

        If IsSearch() = True Then
            ResetFocus()
        Else
            TB_x0y0_Corner.Focus()
        End If

    End Sub


    WithEvents Current_TextBox As New TextBox
    Private Sub Modify_GotFocus_Event(ByVal sender As Object, ByVal e As Object) Handles TB_x0y0_Corner.GotFocus,
                                                                                        TB_x0y1.GotFocus,
                                                                                        TB_x0y2.GotFocus,
                                                                                        TB_x0y3.GotFocus,
                                                                                        TB_x0y4.GotFocus,
                                                                                        TB_x0y5.GotFocus,
                                                                                        TB_x0y6.GotFocus,
                                                                                        TB_x0y7.GotFocus,
                                                                                        TB_x0y8.GotFocus,
                                                                                        TB_x0y9.GotFocus,
                                                                                        TB_x1y0_Header.GotFocus,
                                                                                        TB_x1y1.GotFocus,
                                                                                        TB_x1y2.GotFocus,
                                                                                        TB_x1y3.GotFocus,
                                                                                        TB_x1y4.GotFocus,
                                                                                        TB_x1y5.GotFocus,
                                                                                        TB_x1y6.GotFocus,
                                                                                        TB_x1y7.GotFocus,
                                                                                        TB_x1y8.GotFocus,
                                                                                        TB_x1y9.GotFocus,
                                                                                        TB_x2y0_Header.GotFocus,
                                                                                        TB_x2y1.GotFocus,
                                                                                        TB_x2y2.GotFocus,
                                                                                        TB_x2y3.GotFocus,
                                                                                        TB_x2y4.GotFocus,
                                                                                        TB_x2y5.GotFocus,
                                                                                        TB_x2y6.GotFocus,
                                                                                        TB_x2y7.GotFocus,
                                                                                        TB_x2y8.GotFocus,
                                                                                        TB_x2y9.GotFocus,
                                                                                        TB_x3y0_Header.GotFocus,
                                                                                        TB_x3y1.GotFocus,
                                                                                        TB_x3y2.GotFocus,
                                                                                        TB_x3y3.GotFocus,
                                                                                        TB_x3y4.GotFocus,
                                                                                        TB_x3y5.GotFocus,
                                                                                        TB_x3y6.GotFocus,
                                                                                        TB_x3y7.GotFocus,
                                                                                        TB_x3y8.GotFocus,
                                                                                        TB_x3y9.GotFocus,
                                                                                        TB_x4y0_Header.GotFocus,
                                                                                        TB_x4y1.GotFocus,
                                                                                        TB_x4y2.GotFocus,
                                                                                        TB_x4y3.GotFocus,
                                                                                        TB_x4y4.GotFocus,
                                                                                        TB_x4y5.GotFocus,
                                                                                        TB_x4y6.GotFocus,
                                                                                        TB_x4y7.GotFocus,
                                                                                        TB_x4y8.GotFocus,
                                                                                        TB_x4y9.GotFocus,
                                                                                        TB_x5y0_Header.GotFocus,
                                                                                        TB_x5y1.GotFocus,
                                                                                        TB_x5y2.GotFocus,
                                                                                        TB_x5y3.GotFocus,
                                                                                        TB_x5y4.GotFocus,
                                                                                        TB_x5y5.GotFocus,
                                                                                        TB_x5y6.GotFocus,
                                                                                        TB_x5y7.GotFocus,
                                                                                        TB_x5y8.GotFocus,
                                                                                        TB_x5y9.GotFocus,
                                                                                        TB_Cluster.GotFocus,
                                                                                        TB_Database.GotFocus,
                                                                                        TB_Table.GotFocus,
                                                                                        TB_SearchInput.GotFocus


        Current_TextBox = sender
        TB_Editor.Text = sender.Text
        SafeToStore() = True
    End Sub

    Private Sub Modify_TextChanged_Event(ByVal sender As Object, ByVal e As Object) Handles TB_x0y1.TextChanged,
                                                                                        TB_x0y2.TextChanged,
                                                                                        TB_x0y3.TextChanged,
                                                                                        TB_x0y4.TextChanged,
                                                                                        TB_x0y5.TextChanged,
                                                                                        TB_x0y6.TextChanged,
                                                                                        TB_x0y7.TextChanged,
                                                                                        TB_x0y8.TextChanged,
                                                                                        TB_x0y9.TextChanged,
                                                                                        TB_x1y0_Header.TextChanged,
                                                                                        TB_x1y1.TextChanged,
                                                                                        TB_x1y2.TextChanged,
                                                                                        TB_x1y3.TextChanged,
                                                                                        TB_x1y4.TextChanged,
                                                                                        TB_x1y5.TextChanged,
                                                                                        TB_x1y6.TextChanged,
                                                                                        TB_x1y7.TextChanged,
                                                                                        TB_x1y8.TextChanged,
                                                                                        TB_x1y9.TextChanged,
                                                                                        TB_x2y0_Header.TextChanged,
                                                                                        TB_x2y1.TextChanged,
                                                                                        TB_x2y2.TextChanged,
                                                                                        TB_x2y3.TextChanged,
                                                                                        TB_x2y4.TextChanged,
                                                                                        TB_x2y5.TextChanged,
                                                                                        TB_x2y6.TextChanged,
                                                                                        TB_x2y7.TextChanged,
                                                                                        TB_x2y8.TextChanged,
                                                                                        TB_x2y9.TextChanged,
                                                                                        TB_x3y0_Header.TextChanged,
                                                                                        TB_x3y1.TextChanged,
                                                                                        TB_x3y2.TextChanged,
                                                                                        TB_x3y3.TextChanged,
                                                                                        TB_x3y4.TextChanged,
                                                                                        TB_x3y5.TextChanged,
                                                                                        TB_x3y6.TextChanged,
                                                                                        TB_x3y7.TextChanged,
                                                                                        TB_x3y8.TextChanged,
                                                                                        TB_x3y9.TextChanged,
                                                                                        TB_x4y0_Header.TextChanged,
                                                                                        TB_x4y1.TextChanged,
                                                                                        TB_x4y2.TextChanged,
                                                                                        TB_x4y3.TextChanged,
                                                                                        TB_x4y4.TextChanged,
                                                                                        TB_x4y5.TextChanged,
                                                                                        TB_x4y6.TextChanged,
                                                                                        TB_x4y7.TextChanged,
                                                                                        TB_x4y8.TextChanged,
                                                                                        TB_x4y9.TextChanged,
                                                                                        TB_x5y0_Header.TextChanged,
                                                                                        TB_x5y1.TextChanged,
                                                                                        TB_x5y2.TextChanged,
                                                                                        TB_x5y3.TextChanged,
                                                                                        TB_x5y4.TextChanged,
                                                                                        TB_x5y5.TextChanged,
                                                                                        TB_x5y6.TextChanged,
                                                                                        TB_x5y7.TextChanged,
                                                                                        TB_x5y8.TextChanged,
                                                                                        TB_x5y9.TextChanged,
                                                                                        TB_Cluster.TextChanged,
                                                                                        TB_Database.TextChanged,
                                                                                        TB_Table.TextChanged,
                                                                                        TB_SearchInput.TextChanged

        IsDataUpdated() = True
        TB_Editor.Text = Current_TextBox().Text

        If SafeToStore() = True Then
            SetCell(sender)

            Scroll_Rows.Maximum = MaxGridSize.Y + 5
            Scroll_Columns.Maximum = MaxGridSize.X + 5

        End If


    End Sub

    Private Sub ModifyEditor_Event() Handles TB_Editor.TextChanged
        If TB_Editor.IsFocused = True And SafeToStore() = True And Current_TextBox IsNot TB_x0y0_Corner Then
            Current_TextBox.Text = TB_Editor.Text
            SetCell(Current_TextBox)
        End If
    End Sub

    Public Sub Clear()
        ClusterName() = ""
        DatabaseName() = ""
        TableName() = ""
        LocalTable.Clear()
        LocalColumns.Clear()
        LocalRows.Clear()
        ColumnIndex() = 0
        RowIndex() = 0
        SafeToStore() = True

        CellBackgroundBrush() = Background()
        CellForegroundBrush() = Foreground()
        TB_Database.Text() = ""
        TB_Table.Text() = ""
        Dim df_cell As String = ""

        TB_x0y1.Text = ""
        TB_x0y2.Text = ""
        TB_x0y3.Text = ""
        TB_x0y4.Text = ""
        TB_x0y5.Text = ""
        TB_x0y6.Text = ""
        TB_x0y7.Text = ""
        TB_x0y8.Text = ""
        TB_x0y9.Text = ""

        TB_x1y0_Header.Text = ""
        TB_x1y1.Text = ""
        TB_x1y2.Text = ""
        TB_x1y3.Text = ""
        TB_x1y4.Text = ""
        TB_x1y5.Text = ""
        TB_x1y6.Text = ""
        TB_x1y7.Text = ""
        TB_x1y8.Text = ""
        TB_x1y9.Text = ""

        TB_x2y0_Header.Text = ""
        TB_x2y1.Text = ""
        TB_x2y2.Text = ""
        TB_x2y3.Text = ""
        TB_x2y4.Text = ""
        TB_x2y5.Text = ""
        TB_x2y6.Text = ""
        TB_x2y7.Text = ""
        TB_x2y8.Text = ""
        TB_x2y9.Text = ""

        TB_x3y0_Header.Text = ""
        TB_x3y1.Text = ""
        TB_x3y2.Text = ""
        TB_x3y3.Text = ""
        TB_x3y4.Text = ""
        TB_x3y5.Text = ""
        TB_x3y6.Text = ""
        TB_x3y7.Text = ""
        TB_x3y8.Text = ""
        TB_x3y9.Text = ""

        TB_x4y0_Header.Text = ""
        TB_x4y1.Text = ""
        TB_x4y2.Text = ""
        TB_x4y3.Text = ""
        TB_x4y4.Text = ""
        TB_x4y5.Text = ""
        TB_x4y6.Text = ""
        TB_x4y7.Text = ""
        TB_x4y8.Text = ""
        TB_x4y9.Text = ""

        TB_x5y0_Header.Text = ""
        TB_x5y1.Text = ""
        TB_x5y2.Text = ""
        TB_x5y3.Text = ""
        TB_x5y4.Text = ""
        TB_x5y5.Text = ""
        TB_x5y6.Text = ""
        TB_x5y7.Text = ""
        TB_x5y8.Text = ""
        TB_x5y9.Text = ""
    End Sub


    Public Sub LoadCells(Optional ByVal arg As Integer = -1)
        Dim df_cell As String = ""

        If arg = -1 Or arg = 0 Then
            TB_x0y1.Text = Utility.GetIndex(LocalRows(), RowIndex(), df_cell)
            TB_x0y2.Text = Utility.GetIndex(LocalRows(), RowIndex() + 1, df_cell)
            TB_x0y3.Text = Utility.GetIndex(LocalRows(), RowIndex() + 2, df_cell)
            TB_x0y4.Text = Utility.GetIndex(LocalRows(), RowIndex() + 3, df_cell)
            TB_x0y5.Text = Utility.GetIndex(LocalRows(), RowIndex() + 4, df_cell)
            TB_x0y6.Text = Utility.GetIndex(LocalRows(), RowIndex() + 5, df_cell)
            TB_x0y7.Text = Utility.GetIndex(LocalRows(), RowIndex() + 6, df_cell)
            TB_x0y8.Text = Utility.GetIndex(LocalRows(), RowIndex() + 7, df_cell)
            TB_x0y9.Text = Utility.GetIndex(LocalRows(), RowIndex() + 8, df_cell)
        End If
        If arg = -1 Or arg = 1 Then
            TB_x1y0_Header.Text = Utility.GetIndex(LocalColumns(), ColumnIndex(), df_cell)
            TB_x1y1.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex(), df_cell)
            TB_x1y2.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 1, df_cell)
            TB_x1y3.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 2, df_cell)
            TB_x1y4.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 3, df_cell)
            TB_x1y5.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 4, df_cell)
            TB_x1y6.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 5, df_cell)
            TB_x1y7.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 6, df_cell)
            TB_x1y8.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 7, df_cell)
            TB_x1y9.Text = Utility.GetIndex(LocalTable(), ColumnIndex(), RowIndex() + 8, df_cell)
        End If
        If arg = -1 Or arg = 2 Then
            TB_x2y0_Header.Text = Utility.GetIndex(LocalColumns(), ColumnIndex() + 1, df_cell)
            TB_x2y1.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex(), df_cell)
            TB_x2y2.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 1, df_cell)
            TB_x2y3.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 2, df_cell)
            TB_x2y4.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 3, df_cell)
            TB_x2y5.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 4, df_cell)
            TB_x2y6.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 5, df_cell)
            TB_x2y7.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 6, df_cell)
            TB_x2y8.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 7, df_cell)
            TB_x2y9.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 8, df_cell)
        End If
        If arg = -1 Or arg = 3 Then
            TB_x3y0_Header.Text = Utility.GetIndex(LocalColumns(), ColumnIndex() + 2, df_cell)
            TB_x3y1.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex(), df_cell)
            TB_x3y2.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 1, df_cell)
            TB_x3y3.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 2, df_cell)
            TB_x3y4.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 3, df_cell)
            TB_x3y5.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 4, df_cell)
            TB_x3y6.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 5, df_cell)
            TB_x3y7.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 6, df_cell)
            TB_x3y8.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 7, df_cell)
            TB_x3y9.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 8, df_cell)
        End If
        If arg = -1 Or arg = 4 Then
            TB_x4y0_Header.Text = Utility.GetIndex(LocalColumns(), ColumnIndex() + 3, df_cell)
            TB_x4y1.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex(), df_cell)
            TB_x4y2.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 1, df_cell)
            TB_x4y3.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 2, df_cell)
            TB_x4y4.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 3, df_cell)
            TB_x4y5.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 4, df_cell)
            TB_x4y6.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 5, df_cell)
            TB_x4y7.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 6, df_cell)
            TB_x4y8.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 7, df_cell)
            TB_x4y9.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 8, df_cell)
        End If
        If arg = -1 Or arg = 5 Then
            TB_x5y0_Header.Text = Utility.GetIndex(LocalColumns(), ColumnIndex() + 4, df_cell)
            TB_x5y1.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex(), df_cell)
            TB_x5y2.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 1, df_cell)
            TB_x5y3.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 2, df_cell)
            TB_x5y4.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 3, df_cell)
            TB_x5y5.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 4, df_cell)
            TB_x5y6.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 5, df_cell)
            TB_x5y7.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 6, df_cell)
            TB_x5y8.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 7, df_cell)
            TB_x5y9.Text = Utility.GetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 8, df_cell)
        End If
    End Sub

    Private Sub SetCell(ByVal tb As TextBox)

        If tb Is TB_x0y1 Then
            Utility.SetIndex(LocalRows(), RowIndex(), TB_x0y1.Text)
        ElseIf tb Is TB_x0y2 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 1, TB_x0y2.Text)
        ElseIf tb Is TB_x0y3 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 2, TB_x0y3.Text)
        ElseIf tb Is TB_x0y4 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 3, TB_x0y4.Text)
        ElseIf tb Is TB_x0y5 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 4, TB_x0y5.Text)
        ElseIf tb Is TB_x0y6 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 5, TB_x0y6.Text)
        ElseIf tb Is TB_x0y7 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 6, TB_x0y7.Text)
        ElseIf tb Is TB_x0y8 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 7, TB_x0y8.Text)
        ElseIf tb Is TB_x0y9 Then
            Utility.SetIndex(LocalRows(), RowIndex() + 8, TB_x0y9.Text)
        End If

        If tb Is TB_x1y0_Header Then
            Utility.SetIndex(LocalColumns(), ColumnIndex(), TB_x1y0_Header.Text)
        ElseIf tb Is TB_x1y1 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex(), TB_x1y1.Text)
        ElseIf tb Is TB_x1y2 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 1, TB_x1y2.Text)
        ElseIf tb Is TB_x1y3 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 2, TB_x1y3.Text)
        ElseIf tb Is TB_x1y4 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 3, TB_x1y4.Text)
        ElseIf tb Is TB_x1y5 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 4, TB_x1y5.Text)
        ElseIf tb Is TB_x1y6 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 5, TB_x1y6.Text)
        ElseIf tb Is TB_x1y7 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 6, TB_x1y7.Text)
        ElseIf tb Is TB_x1y8 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 7, TB_x1y8.Text)
        ElseIf tb Is TB_x1y9 Then
            Utility.SetIndex(LocalTable(), ColumnIndex(), RowIndex() + 8, TB_x1y9.Text)
        End If

        If tb Is TB_x2y0_Header Then
            Utility.SetIndex(LocalColumns(), ColumnIndex() + 1, TB_x2y0_Header.Text)
        ElseIf tb Is TB_x2y1 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex(), TB_x2y1.Text)
        ElseIf tb Is TB_x2y2 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 1, TB_x2y2.Text)
        ElseIf tb Is TB_x2y3 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 2, TB_x2y3.Text)
        ElseIf tb Is TB_x2y4 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 3, TB_x2y4.Text)
        ElseIf tb Is TB_x2y5 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 4, TB_x2y5.Text)
        ElseIf tb Is TB_x2y6 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 5, TB_x2y6.Text)
        ElseIf tb Is TB_x2y7 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 6, TB_x2y7.Text)
        ElseIf tb Is TB_x2y8 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 7, TB_x2y8.Text)
        ElseIf tb Is TB_x2y9 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 1, RowIndex() + 8, TB_x2y9.Text)
        End If

        If tb Is TB_x3y0_Header Then
            Utility.SetIndex(LocalColumns(), ColumnIndex() + 2, TB_x3y0_Header.Text)
        ElseIf tb Is TB_x3y1 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex(), TB_x3y1.Text)
        ElseIf tb Is TB_x3y2 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 1, TB_x3y2.Text)
        ElseIf tb Is TB_x3y3 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 2, TB_x3y3.Text)
        ElseIf tb Is TB_x3y4 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 3, TB_x3y4.Text)
        ElseIf tb Is TB_x3y5 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 4, TB_x3y5.Text)
        ElseIf tb Is TB_x3y6 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 5, TB_x3y6.Text)
        ElseIf tb Is TB_x3y7 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 6, TB_x3y7.Text)
        ElseIf tb Is TB_x3y8 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 7, TB_x3y8.Text)
        ElseIf tb Is TB_x3y9 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 2, RowIndex() + 8, TB_x3y9.Text)
        End If

        If tb Is TB_x4y0_Header Then
            Utility.SetIndex(LocalColumns(), ColumnIndex() + 3, TB_x4y0_Header.Text)
        ElseIf tb Is TB_x4y1 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex(), TB_x4y1.Text)
        ElseIf tb Is TB_x4y2 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 1, TB_x4y2.Text)
        ElseIf tb Is TB_x4y3 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 2, TB_x4y3.Text)
        ElseIf tb Is TB_x4y4 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 3, TB_x4y4.Text)
        ElseIf tb Is TB_x4y5 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 4, TB_x4y5.Text)
        ElseIf tb Is TB_x4y6 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 5, TB_x4y6.Text)
        ElseIf tb Is TB_x4y7 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 6, TB_x4y7.Text)
        ElseIf tb Is TB_x4y8 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 7, TB_x4y8.Text)
        ElseIf tb Is TB_x4y9 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 3, RowIndex() + 8, TB_x4y9.Text)
        End If

        If tb Is TB_x5y0_Header Then
            Utility.SetIndex(LocalColumns(), ColumnIndex() + 4, TB_x5y0_Header.Text)
        ElseIf tb Is TB_x5y1 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex(), TB_x5y1.Text)
        ElseIf tb Is TB_x5y2 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 1, TB_x5y2.Text)
        ElseIf tb Is TB_x5y3 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 2, TB_x5y3.Text)
        ElseIf tb Is TB_x5y4 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 3, TB_x5y4.Text)
        ElseIf tb Is TB_x5y5 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 4, TB_x5y5.Text)
        ElseIf tb Is TB_x5y6 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 5, TB_x5y6.Text)
        ElseIf tb Is TB_x5y7 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 6, TB_x5y7.Text)
        ElseIf tb Is TB_x5y8 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 7, TB_x5y8.Text)
        ElseIf tb Is TB_x5y9 Then
            Utility.SetIndex(LocalTable(), ColumnIndex() + 4, RowIndex() + 8, TB_x5y9.Text)
        End If

    End Sub


End Class

