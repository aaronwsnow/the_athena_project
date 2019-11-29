Imports Matrix
Imports MatrixUtil
Imports MatrixBasic
Imports System.Text.RegularExpressions
Imports FastColoredTextBoxNS
Imports System.Windows.Forms


Public Class FastScript

    Private Sub Initial() Handles Me.Initialized
        ScriptLanguage() = FastColoredTextBoxNS.Language.VB
    End Sub

    Private Sub Loads() Handles Me.Loaded
        MouseInfo.Load()

        ScriptBox.Text = ""

        'move caret to start text
        ScriptBox.Selection.Start = Place.Empty
        ScriptBox.DoCaretVisible()
        ScriptBox.IsChanged = False
        ScriptBox.ClearUndo()

        ScriptBox.ShowLineNumbers() = True
        ScriptBox.ShowFoldingLines() = True
        ScriptBox.ShowScrollBars() = True

        If ShowHighlight() = True Then
            InitialHighLighting()
        Else
            ScriptLanguage() = FastColoredTextBoxNS.Language.Custom
        End If

        If ShowPopup() = True Then
            InitialAutoComplete()
        End If

    End Sub

    Public Sub Font(ByVal xfonts As Fonts)

        ScriptBox.Font() = New System.Drawing.Font(Schema.Script_Font(), Schema.Script_FontSize(), System.Drawing.FontStyle.Regular)

        ScriptBox.ForeColor() = Schema.Script_FontColor()
        ScriptBox.BackColor = Schema.Script_BackColor()

        ScriptBox.LineNumberColor() = Local.ToColor(Schema.Global_FontColor())  'Schema.Script_BackColor()
        ScriptBox.IndentBackColor() = Local.ToColor(Schema.Global_FontBackColor())


        'ScriptBox.BookmarkColor() = System.Drawing.Color.Blue
        'ScriptBox.TextAreaBorderColor() = System.Drawing.Color.Blue
        'ScriptBox.CurrentLineColor() = System.Drawing.Color.Blue
        'ScriptBox.ChangedLineColor() = System.Drawing.Color.Blue
        'ScriptBox.LineNumberColor() = System.Drawing.Color.Blue
        'ScriptBox.IndentBackColor() = System.Drawing.Color.Blue
        'ScriptBox.DisabledColor() = System.Drawing.Color.LightGreen
        'ScriptBox.CaretColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceLinesColor() = System.Drawing.Color.Pink
        'ScriptBox.FoldingIndicatorColor() = System.Drawing.Color.Blue
        'ScriptBox.SelectionColor() = System.Drawing.Color.Blue

        'ScriptBox.ServiceColors.CollapseMarkerBackColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceColors.CollapseMarkerBorderColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceColors.CollapseMarkerForeColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceColors.ExpandMarkerBackColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceColors.ExpandMarkerBorderColor() = System.Drawing.Color.Blue
        'ScriptBox.ServiceColors.ExpandMarkerForeColor() = System.Drawing.Color.Blue


        KeyWordStyle = New TextStyle(Schema.Script_KeyWordColor(), Nothing, System.Drawing.FontStyle.Regular)
        ClassStyle = New TextStyle(Schema.Script_ClassColor(), Nothing, System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Underline)
        AttributeStyle = New TextStyle(Schema.Script_AttributeColor(), Nothing, System.Drawing.FontStyle.Regular)
        NumberStyle = New TextStyle(Schema.Script_NumberColor(), Nothing, System.Drawing.FontStyle.Regular)
        CommentStyle = New TextStyle(Schema.Script_CommentColor(), Nothing, System.Drawing.FontStyle.Italic)
        StringStyle = New TextStyle(Schema.Script_StringColor(), Nothing, System.Drawing.FontStyle.Italic)

    End Sub


    Public WriteOnly Property ScriptLanguage() As FastColoredTextBoxNS.Language
        Set(ByVal value As FastColoredTextBoxNS.Language)
            ScriptBox.Language = value
        End Set
    End Property

    Public Property ShowPopup() As Boolean = True
    Public Property ShowHighlight As Boolean = True


    Public Sub Clear()
        Text() = ""
    End Sub

    Public Property Text() As String
        Get
            Return ScriptBox.Text()
        End Get
        Set(value As String)
            ScriptBox.Text() = value
        End Set
    End Property

    Public Sub ShowFindDialog()
        ScriptBox.ShowFindDialog()
    End Sub

    Public Sub ShowReplaceDialog()
        ScriptBox.ShowReplaceDialog()
    End Sub

    Public Sub ShowGoToDialog()
        ScriptBox.ShowGoToDialog()
    End Sub

    Public Sub AddBookmark()
        ScriptBox.Bookmarks.Add(ScriptBox.Selection.Start.iLine)
    End Sub

    Public Sub RemoveBookmark()
        ScriptBox.Bookmarks.Remove(ScriptBox.Selection.Start.iLine)
    End Sub

    Public Sub GotoNextBookmark()
        ScriptBox.GotoNextBookmark(ScriptBox.Selection.Start.iLine)
    End Sub

    Public Sub GotoPreviousBoomark()
        ScriptBox.GotoPrevBookmark(ScriptBox.Selection.Start.iLine)
    End Sub

    Public Function ListOfBookmarks() As List(Of String)
        Dim newList As New List(Of String)
        For Each marks In ScriptBox.Bookmarks
            newList.Add(marks.Name())
        Next

        Return newList
    End Function


#Region "Context Menu"

    WithEvents localContextMenu As New MenuDialog

    Public Sub localContextMenu_Select() Handles ScriptBox.MouseClick

        If MouseInfo.IsRightMousePressed() = True Then
            localContextMenu.Close()
            IsEnabled = False

            localContextMenu = New MenuDialog()
            localContextMenu.ButtonInfo() = {"Paste|Paste", "Copy|Copy", "Cut|Cut", "Undo|Undo", "Redo|Redo", "SelectAll|SelectAll"}
            localContextMenu.Orientation = Orientation.Vertical
            localContextMenu.Buffer() = New Thickness(5)
            localContextMenu.Load()

            localContextMenu.Placement = Primitives.PlacementMode.Absolute
            localContextMenu.Location() = MouseInfo.GetMousePosition()
            localContextMenu.Show()

        End If

    End Sub

    Private Sub MouseEvent() Handles localContextMenu.IsClosed
        Context_Timer.Start(10)
    End Sub

    Private Sub LeftClick_Events(ByVal button As GenerikButton) Handles localContextMenu.LeftClick

        If button.Name() = "Paste" Then
            ScriptBox.Paste()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

        If button.Name() = "Copy" Then
            ScriptBox.Copy()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

        If button.Name() = "Cut" Then
            ScriptBox.Cut()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

        If button.Name() = "SelectAll" Then
            ScriptBox.SelectAll()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

        If button.Name() = "Undo" Then
            ScriptBox.Undo()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

        If button.Name() = "Redo" Then
            ScriptBox.Redo()
            Context_Timer.Start(10)
            localContextMenu.Close()
            Exit Sub
        End If

    End Sub

    WithEvents Context_Timer As New DelayDispatcher()
    Private Sub Context_Delay() Handles Context_Timer.Elapsed
        IsEnabled() = True
    End Sub

#End Region


#Region "AutoComplete"
    Private popupMenu As AutocompleteMenu
    Private keywords As String() = {"Dim", "Public", "Private", "Property", "Function", "Sub", "End", "#Region"}
    Private methods As String() = {"Equals()", "ToString()"}
    Private snippets As String() = {"End Sub", "End Function", "#End Region"}

    Private declarationSnippets As String() = {"Public Class ^" & vbLf & vbLf & "End Class",
                                               "Private Class ^" & vbLf & vbLf & "End Class",
                                               "Public Sub ^" & vbLf & vbLf & "End Sub",
                                               "Private Sub ^" & vbLf & vbLf & "End Sub",
                                               "Public Property ^" & vbLf & "Get" & vbCrLf & vbCrLf & "End Get" & vbCrLf & "Set(value)" & vbCrLf & vbCrLf & "End Set" & vbLf & "End Property",
                                               "Private Property ^" & vbLf & "Get" & vbCrLf & vbCrLf & "End Get" & vbCrLf & "Set(value)" & vbCrLf & vbCrLf & "End Set" & vbLf & "End Property",
                                               "Public Function ^" & vbLf & vbLf & "End Function",
                                               "Private Function ^" & vbLf & vbLf & "End Function",
                                               "If ^ Then" & vbCrLf & vbCrLf & "End If",
                                               "For Each ^ In " & vbCrLf & vbCrLf & "Next",
                                               "For i = 0 To ^" & vbCrLf & vbCrLf & "Next",
                                               ""
    }

    Private Sub InitialAutoComplete()
        Dim newImageList As New ImageList

        Dim RedImage = Bitmaps.CreateBitmap(5, 5, {{Colors.Red, Colors.Green}, {Colors.Blue, Colors.Yellow}}, 5, 5, True)
        Dim BlueImage = Bitmaps.CreateBitmap(20, 20, Colors.Blue)
        Dim GreenImage = Bitmaps.CreateBitmap(20, 20, Colors.Green)

        newImageList.Images.Add("DeclarationSnippet", RedImage)
        newImageList.Images.Add("SnippetAutocompleteItem", BlueImage)
        newImageList.Images.Add("MethodAutocompleteItem", GreenImage)

        newImageList.TransparentColor = System.Drawing.Color.Transparent
        newImageList.Images.SetKeyName(0, "DeclarationSnippet")
        newImageList.Images.SetKeyName(1, "SnippetAutocompleteItem")
        newImageList.Images.SetKeyName(2, "MethodAutocompleteItem")

        'create autocomplete popup menu
        popupMenu = New AutocompleteMenu(ScriptBox)
        popupMenu.Items.ImageList = newImageList
        popupMenu.SearchPattern = "[\w\.:=!<>]"

        BuildAutocompleteMenu()
    End Sub

    Private Sub BuildAutocompleteMenu()
        Dim items As New List(Of AutocompleteItem)()

        For Each item As String In snippets
            items.Add(New SnippetAutocompleteItem(item) With {.ImageIndex = 1})
        Next
        For Each item As String In declarationSnippets
            items.Add(New DeclarationSnippet(item) With {.ImageIndex = 0})
        Next
        For Each item As String In methods
            items.Add(New MethodAutocompleteItem(item) With {.ImageIndex = 2})
        Next
        For Each item As String In keywords
            items.Add(New AutocompleteItem(item))
        Next

        items.Add(New InsertSpaceSnippet())
        items.Add(New InsertSpaceSnippet("^(\w+)([=<>!:]+)(\w+)$"))
        items.Add(New InsertEnterSnippet())

        'set as autocomplete source
        popupMenu.Items.SetAutocompleteItems(items)
    End Sub


#End Region


#Region "Highlighting"

    Private Property KeyWordStyle() As TextStyle
    Private Property ClassStyle() As TextStyle
    Private Property AttributeStyle() As TextStyle
    Private Property NumberStyle() As TextStyle
    Private Property CommentStyle() As TextStyle
    Private Property StringStyle() As TextStyle

    'Private Property MaroonStyle As New TextStyle(System.Drawing.Brushes.Maroon, Nothing, System.Drawing.FontStyle.Regular)
    Private Property SameWordsStyle As New MarkerStyle(New System.Drawing.SolidBrush(System.Drawing.Color.FromArgb(40, System.Drawing.Color.Gray)))


    Private Sub InitialHighLighting()
        ScriptBox.ClearStylesBuffer()
        'add this style explicitly for drawing under other styles
        ScriptBox.AddStyle(SameWordsStyle)
        ScriptBox.OnSyntaxHighlight(New TextChangedEventArgs(ScriptBox.Range))
    End Sub

    Private Sub FastColoredTextBox1_TextChanged(ByVal sender As System.Object, ByVal e As TextChangedEventArgs) Handles ScriptBox.TextChanged
        If ShowHighlight() = True Then
            CustomSyntaxHighlight(e)
        End If
    End Sub

    Private Sub CustomSyntaxHighlight(ByVal e As TextChangedEventArgs)
        '  ScriptBox.LeftBracket = "("
        '  ScriptBox.RightBracket = ")"
        '  ScriptBox.LeftBracket2 = "\x0"
        '  ScriptBox.RightBracket2 = "\x0"

        'clear style of changed range
        e.ChangedRange.ClearStyle(KeyWordStyle, ClassStyle, AttributeStyle, NumberStyle, CommentStyle, StringStyle)

        'string highlighting
        e.ChangedRange.SetStyle(StringStyle, """.*?""|'.+?'")

        'comment highlighting
        e.ChangedRange.SetStyle(CommentStyle, "'.*$", RegexOptions.Multiline)
        e.ChangedRange.SetStyle(CommentStyle, "REM .*$", RegexOptions.Multiline)
        e.ChangedRange.SetStyle(CommentStyle, "Rem .*$", RegexOptions.Multiline)

        'number highlighting
        e.ChangedRange.SetStyle(NumberStyle, "\b\d+[\.]?\d*([eE]\-?\d+)?[lLdDfF]?\b|\b0x[a-fA-F\d]+\b")

        'attribute highlighting
        e.ChangedRange.SetStyle(AttributeStyle, "^\s*(?<range>\[.+?\])\s*$", RegexOptions.Multiline)

        'class name highlighting
        e.ChangedRange.SetStyle(ClassStyle, "\b(Class|Structure|Enum|Module)\s+(?<range>\w+?)\b")
        e.ChangedRange.SetStyle(ClassStyle, "\b(Class|Structure|Enum|Module|End Module)\b")

        'keyword highlighting
        e.ChangedRange.SetStyle(KeyWordStyle, "\b(Public|Private|Partial|Sub|Function|Alias|Lib|Implements|Abstract|Inherits|Interface|Shadows|Option|PramArray|Protected|Friend
                                            |Delegate|Declare|Default|Overridable|NotOverridable|MustInherit|Namespace|MustOverride|Narrowing|Widdening|Overloads|Dim|Property|ReadOnly|WriteOnly|Mod|For|Each|To|Next
                                            |With|While|Error|Resume|Optional|ByVal|ByRef|Get|Set|Then|If|End|Case|Select|Else|Overides|Declare|Overloads|As|Base|Const|Shared|If|Then|When|Imports|Option|Explicit|On
                                            |Off|Strict|Infer|Compare|Binary|Module|Boolean|True|False|Nothing|New|Do|Loop|Step|Until|In|Stop|WithEvents|RaiseEvent|RemoveHandler|AddHandler|AddressOf|Catch|Try|Finally
                                            |Handles|Exception|Exit|Return|Continue|GoTo|String|Byte|SByte|Integer|UInteger|Double|Single|Long|ULong|Short|UShort|Decimal|Char|Object|Not|ReDim|And|AndAlso|Of|Or|OrElse
                                            |Not|Nor|Xor|Xnor|Is|IsNot|TryCast|Let|Find|Yeild|Me|MyBase|Exit|Global|SyncLock|Like|\|+|=|&|#Const|#If)\b|#Region\b|#End\b|Region\b|#ElseIf\b")

        'clear folding markers
        e.ChangedRange.ClearFoldingMarkers()
        'set folding markers
        e.ChangedRange.SetFoldingMarkers("Public Function", "End Function")
        e.ChangedRange.SetFoldingMarkers("Private Function", "End Function")
        e.ChangedRange.SetFoldingMarkers("Public Shared Function", "End Function")
        e.ChangedRange.SetFoldingMarkers("Private Shared Function", "End Function")

        e.ChangedRange.SetFoldingMarkers("Public Sub", "End Sub")
        e.ChangedRange.SetFoldingMarkers("Private Sub", "End Sub")
        e.ChangedRange.SetFoldingMarkers("Public Shared Sub", "End Sub")
        e.ChangedRange.SetFoldingMarkers("Private Shared Sub", "End Sub")

        'e.ChangedRange.SetFoldingMarkers("Public Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Public ReadOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Private ReadOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Public WriteOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Private WriteOnly Property", "End Property")

        'e.ChangedRange.SetFoldingMarkers("Public Shared Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Public Shared ReadOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Private Shared ReadOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Public Shared WriteOnly Property", "End Property")
        'e.ChangedRange.SetFoldingMarkers("Private Shared WriteOnly Property", "End Property")

        e.ChangedRange.SetFoldingMarkers("Module", "End Module")
        e.ChangedRange.SetFoldingMarkers("Public Module", "End Module")
        e.ChangedRange.SetFoldingMarkers("Private Module", "End Module")

        e.ChangedRange.SetFoldingMarkers("Select Case", "End Select")
        e.ChangedRange.SetFoldingMarkers("#Region\b", "#End Region\b") 'allow to collapse #region blocks
    End Sub

#End Region



    ''' <summary>
    ''' This item appears when any part of snippet text is typed
    ''' </summary>
    Private Class DeclarationSnippet
        Inherits SnippetAutocompleteItem
        Public Sub New(ByVal snippet As String)
            MyBase.New(snippet)
        End Sub

        Public Overrides Function Compare(ByVal fragmentText As String) As CompareResult
            Dim pattern = Regex.Escape(fragmentText)
            If Regex.IsMatch(Text, "\b" & pattern, RegexOptions.IgnoreCase) Then
                Return CompareResult.Visible
            End If
            Return CompareResult.Hidden
        End Function
    End Class

    ''' <summary>
    ''' Divides numbers and words: "123AND456" -> "123 AND 456"
    ''' Or "i=2" -> "i = 2"
    ''' </summary>
    Private Class InsertSpaceSnippet
        Inherits AutocompleteItem
        Private pattern As String

        Public Sub New(ByVal pattern As String)
            MyBase.New("")
            Me.pattern = pattern
        End Sub


        Public Sub New()
            Me.New("^(\d+)([a-zA-Z_]+)(\d*)$")
        End Sub

        Public Overrides Function Compare(ByVal fragmentText As String) As CompareResult
            If Regex.IsMatch(fragmentText, pattern) Then
                Text = InsertSpaces(fragmentText)
                If Text <> fragmentText Then
                    Return CompareResult.Visible
                End If
            End If
            Return CompareResult.Hidden
        End Function

        Public Function InsertSpaces(ByVal fragment As String) As String
            Dim m = Regex.Match(fragment, pattern)
            If m Is Nothing Then
                Return fragment
            End If
            If m.Groups(1).Value = "" AndAlso m.Groups(3).Value = "" Then
                Return fragment
            End If
            Return (m.Groups(1).Value & " " & m.Groups(2).Value & " " & m.Groups(3).Value).Trim()
        End Function

        Public Overrides Property ToolTipTitle() As String
            Get
                Return Text
            End Get
            Set(ByVal value As String)
            End Set
        End Property
    End Class

    ''' <summary>
    ''' Inerts line break after '}'
    ''' </summary>
    Private Class InsertEnterSnippet
        Inherits AutocompleteItem
        Private enterPlace As Place = Place.Empty

        Public Sub New()
            MyBase.New("[Line break]")
        End Sub

        Public Overrides Function Compare(ByVal fragmentText As String) As CompareResult
            Dim r = Parent.Fragment.Clone()
            While r.Start.iChar > 0
                'If r.CharBeforeStart = "}"c Then
                '   enterPlace = r.Start
                '   Return CompareResult.Visible
                'End If

                r.GoLeftThroughFolded()
            End While

            Return CompareResult.Hidden
        End Function

        Public Overrides Function GetTextForReplace() As String
            'extend range
            Dim r As Range = Parent.Fragment
            Dim [end] As Place = r.[End]
            r.Start = enterPlace
            r.[End] = r.[End]
            'insert line break
            Return Environment.NewLine + r.Text
        End Function

        Public Overrides Sub OnSelected(ByVal popupMenu As AutocompleteMenu, ByVal e As SelectedEventArgs)
            MyBase.OnSelected(popupMenu, e)
            If Parent.Fragment.tb.AutoIndent Then
                Parent.Fragment.tb.DoAutoIndent()
            End If
        End Sub

        Public Overrides Property ToolTipTitle() As String
            Get
                Return "" '"Insert line break after '}'"
            End Get
            Set(ByVal value As String)
            End Set
        End Property
    End Class


End Class
