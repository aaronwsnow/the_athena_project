Imports Matrix
Imports MatrixUtil

Public Class MessageDialog

    Private Sub Initialize() Handles Me.Initialized
        Me.Icon() = LocalResources.GetIcon()

        Schema.SetStyle(Main_TextBox)
        Main_TextBox.Background = Schema.Global_Window_Color()

        Schema.SetStyle(Main_Border)
        Scroll_Border.BorderBrush = Schema.Global_Border_Color()
        Scroll_Border.BorderThickness = New Thickness(0, 0, 0, 3)

        Main_Grid.Background = Schema.Global_Window_Color
        Main_Menu.ButtonInfo() = {OK & "|" & OK, CANCEL & "|" & CANCEL}
        Main_Menu.HorizontalAlignment = HorizontalAlignment.Right
        Main_Menu.Border(Schema.Global_Border_Color, New Thickness(0))
    End Sub


    Public Property Text() As String
        Get
            Return Main_TextBox.Text
        End Get
        Set(value As String)
            Main_TextBox.Text = value
        End Set
    End Property

    Public Const OK As String = "OK"
    Public Const CANCEL As String = "CANCEL"

    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick
        Returns() = button.Name()
        Close()
    End Sub

    Public Property Returns() As String = ""

    Private m_Wrap As Boolean = True
    Public Property Wrap() As Boolean
        Get
            Return m_Wrap
        End Get
        Set(value As Boolean)

            If value = True Then
                Main_Scroll.HorizontalScrollBarVisibility = ScrollBarVisibility.Disabled
                Main_TextBox.TextWrapping = TextWrapping.Wrap
            Else
                Main_Scroll.HorizontalScrollBarVisibility = ScrollBarVisibility.Visible
                Main_TextBox.TextWrapping = TextWrapping.NoWrap
            End If

            m_Wrap = value
        End Set
    End Property

End Class

Public Class Message
    Private Shared Messanger As New MessageDialog

    Public Const OK As String = MessageDialog.OK
    Public Const CANCEL As String = MessageDialog.CANCEL

    Public Shared Function Show(ByVal text As String, ByVal x As Double, ByVal y As Double) As String
        Location() = New Point(x, y)
        Show(text)
        Return Messanger.Returns()
    End Function
    Public Shared Function Show(ByRef sender As Window, ByVal text As String) As String
        Location() = Local.FindCenter(New Point(sender.Left, sender.Top), New Size(sender.Width, sender.Height), Size())
        Return Show(text)
    End Function
    Public Shared Function Show(ByVal text As String) As String
        Messanger = New MessageDialog
        Messanger.Text() = text
        Messanger.Wrap() = Wrap()

        If Location() <> New Point(-1, -1) Then
            Messanger.Left = Location.X
            Messanger.Top = Location.Y
        Else
            Dim center = Local.FindCenter(Schema.Global_Postion(), Schema.Global_Size(), Size())
            Messanger.Left = center.X
            Messanger.Top = center.Y
        End If

        Messanger.Width = Size.Width
        Messanger.Height = Size.Height

        Messanger.ShowDialog()
        Return Messanger.Returns()
    End Function

    Public Shared Function Show(ByVal text As String(), ByVal x As Double, ByVal y As Double) As String
        Return Show(Utility.Join(text, vbCrLf), x, y)
    End Function
    Public Shared Function Show(ByRef sender As Window, ByVal text As String()) As String
        Location() = Local.FindCenter(New Point(sender.Left, sender.Top), New Size(sender.Width, sender.Height), Size())
        Return Show(text)
    End Function
    Public Shared Function Show(ByVal text As String()) As String
        Return Show(Utility.Join(text, vbCrLf))
    End Function


    Public Shared Property Location() As New Point(-1, -1)
    Public Shared Property Size() As New Size(300, 250)
    Public Shared Property Wrap() As Boolean = True

End Class