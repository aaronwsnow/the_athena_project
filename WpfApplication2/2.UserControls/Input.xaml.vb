Imports MatrixUtil

Public Class InputX

    Private Sub Initialize() Handles Me.Initialized
        InputBox.AcceptsReturn = True
    End Sub

    Public Property Text() As String
        Get
            Return InputBox.Text()
        End Get
        Set(value As String)
            InputBox.Text() = value
        End Set
    End Property

    Public Sub Clear()
        InputBox.Clear()
    End Sub

    Public Shadows Sub Focus()
        InputBox.Focus()
    End Sub

    Public WriteOnly Property Fonts() As Fonts
        Set(value As Fonts)
            InputBox.FontFamily = value.Family
            InputBox.FontSize = value.Size
            InputBox.FontStretch = value.Stretch
            InputBox.FontWeight = value.Weight
            InputBox.FontStyle = value.Style
            InputBox.Foreground = value.Color
        End Set
    End Property


    Private Sub HoverEnter_Event() Handles Main_Grid.MouseEnter
        InputBox.Focus()
        MouseInfo.MouseCurser() = Cursors.IBeam
    End Sub

    Private Sub HoverLeave_Event() Handles Main_Grid.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub


    Public Shadows Event KeyDown(ByVal sender As Object, ByVal e As KeyEventArgs)
    Public Sub KeyDown_Event(ByVal sender As Object, ByVal e As KeyEventArgs) Handles InputBox.KeyDown
        RaiseEvent KeyDown(sender, e)
    End Sub

    Public Property IsReadOnly() As Boolean
        Get
            Return InputBox.IsReadOnly
        End Get
        Set(value As Boolean)
            InputBox.IsReadOnly = value
        End Set
    End Property

    Public Property AcceptsReturn() As Boolean
        Get
            Return InputBox.AcceptsReturn
        End Get
        Set(value As Boolean)
            InputBox.AcceptsReturn = value
        End Set
    End Property

    Public Shadows Event TextChanged(ByVal sender As Object, ByVal e As TextChangedEventArgs)
    Public Sub KeyDown_Event(ByVal sender As Object, ByVal e As TextChangedEventArgs) Handles InputBox.TextChanged
        RaiseEvent TextChanged(sender, e)
    End Sub

    Public Property SelectionStart() As Integer
        Get
            Return InputBox.SelectionStart()
        End Get
        Set(value As Integer)
            InputBox.SelectionStart() = value
        End Set
    End Property

End Class
