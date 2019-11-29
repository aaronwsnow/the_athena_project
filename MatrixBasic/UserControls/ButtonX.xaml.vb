Imports MatrixUtil

Public Class ButtonX

    Public Event HoverEnter(ByVal sender As ButtonX)
    Public Event HoverLeave(ByVal sender As ButtonX)
    Public Event LeftClick(ByVal sender As ButtonX)

    Public WithEvents Main_Button As New GenerikButton

    Private Sub Initialize() Handles ButtonX_Window.Initialized
        Main_Grid.Children.Add(Main_Button.Source)

        NormalFont() = New Fonts(Schema.Global_Font(), Schema.Global_FontSize(), Schema.Global_FontColor())
        HoverFont() = New Fonts(Schema.Global_Font(), Schema.Global_FontSize(), Schema.Global_HoverColor())
        Background() = Schema.Global_FontBackColor()
        Border(Schema.Global_Border_Color(), New Thickness(3))
        TextBuffer(3, 3, 3, 3)
        Buffer(0, 0, 0, 0)

    End Sub

    Public Sub ResetSizeToText()
        Main_Button.ResetSizeToText()
    End Sub

    Public Property SuspendAutoHover() As Boolean
        Get
            Return Main_Button.SuspendAutoHover()
        End Get
        Set(ByVal value As Boolean)
            Main_Button.SuspendAutoHover() = value
        End Set
    End Property


    Public Property Text() As String
        Get
            Return Main_Button.Text
        End Get
        Set(value As String)
            Main_Button.Text = value
        End Set
    End Property

    Public Sub Add(ByVal addX As Double, ByVal addY As Double)
        Main_Button.Add(addX, addY)
    End Sub

    Public Sub Render()
        Main_Button.Render()
    End Sub

    Public Sub Buffer(ByVal top As Double, ByVal bottom As Double, ByVal left As Double, ByVal right As Double)
        Main_Button.Buffer(left, top, right, bottom)
    End Sub
    Public Sub TextBuffer(ByVal top As Double, ByVal bottom As Double, ByVal left As Double, ByVal right As Double)
        Main_Button.TextBuffer(left, top, right, bottom)
    End Sub

    Public Sub Border(ByVal color As Brush, ByVal thickness As Thickness)
        Main_Button.Border(color, thickness)
    End Sub

    Public Property Location() As Point
        Get
            Return New Point(Margin.Left, Margin.Top)
        End Get
        Set(ByVal value As Point)
            Margin = New Thickness(value.X, value.Y, 0, 0)
        End Set
    End Property

    Public Shadows Property Background()
        Get
            Return Main_Button.Background()
        End Get
        Set(value)
            Main_Button.Background() = value
        End Set
    End Property

    Public Property Size() As Size
        Get
            Return New Size(Main_Button.Width, Main_Button.Height)
        End Get
        Set(value As Size)
            Main_Button.Width = value.Width
            Main_Button.Height = value.Height
        End Set
    End Property

    Public ReadOnly Property Postion() As Point
        Get
            Return Main_Button.Position()
        End Get
    End Property

    Public ReadOnly Property IsHovered() As Boolean
        Get
            Return Main_Button.IsHovered()
        End Get
    End Property

    Public ReadOnly Property IsLeftDown() As Boolean
        Get
            Return Main_Button.IsLeftDown()
        End Get
    End Property

    Public Shadows Property Foreground() As Brush
        Get
            Return Main_Button.Foreground()
        End Get
        Set(value As Brush)
            Main_Button.Foreground() = value
        End Set
    End Property

    Public Property NormalFont() As Fonts
        Get
            Return Main_Button.NormalFont()
        End Get
        Set(value As Fonts)
            Main_Button.NormalFont() = value
        End Set
    End Property

    Public Property HoverFont() As Fonts
        Get
            Return Main_Button.HoverFont()
        End Get
        Set(value As Fonts)
            Main_Button.HoverFont() = value
        End Set
    End Property


    Private Sub HoverEnter_Event() Handles Main_Button.HoverEnter
        RaiseEvent HoverEnter(Me)
    End Sub
    Private Sub HoverLeave_Event() Handles Main_Button.HoverLeave
        RaiseEvent HoverLeave(Me)
    End Sub
    Private Sub Left_Click() Handles Main_Button.LeftClick
        RaiseEvent LeftClick(Me)
    End Sub

End Class

