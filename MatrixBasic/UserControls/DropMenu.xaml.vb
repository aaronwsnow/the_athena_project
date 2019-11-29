Imports MatrixUtil

Public Class DropMenu

    WithEvents Buttons As New ManageGenerikButtons
    Public Property ButtonInfo() As String() = {"B1|One", "B2|Two", "B3|Three"}
    Public Property ButtonSize() As New Size(0, 0)
    Public Property Buffer() As New Thickness(10)

    Private Property BorderColor() As Brush = Schema.Global_Border_Color
    Private Shadows Property BorderThickness() As New Thickness(5)
    Public Sub Border(ByVal color As Brush, ByVal thick As Thickness)
        BorderColor() = color
        BorderThickness() = thick
    End Sub

    Private Property ButtonBorderColor() As Brush = Schema.Global_Border_Color()
    Private Shadows Property ButtonBorderThickness() As New Thickness(3)
    Public Sub ButtonBorder(ByVal color As Brush, ByVal thick As Thickness)
        ButtonBorderColor() = color
        ButtonBorderThickness() = thick
    End Sub


    Private Sub Load() Handles Me.Loaded
        Buttons.Background = Schema.Global_Window_Color()
        Buttons.ButtonBackground() = Schema.Global_FontBackColor()
        Buttons.HoveredBackground = Schema.Global_Window_Color
        Buttons.HoverFont = New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Global_HoverColor)
        Buttons.NormalFont = New Fonts(Schema.Global_Font, Schema.Global_FontSize, Schema.Global_FontColor)
        Buttons.Buffer() = Buffer()
        Buttons.ButtonBorder(ButtonBorderColor(), ButtonBorderThickness())
        Buttons.Spacing() = New Thickness(3, 3, 3, 3)
        Buttons.Border(BorderColor(), BorderThickness())
        Buttons.ButtonSize() = ButtonSize()
        Buttons.ButtonInfo() = ButtonInfo()
        Buttons.Load()

        Main_Grid.Children.Clear()
        Main_Grid.Children.Add(Buttons.Source)

    End Sub

    Public Event LeftClick(ByVal button As GenerikButton)
    Private Sub LeftClick_Event(ByVal button As GenerikButton) Handles Buttons.LeftClick
        RaiseEvent LeftClick(button)
    End Sub

    Public Event HoverEnter(ByVal button As GenerikButton)
    Private Sub HoverEnter_Event(ByVal button As GenerikButton) Handles Buttons.HoverEnter
        RaiseEvent HoverEnter(button)
    End Sub

    Public Event HoverLeave(ByVal button As GenerikButton)
    Private Sub HoverLeave_Event(ByVal button As GenerikButton) Handles Buttons.HoverLeave
        RaiseEvent HoverLeave(button)
    End Sub

    Public ReadOnly Property Size() As Size
        Get
            Return Buttons.Size()
        End Get
    End Property

End Class
