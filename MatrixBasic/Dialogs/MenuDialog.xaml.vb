Imports Matrix
Imports MatrixUtil

Public Class MenuDialog
    WithEvents Buttons As New ManageGenerikButtons
    Private MouseWindow As New MouseInfo

    Public Property ButtonInfo() As String() = {"B1|One", "B2|Two", "B3|Three"}
    Public Property ButtonSize() As New Size(0, 0)
    Public Property Buffer() As New Thickness(30)
    Public Property TextHorizontalAlignment() As HorizontalAlignment
    Public Property TextVerticalAlignment() As VerticalAlignment
    Public Property TextSizeOveride() As Double = 0

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


    Public Sub Load()
        Buttons.Background = Schema.Global_Window_Color
        Buttons.ButtonBackground() = Schema.Global_FontBackColor()
        Buttons.HoveredBackground = Schema.Global_Window_Color
        MouseInfo.Load()

        If TextSizeOveride() = 0 Then
            Buttons.HoverFont = Schema.Hover_Fonts()
            Buttons.NormalFont = Schema.Normal_Fonts()
        Else
            Buttons.HoverFont = New Fonts(Schema.Hover_Fonts.Family(),
                                          TextSizeOveride(),
                                          Schema.Hover_Fonts.Style(),
                                          Schema.Hover_Fonts.Weight(),
                                          Schema.Hover_Fonts.Stretch(),
                                          Schema.Hover_Fonts.Color())

            Buttons.NormalFont = New Fonts(Schema.Normal_Fonts.Family(),
                                          TextSizeOveride(),
                                          Schema.Normal_Fonts.Style(),
                                          Schema.Normal_Fonts.Weight(),
                                          Schema.Normal_Fonts.Stretch(),
                                          Schema.Normal_Fonts.Color())

        End If

        Buttons.Buffer() = Buffer()
        Buttons.ButtonBorder(ButtonBorderColor(), ButtonBorderThickness())
        Buttons.Spacing() = New Thickness(3, 3, 3, 3)
        Buttons.Border(BorderColor(), BorderThickness())
        Buttons.TextHorizontalAlignment = TextHorizontalAlignment()
        Buttons.TextVerticalAlignment = TextVerticalAlignment()
        Buttons.ButtonSize() = ButtonSize()
        Buttons.ButtonInfo() = ButtonInfo()
        Buttons.Load()

        Main_Grid.Children.Clear()
        Main_Grid.Children.Add(Buttons.Source)

        Width = Buttons.Size.Width
        Height = Buttons.Size.Height

        Placement = Primitives.PlacementMode.Absolute
        Delay_Timer.Start()
    End Sub


    Public Property Orientation()
        Get
            Return Buttons.Orientation()
        End Get
        Set(value)
            Buttons.Orientation() = value
        End Set
    End Property

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

    Private Property IsHovered() As Boolean = False
    Private Sub MenuHoverEnter_Event() Handles Me.MouseEnter
        IsHovered() = True
    End Sub

    Private Sub MenuHoverLeave_Event() Handles Me.MouseLeave
        IsHovered() = False
    End Sub

    Private Sub Delay_Timer_Event() Handles Delay_Timer.Elapsed
        Leave_Timer.Start()
    End Sub

    Public Event IsClosed()
    WithEvents Delay_Timer As New DelayDispatcher(20)
    WithEvents Leave_Timer As New TimeDispatcher(10)
    Private Sub Leave_Timer_Delay() Handles Leave_Timer.Tick

        If IsHovered() = False Then
            If MouseInfo.IsLeftMousePressed() = True Then
                RaiseEvent IsClosed()
                IsOpen = False
            End If
        End If
    End Sub


    Public Sub Show()
        IsOpen = True
    End Sub

    Public Sub Close()
        IsOpen = False
    End Sub

    Public WriteOnly Property Location() As Point
        Set(value As Point)
            VerticalOffset = value.Y
            HorizontalOffset = value.X
        End Set
    End Property

    Public Shadows ReadOnly Property Size As Size
        Get
            Return Buttons.Size()
        End Get
    End Property

    Public Shadows Property HorizontalAlignment() As HorizontalAlignment
        Get
            Return Buttons.HorizontalAlignment
        End Get
        Set(ByVal value As HorizontalAlignment)
            Buttons.HorizontalAlignment = value
        End Set
    End Property

    Public Shadows Property VerticalAlignment() As VerticalAlignment
        Get
            Return Buttons.VerticalAlignment
        End Get
        Set(ByVal value As VerticalAlignment)
            Buttons.VerticalAlignment = value
        End Set
    End Property


End Class
