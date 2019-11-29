Imports Matrix
Imports MatrixUtil

Public Class ColorPickerDialog

    Private Sub Initialize() Handles Me.Initialized
        Me.Icon() = LocalResources.GetIcon()

        Slider_Red.Background = Schema.Global_Window_Color()
        Slider_Blue.Background = Schema.Global_Window_Color()
        Slider_Green.Background = Schema.Global_Window_Color()
        Slider_Alpha.Background = Schema.Global_Window_Color()
        Main_Grid.Background = Schema.Global_Window_Color()

        Schema.SetStyle(Main_Border)
        Schema.SetStyle(Border_Color)

        Schema.SetStyle(LB_Blue)
        Schema.SetStyle(LB_Green)
        Schema.SetStyle(LB_Red)
        Schema.SetStyle(LB_Alpha)

        Schema.SetStyle(TB_Blue)
        Schema.SetStyle(TB_Green)
        Schema.SetStyle(TB_Red)
        Schema.SetStyle(TB_Alpha)

        Slider_Red.Minimum = 0
        Slider_Red.Maximum = 255
        Slider_Blue.Minimum = 0
        Slider_Blue.Maximum = 255
        Slider_Green.Minimum = 0
        Slider_Green.Maximum = 255
        Slider_Alpha.Minimum = 0
        Slider_Alpha.Maximum = 255

        Main_Menu.ButtonInfo() = {OK & "|" & OK, CANCEL & "|" & CANCEL}
        Main_Menu.HorizontalAlignment = HorizontalAlignment.Right
        Main_Menu.Border(Schema.Global_Border_Color, New Thickness(0))

    End Sub

    Private Sub Loading() Handles Me.Loaded

        Grid_Color.Background = InitialBrush()

        CurrentRed() = InitialBrush.Color.R
        CurrentBlue() = InitialBrush.Color.B
        CurrentGreen() = InitialBrush.Color.G
        CurrentAlpha() = InitialBrush.Color.A

        Slider_Red.Value = CurrentRed()
        Slider_Blue.Value = CurrentBlue()
        Slider_Green.Value = CurrentGreen()
        Slider_Alpha.Value = CurrentAlpha()

        TB_Red.Text() = CurrentRed()
        TB_Blue.Text() = CurrentBlue()
        TB_Green.Text() = CurrentGreen()
        TB_Alpha.Text() = CurrentAlpha()
    End Sub

    Private Property CurrentRed() As Byte = 0
    Private Property CurrentBlue() As Byte = 0
    Private Property CurrentGreen() As Byte = 0
    Private Property CurrentAlpha() As Byte = 255

    Public Property CurrentBrush() As Brush = New SolidColorBrush(CurrentColor())
    Public Property CurrentColor() As Color = Colors.Black

    Private Sub Slider_Red_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider_Red.ValueChanged
        CurrentRed() = e.NewValue
        TB_Red.Text() = CurrentRed()
        ResetColor()
    End Sub
    Private Sub Slider_Blue_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider_Blue.ValueChanged
        CurrentBlue() = e.NewValue
        TB_Blue.Text() = CurrentBlue()
        ResetColor()
    End Sub
    Private Sub Slider_Green_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider_Green.ValueChanged
        CurrentGreen() = e.NewValue
        TB_Green.Text() = CurrentGreen()
        ResetColor()
    End Sub
    Private Sub Slider_Alpha_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider_Alpha.ValueChanged
        CurrentAlpha() = e.NewValue
        TB_Alpha.Text() = CurrentAlpha()
        ResetColor()
    End Sub

    Private Sub ResetColor()
        CurrentColor() = Color.FromArgb(CurrentAlpha(), CurrentRed(), CurrentGreen(), CurrentBlue())
        CurrentBrush() = New SolidColorBrush(CurrentColor())
        Grid_Color.Background = CurrentBrush()
    End Sub

    Private Sub TB_Green_TextChanged_Event() Handles TB_Green.TextChanged
        If Utility.IsInt(TB_Green.Text()) = False Then
            TB_Green.Text() = Utility.IntOnly(TB_Green.Text())
        End If
    End Sub
    Private Sub TB_Green_LostFocus_Event() Handles TB_Green.LostFocus
        Dim intGreen = Utility.ToInt(TB_Green.Text())
        CurrentGreen() = Utility.ToByte(intGreen)

        If intGreen > 255 Then
            CurrentGreen() = 255
            TB_Green.Text() = 255
        End If
        If intGreen < 0 Then
            CurrentGreen() = 0
            TB_Green.Text() = 0
        End If

        Slider_Green.Value = CurrentGreen()
    End Sub

    Private Sub TB_Red_TextChanged_Event() Handles TB_Red.TextChanged
        If Utility.IsInt(TB_Red.Text()) = False Then
            TB_Red.Text() = Utility.IntOnly(TB_Red.Text())
        End If
    End Sub
    Private Sub TB_Red_LostFocus_Event() Handles TB_Red.LostFocus
        Dim intRed = Utility.ToInt(TB_Red.Text)
        CurrentRed() = Utility.ToByte(intRed)

        If intRed > 255 Then
            CurrentRed() = 255
            TB_Red.Text() = 255
        End If
        If intRed < 0 Then
            CurrentRed() = 0
            TB_Red.Text() = 0
        End If

        Slider_Red.Value = CurrentRed()
    End Sub

    Private Sub TB_Blue_TextChanged_Event() Handles TB_Blue.TextChanged
        If Utility.IsInt(TB_Blue.Text()) = False Then
            TB_Blue.Text() = Utility.IntOnly(TB_Blue.Text())
        End If
    End Sub
    Private Sub TB_Blue_LostFocus_Event() Handles TB_Blue.LostFocus
        Dim intBlue = Utility.ToInt(TB_Blue.Text)
        CurrentBlue() = Utility.ToByte(intBlue)

        If intBlue > 255 Then
            CurrentBlue() = 255
            TB_Blue.Text() = 255
        End If
        If intBlue < 0 Then
            CurrentBlue() = 0
            TB_Blue.Text() = 0
        End If

        Slider_Blue.Value = CurrentBlue()
    End Sub

    Private Sub TB_Alpha_TextChanged_Event() Handles TB_Alpha.TextChanged
        If Utility.IsInt(TB_Alpha.Text()) = False Then
            TB_Alpha.Text() = Utility.IntOnly(TB_Alpha.Text())
        End If
    End Sub
    Private Sub TB_Alpha_LostFocus_Event() Handles TB_Alpha.LostFocus
        Dim intAlpha = Utility.ToInt(TB_Alpha.Text)
        CurrentAlpha() = Utility.ToByte(intAlpha)

        If intAlpha > 255 Then
            CurrentAlpha() = 255
            TB_Alpha.Text() = 255
        End If
        If intAlpha < 0 Then
            CurrentAlpha() = 0
            TB_Alpha.Text() = 0
        End If

        Slider_Alpha.Value = CurrentAlpha()
    End Sub

    Public WriteOnly Property Location() As Point
        Set(value As Point)
            Top = value.Y
            Left = value.X
        End Set
    End Property

    Public WriteOnly Property Size() As Size
        Set(value As Size)
            Height = value.Height
            Width = value.Width
        End Set
    End Property

    Public Const OK As String = "OK"
    Public Const CANCEL As String = "CANCEL"

    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick
        Returns() = button.Name()
        Close()
    End Sub

    Public Property Returns() As String = ""
    Public Property InitialBrush() As SolidColorBrush = Brushes.Black

End Class

Public Class ColorPicker
    Private Shared ColorPick As New ColorPickerDialog

    Public Const OK As String = ColorPickerDialog.OK
    Public Const CANCEL As String = ColorPickerDialog.CANCEL

    Public Shared Function Show(ByVal x As Double, ByVal y As Double) As String
        Location() = New Point(x, y)
        Show()
        Return ColorPick.Returns()
    End Function
    Public Shared Function Show() As String
        ColorPick = New ColorPickerDialog
        ColorPick.Location() = Location()
        ColorPick.Size() = Size()
        ColorPick.InitialBrush() = InitialBrush()
        ColorPick.ShowDialog()

        Return ColorPick.Returns()
    End Function

    Public Shared Property Location() As New Point(-1, -1)
    Public Shared Property Size() As New Size(300, 250)
    Public Shared ReadOnly Property CurrentBrush() As Brush
        Get
            Return ColorPick.CurrentBrush()
        End Get
    End Property
    Public Shared ReadOnly Property CurrentColor() As Color
        Get
            Return ColorPick.CurrentColor()
        End Get
    End Property

    Public Shared Property InitialBrush() As SolidColorBrush

    Public Shared Sub Close()
        ColorPick.Close()
    End Sub

End Class