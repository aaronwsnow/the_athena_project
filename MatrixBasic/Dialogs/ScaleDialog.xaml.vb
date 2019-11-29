Imports Matrix
Imports MatrixUtil

Public Class ScaleDialog
    Private Property SafeToStart() As Boolean = False

    Private Sub Initialize() Handles Me.Initialized
        Placement = Primitives.PlacementMode.Absolute

        Main_Slider.Background = Schema.Global_Window_Color()
        Main_Grid.Background = Schema.Global_Window_Color()

        Schema.SetStyle(Main_Border)
        Schema.SetStyle(LB_Title)

        Main_Slider.Minimum = 0.2
        Main_Slider.Maximum = 1

        Main_Slider.Value = Schema.Global_Size().Width / MouseInfo.ScreenSize().Width
        LB_Title.Content = "(" & Math.Round(Schema.Global_Size().Width, 2) & " X " & Math.Round(Schema.Global_Size().Height, 2) & ")"

    End Sub

    Private Sub Loads() Handles Me.Loaded
        MouseInfo.Load()
    End Sub

    Private Sub Scale_Event(ByVal sender As Object, ByVal e As RoutedPropertyChangedEventArgs(Of Double)) Handles Main_Slider.ValueChanged
        If SafeToStart() = True Then
            Dim value = e.NewValue

            Dim width = value * MouseInfo.ScreenSize().Width
            Dim height = value * MouseInfo.ScreenSize().Height

            LB_Title.Content = "(" & Math.Round(width, 2) & " X " & Math.Round(height, 2) & ")"
            Schema.RaiseUpdateSizeEvent(New Size(width, height))
        End If
    End Sub

    Private MouseWindow As New MouseInfo
    WithEvents Delay_Timer As New DelayDispatcher(10)
    Private Sub Delay_Timer_Event() Handles Delay_Timer.Elapsed
        Leave_Timer.Start()
    End Sub

    WithEvents Leave_Timer As New TimeDispatcher(10)
    Private Sub Leave_Timer_Delay() Handles Leave_Timer.Tick

        If MouseWindow.InWindow() = False Then
            If MouseInfo.IsLeftMousePressed() = True Then
                Close()
            End If
        End If

    End Sub

    Public Sub Show()
        IsOpen = True
        SafeToStart() = True
        MouseWindow.WindowSize(HorizontalOffset, VerticalOffset, Width, Height)
        MouseWindow.WindowMonitorStart()
        Delay_Timer.Start()
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

    Private Sub IsOpen_Event() Handles Me.Opened

    End Sub


End Class
