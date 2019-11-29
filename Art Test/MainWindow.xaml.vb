
Class TestWindow


    Private Sub Initialize() Handles Me.Initialized
        Button_Launch.Content = "Hello Art."

        Button_Launch.FontFamily = New FontFamily("Arial")
        Button_Launch.FontSize = 62
        Button_Launch.FontWeight = FontWeights.Normal
        Button_Launch.Foreground = Brushes.White

        'Button_Launch.Background = Brushes.Black
        Main_Grid.Background = Brushes.White
    End Sub

    Private Property IsPressed() As Boolean = False
    Private Sub Button_Launch_Event() Handles Button_Launch.Click

        If IsPressed() = False Then
            Button_Launch.Content = "Hello Art."

            Button_Launch.FontFamily = New FontFamily("Arial")
            Button_Launch.FontSize = 62
            Button_Launch.FontWeight = FontWeights.Normal
            Button_Launch.Foreground = Brushes.White

            'Button_Launch.Background = Brushes.Black
            Main_Grid.Background = Brushes.White
            IsPressed() = True
        Else

            Button_Launch.FontFamily = New FontFamily("Arial")
            Button_Launch.FontSize = 62
            Button_Launch.FontWeight = FontWeights.Bold
            Button_Launch.Foreground = Brushes.Black

            Button_Launch.Content = "Yep, it works."
            'Button_Launch.Background = Brushes.White
            Main_Grid.Background = Brushes.Black
            IsPressed() = False
        End If


    End Sub



End Class
