Imports Matrix
Imports MatrixUtil

Public Class PromptX

    WithEvents HoverTimer As New TimeDispatcher(100)

    Private Sub Initialize() Handles Me.Initialized
        HoverTimer.Start()
    End Sub

    Public Shadows WriteOnly Property BorderBrush() As Brush
        Set(value As Brush)
            RTBBorder.BorderBrush = value
            top_button.BorderBrush = value
            bottom_button.BorderBrush = value
        End Set
    End Property

    Public Sub Send(ByVal text As String, ByVal fonts As Fonts)
        Dim rangeOfText1 = New TextRange(PromptBox.Document.ContentEnd, PromptBox.Document.ContentEnd)

        rangeOfText1.Text = text
        rangeOfText1.ApplyPropertyValue(TextElement.ForegroundProperty, fonts.Color)
        rangeOfText1.ApplyPropertyValue(TextElement.FontFamilyProperty, fonts.Family)
        rangeOfText1.ApplyPropertyValue(TextElement.FontSizeProperty, fonts.Size)
        rangeOfText1.ApplyPropertyValue(TextElement.FontStyleProperty, fonts.Style)
        rangeOfText1.ApplyPropertyValue(TextElement.FontStretchProperty, fonts.Stretch)
        rangeOfText1.ApplyPropertyValue(TextElement.FontWeightProperty, fonts.Weight)

        PromptBox.ScrollToEnd()
    End Sub


    Private Shared Property LeftClicked() As Boolean = False

    Private Sub top_button_enter() Handles top_button.MouseEnter
        Dim oldColor = Local.BrushToColor(Schema.Global_FontColor())
        Dim newBrush = Local.ColorToBrush(Color.FromArgb(30, oldColor.R, oldColor.G, oldColor.B))
        top_button.Background() = newBrush
    End Sub

    Private Sub top_button_leave() Handles top_button.MouseLeave
        top_button.Background() = Brushes.Transparent
    End Sub

    Private Sub bottom_button_enter() Handles bottom_button.MouseEnter
        Dim oldColor = Local.BrushToColor(Schema.Global_FontColor())
        Dim newBrush = Local.ColorToBrush(Color.FromArgb(30, oldColor.R, oldColor.G, oldColor.B))
        bottom_button.Background() = newBrush
    End Sub

    Private Sub bottom_button_leave() Handles bottom_button.MouseLeave
        bottom_button.Background() = Brushes.Transparent
    End Sub

    Private Sub scrolling_clicked() Handles top_button.PreviewMouseLeftButtonDown, bottom_button.PreviewMouseLeftButtonDown
        LeftClicked() = True
    End Sub

    Private Sub scrolling_unclicked() Handles top_button.PreviewMouseLeftButtonUp, bottom_button.PreviewMouseLeftButtonUp
        LeftClicked() = False
    End Sub


    Private Sub scrolling() Handles HoverTimer.Tick

        If LeftClicked() = True Then
            If bottom_button.IsMouseOver = True Then
                PromptBox.Dispatcher.BeginInvoke(Sub() PromptBox.LineDown())
            End If
            If top_button.IsMouseOver = True Then
                PromptBox.Dispatcher.BeginInvoke(Sub() PromptBox.LineUp())
            End If
        End If
    End Sub

    Public ReadOnly Property Text() As String
        Get
            Return New TextRange(PromptBox.Document.ContentStart, PromptBox.Document.ContentEnd).Text
        End Get
    End Property

    Public Sub Copy()
        Clipboard.SetText(Text())
    End Sub

    Public Sub Clear()
        PromptBox.Document.Blocks.Clear()
    End Sub


End Class
