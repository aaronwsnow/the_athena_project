Imports Matrix
Imports MatrixUtil

Public Class StatusX

    Public Shadows WriteOnly Property BorderBrush() As Brush
        Set(value As Brush)
            RTBBorder.BorderBrush = value
        End Set
    End Property

    Public Sub Send(ByVal text As String, ByVal fonts As Fonts)
        Clear()
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

    Public ReadOnly Property Text() As String
        Get
            Return New TextRange(PromptBox.Document.ContentStart, PromptBox.Document.ContentEnd).Text
        End Get
    End Property

    Public Sub Clear()
        PromptBox.Document.Blocks.Clear()
    End Sub

End Class
