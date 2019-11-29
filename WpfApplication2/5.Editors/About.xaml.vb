Imports MatrixUtil
Imports Matrix

Public Class About


    Private Sub Initialize() Handles Me.Initialized
        Me.Background() = Schema.Global_Window_Color()
        Main_Border.BorderBrush() = Schema.Global_Border_Color()
        Main_Menu.ButtonInfo() = {"About|About", "Website|Website", "Donate|Donate", "Folder|Folder"}
    End Sub


    Private Sub Load() Handles Me.Loaded
        Dim text = Utility.TextReader(Folders.Data() & "about.txt")
        Send(text, Schema.Normal_Fonts())
    End Sub

    Private Sub Main_Menu_Event(ByVal button As GenerikButton) Handles Main_Menu.LeftClick

        If button.Name() = "About" Then
            About_RTB.Visibility = Visibility.Visible
            Exit Sub
        End If

        If button.Name() = "Website" Then
            Utility.OpenProgram("www.minervaai.com/")
            Exit Sub
        End If

        If button.Name() = "Donate" Then
            Utility.OpenProgram("https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5DY7B73U8XQVY")
            Exit Sub
        End If

        If button.Name() = "Folder" Then
            Utility.OpenProgram(Folders.Directory())
            Exit Sub
        End If

    End Sub


    Public Sub Send(ByVal text As String, ByVal fonts As Fonts)
        Clear()
        Dim rangeOfText1 = New TextRange(About_RTB.Document.ContentEnd, About_RTB.Document.ContentEnd)

        rangeOfText1.Text = text
        rangeOfText1.ApplyPropertyValue(TextElement.ForegroundProperty, fonts.Color)
        rangeOfText1.ApplyPropertyValue(TextElement.FontFamilyProperty, fonts.Family)
        rangeOfText1.ApplyPropertyValue(TextElement.FontSizeProperty, fonts.Size)
        rangeOfText1.ApplyPropertyValue(TextElement.FontStyleProperty, fonts.Style)
        rangeOfText1.ApplyPropertyValue(TextElement.FontStretchProperty, fonts.Stretch)
        rangeOfText1.ApplyPropertyValue(TextElement.FontWeightProperty, fonts.Weight)

    End Sub

    Public Sub Clear()
        About_RTB.Document.Blocks.Clear()
    End Sub

End Class
