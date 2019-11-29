Imports Matrix
Imports MatrixUtil
Imports MatrixBasic


Public Class LoadSplash


    Private Sub Load() Handles Me.Loaded
        Dim source = LocalResources.GetSplash()
        Main_Image.Source() = source

        Width = source.Width + 20
        Height = source.Height + 20

        Dim center = Local.FindCenter(New Point(0, 0), MouseInfo.ScreenSize(), New Size(Width, Height))
        Left = center.X
        Top = center.Y
    End Sub


End Class
