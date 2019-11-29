Imports MatrixUtil

Public Class GripX

    Private Sub Initialize() Handles Me.Initialized

    End Sub


    Private Sub HoverEnter_Event() Handles Grip_Window.MouseEnter
        MouseInfo.MouseCurser() = Cursors.Hand
    End Sub

    Private Sub HoverLeave_Event() Handles Grip_Window.MouseLeave
        MouseInfo.MouseCurser() = Cursors.Arrow
    End Sub
End Class
