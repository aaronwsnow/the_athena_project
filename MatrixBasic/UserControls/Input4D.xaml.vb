Imports Matrix
Imports MatrixUtil

Public Class Input4D

    Private Sub Initialize() Handles Me.Initialized

        Schema.SetStyle(LB_Input_H)
        Schema.SetStyle(LB_Input_W)
        Schema.SetStyle(LB_Input_X)
        Schema.SetStyle(LB_Input_Y)

        Schema.SetStyle(Main_Border)

        Schema.SetStyle(TB_Input_H)
        Schema.SetStyle(TB_Input_W)
        Schema.SetStyle(TB_Input_X)
        Schema.SetStyle(TB_Input_Y)

    End Sub

    Private Sub TB_Input_H_TextChange() Handles TB_Input_H.TextChanged
        If Utility.IsInt(TB_Input_H.Text()) = False Then
            TB_Input_H.Text() = Utility.IntOnly(TB_Input_H.Text())
        End If
    End Sub

    Private Sub TB_Input_W_TextChange() Handles TB_Input_W.TextChanged
        If Utility.IsInt(TB_Input_W.Text()) = False Then
            TB_Input_W.Text() = Utility.IntOnly(TB_Input_W.Text())
        End If
    End Sub

    Private Sub TB_Input_X_TextChange() Handles TB_Input_X.TextChanged
        If Utility.IsInt(TB_Input_X.Text()) = False Then
            TB_Input_X.Text() = Utility.IntOnly(TB_Input_X.Text())
        End If
    End Sub

    Private Sub TB_Input_Y_TextChange() Handles TB_Input_Y.TextChanged
        If Utility.IsInt(TB_Input_Y.Text()) = False Then
            TB_Input_Y.Text() = Utility.IntOnly(TB_Input_Y.Text())
        End If
    End Sub

    Public Property Return_X() As Double
        Get
            Return Utility.ToDbl(TB_Input_X.Text, 0)
        End Get
        Set(value As Double)
            TB_Input_X.Text() = Utility.ToStr(value, "")
        End Set
    End Property
    Public Property Return_Y() As Double
        Get
            Return Utility.ToDbl(TB_Input_Y.Text, 0)
        End Get
        Set(value As Double)
            TB_Input_Y.Text() = Utility.ToStr(value, "")
        End Set
    End Property
    Public Property Return_W() As Double
        Get
            Return Utility.ToDbl(TB_Input_W.Text, 0)
        End Get
        Set(value As Double)
            TB_Input_W.Text() = Utility.ToStr(value, "")
        End Set
    End Property
    Public Property Return_H() As Double
        Get
            Return Utility.ToDbl(TB_Input_H.Text, 0)
        End Get
        Set(value As Double)
            TB_Input_H.Text() = Utility.ToStr(value, "")
        End Set
    End Property


End Class
