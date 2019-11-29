Imports Matrix
Imports MatrixUtil

Public Class Input3D

    Private Sub Initialize() Handles Me.Initialized

        Schema.SetStyle(LB_Input_X)
        Schema.SetStyle(LB_Input_Y)
        Schema.SetStyle(LB_Input_Z)

        Schema.SetStyle(Main_Border)

        Schema.SetStyle(TB_Input_X)
        Schema.SetStyle(TB_Input_Y)
        Schema.SetStyle(TB_Input_Z)
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

    Private Sub TB_Input_Z_TextChange() Handles TB_Input_Z.TextChanged
        If Utility.IsInt(TB_Input_Y.Text()) = False Then
            TB_Input_Z.Text() = Utility.IntOnly(TB_Input_Z.Text())
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
    Public Property Return_Z() As Double
        Get
            Return Utility.ToDbl(TB_Input_Z.Text, 0)
        End Get
        Set(value As Double)
            TB_Input_Z.Text() = Utility.ToStr(value, "")
        End Set
    End Property


End Class
