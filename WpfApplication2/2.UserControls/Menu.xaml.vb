Imports MatrixUtil
Imports MatrixBasic
Imports Matrix

Public Class MenuX

    Private Sub Initialize() Handles Me.Initialized
        Menu_Button.Text() = "MENU"
        Menu_Button.Background() = Brushes.Transparent
        Menu_Button.Border(Brushes.Transparent, New Thickness(0))
        Menu_Button.HorizontalAlignment = HorizontalAlignment.Center
        Menu_Button.VerticalAlignment = VerticalAlignment.Center
    End Sub

    Public Property NormalFont() As Fonts
        Get
            Return Menu_Button.NormalFont()
        End Get
        Set(value As Fonts)
            Menu_Button.NormalFont() = value
        End Set
    End Property

    Public Property HoverFont() As Fonts
        Get
            Return Menu_Button.HoverFont()
        End Get
        Set(value As Fonts)
            Menu_Button.HoverFont() = value
        End Set
    End Property


    WithEvents Main_Menu As New MenuDialog
    Private Sub Click_Event() Handles Main_Grid.PreviewMouseLeftButtonDown
        Main_Menu.Close()

        Main_Menu = New MenuDialog()

        Main_Menu.ButtonInfo() = {"Dialogs|Dialogs", "Database|Database", "Scripts|Scripts", "Avatar|Avatar", "About|About"}
        Main_Menu.Load()

        Main_Menu.Placement = Primitives.PlacementMode.Absolute

        Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size, New Size(Main_Menu.Width, Main_Menu.Height))

        Main_Menu.Location() = center

        Main_Menu.Show()

    End Sub

    Private NodeEditor_Window As New DialogEditor
    Private DatabaseEditor_Window As New DatabaseEditor
    Private ScriptEditor_Window As New ScriptEditor
    Private AvatarEditor_Window As New AvatarEditor
    Private About_Window As New About

    Private Sub LeftClick_Events(ByVal button As GenerikButton) Handles Main_Menu.LeftClick

        If button.Name() = "Dialogs" Then
            NodeEditor_Window.Close()
            NodeEditor_Window = New DialogEditor

            Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size,
                                          New Size(NodeEditor_Window.Width, NodeEditor_Window.Height))

            NodeEditor_Window.Top() = center.Y
            NodeEditor_Window.Left() = center.X
            NodeEditor_Window.Show()

            Main_Menu.Close()
            Exit Sub
        End If

        If button.Name() = "Database" Then
            DatabaseEditor_Window.Close()
            DatabaseEditor_Window = New DatabaseEditor

            Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size,
                                          New Size(DatabaseEditor_Window.Width, DatabaseEditor_Window.Height))

            DatabaseEditor_Window.Top() = center.Y
            DatabaseEditor_Window.Left() = center.X
            DatabaseEditor_Window.Show()

            Main_Menu.Close()
            Exit Sub
        End If
        If button.Name() = "Scripts" Then
            ScriptEditor_Window.Close()
            ScriptEditor_Window = New ScriptEditor

            Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size,
                                          New Size(ScriptEditor_Window.Width, ScriptEditor_Window.Height))

            ScriptEditor_Window.Top() = center.Y
            ScriptEditor_Window.Left() = center.X
            ScriptEditor_Window.Show()

            Main_Menu.Close()
            Exit Sub
        End If
        If button.Name() = "Avatar" Then
            AvatarEditor_Window.Close()
            AvatarEditor_Window = New AvatarEditor

            Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size,
                                          New Size(AvatarEditor_Window.Width, AvatarEditor_Window.Height))

            AvatarEditor_Window.Top() = center.Y
            AvatarEditor_Window.Left() = center.X
            AvatarEditor_Window.Show()

            Main_Menu.Close()
            Exit Sub
        End If

        If button.Name() = "About" Then

            About_Window.Close()
            About_Window = New About

            Dim center = Local.FindCenter(Schema.Global_Postion, Schema.Global_Size,
                                          New Size(AvatarEditor_Window.Width, AvatarEditor_Window.Height))

            About_Window.Top() = center.Y
            About_Window.Left() = center.X
            About_Window.Show()

            Main_Menu.Close()
            Exit Sub
        End If
    End Sub




End Class
