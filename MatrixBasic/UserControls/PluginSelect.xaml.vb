Imports Matrix
Imports MatrixUtil

Public Class PluginSelect


    Private Sub Initialize() Handles Me.Initialized
        Border_Main.BorderBrush() = Schema.Global_Border_Color()
        Plugin_Window.Background() = Schema.Global_FontBackColor()
    End Sub

    Public Property Fonts() As New Fonts

    Public Sub Clear()
        PluginList().Clear()
        CurrentCheckedList().Clear()
        List_Grid.Children.Clear()
    End Sub

    Public Property PluginsToCheck() As New List(Of String)
    Public Sub Load()

        List_Grid.Children.Clear()
        Dim newCol_1 As New ColumnDefinition
        newCol_1.Width = GridLength.Auto
        List_Grid.ColumnDefinitions.Add(newCol_1)

        Dim newCol_2 As New ColumnDefinition
        newCol_2.Width = GridLength.Auto
        List_Grid.ColumnDefinitions.Add(newCol_2)

        For i = 0 To PluginList.Count() - 1
            Dim newRow As New RowDefinition
            newRow.Height = GridLength.Auto
            List_Grid.RowDefinitions.Add(newRow)

            Dim newCheck As New CheckBox
            newCheck.SetValue(Grid.RowProperty, i)
            newCheck.SetValue(Grid.ColumnProperty, 0)
            newCheck.HorizontalAlignment = HorizontalAlignment.Center
            newCheck.VerticalAlignment = VerticalAlignment.Center

            If Utility.Search(PluginList(i), PluginsToCheck(), Gate.eEQUALS) = True Then
                newCheck.IsChecked = True
            Else
                newCheck.IsChecked = False
            End If

            AddHandler newCheck.Checked, AddressOf Check

            Dim newLable As New Label
            newLable.Content = PluginList(i)
            newLable.SetValue(Grid.RowProperty, i)
            newLable.SetValue(Grid.ColumnProperty, 1)


            newLable.FontFamily() = Fonts.Family()
            newLable.FontSize() = Fonts.Size()
            newLable.FontStretch() = Fonts.Stretch()
            newLable.FontStyle() = Fonts.Style()
            newLable.FontWeight() = Fonts.Weight()
            newLable.Foreground() = Fonts.Color()
            newLable.Background() = Brushes.Transparent()


            CurrentCheckedList.Add(newCheck)

            List_Grid.Children.Add(newCheck)
            List_Grid.Children.Add(newLable)

        Next

    End Sub

    Public Property PluginList() As New List(Of String)
    Private Property CurrentCheckedList() As New List(Of CheckBox)
    Public Function CheckedList()
        Dim newChecks As New List(Of String)

        For i = 0 To CurrentCheckedList.Count - 1
            If CurrentCheckedList(i).IsChecked() = True Then newChecks.Add(PluginList(i))
        Next

        Return newChecks
    End Function


    Public Event IsChecked()

    Private Sub Check()
        RaiseEvent IsChecked()
    End Sub

End Class
