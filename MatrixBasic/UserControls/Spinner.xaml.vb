Imports Matrix
Imports MatrixUtil

Public Class Spinner

    Private Sub Initialize() Handles Me.Initialized

        Button_Up.TextBuffer(0, 0, 6, 6)
        Button_Up.Border(Schema.Global_Border_Color, New Thickness(Schema.Global_Border_Size(), 0, 0, Schema.Global_Border_Size()))

        Button_Down.TextBuffer(0, 0, 6, 6)
        Button_Down.Border(Schema.Global_Border_Color, New Thickness(Schema.Global_Border_Size(), 0, 0, 0))

        Button_Select.TextBuffer(0, 0, 6, 6)
        Button_Select.Border(Schema.Global_Border_Color, New Thickness(Schema.Global_Border_Size(), 0, 0, 0))

        Schema.SetStyle(LB_Index)
        Border_Index.BorderBrush() = Schema.Global_Border_Color()
        Border_Index.BorderThickness() = New Thickness(0, 0, Schema.Global_Border_Size(), 0)

        Schema.SetStyle(Main_Border)
        Schema.SetStyle(Main_Label)

        Main_Grid.Background = Schema.Global_FontBackColor()

    End Sub

    Public Sub BorderOuter(ByVal bordercolor As Brush, ByVal thickness As Thickness)
        Main_Border.BorderBrush = bordercolor
        Main_Border.BorderThickness = thickness
    End Sub

    Public Sub Border(ByVal bordercolor As Brush, ByVal thick As Double)

        Main_Border.BorderBrush = bordercolor
        Main_Border.BorderThickness = New Thickness(thick)

        Border_Index.BorderBrush = bordercolor
        Border_Index.BorderThickness = New Thickness(0, 0, thick, 0)

        Button_Up.Border(bordercolor, New Thickness(thick, 0, 0, thick))
        Button_Down.Border(bordercolor, New Thickness(thick, 0, 0, 0))
        Button_Select.Border(bordercolor, New Thickness(thick, 0, 0, 0))

    End Sub

    Public Shadows Property Background() As Brush
        Get
            Return Main_Border.Background
        End Get
        Set(value As Brush)
            Main_Border.Background = value
            Main_Grid.Background = value
            Button_Up.Background() = value
            Button_Down.Background() = value
            Button_Select.Background() = value
            LB_Index.Background() = value
            Border_Index.Background() = value
        End Set
    End Property

    Public Property HoverFont() As Fonts
        Get
            Return Button_Up.HoverFont()
        End Get
        Set(value As Fonts)
            Button_Up.HoverFont = value
            Button_Down.HoverFont = value
            Button_Select.HoverFont = value
        End Set
    End Property

    Public Property NormalFont() As Fonts
        Get
            Return Button_Up.NormalFont()
        End Get
        Set(value As Fonts)
            Button_Up.NormalFont() = value
            Button_Down.NormalFont() = value
            Button_Select.NormalFont() = value

            Main_Label.FontFamily() = value.Family
            Main_Label.FontSize() = value.Size
            Main_Label.FontStretch() = value.Stretch
            Main_Label.FontStyle() = value.Style
            Main_Label.FontWeight() = value.Weight

            LB_Index.FontFamily() = value.Family
            LB_Index.FontSize() = value.Size
            LB_Index.FontStretch() = value.Stretch
            LB_Index.FontStyle() = value.Style
            LB_Index.FontWeight() = value.Weight
        End Set
    End Property

    Public Property Items() As New List(Of String)

    Public Function Contains(ByVal item As String, ByVal eGate As Gate) As Boolean
        For Each it In Items()
            If Utility.Search(it, item, True, eGate) = True Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Sub Add(ByVal item As String)
        Items.Add(item)
    End Sub

    Public Property IsCounter() As Boolean = False

    Public Property Steps() As Double = 1
    Private Property Center() As Double = -1
    Public Property Base() As Double
        'User if Counter = True
        Get
            Return Center()
        End Get
        Set(value As Double)
            Center() = value
            Main_Label.Content = value
        End Set
    End Property
    Public Property UpperLimit() As Double = 100
    Public Property LowerLimit() As Double = -100

    Public Event Spins()
    Public Event SelectedSpin()

    Public Property Text() As String
        Get
            Return Main_Label.Content
        End Get
        Set(value As String)
            If IsCounter() = False Then
                Dim index = Utility.IndexOf(Items, value, True, Gate.eEQUALS)
                If index = -1 Then index = 0
                Center() = index

                If ShowIndex() = True Then
                    LB_Index.Content() = Utility.ToStr(index, "0")
                End If
            End If

            Main_Label.Content = value
        End Set
    End Property

    Public WriteOnly Property SelectText(Optional ByVal eGate As Gate = Gate.eEQUALS) As String
        Set(value As String)
            For Each Item In Items
                If Utility.Search(Item, value, eGate) = True Then
                    Text() = Item
                    Exit For
                End If
            Next
        End Set
    End Property

    Public Property Index() As Integer
        'Used if Counter = False
        Get
            Return Center()
        End Get
        Set(value As Integer)
            Center() = value
            Main_Label.Content = Utility.GetIndex(Items, value, "")

            If ShowIndex() = True Then
                LB_Index.Content() = Utility.ToStr(value, "0")
            End If
        End Set
    End Property

    Public Sub Remove(ByVal entry As String, ByVal eGate As Gate)
        If IsCounter = False Then
            Items() = Utility.Remove(Items, entry, eGate)
        End If
    End Sub

    Public Sub RemoveAt(ByVal index As Integer)
        If IsCounter = False Then
            Items.RemoveAt(index)
        End If
    End Sub

    Public Sub RemoveCurrent()
        If IsCounter = False Then
            Items.RemoveAt(Center())
            Index() = 0
        End If
    End Sub

    Public Sub Clear()
        Items.Clear()
        Center() = -1
        Main_Label.Content = ""
        LB_Index.Content() = "0"
    End Sub

    Private Sub Spinner_Event(ByVal sender As ButtonX) Handles Button_Up.LeftClick, Button_Down.LeftClick

        If IsCounter() = True Then

            If sender Is Button_Down Then
                Center() += Steps
            ElseIf sender Is Button_Up Then
                Center() -= Steps
            End If

            If Center() <= LowerLimit Then Center() = LowerLimit
            If Center() >= UpperLimit Then Center() = UpperLimit

            Main_Label.Content = Center()
        Else

            If Items.Count() = 1 Or Items.Count() = 0 Then Exit Sub

            If sender Is Button_Down Then
                Center() += 1
            ElseIf sender Is Button_Up Then
                Center() -= 1
            End If

            If Center() <= 0 Then Center() = 0
            If Center() >= Items.Count - 1 Then Center() = Items.Count - 1

            Main_Label.Content = Utility.GetIndex(Items, Utility.ToInt(Center()), "")

            If ShowIndex() = True Then
                LB_Index.Content() = Utility.ToStr(Center(), "0")
            End If
        End If

        RaiseEvent Spins()
    End Sub

    Private Sub Spinner_Select() Handles Button_Select.LeftClick
        RaiseEvent SelectedSpin()
    End Sub

    Public Property ShowSpin() As Boolean
        Get
            If Button_Select.Visibility = Visibility.Visible Then
                Return True
            Else
                Return False
            End If
        End Get
        Set(value As Boolean)
            If value = True Then
                Button_Select.Visibility = Visibility.Visible
            Else
                Button_Select.Visibility = Visibility.Collapsed
            End If
        End Set
    End Property

    Public Property ShowIndex() As Boolean
        Get
            If Border_Index.Visibility = Visibility.Visible Then
                Return True
            Else
                Return False
            End If
        End Get
        Set(value As Boolean)
            If value = True Then
                Border_Index.Visibility = Visibility.Visible
            Else
                Border_Index.Visibility = Visibility.Collapsed
            End If
        End Set
    End Property
End Class
