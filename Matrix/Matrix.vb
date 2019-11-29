Option Compare Binary
Imports Matrix.NeuralNetworking.ClassNeuralNetwork

'*****************
'Utilities Classes
'*****************

Public Class Utility
    '***********************************************
    'This is a collection of functions that give the user easy access to a variety of resources.
    '***********************************************

    Private Const MAX_TRIM As Integer = 500
    Private Const TIME_DELIM As String = Chr(46) '.
    Public Const SPACE As Char = Chr(32)
    Public Const CARRIAGE As String = vbCrLf
    Public Const QUOTE As Char = Chr(34)
    Public Const ASTERISK As Char = Chr(43)
    Public Const SKIP As String = ASTERISK
    Public Const STOPPED As String = ASTERISK & ASTERISK
    Public Const FWD_SLASH As String = "/"
    Public Const DEF_BIRTH As String = "July 1, 2009"


#Region "Find Arguments"


    Public Shared Function GetLeftOfArg(ByVal text As String, ByVal arg As String, ByVal eGate As Gate) As String
        'This Gets the segment from the start of the text to a designated command end 
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        Dim findLeft = InStr(text, arg, eGate)
        If findLeft = 0 Then Return ""

        If findLeft - 1 > 0 Then
            Return Mid(text, 1, findLeft - 1)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetLeftOfArg(ByVal text As String, ByVal arg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        'This Gets the segment from the start of the text to a designated command end 
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        If reverse = True Then
            text = ReverseString(text)
            arg = ReverseString(arg)
        End If

        Dim findLeft = InStr(text, arg, eGate)
        If findLeft = 0 Then Return ""

        If findLeft - 1 > 0 Then
            If reverse = True Then
                Return ReverseString(Mid(text, 1, findLeft - 1))
            Else
                Return Mid(text, 1, findLeft - 1)
            End If
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetLeftOfArg(ByVal intStart As Integer, ByVal text As String, ByVal arg As String, ByVal eGate As Gate) As String
        'This Gets the segment from the start of the text to a designated command end 
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        Dim findLeft = InStr(intStart, text, arg, eGate)
        If findLeft = 0 Then Return ""

        If findLeft - 1 > 0 Then
            Return Mid(text, 1, findLeft - 1)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetLeftOfArg(ByVal intStart As Integer, ByVal text As String, ByVal arg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        'This Gets the segment from the start of the text to a designated command end 
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        If reverse = True Then
            text = ReverseString(text)
            arg = ReverseString(arg)
        End If

        Dim findLeft = InStr(intStart, text, arg, eGate)
        If findLeft = 0 Then Return ""

        If findLeft - 1 > 0 Then
            If reverse = True Then
                Return ReverseString(Mid(text, 1, findLeft - 1))
            Else
                Return Mid(text, 1, findLeft - 1)
            End If
        Else
            Return ""
        End If
    End Function


    Public Shared Function GetRightOfArg(ByVal text As String, ByVal arg As String, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        Dim findLeft = InStr(text, arg, eGate)
        If findLeft = 0 Then Return ""

        Dim CMDLength = arg.Length()
        If CMDLength = 0 Then Return ""

        If findLeft + CMDLength > 0 And text.Length() - findLeft - CMDLength > 0 Then
            Return Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetRightOfArg(ByVal text As String, ByVal arg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        If reverse = True Then
            text = ReverseString(text)
            arg = ReverseString(arg)
        End If

        Dim findLeft = InStr(text, arg, eGate)
        If findLeft = 0 Then Return ""
        Dim CMDLength = arg.Length()
        If CMDLength = 0 Then Return ""

        If findLeft + CMDLength > 0 And text.Length() - findLeft - CMDLength > 0 Then
            If reverse = True Then
                Return ReverseString(Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1))
            Else
                Return Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1)
            End If
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetRightOfArg(ByVal intStart As Integer, ByVal text As String, ByVal arg As String, ByVal eGate As Gate) As String
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        'This Gets the segment from the designated command end to the end of text
        Dim findLeft = InStr(intStart, text, arg, eGate)
        If findLeft = 0 Then Return ""
        Dim CMDLength = arg.Length()
        If CMDLength = 0 Then Return ""

        If findLeft + CMDLength > 0 And text.Length() - findLeft - CMDLength > 0 Then
            Return Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetRightOfArg(ByVal intStart As Integer, ByVal text As String, ByVal arg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        IsNull(text)
        IsNull(arg)

        If IsEmpty(arg) = True Then Return ""
        If IsEmpty(text) = True Then Return ""

        If reverse = True Then
            text = ReverseString(text)
            arg = ReverseString(arg)
        End If

        'This Gets the segment from the designated command end to the end of text
        Dim findLeft = InStr(intStart, text, arg, eGate)
        If findLeft = 0 Then Return ""
        Dim CMDLength = arg.Length()
        If CMDLength = 0 Then Return ""

        If findLeft + CMDLength > 0 And text.Length() - findLeft - CMDLength > 0 Then
            If reverse = True Then
                Return ReverseString(Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1))
            Else
                Return Mid(text, findLeft + CMDLength, text.Length() - findLeft - CMDLength + 1)
            End If
        Else
            Return ""
        End If
    End Function


    Public Shared Function GetMidArg(ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text

        IsNull(text)
        IsNull(startArg)
        IsNull(endArg)

        If IsEmpty(text) = True Then Return ""

        If IsEmpty(endArg) = False And IsEmpty(startArg) = True Then
            Return GetLeftOfArg(text, endArg, eGate)
        ElseIf IsEmpty(endArg) = True Then
            Return GetRightOfArg(text, startArg, eGate)
        Else

            Dim findLeft = InStr(text, startArg, Gate.eEQUALS)
            If findLeft = 0 Then Return ""
            Dim findRight = InStr(findLeft + startArg.Length(), text, endArg, eGate)
            If findRight = 0 Then Return ""
            Dim CMDLength = startArg.Length()
            If CMDLength = 0 Then Return ""

            If findLeft + CMDLength > 0 And findRight - findLeft - CMDLength > 0 Then
                Return Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength)
            Else
                Return ""
            End If

        End If

        Return ""

    End Function
    Public Shared Function GetMidArg(ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text

        IsNull(text)
        IsNull(startArg)
        IsNull(endArg)

        If IsEmpty(text) = True Then Return ""

        If IsEmpty(endArg) = False And IsEmpty(startArg) = True Then
            Return GetLeftOfArg(text, endArg, reverse, eGate)
        ElseIf IsEmpty(endArg) = True Then
            Return GetRightOfArg(text, startArg, reverse, eGate)
        Else

            If reverse = True Then
                text = ReverseString(text)
                startArg = ReverseString(startArg)
                endArg = ReverseString(endArg)
            End If

            Dim findLeft = InStr(text, startArg, eGate)
            If findLeft = 0 Then Return ""
            Dim findRight = InStr(findLeft + startArg.Length(), text, endArg, eGate)
            If findRight = 0 Then Return ""
            Dim CMDLength = startArg.Length()
            If CMDLength = 0 Then Return ""

            If findLeft + CMDLength > 0 And findRight - findLeft - CMDLength > 0 Then
                If reverse = True Then
                    Return ReverseString(Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength))
                Else
                    Return Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength)
                End If
            Else
                Return ""
            End If

        End If

        Return ""
    End Function
    Public Shared Function GetMidArg(ByVal intStart As Integer, ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text

        IsNull(text)
        IsNull(startArg)
        IsNull(endArg)

        If IsEmpty(text) = True Then Return ""

        If IsEmpty(endArg) = False And IsEmpty(startArg) = True Then
            Return GetLeftOfArg(intStart, text, endArg, eGate)

        ElseIf IsEmpty(endArg) = True Then
            Return GetRightOfArg(intStart, text, startArg, eGate)

        Else

            Dim findLeft = InStr(intStart, text, startArg, eGate)
            If findLeft = 0 Then Return ""
            Dim findRight = InStr(findLeft + startArg.Length(), text, endArg, eGate)
            If findRight = 0 Then Return ""
            Dim CMDLength = startArg.Length()
            If CMDLength = 0 Then Return ""

            If findLeft + CMDLength > 0 And findRight - findLeft - CMDLength > 0 Then
                Return Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength)
            Else
                Return ""
            End If

        End If

        Return ""
    End Function
    Public Shared Function GetMidArg(ByVal intStart As Integer, ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal reverse As Boolean, ByVal eGate As Gate) As String
        'This Gets the segment from the designated command end to the end of text

        IsNull(text)
        IsNull(startArg)
        IsNull(endArg)

        If IsEmpty(text) = True Then Return ""

        If IsEmpty(endArg) = False And IsEmpty(startArg) = True Then
            Return GetLeftOfArg(intStart, text, endArg, reverse, eGate)

        ElseIf IsEmpty(endArg) = False Then
            Return GetRightOfArg(intStart, text, startArg, reverse, eGate)

        Else

            If reverse = True Then
                text = ReverseString(text)
                startArg = ReverseString(startArg)
                endArg = ReverseString(endArg)
            End If

            Dim findLeft = InStr(intStart, text, startArg, eGate)
            If findLeft = 0 Then Return ""
            Dim findRight = InStr(findLeft + startArg.Length(), text, endArg, eGate)
            If findRight = 0 Then Return ""
            Dim CMDLength = startArg.Length()
            If CMDLength = 0 Then Return ""

            If findLeft + CMDLength > 0 And findRight - findLeft - CMDLength > 0 Then
                If reverse = True Then
                    Return ReverseString(Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength))
                Else
                    Return Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength)
                End If
            Else
                Return ""
            End If

        End If

        Return ""
    End Function


    Public Shared Function GetArgArray(ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal eGate As Gate) As List(Of String)

        Dim Col As New List(Of String)
        IsNull(text)
        IsNull(startArg)
        IsNull(endArg)

        If IsEmpty(text) = True Then Return New List(Of String)

        If IsEmpty(startArg) = False And IsEmpty(endArg) = False Then

            Dim newStart = 1
            Dim CMDLength = startArg.Length()

            Do
                'Finds the arguement
                Dim findLeft = InStr(newStart, text, startArg, eGate)
                If findLeft = 0 Then Exit Do

                Dim findRight = InStr(findLeft + startArg.Length(), text, endArg, eGate)
                If findRight = 0 Then Exit Do

                If findLeft + CMDLength > 0 And findRight - findLeft - CMDLength > 0 Then
                    Col.Add(Mid(text, findLeft + CMDLength, findRight - findLeft - CMDLength))
                End If

                'Resets the place of start for the next search
                newStart = findRight + endArg.Length()
            Loop

        End If

        Return (Col)

    End Function

#End Region


#Region "Time"

    Public Shared Function TodaysDate() As String
        Return Date.Now.ToLongDateString()
    End Function

    Public Shared Function Year() As String
        Return Date.Now.Year().ToString()
    End Function

    Public Shared Function Month() As String
        Return Date.Now.Month().ToString()
    End Function

    Public Shared Function Day() As String
        Return Date.Now.Day().ToString()
    End Function

    Public Shared Function Hour() As String
        Return Date.Now.Hour().ToString()
    End Function

    Public Shared Function Minute() As String
        Return Date.Now.Minute().ToString()
    End Function

    Public Shared Function Second() As String
        Return Date.Now.Second().ToString() & "." & Date.Now.Millisecond().ToString()
    End Function

    Public Shared Function TimeStamp(Optional isLabled As Boolean = False) As String
        '2013~10~24~5~6~1~100
        Dim Now = Date.Now()
        If isLabled = False Then
            Return Concat({ToStr(Now.Year), ToStr(Now.Month), ToStr(Now.Day), ToStr(Now.Hour), ToStr(Now.Minute), ToStr(Now.Second), ToStr(Now.Millisecond)}, TIME_DELIM)
        Else
            Return Concat({"Y" & ToStr(Now.Year), "M" & ToStr(Now.Month), "D" & ToStr(Now.Day), "H" & ToStr(Now.Hour), "M" & ToStr(Now.Minute), "S" & ToStr(Now.Second), "M" & ToStr(Now.Millisecond)}, TIME_DELIM)
        End If
    End Function

    Public Shared Function TodaysLongTime() As String
        Return Date.Now.ToLongTimeString()
    End Function
    Public Shared Function TodaysShortTime() As String
        Return Date.Now.ToShortTimeString()
    End Function

    Public Shared Function BirthdayAge() As String
        'return birday in years
        Return ToStr(DateDiff(DateInterval.Year, New Date(2009, 7, 1), Date.Now()))
    End Function
    Public Shared Function Birthday() As String
        Return DEF_BIRTH
    End Function

    Public Shared Function TimeStampDiff(ByVal TimeStamp1 As String, ByVal TimeStamp2 As String) As String
        If IsTimeStamp(TimeStamp1) = False Or IsTimeStamp(TimeStamp2) = False Then
            Return False
        End If

        Dim TimeArray1 = ToList(TimeStamp1, TIME_DELIM)
        Dim Date1 As New Date(ToInt(GetIndex(TimeArray1, 0, "0")), ToInt(GetIndex(TimeArray1, 1, "0")), ToInt(GetIndex(TimeArray1, 2, "0")),
                              ToInt(GetIndex(TimeArray1, 3, "0")), ToInt(GetIndex(TimeArray1, 4, "0")), ToInt(GetIndex(TimeArray1, 5, "0")), ToInt(GetIndex(TimeArray1, 6, "0")))

        Dim TimeArray2 = ToList(TimeStamp2, TIME_DELIM)
        Dim Date2 As New Date(ToInt(GetIndex(TimeArray2, 0, "0")), ToInt(GetIndex(TimeArray2, 1, "0")), ToInt(GetIndex(TimeArray2, 2, "0")),
                              ToInt(GetIndex(TimeArray2, 3, "0")), ToInt(GetIndex(TimeArray2, 4, "0")), ToInt(GetIndex(TimeArray2, 5, "0")), ToInt(GetIndex(TimeArray2, 6, "0")))

        Return DateDiff(DateInterval.Second, Date1, Date2)
    End Function

    Public Shared Function TimeStampCompare(ByVal StandardTimeStamp As String, ByVal ComparedTimeStamp As String) As Boolean
        If IsTimeStamp(StandardTimeStamp) = False Or IsTimeStamp(ComparedTimeStamp) = False Then
            Return False
        End If

        Dim TimeArray1 = ToList(StandardTimeStamp, TIME_DELIM)
        Dim Date1 As New Date(ToInt(GetIndex(TimeArray1, 0, "0")), ToInt(GetIndex(TimeArray1, 1, "0")), ToInt(GetIndex(TimeArray1, 2, "0")),
                              ToInt(GetIndex(TimeArray1, 3, "0")), ToInt(GetIndex(TimeArray1, 4, "0")), ToInt(GetIndex(TimeArray1, 5, "0")), ToInt(GetIndex(TimeArray1, 6, "0")))

        Dim TimeArray2 = ToList(ComparedTimeStamp, TIME_DELIM)
        Dim Date2 As New Date(ToInt(GetIndex(TimeArray2, 0, "0")), ToInt(GetIndex(TimeArray2, 1, "0")), ToInt(GetIndex(TimeArray2, 2, "0")),
                              ToInt(GetIndex(TimeArray2, 3, "0")), ToInt(GetIndex(TimeArray2, 4, "0")), ToInt(GetIndex(TimeArray2, 5, "0")), ToInt(GetIndex(TimeArray2, 6, "0")))

        Return Date.Compare(Date1, Date2)
    End Function

    Public Shared Function GetTimes(ByVal StartTimeStamp As String, ByVal EndTimeStamp As String, ByVal TimeStampArray As List(Of String)) As List(Of String)
        'This Returns all the TimeStamps that are within the deffined parimeters from within the TimeStampArray
        Dim allTimes As New List(Of String)

        If IsTimeStamp(StartTimeStamp) = False Or IsTimeStamp(EndTimeStamp) = False Then
            Return allTimes
        End If

        For Each TimeStamp1 In TimeStampArray
            If IsEmpty(StartTimeStamp) = False And IsEmpty(EndTimeStamp) = False Then
                If TimeStampCompare(StartTimeStamp, TimeStamp1) = True And TimeStampCompare(EndTimeStamp, TimeStamp1) = False Then
                    allTimes.Add(TimeStamp1)
                End If
                If TimeStamp1 = StartTimeStamp And TimeStamp1 = EndTimeStamp Then
                    allTimes.Add(TimeStamp1)
                End If
            End If
            If IsEmpty(StartTimeStamp) = False And IsEmpty(EndTimeStamp) = True Then
                If TimeStampCompare(StartTimeStamp, TimeStamp1) = True Then
                    allTimes.Add(TimeStamp1)
                End If
                If TimeStamp1 = StartTimeStamp Then
                    allTimes.Add(TimeStamp1)
                End If
            End If
            If IsEmpty(StartTimeStamp) = True And IsEmpty(EndTimeStamp) = False Then
                If TimeStampCompare(EndTimeStamp, TimeStamp1) = False Then
                    allTimes.Add(TimeStamp1)
                End If
                If TimeStamp1 = StartTimeStamp And TimeStamp1 = EndTimeStamp Then
                    allTimes.Add(TimeStamp1)
                End If
            End If

        Next

        Return allTimes
    End Function

    Public Shared Function IsTimeStamp(ByVal strTimeStamp As String) As Boolean

        Dim TimeArray1 = ToList(strTimeStamp, TIME_DELIM)

        If TimeArray1.Count() <> 7 Then Return False

        For Each Entry1 In TimeArray1
            If IsNumeric(Entry1) = False Then Return False
        Next

        Dim Year1 = GetIndex(TimeArray1, 0, "0")
        Dim Month1 = GetIndex(TimeArray1, 1, "0")
        Dim Day1 = GetIndex(TimeArray1, 2, "0")
        Dim Hour1 = GetIndex(TimeArray1, 3, "0")
        Dim Minute1 = GetIndex(TimeArray1, 4, "0")
        Dim Second1 = GetIndex(TimeArray1, 5, "0")
        Dim Millisecond1 = GetIndex(TimeArray1, 6, "0")

        If Year1.Length() <> 4 Then Return False 'Year

        If Month1 < 1 And Month1 > 12 Then Return False 'Month

        'DayOfMonth
        If Month1 = 4 Or Month1 = 6 Or Month1 = 9 Or Month1 = 11 Then
            If Day1 < 1 And Day1 > 30 Then Return False
        Else
            If Month1 = 2 Then
                If Date.IsLeapYear(Year1) = True Then
                    If Day1 < 1 And Day1 > 29 Then Return False
                Else
                    If Day1 < 1 And Day1 > 28 Then Return False
                End If
            Else
                If Day1 < 1 And Day1 > 31 Then Return False
            End If
        End If

        If Hour1 < 1 And Hour1 > 24 Then Return False 'Hour
        If Minute1 < 0 And Minute1 > 59 Then Return False 'Minute
        If Second1 < 0 And Second1 > 59 Then Return False 'Second
        If Millisecond1 < 0 And Millisecond1 > 999 Then Return False 'Millisecond

        Return True

    End Function

    Public Shared Sub Sleep(ByVal milSeconds As Integer)
        IsNull(milSeconds)
        System.Threading.Thread.Sleep(milSeconds)
    End Sub

#End Region


#Region "Strings"

    Public Shared Function Concat(ByVal strings As String()) As String
        Return Join(strings, "")
    End Function
    Public Shared Function Concat(ByVal strings As String(), ByVal hasSpace As Boolean) As String
        If hasSpace = True Then
            Return Join(strings, SPACE)
        Else
            Return Join(strings, "")
        End If
    End Function
    Public Shared Function Concat(ByVal strings As String(), ByVal deliminator As String) As String
        Return Join(strings, deliminator)
    End Function
    Public Shared Function Concat(ByVal string1 As String(), ByVal string2 As String()) As String()
        Dim count = 0
        Dim newArray(string1.Length + string2.Length) As String

        For Each str1 In string1
            newArray(count) = str1
            count += 1
        Next
        For Each str1 In string2
            newArray(count) = str1
            count += 1
        Next

        Return newArray
    End Function


    Public Shared Function InStr(ByVal text As String, ByVal find As String) As Integer
        IsNull(text)
        IsNull(find)

        Return text.IndexOf(find, StringComparison.OrdinalIgnoreCase) + 1
    End Function
    Public Shared Function InStr(ByVal intStart As Integer, ByVal text As String, ByVal find As String) As Integer
        IsNull(text)
        IsNull(find)

        Return text.IndexOf(find, intStart, StringComparison.OrdinalIgnoreCase) + 1
    End Function
    Public Shared Function InStr(ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean) As Integer
        IsNull(text)
        IsNull(find)

        If ignoreCase = True Then
            Return text.IndexOf(find, StringComparison.OrdinalIgnoreCase) + 1
        Else
            Return text.IndexOf(find, StringComparison.Ordinal) + 1
        End If
    End Function
    Public Shared Function InStr(ByVal intStart As Integer, ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean) As Integer
        IsNull(text)
        IsNull(find)

        If ignoreCase = True Then
            Return text.IndexOf(find, intStart, StringComparison.OrdinalIgnoreCase) + 1
        Else
            Return text.IndexOf(find, intStart, StringComparison.Ordinal) + 1
        End If
    End Function
    Public Shared Function InStr(ByVal text As String, ByVal find As String, ByVal eGate As Gate) As Integer
        IsNull(text)
        IsNull(find)

        Dim CmdCount = text.Length()
        Dim StartCount = find.Length()

        For i = 1 To CmdCount
            If i + StartCount - 1 <= CmdCount Then

                Dim line = Mid(text, i, StartCount)

                If Search(line, find, eGate) = True Then
                    Return i
                End If

            End If
        Next

        Return 0
    End Function
    Public Shared Function InStr(ByVal intStart As Integer, ByVal text As String, ByVal find As String, ByVal eGate As Gate) As Integer
        IsNull(text)
        IsNull(find)
        IntR(intStart, 1)

        Dim CmdCount = text.Length()
        Dim StartCount = find.Length()

        If intStart >= CmdCount Then
            Return 0
        End If

        For i = intStart To CmdCount
            If i + StartCount - 1 <= CmdCount Then

                Dim line = Mid(text, i, StartCount)

                If Search(line, find, eGate) = True Then
                    Return i
                End If

            End If
        Next

        Return 0
    End Function


    Public Shared Function FindFirstLetter(ByVal text As String) As String
        IsNull(text)

        For Each letter In text.ToCharArray()
            If IsAlpha(letter) = True Then
                Return letter
            End If
        Next

        Return ""
    End Function
    Public Shared Function FindFirstLetter(ByVal text As String, ByVal intReach As Integer) As String
        IsNull(text)
        IsNull(intReach)
        Dim cnt = 0

        For Each letter In text.ToCharArray()
            If IsAlpha(letter) = True Then
                Return letter
            End If

            If cnt >= intReach Then Exit For
            cnt += 1
        Next

        Return ""
    End Function


    Public Shared Function Replace(ByVal text As String, ByVal find As String, ByVal substitute As String) As String
        IsNull(text)
        IsNull(find)
        IsNull(substitute)
        Return Defaults(Microsoft.VisualBasic.Replace(text, find, substitute, 1, -1, CompareMethod.Text), "")
    End Function

    Public Shared Function ReplaceRnd(ByVal text As String, ByVal find As String, ByVal substituteArray As List(Of String)) As String
        'Remember to arrange the searchArray from the least to the greatest (length) strings
        Return Replace(text, find, RandArray(substituteArray, ""))
    End Function
    Public Shared Function ReplaceRnd(ByVal text As String, ByVal find As String, ByVal substituteArray As String()) As String
        'Remember to arrange the searchArray from the least to the greatest (length) strings
        Return Replace(text, find, RandArray(substituteArray, ""))
    End Function

    Public Shared Function Replace(ByVal text As String, ByVal searchArray As String(), ByVal substitute As String) As String
        'Remember to arrange the searchArray from the least to the greatest (length) strings

        For Each word In searchArray
            text = Replace(text, word, substitute)
        Next

        Return text
    End Function
    Public Shared Function Replace(ByVal text As String, ByVal searchArray As List(Of String), ByVal substitute As String) As String
        'Remember to arrange the searchArray from the least to the greatest (length) strings

        For Each word In searchArray
            text = Replace(text, word, substitute)
        Next

        Return text
    End Function

    Public Shared Function Replace(ByVal Items As List(Of String), ByVal find As String, ByVal substitute As String) As List(Of String)
        Dim newList As New List(Of String)
        For Each Item In Items
            newList.Add(Replace(Item, find, substitute))
        Next

        Return newList
    End Function
    Public Shared Function Replace(ByVal Items As String(), ByVal find As String, ByVal substitute As String) As String()
        Dim newList(Items.Length) As String
        For i = 0 To Items.Length - 1
            newList(i) = Replace(Items(i), find, substitute)
        Next

        Return newList
    End Function

    Public Shared Function Replace(ByVal Items As List(Of String), ByVal find As String, ByVal substitute As String, ByVal eGate As Gate) As List(Of String)
        Dim newList As New List(Of String)
        For Each Item In Items
            If Search(Item, find, eGate) = True Then
                Item = substitute
            End If

            newList.Add(Item)
        Next

        Return newList
    End Function
    Public Shared Function Replace(ByVal Items As String(), ByVal find As String, ByVal substitute As String, ByVal eGate As Gate) As String()
        Dim newList(Items.Length) As String
        For i = 0 To Items.Length - 1

            If Search(Items(i), find, eGate) = True Then
                newList(i) = substitute
            Else
                newList(i) = Items(i)
            End If
        Next

        Return newList
    End Function

    Public Shared Function ReplaceByArgs(ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal substitute As String, ByVal eGate As Gate) As String

        Dim args = GetArgArray(text, startArg, endArg, eGate)

        For Each arg In args
            text = Replace(text, startArg & arg & endArg, substitute)
        Next

        Return text
    End Function
    Public Shared Function ReplaceByArgs(ByVal text As List(Of String), ByVal startArg As String, ByVal endArg As String, ByVal substitute As String, ByVal eGate As Gate) As List(Of String)
        Dim newText As New List(Of String)

        For Each line In text
            line = ReplaceByArgs(line, startArg, endArg, substitute, eGate)
            newText.Add(line)
        Next


        Return newText
    End Function


    Public Shared Function IsAtIndex(ByVal text As String, ByVal Index As Integer, ByVal find As String) As Boolean
        IsNull(text)
        IsNull(find)
        IntR(Index, 1)

        If Search(Mid(text, Index, find.Length()), find, Gate.eEQUALS) = True Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function FirstCompare(ByVal text As String, ByVal Compare As String, ByVal ignoreCase As Boolean) As Boolean
        IsNull(text)
        IsNull(Compare)

        If ignoreCase = True Then
            If text.Length() > Compare.Length() = True Then
                Return text.StartsWith(Compare, StringComparison.OrdinalIgnoreCase)
            Else
                Return Compare.StartsWith(text, StringComparison.OrdinalIgnoreCase)
            End If
        Else
            If text.Length() > Compare.Length() = True Then
                Return text.StartsWith(Compare, StringComparison.Ordinal)
            Else
                Return Compare.StartsWith(text, StringComparison.Ordinal)
            End If
        End If

    End Function
    Public Shared Function LastCompare(ByVal text As String, ByVal Compare As String, ByVal ignoreCase As Boolean) As Boolean
        IsNull(text)
        IsNull(Compare)

        If ignoreCase = True Then
            If text.Length() > Compare.Length() = True Then
                Return text.EndsWith(Compare, StringComparison.OrdinalIgnoreCase)
            Else
                Return Compare.EndsWith(text, StringComparison.OrdinalIgnoreCase)
            End If
        Else
            If text.Length() > Compare.Length() = True Then
                Return text.EndsWith(Compare, StringComparison.Ordinal)
            Else
                Return Compare.EndsWith(text, StringComparison.Ordinal)
            End If
        End If

    End Function


    Public Shared Function Remove(ByVal text As String, ByVal removeArray As String()) As String
        Return Replace(text, removeArray, "")
    End Function
    Public Shared Function Remove(ByVal text As String, ByVal removeArray As List(Of String)) As String
        Return Replace(text, removeArray, "")
    End Function

    Public Shared Function RemoveIn(ByVal Items As List(Of String), ByVal find As String) As List(Of String)
        Return Replace(Items, find, "")
    End Function
    Public Shared Function RemoveIn(ByVal Items As String(), ByVal find As String) As String()
        Return Replace(Items, find, "")
    End Function

    Public Shared Function RemoveByArgs(ByVal text As String, ByVal startArg As String, ByVal endArg As String, ByVal eGate As Gate) As String

        Dim args = GetArgArray(text, startArg, endArg, eGate)

        For Each arg In args
            text = Remove(text, startArg & arg & endArg)
        Next

        Return text
    End Function
    Public Shared Function RemoveByArgs(ByVal text As List(Of String), ByVal startArg As String, ByVal endArg As String, ByVal eGate As Gate) As List(Of String)
        Dim newText As New List(Of String)

        For Each line In text
            line = RemoveByArgs(line, startArg, endArg, eGate)
            newText.Add(line)
        Next


        Return newText
    End Function


    Public Shared Function Remove(ByVal text As String, ByVal toRemove As String) As String
        Return Replace(text, toRemove, "")
    End Function
    Public Shared Function RemoveDuplicateChar(ByVal text As String) As String
        Dim check As String = text

        For i = 0 To text.Length() - 1
            check = Remove(check, text(i))
            check = check + text(i)
        Next

        Return check
    End Function


    Public Shared Function GetWord(ByVal text As String, ByVal Index As Integer, Optional ByVal Deliminator As String = " ") As String
        Dim newList As New List(Of String)
        Dim Result = ""
        IsNull(Index)
        text = Trim(text)
        IsNull(Deliminator)

        'Removes Dublicate Deliminators
        text = SingleSpace(text, Deliminator)

        If Search(text, Deliminator) = True Then
            newList = ToList(text, Deliminator)

            If Index < newList.Count Then Result = GetIndex(newList, Index, "")
        End If

        Return Result
    End Function
    Public Shared Function GetWords(ByVal text As String, ByVal indexStart As Integer, ByVal Count As Integer, Optional ByVal Deliminator As String = " ") As String
        Dim newList As New List(Of String)
        Dim Result = ""
        text = Trim(text)
        IsNull(Deliminator)

        'Removes Dublicate Deliminators
        text = SingleSpace(text, Deliminator)

        If Search(text, Deliminator) = True Then
            newList = ToList(text, Deliminator)

            For i = 0 To newList.Count - 1
                If i >= indexStart And i < Count + indexStart Then
                    If indexStart < newList.Count Then Result = Result + GetIndex(newList, i, "") + Deliminator
                End If
            Next

        End If

        Return Trim(Result)
    End Function

    Public Shared Function GetWordRange(ByVal text As String, ByVal start As Integer, ByVal count As Integer, Optional ByVal deliminator As String = " ") As List(Of String)
        IsNull(text)
        IsNull(deliminator)
        text = Trim(text)

        'Removes Dublicate Deliminators
        text = SingleSpace(text, deliminator)
        Dim newList = ToList(text, deliminator)

        Return GetRange(newList, start, count)
    End Function

    Public Shared Function CharCount(ByVal strToCheck As String, ByVal strToCount As String) As Integer
        Dim Result = 0
        If strToCheck > "" Then
            Result = strToCheck.Length() - Remove(strToCheck, strToCount).Length()
        End If
        Return Result
    End Function

    Public Shared Function WordCount(ByVal text As String) As Integer
        text = SingleSpace(text)
        Return Split(text, SPACE).Length()
    End Function

    Public Shared Function WordList(ByVal text As String) As List(Of String)
        text = SingleSpace(text)
        Return ToList(text, SPACE)
    End Function

    Public Shared Function WordArray(ByVal text As String) As String()
        text = SingleSpace(text)
        Return ToArray(text, SPACE)
    End Function

    Public Shared Function SentenceSplit(ByVal sentence As String) As List(Of String)
        sentence = TrimLow(sentence)

        Dim AbbCol As New List(Of String)
        AbbCol.Add("sr.")
        AbbCol.Add("mr.")
        AbbCol.Add("jr.")
        AbbCol.Add("inv.")
        AbbCol.Add("...")
        AbbCol.Add("mrs.")
        AbbCol.Add("ms.")
        AbbCol.Add("dr.")
        AbbCol.Add("st.")
        AbbCol.Add("prof.")
        AbbCol.Add("gen.")
        AbbCol.Add("rep.")
        AbbCol.Add("sen.")
        AbbCol.Add("mt.")
        AbbCol.Add("jan.")
        AbbCol.Add("feb.")
        AbbCol.Add("mar.")
        AbbCol.Add("apr.")
        AbbCol.Add("jun.")
        AbbCol.Add("aug.")
        AbbCol.Add("sep.")
        AbbCol.Add("oct.")
        AbbCol.Add("nov.")
        AbbCol.Add("dec.")

        'Encode abbreviations such as Mr. Mrs. and Ms.    
        For Each abb In AbbCol
            Dim abb2 = Replace(abb, ".", "<period>")
            sentence = Replace(sentence, SPACE & abb & SPACE, SPACE & abb2 & SPACE)
        Next

        sentence = Replace(sentence, "?", "?<split>")
        sentence = Replace(sentence, "!", "!<split>")
        sentence = Replace(sentence, ".", ".<split>")
        sentence = Replace(sentence, "<period>", ".")

        Return ToList(sentence, "<split>")
    End Function


    Public Shared Function SingleSpace(ByVal text As String, ByVal Deliminator As String) As String
        'Removes Double spaces or Double Strings within a String
        IsNull(Deliminator)
        IsNull(text)

        Do
            If Search(text, Deliminator + Deliminator) = True Then
                text = Replace(text, Deliminator + Deliminator, Deliminator)
            Else
                Exit Do
            End If
        Loop

        Return Trim(text)
    End Function
    Public Shared Function SingleSpace(ByVal text As String) As String
        'Removes Double spaces
        IsNull(text)

        Do
            If Search(text, SPACE + SPACE) = True Then
                text = Replace(text, SPACE + SPACE, SPACE)
            Else
                Exit Do
            End If
        Loop

        Return Trim(text)
    End Function

    Public Shared Function SingleSpaceBuffer(ByVal text As String, ByVal Count As Integer, ByVal Buffer As String) As String
        'Removes Double spaces or Double Strings within a String
        'Then Adds to the Space or Strings
        text = SingleSpace(text, Buffer)
        text = Replace(text, Buffer, MultiStrings(Buffer, Count))
        Return Trim(text)
    End Function

    Public Shared Function MultiStrings(ByVal text As String, ByVal Count As Integer) As String
        'Returns a the text after its been multiplied a set number of times
        IsNull(text)
        IntR(Count, 1)

        Dim Result = ""
        For i = 1 To Count
            Result = Result + text
        Next

        Return Result
    End Function

    Public Shared Function ReverseString(ByVal text As String) As String
        IsNull(text)

        Dim newString = ""
        For i = text.Length() - 1 To 0 Step -1
            newString = newString + text(i)
        Next

        Return newString
    End Function

    Public Shared Sub SetIndex(ByRef text As String, ByVal index As Integer, ByVal value As Char)

        IsNull(text)
        IsNull(index)

        Dim copy = text
        If index < 0 Then index = 0

        If text.Length <= index Then
            For i = 0 To index - (text.Length - 1)
                copy &= ""
            Next
        End If

        text = ""
        For i = 0 To copy.Length - 1
            If i = index Then
                text &= value
            Else
                text &= copy.ElementAt(i)
            End If
        Next

    End Sub

    Public Shared Function GetIndex(ByVal text As String, ByVal index As Integer) As Char
        IsNull(text)
        IsNull(index)
        If text.Length <= index Then Return ""
        If index < 0 Then index = 0
        Return text.ElementAt(index)
    End Function


#End Region


#Region "Random"

    Private Shared ReadOnly myRandom As New Random(Date.Now.Millisecond())
    Public Shared Function RandBool(Optional ByVal ChanceToBeTrue As Integer = 50) As Boolean
        IsNull(ChanceToBeTrue)
        If myRandom.Next(0, 100) <= ChanceToBeTrue Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function Rand(ByVal MIN As Integer, ByVal MAX As Integer) As Integer
        'Returns a random random number between two digits.
        IsNull(MIN)
        IsNull(MAX)

        If MIN = MAX Then Return MIN
        If MAX > MIN Then
            Return myRandom.Next(MIN, MAX + 1)
        Else
            Return myRandom.Next(MAX, MIN + 1)
        End If
    End Function

    Public Shared Function RandDouble(ByVal MIN As Double, ByVal MAX As Double) As Double
        If MAX = MIN Then Return MIN

        Dim rnd As Integer = 0

        Dim sMIN = Convert.ToString(MIN).Length
        Dim sMAX = Convert.ToString(MAX).Length

        Dim sig As Integer = 0

        If sMIN > sMAX Then
            sig = 10 ^ sMIN
        Else
            sig = 10 ^ sMAX
        End If

        Dim intMIN = Convert.ToInt32(MIN * sig)
        Dim intMAX = Convert.ToInt32(MAX * sig)

        If intMAX > intMIN Then
            rnd = myRandom.Next(intMIN, intMAX + 1)
        Else
            rnd = myRandom.Next(intMAX, intMIN + 1)
        End If

        rnd = Rand(intMIN, intMAX)

        Return rnd / sig
    End Function

    Public Shared Function RandKey(ByVal keyLength As Integer) As String
        Dim Results As String = ""
        If keyLength < 1 Then keyLength = 1

        For i = 0 To keyLength - 1
            Results = Results + KeyArray.Item(myRandom.Next(0, KeyArray.Count - 1))
        Next

        Return Results
    End Function

    Private Shared Function KeyArray() As List(Of String)
        Dim newList As New List(Of String)
        newList.AddRange(AlphabetUC)

        For i = 0 To 9
            newList.Add(i.ToString)
        Next

        Return newList
    End Function



    Public Shared Function IsEven(ByVal Numb As Integer)
        IsNull(Numb)
        Numb = Math.Abs(Numb)

        If Numb Mod (2) = 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function IsOdd(ByVal Numb As Integer)
        IsNull(Numb)
        Numb = Math.Abs(Numb)

        If Numb Mod (2) <> 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function Steps(ByVal Count As Integer, ByVal iStep As Integer) As Boolean
        IsNull(Count)
        IsNull(iStep)

        Count = Math.Abs(Count)
        iStep = Math.Abs(iStep)

        If Count Mod (iStep) = 0 Then
            Return True
        Else
            Return False
        End If
    End Function


#End Region


#Region "Convert"

    Public Shared Function GetHighInt(ByVal intList As List(Of Integer)) As Integer
        Dim HightCount As Integer = Nothing
        Dim Count1 = 0
        Dim Count2 = 0

        For Each Item1 In intList
            For Each Item2 In intList

                If Count1 < Count2 Then
                    If HightCount = Nothing Then HightCount = Count2
                    If Count2 > HightCount Then HightCount = Count2
                End If

            Next
        Next

        Return HightCount
    End Function

    Public Shared Function GetLowInt(ByVal intList As List(Of Integer)) As Integer
        Dim LowCount As Integer = Nothing
        Dim Count1 = 0
        Dim Count2 = 0

        For Each Item1 In intList
            For Each Item2 In intList
                If Count1 > Count2 Then
                    If LowCount = Nothing Then LowCount = Count2
                    If Count2 < LowCount Then LowCount = Count2
                End If
            Next
        Next

        Return LowCount
    End Function


    Public Shared Function ToDbl(ByVal newObject As Object) As Double
        Try
            Return CType(newObject, Double)
        Catch
            Return Nothing
        End Try
    End Function

    Public Shared Function ToDbl(ByVal newObject As Object, ByVal Defaults As Double) As Double
        Try
            Return CType(newObject, Double)
        Catch
            Return Defaults
        End Try
    End Function

    Public Shared Function ToSingle(ByVal newObject As Object, ByVal Defaults As Single) As Single
        Try
            Return CType(newObject, Single)
        Catch
            Return Defaults
        End Try
    End Function

    Public Shared Function ToDec(ByVal newObject As Object) As Decimal
        Try
            Return CType(newObject, Decimal)
        Catch
            Return Nothing
        End Try
    End Function

    Public Shared Function ToDec(ByVal newObject As Object, ByVal Defaults As Decimal) As Decimal
        Try
            Return CType(newObject, Decimal)
        Catch
            Return Defaults
        End Try
    End Function

    Public Shared Function ToInt(ByVal newObject As Object) As Integer
        Try
            Return CType(newObject, Integer)
        Catch
            Return Nothing
        End Try
    End Function

    Public Shared Function ToInt(ByVal newObject As Object, ByVal Defaults As Integer) As Integer
        Try
            Return CType(newObject, Integer)
        Catch
            Return Defaults
        End Try
    End Function

    Public Shared Function ToStr(ByVal newObject As Object) As String
        Try
            Return CType(newObject, String)
        Catch
            Return Nothing
        End Try
    End Function

    Public Shared Function ToStr(ByVal newObject As Object, ByVal Defaults As String) As String
        Try
            Return CType(newObject, String)
        Catch
            Return Defaults
        End Try
    End Function

    Public Shared Function ToByte(ByVal newObject As Object) As Byte
        Try
            Return CType(newObject, Byte)
        Catch
            Return Nothing
        End Try
    End Function

    Public Shared Function ToByte(ByVal newObject As Object, ByVal Defaults As Byte) As Byte
        Try
            Return CType(newObject, Byte)
        Catch
            Return Defaults
        End Try
    End Function



    Public Shared Function IsInt(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is Integer Then
            Return True
        End If

        If TypeOf newObject Is String Then
            Dim out As Integer = 0
            Return Integer.TryParse(newObject, out)
        End If

        Return False
    End Function

    Public Shared Function IsStr(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is String Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function IsArray(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is String() Then
            Return True
        End If
        If TypeOf newObject Is Array Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function IsDec(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is Decimal Then
            Return True
        End If

        If TypeOf newObject Is String Then
            Dim out As Decimal = 0
            Return Decimal.TryParse(newObject, out)
        End If

        Return False
    End Function

    Public Shared Function IsDouble(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is Double Then
            Return True
        End If

        If TypeOf newObject Is String Then
            Dim out As Double = 0
            Return Double.TryParse(newObject, out)
        End If

        Return False
    End Function

    Public Shared Function IsSingle(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is Single Then
            Return True
        End If

        If TypeOf newObject Is String Then
            Dim out As Double = 0
            Return Single.TryParse(newObject, out)
        End If

        Return False
    End Function

    Public Shared Function IsByte(ByVal newObject As Object) As Boolean
        If TypeOf newObject Is Byte Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function IsNumber(ByVal newObject As Object) As Boolean
        Return IsNumeric(newObject)
    End Function

    Public Shared Function IsBool(ByVal Bool As Object) As Boolean

        If TypeOf Bool Is Boolean Then
            If Bool = True Then Return True
            If Bool = False Then Return True
        End If

        If TypeOf Bool Is String Then
            If Search(Bool, {"true", "false", "1", "-1", "0"}, Gate.eEQUALS) = True Then
                Return True
            Else
                Return False
            End If
        End If

        Return False
    End Function

    Public Shared Function ToBool(ByVal Bool As Object) As Boolean
        Return ToBool(Bool, False)
    End Function

    Public Shared Function ToBool(ByVal Bool As Object, ByVal Defaults As Boolean) As Boolean

        If TypeOf Bool Is Boolean Then
            If Bool = True Then Return True
            If Bool = False Then Return False
        End If

        If TypeOf Bool Is String Then
            If Search(Bool, {"true", "1", "-1"}, Gate.eEQUALS) = True Then
                Return True
            Else
                Return False
            End If
        End If

        Return Defaults
    End Function

    Public Shared Function BoolToStr(ByVal inBool As Boolean) As String
        If inBool = True Then
            Return "true"
        Else
            Return "false"
        End If
    End Function


    Public Shared Function IntOnly(ByVal text As String) As String
        'number, ., -
        Dim returns As String = ""
        For Each cnt In text.ToArray
            If IsNumber(cnt) = True Or cnt = Chr(45) Or cnt = Chr(46) Then
                returns &= cnt
            End If
        Next

        Return returns
    End Function


    Public Shared Function IsEmpty(ByVal text As String) As Boolean
        IsNull(text)
        If Trim(text).Length > 0 Then
            Return False
        End If
        Return True
    End Function

    Public Shared Function IsEmpty(ByVal text As String, ByVal delault As String) As String
        IsNull(text)
        If Trim(text).Length > 0 Then
            Return text
        End If
        Return delault
    End Function

    Public Shared Function IsEmpty(ByVal array_1 As String()) As Boolean

        For Each item In array_1
            IsNull(item)
            If item.Length > 0 Then Return False
        Next

        Return True
    End Function

    Public Shared Function IsEmpty(ByVal array_1 As String(), ByVal delault As String()) As String()

        For Each item In array_1
            IsNull(item)
            If item.Length > 0 Then Return array_1
        Next

        Return delault
    End Function

    Public Shared Function IsEmpty(ByVal array_1 As List(Of String)) As Boolean

        For Each item In array_1
            IsNull(item)
            If item.Length > 0 Then Return False
        Next

        Return True
    End Function

    Public Shared Function IsEmpty(ByVal array_1 As List(Of String), ByVal delault As List(Of String)) As List(Of String)

        For Each item In array_1
            IsNull(item)
            If item.Length > 0 Then Return array_1
        Next

        Return delault
    End Function


    Public Shared Function InRange(ByVal number As Double, ByVal min As Double, ByVal max As Double, ByVal inclussive As Boolean) As Boolean

        If inclussive = False Then
            If number > min And number < max Then
                Return True
            Else
                Return False
            End If
        Else
            If number >= min And number <= max Then
                Return True
            Else
                Return False
            End If
        End If

    End Function

    Public Shared Function InRange(ByVal base As String(), ByVal number As Integer) As Boolean
        If base.Count > number And 0 <= number Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function InRange(ByVal base As List(Of String), ByVal number As Integer) As Boolean
        If base.Count > number And 0 <= number Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function RangeTrim(ByVal number As Double, ByVal min As Double, ByVal max As Double) As Double
        If number > max Then number = max
        If number < min Then number = min
        Return number
    End Function


    Public Shared Function CompareInt(ByVal base As Double, ByVal icompare As Double, ByVal eCompare As OCompare) As Boolean

        Select Case eCompare
            Case OCompare.Equals
                If base = icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.Greater
                If base < icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.GreaterEquals
                If base <= icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.Lesser
                If base > icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.LesserEquals
                If base >= icompare Then
                    Return True
                Else
                    Return False
                End If
            Case Else
                Return False
        End Select

    End Function

    Public Shared Function CompareDbl(ByVal base As Integer, ByVal icompare As Integer, ByVal eCompare As OCompare) As Boolean
        Dim int_base = ToDbl(base)
        Dim int_compare = ToDbl(icompare)

        Select Case eCompare
            Case OCompare.Equals
                If base = icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.Greater
                If base < icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.GreaterEquals
                If base <= icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.Lesser
                If base > icompare Then
                    Return True
                Else
                    Return False
                End If
            Case OCompare.LesserEquals
                If base >= icompare Then
                    Return True
                Else
                    Return False
                End If
            Case Else
                Return False
        End Select

    End Function


#End Region


#Region "Read And Write"

    Public Shared Function TextReader(ByVal strFile As String) As String
        IsNull(strFile)
        If FileExists(strFile) = False Then Return ""

        'Try
        Using filereader As New IO.StreamReader(Trim(strFile))
            Return filereader.ReadToEnd()
        End Using
        'Catch : End Try

        Return ""
    End Function
    Public Shared Function TextReader(ByVal strFile As String, ByVal Compress As Boolean) As String
        IsNull(strFile)
        If FileExists(strFile) = False Then Return ""

        'Try
        If Compress = True Then
            Using gzOut As New System.IO.Compression.GZipStream(IO.File.OpenRead(Trim(strFile)), IO.Compression.CompressionMode.Decompress)
                Using filereader As New IO.StreamReader(gzOut)
                    Return filereader.ReadToEnd()
                End Using
            End Using
        Else
            Return TextReader(strFile)
        End If
        'Catch : End Try

        Return ""
    End Function
    Public Shared Function TextReader(ByVal strFile As String, ByVal Decompress As Boolean, ByVal Decryption As Boolean, ByVal Key As String) As String
        IsNull(strFile)
        If FileExists(strFile) = False Then Return ""

        'Try
        If Decompress = True Then
            Using gzOut As New System.IO.Compression.GZipStream(IO.File.OpenRead(Trim(strFile)), IO.Compression.CompressionMode.Decompress)
                Using filereader As New IO.StreamReader(gzOut)
                    If Decryption = True Then
                        Return Decrypt(filereader.ReadToEnd(), Key)
                    Else
                        Return filereader.ReadToEnd()
                    End If
                End Using
            End Using
        Else
            If Decryption = True Then
                Return Decrypt(TextReader(strFile), Key)
            Else
                Return TextReader(strFile)
            End If
        End If
        'Catch : End Try

        Return ""
    End Function

    Public Shared Sub TextWriter(ByVal strFile As String, ByVal TextToWrite As String, ByVal toAppend As Boolean, ByVal Compress As Boolean, ByVal Encryption As Boolean, ByVal Key As String)
        IsNull(strFile)
        IsNull(TextToWrite)

        'Try
        If Compress = True Then
            Using gzOut As New IO.Compression.GZipStream(IO.File.Create(Trim(strFile)), IO.Compression.CompressionLevel.Optimal)
                Using fileWriter As New IO.StreamWriter(gzOut)
                    If Encryption = True Then
                        fileWriter.Write(Encrypt(TextToWrite, Key))
                    Else
                        fileWriter.Write(TextToWrite)
                    End If
                End Using
            End Using
        Else
            If Encryption = True Then
                TextWriter(strFile, Encrypt(TextToWrite, Key), toAppend)
            Else
                TextWriter(strFile, TextToWrite, toAppend)
            End If
        End If
        'Catch : End Try

    End Sub
    Public Shared Sub TextWriter(ByVal strFile As String, ByVal TextToWrite As String, ByVal toAppend As Boolean, ByVal Compress As Boolean)
        IsNull(strFile)
        IsNull(TextToWrite)

        'Try
        If Compress = True Then
            Using gzOut As New IO.Compression.GZipStream(IO.File.Create(Trim(strFile)), IO.Compression.CompressionLevel.Optimal)
                Using fileWriter As New IO.StreamWriter(gzOut)
                    fileWriter.Write(TextToWrite)
                End Using
            End Using
        Else
            TextWriter(strFile, TextToWrite, toAppend)
        End If
        'Catch : End Try

    End Sub
    Public Shared Sub TextWriter(ByVal strFile As String, ByVal TextToWrite As String, ByVal toAppend As Boolean)
        IsNull(strFile)
        IsNull(TextToWrite)

        'Try
        Using fileWriter As New IO.StreamWriter(Trim(strFile), toAppend, New System.Text.UTF8Encoding(False))
            fileWriter.Write(TextToWrite)
        End Using
        'Catch : End Try
    End Sub
    Public Shared Sub TextWriter(ByVal strFile As String, ByVal ms As IO.MemoryStream, ByVal toAppend As Boolean)
        IsNull(strFile)

        'Try
        Using fileWriter As New IO.StreamWriter(Trim(strFile), toAppend, New System.Text.UTF8Encoding(False))
            fileWriter.Write(StreamReader(ms))
        End Using
        'Catch : End Try
    End Sub


    Public Shared Sub FileWriter(ByVal strFile As String, ByVal bytes As Byte(), ByVal toAppend As Boolean)
        IsNull(strFile)
        'Try
        My.Computer.FileSystem.WriteAllBytes(strFile, bytes, False)
        'Catch : End Try
    End Sub
    Public Shared Function FileReader(ByVal strFile As String) As Byte()
        IsNull(strFile)
        'Try
        Return My.Computer.FileSystem.ReadAllBytes(strFile)
        'Catch : End Try
    End Function


    Public Shared Function ZipStreamReader(ByVal strFile As String, ByVal entryName As String) As IO.Stream
        'This will return a coppied Stream
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return New IO.MemoryStream()

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entryName, entry.FullName(), Gate.eEQUALS) = True Then
                        Using Reader As New IO.StreamReader(entry.Open())
                            Dim MS = New IO.MemoryStream()
                            Reader.BaseStream.CopyTo(MS)
                            Return MS
                        End Using
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return Nothing
    End Function
    Public Shared Function ZipStreamReader(ByVal strFile As String, ByVal entryName As String()) As IO.Stream
        'This will return a coppied Stream
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return New IO.MemoryStream()

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                        Using Reader As New IO.StreamReader(entry.Open())
                            Dim MS = New IO.MemoryStream()
                            Reader.BaseStream.CopyTo(MS)
                            Return MS
                        End Using
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return Nothing
    End Function
    Public Shared Function ZipStreamReader(ByVal strFile As String) As TypeMemory
        'This will return a coppied list of Streams
        IsNull(strFile)

        Dim outList As New TypeMemory
        If FileExists(strFile) = False Then Return outList

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If entry.Name().Length() = 0 Then Continue For
                    Using Reader As New IO.StreamReader(entry.Open())
                        Dim MS = New IO.MemoryStream()
                        Reader.BaseStream.CopyTo(MS)
                        outList.Add(entry.FullName, MS)
                    End Using
                Next
            End Using
        End Using
        'Catch : End Try

        Return outList
    End Function

    Public Shared Function ZipTextReader(ByVal strFile As String) As TypeMemory
        'This will return a coppied list of Streams
        IsNull(strFile)

        Dim outList As New TypeMemory
        If FileExists(strFile) = False Then Return outList

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If entry.Name().Length() > 0 Then
                        Using Reader As New IO.StreamReader(entry.Open())
                            outList.Add(entry.FullName, entry.Name(), Reader.ReadToEnd)
                        End Using
                    Else
                        outList.Add(entry.FullName, Nothing, Nothing)
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return outList
    End Function
    Public Shared Sub ZipTextWriter(ByVal strFile As String, ByVal source As TypeMemory, Optional ByVal binary As Boolean = False)
        IsNull(strFile)

        'Try
        Using zipStream As New IO.FileStream(strFile, System.IO.FileMode.OpenOrCreate)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                For Each par In source.Items()
                    IsNull(par.Index())

                    For Each entry In archive.Entries
                        If Search(par.Index(), entry.FullName(), Gate.eEQUALS) = True Then
                            entry.Delete()
                            Exit For
                        End If
                    Next

                    If IsEmpty(par.Index()) = False Then
                        Dim newEntry = archive.CreateEntry(par.Index(), IO.Compression.CompressionLevel.Optimal)
                        If par.Entry() IsNot Nothing Then
                            If binary = False Then
                                Using writer = New IO.StreamWriter(newEntry.Open())
                                    writer.Write(par.Entry())
                                End Using
                            Else
                                Using writer = New IO.BinaryWriter(newEntry.Open())
                                    writer.Write(par.Entry())
                                End Using
                            End If
                        End If
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

    End Sub


    Public Shared Function StreamReader(ByVal ms As IO.MemoryStream) As String
        Using ms
            ms.Position = 0
            Dim sr = New System.IO.StreamReader(ms)
            Return sr.ReadToEnd
        End Using
    End Function
    Public Shared Function StreamWriter(ByVal text As String) As IO.MemoryStream
        Dim ms As New IO.MemoryStream

        Using sr As New System.IO.StreamWriter(New IO.MemoryStream, New System.Text.UTF8Encoding(False))
            sr.Write(text)
            sr.BaseStream.CopyTo(ms)
            Return ms
        End Using
    End Function
    Public Shared Function StreamToByte(ByVal stream As IO.Stream) As Byte()
        Using ms As New IO.MemoryStream
            stream.CopyTo(ms)
            Return ms.ToArray()
        End Using
    End Function


    Public Shared Function ZipReader(ByVal strFile As String, ByVal entryName As String) As String
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return ""

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entryName, entry.FullName(), Gate.eEQUALS) = True Then
                        Using Reader As New IO.StreamReader(entry.Open())
                            Return Reader.ReadToEnd()
                        End Using
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return ""
    End Function
    Public Shared Function ZipReader(ByVal strFile As String, ByVal entryName As String()) As String
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return ""

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                        Using Reader As New IO.StreamReader(entry.Open())
                            Return Reader.ReadToEnd()
                        End Using
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return ""
    End Function


    Public Shared Function ZipReadAll(ByVal strFile As String) As List(Of String)
        IsNull(strFile)
        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, System.IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                For Each entry In archive.Entries
                    Using Reader As New IO.StreamReader(entry.Open())
                        Results.Add(Reader.ReadToEnd())
                    End Using
                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipReader(ByVal strFile As String) As Pairs
        IsNull(strFile)
        Dim Results As New Pairs
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If entry.Name().Length() = 0 Then Continue For
                    Using Reader As New IO.StreamReader(entry.Open())
                        Results.Add(entry.FullName(), Reader.ReadToEnd())
                    End Using
                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function


    Public Shared Sub ZipWriter(ByVal strFile As String, ByVal entryName As String, ByVal entryContent As Object, Optional ByVal binary As Boolean = False)
        IsNull(strFile)
        IsNull(entryName)

        'Try
        Using zipStream = New IO.FileStream(strFile, IO.FileMode.OpenOrCreate)
            Using archive = New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                If IsEmpty(entryName) = False Then

                    For Each entry In archive.Entries
                        If Search(entryName, entry.FullName(), Gate.eEQUALS) = True Then
                            entry.Delete()
                            Exit For
                        End If
                    Next

                    Dim newEntry = archive.CreateEntry(entryName, IO.Compression.CompressionLevel.Optimal)

                    If IsNothing(entryContent) = False Then
                        If binary = False Then
                            Using writer = New IO.StreamWriter(newEntry.Open())
                                writer.Write(entryContent)
                            End Using
                        Else
                            Using writer = New IO.BinaryWriter(newEntry.Open())
                                writer.Write(entryContent)
                            End Using
                        End If
                    End If

                End If
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub ZipWriter(ByVal strFile As String, ByVal entryName As String(), ByVal entryContent As Object, Optional ByVal binary As Boolean = False)
        IsNull(strFile)
        IsNull(entryName)

        'Try
        Using zipStream = New IO.FileStream(strFile, IO.FileMode.OpenOrCreate)
            Using archive = New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                If IsEmpty(entryName) = False Then

                    For Each entry In archive.Entries
                        Dim Items = ToList(entry.FullName, FWD_SLASH)

                        If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                            entry.Delete()
                            Exit For
                        End If
                    Next

                    Dim newEntry = archive.CreateEntry(Join(entryName, FWD_SLASH), IO.Compression.CompressionLevel.Optimal)
                    If IsNothing(entryContent) = False Then
                        If binary = False Then
                            Using writer = New IO.StreamWriter(newEntry.Open())
                                writer.Write(entryContent)
                            End Using
                        Else
                            Using writer = New IO.BinaryWriter(newEntry.Open())
                                writer.Write(entryContent)
                            End Using
                        End If
                    End If
                End If
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub ZipWriter(ByVal strFile As String, ByVal entryName As String)
        'used to make directory in zip
        IsNull(strFile)
        IsNull(entryName)

        'Try
        Using zipStream = New IO.FileStream(strFile, IO.FileMode.OpenOrCreate)
            Using archive = New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                If IsEmpty(entryName) = False Then

                    For Each entry In archive.Entries
                        If Search(entryName, entry.FullName(), Gate.eEQUALS) = True Then
                            entry.Delete()
                            Exit For
                        End If
                    Next

                    Dim newEntry = archive.CreateEntry(entryName, IO.Compression.CompressionLevel.Optimal)

                End If
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub ZipWriter(ByVal strFile As String, ByVal entryName As String())
        'used to make directory in zip
        IsNull(strFile)
        IsNull(entryName)

        'Try
        Using zipStream = New IO.FileStream(strFile, IO.FileMode.OpenOrCreate)
            Using archive = New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)

                If IsEmpty(entryName) = False Then

                    For Each entry In archive.Entries
                        Dim Items = ToList(entry.FullName, FWD_SLASH)

                        If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                            entry.Delete()
                            Exit For
                        End If
                    Next

                    Dim newEntry = archive.CreateEntry(Join(entryName, FWD_SLASH) & FWD_SLASH, IO.Compression.CompressionLevel.Optimal)

                End If
            End Using
        End Using
        'Catch : End Try

    End Sub


    Public Shared Function ZipGetEntries(ByVal strFile As String, ByVal filesOnly As Boolean) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If filesOnly = True Then
                        If entry.Name().Length() = 0 Then Continue For
                    End If

                    Results.Add(entry.FullName())
                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetEntries(ByVal strFile As String, ByVal address As String()) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToArray(entry.FullName, FWD_SLASH)

                    If Search(Items, address, True, Gate.eSTART) = True Then
                        Results.Add(entry.Name())
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function


    Public Shared Function ZipGetFolders(ByVal strFile As String, ByVal address As String) As List(Of String)
        IsNull(strFile)

        Dim Depth = CharCount(address, FWD_SLASH)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries

                    If Search(entry.FullName, address, Gate.eSTART) = True Then
                        Dim Items = ToList(entry.FullName, FWD_SLASH)
                        Dim item = GetIndex(Items, Depth, "")
                        If Results.Contains(item) = False Then
                            If IsEmpty(item) = False Then Results.Add(item)
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFolders(ByVal strFile As String, ByVal address As String()) As List(Of String)
        IsNull(strFile)

        Dim Depth = address.Length()

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)

                    If Search(Items, address, True, Gate.eSTART) = True Then

                        Dim item = GetIndex(Items, Depth, "")
                        If Results.Contains(item) = False Then
                            If IsEmpty(item) = False Then Results.Add(item)
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function


    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(entry.FullName, address, Gate.eSTART) = True Then
                        Results.Add(entry.Name())
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String()) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(Items, address, True, Gate.eSTART) = True Then
                        Results.Add(entry.Name())
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String, ByVal filter As String, ByVal eGate As Gate) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(entry.FullName, address, Gate.eSTART) = True Then
                        If Search(entry.FullName, filter, eGate) = True Then
                            Results.Add(entry.Name())
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String(), ByVal filter As String(), ByVal eGate As Gate) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(Items, address, True, Gate.eSTART) = True Then
                        If Search(entry.FullName, filter, eGate) = True Then
                            Results.Add(entry.Name())
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String, ByVal filter() As String, ByVal eGate As Gate) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(entry.FullName, address, Gate.eSTART) = True Then
                        If Search(entry.FullName, filter, eGate) = True Then
                            Results.Add(entry.Name())
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function
    Public Shared Function ZipGetFiles(ByVal strFile As String, ByVal address As String(), ByVal filter As String, ByVal eGate As Gate) As List(Of String)
        IsNull(strFile)

        Dim Results As New List(Of String)
        If FileExists(strFile) = False Then Return Results

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)

                    If entry.Name().Length() = 0 Then Continue For
                    If Search(Items, address, True, Gate.eSTART) = True Then
                        If Search(entry.FullName, filter, eGate) = True Then
                            Results.Add(entry.Name())
                        End If
                    End If

                Next
            End Using
        End Using
        'Catch : End Try

        Return Results
    End Function


    Public Shared Function DoesZipEntryExist(ByVal strFile As String, ByVal entryName As String) As Boolean
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return False

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entryName, entry.FullName(), Gate.eEQUALS) = True Then
                        Return True
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return False
    End Function
    Public Shared Function DoesZipEntryExist(ByVal strFile As String, ByVal entryName As String()) As Boolean
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return False

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                        Return True
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return False
    End Function

    Public Shared Function DoesZipFolderExist(ByVal strFile As String, ByVal entryName As String) As Boolean
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return False

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entryName, entry.FullName(), Gate.eSTART) = True Then
                        Return True
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return False
    End Function
    Public Shared Function DoesZipFolderExist(ByVal strFile As String, ByVal entryName As String()) As Boolean
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Return False

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, Gate.eSTART) = True Then
                        Return True
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

        Return False
    End Function


    Public Shared Sub DeleteZipEntry(ByVal strFile As String, ByVal entryName As String)
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entry.FullName(), entryName, Gate.eEQUALS) = True Then
                        entry.Delete()
                        Exit Sub
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub DeleteZipEntry(ByVal strFile As String, ByVal entryName As String, ByVal eGate As Gate)
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    If Search(entry.FullName(), entryName, eGate) = True Then
                        entry.Delete()
                        Exit Sub
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub DeleteZipEntry(ByVal strFile As String, ByVal entryName As String())
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, Gate.eEQUALS) = True Then
                        entry.Delete()
                        Exit Sub
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

    End Sub
    Public Shared Sub DeleteZipEntry(ByVal strFile As String, ByVal entryName As String(), ByVal eGate As Gate)
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        'Try
        Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                For Each entry In archive.Entries
                    Dim Items = ToList(entry.FullName, FWD_SLASH)
                    If Search(Items, entryName, True, eGate) = True Then
                        entry.Delete()
                        Exit Sub
                    End If
                Next
            End Using
        End Using
        'Catch : End Try

    End Sub


    Public Shared Sub DeleteZipEntries(ByVal strFile As String, ByVal entryName As String, ByVal eGate As Gate)
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        Dim NoEntry As Boolean = True

        'Try
        Do Until NoEntry = False
            NoEntry = False

            Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)

                Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                    For Each entry In archive.Entries
                        If Search(entry.FullName(), entryName, eGate) = True Then
                            entry.Delete()
                            NoEntry = True
                            Continue Do
                        End If
                    Next
                End Using

            End Using

        Loop
        'Catch : End Try

    End Sub
    Public Shared Sub DeleteZipEntries(ByVal strFile As String, ByVal entryName As String(), ByVal eGate As Gate)
        IsNull(strFile)
        IsNull(entryName)
        If FileExists(strFile) = False Then Exit Sub

        Dim NoEntry As Boolean = True

        'Try
        Do Until NoEntry = False
            NoEntry = False

            Using zipStream As New IO.FileStream(strFile, IO.FileMode.Open)

                Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Update)
                    For Each entry In archive.Entries
                        Dim Items = ToList(entry.FullName, FWD_SLASH)
                        If Search(Items, entryName, True, eGate) = True Then
                            entry.Delete()
                            NoEntry = True
                            Continue Do
                        End If
                    Next
                End Using

            End Using

        Loop
        'Catch : End Try

    End Sub


    Public Shared Sub RenameZipEntries(ByVal strFile As String, ByVal old_Entry As String, ByVal new_Entry As String)

        For Each entry In ZipGetEntries(strFile, False)
            If Search(entry, old_Entry, Gate.eSTART) = True Then

                Dim text = ZipReader(strFile, entry)
                DeleteZipEntry(strFile, entry)

                entry = new_Entry & Mid(entry, old_Entry.Length + 1, entry.Length)
                ZipWriter(strFile, entry, text)

            End If
        Next
    End Sub
    Public Shared Sub RenameZipEntry(ByVal strFile As String, ByVal old_Entry As String, ByVal new_Entry As String)
        Dim text = ZipReader(strFile, old_Entry)
        DeleteZipEntry(strFile, old_Entry)
        ZipWriter(strFile, new_Entry, text)
    End Sub
    Public Shared Sub RenameZipEntries(ByVal strFile As String, ByVal old_Entry As String(), ByVal new_Entry() As String)

        For Each entry In ZipGetEntries(strFile, False)
            Dim Items = ToArray(entry, FWD_SLASH)

            If Search(Items, old_Entry, True, Gate.eSTART) = True Then

                Dim text = ZipReader(strFile, entry)
                DeleteZipEntry(strFile, entry)

                Dim new_address = Concat(new_Entry, GetRange(Items, old_Entry.Length))
                ZipWriter(strFile, new_address, text)

            End If
        Next
    End Sub
    Public Shared Sub RenameZipEntry(ByVal strFile As String, ByVal old_Entry() As String, ByVal new_Entry() As String)
        Dim text = ZipReader(strFile, old_Entry)
        DeleteZipEntry(strFile, old_Entry)
        ZipWriter(strFile, new_Entry, text)
    End Sub


    Public Shared Sub CopyZipEntries(ByVal strFile As String, ByVal old_Entry As String, ByVal new_Entry As String)

        For Each entry In ZipGetEntries(strFile, False)
            If Search(entry, old_Entry, Gate.eSTART) = True Then

                Dim text = ZipReader(strFile, entry)
                entry = new_Entry & Mid(entry, old_Entry.Length + 1, entry.Length)
                ZipWriter(strFile, entry, text)
            End If
        Next
    End Sub
    Public Shared Sub CopyZipEntry(ByVal strFile As String, ByVal old_Entry As String, ByVal new_Entry As String)
        Dim text = ZipReader(strFile, old_Entry)
        ZipWriter(strFile, new_Entry, text)
    End Sub
    Public Shared Sub CopyZipEntries(ByVal strFile As String, ByVal old_Entry As String(), ByVal new_Entry As String())

        For Each entry In ZipGetEntries(strFile, False)
            Dim Items = ToArray(entry, FWD_SLASH)

            If Search(Items, old_Entry, True, Gate.eSTART) = True Then
                Dim text = ZipReader(strFile, entry)

                Dim new_address = Concat(new_Entry, GetRange(Items, old_Entry.Length))
                ZipWriter(strFile, new_address, text)
            End If
        Next
    End Sub
    Public Shared Sub CopyZipEntry(ByVal strFile As String, ByVal old_Entry() As String, ByVal new_Entry() As String)
        Dim text = ZipReader(strFile, old_Entry)
        ZipWriter(strFile, new_Entry, text)
    End Sub


    Public Shared Function Encrypt(ByVal TextToCypher As String, ByVal Key As String) As String
        IsNull(TextToCypher)
        IsNull(Key)
        Dim Code As New Simple3DES(Key)
        Return Code.Encrypt(TextToCypher)
    End Function

    Public Shared Function Decrypt(ByVal TextToUnCypher As String, ByVal Key As String) As String
        IsNull(TextToUnCypher)
        IsNull(Key)
        Dim Code As New Simple3DES(Key)
        Return Code.Decrypt(TextToUnCypher)
    End Function


#End Region


#Region "Programming"

    Public Shared Function Directory() As String
        Return AppDomain.CurrentDomain.BaseDirectory()
    End Function

    Public Shared Function ProgramName() As String
        Return System.Diagnostics.Process.GetCurrentProcess().ProcessName()
    End Function

    Public Shared Function CommandPrompt(ByVal command As String) As String
        IsNull(command)
        Dim Output = ""
        Dim ConError = ""

        Try
            Dim startInfo As New System.Diagnostics.ProcessStartInfo("cmd.exe")
            startInfo.Arguments = "/C CD/ && " & command
            startInfo.RedirectStandardError = True 'Change to true to allow Prompt Errors.
            startInfo.RedirectStandardOutput = True
            startInfo.UseShellExecute = False
            startInfo.CreateNoWindow = True
            Dim proc = System.Diagnostics.Process.Start(startInfo)
            Output = proc.StandardOutput.ReadToEnd()
            ConError = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
        Catch ex As Exception
            Return Debug.Error_Message(ex)
        End Try

        If ConError > "" Then
            Return Debug.eERROR & ConError
        Else
            Return Output
        End If

    End Function

    Public Shared Sub ExitProgram(ByVal name As String)
        'Used to Kill the Debugs.exe program from an outside source
        IsNull(name)
        name = RemoveExtension(name)
        For Each prog In System.Diagnostics.Process.GetProcesses
            If Search(prog.ProcessName(), name, Gate.eEQUALS) = True Then
                prog.Kill()
            End If
        Next
    End Sub

    Public Shared Sub ExitProgram()
        'Used to Kill this program
        Try
            Application.Current.Shutdown()
        Catch
            Try
                Environment.Exit(0)
            Catch
                Try
                    ExitProgram(ProgramName())
                Catch
                End Try
            End Try
        End Try

    End Sub

    Public Shared Function OpenFolder(ByVal filepath As String)
        Return OpenProgram("explorer.exe", filepath, False)
    End Function

    Public Shared Function ProgramCount(ByVal name As String) As Integer
        IsNull(name)
        name = RemoveExtension(name)
        Dim count = 0
        For Each prog In System.Diagnostics.Process.GetProcesses
            If Search(prog.ProcessName(), name, Gate.eEQUALS) = True Then
                count = count + 1
            End If
        Next
        Return count
    End Function


    Public Shared Function OpenProgram(ByVal name As String) As String
        Return OpenProgram(name, False)
    End Function

    Public Shared Function OpenProgram(ByVal strProgram As String, ByVal arguments As String, ByVal OutputWait As Boolean) As String
        IsNull(strProgram)
        IsNull(arguments)

        Dim output = ""
        Dim errors = ""

        Try
            Dim startInfo = New System.Diagnostics.ProcessStartInfo(strProgram)
            If IsEmpty(arguments) = False Then
                startInfo.Arguments = Trim(arguments)
            End If

            startInfo.RedirectStandardError = True
            startInfo.RedirectStandardOutput = True
            startInfo.UseShellExecute = False
            startInfo.CreateNoWindow = True
            Dim proc = System.Diagnostics.Process.Start(startInfo)

            If OutputWait = True Then
                output = proc.StandardOutput.ReadToEnd()
                errors = proc.StandardError.ReadToEnd()
                proc.WaitForExit()
            End If

        Catch ex As Exception
            Return Debug.Error_Message(ex)
        End Try

        If IsEmpty(errors) = False Then
            Return Debug.eERROR + errors
        Else
            Return output
        End If

    End Function

    Public Shared Function OpenProgram(ByVal strProgram As String, ByVal OutputWait As Boolean) As String
        IsNull(strProgram)

        Dim output = ""
        Dim errors = ""

        Try

            Dim proc = System.Diagnostics.Process.Start(strProgram)

            If OutputWait = True Then
                output = proc.StandardOutput.ReadToEnd()
                errors = proc.StandardError.ReadToEnd()
                proc.WaitForExit()
            End If

        Catch ex As Exception
            Return Debug.Error_Message(ex)
        End Try

        If IsEmpty(errors) = False Then
            Return Debug.eERROR + errors
        Else
            Return output
        End If

    End Function



    Public Shared Function Compiler(ByVal strCode As String, ByVal exeLocation As String, ByVal Language As String, ByVal Assemblies As List(Of String), ByVal Options As String) As String
        IsNull(strCode)
        IsNull(exeLocation)
        IsNull(strCode)

        If IsNothing(Assemblies) = True Then
            Assemblies = New List(Of String)({"System.dll"})
        End If

        Dim codeProvider = System.CodeDom.Compiler.CodeDomProvider.CreateProvider(Language)
        Dim parameters = New System.CodeDom.Compiler.CompilerParameters(Assemblies.ToArray())

        parameters.GenerateExecutable = True
        parameters.OutputAssembly() = exeLocation
        parameters.IncludeDebugInformation() = True
        parameters.TreatWarningsAsErrors() = False
        parameters.WarningLevel() = 3
        parameters.CompilerOptions = Options

        Dim results = codeProvider.CompileAssemblyFromSource(parameters, Trim(strCode))

        If results.Errors.HasErrors = True Then

            Dim errors = ""
            For Each Errs In results.Errors
                Dim line1 = Convert.ToString(Errs.Line())
                errors = errors + "Line# " + line1 + ": " + Errs.ErrorText() + vbCrLf
            Next

            Return Debug.eERROR & " '" & exeLocation & "' " & errors
        Else
            Return Debug.eSUCCESS
        End If

    End Function

    Public Shared Function IsProgramRunning(ByVal name As String) As Boolean
        name = RemoveExtension(name)
        Dim prog = Process.GetProcessesByName(name)
        If prog.Count > 0 Then
            Return True
        Else
            Return False
        End If
    End Function

#End Region


#Region "Array Funtions"
    Public Shared Function C(ByVal list As String()) As String()
        'used to add a compound word within an object array
        Return list
    End Function



    Public Shared Function Copy(ByVal Items As List(Of String)) As List(Of String)
        IsNull(Items)

        Dim newItems As New List(Of String)
        newItems.AddRange(Items)
        Return newItems
    End Function
    Public Shared Function Copy(ByVal Items As String()) As String()
        IsNull(Items)

        Dim newItems(Items.Length) As String
        Items.CopyTo(newItems, 0)
        Return newItems
    End Function
    Public Shared Function ToArray(ByVal Items As List(Of String)) As String()
        IsNull(Items)

        Dim Count = Items.Count
        Dim newString(Count) As String

        For i = 0 To Count - 1
            newString(i) = Items(i)
        Next

        Return newString
    End Function
    Public Shared Function ToArray(ByVal Items As String) As String()
        Dim newArray(Items.Length()) As String
        Dim cnt As Integer = 0
        For Each item In Items
            newArray(cnt) = item
            cnt += 1
        Next
        Return newArray
    End Function
    Public Shared Function ToList(ByVal Items As String()) As List(Of String)
        IsNull(Items)

        Dim newList As New List(Of String)
        newList.AddRange(Items)
        Return newList
    End Function
    Public Shared Function ToLists(ByVal Items As String(,)) As List(Of String())
        Dim newList As New List(Of String())

        For Each itemArray In Items
            newList.Add(ToArray(itemArray))
        Next

        Return newList
    End Function

    Public Shared Function RandArray(ByVal Items As String(), ByVal Defaults As String) As String
        'Returns a random entry within an array of strings
        IsNull(Items)
        If Items.Length() > 0 Then
            Return Items(Rand(0, Items.Length() - 1))
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function RandArray(ByVal Items As List(Of String), ByVal Defaults As String) As String
        IsNull(Items)

        'Returns a random entry within an array of strings
        If Items.Count() > 0 Then
            Return Items(Rand(0, Items.Count() - 1))
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function RandArray(ByVal Items As List(Of String()), ByVal Defaults As String()) As String()
        IsNull(Items)

        If Items.Count() > 0 Then
            Return Items(Rand(0, Items.Count() - 1))
        Else
            Return Defaults
        End If
    End Function

    Public Shared Function RandArray(ByVal Items As String(), ByVal chances As Integer(), ByVal Defaults As String) As String
        'Returns a random entry within an array of strings
        IsNull(Items)
        IsNull(chances)

        Dim total As Integer = 0
        For Each ichance In chances
            total += ichance
        Next

        Dim guess = Rand(0, total)

        total = 0
        For i = 0 To chances.Count - 1
            total += chances(i)

            If total >= guess Then
                Return Items(i)
            End If
        Next

        Return Defaults
    End Function
    Public Shared Function RandArray(ByVal Items As List(Of String), ByVal chances As Integer(), ByVal Defaults As String) As String
        'Returns a random entry within an array of strings
        IsNull(Items)
        IsNull(chances)

        Dim total As Integer = 0
        For Each ichance In chances
            total += ichance
        Next

        Dim guess = Rand(0, total)

        total = 0
        For i = 0 To chances.Count - 1
            total += chances(i)

            If total >= guess Then
                Return Items(i)
            End If
        Next

        Return Defaults
    End Function
    Public Shared Function RandArray(ByVal Items As List(Of String()), ByVal chances As Integer(), ByVal Defaults As String()) As String()
        'Returns a random entry within an array of strings
        IsNull(Items)
        IsNull(chances)

        Dim total As Integer = 0
        For Each ichance In chances
            total += ichance
        Next

        Dim guess = Rand(0, total)

        total = 0
        For i = 0 To chances.Count - 1
            total += chances(i)

            If total >= guess Then
                Return Items(i)
            End If
        Next

        Return Defaults
    End Function


    Public Shared Function ToArray(ByVal text As String, ByVal deliminator As String) As String()
        IsNull(text)
        IsNull(deliminator)
        Return text.Split({deliminator}, StringSplitOptions.RemoveEmptyEntries)
    End Function
    Public Shared Function ToList(ByVal text As String, ByVal deliminator As String) As List(Of String)
        IsNull(text)
        IsNull(deliminator)
        Return New List(Of String)(ToArray(text, deliminator))
    End Function

    Public Shared Function Join(ByVal Input As String(), ByVal deliminator As String) As String
        IsNull(deliminator)
        IsNull(Input)

        Dim Results = ""
        Dim leng = Input.Length() - 1

        For i = 0 To leng
            If i = leng Then
                Results = Results + Input(i)
            Else
                Results = Results + Input(i) + deliminator
            End If
        Next

        Return Results
    End Function
    Public Shared Function Join(ByVal Items As List(Of String), ByVal deliminator As String) As String
        IsNull(Items)
        IsNull(deliminator)

        Dim Results = ""
        Dim leng = Items.Count() - 1

        For i = 0 To leng
            If i = leng Then
                Results = Results + Items(i)
            Else
                Results = Results + Items(i) + deliminator
            End If
        Next

        Return Results
    End Function

    Public Shared Function AlphabetLC() As String()
        Return {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
    End Function
    Public Shared Function AlphabetUC() As String()
        Return {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"}
    End Function

    Public Shared Function IsAlpha(ByVal alph As String) As Boolean
        If Search(AlphabetLC, alph, True, Gate.eEQUALS) = True Then
            Return True
        End If
        Return False
    End Function
    Public Shared Function IsCapitalized(ByVal word As String) As Boolean
        IsNull(word, "")
        If word.Length = 0 Then Return False
        Dim letter As String = word(0)

        For Each cap In AlphabetUC()
            If letter = cap Then Return True
        Next

        Return False

    End Function

    Public Shared Function ToArray(ByVal entry As String(,), ByVal index As Integer) As String()
        IsNull(entry)

        If index > entry.GetUpperBound(0) Then Return {""}
        Dim out As New List(Of String)

        For i = 0 To entry.GetUpperBound(1)
            out.Add(entry(index, i))
        Next

        Return out.ToArray
    End Function
    Public Shared Function ToArray(ByVal entry As List(Of List(Of String))) As String(,)
        IsNull(entry)

        Dim CountY = entry.Count

        Dim CountX = 0
        For Each ent In entry
            If ent.Count > CountX Then CountX = ent.Count
        Next

        Dim newArray(CountY, CountX) As String

        For i = 0 To CountY
            For j = 0 To CountX
                newArray(i, j) = entry(i)(j)
            Next
        Next

        Return newArray
    End Function

    Public Shared Function ToList(ByVal entry As String(,), ByVal index As Integer) As List(Of String)
        IsNull(entry)

        If index > entry.GetUpperBound(0) Then Return New List(Of String)({""})
        Dim out As New List(Of String)

        For j = 0 To entry.GetUpperBound(1)
            out.Add(entry(index, j))
        Next
        Return out
    End Function
    Public Shared Function ToList(ByVal entry As String(,)) As List(Of List(Of String))
        IsNull(entry)

        Dim out As New List(Of List(Of String))

        For i = 0 To entry.GetUpperBound(0)
            Dim arr As New List(Of String)

            For j = 0 To entry.GetUpperBound(1)
                arr.Add(entry(i, j))
            Next
            out.Add(arr)
        Next
        Return out
    End Function

    Public Shared Function ToIntArray(ByVal entry As String()) As Integer()
        Dim newInts(entry.Length) As Integer

        For i = 0 To entry.Length - 1
            newInts(i) = entry(i)
        Next

        Return newInts
    End Function


    '************************************************************
    '************************************************************

    'search compare

    Public Shared Function SearchCompare(ByVal base As List(Of List(Of String())), ByVal find As String(,), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, and adds the array index to the UnEqualsArgs list, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(UnEqualArgs)
        IsNull(base)
        IsNull(find)

        If find.Length() <> base.Count() Then Return False
        If find.Length() = 0 And base.Count() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To find.Length() - 1
            If SearchCompare(base(i), ToArray(find, i), ignoreCase, eGate) = False Then
                If GetIndex(skips, i, 0) = 1 Then
                    UnEqualArgs.Add(i)
                Else
                    Return False
                End If
            End If
        Next

        Return True
    End Function
    Public Shared Function SearchCompare(ByVal base As List(Of List(Of String())), ByVal find As List(Of String()), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, and adds the array index to the UnEqualsArgs list, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(UnEqualArgs)
        IsNull(base)
        IsNull(find)

        If find.Count() <> base.Count() Then Return False
        If find.Count() = 0 And base.Count() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To find.Count() - 1
            If SearchCompare(base(i), find(i), ignoreCase, eGate) = False Then
                If GetIndex(skips, i, 0) = 1 Then
                    UnEqualArgs.Add(i)
                Else
                    Return False
                End If
            End If
        Next

        Return True
    End Function


    Public Shared Function SearchCompare(ByVal base As List(Of String()), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        'Special Compare/Contain
        IsNull(base)
        IsNull(find)

        If find.Length() = 0 And base.Count() = 0 Then Return True

        For Each arr In base
            If Compare(arr, find, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function
    Public Shared Function SearchCompare(ByVal find As String(), ByVal base As List(Of String()), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        'Special Compare/Contain
        IsNull(base)
        IsNull(find)

        If find.Length() = 0 And base.Count() = 0 Then Return True

        For Each arr In base
            If Compare(arr, find, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function
    Public Shared Function SearchCompare(ByVal base As String(,), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        'Special Compare/Contain
        IsNull(base)
        IsNull(find)

        If find.Length() = 0 And base.Length() = 0 Then Return True

        For Each arr In ToList(base)
            If Compare(arr, find, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function
    Public Shared Function SearchCompare(ByVal base As String(), ByVal find As String(,), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        'Special Compare/Contain
        IsNull(find)
        IsNull(base)

        If base.Length() = 0 And find.Length() = 0 Then Return True

        For Each arr In ToList(find)
            If Compare(arr, base, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function


    Public Shared Function InArraySearch(ByVal base As List(Of List(Of String())), ByVal find As String(,), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(UnEqualArgs)
        IsNull(base)
        IsNull(find)

        Dim Compare_Count = base.Count()
        Dim Match_Count = find.Length()

        If Match_Count > Compare_Count Then Return -1
        If Compare_Count = 0 And Match_Count = 0 Then Return 0
        If Match_Count = 0 Then Return -1

        For i = 0 To Compare_Count - Match_Count
            If SearchCompare(base.GetRange(i, Match_Count), find, skips, UnEqualArgs, ignoreCase, eGate) = True Then
                Return i
            End If
        Next
        Return -1
    End Function
    Public Shared Function InArraySearch(ByVal base As List(Of List(Of String())), ByVal find As List(Of String()), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(UnEqualArgs)
        IsNull(base)
        IsNull(find)

        Dim Compare_Count = base.Count()
        Dim Match_Count = find.Count()

        If Match_Count > Compare_Count Then Return -1
        If Compare_Count = 0 And Match_Count = 0 Then Return 0
        If Match_Count = 0 Then Return -1

        For i = 0 To Compare_Count - Match_Count
            If SearchCompare(base.GetRange(i, Match_Count), find, skips, UnEqualArgs, ignoreCase, eGate) = True Then
                Return i
            End If
        Next
        Return -1
    End Function
    Public Shared Function InArraySearch(ByVal base As List(Of String()), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(base)
        IsNull(find)

        Dim Compare_Count = base.Count()
        Dim Match_Count = find.Count()

        If Compare_Count < Match_Count Then Return -1
        If Compare_Count = 0 And Match_Count = 0 Then Return 0
        If Match_Count = 0 Then Return -1

        For i = 0 To Compare_Count - Match_Count
            If SearchCompare(base.GetRange(i, Match_Count), find, ignoreCase, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function


    Public Shared Function Contains(ByVal base As String(), ByVal Item As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(Item)
        IsNull(base)

        If base.Length() = 0 And IsEmpty(Item) = True Then Return True

        For Each Item_1 In base
            If Search(Item_1, Item, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function
    Public Shared Function Contains(ByVal base As List(Of String), ByVal Item As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)
        IsNull(Item)

        If base.Count() = 0 And IsEmpty(Item) = True Then Return True

        For Each Item_1 In base
            If Search(Item_1, Item, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function


    'true compare

    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() <> array_2.Count() Then Return False
        If array_1.Count() = 0 And array_2.Count() = 0 Then Return True

        For i = 0 To array_1.Count() - 1
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length() <> array_2.Count() Then Return False
        If array_1.Length() = 0 And array_2.Count() = 0 Then Return True

        For i = 0 To array_1.Length() - 1
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length <> array_2.Length Then Return False
        If array_1.Length() = 0 And array_2.Length() = 0 Then Return True

        For i = 0 To array_1.Length - 1
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() <> array_2.Length() Then Return False
        If array_1.Count() = 0 And array_2.Length() = 0 Then Return True

        For i = 0 To array_1.Count() - 1
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function


    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal skips As Integer(), ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() <> array_2.Count() Then Return False
        If array_1.Count() = 0 And array_2.Count() = 0 Then Return True

        For i = 0 To array_1.Count() - 1
            If GetIndex(skips, i, 0) = 1 Then Continue For
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal skips As Integer(), ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length() <> array_2.Count() Then Return False
        If array_1.Length() = 0 And array_2.Count() = 0 Then Return True

        For i = 0 To array_1.Length() - 1
            If GetIndex(skips, i, 0) = 1 Then Continue For
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal skips As Integer(), ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length <> array_2.Length Then Return False
        If array_1.Length() = 0 And array_2.Length() = 0 Then Return True

        For i = 0 To array_1.Length - 1
            If GetIndex(skips, i, 0) = 1 Then Continue For
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal skips As Integer(), ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() <> array_2.Length() Then Return False
        If array_1.Count() = 0 And array_2.Length() = 0 Then Return True

        For i = 0 To array_1.Count() - 1
            If GetIndex(skips, i, 0) = 1 Then Continue For
            If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then Continue For
            If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then Return False
        Next
        Return True
    End Function


    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As String(,), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Length() <> array_1.Count() Then Return False
        If array_2.Length() = 0 And array_1.Count() = 0 Then Return True

        For i = 0 To array_2.Length() - 1
            If Compare(array_1(i), ToArray(array_2, i), ignoreCase, eGate) = False Then
                Return False
            End If
        Next

        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As String(,), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Length() <> array_1.Count() Then Return False
        If array_2.Length() = 0 And array_1.Count() = 0 Then Return True

        For i = 0 To array_2.Length() - 1
            If GetIndex(skips, i, 0) <> 1 Then
                If Compare(array_1(i), ToArray(array_2, i), ignoreCase, eGate) = False Then
                    Return False
                End If
            End If
        Next

        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As String(,), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, and adds the array index to the UnEqualsArgs list, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(UnEqualArgs)
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Length() <> array_1.Count() Then Return False
        If array_2.Length() = 0 And array_1.Count() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_2.Length() - 1
            If Compare(array_1(i), ToArray(array_2, i), ignoreCase, eGate) = False Then
                If GetIndex(skips, i, 0) = 1 Then
                    UnEqualArgs.Add(i)
                Else
                    Return False
                End If
            End If
        Next

        Return True
    End Function


    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As String(), ByRef UnEqualArgs As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)
        IsNull(UnEqualArgs)

        If array_1.Length <> array_2.Length Then Return False
        If array_1.Length() = 0 And array_2.Length() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_1.Length - 1
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
                If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then
                    UnEqualArgs.Add(array_1(i))
                Else
                    Return False
                End If
            End If
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As String(), ByVal array_2 As String(), ByVal start As Integer, ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)
        IsNull(UnEqualArgs)

        If array_1.Length <> array_2.Length Then Return False
        If array_1.Length() = 0 And array_2.Length() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_1.Length - 1
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
                If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then
                    UnEqualArgs.Add(start + i)
                Else
                    Return False
                End If
            End If
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As String(), ByRef UnEqualArgs As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)
        IsNull(UnEqualArgs)

        If array_1.Count() <> array_2.Length() Then Return False
        If array_1.Count() = 0 And array_2.Length() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_1.Count() - 1
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
                If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then
                    UnEqualArgs.Add(array_1(i))
                Else
                    Return False
                End If
            End If
        Next
        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String), ByVal array_2 As String(), ByVal start As Integer, ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)
        IsNull(UnEqualArgs)

        If array_1.Count() <> array_2.Length() Then Return False
        If array_1.Count() = 0 And array_2.Length() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_1.Count() - 1
            If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                If Search(array_2(i), STOPPED, Gate.eEQUALS) = True Then Return True
                If Search(array_2(i), SKIP, Gate.eEQUALS) = True Then
                    UnEqualArgs.Add(start + i)
                Else
                    Return False
                End If
            End If
        Next
        Return True
    End Function


    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As List(Of String()), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Count() <> array_1.Count() Then Return False
        If array_2.Count() = 0 And array_1.Count() = 0 Then Return True

        For i = 0 To array_2.Count() - 1
            If Compare(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                Return False
            End If
        Next

        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As List(Of String()), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Count() <> array_1.Count() Then Return False
        If array_2.Count() = 0 And array_1.Count() = 0 Then Return True

        For i = 0 To array_2.Count() - 1
            If GetIndex(skips, i, 0) = 1 Then Continue For
            If Compare(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                Return False
            End If
        Next

        Return True
    End Function
    Public Shared Function Compare(ByVal array_1 As List(Of String()), ByVal array_2 As List(Of String()), ByVal skips As Integer(), ByRef UnEqualArgs As List(Of Integer), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        '{0, 1, 0}
        ' '1' skips it from being checked, and adds the array index to the UnEqualsArgs list, without Returning a False
        ' '0' tells the function to return a False without further checking
        IsNull(UnEqualArgs)
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Count() <> array_1.Count() Then Return False
        If array_2.Count() = 0 And array_1.Count() = 0 Then Return True
        UnEqualArgs.Clear()

        For i = 0 To array_2.Count() - 1
            If Compare(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                If GetIndex(skips, i, 0) = 1 Then
                    UnEqualArgs.Add(i)
                Else
                    Return False
                End If
            End If
        Next

        Return True
    End Function


    Public Shared Function CompareUnordered(ByVal array_1 As String(), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        'If the two arrays are equivalent but not in the same order then they are equal
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Length() = 0 And array_1.Length() = 0 Then Return True

        If array_1.Length() = array_2.Length() Then
            For Each Entry In array_1
                If Count(array_1, Entry, ignoreCase, eGate) <> Count(array_2, Entry, ignoreCase, eGate) Then
                    Return False
                End If
            Next
        Else
            Return False
        End If

        Return True
    End Function
    Public Shared Function CompareUnordered(ByVal array_1 As List(Of String), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Count() = 0 And array_1.Count() = 0 Then Return True

        'If the two arrays are equivalent but not in the same order then they are equal
        If array_1.Count() = array_2.Count() Then
            For Each Entry In array_1
                If Count(array_1, Entry, ignoreCase, eGate) <> Count(array_2, Entry, ignoreCase, eGate) Then
                    Return False
                End If
            Next
        Else
            Return False
        End If

        Return True
    End Function
    Public Shared Function CompareUnordered(ByVal array_1 As String(), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_2.Count() = 0 And array_1.Length() = 0 Then Return True

        'If the two arrays are equivalent but not in the same order then they are equal
        If array_1.Length() = array_2.Count() Then
            For Each Entry In array_1
                If Count(array_1, Entry, ignoreCase, eGate) <> Count(array_2, Entry, ignoreCase, eGate) Then
                    Return False
                End If
            Next
        Else
            Return False
        End If

        Return True
    End Function


    Public Shared Function InArray(ByVal base As String(), ByVal find As String(), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        'finds index of match of the find array in the search array
        'it can skip indicated idexes to make a match more likely
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Length()
        Dim findCount = find.Length()

        If searchCount < findCount Then Return -1
        If findCount = 0 Then Return 0

        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, (findCount + i) - 1), find, ignoreCase, skips, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function
    Public Shared Function InArray(ByVal base As List(Of String), ByVal find As String(), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        'finds index of match of the find array in the search array
        'it can skip indicated idexes to make a match more likely
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Count()
        Dim findCount = find.Length()

        If searchCount < findCount Then Return -1
        If findCount = 0 Then Return 0

        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, (findCount + i) - 1), find, ignoreCase, skips, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function
    Public Shared Function InArray(ByVal base As String(), ByVal find As List(Of String), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        'finds index of match of the find array in the search array
        'it can skip indicated idexes to make a match more likely
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Length()
        Dim findCount = find.Count()

        If searchCount < findCount Then Return -1
        If findCount = 0 Then Return 0

        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, (findCount + i) - 1), find, ignoreCase, skips, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function
    Public Shared Function InArray(ByVal base As List(Of String), ByVal find As List(Of String), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        'finds index of match of the find array in the search array
        'it can skip indicated idexes to make a match more likely
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Count()
        Dim findCount = find.Count()

        If searchCount < findCount Then Return -1
        If findCount = 0 Then Return 0

        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, (findCount + i) - 1), find, ignoreCase, skips, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function
    Public Shared Function InArray(ByVal base As List(Of String()), ByVal find As List(Of String()), ByVal skips As Integer(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        'finds index of match of the find array in the search array
        'it can skip indicated idexes to make a match more likely
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Count()
        Dim findCount = find.Count()

        If searchCount < findCount Then Return -1
        If findCount = 0 Then Return 0

        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, (findCount + i) - 1), find, skips, ignoreCase, eGate) = True Then
                Return i
            End If
        Next

        Return -1
    End Function


    Public Shared Function Contains(ByVal array_1 As String(), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length() < array_2.Length Then
            For Each Item_1 In array_1
                If Contains(array_2, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        Else
            For Each Item_1 In array_2
                If Contains(array_1, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        End If

        Return True
    End Function
    Public Shared Function Contains(ByVal array_1 As List(Of String), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() < array_2.Count Then
            For Each Item_1 In array_1
                If Contains(array_2, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        Else
            For Each Item_1 In array_2
                If Contains(array_1, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        End If

        Return True
    End Function
    Public Shared Function Contains(ByVal array_1 As List(Of String), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() < array_2.Length() Then
            For Each Item_1 In array_1
                If Contains(array_2, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        Else
            For Each Item_1 In array_2
                If Contains(array_1, Item_1, ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        End If

        Return True
    End Function


    '************************************************************
    '************************************************************


    Public Shared Function Count(ByVal array_1 As String(), ByVal Item As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(array_1)
        IsNull(Item)

        Dim Count1 As Integer = 0

        For Each Entry In array_1
            If Search(Entry, Item, ignoreCase, eGate) = True Then
                Count1 += 1
            End If
        Next
        Return Count1
    End Function
    Public Shared Function Count(ByVal array_1 As List(Of String), ByVal Item As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        Dim Count1 As Integer = 0
        IsNull(array_1)
        IsNull(Item)

        For Each Entry In array_1
            If Search(Entry, Item, ignoreCase, eGate) = True Then
                Count1 += 1
            End If
        Next
        Return Count1
    End Function


    Public Shared Function FirstCompare(ByVal array_1 As String(), ByVal array_2 As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Length() >= array_2.Length() Then
            For i = 0 To array_2.Length() - 1

                If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        Else
            For i = 0 To array_1.Length() - 1
                If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        End If

        Return True
    End Function
    Public Shared Function FirstCompare(ByVal array_1 As List(Of String), ByVal array_2 As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(array_1)
        IsNull(array_2)

        If array_1.Count() >= array_2.Count() Then
            For i = 0 To array_2.Count() - 1

                If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        Else
            For i = 0 To array_1.Count() - 1
                If Search(array_1(i), array_2(i), ignoreCase, eGate) = False Then
                    Return False
                End If
            Next
        End If

        Return True
    End Function

    Public Shared Function RemoveLast(ByVal Input As String()) As String()
        IsNull(Input)

        If Input.Length() = 0 Then Return Input
        If Input.Length() = 1 Then Return Nothing

        Dim newArray(Input.Length() - 2) As String

        For i = 0 To newArray.Length - 1
            newArray(i) = Input(i)
        Next

        Return newArray
    End Function
    Public Shared Function RemoveLast(ByVal Input As List(Of String)) As List(Of String)
        IsNull(Input)

        If Input.Count() = 0 Then Return Input
        If Input.Count() = 1 Then Return New List(Of String)
        Input.RemoveAt(Input.Count() - 1)
        Return Input
    End Function

    Public Shared Function RemoveAt(ByVal Input As String(), ByVal index As Integer) As String()
        IsNull(Input)

        If Input.Length() = 0 Then Return Input
        Dim newArray(Input.Length() - 2) As String

        Dim i = 0
        Dim j = 0
        For Each entry In Input

            If index <> j Then
                newArray(i) = entry
                i += 1
            End If

            j += 1
        Next

        Return newArray
    End Function
    Public Shared Function RemoveAt(ByVal Input As List(Of String), ByVal index As Integer) As List(Of String)
        IsNull(Input)
        If Input.Count() = 0 Then Return Input
        Input.RemoveAt(index)
        Return Input
    End Function

    Public Shared Function GetRange(ByVal Items As String(), ByVal start As Integer) As String()
        IsNull(Items)

        Dim intEnd = Items.Length()
        If start < 0 Then start = 0
        If start >= intEnd Then Return {}
        Dim cnt = 0

        Dim newArray(intEnd - start - 1) As String

        For i = 0 To intEnd - 1
            If i >= start Then
                newArray(cnt) = Items(i)
                cnt += 1
            End If
        Next

        Return newArray
    End Function
    Public Shared Function GetRange(ByVal Items As List(Of String), ByVal start As Integer) As List(Of String)
        IsNull(Items)

        Dim intEnd = Items.Count()
        If start < 0 Then start = 0
        If start >= intEnd Then Return New List(Of String)
        Dim cnt = 0

        Dim newList As New List(Of String)

        For i = 0 To intEnd - 1
            If i >= start Then
                newList.Add(Items(i))
                cnt += 1
            End If
        Next

        Return newList
    End Function
    Public Shared Function GetRange(ByVal Items As List(Of String()), ByVal start As Integer) As List(Of String())
        IsNull(Items)

        Dim intEnd = Items.Count()
        If start < 0 Then start = 0
        If start >= intEnd Then Return New List(Of String())
        Dim cnt = 0

        Dim newList As New List(Of String())

        For i = 0 To intEnd - 1
            If i >= start Then
                newList.Add(Items(i))
                cnt += 1
            End If
        Next

        Return newList
    End Function

    Public Shared Function GetRange(ByVal Items As String(), ByVal start As Integer, ByVal intEnd As Integer) As String()
        IsNull(Items)

        If intEnd < start Then Return {}
        If start < 0 Then start = 0
        If start >= Items.Length Then Return {}
        Dim cnt = 0

        Dim newArray(intEnd - start) As String

        For i = 0 To Items.Length() - 1
            If i >= start And i <= intEnd Then
                newArray(cnt) = Items(i)
                cnt += 1
            End If
        Next

        Return newArray
    End Function
    Public Shared Function GetRange(ByVal Items As List(Of String), ByVal start As Integer, ByVal intEnd As Integer) As List(Of String)
        IsNull(Items)

        If intEnd < start Then Return New List(Of String)
        If start < 0 Then start = 0
        If start >= Items.Count() Then Return New List(Of String)
        Dim cnt = 0

        Dim newList As New List(Of String)

        For i = 0 To Items.Count() - 1
            If i >= start And i <= intEnd Then
                newList.Add(Items(i))
                cnt += 1
            End If
        Next

        Return newList
    End Function
    Public Shared Function GetRange(ByVal Items As List(Of String()), ByVal start As Integer, ByVal intEnd As Integer) As List(Of String())
        IsNull(Items)

        If intEnd < start Then Return New List(Of String())
        If start < 0 Then start = 0
        If start >= Items.Count() Then Return New List(Of String())
        Dim cnt = 0

        Dim newList As New List(Of String())

        For i = 0 To Items.Count() - 1
            If i >= start And i <= intEnd Then
                newList.Add(Items(i))
                cnt += 1
            End If
        Next

        Return newList
    End Function


    Public Shared Function SetRange(ByVal Items As String(), ByVal start As Integer, ByVal Values As String()) As String()
        IsNull(Items)

        If Items.Length <= start Then Return {}
        If start < 0 Then start = 0
        Dim cnt = 0

        Dim newArray(Items.Length) As String

        For i = 0 To Items.Length() - 1
            If i >= start And cnt < Values.Length Then
                newArray(i) = Values(cnt)
                cnt += 1
            Else
                newArray(i) = Items(i)
            End If
        Next

        Return newArray
    End Function
    Public Shared Function SetRange(ByVal Items As String(), ByVal start As Integer, ByVal Values As List(Of String)) As String()
        IsNull(Items)

        If Items.Length <= start Then Return {}
        If start < 0 Then start = 0
        Dim cnt = 0

        Dim newArray(Items.Length) As String

        For i = 0 To Items.Length() - 1
            If i >= start And cnt < Values.Count Then
                newArray(i) = Values(cnt)
                cnt += 1
            Else
                newArray(i) = Items(i)
            End If
        Next

        Return newArray
    End Function
    Public Shared Function SetRange(ByVal Items As List(Of String), ByVal start As Integer, ByVal Values As List(Of String)) As List(Of String)

        If Items.Count() <= start Then Return New List(Of String)
        If start < 0 Then start = 0
        Dim cnt = 0

        Dim newArray As New List(Of String)

        For i = 0 To Items.Count() - 1
            If i >= start And cnt < Values.Count Then
                newArray.Add(Values(cnt))
                cnt += 1
            Else
                newArray.Add(Items(i))
            End If
        Next

        Return newArray
    End Function
    Public Shared Function SetRange(ByVal Items As List(Of String), ByVal start As Integer, ByVal Values As String()) As List(Of String)
        IsNull(Items)

        If Items.Count() <= start Then Return New List(Of String)
        If start < 0 Then start = 0
        Dim cnt = 0

        Dim newArray As New List(Of String)

        For i = 0 To Items.Count() - 1
            If i >= start And cnt < Values.Length Then
                newArray.Add(Values(cnt))
                cnt += 1
            Else
                newArray.Add(Items(i))
            End If
        Next

        Return newArray
    End Function
    Public Shared Function SetRange(ByVal Items As List(Of String()), ByVal start As Integer, ByVal Values As List(Of String())) As List(Of String())

        If Items.Count() <= start Then Return New List(Of String())
        If start < 0 Then start = 0
        Dim cnt = 0

        Dim newArray As New List(Of String())

        For i = 0 To Items.Count() - 1
            If i >= start And cnt < Values.Count Then
                newArray.Add(Values(cnt))
                cnt += 1
            Else
                newArray.Add(Items(i))
            End If
        Next

        Return newArray
    End Function


    Public Shared Function GetLast(ByVal Input As String()) As String
        IsNull(Input)
        If Input.Count() = 0 Then Return ""
        Return Input(Input.Length - 1)
    End Function
    Public Shared Function SetLast(ByVal Input As String(), ByVal value As String) As String()
        IsNull(Input)

        If Input.Length = 0 Then Return {value}
        Input(Input.Length - 1) = value
        Return Input
    End Function

    Public Shared Function GetLast(ByVal Input As List(Of String)) As String
        IsNull(Input)

        If Input.Count() = 0 Then Return ""
        Return Input(Input.Count - 1)
    End Function
    Public Shared Function SetLast(ByVal Input As List(Of String), ByVal value As String) As List(Of String)
        IsNull(Input)

        If Input.Count = 0 Then Return New List(Of String)({value})
        Input(Input.Count - 1) = value
        Return Input
    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of String()), ByVal index As Integer) As String()
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return {}
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As List(Of String()), ByVal index As Integer, ByVal Defaults As String()) As String()
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As List(Of String()), ByVal index As Integer, ByVal value As String()) As List(Of String())
        IsNull(array_1)

        If index < 0 Then index = 0
        Dim LastIndex = array_1.Count() - 1

        If index > LastIndex Then
            For i = LastIndex To index
                array_1.Add({Nothing})
            Next
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As Object(), ByVal index As Integer) As Object
        IsNull(array_1)

        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As Object(), ByVal index As Integer, ByVal Defaults As Object) As Object
        IsNull(array_1)

        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As Object(), ByVal index As Integer, ByVal value As Object) As Object()
        IsNull(array_1)
        If index < 0 Then index = 0

        If index > array_1.GetUpperBound(0) Then
            ReDim Preserve array_1(index)
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As String(), ByVal index As Integer) As String
        IsNull(array_1)

        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As String(), ByVal index As Integer, ByVal Defaults As String) As String
        IsNull(array_1)

        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As String(), ByVal index As Integer, ByVal value As String) As String()
        IsNull(array_1)
        If index < 0 Then index = 0

        If index > array_1.GetUpperBound(0) Then
            ReDim Preserve array_1(index)
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As String(,), ByVal indexX As Integer, ByVal indexY As Integer) As String
        IsNull(array_1)

        If indexX < 0 Then indexX = 0
        If indexY < 0 Then indexY = 0

        Dim LastIndexY = array_1.GetUpperBound(0)
        Dim LastIndexX = array_1.GetUpperBound(1)

        If indexY < LastIndexY And indexX < LastIndexX Then
            Return array_1(indexY, indexX)
        End If

        Return ""
    End Function
    Public Shared Function GetIndex(ByVal array_1 As String(,), ByVal indexX As Integer, ByVal indexY As Integer, ByVal Defaults As String) As String
        IsNull(array_1)

        If indexX < 0 Then indexX = 0
        If indexY < 0 Then indexY = 0

        Dim LastIndexY = array_1.GetUpperBound(0)
        Dim LastIndexX = array_1.GetUpperBound(1)

        If indexY < LastIndexY And indexX < LastIndexX Then
            Return array_1(indexY, indexX)
        End If

        Return Defaults
    End Function
    Public Shared Function SetIndex(ByVal array_1 As String(,), ByVal indexY As Integer, ByVal indexX As Integer, ByVal value As String) As String(,)
        IsNull(array_1)

        If indexX < 0 Then indexX = 0
        If indexY < 0 Then indexY = 0

        Dim LastIndexY = array_1.GetUpperBound(0)
        Dim LastIndexX = array_1.GetUpperBound(1)

        If indexY <= LastIndexY And indexX > LastIndexX Then
            ReDim Preserve array_1(LastIndexY, indexX)
        ElseIf indexY > LastIndexY And indexX <= LastIndexX Then
            ReDim Preserve array_1(indexY, LastIndexX)
        ElseIf indexY > LastIndexY And indexX > LastIndexX Then
            ReDim Preserve array_1(indexY, indexX)
        End If

        array_1(indexY, indexX) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As Integer(), ByVal index As Integer) As Integer
        IsNull(array_1)
        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return 0
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As Integer(), ByVal index As Integer, ByVal Defaults As Integer) As Integer
        IsNull(array_1)
        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As Integer(), ByVal index As Integer, ByVal value As String) As Integer()
        IsNull(array_1)
        If index < 0 Then index = 0

        If index > array_1.GetUpperBound(0) Then
            ReDim Preserve array_1(index)
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of String), ByVal index As Integer) As String
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As List(Of String), ByVal index As Integer, ByVal Defaults As String) As String
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As List(Of String), ByVal index As Integer, ByVal value As String) As List(Of String)
        IsNull(array_1)

        If index < 0 Then index = 0
        Dim LastIndex = array_1.Count() - 1

        If index > LastIndex Then
            For i = LastIndex To index
                array_1.Add(Nothing)
            Next
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String)), ByVal indexY As Integer, ByVal indexX As Integer) As String
        IsNull(array_1)

        If indexY < array_1.Count() And indexY > -1 Then
            Return GetIndex(array_1(indexY), indexX, "")
        Else
            Return ""
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String)), ByVal indexY As Integer, ByVal indexX As Integer, ByVal Defaults As String) As String
        IsNull(array_1)

        If indexY < array_1.Count() And indexY > -1 Then
            Return GetIndex(array_1(indexY), indexX, Defaults)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As List(Of List(Of String)), ByVal indexY As Integer, ByVal indexX As Integer, ByVal value As String) As List(Of List(Of String))
        IsNull(array_1)

        If indexX < 0 Then indexX = 0
        If indexY < 0 Then indexY = 0

        Dim LastIndex = array_1.Count() - 1

        If indexY > LastIndex Then
            For i = LastIndex To indexY
                array_1.Add(New List(Of String))
            Next
        End If

        array_1(indexY) = SetIndex(array_1(indexY), indexX, value)
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String)), ByVal index As Integer) As List(Of String)
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return New List(Of String)
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String)), ByVal index As Integer, ByVal Defaults As List(Of String)) As List(Of String)
        IsNull(array_1)

        If index < array_1.Count() And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As List(Of List(Of String)), ByVal index As Integer, ByVal value As List(Of String)) As List(Of List(Of String))
        IsNull(array_1)

        If index < 0 Then index = 0

        Dim LastIndex = array_1.Count() - 1

        If index > LastIndex Then
            For i = LastIndex To index
                array_1.Add(New List(Of String))
            Next
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As Boolean(), ByVal index As Integer) As Boolean
        IsNull(array_1)
        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return False
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As Boolean(), ByVal index As Integer, ByVal Defaults As Boolean) As Boolean
        IsNull(array_1)
        If index < array_1.Length And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As Boolean(), ByVal index As Integer, ByVal value As Boolean) As Boolean()
        IsNull(array_1)
        If index < 0 Then index = 0

        If index > array_1.GetUpperBound(0) Then
            ReDim Preserve array_1(index)
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String())), ByVal index As Integer) As List(Of String())
        IsNull(array_1)
        If index < array_1.Count And index > -1 Then
            Return array_1(index)
        Else
            Return New List(Of String())
        End If
    End Function
    Public Shared Function GetIndex(ByVal array_1 As List(Of List(Of String())), ByVal index As Integer, ByVal Defaults As List(Of String())) As Object
        IsNull(array_1)
        If index < array_1.Count And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function
    Public Shared Function SetIndex(ByVal array_1 As List(Of List(Of String())), ByVal index As Integer, ByVal value As List(Of String())) As Object
        IsNull(array_1)

        If index < 0 Then index = 0
        Dim LastIndex = array_1.Count() - 1

        If index > LastIndex Then
            For i = LastIndex To index
                array_1.Add(Nothing)
            Next
        End If

        array_1(index) = value
        Return array_1
    End Function

    Public Shared Function Insert(ByVal Items As List(Of String), ByVal Index As Integer, ByVal entry As String) As List(Of String)
        IsNull(Items)

        If Index < 0 Then Index = 0
        Dim arrCount = Items.Count()

        If Index <= arrCount Then
            Items.Insert(Index, entry)
        Else
            For i = arrCount To Index - 1
                Items.Add("")
            Next

            Items.Insert(Index, entry)
        End If

        Return Items
    End Function
    Public Shared Function InsertRange(ByVal Items As List(Of String), ByVal Index As Integer, ByVal values As List(Of String)) As List(Of String)
        IsNull(Items)
        IsNull(values)

        If Index < 0 Then Index = 0
        Dim arrCount = Items.Count()

        If Index <= arrCount Then
            Items.InsertRange(Index, values)
        Else
            For i = arrCount To Index - 1
                Items.Add("")
            Next
            Items.InsertRange(Index, values)
        End If

        Return Items
    End Function

    Public Shared Function IndexOf(ByVal base As String(), ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(base)
        IsNull(find)

        For i = 0 To base.Length - 1
            If Search(base(i), find, ignoreCase, eGate) = True Then Return i
        Next

        Return -1
    End Function
    Public Shared Function IndexOf(ByVal base As List(Of String), ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
        IsNull(base)
        IsNull(find)

        For i = 0 To base.Count - 1
            If Search(base(i), find, ignoreCase, eGate) = True Then Return i
        Next

        Return -1
    End Function

    Public Shared Function GetArgsInArray(ByVal base As String(), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String)
        'USE * to get arguments
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Length()
        Dim findCount = find.Length()
        Dim newList As New List(Of String)

        If searchCount < findCount Then Return newList
        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, findCount), find, newList, ignoreCase, eGate) = True Then
                Return newList
            End If
        Next

        Return newList
    End Function
    Public Shared Function GetArgsInArray(ByVal base As List(Of String), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String)
        'USE * to get arguments
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Count()
        Dim findCount = find.Length()
        Dim newList As New List(Of String)

        If searchCount < findCount Then Return newList
        For i = 0 To searchCount - findCount
            If Compare(base.GetRange(i, findCount), find, newList, ignoreCase, eGate) = True Then
                Return newList
            End If
        Next

        Return newList
    End Function
    Public Shared Function GetIndexOfArrayArgs(ByVal base As String(), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of Integer)
        'USE * to get arguments
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Length()
        Dim findCount = find.Length()
        Dim newList As New List(Of Integer)

        If searchCount < findCount Then Return newList
        For i = 0 To searchCount - findCount
            If Compare(GetRange(base, i, findCount), find, i, newList, ignoreCase, eGate) = True Then
                Return newList
            End If
        Next

        Return newList
    End Function
    Public Shared Function GetIndexOfArrayArgs(ByVal base As List(Of String), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of Integer)
        'USE * to get arguments
        IsNull(base)
        IsNull(find)

        Dim searchCount = base.Count()
        Dim findCount = find.Length()
        Dim newList As New List(Of Integer)

        If searchCount < findCount Then Return newList
        For i = 0 To searchCount - findCount
            If Compare(base.GetRange(i, findCount), find, i, newList, ignoreCase, eGate) = True Then
                Return newList
            End If
        Next

        Return newList
    End Function

    Public Shared Function Remove(ByVal Items As List(Of String), ByVal Item As String, ByVal removeIndex As Boolean) As List(Of String)
        IsNull(Items)
        IsNull(Item)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If removeIndex = True Then
                If Search(Entry, Item, Gate.eEQUALS) = False Then
                    newList.Add(Entry)
                End If
            Else
                newList.Add(Remove(Entry, Item))
            End If
        Next
        Return newList
    End Function
    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As String(), ByVal removeIndex As Boolean) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If removeIndex = True Then
                If Search(Entry, removeItems, Gate.eEQUALS) = False Then
                    newList.Add(Entry)
                End If
            Else
                newList.Add(Remove(Entry, removeItems))
            End If
        Next
        Return newList
    End Function
    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As List(Of String), ByVal removeIndex As Boolean) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If removeIndex = True Then
                If Search(Entry, removeItems, Gate.eEQUALS) = False Then
                    newList.Add(Entry)
                End If
            Else
                newList.Add(Remove(Entry, removeItems))
            End If
        Next
        Return newList
    End Function

    Public Shared Function Remove(ByVal Items As List(Of String), ByVal Item As String) As List(Of String)
        IsNull(Items)
        IsNull(Item)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, Item, Gate.eEQUALS) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function
    Public Shared Function Remove(ByVal Items As List(Of String), ByVal Item As String, ByVal eGate As Gate) As List(Of String)
        IsNull(Items)
        IsNull(Item)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, Item, eGate) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function

    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As String()) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, removeItems, Gate.eEQUALS) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function
    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As List(Of String)) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, removeItems, Gate.eEQUALS) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function

    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As String(), ByVal eGate As Gate) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, removeItems, eGate) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function
    Public Shared Function Remove(ByVal Items As List(Of String), ByVal removeItems As List(Of String), ByVal eGate As Gate) As List(Of String)
        IsNull(Items)
        IsNull(removeItems)

        Dim newList As New List(Of String)
        For Each Entry In Items
            If Search(Entry, removeItems, eGate) = False Then
                newList.Add(Entry)
            End If
        Next
        Return newList
    End Function


    Public Shared Function CleanArray(ByVal Items As List(Of String)) As List(Of String)
        IsNull(Items)
        'Removes empty strings

        Dim newList As New List(Of String)
        For Each Item In Items
            If Trim(Item) > "" Then newList.Add(Trim(Item))
        Next
        Return newList
    End Function
    Public Shared Function CleanArray(ByVal Items As String()) As String()
        'Removes empty strings
        Return CleanArray(New List(Of String)(Items)).ToArray
    End Function


    Public Shared Function RemoveDuplicates(ByVal Items As List(Of String)) As List(Of String)
        'Removes Duplicate entries 
        IsNull(Items)

        Dim newItems As New List(Of String)

        For Each Item In Items
            Remove(Items, Item, False)
            newItems.Add(Item)
        Next

        Return newItems
    End Function


#End Region


#Region "Files And Folders"

    Public Shared Function GetExtension(ByVal file As String) As String
        IsNull(file)
        Return System.IO.Path.GetExtension(file)
    End Function

    Public Shared Function RemoveExtension(ByVal file As String) As String
        IsNull(file)
        Return Remove(file, System.IO.Path.GetExtension(file))
    End Function

    Public Shared Function ReplaceExtension(ByVal file As String, ByVal extension As String) As String
        Return RemoveExtension(file) & extension
    End Function

    Public Shared Sub FileDelete(ByVal path As String)
        IsNull(path)
        IO.File.Delete(path)
    End Sub

    Public Shared Sub FileDeleteAll(ByVal path As String)
        IsNull(path)
        For Each file In GetAllFiles(path)
            IO.File.Delete(file)
        Next

    End Sub

    Public Shared Sub FileCopy(ByVal path As String, ByVal CopyTo As String)
        IsNull(path)
        IsNull(CopyTo)
        IO.File.Copy(Trim(path), Trim(CopyTo), True)
    End Sub

    Public Shared Sub FileRename(ByVal path As String, ByVal name As String)
        IsNull(path)
        IsNull(name)
        FileIO.FileSystem.RenameFile(Trim(path), Trim(name))
    End Sub

    Public Shared Function GetFileName(ByVal path As String, ByVal no_extension As Boolean) As String
        IsNull(path)
        If no_extension = True Then
            Return RemoveExtension(FileIO.FileSystem.GetName(Trim(path)))
        Else
            Return FileIO.FileSystem.GetName(Trim(path))
        End If
    End Function

    Public Shared Function GetFolderName(ByVal path As String) As String
        IsNull(path)
        Return New IO.DirectoryInfo(Trim(path)).Name()
    End Function

    Public Shared Function GetPathRoot(ByVal path As String) As String
        IsNull(path)
        Return New IO.DirectoryInfo(Trim(path)).Root.FullName()
    End Function

    Public Shared Function IsPathRoot(ByVal path As String) As Boolean
        IsNull(path)
        Return Search(GetPathRoot(path), Trim(path), Gate.eEQUALS)
    End Function

    Public Shared Function GetParentFolder(ByVal path As String) As String
        IsNull(path)

        Dim info As New IO.DirectoryInfo(Trim(path))
        If Search(info.Root.FullName, path, Gate.eEQUALS) = True Then
            Return info.Root.FullName
        End If

        Return FileIO.FileSystem.GetParentPath(Trim(path))
    End Function

    Public Shared Function GetAllFiles(ByVal path As String) As List(Of String)
        IsNull(path)
        Dim col As New List(Of String)
        col.AddRange(IO.Directory.GetFiles(Trim(path)))

        Return col
    End Function
    Public Shared Function GetAllFiles(ByVal path As String, ByVal filter As String, ByVal eGate As Gate) As List(Of String)
        IsNull(path)
        Dim col As New List(Of String)

        For Each file In IO.Directory.GetFiles(Trim(path))
            If Search(file, filter, eGate) = True Then
                col.Add(file)
            End If
        Next

        Return col
    End Function
    Public Shared Function GetAllFiles(ByVal path As String, ByVal filter As String(), ByVal eGate As Gate) As List(Of String)
        IsNull(path)
        Dim col As New List(Of String)

        For Each file In IO.Directory.GetFiles(Trim(path))
            If Search(file, filter, eGate) = True Then
                col.Add(file)
            End If
        Next

        Return col
    End Function

    Public Shared Function FileExists(ByVal path As String) As Boolean
        IsNull(path)
        Return IO.File.Exists(Trim(path))
    End Function

    Public Shared Function FolderExists(ByVal path As String) As Boolean
        IsNull(path)
        Return IO.Directory.Exists(Trim(path))
    End Function

    Public Shared Function GetAllFolders(ByVal path As String, Optional ByVal subfolders As Boolean = True) As List(Of String)
        IsNull(path)
        Dim col As New List(Of String)
        If subfolders = True Then
            col.AddRange(IO.Directory.EnumerateDirectories(Trim(path), "*", System.IO.SearchOption.AllDirectories))
        Else
            col.AddRange(IO.Directory.GetDirectories(Trim(path)))
        End If

        Return col
    End Function

    Public Shared Sub FolderDelete(ByVal path As String)
        IsNull(path)
        IO.Directory.Delete(Trim(path), True)
    End Sub

    Public Shared Sub FolderCreate(ByVal path As String)
        IsNull(path)
        System.IO.Directory.CreateDirectory(Trim(path))
    End Sub

    Public Shared Sub FolderCopy(ByVal path As String, ByVal newpath As String)
        IsNull(path)
        IsNull(newpath)
        FileIO.FileSystem.CopyDirectory(Trim(path), Trim(newpath), True)
    End Sub

    Public Shared Sub FolderRename(ByVal path As String, ByVal name As String)
        IsNull(path)
        IsNull(name)
        FileIO.FileSystem.RenameDirectory(Trim(path), Trim(name))
    End Sub

    Public Shared Function CreateTempDirectory(Optional prefix As String = "temp") As String
        While True
            Dim folder As String = Path({IO.Path.GetTempPath(), prefix, RandKey(10)})
            If FolderExists(folder) = False Then
                FolderCreate(folder)
                Return folder
            End If
        End While

        Return ""
    End Function

    Public Shared Function GetTempDirectory() As String
        Return IO.Path.GetTempPath()
    End Function

    Public Shared Function Path(ByVal patharray As String()) As String
        Return IO.Path.Combine(patharray)
    End Function

    Private Shared sound As New System.Media.SoundPlayer()
    Public Shared Function PlaySound(ByVal wavFile As String) As String
        IsNull(wavFile)

        Try
            sound.SoundLocation() = wavFile
            sound.Play()
        Catch ex As Exception
            Return Debug.Error_Message(ex)
        End Try

        Return ""
    End Function

    Public Shared Function PlaySound(ByVal msWAV As System.IO.MemoryStream) As String
        Try
            msWAV.Position() = 0
            sound.Stream() = msWAV
            sound.Play()
        Catch ex As Exception
            Return Debug.Error_Message(ex)
        End Try

        Return ""
    End Function

    Public Shared Sub StopSound()
        Try
            sound.Stop()
        Catch : End Try
    End Sub

    Public Shared Sub Beep(ByVal frequency As Integer, ByVal duration As Integer)
        Console.Beep(frequency, duration)
    End Sub

    Public Shared Sub Beep()
        Console.Beep()
    End Sub

#End Region


#Region "Gates"

    Public Shared Function GetGate(ByVal sGate As String, ByVal defaults As Gate) As Gate
        IsNull(sGate)
        sGate = TrimLow(sGate)

        If IsEmpty(sGate) = True Then Return defaults

        Select Case sGate
            Case "and"
                Return Gate.eAND
            Case "end"
                Return Gate.eEND
            Case "equals"
                Return Gate.eEQUALS
            Case "nand"
                Return Gate.eNAND
            Case "nor"
                Return Gate.eNOR
            Case "not"
                Return Gate.eNOT
            Case "or"
                Return Gate.eOR
            Case "search"
                Return Gate.eSEARCH
            Case "seq"
                Return Gate.eSEQ
            Case "start"
                Return Gate.eSTART
            Case "xnor"
                Return Gate.eXNOR
            Case "xor"
                Return Gate.eXOR
            Case Else
                Return defaults
        End Select

    End Function

    Public Shared Function Search(ByVal text As String, ByVal find As String) As Boolean
        Return Search(text, find, True, Gate.eSEARCH)
    End Function
    Public Shared Function Search(ByVal text As String, ByVal find As String, ByVal eGate As Gate) As Boolean
        Return Search(text, find, True, eGate)
    End Function

    Public Shared Function Search(ByVal text As String, ByVal find As String(), ByVal eGate As Gate) As Boolean
        Return Search(text, find, True, eGate)
    End Function
    Public Shared Function Search(ByVal text As String, ByVal find As List(Of String), ByVal eGate As Gate) As Boolean
        Return Search(text, find, True, eGate)
    End Function

    Public Shared Function Search(ByVal base As String(), ByVal find As String, ByVal eGate As Gate) As Boolean
        Return Search(base, find, True, eGate)
    End Function
    Public Shared Function Search(ByVal base As List(Of String), ByVal find As String, ByVal eGate As Gate) As Boolean
        Return Search(base, find, True, eGate)
    End Function

    Public Shared Function Search(ByVal base As List(Of String), ByVal find As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(base, find, ignoreCase, eGate)
    End Function
    Public Shared Function Search(ByVal base As String(), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(base, find, ignoreCase, eGate)
    End Function
    Public Shared Function Search(ByVal base As List(Of String), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(base, find, ignoreCase, eGate)
    End Function
    Public Shared Function Search(ByVal base As String(), ByVal find As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(base, find, ignoreCase, eGate)
    End Function

    Public Shared Function SearchObject(ByVal base As String(), ByVal find As Object, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

        If TypeOf find Is List(Of String()) Then
            Dim newList = TryCast(find, List(Of String()))

            For Each item In newList
                If LogicGate(base, item, ignoreCase, eGate) = True Then
                    Return True
                End If
            Next

        ElseIf TypeOf find Is Object() Then
            Dim newList = TryCast(find, Object())

            For Each item In newList
                Dim newString = TryCast(item, String())
                If LogicGate(base, newString, ignoreCase, eGate) = True Then
                    Return True
                End If
            Next
        End If

        Return False
    End Function


    Public Shared Function Search(ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean) As Boolean
        Return LogicGate(text, find, ignoreCase, Gate.eSEARCH)
    End Function
    Public Shared Function Search(ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(text, find, ignoreCase, eGate)
    End Function

    Public Shared Function Search(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(text, find, ignoreCase, eGate)
    End Function
    Public Shared Function Search(ByVal text As String, ByVal find As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Return LogicGate(text, find.ToArray, ignoreCase, eGate)
    End Function

    Public Shared Function Search(ByVal base As String(), ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)

        For Each entry In base
            If Search(entry, find, ignoreCase, eGate) = True Then
                Return True
            End If
        Next
        Return False
    End Function
    Public Shared Function Search(ByVal base As List(Of String), ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)

        For Each entry In base
            If Search(entry, find, ignoreCase, eGate) = True Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Shared Function LogicGate(ByVal base As List(Of String), ByVal find As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return Compare(base, find, ignoreCase, Gate.eEQUALS)
            Case Gate.eSTART
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eEND
                base.Reverse()
                find.Reverse()

                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eSEARCH, Gate.eSEQ
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eNOT
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return False
                Else
                    Return True
                End If
            Case Else
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
        End Select

    End Function
    Public Shared Function LogicGate(ByVal base As String(), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return Compare(base, find, ignoreCase, Gate.eEQUALS)
            Case Gate.eSTART
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eEND
                base.Reverse()
                find.Reverse()

                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eSEARCH, Gate.eSEQ
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eNOT
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return False
                Else
                    Return True
                End If
            Case Else
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
        End Select

    End Function
    Public Shared Function LogicGate(ByVal base As List(Of String), ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return Compare(base, find, ignoreCase, Gate.eEQUALS)
            Case Gate.eSTART
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eEND
                base.Reverse()
                find.Reverse()

                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eSEARCH, Gate.eSEQ
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eNOT
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return False
                Else
                    Return True
                End If
            Case Else
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
        End Select

    End Function
    Public Shared Function LogicGate(ByVal base As String(), ByVal find As List(Of String), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(base)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return Compare(base, find, ignoreCase, Gate.eEQUALS)
            Case Gate.eSTART
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eEND
                base.Reverse()
                find.Reverse()

                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) = 0 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eSEARCH, Gate.eSEQ
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
            Case Gate.eNOT
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return False
                Else
                    Return True
                End If
            Case Else
                If InArray(base, find, Nothing, ignoreCase, Gate.eEQUALS) > -1 Then
                    Return True
                Else
                    Return False
                End If
        End Select

    End Function

    Public Shared Function LogicGate(ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(text)
        IsNull(find)

        Select Case eGate
            Case Gate.eNOT
                Return LikeGate(text, "*" & find & "*", ignoreCase) = False
            Case Gate.eSEARCH
                Return LikeGate(text, "*" & find & "*", ignoreCase)
            Case Gate.eEQUALS
                Return LikeGate(text, find, ignoreCase)
            Case Gate.eSTART
                Return LikeGate(text, find & "*", ignoreCase)
            Case Gate.eEND
                Return LikeGate(text, "*" & find, ignoreCase)
            Case Else
                Return False
        End Select
    End Function

    Public Shared Function LogicGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate, ByRef OrReturn As String) As Boolean
        IsNull(text)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return ORGate(text, find, OrReturn, ignoreCase, False)
            Case Gate.eOR, Gate.eSEARCH
                Return ORGate(text, find, OrReturn, ignoreCase, True)
            Case Gate.eNOR
                Return ORGate(text, find, OrReturn, ignoreCase) = False
            Case Gate.eAND
                Return ANDGate(text, find, ignoreCase, True)
            Case Gate.eNAND
                Return ANDGate(text, find, ignoreCase, True) = False
            Case Gate.eXOR
                Return XORGate(text, find, ignoreCase)
            Case Gate.eXNOR
                Return XORGate(text, find, ignoreCase) = False
            Case Gate.eSEQ
                Return SEQGate(text, find, ignoreCase)
            Case Else
                Return False
        End Select

    End Function
    Public Shared Function LogicGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        IsNull(text)
        IsNull(find)

        Select Case eGate
            Case Gate.eEQUALS
                Return ORGate(text, find, ignoreCase, False)
            Case Gate.eOR, Gate.eSEARCH
                Return ORGate(text, find, ignoreCase, True)
            Case Gate.eNOR
                Return ORGate(text, find, ignoreCase, True) = False
            Case Gate.eAND
                Return ANDGate(text, find, ignoreCase, True)
            Case Gate.eNAND
                Return ANDGate(text, find, ignoreCase, True) = False
            Case Gate.eXOR
                Return XORGate(text, find, ignoreCase)
            Case Gate.eXNOR
                Return XORGate(text, find, ignoreCase) = False
            Case Gate.eSEQ
                Return SEQGate(text, find, ignoreCase)
            Case Else
                Return False
        End Select

    End Function


    Private Shared Function LikeGate(ByVal text As String, ByVal find As String, ByVal ignoreCase As Boolean) As Boolean
        'If String is found Then return True
        IsNull(text)
        IsNull(find)

        'the asterisk will always be in use  

        If find.EndsWith("/s") = True Or find.EndsWith("/s*") Then
            find = find.Replace("/s", "")
        Else
            find = find.Replace("[", "<91>")
            find = find.Replace("?", "<63>")
            find = find.Replace("#", "<35>")

            text = text.Replace("[", "<91>")
            text = text.Replace("?", "<63>")
            text = text.Replace("#", "<35>")
        End If

        If ignoreCase = True Then
            text = text.ToLower()
            find = find.ToLower()
        End If

        Return text Like find

        ' The following statement returns True (does "F" satisfy "F"?)
        'testCheck = "F" Like "F"
        ' The following statement returns False for Option Compare Binary
        '    and True for Option Compare Text (does "F" satisfy "f"?)
        'testCheck = "F" Like "f"
        ' The following statement returns False (does "F" satisfy "FFF"?)
        'testCheck = "F" Like "FFF"
        ' The following statement returns True (does "aBBBa" have an "a" at the
        '    beginning, an "a" at the end, and any number of characters in 
        '    between?)
        'testCheck = "aBBBa" Like "a*a"
        ' The following statement returns True (does "F" occur in the set of
        '    characters from "A" through "Z"?)
        'testCheck = "F" Like "[A-Z]"
        ' The following statement returns False (does "F" NOT occur in the 
        '    set of characters from "A" through "Z"?)
        'testCheck = "F" Like "[!A-Z]"
        ' The following statement returns True (does "a2a" begin and end with
        '    an "a" and have any single-digit number in between?)
        'testCheck = "a2a" Like "a#a"
        ' The following statement returns True (does "aM5b" begin with an "a",
        '    followed by any character from the set "L" through "P", followed
        '    by any single-digit number, and end with any character NOT in
        '    the character set "c" through "e"?)
        'testCheck = "aM5b" Like "a[L-P]#[!c-e]"
        ' The following statement returns True (does "BAT123khg" begin with a
        '    "B", followed by any single character, followed by a "T", and end
        '    with zero or more characters of any type?)
        'testCheck = "BAT123khg" Like "B?T*"
        ' The following statement returns False (does "CAT123khg"?) begin with
        '    a "B", followed by any single character, followed by a "T", and
        '    end with zero or more characters of any type?)
        'testCheck = "CAT123khg" Like "B?T*"


        'To match the special characters left bracket ([), question mark (?), number sign (#), 
        'and asterisk (*), enclose them in brackets. The right bracket (]) cannot be used within 
        'a group To match itself, but it can be used outside a group as an individual character.

        Return False
    End Function

    Private Shared Function ANDGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean, ByVal toSearch As Boolean) As Boolean
        'If All in the Array is found Then return True

        For Each line In find
            If line.Contains("*") = False Then
                If toSearch = True Then line = "*" & line & "*"
            End If

            If LikeGate(text, line, ignoreCase) = False Then
                Return False
            End If
        Next

        Return True
    End Function

    Private Shared Function ORGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean, ByVal toSearch As Boolean) As Boolean
        'If Any in the Array is found Then return True

        For Each line In find
            If line.Contains("*") = False Then
                If toSearch = True Then line = "*" & line & "*"
            End If

            If LikeGate(text, line, ignoreCase) = True Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Shared Function ORGate(ByVal text As String, ByVal find As String(), ByRef OrReturn As String, ByVal ignoreCase As Boolean, ByVal toSearch As Boolean) As Boolean
        'If Any in the Array is found Then return True

        For Each line In find
            If line.Contains("*") = False Then
                If toSearch = True Then line = "*" & line & "*"
            End If

            If LikeGate(text, line, ignoreCase) = True Then
                OrReturn = line
                Return True
            End If
        Next
        Return False
    End Function

    Private Shared Function XORGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean) As Boolean
        'If Any in the Array is found And If All the Array is Not found Then Return True

        If ORGate(text, find, ignoreCase, True) = True And ANDGate(text, find, ignoreCase, True) = False Then
            Return True
        End If
        Return False
    End Function

    Private Shared Function SEQGate(ByVal text As String, ByVal find As String(), ByVal ignoreCase As Boolean) As Boolean
        'If All in the Array is found in sequence Then return True
        Dim newIndex = 0

        For Each line In find
            newIndex = InStr(newIndex, text, line, Gate.eEQUALS)
            If newIndex = 0 Then Return False
        Next

        Return True
    End Function


#End Region


#Region "Punctuation"

    Public Shared Function DetectQuestion(ByVal text As String) As Boolean
        IsNull(text)

        Dim Q As String() = {
        "who *", "what *", "when *", "where *", "why *", "how *", "to whom *",
        "is *", "are *", "will *", "does *", "could *", "should *", "might *",
        "*?"}

        Return Search(text, Q, True, Gate.eEQUALS)
    End Function

    Public Shared Function DetectMark(ByVal text As String, ByVal mark As Char) As Boolean
        IsNull(text)
        Return Search(text, mark, Gate.eEND)
    End Function

    Public Shared Function AddMark(ByVal text As String, ByVal mark As Char) As String
        IsNull(text)
        If DetectMark(text, "?") = False And DetectMark(text, ".") = False And DetectMark(text, "!") = False Then
            Return Trim(text) & mark
        Else
            Return text
        End If
    End Function

    Public Shared Function RemoveMarks(ByVal text As String) As String
        Return Remove(text, {".", "?", "!", Chr(10), Chr(13)})
    End Function

    Public Shared Function RemoveDuplicateMarks(ByVal sentence As String) As String
        sentence = SingleSpace(sentence, "!")
        sentence = SingleSpace(sentence, "?")
        sentence = SingleSpace(sentence, ".")
        sentence = Replace(sentence, "!?", "?")
        sentence = Replace(sentence, "?!", "?")
        Return Trim(sentence)
    End Function

    Public Shared Function Capitalize(ByVal sentence As String) As String
        IsNull(sentence)

        If sentence.Length() = 1 Then
            Return UCase(sentence(0))
        End If
        If sentence.Length() > 1 Then
            Return UCase(sentence(0)) & Mid(sentence, 2, sentence.Length())
        End If
        Return sentence
    End Function

    Public Shared Function TotalMarkRemoval(ByVal text As String) As String
        Return Remove(text, {",", "@", "(", ")", "*", "{", "}", "[", "]", "-", "+", "=", "\", "/", "<", ">", "_", "|", "#", "~", ":", ";", ".", "?", "!", Chr(10), Chr(13)})
    End Function

    Public Shared Function Article(ByVal word As String, ByVal capitalize As Boolean) As String

        Dim first = FindFirstLetter(word)

        If Search(first, "e", True, Gate.eEQUALS) = True Or Search(first, "a", True, Gate.eEQUALS) = True Then
            If capitalize = True Then
                Return "An " & word
            Else
                Return "an " & word
            End If
        Else
            If capitalize = True Then
                Return "A " & word
            Else
                Return "a" & word
            End If
        End If

    End Function


#End Region


#Region "Trimming"


    Public Shared Function LTrim(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = Microsoft.VisualBasic.LTrim(text)
        End If
        Return Defaults(text, "")
    End Function

    Public Shared Function LMidTrim(ByVal text As String, ByVal Count As Integer) As String
        IntR(Count, 0)
        IsNull(text)
        Return Mid(text, Count + 1, text.Length())
    End Function

    Public Shared Function LMidTrim(ByVal text As String, ByVal junction As String, ByVal eGate As Gate) As String
        Return GetRightOfArg(text, junction, eGate)
    End Function


    Public Shared Function RTrim(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = Microsoft.VisualBasic.RTrim(text)
        End If
        Return Defaults(text, "")
    End Function

    Public Shared Function RMidTrim(ByVal text As String, ByVal Count As Integer) As String
        IsNull(text)
        IntR(Count, 0)
        Return Mid(text, 1, text.Length() - Count)
    End Function

    Public Shared Function RMidTrim(ByVal text As String, ByVal junction As String, ByVal eGate As Gate) As String
        Return GetLeftOfArg(text, junction, eGate)
    End Function


    Public Shared Function Trim(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = text.Trim()
        End If
        If Max = -1 Then
            text = text.Trim()
        End If
        Return text
    End Function

    Public Shared Function Trim(ByVal base As String(), Optional ByVal Max As Integer = MAX_TRIM) As String()
        Dim newList(base.Count() - 1) As String

        For i = 0 To base.Count() - 1
            newList(i) = Trim(base(i), Max)
        Next

        Return newList
    End Function

    Public Shared Function Trim(ByVal base As List(Of String), Optional ByVal Max As Integer = MAX_TRIM) As List(Of String)
        Dim newList As New List(Of String)
        For Each Entry In base
            newList.Add(Trim(Entry, Max))
        Next
        Return newList
    End Function


    Public Shared Function TrimLow(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = text.Trim().ToLower()
        End If
        If Max = -1 Then
            text = text.Trim().ToLower()
        End If
        Return text
    End Function

    Public Shared Function TrimUp(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = text.Trim().ToUpper()
        End If
        If Max = -1 Then
            text = text.Trim().ToUpper()
        End If
        Return text
    End Function

    Public Shared Function ToLower(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = text.ToLower()
        End If
        If Max = -1 Then
            text = text.ToLower()
        End If
        Return text
    End Function

    Public Shared Function ToUpper(ByVal text As String, Optional ByVal Max As Integer = MAX_TRIM) As String
        IsNull(text)
        If text.Length() < Max Or Max = 0 Then
            text = text.ToUpper()
        End If
        If Max = -1 Then
            text = text.ToLower()
        End If
        Return text
    End Function


    Public Shared Function Mid(ByVal text As String, ByVal Start As Integer, ByVal Count As Integer) As String
        IsNull(text)
        IntR(Start, 1)
        IntR(Count, 1)
        Return Defaults(Microsoft.VisualBasic.Mid(text, Start, Count), "")
    End Function

    Public Shared Function MidTrim(ByVal text As String, ByVal Count As Integer) As String
        IsNull(text)
        IntR(Count, 0)
        Return Mid(text, Count + 1, text.Length() - (2 * Count))
    End Function

    Public Shared Function MidTrim(ByVal text As String, ByVal LCount As Integer, ByVal RCount As Integer) As String
        IsNull(text)
        IntR(RCount, 0)
        IntR(LCount, 0)
        Return Mid(text, LCount + 1, text.Length() - LCount - RCount)
    End Function

    Public Shared Function MidTrim(ByVal text As String, ByVal Ljunction As String, ByVal Rjunction As String, ByVal eGate As Gate) As String
        Return GetMidArg(text, Ljunction, Rjunction, eGate)
    End Function


    Public Shared Function LBuffer(ByVal text As String, ByVal padding As String, ByVal count As Integer) As String
        IsNull(text)
        Return MultiStrings(padding, count) + text
    End Function

    Public Shared Function LBuffer(ByVal text As String, ByVal padding As String) As String
        IsNull(text)
        IsNull(padding)
        Return padding & text
    End Function

    Public Shared Function RBuffer(ByVal text As String, ByVal padding As String, ByVal count As Integer) As String
        IsNull(text)
        Return text & MultiStrings(padding, count)
    End Function

    Public Shared Function RBuffer(ByVal text As String, ByVal padding As String) As String
        IsNull(text)
        IsNull(padding)
        Return text & padding
    End Function

    Public Shared Function Buffer(ByVal text As String, ByVal padding As String, ByVal count As Integer) As String
        IsNull(text)
        padding = MultiStrings(padding, count)
        Return padding & text & padding
    End Function

    Public Shared Function Buffer(ByVal text As String, ByVal padding As String) As String
        IsNull(text)
        IsNull(padding)
        Return padding & text & padding
    End Function

    Public Shared Function Buffer(ByVal text As String(), ByVal padding As String) As String()
        IsNull(text)
        IsNull(padding)

        Dim newText(text.Length) As String
        For i = 0 To text.Length
            newText(i) = padding & text(i) & padding
        Next

        Return newText
    End Function

    Public Shared Function Buffer(ByVal text As List(Of String), ByVal padding As String) As List(Of String)
        IsNull(padding)

        Dim newText As New List(Of String)
        For Each line In text
            newText.Add(padding & line & padding)
        Next

        Return newText
    End Function

    Public Shared Function Buffer(ByVal text As String(), ByVal padding As String, ByVal count As Integer) As String()
        IsNull(text)
        IsNull(padding)
        padding = MultiStrings(padding, count)

        Dim newText(text.Length) As String
        For i = 0 To text.Length
            newText(i) = padding & text(i) & padding
        Next

        Return newText
    End Function

    Public Shared Function Buffer(ByVal text As List(Of String), ByVal padding As String, ByVal count As Integer) As List(Of String)
        IsNull(padding)
        padding = MultiStrings(padding, count)

        Dim newText As New List(Of String)
        For Each line In text
            newText.Add(padding & line & padding)
        Next

        Return newText
    End Function

#End Region


#Region "Numbers"


    Public Shared Function GetGreater(ByVal ints As Integer()) As Integer
        Dim results As Integer = 0

        For Each i In ints
            If i > results Then results = i
        Next

        Return results
    End Function
    Public Shared Function GetLesser(ByVal ints As Integer()) As Integer
        Dim results As Integer = GetGreater(ints)

        For Each i In ints
            If i < results Then results = i
        Next

        Return results
    End Function

    Public Shared Function GetGreater(ByVal ints As Double()) As Double
        Dim results As Double = 0

        For Each i In ints
            If i > results Then results = i
        Next

        Return results
    End Function
    Public Shared Function GetLesser(ByVal ints As Double()) As Double
        Dim results As Double = GetGreater(ints)

        For Each i In ints
            If i < results Then results = i
        Next

        Return results
    End Function

    Public Shared Function GetGreater(ByVal ints As Decimal()) As Decimal
        Dim results As Decimal = 0

        For Each i In ints
            If i > results Then results = i
        Next

        Return results
    End Function
    Public Shared Function GetLesser(ByVal ints As Decimal()) As Decimal
        Dim results As Decimal = GetGreater(ints)

        For Each i In ints
            If i < results Then results = i
        Next

        Return results
    End Function


    Public Shared Function IsGreater(ByVal base As Integer, ByVal int As Integer) As Boolean
        If base < int Then
            Return True
        End If

        Return False
    End Function
    Public Shared Function IsLesser(ByVal base As Integer, ByVal int As Integer) As Boolean
        If base > int Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function IsGreater(ByVal base As Double, ByVal int As Double) As Boolean
        If base < int Then
            Return True
        End If

        Return False
    End Function
    Public Shared Function IsLesser(ByVal base As Double, ByVal int As Double) As Boolean
        If base > int Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function IsGreater(ByVal base As Decimal, ByVal int As Decimal) As Boolean
        If base < int Then
            Return True
        End If

        Return False
    End Function
    Public Shared Function IsLesser(ByVal base As Decimal, ByVal int As Decimal) As Boolean
        If base > int Then
            Return True
        End If

        Return False
    End Function

    Public Shared Function WordsToNumbers(ByVal strNumber As String) As Long

        '10,000,000,000,000,000,000,000,000,000

        Dim BigNumbCol As New List(Of String)
        BigNumbCol.Add("octillion")
        BigNumbCol.Add("septillion")
        BigNumbCol.Add("sextillion")
        BigNumbCol.Add("quintillion")
        BigNumbCol.Add("quadrillion")
        BigNumbCol.Add("trillion")
        BigNumbCol.Add("billion")
        BigNumbCol.Add("million")
        BigNumbCol.Add("thousand")

        Dim Result As Decimal = 0D
        Dim intHundred As Decimal = 0D
        Dim intRes As Decimal = 0D
        Dim firstindex = 1
        Dim part1 = ""
        Dim Index = 0

        For Each BigN In BigNumbCol
            Index = InStr(strNumber, BigN)

            If Index > 0 Then

                part1 = Mid(strNumber, firstindex, Index - firstindex - 1)
                intHundred = ToNumbsHundreds(part1)
                intRes = intHundred * ToNumbsBigNumbers(BigN)
                firstindex = Index + BigN.Length()

            End If
            Result = Result + intRes

            intRes = 0
            intHundred = 0
        Next

        Index = InStr(strNumber, "thousand") + 9
        part1 = Mid(strNumber, Index, strNumber.Length() - Index + 1)
        Result += ToNumbsHundreds(part1)

        Return (Result)

    End Function
    Public Shared Function NumbersToWords(ByVal strNumber As String) As String

        Dim NumL = strNumber.Length()

        If NumL < 70 Then
            Dim f1 = ""
            Dim Line1 = ""
            Dim preLine = ""
            Dim r = 0

            If NumL <= 3 Then
                Dim Results = ToWordsHundreds(strNumber)
                Return (Results)
            Else

                Dim Remd = NumL Mod (3)
                If Remd = 0 Then Remd = 3

                Dim Nub1 = strNumber.Length - Remd
                Dim strNumb = Mid(strNumber, Remd + 1, strNumber.Length - Remd + 1)

                For i = Nub1 To 0 Step -3
                    f1 = Mid(strNumb, (strNumb.Length() - i) + 1, 3)
                    Line1 += ToWordsHundreds(f1) & " " & ToWordsBigNumbers((i - 3) / 3) & " "
                Next

                f1 = Mid(strNumber, 1, Remd)
                preLine = ToWordsHundreds(f1) & " " & ToWordsBigNumbers(Nub1 / 3) & " "
                Line1 = preLine & Line1

            End If

            Return (Line1)
        Else

            Return ""
        End If
    End Function


    'Private

    Private Shared Function ToWordsHundreds(ByVal strNumber As String) As String
        strNumber = Trim(strNumber)

        Dim strFirst = ""
        Dim strSecond = ""
        Dim strThird = ""
        Dim strTeens = ""
        Dim strSingles = ""

        If strNumber.Length = 1 Then

            'singles
            strSingles = ToWordsOnes(strNumber)
            Return (strSingles)
        End If

        If strNumber.Length = 2 Then

            'Teens
            strTeens = ToWordsTeens(strNumber)

            If strTeens > "" Then
                Return (strTeens)
            End If

            Dim first1 = strNumber(0)
            Dim second1 = strNumber(1)

            '20 through 99
            strFirst = ToWordsTens(first1)
            strSecond = ToWordsOnes(second1)

            Return (strFirst & "-" & strSecond)
        End If

        If strNumber.Length = 3 Then

            'hundreds
            Dim first1 = strNumber(0)
            Dim second1 = strNumber(1)
            Dim third1 = strNumber(2)

            strFirst = ToWordsOnes(first1) & " hundred and "

            If second1 = "1" Then
                strSecond = ToWordsTeens(second1 & third1)
                Return (strFirst & strSecond)
            ElseIf second1 = "0" Then
                strThird = ToWordsOnes(third1)
                Return (strFirst & strThird)
            Else
                strSecond = ToWordsTens(second1)
                strThird = ToWordsOnes(third1)
                Return (strFirst & strSecond & "-" & strThird)
            End If

        End If

        Return ("")
    End Function

    Private Shared Function ToWordsOnes(ByVal strNumber As String) As String

        Dim strSingles = ""

        'single digit
        Select Case strNumber
            Case "0"
                Return "zero"
            Case "1"
                Return "one"
            Case "2"
                Return "two"
            Case "3"
                Return "three"
            Case "4"
                Return "four"
            Case "5"
                Return "five"
            Case "6"
                Return "six"
            Case "7"
                Return "seven"
            Case "8"
                Return "eight"
            Case "9"
                Return "nine"
        End Select

        Return ""
    End Function

    Private Shared Function ToWordsTens(ByVal strNumber As String) As String

        Dim strTens = ""

        Select Case strNumber
            Case "2"
                Return "twenty"
            Case "3"
                Return "thirty"
            Case "4"
                Return "fourty"
            Case "5"
                Return "fifty"
            Case "6"
                Return "sixty"
            Case "7"
                Return "seventy"
            Case "8"
                Return "eighty"
            Case "9"
                Return "ninety"
        End Select

        Return ""
    End Function

    Private Shared Function ToWordsTeens(ByVal strNumber As String) As String

        Dim strTeens = ""

        '10s through 19
        Select Case strNumber
            Case "10"
                Return "ten"
            Case "11"
                Return "eleven"
            Case "12"
                Return "twelve"
            Case "13"
                Return "thirteen"
            Case "14"
                Return "fourteen"
            Case "15"
                Return "fifteen"
            Case "16"
                Return "sixteen"
            Case "17"
                Return "seventeen"
            Case "18"
                Return "eighteen"
            Case "19"
                Return "nineteen"
        End Select

        Return ""
    End Function

    Private Shared Function ToWordsBigNumbers(ByVal strNumber As String) As String
        Dim strBig = ""

        Select Case strNumber
            Case "1"
                Return "thousand"
            Case "2"
                Return "million"
            Case "3"
                Return "billion"
            Case "4"
                Return "trillion"
            Case "5"
                Return "quadrillion"
            Case "6"
                Return "quintillion"
            Case "7"
                Return "sextillion"
            Case "8"
                Return "septillion"
            Case "9"
                Return "octillion"
            Case "10"
                Return "nonillion"
            Case "11"
                Return "decillion"
            Case "12"
                Return "undecillion"
            Case "13"
                Return "duodecillion"
            Case "14"
                Return "tredecillion"
            Case "15"
                Return "quattuordecillion"
            Case "16"
                Return "quindecillion"
            Case "17"
                Return "sexdecillion"
            Case "18"
                Return "septendecillion"
            Case "19"
                Return "octodecillion"
            Case "20"
                Return "novemdecillion"
            Case "21"
                Return "vigintillion"
            Case "22"
                Return "centillion"
        End Select

        Return ""
    End Function


    'Private

    Private Shared Function ToNumbsHundreds(ByVal strNumber As String) As Long

        strNumber = TrimLow(strNumber)
        strNumber = Remove(strNumber.ToLower(), "and")

        'space clean-up
        strNumber = SingleSpace(strNumber)

        Dim strNumberArr As New List(Of String)
        Dim arrCount = 0

        Dim intT1 As Decimal = 0D
        Dim intT2 As Decimal = 0D
        Dim intT3 As Decimal = 0D

        Dim first1 = ""
        Dim second1 = ""
        Dim third1 = ""
        Dim forth1 = ""

        Dim first2 = ""
        Dim second2 = ""
        Dim third2 = ""

        If Search(strNumber, " ") = True Then
            strNumberArr = ToList(strNumber, " ")
            arrCount = strNumberArr.Count()
        End If

        If arrCount = 0 Then

            If intT1 = 0 Then intT1 = ToNumbsOnes(strNumber)
            If intT1 = -1 Then intT1 = ToNumbsTeens(strNumber)
            If intT1 = -1 Then intT1 = ToNumbsTens(strNumber)

            If intT1 > -1 Then
                Return (intT1)
            End If

            If Search(strNumber, "-") = True Then

                Dim NumArr = ToArray(strNumber, "-")

                first1 = GetIndex(NumArr, 0, "")
                second1 = GetIndex(NumArr, 1, "")

                intT1 = ToNumbsTens(first1)
                intT2 = ToNumbsOnes(second1)

                Return (intT1 + intT2)
            End If

        End If

        If arrCount = 2 Then

            first1 = GetIndex(strNumberArr, 0, "")
            second1 = GetIndex(strNumberArr, 1, "")

            If second1 = "hundred" Then

                intT1 = ToNumbsOnes(first1) * 100
                Return (intT1)
            Else

                intT1 = ToNumbsTens(first1)
                intT2 = ToNumbsOnes(second1)

                If intT1 > -1 And intT2 > -1 Then
                    Return (intT1 + intT2)
                End If

            End If

        End If

        If arrCount = 3 Then

            'hundreds
            first1 = GetIndex(strNumberArr, 0, "")
            second1 = GetIndex(strNumberArr, 1, "")
            third1 = GetIndex(strNumberArr, 2, "")

            If second1 = "hundred" Then

                intT1 = ToNumbsOnes(first1) * 100

                If Search(third1, "-") = True Then
                    Dim NumArr = ToArray(third1, "-")

                    first1 = GetIndex(NumArr, 0, "")
                    second1 = GetIndex(NumArr, 1, "")

                    intT2 = ToNumbsTens(first1)
                    intT3 = ToNumbsOnes(second1)

                    Return (intT1 + intT2 + intT3)
                Else

                    intT3 = ToNumbsOnes(third1)
                    If intT3 = 0 Then intT3 = ToNumbsTens(third1)
                    If intT3 = -1 Then intT3 = ToNumbsTeens(third1)

                    Return (intT1 + intT3)
                End If
            End If


        End If

        If arrCount = 4 Then

            'hundreds
            first1 = GetIndex(strNumberArr, 0, "")
            second1 = GetIndex(strNumberArr, 1, "")
            third1 = GetIndex(strNumberArr, 2, "")
            forth1 = GetIndex(strNumberArr, 3, "")

            If second1 = "hundred" Then

                intT1 = ToNumbsOnes(first1) * 100

                intT2 = ToNumbsTens(third1)
                intT3 = ToNumbsOnes(forth1)

                If intT1 > -1 And intT2 > -1 Then
                    Return (intT1 + intT2 + intT3)
                End If

            End If

        End If

        Return (0)
    End Function

    Private Shared Function ToNumbsOnes(ByVal strNumber As String) As Long

        'single digit
        Select Case strNumber
            Case "zero"
                Return 0
            Case "one"
                Return 1
            Case "two"
                Return 2
            Case "three"
                Return 3
            Case "four"
                Return 4
            Case "five"
                Return 5
            Case "six"
                Return 6
            Case "seven"
                Return 7
            Case "eight"
                Return 8
            Case "nine"
                Return 9
            Case Else
                Return -1
        End Select

    End Function

    Private Shared Function ToNumbsTens(ByVal strNumber As String) As Long

        Select Case strNumber
            Case "twenty"
                Return 20
            Case "thirty"
                Return 30
            Case "fourty"
                Return 40
            Case "fifty"
                Return 50
            Case "sixty"
                Return 60
            Case "seventy"
                Return 70
            Case "eighty"
                Return 80
            Case "ninety"
                Return 90
            Case Else
                Return -1
        End Select

    End Function

    Private Shared Function ToNumbsTeens(ByVal strNumber As String) As Long

        '10s through 19
        Select Case strNumber
            Case "ten"
                Return 10
            Case "eleven"
                Return 11
            Case "twelve"
                Return 12
            Case "thirteen"
                Return 13
            Case "fourteen"
                Return 14
            Case "fifteen"
                Return 15
            Case "sixteen"
                Return 16
            Case "seventeen"
                Return 17
            Case "eighteen"
                Return 18
            Case "nineteen"
                Return 19
            Case Else
                Return -1
        End Select

    End Function

    Private Shared Function ToNumbsBigNumbers(ByVal intNumber As String) As Decimal

        Select Case intNumber
            Case "thousand"
                Return 1000D
            Case "million"
                Return 1000000D
            Case "billion"
                Return 1000000000D
            Case "trillion"
                Return 1000000000000D
            Case "quadrillion"
                Return 1000000000000000D
            Case "quintillion"
                Return 1000000000000000000D
            Case "sextillion"
                Return 1000000000000000000000D
            Case "septillion"
                Return 1000000000000000000000000D
            Case "octillion"
                Return 1000000000000000000000000000D
            Case Else
                Return -1
        End Select

    End Function

#End Region


#Region "Util"

    Public Shared Sub IsNull(ByRef Var As String, Optional ByVal value As String = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = ""
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As Integer, Optional ByVal value As Integer = Nothing)
        If IsNothing(Var) = True Then
            If IsNumeric(value) = True Then
                Var = value
            Else
                Var = 0
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As Object(), Optional ByVal value As Object() = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.Length)
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As String(), Optional ByVal value As String() = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.Length)
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As String(,), Optional ByVal value As String(,) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.GetUpperBound(0), value.GetUpperBound(1))
                Var = value
            Else
                Var = {{}}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of String), Optional ByVal value As List(Of String) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of String)
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of String()), Optional ByVal value As List(Of String()) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of String())
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of List(Of String)), Optional ByVal value As List(Of List(Of String)) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of List(Of String))
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of List(Of String())), Optional ByVal value As List(Of List(Of String())) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of List(Of String()))
            End If
        End If
    End Sub


    Public Shared Sub IsNull(ByRef Var As Integer(), Optional ByVal value As Integer() = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.Length)
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As Integer(,), Optional ByVal value As Integer(,) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.GetUpperBound(0), value.GetUpperBound(1))
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of Integer), Optional ByVal value As List(Of Integer) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of Integer)
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of List(Of Integer)), Optional ByVal value As List(Of List(Of Integer)) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of List(Of Integer))
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As Boolean(), Optional ByVal value As Boolean() = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.Length)
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As Boolean(,), Optional ByVal value As Boolean(,) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                ReDim Var(value.GetUpperBound(0), value.GetUpperBound(1))
                Var = value
            Else
                Var = {}
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of Boolean), Optional ByVal value As List(Of Boolean) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of Boolean)
            End If
        End If
    End Sub

    Public Shared Sub IsNull(ByRef Var As List(Of List(Of Boolean)), Optional ByVal value As List(Of List(Of Boolean)) = Nothing)
        If IsNothing(Var) = True Then
            If IsNothing(value) = False Then
                Var = value
            Else
                Var = New List(Of List(Of Boolean))
            End If
        End If
    End Sub


    Public Shared Function Defaults(ByVal Var As String, Optional value As String = Nothing) As String
        IsNull(Var, value)
        Return Var
    End Function

    Public Shared Function Defaults(ByVal Var As Integer, Optional value As Integer = Nothing) As Integer
        IsNull(Var, value)
        Return Var
    End Function

    Public Shared Function Defaults(ByVal Var As Object, ByVal value As Object) As Object
        If IsNothing(Var) = True Then
            Return value
        Else
            Return Var
        End If
    End Function

    Private Shared Sub IntR(ByRef Var As Integer, ByVal Min As Integer)
        If IsNothing(Var) = True Then Var = Min
        If Var < Min Then Var = Min
    End Sub

    Private Shared Sub IntR(ByRef Var As Integer, ByVal Min As Integer, ByVal Max As Integer)
        If IsNothing(Var) = True Then Var = Min
        If Var < Min Then Var = Min
        If Var > Max Then Var = Max
    End Sub


#End Region

End Class

Public Class Simple3DES
    'The Rijndael (now referred to as Advanced Encryption Standard [AES]) and Triple Data Encryption Standard (3DES)
    'algorithms provide greater security than DES because they are more computationally intensive.

    Private TripleDes As New Security.Cryptography.TripleDESCryptoServiceProvider

    Sub New(ByVal key As String)
        ' Initialize the crypto provider.
        TripleDes.Key = TruncateHash(key, TripleDes.KeySize \ 8)
        TripleDes.IV = TruncateHash("", TripleDes.BlockSize \ 8)
    End Sub

    Private Function TruncateHash(ByVal key As String, ByVal length As Integer) As Byte()
        Dim sha1 As New Security.Cryptography.SHA1CryptoServiceProvider

        ' Hash the key. 
        Dim keyBytes() As Byte = System.Text.Encoding.Unicode.GetBytes(key)
        Dim hash() As Byte = sha1.ComputeHash(keyBytes)

        ' Truncate or pad the hash. 
        ReDim Preserve hash(length - 1)
        Return hash
    End Function

    Public Function Encrypt(ByVal plaintext As String) As String

        ' Convert the plaintext string to a byte array. 
        Dim plaintextBytes() As Byte = System.Text.Encoding.Unicode.GetBytes(plaintext)

        ' Create the stream. 
        Dim ms As New System.IO.MemoryStream

        ' Create the encoder to write to the stream. 
        Dim encStream As New Security.Cryptography.CryptoStream(ms, TripleDes.CreateEncryptor(), Security.Cryptography.CryptoStreamMode.Write)

        ' Use the crypto stream to write the byte array to the stream.
        encStream.Write(plaintextBytes, 0, plaintextBytes.Length)
        encStream.FlushFinalBlock()

        ' Convert the encrypted stream to a printable string. 
        Return Convert.ToBase64String(ms.ToArray)
    End Function

    Public Function Decrypt(ByVal encryptedtext As String) As String
        Try
            ' Convert the encrypted text string to a byte array. 
            Dim encryptedBytes() As Byte = Convert.FromBase64String(encryptedtext)

            ' Create the stream. 
            Dim ms As New System.IO.MemoryStream

            ' Create the decoder to write to the stream. 
            Dim decStream As New Security.Cryptography.CryptoStream(ms, TripleDes.CreateDecryptor(), Security.Cryptography.CryptoStreamMode.Write)

            ' Use the crypto stream to write the byte array to the stream.
            decStream.Write(encryptedBytes, 0, encryptedBytes.Length)
            decStream.FlushFinalBlock()

            ' Convert the plaintext stream to a string. 
            Return System.Text.Encoding.Unicode.GetString(ms.ToArray)
        Catch
            Return "-1"
        End Try
    End Function

End Class

Public Class TypeMemory
    Public Property Items() As New List(Of Memory)
    Public ReadOnly Property CopyItems() As List(Of Memory)
        Get
            Dim newItems As New List(Of Memory)
            newItems.AddRange(Items())
            Return newItems
        End Get
    End Property

    Public Function PrintIndex()
        Dim results = ""

        For Each line In Items
            results = results + line.Index() + vbCrLf
        Next

        Return results
    End Function


    Public Sub Add(ByVal newType As Memory)
        Items.Add(newType)
    End Sub

    Public Sub Add(ByVal Index As String, ByVal Entry As Object)
        Items.Add(New Memory(Index, Entry))
    End Sub

    Public Sub Add(ByVal Index As String, ByVal Tag As String, ByVal Entry As Object)
        Items.Add(New Memory(Index, Tag, Entry))
    End Sub

    Public Function Copy() As TypeMemory
        Dim newMemory As New TypeMemory
        newMemory.Items() = CopyItems()
        Return newMemory
    End Function


    Public Property IndexSearch(ByVal ID As String, ByVal TAG As String, ByVal eGate As Gate, ByVal Defaults As Object) As Object
        Get
            For Each item In Items()
                If Utility.Search(item.Index(), ID, eGate) = True Then

                    If TAG Is Nothing Then GoTo L1
                    If Utility.Search(item.Tag(), TAG, eGate) = True Then
L1:                     Return item.Entry()
                    End If
                End If
            Next
            Return Defaults
        End Get
        Set(value As Object)
            If DoesIndexExist(ID) = False Then
                Add(ID, TAG, value)
            Else
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If Utility.Search(item.Index(), ID, eGate) = True Then

                        If TAG Is Nothing Then GoTo L1
                        If Utility.Search(item.Tag(), TAG, eGate) = True Then
L1:                         item.Entry() = value
                        End If
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property

    Public Property IndexSearch(ByVal ID As String, ByVal eGate As Gate) As Object
        Get
            For Each item In Items()
                If Utility.Search(item.Index(), ID, eGate) = True Then
                    Return item.Entry()
                End If
            Next
            Return New Object
        End Get
        Set(value As Object)
            If DoesIndexExist(ID) = False Then
                Add(ID, value)
            Else
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If Utility.Search(item.Index(), ID, eGate) = True Then
                        item.Entry() = value
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property

    Public Property Index(ByVal ID As String, ByVal TAG As String, ByVal Defaults As Object) As Object
        Get
            For Each item In Items()
                If String.Compare(ID, item.Index(), True) = 0 Then

                    If TAG Is Nothing Then GoTo L1
                    If String.Compare(TAG, item.Tag(), True) = 0 Then
L1:                     Return item.Entry()
                    End If
                End If
            Next
            Return Defaults
        End Get
        Set(value As Object)
            If DoesIndexExist(ID) = False Then
                Add(ID, TAG, value)
            Else
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If String.Compare(ID, item.Index(), True) = 0 Then

                        If TAG Is Nothing Then GoTo L1
                        If String.Compare(TAG, item.Tag(), True) = 0 Then
L1:                         item.Entry() = value
                        End If
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property

    Public Property Index(ByVal ID As String, ByVal Defaults As Object) As Object
        Get
            For Each item In Items()
                If String.Compare(ID, item.Index(), True) = 0 Then
                    Return item.Entry()
                End If
            Next
            Return Defaults
        End Get
        Set(value As Object)
            If DoesIndexExist(ID) = False Then
                Add(ID, value)
            Else
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If String.Compare(ID, item.Index(), True) = 0 Then
                        item.Entry() = value
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property

    Public Property Index(ByVal ID As String) As Object
        Get
            For Each item In Items()
                If String.Compare(ID, item.Index(), True) = 0 Then
                    Return item.Entry()
                End If
            Next
            Return New Object
        End Get
        Set(value As Object)
            If DoesIndexExist(ID) = False Then
                Add(ID, value)
            Else
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If String.Compare(ID, item.Index(), True) = 0 Then
                        item.Entry() = value
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property


    Public ReadOnly Property GetString(ByVal ID As String) As String
        Get
            Return Utility.ToStr(Index(ID), "")
        End Get
    End Property
    Public ReadOnly Property GetString(ByVal ID As String, ByVal defaults As String) As String
        Get
            Return Utility.ToStr(Index(ID), defaults)
        End Get
    End Property

    Public ReadOnly Property GetInteger(ByVal ID As String) As Integer
        Get
            Return Utility.ToInt(Index(ID), 0)
        End Get
    End Property
    Public ReadOnly Property GetInteger(ByVal ID As String, ByVal defaults As Integer) As Integer
        Get
            Return Utility.ToInt(Index(ID), defaults)
        End Get
    End Property

    Public ReadOnly Property GetBoolean(ByVal ID As String) As Boolean
        Get
            Return Utility.ToBool(Index(ID), False)
        End Get
    End Property
    Public ReadOnly Property GetBoolean(ByVal ID As String, ByVal defaults As Boolean) As Boolean
        Get
            Return Utility.ToBool(Index(ID), defaults)
        End Get
    End Property

    Public ReadOnly Property GetDouble(ByVal ID As String) As Double
        Get
            Return Utility.ToDbl(Index(ID), 0)
        End Get
    End Property
    Public ReadOnly Property GetDouble(ByVal ID As String, ByVal defaults As Double) As Double
        Get
            Return Utility.ToDbl(Index(ID), defaults)
        End Get
    End Property


    Public Property Tag(ByVal ID As String, ByVal eGate As Gate) As String
        Get
            For Each item In Items()
                If Utility.Search(ID, item.Index(), eGate) = True Then
                    Return item.Tag()
                End If
            Next
            Return ""
        End Get
        Set(value As String)
            If DoesIndexExist(ID) = True Then
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If Utility.Search(ID, item.Index(), eGate) = True Then
                        item.Tag() = value
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property

    Public Property Tag(ByVal ID As String) As String
        Get
            For Each item In Items()
                If String.Compare(ID, item.Index(), True) = 0 Then
                    Return item.Tag()
                End If
            Next
            Return ""
        End Get
        Set(value As String)
            If DoesIndexExist(ID) = True Then
                Dim newItems As New List(Of Memory)

                For Each item In Items()
                    If String.Compare(ID, item.Index(), True) = 0 Then
                        item.Tag() = value
                    End If
                    newItems.Add(item)
                Next

                Items = newItems
            End If
        End Set
    End Property


    Public Function DoesIndexExist(ByVal ID As String) As Boolean
        For Each item In Items()
            If String.Compare(ID, item.Index(), True) = 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function DoesIndexExist(ByVal ID As String, ByVal eGate As Gate) As Boolean
        For Each item In Items()
            If Utility.Search(item.Index(), ID, eGate) = True Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Sub Remove(ByVal ID As String)
        Dim newItems As New List(Of Memory)

        For Each item In Items()
            If String.Compare(ID, item.Index(), True) <> 0 Then
                newItems.Add(item)
            End If
        Next

        Items = newItems
    End Sub

    Public Sub Remove(ByVal ID As String, ByVal eGate As Gate)
        Dim newItems As New List(Of Memory)

        For Each item In Items()
            If Utility.Search(item.Index(), ID, eGate) = False Then
                newItems.Add(item)
            End If
        Next

        Items = newItems
    End Sub

    Public ReadOnly Property Count()
        Get
            Return Items.Count()
        End Get
    End Property

    Public Sub Clear()
        Items.Clear()
    End Sub

    Public Class Memory

        Public Sub New()

        End Sub

        Public Sub New(ByVal Index As String, ByVal Entry As Object)
            Me.Index = Index
            Me.Entry = Entry
        End Sub

        Public Sub New(ByVal Index As String, ByVal Tag As String, ByVal Entry As Object)
            Me.Index() = Index
            Me.Tag() = Tag
            Me.Entry() = Entry
        End Sub

        Public Property Index() As String = ""
        Public Property Tag() As String = ""
        Public Property Entry() As Object = Nothing

    End Class

End Class

Public Class XmlSource

    'Adresss example = /database/table/column
    '<bill tag{@i love you=you do @bill is cool=on the fence}>
    '<bill tag{@name=bill @age=50}>

    Public Property NodeList() As New List(Of Node)
    Public ReadOnly Property CopyNodeList() As List(Of Node)
        Get
            Dim newList As New List(Of Node)
            newList.AddRange(NodeList())
            Return newList
        End Get
    End Property

#Region "CONSTANTS"

    Private Const BEG_REPLACE As String = "[Chr(60)]" '<
    Private Const END_REPLACE As String = "[Chr(62)]" '>
    Private Const CARRIAGE_REPLACE As String = "[CrLf]"
    Private Const FORMAT_DEPTH As Integer = 6

    Private Const BEG_ST As Char = "<"
    Private Const BEG_END As String = "</"
    Private Const BEG_SLH As Char = "/"
    Private Const END_IT As Char = ">"

    Private Const HEADER_STR As String = "<?"
    Private Const HEADER_END As String = "?>"

    Private Const COMMENT_ELMT As String = "XMLCOM"
    Private Const COMMENT_ELMT_STR As String = "<XMLCOM>"
    Private Const COMMENT_ELMT_END As String = "</XMLCOM>"
    Private Const COMMENT_STR As String = "<!--"
    Private Const COMMENT_END As String = "-->"

    Private Const TAG_ST As String = "tag{"
    Private Const TAG_END As String = "}"
    Private Const TAG_DELIM As String = "@"
    Private Const TAG_EQL As String = "="

#End Region

#Region "Private"

    Private Function Beg_Node(ByVal strName As String) As String
        strName = Utility.Trim(strName)
        Return BEG_ST & strName & END_IT
    End Function

    Private Function End_Node(ByVal strName As String) As String
        strName = Utility.Trim(strName)
        Return BEG_END & strName & END_IT
    End Function

    Private Sub Get_header_main()
        Dim header1 = Utility.GetMidArg(Document(), HEADER_STR, HEADER_END, Gate.eEQUALS)
        Document() = Utility.Remove(Document(), HEADER_STR & header1 & HEADER_END)
        Header() = Utility.Trim(header1)
    End Sub

    Private Function Max_Depth() As Integer
        Dim Result = 0
        For Each Node1 In NodeList()
            Dim ed1 = Node1.Depth()
            If ed1 > Result Then Result = ed1
        Next
        Return Result
    End Function

    Friend Function Replace_Tags(ByVal textToClean As String, ByVal toTags As Boolean)
        If textToClean = Nothing Then textToClean = ""
        If Utility.IsEmpty(textToClean) = True Then Return ""

        If toTags = False Then
            textToClean = Utility.Replace(textToClean, CARRIAGE_REPLACE, vbCrLf)
            textToClean = Utility.Replace(textToClean, BEG_REPLACE, BEG_ST)
            textToClean = Utility.Replace(textToClean, END_REPLACE, END_IT)
        Else
            textToClean = Utility.Replace(textToClean, vbCrLf, CARRIAGE_REPLACE)
            textToClean = Utility.Replace(textToClean, BEG_ST, BEG_REPLACE)
            textToClean = Utility.Replace(textToClean, END_IT, END_REPLACE)
        End If

        Return textToClean
    End Function

    Private Function Remove_Attributes(ByVal strfullEntry As String) As String
        Dim strName = ""
        'This removes the Attributes tags to only leave the Nodes name
        If Utility.Search(strfullEntry, TAG_ST) = True Then
            Dim strPartEntry = Utility.GetMidArg(strfullEntry, TAG_ST, TAG_END, Gate.eEQUALS)

            strName = Utility.Trim(Utility.Remove(strfullEntry, TAG_ST & strPartEntry & TAG_END))
        Else
            strName = Utility.Trim(strfullEntry)
        End If

        Return strName
    End Function

    Private Function GetAttributes(ByVal strfullEntry As String) As List(Of nAttribute)
        Dim NewAttributeList As New List(Of nAttribute)

        '<bill tag{@i love you=you do @bill is cool=on the fence}>
        '<bill tag{@name=bill @age=50}>

        If Utility.Search(strfullEntry, TAG_ST) = True Then

            strfullEntry = Utility.GetMidArg(strfullEntry, TAG_ST, TAG_END, Gate.eEQUALS)

            'removing quotes
            strfullEntry = Utility.Remove(strfullEntry, Chr(34))

            If Utility.Search(strfullEntry, TAG_DELIM) = True And
               Utility.Search(strfullEntry, TAG_EQL) = True Then

                Dim fullattr = Utility.ToArray(strfullEntry, TAG_DELIM)

                For Each attrib In fullattr

                    If Utility.Search(attrib, TAG_EQL) = True Then

                        Dim partattr = Utility.ToArray(attrib, TAG_EQL)

                        If partattr.Count() = 2 Then

                            If Utility.GetIndex(partattr, 0, "") > "" And Utility.GetIndex(partattr, 1, "") > "" Then

                                Dim Attribute1 As New nAttribute
                                Attribute1.Variable = Utility.Trim(Utility.GetIndex(partattr, 0, ""))
                                Attribute1.Value = Utility.Trim(Utility.GetIndex(partattr, 1, ""))

                                NewAttributeList.Add(Attribute1)

                            End If
                        End If
                    End If

                Next

            End If

        End If

        Return NewAttributeList
    End Function

    Private Function SetAttributes(ByVal strEntryName As String, ByVal Attributes As List(Of nAttribute)) As String
        Dim fullEntry = ""
        strEntryName = Utility.Trim(strEntryName)
        '<bill tag{@i love you=you do @bill is cool=on the fence}>

        If Attributes.Count() > 0 Then
            For Each Attrib In Attributes
                fullEntry = fullEntry & " " & TAG_DELIM & Attrib.Variable & TAG_EQL & Attrib.Value
            Next

            fullEntry = strEntryName & " " & TAG_ST & Utility.Trim(fullEntry) & TAG_END
        Else
            fullEntry = strEntryName
        End If

        Return fullEntry
    End Function

    Private Sub Node_Disect_Address(ByVal Node1 As Node)
        'This Adds all under Nodes to the NodeList
        Dim buildAddress As New List(Of String)

        For Each Address1 In Node1.Address()
            buildAddress.Add(Address1)

            If DoesNodeExist(buildAddress.ToArray) = False Then
                Dim newNode As New Node
                If Node1.Address().Length() = buildAddress.Count() Then
                    newNode = Node1
                Else
                    newNode.Address() = buildAddress.ToArray
                End If

                Add(newNode)
            End If

        Next

    End Sub

    Private Function HasChildren(ByVal Node1 As Node) As Boolean

        For Each Nod1 In NodeList()
            If Utility.Search(Nod1.Address(), Node1.Address(), True, Gate.eSTART) = True Then
                If Nod1.Address().Length() > Node1.Address().Length() Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function


#End Region

#Region "Main"

    Public Property Document() As String = ""
    Public Property Compress() As Boolean = False
    Public Property Encrypt() As Boolean = False
    Public Property Key() As String = "secret"

    Public Sub Loadfile(ByVal strLocation As String)

        Document() = Utility.TextReader(strLocation, Compress(), Encrypt(), Key())

        If Utility.Search(Document(), HEADER_STR) = False Then
            Document() = HEADER_STR & HEADER_END & vbCrLf & Document()
        End If

        'pre-format
        Document() = Utility.Replace(Document(), BEG_ST, vbCrLf & BEG_ST)
        Document() = Utility.Replace(Document(), END_IT, END_IT & vbCrLf)
        Document() = Utility.Replace(Document(), COMMENT_STR, vbCrLf & COMMENT_ELMT_STR)
        Document() = Utility.Replace(Document(), COMMENT_END, COMMENT_ELMT_END & vbCrLf)
        Clear()

        Get_header_main()
        Load(Document())
    End Sub

    Public Sub Save(ByVal strLocation As String)
        Utility.TextWriter(strLocation, Format(), False, Compress(), Encrypt(), Key())
    End Sub

    Public Sub Load(ByVal strText As String)

        Dim start_Recording_Name = False
        Dim start_Checking_NodeName = False
        Dim start_Recording = False
        Dim saving_Node = False
        Dim Skip = False

        Dim strBuild = ""
        Dim Attributes1 As New List(Of nAttribute)
        Dim innerText = ""
        Dim addresslog As New List(Of String)


        For i = 0 To strText.Length() - 1

            '*********************

            Dim chr1 = Utility.GetIndex(strText, i)

            '*********************

            If chr1 = BEG_ST Then '<
                start_Recording = True 'starts all recording, thus ignoring empty spaces
                start_Recording_Name = True 'starts recording node's name, false will record innerText
                Skip = True 'this will skip the current char from being recorded, i.e. < or > 
            End If


            If start_Recording = True Then

                If chr1 = END_IT Then '>
                    'The > char has indicated that the node name has been recorded 
                    start_Recording_Name = False 'the recording of the node name is now ended, starts recording innerText
                    start_Checking_NodeName = True 'this will check to see if the first or second node name is found 
                    Skip = True 'This > char will not be recorded
                End If

                '*********************

                If Skip = False Then
                    If start_Recording_Name = True Then
                        strBuild = strBuild + chr1 'building the node's name
                    Else
                        innerText = innerText + chr1 'building the innerText
                    End If
                End If

                If start_Checking_NodeName = True Then
                    'This will check to see if the node name is either the first or last of the pair
                    'If it is the first name we will get it's attributes and add it to the addresslog
                    'If it is a second pair, last node, we will save that node
                    'It is saved because the innerText will have been collected

                    If Utility.GetIndex(strBuild, 0) <> BEG_SLH Then

                        Dim nodename = Remove_Attributes(strBuild) 'NODE NAME
                        addresslog.Add(nodename) 'NODE ADDRESS
                        Attributes1.AddRange(GetAttributes(strBuild))
                        strBuild = ""
                        innerText = ""
                    Else
                        saving_Node = True
                    End If

                    start_Checking_NodeName = False
                End If


                If saving_Node = True Then
                    Dim Node1 As New Node
                    Node1.Attributes() = Attributes1
                    Node1.Address() = addresslog.ToArray
                    Node1.InnerText() = Utility.Trim(innerText, 0)
                    Add(Node1)
                    Attributes1.Clear()

                    innerText = ""
                    strBuild = ""

                    'This keeps track of the address level
                    addresslog = Utility.RemoveLast(addresslog)
                    saving_Node = False
                End If
            End If

            Skip = False
        Next

    End Sub

    Public Function Copy() As XmlSource
        Dim newXml As New XmlSource
        newXml.Document() = Document()
        newXml.Compress() = Compress()
        newXml.Encrypt() = Encrypt()
        newXml.Key() = Key()
        newXml.Header() = Header()
        newXml.NodeList() = CopyNodeList()
        Return newXml
    End Function

    Public Sub Refresh()
        Document() = Format()
        Load(Document())
    End Sub

    Public Sub Clear()
        Header() = ""
        NodeList().Clear()
    End Sub

    Public Function Format()

        Dim newDocument As New List(Of String)
        Dim i = 0
        Dim SpacMark = "[SM]"
        Dim MaxDepth = Max_Depth()

        Dim CopyList = CopyNodeList()
        CopyList.Reverse()

        Do

            If i > MaxDepth Then
                Exit Do
            End If


            For Each Node1 In CopyList

                Dim Depth = Node1.Depth()

                If Depth = i Then


                    Dim BeginningNode = Beg_Node(SetAttributes(Utility.Join(Node1.Address(), ":"), Node1.Attributes()))
                    Dim EndingNode = End_Node(Node1.Name())

                    If Depth = 0 Then

                        newDocument = Utility.SetIndex(newDocument, 0, i & SpacMark & BeginningNode)
                        newDocument = Utility.SetIndex(newDocument, 1, (i + 1) & SpacMark & Replace_Tags(Node1.InnerText(), True))
                        newDocument = Utility.SetIndex(newDocument, 2, i & SpacMark & EndingNode)

                    Else
                        Dim Parent1 = Parent(Node1)
                        Dim ParentNode = Beg_Node(SetAttributes(Utility.Join(Parent1.Address(), ":"), Parent1.Attributes()))

                        Dim Count = 0
                        For Each Line1 In Utility.Copy(newDocument)

                            If Utility.Search(Line1, (i - 1) & SpacMark & ParentNode, Gate.eEQUALS) = True Then

                                newDocument = Utility.Insert(newDocument, Count + 1, i & SpacMark & EndingNode)
                                newDocument = Utility.Insert(newDocument, Count + 1, (i + 1) & SpacMark & Replace_Tags(Node1.InnerText(), True))
                                newDocument = Utility.Insert(newDocument, Count + 1, i & SpacMark & BeginningNode)

                            End If

                            Count = Count + 1
                        Next
                    End If

                End If
            Next

            i = i + 1
        Loop

        'Space Format
        For i = 0 To MaxDepth + 1
            If i = 0 Then
                newDocument = Utility.RemoveIn(newDocument, i & SpacMark)
            Else
                Dim NodeSpace = Utility.MultiStrings(Chr(32), FORMAT_DEPTH * i)
                newDocument = Utility.Replace(newDocument, i & SpacMark, NodeSpace)
            End If

        Next

        'Replace addresses
        For Each Node1 In CopyList
            newDocument = Utility.Replace(newDocument, BEG_ST & Utility.Join(Node1.Address(), ":") & END_IT, BEG_ST & Node1.Name() & END_IT)
        Next

        'post-format
        newDocument = Utility.Replace(newDocument, COMMENT_ELMT_STR, COMMENT_STR)
        newDocument = Utility.Replace(newDocument, COMMENT_ELMT_END, COMMENT_END)

        'ADDING HEADER
        If Utility.IsEmpty(Header()) = False Then
            Return HEADER_STR & Header() & HEADER_END & Utility.MultiStrings(vbCrLf, 2) & Utility.Join(newDocument, vbCrLf)
        Else
            Return Utility.MultiStrings(vbCrLf, 2) & Utility.Join(newDocument, vbCrLf)
        End If

    End Function


    Public Function PrintNodes() As List(Of String)
        Dim newArray As New List(Of String)

        For Each Nod1 In NodeList()
            newArray.Add("ADDRESS::" & Utility.Join(Nod1.Address(), BEG_SLH) & "::ATTRIBUTES::" & Nod1.attributesToString() & "::INNERTEXT::" & Nod1.InnerText())
        Next

        Return newArray
    End Function

#End Region

#Region "Nodes"

    Public Sub Add(ByVal Node1 As Node)
        NodeList().Add(Node1)
    End Sub

    Public Sub Remove(ByVal Node1 As Node)
        Dim newList As New List(Of Node)

        For Each nod In NodeList()
            If Utility.Compare(nod.Address(), Node1.Address(), True, Gate.eEQUALS) = True Then
                Continue For
            End If

            newList.Add(nod)
        Next

        NodeList() = newList
    End Sub

    Public Sub Remove(ByVal Address As String())
        Dim newList As New List(Of Node)

        For Each nod In NodeList()
            If Utility.Compare(nod.Address(), Address, True, Gate.eEQUALS) = True Then
                Continue For
            End If

            newList.Add(nod)
        Next

        NodeList() = newList
    End Sub

    Public Sub Update(ByVal Node1 As Node)
        Dim newNodes As New List(Of Node)

        If DoesNodeExist(Node1.Address()) = True Then

            For Each Nod1 In NodeList()
                If Utility.Compare(Nod1.Address(), Node1.Address(), True, Gate.eEQUALS) = True Then
                    Nod1 = Node1
                End If

                newNodes.Add(Nod1)
            Next

            NodeList() = newNodes

        Else
            Node_Disect_Address(Node1)
        End If

    End Sub

    Public Function FindNode(ByVal address As String()) As Node
        For Each Nod1 In NodeList()
            If Utility.Compare(Nod1.Address(), address, True, Gate.eEQUALS) = True Then
                Return Nod1
            End If
        Next

        Dim Node1 As New Node
        Return Node1
    End Function


    Public Sub Create(ByVal Node1 As Node)
        Node_Disect_Address(Node1)
    End Sub

    Public Sub Create(ByVal address As String(), ByVal strText As String, ByVal strAttributes As String)
        Dim Node1 As New Node
        Node1.Address() = address
        Node1.InnerText() = strText
        Node1.Attributes() = GetAttributes(strAttributes)
        Node_Disect_Address(Node1)
    End Sub

    Public Sub Create(ByVal address As String(), ByVal strText As String, ByVal Attributes As List(Of nAttribute))
        Dim Node1 As New Node
        Node1.Address() = address
        Node1.InnerText() = strText
        Node1.Attributes() = Attributes
        Node_Disect_Address(Node1)
    End Sub

    Public Sub Create(ByVal address As String(), ByVal strText As String)
        Dim Node1 As New Node
        Node1.Address() = address
        Node1.InnerText() = strText
        Node_Disect_Address(Node1)
    End Sub

    Public Sub Create(ByVal address As String())
        Dim Node1 As New Node
        Node1.Address() = address
        Node_Disect_Address(Node1)
    End Sub


    Public Sub Delete(ByVal address As String())
        'Deletes node and children
        Dim newNodes As New List(Of Node)

        For Each Node1 In NodeList()
            If Node1.Address.Length() >= address.Length() Then
                If Utility.Search(Node1.Address(), address, True, Gate.eSTART) = False Then
                    newNodes.Add(Node1)
                End If
            Else
                newNodes.Add(Node1)
            End If
        Next

        NodeList() = newNodes
    End Sub

    Public Sub Rename(ByVal address As String(), ByVal strName As String)
        'Renaming a Node or Element
        Dim newNodes As New List(Of Node)
        Dim Test = False

        For Each Node1 In NodeList()

            If Test = False Then
                If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                    Node1.Name() = strName
                    Test = True
                End If
            End If

            newNodes.Add(Node1)
        Next

        NodeList() = newNodes
    End Sub

    Public Sub Move(ByVal address As String(), ByVal newaddress As String())
        'move contents of one Node to another
        Dim newNodes As New List(Of Node)
        Dim Test = False

        If DoesNodeExist(address) = True Then

            For Each Node1 In NodeList()

                If Test = False Then
                    If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                        Node1.Address() = newaddress
                        Test = True
                    End If
                End If

                newNodes.Add(Node1)
            Next

            NodeList() = newNodes
        End If
    End Sub

    Public Sub Copy(ByVal address As String(), ByVal newaddress As String())

        Dim newNode As New Node

        For Each Node1 In NodeList()

            If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                newNode = Node1
                newNode.Address() = newaddress
                Node_Disect_Address(newNode)
                Exit For
            End If

        Next
    End Sub

    Public Function DoesNodeExist(ByVal address As String()) As Boolean
        For Each Node1 In NodeList()
            If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function GetDepth(ByVal address As String()) As String
        Return address.Length - 1
    End Function


    Public Property Header() As String = ""

    Public Function RootNodes(ByVal address As String()) As List(Of String)
        Dim NodeArray As New List(Of String)
        Dim Count = address.Length() + 1

        For Each Node1 In NodeList()
            If Node1.Address().Length = Count Then
                NodeArray.Add(Node1.Name)
            End If
        Next

        Return NodeArray
    End Function

    Public Function RootNodes() As List(Of String)
        Dim RootArray As New List(Of String)

        For Each Node1 In NodeList()
            If Node1.IsRootNode = True Then
                RootArray.Add(Node1.Name)
            End If
        Next

        Return RootArray
    End Function

    Public Function Parent(ByVal Node1 As Node) As Node
        Return FindNode(Utility.RemoveLast(Node1.Address()))
    End Function

    Public Function Parent(ByVal address As String()) As Node
        Return FindNode(Utility.RemoveLast(address))
    End Function

    Public Function Children(ByVal Node1 As Node) As List(Of Node)
        Dim ChildNodes As New List(Of Node)
        Dim Count = Node1.Address().Length()

        For Each Node1 In NodeList()
            If Count < Node1.Address().Length() Then
                If Utility.Search(Node1.Address(), Node1.Address(), True, Gate.eSTART) = True Then
                    ChildNodes.Add(Node1)
                End If
            End If
        Next

        Return ChildNodes
    End Function

    Public Function Children(ByVal address As String()) As List(Of Node)
        Dim ChildNodes As New List(Of Node)
        Dim Count = address.Length()

        For Each Node1 In NodeList()
            If Count < Node1.Address().Length() Then
                If Utility.Search(Node1.Address(), address, True, Gate.eSTART) = True Then
                    ChildNodes.Add(Node1)
                End If
            End If
        Next

        Return ChildNodes
    End Function

    Public Function Siblings(ByVal Node1 As Node) As List(Of Node)
        Return Children(Utility.RemoveLast(Node1.Address()))
    End Function

    Public Function Siblings(ByVal address As String()) As List(Of Node)
        Return Children(Utility.RemoveLast(address))
    End Function


    Public Function Elements() As List(Of Node)
        Dim newNodes As New List(Of Node)

        For Each Node1 In NodeList()
            If HasChildren(Node1) = False Then
                newNodes.Add(Node1)
            End If
        Next

        Return newNodes
    End Function

    Public Property InnerText(ByVal address As String()) As String
        Get
            For Each Node1 In NodeList()
                If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                    Return Node1.InnerText(True)
                End If
            Next
            Return ""
        End Get
        Set(ByVal value As String)

            Dim newNodes As New List(Of Node)
            Dim Test = False

            For Each Node1 In NodeList()
                If Test = False Then
                    If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                        Node1.InnerText() = value
                        Test = True
                    End If
                End If

                newNodes.Add(Node1)
            Next

            NodeList() = newNodes

        End Set
    End Property

    Public Property InnerText(ByVal address As String(), ByVal Append As Boolean) As String
        Get
            For Each Node1 In NodeList()
                If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                    Return Node1.InnerText(True)
                End If
            Next

            Return ""
        End Get
        Set(ByVal value As String)

            Dim newNodes As New List(Of Node)
            Dim Test = False

            For Each Node1 In NodeList()
                If Test = False Then
                    If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                        If Append = True Then
                            Node1.InnerText() = Node1.InnerText() + value
                        Else
                            Node1.InnerText() = value
                        End If
                        Test = True
                    End If
                End If

                newNodes.Add(Node1)
            Next

            NodeList() = newNodes

        End Set
    End Property

    Public Function IsElement(ByVal address As String()) As Boolean

        For Each Node1 In NodeList()
            If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                If HasChildren(Node1) = False Then
                    Return True
                Else
                    Return False
                End If
            End If
        Next

        Return False
    End Function

    Public Property Attributes(ByVal address As String()) As List(Of nAttribute)
        Get
            Return FindNode(address).Attributes
        End Get
        Set(ByVal value As List(Of nAttribute))

            Dim newNodes As New List(Of Node)
            Dim Test = False

            For Each Node1 In NodeList()
                If Test = False Then
                    If Utility.Compare(Node1.Address(), address, True, Gate.eEQUALS) = True Then
                        Node1.Attributes = value
                        Test = True
                    End If
                End If

                newNodes.Add(Node1)
            Next

            NodeList() = newNodes
        End Set
    End Property

    Public Function FindAttributeValue(ByVal address As String(), ByVal strVariable As String) As String
        Return FindNode(address).attributeByVariable(strVariable)
    End Function


#End Region


    Public Class nAttribute
        Public Property Variable() As String = ""
        Public Property Value() As String = ""
    End Class

    Public Class Node
        Private Utility As New Matrix.Utility
        Private Gate As New Gate
        Private Xml_Source As New XmlSource

        Public Sub New()

        End Sub

        Public Sub New(ByVal address As String(), ByVal innerText As String, ByVal attributes As List(Of nAttribute))
            Me.Address = address
            Me.InnerText = innerText
            Me.Attributes = attributes
        End Sub

        Public Property Attributes() As New List(Of nAttribute)

        Public ReadOnly Property attributesToString() As String
            Get
                Dim fullEntry = ""

                If Attributes.Count() > 0 Then
                    For Each Attrib In Attributes
                        fullEntry = fullEntry & " " & TAG_DELIM & Attrib.Variable & TAG_EQL & Attrib.Value
                    Next

                    fullEntry = TAG_ST & Utility.Trim(fullEntry) & TAG_END
                End If

                Return fullEntry
            End Get
        End Property

        Public ReadOnly Property attributeByVariable(ByVal strVariable As String) As String
            Get
                For Each Attrib In Attributes
                    If Utility.Search(Attrib.Variable, strVariable, Gate.eEQUALS) = True Then
                        Return Attrib.Value
                    End If
                Next

                Return ""
            End Get
        End Property


        Public ReadOnly Property IsRootNode() As Boolean
            Get
                If Depth() = 0 Then
                    Return True
                Else
                    Return False
                End If
            End Get
        End Property

        Public ReadOnly Property IsComment() As Boolean
            Get
                Return Utility.Search(COMMENT_ELMT, Address(), Gate.eEQUALS)
            End Get
        End Property

        Public Property Name() As String
            Get
                Dim strName = "temp1"
                If Address().Length > 0 Then
                    strName = Utility.GetLast(Address())
                End If

                Return Utility.Trim(strName)
            End Get
            Set(ByVal value As String)
                Address() = Utility.SetLast(Address(), value)
            End Set
        End Property

        Private strText As String = ""
        Public Property InnerText(Optional ByVal DecodeTags As Boolean = False) As String
            Get
                If DecodeTags = True Then
                    strText = Xml_Source.Replace_Tags(strText, False)
                End If

                Return Utility.Trim(strText)
            End Get
            Set(ByVal value As String)
                strText = value
            End Set
        End Property

        Public ReadOnly Property AddressRange(ByVal intDepth As Integer) As String()
            Get
                Return Utility.GetRange(Address(), 0, intDepth)
            End Get
        End Property

        Public Property Address() As String()

        Public ReadOnly Property Index(ByVal intIndex As Integer) As String
            Get
                If intIndex < 0 Then intIndex = 0
                If Address().Length() > intIndex Then
                    Return Address(intIndex)
                Else
                    Return ""
                End If
            End Get
        End Property

        Public ReadOnly Property Depth() As Integer
            Get
                Return Address().Length() - 1
            End Get
        End Property

        Public Sub Clean()
            InnerText() = ""
            Address() = {}
            Attributes.Clear()
        End Sub

    End Class

End Class

Public Class Pairs

    Sub New()
    End Sub

    Sub New(ByVal name As String, ByVal entry As Object)
        Add(name, entry)
    End Sub
    Sub New(ByVal name As String, ByVal entry As Object, ByVal arguments As String())
        Add(name, entry, arguments)
    End Sub

    Private Const DELIM As String = ","

    Public Property Items() As New List(Of Pair)
    Public ReadOnly Property CopyItems() As List(Of Pair)
        Get
            Dim newList As New List(Of Pair)
            newList.AddRange(Items())
            Return newList
        End Get
    End Property

    Public Function Copy() As Pairs
        Dim newPairs As New Pairs
        newPairs.Items() = CopyItems()
        Return newPairs
    End Function

    Public Sub Clear()
        Items.Clear()
    End Sub

    Public Sub Add(ByVal name As String, ByVal entry As Object)
        Dim newPair As New Pair
        newPair.Name() = name
        newPair.Entry() = entry
        Items.Add(newPair)
    End Sub

    Public Sub Add(ByVal name As String, ByVal entry As Object, ByVal arguments As String())
        Dim newPair As New Pair
        newPair.Name() = name
        newPair.Entry() = entry
        newPair.Arguments() = arguments
        Items.Add(newPair)
    End Sub

    Public Sub Add(ByVal iPairs As Pairs)
        Items().AddRange(iPairs.Items())
    End Sub

    Public Sub Add(ByVal iPair As Pair)
        Items().AddRange(iPair)
    End Sub

    Private Function GetIndex(ByVal array_1 As Object, ByVal index As Integer, ByVal Defaults As Object) As Object
        If index < array_1.Length() And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function


    Public Shadows ReadOnly Property ToArray() As String()
        Get
            Dim Results(Items.Count()) As String

            For i = 0 To Items.Count() - 1
                Results(i) = Items(i).Name() + DELIM + Items(i).Entry()
            Next

            Return Results
        End Get
    End Property

    Public Sub RemoveDuplicates()
        Dim newItems As New List(Of Pair)

        For Each Item In Items()
            If DoesPairExist(newItems, Item) = False Then
                newItems.Add(Item)
            End If
        Next

        Items() = newItems
    End Sub

    Public Sub RemoveDuplicatesByName()
        Dim newItems As New List(Of Pair)

        For Each Item In Items()
            If DoesNameExist(newItems, Item.Name()) = False Then
                newItems.Add(Item)
            End If
        Next

        Items() = newItems
    End Sub


    Private Function DoesPairExist(ByVal localPairs As List(Of Pair), ByVal Item As Pair) As Boolean
        For Each localPair In localPairs
            If Utility.Search(localPair.Name(), Item.Name(), Gate.eEQUALS) = True Then

                If TypeOf Item.Entry() Is String Then
                    If Utility.Search(Utility.ToStr(localPair.Entry()), Utility.ToStr(Item.Entry()), Gate.eEQUALS) = True Then
                        Return True
                    End If
                ElseIf Utility.IsNumber(Item.Entry()) = True Then
                    If localPair.Entry() = Item.Entry() Then
                        Return True
                    End If
                Else
                    If localPair.Entry() Is Item.Entry() Then
                        Return True
                    End If
                End If

            End If
        Next

        Return False
    End Function
    Private Function DoesNameExist(ByVal localPairs As List(Of Pair), ByVal name As String) As Boolean
        For Each localPair In localPairs
            If Utility.Search(localPair.Name(), name, Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function


    Public Function DoesPairExist(ByVal name As String, ByVal entry As Object) As Boolean
        For Each Item In Items()
            If Utility.Search(Item.Name(), name, Gate.eEQUALS) = True Then

                If TypeOf Item.Entry() Is String Then
                    If Utility.Search(Utility.ToStr(Item.Entry()), Utility.ToStr(entry), Gate.eEQUALS) = True Then
                        Return True
                    End If
                ElseIf Utility.IsNumber(Item.Entry()) = True Then
                    If Item.Entry() = entry Then
                        Return True
                    End If
                Else
                    If Item.Entry() Is entry Then
                        Return True
                    End If
                End If

            End If
        Next

        Return False
    End Function

    Public Function DoesPairExist(ByVal Item As Pair) As Boolean
        For Each localPair In Items()
            If Utility.Search(localPair.Name(), Item.Name(), Gate.eEQUALS) = True Then

                If TypeOf Item.Entry() Is String Then
                    If Utility.Search(Utility.ToStr(localPair.Entry()), Utility.ToStr(Item.Entry()), Gate.eEQUALS) = True Then
                        Return True
                    End If
                ElseIf Utility.IsNumber(Item.Entry()) = True Then
                    If localPair.Entry() = Item.Entry() Then
                        Return True
                    End If
                Else
                    If localPair.Entry() Is Item.Entry() Then
                        Return True
                    End If
                End If

            End If
        Next

        Return False
    End Function

    Public ReadOnly Property Count() As Integer
        Get
            Return Items().Count()
        End Get
    End Property

    Public Class Pair
        Sub New()
        End Sub

        Sub New(ByVal name As String, ByVal entry As Object)
            Me.Name() = name
            Me.Entry() = entry
        End Sub
        Sub New(ByVal name As String, ByVal entry As Object, ByVal arguments As String())
            Me.Name() = name
            Me.Entry() = entry
            Me.Arguments() = arguments
        End Sub

        Public Property Name() As String = ""
        Public Property Entry() As Object = ""
        Public Property Arguments() As String()

        Public ReadOnly Property GetArg(ByVal index As Integer) As String
            Get
                Return Utility.GetIndex(Arguments(), index, "")
            End Get
        End Property

        Public ReadOnly Property GetString() As String
            Get
                Return Utility.ToStr(Entry(), "")
            End Get
        End Property
        Public ReadOnly Property GetString(ByVal defaults As String) As String
            Get
                Return Utility.ToStr(Entry(), defaults)
            End Get
        End Property

        Public ReadOnly Property GetInteger() As Integer
            Get
                Return Utility.ToInt(Entry(), 0)
            End Get
        End Property
        Public ReadOnly Property GetInteger(ByVal defaults As Integer) As Integer
            Get
                Return Utility.ToInt(Entry(), defaults)
            End Get
        End Property

        Public ReadOnly Property GetBoolean() As Boolean
            Get
                Return Utility.ToBool(Entry(), False)
            End Get
        End Property
        Public ReadOnly Property GetBoolean(ByVal defaults As Boolean) As Boolean
            Get
                Return Utility.ToBool(Entry(), defaults)
            End Get
        End Property

        Public ReadOnly Property GetDouble() As Double
            Get
                Return Utility.ToDbl(Entry(), 0)
            End Get
        End Property
        Public ReadOnly Property GetDouble(ByVal defaults As Double) As Double
            Get
                Return Utility.ToDbl(Entry(), defaults)
            End Get
        End Property


    End Class

End Class

Public Class Debug

    Private Const SLH As String = "/"
    Private Const R_BRC As String = "]"
    Private Const L_BRC As String = "["

    Public Const eERROR As String = "<ERROR>"
    Public Const eNULL As String = "<NULL>"
    Public Const eSUCCESS As String = "<SUCCESS>"
    Public Const eMESSAGE As String = "<MESSAGE>"

    Public Shared Property Debugfile() As String = ""
    Public Shared Property ShowPopups() As Boolean = False
    Public Shared Property ExitProgramOnError() As Boolean = False
    Public Shared Property Report() As String
    Public Shared Property Buffer() As String = vbCrLf + vbCrLf


    Public Shared Sub Clear()
        Utility.TextWriter(Debugfile(), "", False)
    End Sub

    Public Shared Function Readfile() As String
        Return Utility.TextReader(Debugfile())
    End Function

    Public Shared Sub Debug(ByVal message As String)
        Send(message)
        If ShowPopups = True Then MsgBox(message)
    End Sub

    Public Shared Sub Debug(ByVal ID As String, ByVal message As String)
        Send(ID & L_BRC & message & R_BRC)
        If ShowPopups = True Then MsgBox(ID & L_BRC & message & R_BRC)
        If ExitProgramOnError Then Utility.ExitProgram()
    End Sub

    Public Shared Sub Debug(ByVal ID As String, ByVal message As String, ByVal ex As Exception)
        Send(ID & L_BRC & message & Error_Message(ex) & R_BRC)
        If ShowPopups = True Then MsgBox(ID & L_BRC & message & R_BRC & Error_Message(ex))
        If ExitProgramOnError Then Utility.ExitProgram()
    End Sub

    Public Shared Sub Debug(ByVal ID As String, ByVal ex As Exception)
        Send(ID & L_BRC & Error_Message(ex) & R_BRC)
        If ShowPopups = True Then MsgBox(ID & L_BRC & Error_Message(ex) & R_BRC)
        If ExitProgramOnError Then Utility.ExitProgram()
    End Sub

    Public Shared Function Error_Message(ByVal ex As Exception) As String
        Return eERROR & Trim(ex.StackTrace) & eMESSAGE & Trim(ex.Message)
    End Function

    Public Shared Sub Debug(ByVal CLASSNAME As String, ByVal METHODNAME As String, ByVal INSTANCE As String)
        Dim ID = CLASSNAME & SLH & METHODNAME & SLH & INSTANCE
        Send(ID)
        If ShowPopups = True Then MsgBox(ID)
        If ExitProgramOnError = True Then Utility.ExitProgram()
    End Sub

    Public Shared Sub Debug(ByVal CLASSNAME As String, ByVal METHODNAME As String, ByVal INSTANCE As String, ByVal message As String)
        Dim ID = CLASSNAME & SLH & METHODNAME & SLH & INSTANCE
        Send(ID & L_BRC & message & R_BRC)
        If ShowPopups = True Then MsgBox(ID & L_BRC & message & R_BRC)
        If ExitProgramOnError = True Then Utility.ExitProgram()
    End Sub

    Public Shared Sub Debug(ByVal CLASSNAME As String, ByVal METHODNAME As String, ByVal INSTANCE As String, ByVal message As String, ByVal boolPopUp As Boolean, ByVal boolExit As Boolean)
        Dim ID = CLASSNAME & SLH & METHODNAME & SLH & INSTANCE
        Send(ID & L_BRC & message & R_BRC)
        If boolPopUp = True Then MsgBox(ID & L_BRC & message & R_BRC)
        If boolExit = True Then Utility.ExitProgram()
    End Sub

    Private Shared Sub Send(ByVal message As String)
        If Utility.IsEmpty(Report()) = False Then message = message & vbCrLf & Report
        Utility.TextWriter(Debugfile(), Utility.TimeStamp() & vbCrLf & message & Buffer(), True)
    End Sub

    Public Shared Sub Message(ByVal text As String)
        MsgBox(text)
    End Sub

    Private Shared Property Stamps() As New List(Of String)
    Public Shared Sub Stamp(ByVal ID As Double)
        If ID = 0 Then Stamps.Clear()
        Stamps().Add("(" & ID.ToString & ")" & Utility.TimeStamp(True))
    End Sub

    Public Shared ReadOnly Property PrintStamp()
        Get
            Return Utility.Join(Stamps(), vbCrLf)
        End Get
    End Property



End Class

Public Class Delay

    Public Sub New()
    End Sub

    Public Sub New(ByVal delay As ULong)
        Me.Interval() = 1
        Me.Delay() = delay
    End Sub

    Public Sub New(ByVal delay As ULong, ByVal interval As Double)
        Me.Interval() = interval
        Me.Delay() = delay
    End Sub

    WithEvents LocalTimer As New System.Timers.Timer
    Public Event Elapsed()
    Public Property IsWorking() As Boolean = False

    Public Sub Start()
        Count() = 0
        LocalTimer.Start()
    End Sub

    Public Sub Start(ByVal delay As ULong)
        Me.Delay() = delay
        Me.Interval() = 1
        Start()
    End Sub

    Public Sub [Stop]()
        LocalTimer.Stop()
    End Sub


    'Set Delay to Nothing to set Timer to Millisecond mode
    Public Property Delay() As ULong = 0
    Public Property Interval() As Double
        Get
            Return LocalTimer.Interval()
        End Get
        Set(value As Double)
            LocalTimer.Interval() = value
        End Set
    End Property

    Private Property Count() As ULong = 0

    Private Sub Delay_Time() Handles LocalTimer.Elapsed

        If Count >= Delay() Then
            IsWorking() = False
            Count = 0
            RaiseEvent Elapsed()
            LocalTimer.Stop()
        End If

        Count += 1
    End Sub


End Class

Public Class Timer

    Public Sub New()
    End Sub

    Public Sub New(ByVal interval As ULong)
        Me.Interval() = interval
        IsSecondTimer() = False
    End Sub

    Public Sub New(ByVal day As Integer, ByVal hour As Integer, ByVal minute As Integer, ByVal second As Integer)
        'Used in the timer
        Me.Days() = day
        Me.Hours() = hour
        Me.Minutes() = minute
        Me.Seconds() = second
        Me.Interval() = 1000
        IsSecondTimer() = True
    End Sub

    WithEvents LocalTimer As New System.Timers.Timer

    Public Property Days() As Integer = 0
    Public Property Hours() As Integer = 0
    Public Property Minutes() As Integer = 0
    Public Property Seconds() As Integer = 0

    Public Event Tick()
    Public Property IsWorking() As Boolean = False
    Public Property IsSecondTimer() As Boolean = False

    Public Sub Start()
        IsWorking() = True
        cDays = 0
        cHours = 0
        cMinutes = 0
        cSeconds = 0
        LocalTimer.Start()
    End Sub

    Public Sub [Stop]()
        LocalTimer.Stop()
        IsWorking() = False
    End Sub

    Public Sub Sticky(ByVal interval As Double)
        Me.Interval() = interval
        If IsWorking() = False Then Start()
        Do Until IsWorking() = False
            Utility.Sleep(interval)
        Loop
    End Sub

    Public Property Interval() As Double
        Get
            Return LocalTimer.Interval()
        End Get
        Set(value As Double)
            LocalTimer.Interval() = value
        End Set
    End Property

    Private Property cSeconds() As Integer = 0
    Private Property cMinutes() As Integer = 0
    Private Property cHours() As Integer = 0
    Private Property cDays() As Integer = 0

    Private Sub Delay_Time() Handles LocalTimer.Elapsed

        If IsSecondTimer() = True Then

            If Me.Days() = cDays Then
                If Me.Hours() = cHours Then
                    If Me.Minutes() = cMinutes Then
                        If Me.Seconds() = cSeconds Then
                            cDays = 0
                            cHours = 0
                            cMinutes = 0
                            cSeconds = 0
                            RaiseEvent Tick()
                        End If
                    End If
                End If
            End If

            cSeconds += 1

            If cHours = 24 Then
                cDays += 1
                cHours = 0
            End If

            If cMinutes = 60 Then
                cHours += 1
                cMinutes = 0
            End If

            If cSeconds = 60 Then
                cMinutes += 1
                cSeconds = 0
            End If

        Else
            RaiseEvent Tick()
        End If

    End Sub

End Class

Public Class Text

    Property m_original As String = ""
    Private Property m_array As String()

    Public Function Copy() As Text
        Dim newText As New Text
        newText.Original() = Original()
        Return newText
    End Function

    Public Property Original() As String
        Get
            Return m_original
        End Get
        Set(value As String)
            m_original = value
            m_array = Utility.ToArray(Utility.SingleSpace(value), Utility.SPACE)
        End Set
    End Property

    Public Sub New()

    End Sub

    Public Sub New(ByVal original As String)
        Me.Original() = original
    End Sub

    Public ReadOnly Property SingleSpace As String
        Get
            Return Utility.SingleSpace(Original())
        End Get
    End Property

    Public ReadOnly Property Trim() As String
        Get
            Return Utility.Trim(Original)
        End Get
    End Property

    Public ReadOnly Property Encrypt(ByVal key As String) As String
        Get
            Return Utility.Encrypt(Original, key)
        End Get
    End Property

    Public ReadOnly Property Decrypt(ByVal key As String) As String
        Get
            Return Utility.Decrypt(Original, key)
        End Get
    End Property

    Public ReadOnly Property Array() As String()
        Get
            Return m_array
        End Get
    End Property

    Public ReadOnly Property List() As List(Of String)
        Get
            Return New List(Of String)(m_array)
        End Get
    End Property

    Public ReadOnly Property Index(ByVal intIndex As Integer) As String
        Get
            Return Utility.GetIndex(m_array, intIndex, "")
        End Get
    End Property

    Public ReadOnly Property ToUpper() As String
        Get
            Return Utility.ToUpper(Original)
        End Get
    End Property

    Public ReadOnly Property ToLower() As String
        Get
            Return Utility.ToLower(Original)
        End Get
    End Property

    Public ReadOnly Property Item(ByVal index As Integer) As String
        Get
            Return Array(index)
        End Get
    End Property

    Public Sub Remove(ByVal text As String)
        Original() = Utility.Remove(Original, text)
    End Sub

    Public Sub Replace(ByVal search As String, ByVal substitute As String)
        Original() = Utility.Replace(Original(), search, substitute)
    End Sub

    Public ReadOnly Property WCount() As Integer
        Get
            Return m_array.Length()
        End Get
    End Property

    Public ReadOnly Property Length() As Integer
        Get
            Return Original.Length()
        End Get
    End Property

    Public ReadOnly Property Search(ByVal find As String, ByVal eGate As Gate) As Boolean
        Get
            Return Utility.Search(Original, find, eGate)
        End Get
    End Property

    Public ReadOnly Property Search(ByVal find As String(), ByVal eGate As Gate) As Boolean
        Get
            Return Utility.Search(Original, find, eGate)
        End Get
    End Property

End Class

Public Class Data

    'This is a 1-based count system
    'indexes start at 1 rather than 0

#Region "Properties"
    Public Property XML() As New XmlSource
    Public Property Cells() As New List(Of Cell)
    Public ReadOnly Property CopyCells() As List(Of Cell)
        Get
            Dim newCells As New List(Of Cell)
            newCells.AddRange(Cells)
            Return newCells
        End Get
    End Property

    Public Function Copy() As Data
        Dim newData As New Data
        newData.Connection() = Connection()
        newData.Key() = Key()
        newData.Compress() = Compress()
        newData.Encrypt() = Encrypt()
        newData.IsNameOnlyLoaded() = IsNameOnlyLoaded()
        newData.IgnoreAddCheckExist() = IgnoreAddCheckExist()
        newData.Name() = Name()
        newData.XML() = XML.Copy()
        newData.Source() = Source().Copy()
        newData.Cells() = CopyCells()
        Return newData
    End Function

    Public Property Key() As String = "secret"
    Public Property Compress() As Boolean = False
    Public Property Encrypt() As Boolean = False
    Public Property IsNameOnlyLoaded() As Boolean = False
    Public Property IgnoreCase() As Boolean = True

    Private m_Connection As String = ""
    Public Property Connection() As String
        Get
            Return m_Connection
        End Get
        Set(value As String)
            m_Connection = value
            Name() = Utility.GetFileName(value, False)
        End Set
    End Property

    Public Property Name() As String = ""


#End Region

#Region "CONSTANTS"
    Private Const EXTEN_FILE As String = ".ath"
    Private Const FWD_SLH As String = "/"
    Private Const NULL As String = "NULL"
    Private Const ENTRY_NAME As String = "E"
    Private Const META_NAME As String = "M"
    Private Const ROW_NAME As String = "R"
    Private Const DATABASE_NAME As String = "D"
    Private Const TABLE_NAME As String = "T"
    Private Const COLUMN_NAME As String = "C"
    Private Const DELIM As String = "|"
    Private Const DELIM_REPLACE As String = "[BAR]"
    Private Const CARRIAGE_REPLACE As String = "[CrLf]"
    Private Const REVERSE As String = "-r"


#End Region

#Region "Private"

    Private Function Replace_Tags(ByVal textToClean As String, ByVal toTags As Boolean)
        If textToClean = Nothing Then textToClean = ""

        If toTags = False Then
            textToClean = Utility.Replace(textToClean, vbCrLf, CARRIAGE_REPLACE)
            textToClean = Utility.Replace(textToClean, vbLf, CARRIAGE_REPLACE)
            textToClean = Utility.Replace(textToClean, DELIM, DELIM_REPLACE)
        Else
            textToClean = Utility.Replace(textToClean, CARRIAGE_REPLACE, vbCrLf)
            textToClean = Utility.Replace(textToClean, DELIM_REPLACE, DELIM)
        End If

        Return textToClean
    End Function

    Private Function Replace_Name(ByVal strName As String, ByVal ToFile As Boolean) As String
        If strName = Nothing Then strName = ""

        If ToFile = True Then
            strName = Utility.Replace(strName, "/", "[" & Asc("/") & "]")
            strName = Utility.Replace(strName, "\", "[" & Asc("\") & "]")
            strName = Utility.Replace(strName, "*", "[" & Asc("*") & "]")
            strName = Utility.Replace(strName, "?", "[" & Asc("?") & "]")
            strName = Utility.Replace(strName, "<", "[" & Asc("<") & "]")
            strName = Utility.Replace(strName, ">", "[" & Asc(">") & "]")
            strName = Utility.Replace(strName, "|", "[" & Asc("|") & "]")
            strName = Utility.Replace(strName, ":", "[" & Asc("|") & "]")
            strName = Utility.Replace(strName, Chr(34), "(34)")
        Else
            strName = Utility.Replace(strName, "[" & Asc("/") & "]", "/")
            strName = Utility.Replace(strName, "[" & Asc("\") & "]", "\")
            strName = Utility.Replace(strName, "[" & Asc("*") & "]", "*")
            strName = Utility.Replace(strName, "[" & Asc("?") & "]", "?")
            strName = Utility.Replace(strName, "[" & Asc("<") & "]", "<")
            strName = Utility.Replace(strName, "[" & Asc(">") & "]", ">")
            strName = Utility.Replace(strName, "[" & Asc("|") & "]", "|")
            strName = Utility.Replace(strName, "[34]", Chr(34))
        End If

        Return strName
    End Function

    Private Function Search(ByVal text As String, ByVal find As String)
        Return Utility.Search(text, find, True, Gate.eEQUALS)
    End Function
    Private Function Search(ByVal text As String, ByVal find As String, ByVal eGate As Gate)
        Return Utility.Search(text, find, True, eGate)
    End Function
    Private Function Search(ByVal text As String, ByVal find As String, ByVal arguments As String(), ByVal eGate As Gate)

        Dim sGate = Utility.GetIndex(arguments, 0)
        Dim reverse = Utility.GetIndex(arguments, 1)

        If Utility.Search(reverse, Data.REVERSE, Gate.eEQUALS) = True Then
            Return Utility.Search(find, text, True, Utility.GetGate(sGate, eGate))
        Else
            Return Utility.Search(text, find, True, Utility.GetGate(sGate, eGate))
        End If

    End Function

    Private Function Search(ByVal text As String, ByVal find As String(), ByVal eGate As Gate)
        Return Utility.Search(text, find, True, eGate)
    End Function
    Private Function Search(ByVal text As String(), ByVal find As String(), ByVal eGate As Gate)
        Return Utility.Search(text, find, IgnoreCase(), eGate)
    End Function
    Private Function Search(ByVal text As List(Of String), ByVal find As String)
        Return Utility.Search(text, find, Gate.eEQUALS)
    End Function


    '''
    '''database
    ''' '''table
    ''' ''' ''' column
    ''' ''' ''' ''' row
    ''' ''' ''' ''' ''' entry
    ''' ''' ''' ''' ''' meta

#End Region


#Region "LOAD BUFFER ZIP"
    Private Property Source() As New TypeMemory

    Public Sub LoadSource()
        Source() = Utility.ZipTextReader(Connection())
    End Sub

    'Source Load
    Private Function LoadSourceText(ByVal address As String()) As String
        Dim ID = Utility.Join(address, FWD_SLH)
        Dim text = Source.Index(ID)

        If Encrypt() = True Then
            text = Utility.Decrypt(text, Key())
        End If
        Return text
    End Function


    Public Sub LoadNamesFromSource()
        IgnoreAddCheckExist() = False

        For Each item In Source.Items
            Dim address = Utility.ToArray(item.Index, FWD_SLH)
            LoadFromZip(address)
        Next

        IsNameOnlyLoaded() = True
    End Sub
    Public Sub LoadColumnFromSource(ByVal database As String, ByVal table As String, ByVal column As String)

        database = Utility.Trim(Replace_Name(database, False))
        table = Utility.Trim(Replace_Name(table, False))
        column = Utility.Trim(Replace_Name(column, False))

        Dim address = {database, table, column & EXTEN_FILE}
        LoadFromZip(address, LoadSourceText(address))
    End Sub
    Public Sub LoadTableFromSource(ByVal database As String, ByVal table As String)

        database = Utility.Trim(Replace_Name(database, False))
        table = Utility.Trim(Replace_Name(table, False))

        Dim indexes = SourceGetFiles({database, table})

        Dim noEntries As New List(Of String())
        For Each index In indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next

    End Sub
    Public Sub LoadDatabaseFromSource(ByVal database As String)
        database = Utility.Trim(Replace_Name(database, False))
        Dim indexes = SourceGetFiles({database})

        Dim noEntries As New List(Of String())
        For Each index In indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next
    End Sub
    Public Sub LoadClusterFromSource()

        Dim indexes = SourceGetFiles()

        Dim noEntries As New List(Of String())
        For Each index In indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next
    End Sub

    'Source General
    Public Sub Save()
        RemoveDeletedFromZip()
        Utility.ZipTextWriter(Connection(), Source())
    End Sub
    Public Function SourceWriter(ByVal entryName As String()) As TypeMemory
        'used to make directory in zip

        'Try
        If Utility.IsEmpty(entryName) = False Then
            Dim ID = Join(entryName, FWD_SLH) & FWD_SLH

            If Source.DoesIndexExist(ID) = True Then
                Source.Index(ID) = Nothing
                Source.Tag(ID) = Nothing
            Else
                Source.Add(ID, Nothing, Nothing)
            End If

        End If

        'Catch : End Try
        Return Source
    End Function
    Public Function SourceWriter(ByVal entryName As String(), ByVal entryContent As Object) As TypeMemory

        'Try
        If Utility.IsEmpty(entryName) = False Then
            Dim ID = Join(entryName, FWD_SLH)

            If Source.DoesIndexExist(ID) = True Then
                Source.Index(ID) = entryContent
                Source.Tag(ID) = Utility.GetLast(entryName)
            Else
                Source.Add(ID, Utility.GetLast(entryName), entryContent)
            End If

        End If

        'Catch : End Try
        Return Source
    End Function
    Private Function SourceGetFiles(ByVal address As String()) As List(Of String)
        Dim Results As New List(Of String)

        'Try
        For Each entry In Source.Items
            Dim Items = Utility.ToArray(entry.Index, FWD_SLH)

            If Search(Items, address, Gate.eSTART) = True Then
                Results.Add(entry.Index())
            End If
        Next
        'Catch : End Try

        Return Results
    End Function
    Private Function SourceGetFiles() As List(Of String)
        Dim Results As New List(Of String)

        'Try
        For Each entry In Source.Items
            Dim Items = Utility.ToArray(entry.Index, FWD_SLH)

            If entry.Entry() Is Nothing Then Continue For
            Results.Add(entry.Index())
        Next
        'Catch : End Try

        Return Results
    End Function

    'Source Save
    Public Sub SaveToSource()

        For Each Database1 In DatabasesList()
            DeleteDatabaseFromSource(Database1)

            Dim TL = TableList(Database1)
            If TL.Count() > 0 Then

                For Each Table1 In TL

                    Dim CL = ColumnList(Database1, Table1)
                    If CL.Count() > 0 Then

                        For Each Column1 In CL
                            SaveColumnToSource_Direct(Database1, Table1, Column1)
                        Next
                    Else
                        CreateTableInSource(Database1, Table1)
                    End If

                Next

            Else
                CreateDatabaseInSource(Database1)
            End If

        Next

    End Sub
    Public Sub SaveNamesToSource()

        For Each Database1 In DatabasesList()
            DeleteDatabaseFromSource(Database1)

            Dim TL = TableList(Database1)
            If TL.Count() > 0 Then

                For Each Table1 In TL

                    Dim CL = ColumnList(Database1, Table1)
                    If CL.Count() > 0 Then

                        For Each Column1 In CL
                            CreateColumnInSource(Database1, Table1, Column1)
                        Next
                    Else
                        CreateTableInSource(Database1, Table1)
                    End If

                Next

            Else
                CreateDatabaseInSource(Database1)
            End If

        Next

    End Sub
    Private Sub SaveColumnToSource_Direct(ByVal database As String, ByVal table As String, ByVal column As String)
        Dim Text = ""

        For Each Entry In Cells
            If Entry.Remove() = True Then Continue For

            If Search(Entry.Database(), database) = True Then
                If Search(Entry.Table(), table) = True Then
                    If Search(Entry.Column(), column) = True Then
                        Text = Text + EntryToZip(Entry) + vbCrLf
                    End If
                End If
            End If
        Next

        SourceWriter({database, table, column}, Text)
    End Sub
    Public Sub SaveColumnToSource(ByVal database As String, ByVal table As String, ByVal column As String)
        DeleteColumnFromSource(database, table, column)
        SaveColumnToSource_Direct(database, table, column)
    End Sub
    Public Sub SaveTableToSource(ByVal database As String, ByVal table As String)

        For Each Database1 In DatabasesList()

            If Search(Database1, database) = True Then

                For Each Table1 In TableList(Database1)
                    If Search(Table1, table) = True Then

                        DeleteTableFromSource(Database1, Table1)

                        Dim CL = ColumnList(Database1, Table1)
                        If CL.Count() > 0 Then

                            For Each Column1 In CL
                                SaveColumnToSource_Direct(Database1, Table1, Column1)
                            Next
                        Else
                            CreateTableInSource(Database1, Table1)
                        End If
                    End If
                Next
            End If
        Next

    End Sub
    Public Sub SaveDatabaseToSource(ByVal database As String)

        For Each Database1 In DatabasesList()
            If Search(Database1, database) = True Then

                DeleteDatabaseFromSource(Database1)

                Dim TL = TableList(Database1)
                If TL.Count() > 0 Then

                    For Each Table1 In TL

                        Dim CL = ColumnList(Database1, Table1)
                        If CL.Count() > 0 Then

                            For Each Column1 In CL
                                SaveColumnToSource_Direct(Database1, Table1, Column1)
                            Next
                        Else
                            CreateTableInSource(Database1, Table1)
                        End If

                    Next

                Else
                    CreateDatabaseInSource(Database1)
                End If
            End If
        Next

    End Sub


    'Source Create
    Public Sub CreateDatabaseInSource(ByVal database As String)
        SourceWriter({Replace_Name(database, True)})
    End Sub
    Public Sub CreateTableInSource(ByVal database As String, ByVal table As String)
        SourceWriter({Replace_Name(database, True), Replace_Name(table, True)})
    End Sub
    Public Sub CreateColumnInSource(ByVal database As String, ByVal table As String, ByVal column As String, Optional ByVal text As String = "")
        If Encrypt() = True Then
            text = Utility.Encrypt(text, Key())
        End If

        SourceWriter({Replace_Name(database, True), Replace_Name(table, True), Replace_Name(column, True) & EXTEN_FILE}, text)
    End Sub


    'Source Delete
    Public Sub DeleteSourceEntries(ByVal entryName As String())
        Dim newSource As New TypeMemory

        'Try
        For Each entry In Source.Items
            Dim Items = Utility.ToArray(entry.Index, FWD_SLH)
            If Search(Items, entryName, Gate.eSTART) = False Then
                newSource.Add(entry)
            End If
        Next

        Source() = newSource
        'Catch : End Try

    End Sub

    Public Sub DeleteDatabaseFromSource(ByVal database As String)
        DeleteSourceEntries({Replace_Name(database, True)})
    End Sub
    Public Sub DeleteTableFromSource(ByVal database As String, ByVal table As String)
        DeleteSourceEntries({Replace_Name(database, True), Replace_Name(table, True)})
    End Sub
    Public Sub DeleteColumnFromSource(ByVal database As String, ByVal table As String, ByVal column As String)
        DeleteSourceEntries({Replace_Name(database, True), Replace_Name(table, True), Replace_Name(column, True) & EXTEN_FILE})
    End Sub


#End Region

#Region "SAVE AND LOAD ZIP"

    Private Function LoadZipText(ByVal address As String()) As String
        Dim text = Utility.ZipReader(Connection(), address)
        If Encrypt() = True Then
            text = Utility.Decrypt(text, Key())
        End If
        Return text
    End Function

    'Create Zip Only
    Public Sub CreateZip()
        Utility.ZipWriter(Connection(), {}, "")
    End Sub
    Public Sub CreateDatabaseInZip(ByVal database As String)
        Utility.ZipWriter(Connection(), {Replace_Name(database, True)})
    End Sub
    Public Sub CreateTableInZip(ByVal database As String, ByVal table As String)
        Utility.ZipWriter(Connection(), {Replace_Name(database, True), Replace_Name(table, True)})
    End Sub
    Public Sub CreateColumnInZip(ByVal database As String, ByVal table As String, ByVal column As String, Optional ByVal text As String = "")
        If Encrypt() = True Then
            text = Utility.Encrypt(text, Key())
        End If

        Utility.ZipWriter(Connection(), {Replace_Name(database, True), Replace_Name(table, True), Replace_Name(column, True) & EXTEN_FILE}, Utility.Trim(text, -1))
    End Sub

    'Delete Zip Only
    Private Sub DeleteZipEntries(ByVal address As String())
        Utility.DeleteZipEntries(Connection(), address, Gate.eSTART)
    End Sub
    Public Sub DeleteZip()
        Utility.FileDelete(Connection())
    End Sub
    Public Sub DeleteDatabaseFromZip(ByVal database As String)
        DeleteZipEntries({Replace_Name(database, True)})
    End Sub
    Public Sub DeleteTableFromZip(ByVal database As String, ByVal table As String)
        DeleteZipEntries({Replace_Name(database, True), Replace_Name(table, True)})
    End Sub
    Public Sub DeleteColumnFromZip(ByVal database As String, ByVal table As String, ByVal column As String)
        DeleteZipEntries({Replace_Name(database, True), Replace_Name(table, True), Replace_Name(column, True) & EXTEN_FILE})
    End Sub

    'DoesExist Zip Only
    Public Function DoesZipExist() As Boolean
        Return Utility.FileExists(Connection())
    End Function
    Public Function DoesDatabaseExistInZip(ByVal database As String) As Boolean
        Return Utility.DoesZipFolderExist(Connection(), {Replace_Name(database, True)})
    End Function
    Public Function DoesTableExistInZip(ByVal database As String, ByVal table As String) As Boolean
        Return Utility.DoesZipFolderExist(Connection(), {Replace_Name(database, True), Replace_Name(table, True)})
    End Function
    Public Function DoesColumnExistInZip(ByVal database As String, ByVal table As String, ByVal column As String) As Boolean
        Return Utility.DoesZipEntryExist(Connection(), {Replace_Name(database, True), Replace_Name(table, True), Replace_Name(column, True) & EXTEN_FILE})
    End Function


    'General
    Public Sub LoadClusterDirectZip()
        LoadDirectFromZip({})
    End Sub
    Public Sub LoadDatabaseDirectZip(ByVal database As String)
        LoadDirectFromZip({database})
    End Sub
    Public Sub LoadTableDirectZip(ByVal database As String, ByVal table As String)
        LoadDirectFromZip({database, table})
    End Sub
    Public Sub LoadColumnDirectZip(ByVal database As String, ByVal table As String, ByVal column As String)
        LoadDirectFromZip({database, table, column})
    End Sub


    Public Sub LoadDirectFromZip(ByVal address As String())

        If Utility.FileExists(Connection()) = False Then Exit Sub

        'Try
        Using zipStream As New IO.FileStream(Connection(), IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Read)
                Dim noEntries As New List(Of String())

                For Each entry In archive.Entries
                    Dim Items = Utility.ToArray(entry.FullName, FWD_SLH)

                    If Search(Items, address, Gate.eSTART) = True Then

                        If entry.Name().Length() > 0 Then
                            Dim text = ""
                            Using Reader As New IO.StreamReader(entry.Open())
                                text = Reader.ReadToEnd()
                            End Using

                            If Utility.IsEmpty(text) = False Then
                                LoadFromZip(Items, text)
                            Else
                                LoadFromZip(Items)
                            End If

                        Else
                            noEntries.Add(Items)
                        End If
                    End If
                Next
                For Each address In noEntries
                    LoadFromZip(address)
                Next

            End Using
        End Using
        'Catch : End Try

    End Sub
    Private Sub LoadFromZip(ByVal address As String())
        IgnoreAddCheckExist() = False

        Dim database = Utility.GetIndex(address, 0, "")
        Dim table = Utility.GetIndex(address, 1, "")
        Dim column = Utility.GetIndex(address, 2, "")

        column = Utility.RemoveExtension(column)

        If Utility.IsEmpty(column) = False Then
            AddColumnByName(database, table, column)
        ElseIf Utility.IsEmpty(table) = False Then
            AddTable(database, table)
        ElseIf Utility.IsEmpty(database) = False Then
            AddDatabase(database)
        End If

    End Sub
    Private Sub LoadFromZip(ByVal address As String(), ByVal text As String)
        IgnoreAddCheckExist() = True

        Dim TextArray = Utility.ToList(text, vbCrLf)
        Dim database = Utility.GetIndex(address, 0, "")
        Dim table = Utility.GetIndex(address, 1, "")
        Dim column = Utility.GetIndex(address, 2, "")

        column = Utility.RemoveExtension(column) 'Removes the file extension

        For Each Line In TextArray

            Dim InnerAddress = Utility.ToList(Line, DELIM)
            Dim EntryOrMeta = Utility.GetIndex(InnerAddress, 0, "")
            Dim row = Utility.GetIndex(InnerAddress, 1, "")
            Dim entry = Utility.GetIndex(InnerAddress, 2, "")

            If Utility.IsEmpty(EntryOrMeta) = False Then
                entry = Replace_Tags(entry, True)

                If Search(EntryOrMeta, ENTRY_NAME) = True Then
                    If DoesAnyEntryExist(database, table, column, row) = False Then
                        AddEntry(database, table, column, row, entry)
                    Else
                        'just in case a meta entry was added first then this will add the new entry next to it
                        UpdateEntry(database, table, column, row, entry, False)
                    End If
                ElseIf Search(EntryOrMeta, META_NAME) = True Then
                    If DoesAnyEntryExist(database, table, column, row) = True Then
                        If DoesAnyMetaExist(database, table, column, row) = False Then
                            Meta(database, table, column, row) = entry
                        End If
                    Else
                        AddMeta(database, table, column, row, entry)
                    End If
                End If

            End If

        Next

        IsNameOnlyLoaded() = False
        IgnoreAddCheckExist() = False
    End Sub



    'Load Zip Only
    Public Sub LoadNamesFromZip()

        Dim Indexes = Utility.ZipGetEntries(Connection(), False)
        IgnoreAddCheckExist() = False

        For Each index In Indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            LoadFromZip(address)
        Next

        IsNameOnlyLoaded() = True
    End Sub
    Public Sub LoadColumnFromZip(ByVal database As String, ByVal table As String, ByVal column As String)

        database = Utility.Trim(Replace_Name(database, False))
        table = Utility.Trim(Replace_Name(table, False))
        column = Utility.Trim(Replace_Name(column, False))

        Dim address = {database, table, column & EXTEN_FILE}
        LoadFromZip(address, LoadZipText(address))
    End Sub
    Public Sub LoadTableFromZip(ByVal database As String, ByVal table As String)

        database = Utility.Trim(Replace_Name(database, False))
        table = Utility.Trim(Replace_Name(table, False))

        Dim Indexes = Utility.ZipGetEntries(Connection(), {database, table})

        Dim noEntries As New List(Of String())
        For Each index In Indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next

    End Sub
    Public Sub LoadDatabaseFromZip(ByVal database As String)

        database = Utility.Trim(Replace_Name(database, False))
        Dim Indexes = Utility.ZipGetEntries(Connection(), {database})

        Dim noEntries As New List(Of String())
        For Each index In Indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next
    End Sub
    Public Sub LoadClusterFromZip()
        Dim Indexes = Utility.ZipGetEntries(Connection(), False)

        Dim noEntries As New List(Of String())
        For Each index In Indexes
            Dim address = Utility.ToArray(index, FWD_SLH)
            If address.Length = 3 Then
                LoadFromZip(address, LoadZipText(address))
            Else
                noEntries.Add(address)
            End If
        Next
        For Each address In noEntries
            LoadFromZip(address)
        Next
    End Sub

    'special
    Public Sub RemoveDeletedFromZip()
        'This can remove databases, tables, and columns that have been renamed 
        For Each Cell1 In Cells
            If Cell1.Remove() = True Then
                Utility.DeleteZipEntry(Connection(), Cell1.Address())
            End If
        Next
    End Sub
    Private Function EntryToZip(ByVal Entry As Cell) As String

        If Entry.Text() <> NULL Or Entry.Meta() <> NULL Then
            If Entry.Text() <> NULL Then
                Entry.Text() = Replace_Tags(Entry.Text(), False)
                Return ENTRY_NAME & DELIM & Entry.Row() & DELIM & Entry.Text()
            End If
            If Entry.Meta() <> NULL Then
                Entry.Meta() = Replace_Tags(Entry.Meta(), False)
                Return META_NAME & DELIM & Entry.Row() & DELIM & Entry.Meta()
            End If
        End If

        Return ""
    End Function


    'Save Zip Only
    Public Sub SaveToZip()
        RemoveDeletedFromZip()

        For Each Database1 In DatabasesList()
            DeleteDatabaseFromZip(Database1)

            Dim TL = TableList(Database1)
            If TL.Count() > 0 Then

                For Each Table1 In TL

                    Dim CL = ColumnList(Database1, Table1)
                    If CL.Count() > 0 Then

                        For Each Column1 In CL
                            SaveColumnToZip_Direct(Database1, Table1, Column1)
                        Next
                    Else
                        CreateTableInZip(Database1, Table1)
                    End If

                Next

            Else
                CreateDatabaseInZip(Database1)
            End If

        Next

    End Sub
    Public Sub SaveNamesToZip()
        RemoveDeletedFromZip()

        For Each Database1 In DatabasesList()
            DeleteDatabaseFromZip(Database1)

            Dim TL = TableList(Database1)
            If TL.Count() > 0 Then

                For Each Table1 In TL

                    Dim CL = ColumnList(Database1, Table1)
                    If CL.Count() > 0 Then

                        For Each Column1 In CL
                            CreateColumnInZip(Database1, Table1, Column1)
                        Next
                    Else
                        CreateTableInZip(Database1, Table1)
                    End If

                Next

            Else
                CreateDatabaseInZip(Database1)
            End If

        Next

    End Sub
    Private Sub SaveColumnToZip_Direct(ByVal database As String, ByVal table As String, ByVal column As String)
        Dim Text = ""

        For Each Entry In Cells
            If Entry.Remove() = True Then Continue For

            If Search(Entry.Database(), database) = True Then
                If Search(Entry.Table(), table) = True Then
                    If Search(Entry.Column(), column) = True Then
                        Text = Text + EntryToZip(Entry) + vbCrLf
                    End If
                End If
            End If
        Next


        CreateColumnInZip(database, table, column, Text)
    End Sub
    Public Sub SaveColumnToZip(ByVal database As String, ByVal table As String, ByVal column As String)
        DeleteColumnFromZip(database, table, column)
        SaveColumnToZip_Direct(database, table, column)
    End Sub
    Public Sub SaveTableToZip(ByVal database As String, ByVal table As String)

        For Each Database1 In DatabasesList()

            If Search(Database1, database) = True Then

                For Each Table1 In TableList(Database1)
                    If Search(Table1, table) = True Then

                        DeleteTableFromZip(Database1, Table1)

                        Dim CL = ColumnList(Database1, Table1)
                        If CL.Count() > 0 Then

                            For Each Column1 In CL
                                SaveColumnToZip_Direct(Database1, Table1, Column1)
                            Next
                        Else
                            CreateTableInZip(Database1, Table1)
                        End If
                    End If
                Next
            End If
        Next

    End Sub
    Public Sub SaveDatabaseToZip(ByVal database As String)

        For Each Database1 In DatabasesList()
            If Search(Database1, database) = True Then

                DeleteDatabaseFromZip(Database1)

                Dim TL = TableList(Database1)
                If TL.Count() > 0 Then

                    For Each Table1 In TL

                        Dim CL = ColumnList(Database1, Table1)
                        If CL.Count() > 0 Then

                            For Each Column1 In CL
                                SaveColumnToZip_Direct(Database1, Table1, Column1)
                            Next
                        Else
                            CreateTableInZip(Database1, Table1)
                        End If

                    Next

                Else
                    CreateDatabaseInZip(Database1)
                End If
            End If
        Next

    End Sub


    'Rename Zip Only
    Public Sub RenameZip(ByVal newZip_name As String)
        Utility.FileRename(Connection(), newZip_name)
    End Sub
    Public Sub RenameZipEntries(ByVal old_address As String(), ByVal new_address As String())
        Utility.RenameZipEntries(Connection(), old_address, new_address)
    End Sub
    Public Sub RenameDatabaseInZip(ByVal old_database As String, ByVal new_database As String)
        RenameZipEntries({Replace_Name(old_database, True)},
                         {Replace_Name(new_database, True)})
    End Sub
    Public Sub RenameTableInZip(ByVal old_database As String, ByVal new_database As String, ByVal old_table As String, ByVal new_table As String)
        RenameZipEntries({Replace_Name(old_database, True), Replace_Name(old_table, True)},
                        {Replace_Name(new_database, True), Replace_Name(new_table, True)})
    End Sub
    Public Sub RenameColumnInZip(ByVal old_database As String, ByVal new_database As String, ByVal old_table As String, ByVal new_table As String, ByVal old_column As String, ByVal new_column As String)
        RenameZipEntries({Replace_Name(old_database, True), Replace_Name(old_table, True), Replace_Name(old_column, True) & EXTEN_FILE},
                         {Replace_Name(new_database, True), Replace_Name(new_table, True), Replace_Name(new_column, True) & EXTEN_FILE})
    End Sub


    'Copy Zip Only
    Public Sub CopyZip(ByVal newZip As String)
        Utility.FileCopy(Connection(), newZip)
    End Sub
    Public Sub CopyZipEntries(ByVal old_address As String(), ByVal new_address As String())
        Utility.CopyZipEntries(Connection(), old_address, new_address)
    End Sub
    Public Sub CopyDatabaseInZip(ByVal old_database As String, ByVal new_database As String)
        CopyZipEntries({Replace_Name(old_database, True)},
                       {Replace_Name(new_database, True)})
    End Sub
    Public Sub CopyTableInZip(ByVal old_database As String, ByVal new_database As String, ByVal old_table As String, ByVal new_table As String)
        CopyZipEntries({Replace_Name(old_database, True) & FWD_SLH & Replace_Name(old_table, True)},
                       {Replace_Name(new_database, True) & FWD_SLH & Replace_Name(new_table, True)})
    End Sub
    Public Sub CopyColumnInZip(ByVal old_database As String, ByVal new_database As String, ByVal old_table As String, ByVal new_table As String, ByVal old_column As String, ByVal new_column As String)
        CopyZipEntries({Replace_Name(old_database, True), Replace_Name(old_table, True), Replace_Name(old_column, True) & EXTEN_FILE},
                       {Replace_Name(new_database, True), Replace_Name(new_table, True), Replace_Name(new_column, True) & EXTEN_FILE})
    End Sub

#End Region

#Region "SAVE AND LOAD XML"

    Public Sub LoadFromXml()
        XML.Compress() = Compress()
        XML.Encrypt() = Encrypt()
        XML.Key() = Key()
        XML.Loadfile(Connection())

        Dim allEntries As New List(Of Cell)
        Dim newEntry As New Cell

        For Each Node1 In XML.NodeList()
            IgnoreAddCheckExist() = False

            Dim database = Node1.Index(0)
            Dim table = Node1.Index(1)
            Dim column = Node1.Index(2)
            Dim row = Node1.Index(3)
            Dim EntryOrMeta = Node1.Index(4)
            Dim entry = Node1.InnerText(True)

            If Utility.IsEmpty(EntryOrMeta) = False Then
                IgnoreAddCheckExist() = True

                If EntryOrMeta = ENTRY_NAME Then
                    If DoesAnyEntryExist(database, table, column, row) = False Then
                        AddEntry(database, table, column, row, entry)
                    Else
                        'just in case a meta entry was added first then this will add the new entry next to it
                        UpdateEntry(database, table, column, row, entry, False)
                    End If
                ElseIf EntryOrMeta = META_NAME Then
                    If DoesAnyEntryExist(database, table, column, row) = True Then
                        If DoesAnyMetaExist(database, table, column, row) = False Then
                            Meta(database, table, column, row) = entry
                        End If
                    Else
                        AddMeta(database, table, column, row, entry)
                    End If
                End If
            ElseIf Utility.IsEmpty(row) = False Then
                AddColumnByName(database, table, column)
                AddRowByName(database, table, row)
            ElseIf Utility.IsEmpty(column) = False Then
                AddColumnByName(database, table, column)
            ElseIf Utility.IsEmpty(table) = False Then
                AddTable(database, table)
            ElseIf Utility.IsEmpty(database) = False Then
                AddDatabase(database)
            End If

        Next

        IgnoreAddCheckExist() = False
        IsNameOnlyLoaded() = False
    End Sub
    Public Sub LoadNamesFromXml()
        XML.Compress() = Compress()
        XML.Encrypt() = Encrypt()
        XML.Key() = Key()
        XML.Loadfile(Connection())
        IgnoreAddCheckExist() = True

        Dim allEntries As New List(Of Cell)
        Dim newEntry As New Cell

        For Each Node1 In XML.NodeList()

            Dim database = Node1.Index(0)
            Dim table = Node1.Index(1)
            Dim column = Node1.Index(2)
            Dim row = Node1.Index(3)
            Dim EntryOrMeta = Node1.Index(4)

            If Utility.IsEmpty(EntryOrMeta) = False Then

            ElseIf Utility.IsEmpty(row) = False Then
                AddColumnByName(database, table, column)
                AddRowByName(database, table, row)
            ElseIf Utility.IsEmpty(column) = False Then
                AddColumnByName(database, table, column)
            ElseIf Utility.IsEmpty(table) = False Then
                AddTable(database, table)
            ElseIf Utility.IsEmpty(database) = False Then
                AddDatabase(database)
            End If

        Next

        IgnoreAddCheckExist() = False
        IsNameOnlyLoaded() = True
    End Sub


    Public Sub SaveToXml()
        XML.Clear()
        XML.Compress() = Compress()
        XML.Encrypt() = Encrypt()
        XML.Key() = Key()

        For Each Entry In Cells
            CreatingNodesXml(Entry)
        Next

        XML.Save(Connection())
    End Sub
    Public Sub SaveColumnToXml(ByVal database As String, ByVal table As String, ByVal column As String)
        XML.Clear()

        For Each Entry In Cells
            If Search(Entry.Database(), database) = True Then
                If Search(Entry.Table(), table) = True Then
                    If Search(Entry.Column(), column) = True Then
                        CreatingNodesXml(Entry)
                    End If
                End If
            End If
        Next

        XML.Save(Connection())
    End Sub
    Public Sub SaveTableToXml(ByVal database As String, ByVal table As String)
        XML.Clear()

        For Each Entry In Cells
            If Search(Entry.Database(), database) = True Then
                If Search(Entry.Table(), table) = True Then
                    CreatingNodesXml(Entry)
                End If
            End If
        Next

        XML.Save(Connection())
    End Sub
    Public Sub SaveDatabaseToXml(ByVal database As String)
        XML.Clear()

        For Each Entry In Cells
            If Search(Entry.Database(), database) = True Then
                CreatingNodesXml(Entry)
            End If
        Next

        XML.Save(Connection())
    End Sub


    Private Sub CreatingNodesXml(ByVal Entry As Cell)

        If Entry.Remove() = True Then Exit Sub
        If Utility.IsEmpty(Entry.Database()) = False And Entry.Database() <> NULL Then

            If (Utility.IsEmpty(Entry.Text) = False And Entry.Text <> NULL) Or (Utility.IsEmpty(Entry.Meta) = False And Entry.Meta <> NULL) Then
                If Entry.Text <> NULL Then
                    XML.Create({Entry.Database(), Entry.Table(), Entry.Column(), Entry.Row(), ENTRY_NAME}, Entry.Text)
                End If

                If Entry.Meta <> NULL Then
                    XML.Create({Entry.Database(), Entry.Table(), Entry.Column(), Entry.Row(), META_NAME}, Entry.Meta())
                End If
                Exit Sub
            End If

            If Utility.IsEmpty(Entry.Row) = False And Entry.Row <> NULL Then
                XML.Create({Entry.Database(), Entry.Table(), Entry.Column(), Entry.Row()}, "")
                Exit Sub
            End If

            If Utility.IsEmpty(Entry.Column) = False And Entry.Column <> NULL Then
                XML.Create({Entry.Database(), Entry.Table(), Entry.Column()}, "")
                Exit Sub
            End If

            If Utility.IsEmpty(Entry.Table) = False And Entry.Table <> NULL Then
                XML.Create({Entry.Database(), Entry.Table()}, "")
                Exit Sub
            End If

            If Utility.IsEmpty(Entry.Database) = False And Entry.Database <> NULL Then
                XML.Create({Entry.Database()}, "")
                Exit Sub
            End If

        End If

    End Sub


    'Rename Xml Only
    Public Sub RenameXml(ByVal newXml_name As String)
        Utility.FileRename(Connection(), newXml_name)
    End Sub

    'Copy Xml Only
    Public Sub CopyXml(ByVal newXml As String)
        Utility.FileCopy(Connection(), newXml)
    End Sub

    'Create Xml
    Public Sub CreateXml()
        Utility.ZipWriter(Connection(), {}, "")
    End Sub


#End Region


#Region "Database Tools"

    Public Sub Clear()
        Cells.Clear()
    End Sub

    Public Sub Clean()
        'Cleans Cells of any Entry set to be removed
        Dim allCells As New List(Of Cell)

        For Each Cell1 In Cells
            If Cell1.Remove() = False Then
                allCells.Add(Cell1)
            End If
        Next

        Cells = allCells
    End Sub

    Public Function PrintAllCells() As List(Of String)
        Dim Results As New List(Of String)

        For Each cell In Cells
            Dim line = ""
            If cell.Remove() = True Then
                line = "REMOVE: " & DATABASE_NAME & DELIM & cell.Database & DELIM & TABLE_NAME & DELIM & cell.Table & DELIM & COLUMN_NAME & DELIM & cell.Column
            Else
                line = DATABASE_NAME & DELIM & cell.Database & DELIM & TABLE_NAME & DELIM & cell.Table & DELIM & COLUMN_NAME & DELIM & cell.Column & DELIM & ROW_NAME & DELIM & cell.Row & DELIM & ENTRY_NAME & DELIM & cell.Text & DELIM & META_NAME & DELIM & cell.Meta
            End If

            Results.Add(line)
        Next

        Return Results
    End Function


#Region "Search"

    Public Function SearchDatabases(ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Text(), findentry, eGate) = True Then
                newCells.Add(iCell)
            End If
        Next

        Return newCells
    End Function

    Public Function SearchByDatabase(ByVal database As String, ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Database(), database) = True Then
                If Search(iCell.Text(), findentry, eGate) = True Then
                    newCells.Add(iCell)
                End If
            End If
        Next

        Return newCells
    End Function

    Public Function SearchByTable(ByVal database As String, ByVal table As String, ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Database(), database) = True Then
                If Search(iCell.Table(), table) = True Then
                    If Search(iCell.Text(), findentry, eGate) = True Then
                        newCells.Add(iCell)
                    End If
                End If
            End If
        Next

        Return newCells
    End Function

    Public Function SearchByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Database(), database) = True Then
                If Search(iCell.Table(), table) = True Then
                    If Search(iCell.Column(), column) = True Then
                        If Search(iCell.Text(), findentry, eGate) = True Then
                            newCells.Add(iCell)
                        End If
                    End If
                End If
            End If
        Next

        Return newCells
    End Function

    Public Function SearchByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Database(), database) = True Then
                If Search(iCell.Table(), table) = True Then
                    If Search(iCell.Row(), row) = True Then
                        If Search(iCell.Text(), findentry, eGate) = True Then
                            newCells.Add(iCell)
                        End If
                    End If
                End If
            End If
        Next

        Return newCells
    End Function

    Public Function SearchByMeta(ByVal database As String, ByVal table As String, ByVal meta As String, ByVal findentry As String, ByVal eGate As Gate) As List(Of Cell)
        Dim newCells As New List(Of Cell)

        For Each iCell In Cells
            If Search(iCell.Database(), database) = True Then
                If Search(iCell.Table(), table) = True Then
                    If Search(iCell.Meta(), meta) = True Then
                        If Search(iCell.Text(), findentry, eGate) = True Then
                            newCells.Add(iCell)
                        End If
                    End If
                End If
            End If
        Next

        Return newCells
    End Function

#End Region

#Region "Add"

    Public Property IgnoreAddCheckExist() As Boolean = False

    Public Sub AddDatabase(ByVal database As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesDatabaseExistByName(database) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database))
    End Sub

    Public Sub AddTable(ByVal database As String, ByVal table As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesTableExistByName(database, table) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table))
    End Sub


    Public Sub AddRowByName(ByVal database As String, ByVal table As String, ByVal row As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesRowExistByName(database, table, row) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table, NULL, row))
    End Sub

    Public Sub AddRowByColumns(ByVal database As String, ByVal table As String, ByVal colPairs As Pairs, Optional ByVal ignorecheck As Boolean = False)
        colPairs.RemoveDuplicatesByName()

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesRowExist(database, table, colPairs, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountOfLargestColumn(database, table) + 1).ToString()

        For Each par In colPairs.Items
            Cells.Add(New Cell(database, table, par.Name(), Count, par.GetString()))
        Next

    End Sub

    Public Sub AddRowByColumns(ByVal database As String, ByVal table As String, ByVal row As String, ByVal colPairs As Pairs, Optional ByVal ignorecheck As Boolean = False)
        colPairs.RemoveDuplicatesByName()

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesRowExistByName(database, table, row) = True Then
                Exit Sub
            End If

            If DoesRowExist(database, table, colPairs, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        For Each par In colPairs.Items
            Cells.Add(New Cell(database, table, par.Name(), row, par.GetString()))
        Next

    End Sub

    Public Sub AddRowByEntries(ByVal database As String, ByVal table As String, ByVal row As String, ByVal Entries As String(), Optional ByVal ignorecheck As Boolean = False)

        Dim Count = (CountEntriesByRow(database, table, row) + 1).ToString()

        For Each Entry In Entries
            If IgnoreAddCheckExist() = False And ignorecheck = False Then
                If DoesEntryExistInRow(database, table, row, Entry, Gate.eEQUALS) = False Then
                    Cells.Add(New Cell(database, table, Count.ToString(), row, Entry))
                    Count = Count + 1
                End If
            Else
                Cells.Add(New Cell(database, table, Count.ToString(), row, Entry))
                Count = Count + 1
            End If
        Next

    End Sub


    Public Sub AddColumnByName(ByVal database As String, ByVal table As String, ByVal column As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesColumnExistByName(database, table, column) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table, column, NULL))
    End Sub

    Public Sub AddColumnByRows(ByVal database As String, ByVal table As String, ByVal rowPairs As Pairs, Optional ByVal ignorecheck As Boolean = False)
        rowPairs.RemoveDuplicatesByName()

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesColumnExist(database, table, rowPairs, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountAllColumns(database, table) + 1).ToString()

        For Each par In rowPairs.Items
            Cells.Add(New Cell(database, table, Count, par.Name(), par.GetString()))
        Next

    End Sub

    Public Sub AddColumnByRows(ByVal database As String, ByVal table As String, ByVal column As String, ByVal rowPairs As Pairs, Optional ByVal ignorecheck As Boolean = False)
        rowPairs.RemoveDuplicatesByName()

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesColumnExistByName(database, table, column) = True Then
                Exit Sub
            End If

            If DoesColumnExist(database, table, rowPairs, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        For Each par In rowPairs.Items
            Cells.Add(New Cell(database, table, column, par.Name(), par.GetString()))
        Next

    End Sub

    Public Sub AddColumnByEntries(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entries As String(), Optional ByVal ignorecheck As Boolean = False)

        Dim Count = (CountEntriesByColumn(database, table, column) + 1).ToString()

        For Each Entry In entries
            If IgnoreAddCheckExist() = False And ignorecheck = False Then
                If DoesEntryExistInColumn(database, table, column, Entry, Gate.eEQUALS) = False Then
                    Cells.Add(New Cell(database, table, column, Count.ToString(), Entry))
                    Count = Count + 1
                End If
            Else
                Cells.Add(New Cell(database, table, column, Count.ToString(), Entry))
                Count = Count + 1
            End If
        Next

    End Sub


    Public Sub AddEntryByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExistInColumn(database, table, column, entry, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountEntriesByColumn(database, table, column) + 1).ToString()
        Cells.Add(New Cell(database, table, column, Count, entry))
    End Sub

    Public Sub AddEntryByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal meta As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExistInColumn(database, table, column, entry, meta, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountEntriesByColumn(database, table, column) + 1).ToString()
        Cells.Add(New Cell(database, table, column, Count, entry, meta))
    End Sub


    Public Sub AddEntryByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExistInRow(database, table, row, entry, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountEntriesByRow(database, table, row) + 1).ToString()
        Cells.Add(New Cell(database, table, Count, row, entry))
    End Sub

    Public Sub AddEntryByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal meta As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExistInRow(database, table, row, entry, meta, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Dim Count = (CountEntriesByRow(database, table, row) + 1).ToString()
        Cells.Add(New Cell(database, table, Count, row, entry, meta))
    End Sub


    Public Sub AddEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExist(database, table, column, row, entry, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table, column, row, entry))
    End Sub

    Public Sub AddEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String, ByVal meta As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesEntryExist(database, table, column, row, entry, meta, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table, column, row, entry, meta))
    End Sub

    Public Sub AddMeta(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal meta As String, Optional ByVal ignorecheck As Boolean = False)

        If IgnoreAddCheckExist() = False And ignorecheck = False Then
            If DoesMetaExist(database, table, column, row, meta, Gate.eEQUALS) = True Then
                Exit Sub
            End If
        End If

        Cells.Add(New Cell(database, table, column, row, NULL, meta))
    End Sub


    Public Sub AddEntries(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entries As String(), Optional ByVal ignorecheck As Boolean = False)

        For Each entry In entries
            AddEntry(database, table, column, row, entry, ignorecheck)
        Next
    End Sub

    Public Sub AddEntries(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entries As String(), ByVal metas As String(), Optional ByVal ignorecheck As Boolean = False)

        If metas.Length < entries.Length Then
            For i = 0 To metas.Length - 1
                AddEntry(database, table, column, row, entries(i), metas(i), ignorecheck)
            Next
        Else
            For i = 0 To entries.Length - 1
                AddEntry(database, table, column, row, entries(i), metas(i), ignorecheck)
            Next
        End If
    End Sub

    Public Sub AddMetas(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal metas As String(), Optional ByVal ignorecheck As Boolean = False)

        For Each meta_1 In metas
            AddMeta(database, table, column, row, meta_1, ignorecheck)
        Next
    End Sub

#End Region

#Region "Remove"

    Public Sub RemoveDatabases()
        For Each Cell1 In Cells
            Cell1.Remove() = True
        Next
    End Sub

    Public Sub RemoveDatabase(ByVal database As String)
        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                Cell1.Remove = True
            End If
        Next
    End Sub


    Public Sub RemoveTables(ByVal database As String)
        RemoveDatabase(database)
        Cells.Add(New Cell(database))
    End Sub

    Public Sub RemoveTable(ByVal database As String, ByVal table As String)
        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    Cell1.Remove() = True
                End If
            End If
        Next

        AddDatabase(database)
    End Sub


    Public Sub RemoveColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal eGate As Gate, Optional ByVal rowskip As String() = Nothing)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column, eGate) = True Then
                        If rowskip Is Nothing Then
                            Cell1.Remove() = True
                        Else
                            If Search(Cell1.Row(), rowskip, Gate.eEQUALS) = False Then
                                Cell1.Remove() = True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        AddTable(database, table)
    End Sub

    Public Sub RemoveColumn(ByVal database As String, ByVal table As String, ByVal column As String, Optional ByVal rowskip As String() = Nothing)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If rowskip Is Nothing Then
                            Cell1.Remove() = True
                        Else
                            If Search(Cell1.Row(), rowskip, Gate.eEQUALS) = False Then
                                Cell1.Remove() = True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        AddTable(database, table)
    End Sub

    Public Sub RemoveColumn(ByVal database As String, ByVal table As String, ByVal columns As Pairs, Optional ByVal inclussive As Boolean = False)

        Dim removeCols As New List(Of String)
        Dim localCols = ColumnList(database, table)

        For Each column In localCols
            Dim IsFound As Boolean = True

            For Each rowpars In columns.Items()
                If DoesEntryExist(database, table, column, rowpars.Name(), rowpars.GetString(), Gate.eEQUALS) = False Then
                    IsFound = False
                    Exit For
                End If
            Next

            If IsFound = True Then
                removeCols.Add(column)
            End If
        Next

        For Each column In removeCols
            If inclussive = True Then
                For Each rowpars In columns.Items()
                    RemoveEntry(database, table, column, rowpars.Name())
                Next
            Else
                RemoveColumn(database, table, column)
            End If
        Next

    End Sub

    Public Sub RemoveColumns(ByVal database As String, ByVal table As String)
        RemoveTable(database, table)
        Cells.Add(New Cell(database, table))
    End Sub

    Public Sub RemoveColumns(ByVal database As String)
        Dim alltables = TableList(database)
        RemoveTables(database)

        For Each table1 In alltables
            Cells.Add(New Cell(database, table1))
        Next
    End Sub

    Public Sub RemoveColumnByRowEntry(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate, Optional ByVal rowskip As String() = Nothing)
        Dim newList As New List(Of String)

        For Each Cell1 In Cells()
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If newList.Contains(Cell1.Column()) = False Then newList.Add(Cell1.Column())
                        End If
                    End If
                End If
            End If
        Next

        For Each entry In newList
            RemoveColumn(database, table, entry, rowskip)
        Next
    End Sub


    Public Sub RemoveRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal eGate As Gate, Optional ByVal columnskip As String() = Nothing)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row, eGate) = True Then
                        If columnskip Is Nothing Then
                            Cell1.Remove() = True
                        Else
                            If Search(Cell1.Column(), columnskip, Gate.eEQUALS) = False Then
                                Cell1.Remove() = True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        AddTable(database, table)
    End Sub

    Public Sub RemoveRow(ByVal database As String, ByVal table As String, ByVal row As String, Optional ByVal columnskip As String() = Nothing)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If columnskip Is Nothing Then
                            Cell1.Remove() = True
                        Else
                            If Search(Cell1.Column(), columnskip, Gate.eEQUALS) = False Then
                                Cell1.Remove() = True
                            End If
                        End If
                    End If
                End If

            End If

        Next

        AddTable(database, table)
    End Sub

    Public Sub RemoveRow(ByVal database As String, ByVal table As String, ByVal rows As Pairs, Optional ByVal inclussive As Boolean = False)

        Dim removeRows As New List(Of String)
        Dim localRows = RowList(database, table)

        For Each row In localRows
            Dim IsFound As Boolean = True

            For Each colpars In rows.Items()
                If DoesEntryExist(database, table, colpars.Name(), colpars.Entry(), row, colpars.GetString(), Gate.eEQUALS) = False Then
                    IsFound = False
                    Exit For
                End If
            Next

            If IsFound = True Then
                removeRows.Add(row)
            End If
        Next

        For Each row In removeRows
            If inclussive = True Then
                For Each colpars In rows.Items()
                    RemoveEntry(database, table, colpars.Name(), row)
                Next
            Else
                RemoveRow(database, table, row)
            End If
        Next

    End Sub

    Public Sub RemoveRows(ByVal database As String, ByVal table As String)
        RemoveColumns(database, table)
    End Sub

    Public Sub RemoveRows(ByVal database As String)
        RemoveColumns(database)
    End Sub

    Public Sub RemoveRowByColumnEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate, Optional ByVal columnskip As String() = Nothing)
        Dim newList As New List(Of String)

        For Each Cell1 In Cells()
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If newList.Contains(Cell1.Row()) = False Then newList.Add(Cell1.Row())
                        End If
                    End If
                End If
            End If
        Next

        For Each entry In newList
            RemoveRow(database, table, entry, columnskip)
        Next
    End Sub



    Public Sub RemoveEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Public Sub RemoveEntryByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next
    End Sub

    Public Sub RemoveEntryByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Public Sub RemoveAllEntriesInDatabase(ByVal database As String)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Cell1.Table() <> NULL Then
                    If Cell1.Column() <> NULL Then
                        If Cell1.Text() <> NULL Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub RemoveAllEntriesInTable(ByVal database As String, ByVal table As String)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Cell1.Column() <> NULL Then
                        If Cell1.Text() <> NULL Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub RemoveEntriesInRow(ByVal database As String, ByVal table As String, ByVal row As String)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Cell1.Text() <> NULL Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next
    End Sub

    Public Sub RemoveEntriesInColumn(ByVal database As String, ByVal table As String, ByVal column As String)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Cell1.Text() <> NULL Then
                            Cell1.Remove() = True
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Public Sub RemoveEntryInTable(ByVal database As String, ByVal table As String, ByVal entry As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Text(), entry, eGate) = True Then
                        Cell1.Remove() = True
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub RemoveEntryInDatabase(ByVal database As String, ByVal entry As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Text(), entry, eGate) = True Then
                    Cell1.Remove() = True
                End If
            End If
        Next

    End Sub


    Public Sub RemoveEntryByColumnCompare(ByVal database As String, ByVal table As String, ByVal column As String, ByVal columnPairsCompare As Pairs, ByVal eGate As Gate)
        Dim CopyCells As New List(Of Cell)
        columnPairsCompare.RemoveDuplicatesByName()

        For Each par In columnPairsCompare.Items
            Dim newCells As New List(Of Cell)

            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                newCells.Add(Cell1)
                            End If
                        End If
                    End If
                End If
            Next

            CopyCells.AddRange(newCells)
        Next

        For Each Cell1 In CopyCells
            RemoveEntry(database, table, Cell1.Column(), Cell1.Row())
        Next

    End Sub

    Public Sub RemoveEntryByRowCompare(ByVal database As String, ByVal table As String, ByVal row As String, ByVal rowPairsCompare As Pairs, ByVal eGate As Gate)
        Dim CopyCells As New List(Of Cell)
        rowPairsCompare.RemoveDuplicatesByName()

        For Each par In rowPairsCompare.Items
            Dim newCells As New List(Of Cell)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                newCells.Add(Cell1)
                            End If
                        End If
                    End If
                End If
            Next

            CopyCells.AddRange(newCells)
        Next

        For Each Cell1 In CopyCells
            RemoveEntry(database, table, Cell1.Column(), Cell1.Row())
        Next

    End Sub


#End Region

#Region "Update"

    Public Sub UpdateEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal newEntry As String, ByVal Append As Boolean)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Append = True Then
                                Cell1.Text() = Cell1.Text() + newEntry
                            Else
                                Cell1.Text() = newEntry
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Public Sub UpdateEntryByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then

                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Append = True Then
                                Cell1.Text() = Cell1.Text() + newEntry
                            Else
                                Cell1.Text() = newEntry
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntryByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then

                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Append = True Then
                                Cell1.Text() = Cell1.Text() + newEntry
                            Else
                                Cell1.Text() = newEntry
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Public Sub UpdateEntriesByDatabase(ByVal database As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)
        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Text(), entry, eGate) = True Then
                    If Append = True Then
                        Cell1.Text() = Cell1.Text() + newEntry
                    Else
                        Cell1.Text() = newEntry
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntriesByTable(ByVal database As String, ByVal table As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Text(), entry, eGate) = True Then
                        If Append = True Then
                            Cell1.Text() = Cell1.Text() + newEntry
                        Else
                            Cell1.Text() = newEntry
                        End If
                    End If
                End If
            End If
        Next

    End Sub



    Public Sub UpdateEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Append = True Then
                                Cell1.Text() = Cell1.Text() + newEntry
                            Else
                                Cell1.Text() = newEntry
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entries As String())

        Dim index As Integer = 0
        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        Cell1.Text() = Utility.GetIndex(entries, index, Nothing)
                        index += 1
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal rowPairs As Pairs, ByVal Append As Boolean)
        For Each par In rowPairs.Items
            UpdateEntry(database, table, column, par.Name(), par.GetString(), Append)
        Next
    End Sub

    Public Sub UpdateEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal Append As Boolean)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Append = True Then
                            Cell1.Text() = Cell1.Text() + entry
                        Else
                            Cell1.Text() = entry
                        End If
                    End If
                End If
            End If
        Next

    End Sub



    Public Sub UpdateEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal newEntry As String, ByVal Append As Boolean, ByVal eGate As Gate)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Append = True Then
                                Cell1.Text() = Cell1.Text() + newEntry
                            Else
                                Cell1.Text() = newEntry
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entries As String())

        Dim index As Integer = 0
        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        Cell1.Text() = Utility.GetIndex(entries, index, Nothing)
                        index += 1
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub UpdateEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal colPairs As Pairs, ByVal Append As Boolean)
        For Each par In colPairs.Items
            UpdateEntry(database, table, par.Name(), row, par.GetString(), Append)
        Next
    End Sub

    Public Sub UpdateEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal Append As Boolean)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Append = True Then
                            Cell1.Text() = Cell1.Text() + entry
                        Else
                            Cell1.Text() = entry
                        End If
                    End If
                End If
            End If
        Next

    End Sub



    Public Sub UpdataEntryCompareByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal newEntry As String, ByVal columnPairsCompare As Pairs, ByVal Append As Boolean, ByVal eGate As Gate)
        Dim CopyCells As New List(Of Cell)
        columnPairsCompare.RemoveDuplicatesByName()

        For Each par In columnPairsCompare.Items
            Dim newCells As New List(Of Cell)

            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                newCells.Add(Cell1)
                            End If
                        End If
                    End If
                End If
            Next

            CopyCells.AddRange(newCells)
        Next

        For Each Cell1 In CopyCells
            UpdateEntry(database, table, column, Cell1.Row(), newEntry, Append)
        Next

    End Sub

    Public Sub UpdataEntryCompareByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal newEntry As String, ByVal rowPairsCompare As Pairs, ByVal Append As Boolean, ByVal eGate As Gate)
        Dim CopyCells As New List(Of Cell)
        rowPairsCompare.RemoveDuplicatesByName()

        For Each par In rowPairsCompare.Items
            Dim newCells As New List(Of Cell)

            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                newCells.Add(Cell1)
                            End If
                        End If
                    End If
                End If
            Next

            CopyCells.AddRange(newCells)
        Next

        For Each Cell1 In CopyCells
            UpdateEntry(database, table, Cell1.Column(), row, newEntry, Append)
        Next
    End Sub


#End Region

#Region "Rename"

    Public Sub Rename(ByVal database As String, ByVal newDatabase As String)
        Dim newCells As New List(Of Cell)
        newDatabase = Utility.Trim(newDatabase)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                Cell1.Remove() = True
                newCells.Add(Cell1.Copy())
            End If
        Next

        Cells.AddRange(newCells)
    End Sub

    Public Sub Rename(ByVal database As String, ByVal table As String, ByVal newTable As String)
        Dim newCells As New List(Of Cell)
        newTable = Utility.TrimLow(newTable)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    Cell1.Remove() = True
                    newCells.Add(Cell1.Copy())
                End If
            End If
        Next

        Cells.AddRange(newCells)
    End Sub

    Public Sub Rename(ByVal database As String, ByVal table As String, ByVal column As String, ByVal newColumn As String)
        Dim newCells As New List(Of Cell)
        newColumn = Utility.Trim(newColumn)

        For Each Cell1 In Cells
            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        Cell1.Remove() = True
                        newCells.Add(Cell1.Copy())
                    End If
                End If
            End If
        Next

        Cells.AddRange(newCells)
    End Sub

#End Region

#Region "Copy"

    Public Sub Copy(ByVal database As String, ByVal newDatabase As String)
        Dim allCells As New List(Of Cell)
        newDatabase = Utility.Trim(newDatabase)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For
            Dim CopyCell = Cell1.Copy()

            If Search(Cell1.Database(), database) = True Then
                CopyCell.Database() = newDatabase
                allCells.Add(CopyCell)
            End If

            If Search(Cell1.Database(), newDatabase) = True Then
                Cell1.Remove() = True
            End If
        Next

        Cells.AddRange(allCells)
    End Sub

    Public Sub Copy(ByVal database As String, ByVal table As String, ByVal newDatabase As String, ByVal newTable As String)
        Dim allCells As New List(Of Cell)
        newDatabase = Utility.Trim(newDatabase)
        newTable = Utility.Trim(newTable)

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For
            Dim CopyCell = Cell1.Copy()

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    CopyCell.Database() = newDatabase
                    CopyCell.Table() = newTable
                    allCells.Add(CopyCell)
                End If
            End If

            If Search(Cell1.Database(), newDatabase) = True Then
                If Search(Cell1.Table(), newTable) = True Then
                    Cell1.Remove() = True
                End If
            End If
        Next

        Cells.AddRange(allCells)
    End Sub

    Public Sub Copy(ByVal database As String, ByVal table As String, ByVal column As String, ByVal newDatabase As String, ByVal newTable As String, ByVal newColumn As String)
        Dim allCells As New List(Of Cell)

        newDatabase = Utility.Trim(newDatabase)
        newTable = Utility.Trim(newTable)
        newColumn = Utility.Trim(newColumn)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For
            Dim CopyCell = Cell1.Copy()

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        CopyCell.Database() = newDatabase
                        CopyCell.Table() = newTable
                        CopyCell.Column() = newColumn
                        allCells.Add(CopyCell)
                    End If
                End If
            End If

            If Search(Cell1.Database(), newDatabase) = True Then
                If Search(Cell1.Table(), newTable) = True Then
                    If Search(Cell1.Column(), newColumn) = True Then
                        Cell1.Remove() = True
                    End If
                End If
            End If
        Next

        Cells.AddRange(allCells)
    End Sub

#End Region

#Region "DoesExist"

    Public Function DoesAnyExist() As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text <> NULL Or Cell1.Meta <> NULL Then
                Return True
            End If
        Next

        Return False
    End Function


    Public Function DoesDatabaseExistByName(ByVal database As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function DoesDatabasesExist(ByVal Databases As String()) As Boolean
        Return Utility.CompareUnordered(Databases, DatabasesList(), True, Gate.eEQUALS)
    End Function

    Public Function DoesAnyExistInDatabase(ByVal database As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Cell1.Text <> NULL Or Cell1.Meta <> NULL Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function


    Public Function DoesTableExistByName(ByVal database As String, ByVal table As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesTablesExist(ByVal database As String, ByVal Tables As String()) As Boolean
        Dim AllTables = TableList(database)
        Return Utility.CompareUnordered(Tables, AllTables, True, Gate.eEQUALS)
    End Function

    Public Function DoesAnyExistInTable(ByVal database As String, ByVal table As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Cell1.Text <> NULL Or Cell1.Meta <> NULL Then
                        Return True
                    End If
                End If
            End If
        Next

        Return False
    End Function


    Public Function DoesColumnExistByName(ByVal database As String, ByVal table As String, ByVal column As String) As Boolean
        Dim Results As Boolean = False

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        Return True
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesColumnsExist(ByVal database As String, ByVal table As String, ByVal Columns As String()) As Boolean
        Dim AllColumns = ColumnList(database, table)
        Return Utility.CompareUnordered(Columns, AllColumns, True, Gate.eEQUALS)
    End Function

    Public Function DoesAnyExistInColumn(ByVal database As String, ByVal table As String, ByVal column As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Cell1.Text <> NULL Or Cell1.Meta <> NULL Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesColumnExist(ByVal database As String, ByVal table As String, ByVal Entries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim AllColumns = ColumnList(database, table)

        For Each Column1 In AllColumns
            Dim AllEntries = EntriesByColumn(database, table, Column1)
            If Utility.CompareUnordered(Entries, AllEntries, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function DoesColumnExist(ByVal database As String, ByVal table As String, ByVal rowPairs As Pairs, ByVal eGate As Gate) As Boolean
        rowPairs.RemoveDuplicatesByName()
        Dim columns = ColumnList(database, table)
        Dim results As Boolean = True

        If columns.Count() = 0 Or rowPairs.Count() = 0 Then Return False

        For Each column In columns
            results = True

            For Each par In rowPairs.Items
                If DoesEntryExist(database, table, column, par.Name(), par.GetString(), eGate) = False Then
                    results = False
                End If
            Next

            If results = True Then
                Exit For
            End If
        Next

        Return results
    End Function

    Public Function DoesRowExistByName(ByVal database As String, ByVal table As String, ByVal row As String) As Boolean

        For Each Cell1 In Cells()
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        Return True
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesRowsExist(ByVal database As String, ByVal table As String, ByVal Row As String()) As Boolean
        Dim AllRows = RowList(database, table)
        Return Utility.CompareUnordered(Row, AllRows, True, Gate.eEQUALS)
    End Function

    Public Function DoesAnyExistInRow(ByVal database As String, ByVal table As String, ByVal row As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Cell1.Text <> NULL Or Cell1.Meta <> NULL Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function


    Public Function DoesRowExist(ByVal database As String, ByVal table As String, ByVal Entries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim AllRows = RowList(database, table)

        For Each Row1 In AllRows
            Dim AllEntries = EntriesByRow(database, table, Row1)
            If Utility.CompareUnordered(Entries, AllEntries, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function DoesRowExist(ByVal database As String, ByVal table As String, ByVal colPairs As Pairs, ByVal eGate As Gate) As Boolean
        colPairs.RemoveDuplicatesByName()
        Dim rows = RowList(database, table)
        Dim results As Boolean = True

        If rows.Count() = 0 Or colPairs.Count() = 0 Then Return False

        For Each row In rows
            results = True

            For Each par In colPairs.Items
                If DoesEntryExist(database, table, par.Name(), row, par.GetString(), eGate) = False Then
                    results = False
                End If
            Next

            If results = True Then
                Exit For
            End If
        Next

        Return results
    End Function


    Public Function DoesAnyEntryExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Cell1.Text() <> NULL Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function


    Public Function DoesEntryExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesEntryExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                If Search(Cell1.Meta(), meta, eGate) = True Then
                                    Return True
                                End If
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function



    Public Function DoesColumnExistInColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal columnEntries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim newColumn = EntriesByColumn(database, table, column)
        Return Utility.CompareUnordered(columnEntries, newColumn, ignoreCase, eGate)
    End Function


    Public Function DoesEntryExistInColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesEntryExistInColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Search(Cell1.Meta(), meta, eGate) = True Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesEntryExistInColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal Entries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim newColumn = EntriesByColumn(database, table, column)
        Return Utility.Contains(newColumn, Entries, ignoreCase, eGate)
    End Function


    Public Function DoesRowExistInRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal rowEntries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim newRow = EntriesByRow(database, table, row)
        Return Utility.CompareUnordered(rowEntries, newRow, ignoreCase, eGate)
    End Function


    Public Function DoesEntryExistInRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesEntryExistInRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Search(Cell1.Meta(), meta, eGate) = True Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesEntryExistInRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal Entries As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean
        Dim newRow = EntriesByRow(database, table, row)
        Return Utility.Contains(newRow, Entries, ignoreCase, eGate)
    End Function

#End Region

#Region "Count"

    Public Function CountAllDatabases() As Integer
        Return DatabasesList().Count()
    End Function

    Public Function CountAllTables(ByVal database As String) As Integer
        Return TableList(database).Count()
    End Function

    Public Function CountAllColumns(ByVal database As String, ByVal table As String) As Integer
        Return ColumnList(database, table).Count()
    End Function

    Public Function CountEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String) As Integer
        Return EntriesByColumn(database, table, column).Count()
    End Function

    Public Function CountEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String) As Integer
        Return EntriesByRow(database, table, row).Count()
    End Function

    Public Function CountEntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As Integer
        Return EntriesByColumn(database, table, column, entry, eGate).Count()
    End Function

    Public Function CountEntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate) As Integer
        Return EntriesByRow(database, table, row, entry, eGate).Count()
    End Function

    Public Function CountOfLargestColumn(ByVal database As String, ByVal table As String) As Integer
        'Returns the count of the longest column in the Table

        Dim Total = 0
        Dim Columns = ColumnList(database, table)

        For Each Column1 In Columns

            If Utility.Trim(Column1) > "" Then

                Dim Count = CountEntriesByColumn(database, table, Column1)
                If Count > -1 Then
                    If Total < Count Then Total = Count
                End If
            End If
        Next

        Return Total
    End Function

    Public Function CountOfLargestRow(ByVal database As String, ByVal table As String) As Integer
        'Returns the count of the longest column in the Table

        Dim Total = 0
        Dim Rows = RowList(database, table)

        For Each Row1 In Rows

            If Utility.Trim(Row1) > "" Then

                Dim Count = CountEntriesByRow(database, table, Row1)
                If Count > -1 Then
                    If Total < Count Then Total = Count
                End If
            End If
        Next

        Return Total
    End Function

#End Region

#Region "Encryption"

    Public Sub EncryptDatabases(ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Decrypt = True Then
                Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
            Else
                Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
            End If

        Next

    End Sub

    Public Sub EncryptDatabase(ByVal database As String, ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Decrypt = True Then
                    Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                Else
                    Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                End If
            End If

        Next

    End Sub

    Public Sub EncryptTable(ByVal database As String, ByVal table As String, ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Decrypt = True Then
                        Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                    Else
                        Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                    End If
                End If
            End If

        Next

    End Sub

    Public Sub EncryptColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Decrypt = True Then
                            Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                        Else
                            Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                        End If
                    End If
                End If
            End If

        Next

    End Sub

    Public Sub EncryptRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Decrypt = True Then
                            Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                        Else
                            Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub EncryptEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal Decrypt As Boolean, ByVal strKey As String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Decrypt = True Then
                                Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                            Else
                                Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub EncryptEntryByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal Decrypt As Boolean, ByVal strKey As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Decrypt = True Then
                                Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                            Else
                                Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub EncryptEntryByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal Decrypt As Boolean, ByVal strKey As String, ByVal eGate As Gate)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Text(), entry, eGate) = True Then
                            If Decrypt = True Then
                                Cell1.Text() = Utility.Decrypt(Cell1.Text(), strKey)
                            Else
                                Cell1.Text() = Utility.Encrypt(Cell1.Text(), strKey)
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub


#End Region

#Region "META"

    Public Property Meta(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As String
        Get
            Dim Results = ""

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Row(), row) = True Then
                                Results = Cell1.Meta()
                                Exit For
                            End If
                        End If
                    End If
                End If
            Next

            Return Results
        End Get
        Set(value As String)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Column(), column) = True Then
                                Cell1.Meta() = value
                            End If
                        End If
                    End If
                End If
            Next

        End Set
    End Property

    Public ReadOnly Property GetMetaByEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As List(Of String)
        Get
            Dim Results As New List(Of String)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Results.Add(Cell1.Meta())
                            End If
                        End If
                    End If
                End If
            Next

            Return Results
        End Get
    End Property

    Public WriteOnly Property SetMetaByEntry(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As String
        Set(value As String)
            value = Utility.Trim(value)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Cell1.Meta() = value
                            End If
                        End If
                    End If
                End If
            Next

        End Set
    End Property


    Public Property MetaByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As String
        Get
            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Return Cell1.Meta()
                            End If
                        End If
                    End If
                End If
            Next

            Return ""
        End Get
        Set(value As String)
            value = Utility.Trim(value)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Cell1.Meta() = value
                            End If
                        End If
                    End If
                End If
            Next

        End Set
    End Property

    Public Property MetaByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate) As String
        Get

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Return Cell1.Meta()
                            End If
                        End If
                    End If
                End If
            Next

            Return ""
        End Get
        Set(value As String)
            value = Utility.Trim(value)

            For Each Cell1 In Cells
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Cell1.Meta() = value
                            End If
                        End If
                    End If
                End If
            Next

        End Set
    End Property


    Public Function DoesAnyMetaExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Cell1.Meta() <> NULL Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesMetaExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Meta(), meta, eGate) = True Then
                                Return True
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesMetaExistByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Meta(), meta, eGate) = True Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next
        Return False
    End Function

    Public Function DoesMetaExistByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal meta As String, ByVal eGate As Gate) As Boolean

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        If Search(Cell1.Meta(), meta, eGate) = True Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return False
    End Function

#End Region

#Region "Returns"

    'Returns names 
    Public Function DatabasesList() As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Database() <> NULL Then
                If Search(Results, Cell1.Database()) = False Then
                    Results.Add(Cell1.Database())
                End If
            End If
        Next

        Return Results
    End Function

    Public Function TableList(ByVal database As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Table <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Results, Cell1.Table()) = False Then
                        Results.Add(Cell1.Table())
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function ColumnList(ByVal database As String, ByVal table As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Column() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Results, Cell1.Column()) = False Then
                            Results.Add(Cell1.Column())
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function RowList(ByVal database As String, ByVal table As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Row() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Results, Cell1.Row()) = False Then
                            Results.Add(Cell1.Row())
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function


    'Returns All Entries
    Public Function Entries(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Row(), row) = True Then
                                Results.Add(Cell1.Text())
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function EntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            Results.Add(Cell1.Text())
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function EntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            Results.Add(Cell1.Text())
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function EntriesByColumn(ByVal database As String, ByVal table As String, ByVal column As String, ByVal entry As String, ByVal eGate As Gate) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), column) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Results.Add(Cell1.Text())
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

    Public Function EntriesByRow(ByVal database As String, ByVal table As String, ByVal row As String, ByVal entry As String, ByVal eGate As Gate) As List(Of String)
        Dim Results As New List(Of String)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Cell1.Text() <> NULL Then
                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), row) = True Then
                            If Search(Cell1.Text(), entry, eGate) = True Then
                                Results.Add(Cell1.Text())
                            End If
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function


    'Return full Table
    Public Function GetDatabase(ByVal database As String) As List(Of Cell)
        Dim Results As New List(Of Cell)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                Results.Add(Cell1)
            End If
        Next

        Return Results
    End Function
    Public Function GetTable(ByVal database As String, ByVal table As String) As List(Of Cell)
        Dim Results As New List(Of Cell)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    Results.Add(Cell1)
                End If
            End If
        Next

        Return Results
    End Function
    Public Function GetColumn(ByVal database As String, ByVal table As String, ByVal column As String) As List(Of Cell)
        Dim Results As New List(Of Cell)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        Results.Add(Cell1)
                    End If
                End If
            End If
        Next

        Return Results
    End Function
    Public Function GetRow(ByVal database As String, ByVal table As String, ByVal row As String) As List(Of Cell)
        Dim Results As New List(Of Cell)

        For Each Cell1 In Cells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Row(), row) = True Then
                        Results.Add(Cell1)
                    End If
                End If
            End If
        Next

        Return Results
    End Function


    'Set full Table
    Public Sub SetDatabase(ByVal old_database As String, ByVal newCells As List(Of Cell))
        RemoveDatabase(old_database)
        SetCells(newCells)
    End Sub
    Public Sub SetTable(ByVal old_database As String, ByVal old_table As String, ByVal newCells As List(Of Cell))
        RemoveTable(old_database, old_table)
        SetCells(newCells)
    End Sub
    Public Sub SetColumn(ByVal old_database As String, ByVal old_table As String, ByVal old_column As String, ByVal newCells As List(Of Cell))
        RemoveColumn(old_database, old_table, old_column)
        SetCells(newCells)
    End Sub
    Public Sub SetRow(ByVal old_database As String, ByVal old_table As String, ByVal old_row As String, ByVal newCells As List(Of Cell))
        RemoveRow(old_database, old_table, old_row)
        SetCells(newCells)
    End Sub
    Public Sub SetCells(ByVal cells As List(Of Cell))
        Me.Cells.AddRange(cells)
    End Sub



    'Return By Compare

    Public Function EntriesCompareByColumnInclussive(ByVal database As String, ByVal table As String, ByVal column As String, ByVal columnPairsCompare As Pairs, ByVal eGate As Gate) As List(Of String)
        'Includes all column/entry pairs and returns every associated entry

        Dim Results As New List(Of String)
        Dim CopyRows As New List(Of String)
        columnPairsCompare.RemoveDuplicatesByName()
        'remember to use a different column per entry

        For Each par In columnPairsCompare.Items
            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                If Search(CopyRows, Cell1.Row()) = False Then
                                    CopyRows.Add(Cell1.Row())
                                End If
                            End If
                        End If
                    End If
                End If
            Next

        Next

        For Each row In CopyRows
            Results.AddRange(Entries(database, table, column, row))
        Next

        Return Results
    End Function

    Public Function EntriesCompareByRowInclussive(ByVal database As String, ByVal table As String, ByVal row As String, ByVal rowPairsCompare As Pairs, ByVal eGate As Gate) As List(Of String)
        'Includes all row/entry pairs and returns every associated entry

        Dim Results As New List(Of String)
        Dim CopyColumns As New List(Of String)
        rowPairsCompare.RemoveDuplicatesByName()
        'remember to use a different row per entry

        For Each par In rowPairsCompare.Items
            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                If Search(CopyColumns, Cell1.Column()) = False Then
                                    CopyColumns.Add(Cell1.Column())
                                End If
                            End If
                        End If
                    End If
                End If
            Next

        Next

        For Each column In CopyColumns
            Results.AddRange(Entries(database, table, column, row))
        Next

        Return Results
    End Function


    Public Function EntriesCompareByColumnExclussive(ByVal database As String, ByVal table As String, ByVal column As String, ByVal columnPairsCompare As Pairs, ByVal eGate As Gate) As List(Of String)
        'all column/entry pairs have to match to return associated entry (if column name exits in table)

        Dim Results As New List(Of String)
        Dim CopyRows As New List(Of String)
        columnPairsCompare.RemoveDuplicatesByName()
        'remember to use a different column per entry

        For Each par In columnPairsCompare.Items

            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Column(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                CopyRows.Add(Cell1.Row())
                            End If
                        End If
                    End If
                End If
            Next

        Next

        Dim rowFilter As New List(Of String)
        For Each row In CopyRows
            If Utility.Count(CopyRows, row, True, Gate.eEQUALS) = columnPairsCompare.Count() Then
                If Search(rowFilter, row) = False Then
                    rowFilter.Add(row)
                End If
            End If
        Next

        For Each row In rowFilter
            Results.AddRange(Entries(database, table, column, row))
        Next

        Return Results
    End Function

    Public Function EntriesCompareByRowExclussive(ByVal database As String, ByVal table As String, ByVal row As String, ByVal rowPairsCompare As Pairs, ByVal eGate As Gate) As List(Of String)
        'all row/entry pairs have to match to return associated entry (if row name exits in table)

        Dim Results As New List(Of String)
        Dim CopyColumns As New List(Of String)
        rowPairsCompare.RemoveDuplicatesByName()
        'remember to use a different row per entry

        For Each par In rowPairsCompare.Items
            For Each Cell1 In Cells()
                If Cell1.Remove() = True Then Continue For

                If Search(Cell1.Database(), database) = True Then
                    If Search(Cell1.Table(), table) = True Then
                        If Search(Cell1.Row(), par.Name()) = True Then
                            If Search(Cell1.Text(), par.GetString(), par.Arguments(), eGate) = True Then
                                CopyColumns.Add(Cell1.Column())
                            End If
                        End If
                    End If
                End If
            Next

        Next

        Dim colFilter As New List(Of String)
        For Each columns In CopyColumns
            If Utility.Count(CopyColumns, columns, True, Gate.eEQUALS) = rowPairsCompare.Count() Then
                If Search(colFilter, columns) = False Then
                    colFilter.Add(columns)
                End If
            End If
        Next

        For Each column In colFilter
            Results.AddRange(Entries(database, table, column, row))
        Next

        Return Results
    End Function


    Public Function DoesCellExist(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As Boolean
        Return DoesCellExist(Cells(), database, table, column, row)
    End Function

    Private Function DoesCellExist(ByVal localCells As List(Of Cell), ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String) As Boolean
        Dim Results As Boolean = False

        For Each Cell1 In localCells
            If Cell1.Remove() = True Then Continue For

            If Search(Cell1.Database(), database) = True Then
                If Search(Cell1.Table(), table) = True Then
                    If Search(Cell1.Column(), column) = True Then
                        If Search(Cell1.Text(), row) = True Then
                            Return True
                        End If
                    End If
                End If
            End If
        Next

        Return Results
    End Function

#End Region

#End Region

    Public Class Cell

        Public Sub New()

        End Sub
        Public Sub New(ByVal database As String)
            Me.Database() = Utility.Trim(database)
        End Sub
        Public Sub New(ByVal database As String, ByVal table As String)
            Me.Database() = Utility.Trim(database)
            Me.Table() = Utility.Trim(table)
        End Sub
        Public Sub New(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String)
            Me.Database() = Utility.Trim(database)
            Me.Table() = Utility.Trim(table)
            Me.Column() = Utility.Trim(column)
            Me.Row() = Utility.Trim(row)
        End Sub
        Public Sub New(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String)
            Me.Database() = Utility.Trim(database)
            Me.Table() = Utility.Trim(table)
            Me.Column() = Utility.Trim(column)
            Me.Row() = Utility.Trim(row)
            Me.Text() = Utility.Trim(entry)
        End Sub
        Public Sub New(ByVal database As String, ByVal table As String, ByVal column As String, ByVal row As String, ByVal entry As String, ByVal meta As String)
            Me.Database() = Utility.Trim(database)
            Me.Table() = Utility.Trim(table)
            Me.Column() = Utility.Trim(column)
            Me.Row() = Utility.Trim(row)
            Me.Text() = Utility.Trim(entry)
            Me.Meta() = Utility.Trim(meta)
        End Sub

        Public Function Copy() As Cell
            Return New Cell(Database(), Table(), Column(), Row(), Text(), Meta())
        End Function

        Public Property Remove() As Boolean = False
        Public Function Address() As String
            If Column() <> NULL Then
                Return Database() & FWD_SLH & Table() & FWD_SLH & Column() & EXTEN_FILE
            ElseIf Table() <> NULL Then
                Return Database() & FWD_SLH & Table() & FWD_SLH
            ElseIf Database() <> NULL Then
                Return Database() & FWD_SLH
            Else
                Return FWD_SLH
            End If
        End Function

        Public Property Database() As String = NULL
        Public Property Table() As String = NULL
        Public Property Column() As String = NULL
        Public Property Row() As String = NULL
        Public Property Text() As String = NULL
        Public Property Meta() As String = NULL
    End Class


End Class

Public Class Audio

    Private Shared Property PlayerList() As New List(Of AudioPlayer)


    Public Shared Sub Open(ByVal name As String, ByVal source As Object, ByVal isloop As Boolean)

        If DoesPlayerExist(name) = False Then
            Dim player As New AudioPlayer
            player.Name() = name

            If TypeOf source Is String Then
                player.Source() = source
            ElseIf TypeOf source Is IO.MemoryStream Then
                player.Stream() = source
            End If

            player.IsLooped() = isloop
            player.Open()
            PlayerList.Add(player)
        End If

    End Sub


    Public Shared Sub Instance(ByVal name As String, ByVal source As Object, ByVal isloop As Boolean, ByVal postion As Double, ByVal volume As Double, ByVal balance As Double, ByVal speed As Double)

        Dim player As New AudioPlayer
        player.Name() = name

        If TypeOf source Is String Then
            player.Source() = source
        ElseIf TypeOf source Is IO.MemoryStream Then
            player.Stream() = source
        End If

        player.IsLooped() = isloop
        player.Open()

        If postion <> Nothing Then player.Position() = TimeSpan.FromSeconds(postion)
        If volume <> Nothing Then player.Volume() = volume
        If balance <> Nothing Then player.Balance() = balance
        If speed <> Nothing Then player.Speed() = speed

        AddHandler player.Finished, AddressOf IsFinished

        player.Play()
        PlayerList.Add(player)
    End Sub

    Public Shared Sub Play()
        For Each plays In PlayerList()
            plays.Play()
        Next
    End Sub

    Public Shared Sub Play(ByVal name As String)
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                plays.Play()
            End If
        Next
    End Sub

    Public Shared Sub Play(ByVal name As String, ByVal postion As Double, ByVal volume As Double, ByVal balance As Double, ByVal speed As Double)
        For Each player In PlayerList()
            If Utility.Search(player.Name(), name, Gate.eEQUALS) = True Then

                player.Stop()
                If postion <> Nothing Then player.Position() = TimeSpan.FromSeconds(postion)
                If volume <> Nothing Then player.Volume() = volume
                If balance <> Nothing Then player.Balance() = balance
                If speed <> Nothing Then player.Speed() = speed

                AddHandler player.Finished, AddressOf IsFinished
                player.Play()

            End If
        Next

    End Sub

    Public Shared Sub Play(ByVal name As String, ByVal source As Object, ByVal isloop As Boolean, ByVal postion As Double, ByVal volume As Double, ByVal balance As Double, ByVal speed As Double, ByVal instance As Boolean)
        If instance = True Then
            Audio.Instance(name, source, isloop, postion, volume, balance, speed)
        Else
            Audio.Open(name, source, isloop)
            Audio.Play(name, postion, volume, balance, speed)
        End If
    End Sub

    Public Shared Sub [Stop]()
        For Each plays In PlayerList()
            plays.IsLooped() = False
            plays.Stop()
        Next
    End Sub

    Public Shared Sub [Stop](ByVal name As String)
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                plays.IsLooped() = False
                plays.Stop()
            End If
        Next
    End Sub

    Public Shared Sub Pause()
        For Each plays In PlayerList()
            plays.Pause()
        Next
    End Sub

    Public Shared Sub Pause(ByVal name As String)
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                plays.Pause()
            End If
        Next
    End Sub

    Public Shared Function DoesPlayerExist(ByVal name As String) As Boolean
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Shared Sub Close()
        For Each plays In PlayerList()
            plays.Close()
            plays.Clean()
        Next

        PlayerList().Clear()
    End Sub

    Public Shared Sub Close(ByVal name As String)
        Dim newList As New List(Of AudioPlayer)

        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = False Then
                newList.Add(plays)
            Else
                plays.Close()
                plays.Clean()
            End If
        Next

        PlayerList() = newList
    End Sub


    Public Shared Function IsPaused(ByVal name As String) As Boolean
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                Return plays.IsPaused()
            End If
        Next
        Return False
    End Function

    Public Shared Function IsPlaying(ByVal name As String) As Boolean
        For Each plays In PlayerList()
            If Utility.Search(plays.Name(), name, Gate.eEQUALS) = True Then
                Return plays.IsPlaying()
            End If
        Next
        Return False
    End Function

    Private Shared Sub IsFinished()
        For Each plays In PlayerList()
            If plays.IsPlaying() = False Then
                If plays.IsLooped() = True Then
                    plays.Position() = New TimeSpan(0)
                    plays.Play()
                End If
            End If
        Next
    End Sub


    Public Class AudioPlayer

        Public Const TEMP_FOLDER As String = "athena_temp"
        Private Property IsTempfile() As Boolean = False

        Public WithEvents Player As New MediaPlayer()
        Public Property Name() As String = "temp"
        Public Property Source() As String = ""
        Public Property Stream() As IO.MemoryStream
        Public Property IsPlaying() As Boolean = False
        Public Property IsLooped() As Boolean = False
        Public Property IsPaused() As Boolean = False
        Public Property IsOpen() As Boolean = False

        Public Property Volume() As Double
            Get
                Return (Player.Volume() * 100)
            End Get
            Set(ByVal value As Double)
                Player.Volume() = (value / 100)
            End Set
        End Property

        Private m_length As Duration = New Duration(New TimeSpan(0))
        Public ReadOnly Property Length() As Duration
            Get
                If Player.NaturalDuration.HasTimeSpan() = True Then
                    Return Player.NaturalDuration()
                Else
                    Return m_length
                End If
            End Get
        End Property

        Public Property Position() As TimeSpan
            Get
                Return Player.Position()
            End Get
            Set(ByVal value As TimeSpan)
                Player.Position() = value
            End Set
        End Property

        Public Property Speed() As Double
            Get
                Return (Player.SpeedRatio() * 100)
            End Get
            Set(ByVal value As Double)
                Player.SpeedRatio() = (value / 100)
            End Set
        End Property

        'use only if media is already open
        Public Property PositionRatio() As Double
            Get
                Return Position().Ticks() * 100 / Length.TimeSpan.Ticks()
            End Get
            Set(ByVal value As Double)
                Position() = New TimeSpan(value / 100 * Length.TimeSpan.Ticks())
            End Set
        End Property

        Public Property Balance() As Double
            Get
                Return (Player.Balance() * 100)
            End Get
            Set(ByVal value As Double)
                Player.Balance() = (value / 100)
            End Set
        End Property

        Public Event Opened()
        Private Sub MediaOpened_Event() Handles Player.MediaOpened
            IsOpen() = True
            RaiseEvent Opened()
        End Sub

        Public Event Finished()
        Private Sub MediaEnded_Event() Handles Player.MediaEnded
            IsPlaying() = False
            RaiseEvent Finished()
        End Sub


        Public Sub Open()
            If Stream() IsNot Nothing Then
                Dim temp As String = Utility.Path({Utility.CreateTempDirectory(TEMP_FOLDER), Name()})
                IsTempfile() = True

                If Utility.FileExists(temp) = False Then
                    Utility.FileWriter(temp, Stream().ToArray(), False)
                    Source() = temp
                End If
            End If

            Player.Open(New Uri(Source(), UriKind.RelativeOrAbsolute))
        End Sub

        Public Sub Play()
            If IsPlaying() = False Then
                IsPlaying() = True
                Player.Play()
                Exit Sub
            End If

            If IsPaused() = True Then
                IsPaused() = False
                Player.Play()
                Exit Sub
            End If
        End Sub

        Public Sub [Stop]()
            If IsPlaying() = False Then
                Exit Sub
            End If

            IsPlaying() = False
            IsPaused() = False
            Player.Stop()
        End Sub

        Public Sub Pause()
            If IsPlaying() = True Then
                If IsPaused() = True Then
                    Exit Sub
                End If

                IsPaused() = True
                Player.Pause()
            End If
        End Sub

        Public Sub Close()
            IsLooped() = False
            IsPlaying() = False
            Player.Close()
        End Sub

        Public Sub Clean()
            If IsTempfile() = True Then
                Do Until Utility.FolderExists(Utility.GetParentFolder(Source())) = False
                    Try
                        Utility.Sleep(10)
                        Utility.FolderDelete(Utility.GetParentFolder(Source()))
                    Catch : End Try
                Loop
            End If
        End Sub

    End Class


End Class

Public Enum Gate
    eAND
    eNAND
    eOR
    eNOR
    eXOR
    eXNOR
    eEQUALS
    eSEARCH
    eNOT
    eSEQ
    eSTART
    eEND
End Enum


'***************
'Engine Specific
'***************

Public Class Tables

    Private Shared CLASSNAME = GetType(Tables).Name()
    Public Const DEF_TAGID As String = "@"
    Public Const DEF_TEMP As String = "[temp]"
    Public Const DEF_ROOT As String = "[root]"
    Public Const DEF_TAG_TEMP As String = "$"

    Public Const DEF_PREFIX_DETECT_AND As String = "AND_"
    Public Const DEF_PREFIX_DETECT_NAND As String = "NAND_"
    Public Const DEF_PREFIX_DETECT_OR As String = "OR_"
    Public Const DEF_PREFIX_DETECT_NOR As String = "NOR_"
    Public Const DEF_PREFIX_DETECT_SEQ As String = "SEQ_"


    Private Shared Property DB() As New Data
    Public Shared Sub Load()
        DB.LoadClusterDirectZip()
    End Sub

    Public Shared Sub Save()
        DB.SaveDatabaseToZip(Database())
        DB.Clean()
    End Sub

    Private Shared m_Connection As String
    Public Shared Property Connection() As String
        Get
            Return m_Connection
        End Get
        Set(ByVal value As String)
            m_Connection = value
            DB.Connection() = value
        End Set
    End Property


#Region "Database"

    Public Shared Property Database() As String = DEF_ROOT
    Public Shared Property RootDatabase() As String = DEF_ROOT

    Public Shared Sub CreateDatabase()
        DB.AddDatabase(Database())
    End Sub
    Public Shared Sub CopyDatabaseFromRoot()
        If DB.DoesDatabaseExistByName(RootDatabase()) = True Then
            If DB.DoesDatabaseExistByName(Database()) = False Then
                DB.Copy(RootDatabase, Database())
            End If
        End If
    End Sub

    Public Shared Sub CreateTempDatabase()
        Database() = DEF_TEMP
        CopyDatabaseFromRoot()
    End Sub

    Public Shared Sub DeleteTempDatabase()
        DB.RemoveDatabase(DEF_TEMP)
    End Sub

    Public Shared Function Join(ByVal input As String()) As String
        Dim db = Utility.Join(input, ".")
        Return "[" & Utility.ToLower(Replace(db, " ", "_")) & "]"
    End Function

#End Region


#Region "Repository Table"

    Private Shared Property CurrentRepositoryTable() As String = Table.REPOSITORY

    Public Shared Sub CreateRepositoryTable()
        DB.AddColumnByName(Database(), CurrentRepositoryTable(), Column.Detect)
        DB.AddColumnByName(Database(), CurrentRepositoryTable(), Column.Response)
        Debug.Debug(CLASSNAME, "CreateRepositoryTable", 1, CurrentRepositoryTable())
    End Sub

    Public Shared Sub AddRepository(ByVal detect As String, ByVal response As String)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)
        DB.AddRowByColumns(Database(), CurrentRepositoryTable(), ColPairs, True)

        Debug.Debug(CLASSNAME, "AddRepositoryTable", 1, response)
    End Sub

    Public Shared Sub AddRepository(ByVal detect As String, ByVal response As String, ByVal compare As Pairs)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)
        ColPairs.Add(compare)

        DB.AddRowByColumns(Database(), CurrentRepositoryTable(), ColPairs, True)

        Debug.Debug(CLASSNAME, "AddRepositoryTable", 2, response)
    End Sub

    Public Shared Property RespositCount() As Integer = 0
    Public Shared Function Repository(ByVal detect As String) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentRepositoryTable(), Column.Response, New Pairs(Column.Detect, detect), Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")
        RespositCount() = entries.Count()

        Debug.Debug(CLASSNAME, "Repository", 1, results)
        Return results
    End Function

    Public Shared Function Repository(ByVal detect As String, ByVal tags As Tags) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim Compare As New Pairs
        Compare.Add(Column.Detect, detect)

        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), CurrentRepositoryTable(), tag.Name()) = True Then
                    Compare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentRepositoryTable(), Column.Response, Compare, Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")
        RespositCount() = entries.Count()

        Debug.Debug(CLASSNAME, "Repository", 2, results)
        Return results
    End Function


    Public Shared Sub RemoveRepository(ByVal detect As String, ByVal response As String)
        Dim newPair As New Pairs
        newPair.Add(Column.Detect, detect)
        newPair.Add(Column.Response, response)

        DB.RemoveRow(Database(), CurrentRepositoryTable(), newPair)
        Debug.Debug(CLASSNAME, "RemoveRepository", 1, response)
    End Sub

    Public Shared Sub RemoveRepositoryByResponse(ByVal response As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentRepositoryTable(), Column.Response(), response, eGate)
        Debug.Debug(CLASSNAME, "RemoveRepositoryByResponse", 1, response)
    End Sub

    Public Shared Sub RemoveRepositoryByDetect(ByVal detect As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentRepositoryTable(), Column.Detect(), detect, eGate)
        Debug.Debug(CLASSNAME, "RemoveRepositoryByDetect", 1, detect)
    End Sub

#End Region


#Region "Query Table"

    Private Shared Property CurrentQueryTable() As String = Table.QUERY

    Public Shared Sub CreateQueryTable()
        DB.AddColumnByName(Database(), CurrentQueryTable(), Column.Detect)
        DB.AddColumnByName(Database(), CurrentQueryTable(), Column.Response)
        Debug.Debug(CLASSNAME, "CreateQueryTable", 1, CurrentQueryTable())
    End Sub

    Public Shared Sub AddQuery(ByVal detect As String, ByVal response As String)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)
        DB.AddRowByColumns(Database(), CurrentQueryTable(), ColPairs, True)

        Debug.Debug(CLASSNAME, "AddQueryTable", 1, response)
    End Sub

    Public Shared Sub AddQuery(ByVal detect As String, ByVal response As String, ByVal compare As Pairs)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)
        ColPairs.Add(compare)

        DB.AddRowByColumns(Database(), CurrentQueryTable(), ColPairs, True)

        Debug.Debug(CLASSNAME, "AddQueryTable", 1, response)
    End Sub

    Public Shared Property QueryCount() As Integer = 0
    Public Shared Function Query(ByVal detect As String) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentQueryTable(), Column.Response, New Pairs(Column.Detect, detect), Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")
        QueryCount() = entries.Count()

        Debug.Debug(CLASSNAME, "Query", 1, results)
        Return results
    End Function

    Public Shared Function Query(ByVal detect As String, ByVal tags As Tags) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim Compare As New Pairs
        Compare.Add(Column.Detect, detect)

        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), CurrentQueryTable(), tag.Name()) = True Then
                    Compare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentQueryTable(), Column.Response, Compare, Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")
        QueryCount() = entries.Count()

        Debug.Debug(CLASSNAME, "Query", 2, results)
        Return results
    End Function


    Public Shared Function GetRandomQuery() As String

        Dim entries = DB.EntriesByColumn(Database(), CurrentQueryTable(), Column.Detect)
        Dim results = Utility.RandArray(entries, "")
        QueryCount() = entries.Count()

        Debug.Debug(CLASSNAME, "GetRandomQuery", 1, results)
        Return results
    End Function

    Public Shared Function GetRandomQuery(ByVal tags As Tags) As String

        Dim Compare As New Pairs

        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), CurrentQueryTable(), tag.Name()) = True Then
                    Compare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentQueryTable(), Column.Detect, Compare, Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")
        QueryCount() = entries.Count()

        Debug.Debug(CLASSNAME, "GetRandomQuery", 2, results)
        Return results
    End Function

    'isAnswer
    Public Shared Function IsAnswer(ByVal detect As String, ByVal response As String) As Boolean

        Dim colPairs As New Pairs()
        colPairs.Add(Column.Detect, detect)
        colPairs.Add(Column.Response, response)


        colPairs.RemoveDuplicatesByName()
        Dim rows = DB.RowList(Database, CurrentQueryTable())
        Dim results As Boolean = True

        If rows.Count() = 0 Or colPairs.Count() = 0 Then Return False

        For Each row In rows
            results = True

            For Each par In colPairs.Items
                If DB.DoesEntryExist(Database, CurrentQueryTable(), par.Name(), row, par.GetString(), Gate.eEQUALS) = False Then
                    results = False
                End If
            Next

            If results = True Then
                Exit For
            End If
        Next

        Return results


    End Function


    Public Shared Sub RemoveQuery(ByVal detect As String, ByVal response As String)
        Dim newPair As New Pairs
        newPair.Add(Column.Detect, detect)
        newPair.Add(Column.Response, response)

        DB.RemoveRow(Database(), CurrentQueryTable(), newPair)
        Debug.Debug(CLASSNAME, "RemoveQuery", 1, response)
    End Sub

    Public Shared Sub RemoveQueryByResponse(ByVal response As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentQueryTable(), Column.Response(), response, eGate)
        Debug.Debug(CLASSNAME, "RemoveQueryByResponse", 1, response)
    End Sub

    Public Shared Sub RemoveQueryByDetect(ByVal detect As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentQueryTable(), Column.Detect(), detect, eGate)
        Debug.Debug(CLASSNAME, "RemoveQueryByDetect", 1, detect)
    End Sub

#End Region


    'more specific tables

    'detect pool / normalizer
    'replace pool / variety

    'substitutions
    'filter
    'repository
    'query
    'detecting
    'store
    'lists
    'tags
    'bank
    'topics



#Region "Normalize Pool"

    Private Shared Property CurrentNormalsTable() As String = Table.NORMALS

    Public Shared Sub CreateNormalsTable()
        DB.AddColumnByName(Database(), CurrentNormalsTable(), Column.Pool)
        DB.AddColumnByName(Database(), CurrentNormalsTable(), Column.Normal)
        Debug.Debug(CLASSNAME, "CreateNormalsTable", 1, CurrentNormalsTable())
    End Sub


    'pool/normal
    'hi there people/greetings
    Public Shared Sub AddNormal(ByVal pool As String, ByVal normal As String)
        DB.RemoveRowByColumnEntry(Database(), CurrentNormalsTable(), Column.Pool, pool, Gate.eEQUALS)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Pool, pool)
        ColPairs.Add(Column.Normal, normal)

        DB.AddRowByColumns(Database(), CurrentNormalsTable(), ColPairs)
        Debug.Debug(CLASSNAME, "AddNormal", 1, pool)
    End Sub


    Public Shared Function GetNormal(ByVal detect As String) As String

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentNormalsTable(), Column.Normal(), New Pairs(Column.Pool, detect), Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, "")

        Debug.Debug(CLASSNAME, "GetNormal", 1, results)
        Return results
    End Function

    Public Shared Function GetPool(ByVal detect As String) As String

        Dim entries = DB.EntriesCompareByColumnInclussive(Database(), CurrentNormalsTable(), Column.Pool(), New Pairs(Column.Normal, detect), Gate.eEQUALS)
        Dim results = Utility.RandArray(entries, detect)

        If Utility.IsEmpty(results) = True Then results = detect

        Debug.Debug(CLASSNAME, "GetPool", 1, results)
        Return results
    End Function


    Public Shared Sub RemoveNormal(ByVal pool As String, ByVal normal As String)
        Dim newPair As New Pairs
        newPair.Add(Column.Pool, pool)
        newPair.Add(Column.Normal, normal)

        DB.RemoveRow(Database(), CurrentNormalsTable(), newPair)
        Debug.Debug(CLASSNAME, "RemoveNormal", 1, normal)
    End Sub

    Public Shared Sub RemoveNormalByNormal(ByVal normal As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentNormalsTable(), Column.Normal(), normal, eGate)
        Debug.Debug(CLASSNAME, "RemoveNormalByNormal", 1, normal)
    End Sub

    Public Shared Sub RemoveNormalByPool(ByVal pool As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentNormalsTable(), Column.Pool(), pool, eGate)
        Debug.Debug(CLASSNAME, "RemoveNormalByPool", 1, pool)
    End Sub

#End Region


#Region "Substitutions Table"

    'This is used to substitute text in a general way
    'There is only one Substitutions Table

    Private Shared Property CurrentSubstituteTable() As String = Table.SUBSTITUTE

    Public Shared Sub CreateSubstituteTable()
        If DB.DoesTableExistByName(Database(), CurrentSubstituteTable()) = False Then
            DB.AddColumnByName(Database(), CurrentSubstituteTable(), Column.Detect)
            DB.AddColumnByName(Database(), CurrentSubstituteTable(), Column.Replace)
            Debug.Debug(CLASSNAME, "CreateSubstituteTable", 2, CurrentSubstituteTable())
        End If
    End Sub

    Public Shared Sub AddSubstitute(ByVal find As String, ByVal substitution As String)
        DB.RemoveRowByColumnEntry(Database(), CurrentSubstituteTable(), Column.Detect, find, Gate.eEQUALS)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, find)
        ColPairs.Add(Column.Replace, substitution)

        DB.AddRowByColumns(Database(), CurrentSubstituteTable(), ColPairs)
        Debug.Debug(CLASSNAME, "AddSubstituteTable", 1, find)
    End Sub

    Public Shared Sub RemoveSubstitute(ByVal find As String)
        DB.RemoveRowByColumnEntry(Database(), CurrentSubstituteTable(), Column.Detect, find, Gate.eEQUALS)
        Debug.Debug(CLASSNAME, "RemoveSubstitute", 1, find)
    End Sub

    Public Shared Function Substitute(ByVal text As String, ByVal eGate As Gate) As String
        'if you find two or more identicle detects then a random response will be chosen from the same row
        text = Utility.Buffer(text, " ")
        Dim entries = DB.EntriesByColumn(Database(), CurrentSubstituteTable(), Column.Detect)

        For Each phrase In entries
            phrase = Utility.Buffer(Utility.Trim(phrase), " ")

            If Utility.Search(text, phrase, True, eGate) = True Then
                Dim Text1 = Utility.RandArray(DB.EntriesCompareByColumnInclussive(Database(), CurrentSubstituteTable(), Column.Replace, New Pairs(Column.Detect, phrase), Gate.eEQUALS), "")
                Return Utility.Replace(text, phrase, Text1)
            End If
        Next

        Debug.Debug(CLASSNAME, "Substitute", 2, text)
        Return Trim(text)
    End Function


#End Region


#Region "Detecting Table"

    Public Shared Sub CreateDetectTable(ByVal table As String)
        DB.AddColumnByName(Database(), table, Column.Detect)
        Debug.Debug(CLASSNAME, "CreateDetectTable", 1, table)
    End Sub
    Public Shared Sub CreateResponseTable(ByVal table As String)
        DB.AddColumnByName(Database(), table, Column.Response)
        DB.AddColumnByName(Database(), table, Column.Repeat)
        Debug.Debug(CLASSNAME, "CreateResponseTable", 1, table)
    End Sub

    Public Shared Sub AddResponse(ByVal table As String, ByVal response As String)
        Dim ColEntry As New Pairs
        ColEntry.Add(Column.Response, response)
        ColEntry.Add(Column.Repeat, Utility.BoolToStr(True))

        DB.RemoveRowByColumnEntry(Database, table, Column.Response, response, Gate.eEQUALS, {Column.Detect})
        DB.AddRowByColumns(Database(), table, ColEntry)

        Debug.Debug(CLASSNAME, "AddResponse", 1, table)
    End Sub
    Public Shared Sub AddResponse(ByVal table As String, ByVal response As String, ByVal compare As Pairs)
        Dim ColEntry As New Pairs
        ColEntry.Add(Column.Response, response)
        ColEntry.Add(Column.Repeat, Utility.BoolToStr(True))
        ColEntry.Add(compare)

        DB.RemoveRowByColumnEntry(Database, table, Column.Response, response, Gate.eEQUALS, {Column.Detect})
        DB.AddRowByColumns(Database(), table, ColEntry)

        Debug.Debug(CLASSNAME, "AddResponse", 2, table)
    End Sub
    Public Shared Sub AddResponse(ByVal table As String, ByVal responses As String())

        For Each response In responses
            Dim ColEntry As New Pairs
            ColEntry.Add(Column.Response, response)
            ColEntry.Add(Column.Repeat, Utility.BoolToStr(True))

            DB.RemoveRowByColumnEntry(Database, table, Column.Response, response, Gate.eEQUALS, {Column.Detect})
            DB.AddRowByColumns(Database(), table, ColEntry)
        Next

        Debug.Debug(CLASSNAME, "AddResponse", 3, table)
    End Sub

    Public Shared Sub AddDetect(ByVal table As String, ByVal detect As String())
        DB.AddColumnByEntries(Database, table, Column.Detect, detect)
        Debug.Debug(CLASSNAME, "AddDetect", 1, table)
    End Sub
    Public Shared Sub AddDetect(ByVal table As String, ByVal detect As String)
        DB.AddEntryByColumn(Database, table, Column.Detect, detect)
        Debug.Debug(CLASSNAME, "AddDetect", 1, table)
    End Sub

    Public Shared Sub RemoveDetect(ByVal table As String, ByVal detect As String)
        DB.RemoveEntryByColumn(Database, table, Column.Detect, detect, Gate.eEQUALS)
    End Sub
    Public Shared Sub RemoveResponse(ByVal table As String, ByVal response As String)
        DB.RemoveRowByColumnEntry(Database, table, Column.Response, response, Gate.eEQUALS)
    End Sub

    Public Shared Function GeneralDetect(ByVal table As String, ByVal text As String, ByVal tags As Tags, ByVal eGate As Gate) As String
        If Detect(table, text, eGate) = True Then
            Return Respond(table, tags)
        End If

        Return ""
    End Function
    Public Shared Function GeneralDetect(ByVal table As String, ByVal text As String(), ByVal tags As Tags, ByVal eGate As Gate) As String
        If Detect(table, text, eGate) = True Then
            Return Respond(table, tags)
        End If

        Return ""
    End Function
    Public Shared Function GeneralDetect(ByVal text As String, ByVal randomSelect As Boolean, ByVal tags As Tags) As String

        Dim tables = DB.TableList(Database())

        If randomSelect = True Then
            Dim newList As New List(Of String)
            For Each table In tables
                If Utility.Search(table, DEF_PREFIX_DETECT_AND, True, Gate.eSTART) = True Then
                    newList.Add(GeneralDetect(table, text, tags, Gate.eAND))
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_NAND, True, Gate.eSTART) = True Then
                    newList.Add(GeneralDetect(table, text, tags, Gate.eNAND))
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_NOR, True, Gate.eSTART) = True Then
                    newList.Add(GeneralDetect(table, text, tags, Gate.eNOR))
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_OR, True, Gate.eSTART) = True Then
                    newList.Add(GeneralDetect(table, text, tags, Gate.eEQUALS))
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_SEQ, True, Gate.eSTART) = True Then
                    newList.Add(GeneralDetect(table, text, tags, Gate.eSEQ))
                End If
            Next

            Return Utility.RandArray(newList, "")
        Else
            For Each table In tables
                If Utility.Search(table, DEF_PREFIX_DETECT_AND, True, Gate.eSTART) = True Then
                    Return GeneralDetect(table, text, tags, Gate.eAND)
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_NAND, True, Gate.eSTART) = True Then
                    Return GeneralDetect(table, text, tags, Gate.eNAND)
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_NOR, True, Gate.eSTART) = True Then
                    Return GeneralDetect(table, text, tags, Gate.eNOR)
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_OR, True, Gate.eSTART) = True Then
                    Return GeneralDetect(table, text, tags, Gate.eEQUALS)
                End If
                If Utility.Search(table, DEF_PREFIX_DETECT_SEQ, True, Gate.eSTART) = True Then
                    Return GeneralDetect(table, text, tags, Gate.eSEQ)
                End If
            Next

            Return ""
        End If

    End Function


    Public Shared Function Detect(ByVal table As String, ByVal column As String, ByVal text As String, ByVal eGate As Gate) As Boolean
        'AND, NAND, OR/SEARCH, NOR, SEQ, XOR, XNOR
        Dim textarray = DB.EntriesByColumn(Database(), table, column)

        textarray = Utility.Buffer(Utility.Trim(textarray), " ")
        text = Utility.Buffer(Utility.Trim(text), " ")

        Dim Results = Utility.Search(text, textarray, eGate)
        Debug.Debug(CLASSNAME, "Detect", 1, Results)
        Return Results
    End Function
    Public Shared Function Detect(ByVal table As String, ByVal text As String, ByVal eGate As Gate) As Boolean
        'AND, NAND, OR/SEARCH, NOR, SEQ, XOR, XNOR
        Dim textarray = DB.EntriesByColumn(Database(), table, Column.Detect)

        textarray = Utility.Buffer(Utility.Trim(textarray), " ")
        text = Utility.Buffer(Utility.Trim(text), " ")

        Dim Results = Utility.Search(text, textarray, eGate)
        Debug.Debug(CLASSNAME, "Detect", 2, Results)
        Return Results
    End Function

    Public Shared Function Detect(ByVal table As String, ByVal column As String, ByVal text As String(), ByVal eGate As Gate) As Boolean
        'EQUALS, START, END, SEARCH/SEQ, NOT
        Dim textarray = DB.EntriesByColumn(Database(), table, column)
        Dim Results As Boolean = False

        For Each line In textarray
            If Utility.Search(text, Utility.WordArray(line), True, eGate) = True Then
                Results = True
                Exit For
            End If
        Next

        Debug.Debug(CLASSNAME, "Detect", 3, Results)
        Return Results
    End Function
    Public Shared Function Detect(ByVal table As String, ByVal text As String(), ByVal eGate As Gate) As Boolean
        'EQUALS, START, END, SEARCH/SEQ, NOT
        Dim textarray = DB.EntriesByColumn(Database(), table, Column.Detect())
        Dim Results As Boolean = False

        For Each line In textarray
            If Utility.Search(text, Utility.WordArray(line), True, eGate) = True Then
                Results = True
                Exit For
            End If
        Next

        Debug.Debug(CLASSNAME, "Detect", 4, Results)
        Return Results
    End Function


    'Response

    Public Shared Function Respond(ByVal table As String, ByVal column As String, ByVal tags As Tags) As String
        'Convert Tags to Pairs (for use in tables)
        'remove pairs/tags (by column) that are not avaliable in the table
        Dim Compare As New Pairs
        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), table, tag.Name()) = True Then
                    Compare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Dim Results = Utility.RandArray(DB.EntriesCompareByColumnExclussive(Database(), table, column, Compare, Gate.eEQUALS), "")

        Debug.Debug(CLASSNAME, "Respond", 1, Results)
        Return Results
    End Function

    Public Shared Function Respond(ByVal table As String, ByVal column As String, ByVal compare As Pairs) As String
        'Convert Tags to Pairs (for use in tables)
        'remove pairs/tags (by column) that are not avaliable in the table

        Dim Results = Utility.RandArray(DB.EntriesCompareByColumnExclussive(Database(), table, column, compare, Gate.eEQUALS), "")

        Debug.Debug(CLASSNAME, "Respond", 2, Results)
        Return Results
    End Function

    Public Shared Function Respond(ByVal table As String) As String

        Dim Compare As New Pairs
        Compare.Add(Column.Repeat, Utility.BoolToStr(True))

        Dim textarray = DB.EntriesCompareByColumnInclussive(Database(), table, Column.Response, Compare, Gate.eEQUALS)
        Dim Results = Utility.RandArray(textarray, "")

        If Utility.IsEmpty(Results) = False And DB.DoesColumnExistByName(Database(), table, Column.Repeat()) = True Then
            'Updates Repeat Column to False to show that the Results should not be repeated until cycle ends 
            DB.UpdataEntryCompareByColumn(Database(), table, Column.Repeat, Utility.BoolToStr(False), New Pairs(Column.Response, Results), False, Gate.eEQUALS)

            'Updating Repeat Column to True if all are set to False
            If Utility.RemoveDuplicates(textarray).Count() = 1 Then
                DB.UpdateEntriesByColumn(Database(), table, Column.Repeat, Utility.BoolToStr(True), False)
            End If
        End If

        Debug.Debug(CLASSNAME, "Respond", 3, Results)
        Return Results
    End Function


    Public Shared Function Respond(ByVal table As String, ByVal tags As Tags) As String

        'Convert Tags to Pairs (for use in tables)
        'remove pairs/tags (by column) that are not avaliable in the table
        Dim Compare As New Pairs
        Compare.Add(Column.Repeat, Utility.BoolToStr(True))

        Dim tagCompare As New Pairs
        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), table, tag.Name()) = True Then
                    tagCompare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Compare.Add(tagCompare)

        Dim textarray = DB.EntriesCompareByColumnExclussive(Database(), table, Column.Response, Compare, Gate.eEQUALS)
        Dim Results = Utility.RandArray(textarray, "")


        If Utility.IsEmpty(Results) = False And DB.DoesColumnExistByName(Database(), table, Column.Repeat()) = True Then
            'Updates Repeat Column to False to show that the Results should not be repeated until cycle ends 
            DB.UpdataEntryCompareByColumn(Database(), table, Column.Repeat, Utility.BoolToStr(False), New Pairs(Column.Response, Results), False, Gate.eEQUALS)

            'Updating Repeat Column to True if all are set to False
            If Utility.RemoveDuplicates(textarray).Count() = 1 Then
                DB.UpdataEntryCompareByColumn(Database(), table, Column.Repeat, Utility.BoolToStr(True), tagCompare, False, Gate.eEQUALS)
            End If
        End If

        Debug.Debug(CLASSNAME, "Respond", 4, Results)
        Return Results
    End Function

    Public Shared Function Respond(ByVal table As String, ByVal compare As Pairs) As String

        Dim newCompare As New Pairs
        newCompare.Add(Column.Repeat, Utility.BoolToStr(True))
        newCompare.Add(compare)

        Dim textarray = DB.EntriesCompareByColumnExclussive(Database(), table, Column.Response, newCompare, Gate.eEQUALS)
        Dim Results = Utility.RandArray(textarray, "")
        Dim UpdateResults As String = Results

        If Utility.IsEmpty(Results) = False And DB.DoesColumnExistByName(Database(), table, Column.Repeat()) = True Then
            'Updates Repeat Column to False to show that the Results should not be repeated until cycle ends 
            DB.UpdataEntryCompareByColumn(Database(), table, Column.Repeat, Utility.BoolToStr(False), New Pairs(Column.Response, Results), False, Gate.eEQUALS)

            'Updating Repeat Column to True if all are set to False
            If Utility.RemoveDuplicates(textarray).Count() = 1 Then
                DB.UpdataEntryCompareByColumn(Database(), table, Column.Repeat(), Utility.BoolToStr(True), compare, False, Gate.eEQUALS)
            End If
        End If

        Debug.Debug(CLASSNAME, "Respond", 5, Results)
        Return Results
    End Function


#End Region


#Region "Lists Table"

    Private Shared Property CurrentListsTable() As String = Table.LISTS

    Public Shared Sub CreateListsTable()
        DB.AddTable(Database(), Table.LISTS())
        Debug.Debug(CLASSNAME, "CreateListsTable", 1, Table.LISTS())
    End Sub

    Public Shared Sub AddToLists(ByVal column As String, ByVal entry As String)
        DB.AddEntryByColumn(Database(), CurrentListsTable(), column, entry)
        Debug.Debug(CLASSNAME, "AddToLists", 1, CurrentListsTable())
    End Sub
    Public Shared Sub AddToLists(ByVal column As String, ByVal entries As String())
        DB.AddColumnByEntries(Database(), CurrentListsTable(), column, entries)
        Debug.Debug(CLASSNAME, "AddToLists", 2, CurrentListsTable())
    End Sub

    Public Shared Function GetFromLists(ByVal column As String, ByVal defaults As String) As String
        'This will grab a random Sentence out of a column of Sentences
        Dim Results = Utility.RandArray(DB.EntriesByColumn(Database(), CurrentListsTable(), column), defaults)
        Debug.Debug(CLASSNAME, "GetFromLists", 1, Results)
        Return Results
    End Function
    Public Shared Function GetFromLists(ByVal column As String, ByVal search As String, ByVal eGate As Gate, ByVal defaults As String) As String
        'This function looks up a Sentence inside the Sentence column based upon a subject.
        'This function returns an Array of Sentences with the same common subject.
        Dim values As New List(Of String)
        Dim newList = DB.EntriesByColumn(Database(), CurrentListsTable(), column)

        For Each Sen1 In newList
            If Utility.Search(Sen1, search, eGate) = True Then
                values.Add(Sen1)
            End If
        Next

        Dim results = Utility.RandArray(values, defaults)
        Debug.Debug(CLASSNAME, "GetFromLists", 2, results)
        Return results
    End Function

    Public Shared Sub RemoveFromList(ByVal column As String, ByVal entry As String)
        DB.RemoveEntryByColumn(Database(), CurrentListsTable(), column, entry, Gate.eEQUALS)
        Debug.Debug(CLASSNAME, "RemoveFromList", 1, column)
    End Sub

    Public Shared Function GetList(ByVal column As String) As List(Of String)
        'This will grab a random Sentence out of a column of Sentences
        Debug.Debug(CLASSNAME, "GetList", 1, CurrentListsTable())
        Return DB.EntriesByColumn(Database(), CurrentListsTable(), column)
    End Function

    Public Shared WriteOnly Property SetList(ByVal column As String) As String()
        Set(value As String())
            DB.RemoveColumn(Database(), CurrentListsTable(), column)
            DB.AddColumnByEntries(Database(), CurrentListsTable(), column, value)
            Debug.Debug(CLASSNAME, "SetList", 1, CurrentListsTable())
        End Set
    End Property


#End Region


#Region "Bank Table"
    Private Shared Property CurrentBankTable() As String = Table.BANK

    Public Shared Sub CreateBankTable()
        DB.AddColumnByName(Database(), CurrentBankTable(), Column.Name)
        DB.AddColumnByName(Database(), CurrentBankTable(), Column.Response)
        Debug.Debug(CLASSNAME, "CreateBankTable", 1, CurrentBankTable())
    End Sub

    Public Shared Sub BankDeposite(ByVal name As String, ByVal deposites As String)
        Dim text = Utility.Encrypt(deposites, name)
        name = Utility.Encrypt(name, name)
        AddResponse(CurrentBankTable, text, New Pairs(Column.Name(), name))
        Debug.Debug(CLASSNAME, "BankDeposite", 1, "password")
    End Sub

    Public Shared Function BankWithdrawal(ByVal name As String) As String
        Dim password = Utility.Encrypt(name, name)
        Dim text = Respond(CurrentBankTable, New Pairs(Column.Name(), password))
        Debug.Debug(CLASSNAME, "BankWithdrawal", 1, "password")
        Return Utility.Decrypt(text, name)
    End Function

    Public Shared Sub RemoveBank(ByVal name As String)
        DB.RemoveEntryByColumnCompare(Database(), CurrentBankTable(), Column.Response(), New Pairs(Column.Name(), name), Gate.eEQUALS)
        Debug.Debug(CLASSNAME, "RemoveBank", 1, "password")
    End Sub

#End Region


#Region "Topics Table"
    Private Shared Property CurrentTopicsTable() As String = Table.TOPICS

    Public Shared Sub CreateTopicsTable()
        DB.AddColumnByName(Database(), CurrentTopicsTable(), Column.Detect)
        DB.AddColumnByName(Database(), CurrentTopicsTable(), Column.Response)
        Debug.Debug(CLASSNAME, "CreateTopicsTable", 1, CurrentTopicsTable())
    End Sub

    Public Shared Sub AddTopics(ByVal detect As String, ByVal response As String)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)

        DB.AddRowByColumns(Database(), CurrentTopicsTable(), ColPairs)

        Debug.Debug(CLASSNAME, "AddTopics", 1, response)
    End Sub

    Public Shared Sub AddTopics(ByVal detect As String, ByVal response As String, ByVal compare As Pairs)

        Dim ColPairs As New Pairs
        ColPairs.Add(Column.Detect, detect)
        ColPairs.Add(Column.Response, response)
        ColPairs.Add(compare)

        DB.AddRowByColumns(Database(), CurrentTopicsTable(), ColPairs)

        Debug.Debug(CLASSNAME, "AddTopics", 1, response)
    End Sub

    Public Shared Function Topics(ByVal detect As String, ByVal eGate As Gate) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim results = Utility.RandArray(DB.EntriesCompareByColumnInclussive(Database(), CurrentTopicsTable(), Column.Response, New Pairs(Column.Detect, detect, {"equals", "-r"}), eGate), "")

        Debug.Debug(CLASSNAME, "Topics", 1, results)
        Return results
    End Function

    Public Shared Function Topics(ByVal detect As String, ByVal tags As Tags, ByVal eGate As Gate) As String
        'This function looks up a response inside the Response column based upon a detect in the Detect column.

        Dim Compare As New Pairs
        Compare.Add(Column.Detect, detect, {"equals", "-r"})

        For Each tag In tags.TagList()
            If Utility.Search(tag.Name(), DEF_TAGID, True, Gate.eSTART) = True Then
                tag.Name() = Utility.LMidTrim(tag.Name(), DEF_TAGID.Length())
                If DB.DoesAnyExistInColumn(Database(), CurrentTopicsTable(), tag.Name()) = True Then
                    Compare.Add(tag.Name(), tag.Values(0), {tag.Search()})
                End If
            End If
        Next

        Dim results = Utility.RandArray(DB.EntriesCompareByColumnInclussive(Database(), CurrentTopicsTable(), Column.Response, Compare, eGate), "")

        Debug.Debug(CLASSNAME, "Topics", 2, results)
        Return results
    End Function

    Public Shared Sub RemoveTopics(ByVal detect As String, ByVal response As String)
        Dim newPair As New Pairs
        newPair.Add(Column.Detect, detect)
        newPair.Add(Column.Response, response)

        DB.RemoveRow(Database(), CurrentTopicsTable(), newPair)
        Debug.Debug(CLASSNAME, "RemoveTopics", 1, response)
    End Sub

    Public Shared Sub RemoveTopicsByResponse(ByVal response As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentTopicsTable(), Column.Response(), response, eGate)
        Debug.Debug(CLASSNAME, "RemoveTopicsByResponse", 1, response)
    End Sub

    Public Shared Sub RemoveTopicsByDetect(ByVal detect As String, ByVal eGate As Gate)
        DB.RemoveRowByColumnEntry(Database(), CurrentTopicsTable(), Column.Detect(), detect, eGate)
        Debug.Debug(CLASSNAME, "RemoveTopicsByDetect", 1, detect)
    End Sub

#End Region


#Region "Store Table"

    '****************************************
    'This Gets and Sets Mood, Desire, State, and Topic of Discussion of the A.I.
    '****************************************

    Private Shared Property CurrentStoreTable() As String = Table.STORE()
    Public Shared Sub CreateStoreTable()
        DB.AddTable(Database(), CurrentStoreTable())
    End Sub

    Public Shared Sub DeleteStore(ByVal name As String)
        DB.RemoveRow(Database(), CurrentStoreTable(), name)
    End Sub

    Public Shared Property Store(ByVal name As String) As String
        Get
            Return Utility.GetIndex(DB.Entries(Database(), CurrentStoreTable(), Column.Store, name), 0, "")
        End Get
        Set(ByVal value As String)
            If DB.DoesAnyEntryExist(Database(), CurrentStoreTable(), Column.Store, name) = False Then
                DB.AddEntry(Database(), CurrentStoreTable(), Column.Store, name, value)
            Else
                DB.UpdateEntry(Database(), CurrentStoreTable(), Column.Store, name, value, False)
            End If
        End Set
    End Property
    Public Shared ReadOnly Property Store(ByVal name As String, ByVal defaults As String) As String
        Get
            Dim results = Store(name)

            If Utility.IsEmpty(results) = True Then
                Return defaults
            Else
                Return results
            End If
        End Get
    End Property

    Public Shared Property Stores(ByVal name As String) As String()
        Get
            Return DB.EntriesByRow(Database(), CurrentStoreTable(), name).ToArray()
        End Get
        Set(ByVal value As String())
            DB.RemoveRow(Database(), CurrentStoreTable(), name)
            DB.AddRowByEntries(Database(), CurrentStoreTable(), name, value)
        End Set
    End Property
    Public Shared ReadOnly Property Stores(ByVal name As String, ByVal defaults As String()) As String()
        Get
            Dim results = Stores(name)

            If Utility.IsEmpty(results) = True Then
                Return defaults
            Else
                Return results
            End If
        End Get
    End Property

    Public Shared Function DoesStoreExist(ByVal name As String) As Boolean
        Return DB.DoesAnyEntryExist(Database(), CurrentStoreTable(), Column.Store, name)
    End Function


    'StoreCount
    Public Shared Function Count(ByVal name As String, ByVal max As Double, ByVal reset As Boolean) As Double

        Dim cnt = Utility.ToDbl(Store(name), max)
        If cnt < 1 Then cnt = 1

        If max > cnt Then
            cnt += 1
            Store(name) = Utility.ToStr(cnt)
            Return cnt
        Else
            If reset = True Then
                Store(name) = Utility.ToStr(1)
                Return 1
            Else
                DeleteStore(name)
                Return max
            End If
        End If

    End Function
    Public Shared Function Count(ByVal name As String, ByVal min As Double, ByVal max As Double, ByVal reset As Boolean) As Double

        Dim cnt = Utility.ToDbl(Store(name), min)
        If cnt < min Then cnt = min

        If max > cnt Then
            cnt += 1
            Store(name) = Utility.ToStr(cnt)
            Return cnt
        Else
            If reset = True Then
                Store(name) = Utility.ToStr(min)
                Return min
            Else
                DeleteStore(name)
                Return max
            End If
        End If

    End Function
    Public Shared Function Count(ByVal name As String, ByVal min As Double, ByVal max As Double, ByVal skip As Double, ByVal reset As Boolean) As Double

        Dim cnt = Utility.ToDbl(Store(name), min)
        If cnt < min Then cnt = min

        If max > cnt Then
            cnt += skip
            Store(name) = Utility.ToStr(cnt)
            Return cnt
        Else
            If reset = True Then
                Store(name) = Utility.ToStr(min)
                Return min
            Else
                DeleteStore(name)
                Return max
            End If
        End If

    End Function

    Public Shared Sub CountReset(ByVal name As String, ByVal min As Double)
        Store(name) = Utility.ToStr(min)
    End Sub
    Public Shared Sub CountReset(ByVal name As String)
        Store(name) = Utility.ToStr(1)
    End Sub

#End Region


#Region "Tags"
    'Storing and Retrieving tags that will be fed back into the iTags
    Private Shared Property CurrentTagsTable() As String = Table.TAGS
    Public Shared Sub CreateTagsTable(ByVal name As String)
        CurrentTagsTable() = name
        DB.AddTable(Database(), CurrentTagsTable())
        Debug.Debug(CLASSNAME, "CreateTagsTable", 1, CurrentTagsTable())
    End Sub

    Public Shared Sub CreateTagsTable()
        DB.AddTable(Database(), CurrentTagsTable())
        Debug.Debug(CLASSNAME, "CreateTagsTable", 2, CurrentTagsTable())
    End Sub


    Public Shared Sub SetTags(ByVal name As String, ByVal value As String())
        'Save a single tag to table
        DB.RemoveColumns(Database(), CurrentTagsTable())
        DB.AddColumnByEntries(Database(), CurrentTagsTable(), name, value, True)
        Debug.Debug(CLASSNAME, "SetTags", 1, name)
    End Sub

    Public Shared Sub SetTags(ByVal tags As Tags)
        'Save multiple tags to table
        DB.RemoveColumns(Database(), CurrentTagsTable())

        For Each tag In tags.TagList()
            DB.AddColumnByEntries(Database(), CurrentTagsTable(), tag.Name(), tag.Values(), True)
            Debug.Debug(CLASSNAME, "SetTags", 2, tag.Name())
        Next
    End Sub

    Public Shared Function GetTags() As Tags
        'Load all tags in tag table
        Dim outTags As New Tags
        Dim Columns = DB.ColumnList(Database(), CurrentTagsTable())

        For Each column In Columns
            Dim values = DB.EntriesByColumn(Database(), CurrentTagsTable(), column).ToArray()
            outTags.Add(column, values)
        Next

        Debug.Debug(CLASSNAME, "GetTags", 1, CurrentTagsTable())
        Return outTags
    End Function


    Public Shared Sub RemoveTempTags()
        DB.RemoveColumn(Database(), CurrentTagsTable(), DEF_TAG_TEMP, Gate.eSEARCH)
        Debug.Debug(CLASSNAME, "RemoveTempTags", 1, CurrentTagsTable())
    End Sub


#End Region


End Class

Public Class Memory

    'The Memory Class give a direct interface to Athena's UI 
    'Setting the Registry Memory will be done in the UI


#Region "CONSTANTS"

    Private XmlSrc As New Matrix.XmlSource
    Private Const ADDRESS_L1 As String = "relay"
    Private Const ADDRESS_L2 As String = "save"
    Private Const DELIM As String = "{S}"

    Public Class REG
        Public Const userinput As String = "userinput"
        Public Const package As String = "package"
        Public Const profile As String = "profile"
        Public Const usercluster As String = "usercluster"
        Public Const scripterror As String = "errors"
        Public Const avatarscript As String = "avatarscript"
        Public Const avatarname As String = "avatarname"
        Public Const avatarcode As String = "avatarcode"
        Public Const response As String = "response"
        Public Const status As String = "status"
        Public Const commands As String = "commands"
        Public Const brainready As String = "brainready"
        Public Const database As String = "database"
        Public Const word As String = "word"
        Public Const book As String = "book"
    End Class

#End Region


#Region "Main"

    Public Property Location() As String = ""

    Public Sub Load()
        XmlSrc.Loadfile(Location())

        Ready() = Utility.ToBool(Memory(REG.brainready))
        Script() = Memory(REG.avatarscript)
        UserInput() = Memory(REG.userinput)
        Package() = Memory(REG.package)
        Profile() = Memory(REG.profile)
        Cluster() = Memory(REG.usercluster)
        Word() = Memory(REG.word)
        Avatar() = Memory(REG.avatarname)
        Book() = Memory(REG.book)

        Dim out = ""

        out = Memory(REG.scripterror)
        If Utility.Search(out, DELIM) = True Then
            Errors() = Utility.ToList(out, DELIM)
            Errors() = Utility.CleanArray(Errors())
        End If

        out = Memory(REG.status)
        If Utility.Search(out, DELIM) = True Then
            Status() = Utility.ToList(out, DELIM)
            Status() = Utility.CleanArray(Status())
        End If

        out = Memory(REG.response)
        If Utility.Search(out, DELIM) = True Then
            Response() = Utility.ToList(out, DELIM)
            Response() = Utility.CleanArray(Response())
        End If

        out = Memory(REG.avatarcode)
        If Utility.Search(out, DELIM) = True Then
            Code() = Utility.ToList(out, DELIM)
            Code() = Utility.CleanArray(Code())
        End If

        out = Memory(REG.commands)
        If Utility.Search(out, DELIM) = True Then
            Commands() = Utility.ToList(out, DELIM)
            Commands() = Utility.CleanArray(Commands())
        End If

        XmlSrc.Clear()
    End Sub

    Public Sub Save()
        XmlSrc.Clear()

        If Utility.IsEmpty(UserInput()) = False Then Memory(REG.userinput) = UserInput()
        If Utility.IsEmpty(Package()) = False Then Memory(REG.package) = Package()
        If Utility.IsEmpty(Profile()) = False Then Memory(REG.profile) = Profile()
        If Utility.IsEmpty(Cluster()) = False Then Memory(REG.usercluster) = Cluster()
        If Utility.IsEmpty(Word()) = False Then Memory(REG.word) = Word()
        If Utility.IsEmpty(Avatar()) = False Then Memory(REG.avatarname) = Avatar()
        If Utility.IsEmpty(Book()) = False Then Memory(REG.book) = Book()

        If Utility.IsEmpty(Script()) = False Then Memory(REG.avatarscript) = Script()
        Memory(REG.brainready) = Utility.BoolToStr(Ready())

        If Utility.IsEmpty(Errors()) = False Then
            Errors() = Utility.CleanArray(Errors())
            Memory(REG.scripterror) = Utility.Join(Errors(), DELIM) & DELIM
        End If
        If Utility.IsEmpty(Status()) = False Then
            Status() = Utility.CleanArray(Status())
            Memory(REG.status) = Utility.Join(Status(), DELIM) & DELIM
        End If
        If Utility.IsEmpty(Response()) = False Then
            Response() = Utility.CleanArray(Response())
            Memory(REG.response) = Utility.Join(Response(), DELIM) & DELIM
        End If
        If Utility.IsEmpty(Code()) = False Then
            Code() = Utility.CleanArray(Code())
            Memory(REG.avatarcode) = Utility.Join(Code(), DELIM) & DELIM
        End If
        If Utility.IsEmpty(Commands()) = False Then
            Commands() = Utility.CleanArray(Commands())
            Memory(REG.commands) = Utility.Join(Commands(), DELIM) & DELIM
        End If

        XmlSrc.Save(Location())

    End Sub

    Public Sub Clear()
        XmlSrc.Clear()

        Ready() = False
        UserInput() = ""
        Profile() = ""
        Cluster() = ""
        Word() = ""
        Package() = ""
        Avatar() = ""
        Script() = ""
        Book() = ""
        Errors().Clear()
        Status().Clear()
        Code().Clear()
        Commands().Clear()
        Response().Clear()

    End Sub

    Public Sub Reset()
        Clear()
        Save()
    End Sub

    Private Property Memory(ByVal name As String) As String
        Get
            Return Utility.Trim(XmlSrc.InnerText({ADDRESS_L1, ADDRESS_L2, name}))
        End Get
        Set(value As String)
            value = Utility.Trim(value)
            Dim address As String() = {ADDRESS_L1, ADDRESS_L2, name}
            If XmlSrc.DoesNodeExist(address) = True Then
                XmlSrc.InnerText(address, False) = value
            Else
                XmlSrc.Create(address, value)
            End If
        End Set
    End Property

#End Region


#Region "Memory Relays"
    Public Property Ready() As Boolean = False 'set to True if ready to read Memory file

    Public Property UserInput() As String = "" 'retrieves the UserInput from the UI
    Public Property Package() As String = "" 'retrieves the Package from the UI
    Public Property Cluster() As String = "" 'retrieves the current cluster location from the UI
    Public Property Word() As String = "" 'retrieves the current word database location from the UI
    Public Property Avatar() As String = "" 'retrieves the current avatar's name
    Public Property Profile() As String = "" 'retrieves the current profile name

    Public Property Script() As String = "" 'sends the mame of a avatar script that will be ran
    Public Property Code() As New List(Of String) 'sends lines of code to the avatar, creates an avatar script
    Public Property Errors() As New List(Of String) 'sends a list of errors to UI errer Prompt
    Public Property Response() As New List(Of String) 'sends list of responses to UI response Prompt
    Public Property Status() As New List(Of String) 'sends some text to UI status Prompt
    Public Property Commands() As New List(Of String) 'sends a list of brain commands (i.e. playsound) to UI 
    Public Property Book() As String = "" 'sends a textfile link to be read by the UI

#End Region

End Class

Public Class Folders

    Public Shared Sub MakeDirectories()
        Utility.FolderCreate(Athena())
        Utility.FolderCreate(Data())
        Utility.FolderCreate(Databases())
        Utility.FolderCreate(Avatars())
        Utility.FolderCreate(Brains())
        Utility.FolderCreate(Plugins())
        Utility.FolderCreate(Books())
        Utility.FolderCreate(Shortcuts())
        Utility.FolderCreate(Sounds())
        Utility.FolderCreate(Dialogs())
        Utility.FolderCreate(Word())
        Utility.FolderCreate(Temp())
    End Sub

    Public Shared Sub CleanTemp()
        If Utility.FolderExists(Temp()) = True Then
            Utility.FileDeleteAll(Temp())
        End If
    End Sub

    'Return to Utility.Directory() when ready for merge.
    Private Shared m_Directory As String = "C:\Users\Aaron\source\repos\WpfApplication2\WpfApplication2\bin\Debug\" 'Utility.Directory()
    Public Shared Property Directory() As String
        Get
            Return m_Directory
        End Get
        Set(value As String)
            m_Directory = value
        End Set
    End Property

    Private Shared m_Package As String = "standard"
    Public Shared Property Package() As String
        Get
            Return m_Package
        End Get
        Set(value As String)
            m_Package = value
        End Set
    End Property


    Public Shared ReadOnly Property Athena() As String
        Get
            Return Utility.Path({Directory(), "Athena\"})
        End Get
    End Property

    Public Shared ReadOnly Property Data() As String
        Get
            Return Utility.Path({Directory(), "Data\"})
        End Get
    End Property

    Public Shared ReadOnly Property Temp() As String
        Get
            Return Utility.Path({Data(), "Temp\"})
        End Get
    End Property


    Public Shared ReadOnly Property Databases() As String
        Get
            Return Utility.Path({Athena(), Package(), "Databases\"})
        End Get
    End Property

    Public Shared ReadOnly Property Avatars() As String
        Get
            Return Utility.Path({Athena(), Package(), "Avatars\"})
        End Get
    End Property

    Public Shared ReadOnly Property Brains() As String
        Get
            Return Utility.Path({Athena(), Package(), "Brains\"})
        End Get
    End Property

    Public Shared ReadOnly Property Plugins() As String
        Get
            Return Utility.Path({Athena(), Package(), "Plugins\"})
        End Get
    End Property

    Public Shared ReadOnly Property Books() As String
        Get
            Return Utility.Path({Athena(), Package(), "Books\"})
        End Get
    End Property

    Public Shared ReadOnly Property Shortcuts() As String
        Get
            Return Utility.Path({Athena(), Package(), "Shortcuts\"})
        End Get
    End Property

    Public Shared ReadOnly Property Sounds() As String
        Get
            Return Utility.Path({Athena(), Package(), "Sounds\"})
        End Get
    End Property

    Public Shared ReadOnly Property Word() As String
        Get
            Return Utility.Path({Athena(), Package(), "Word\"})
        End Get
    End Property

    Public Shared ReadOnly Property Dialogs() As String
        Get
            Return Utility.Path({Athena(), Package(), "Dialogs\"})
        End Get
    End Property

End Class

Public Class Files
    Public Shared ReadOnly Property UIPROGRAM() As String
        Get
            Return "The Athena Project"
        End Get
    End Property

    Public Shared ReadOnly Property Input() As String
        Get
            Return Folders.Data() & "input" & Ext.IO
        End Get
    End Property
    Public Shared ReadOnly Property Output() As String
        Get
            Return Folders.Data() & "output" & Ext.IO
        End Get
    End Property
    Public Shared ReadOnly Property Save() As String
        Get
            Return Folders.Data() & "save" & Ext.XML
        End Get
    End Property
    Public Shared ReadOnly Property Debug() As String
        Get
            Return Folders.Data() & "debug" & Ext.DBG
        End Get
    End Property
    Public Shared ReadOnly Property BrainDbg() As String
        Get
            Return Folders.Plugins() & "Brain" & Ext.DBG
        End Get
    End Property
    Public Shared ReadOnly Property GeneralDialog() As String
        Get
            Return Folders.Dialogs() & "General" & Ext.XML
        End Get
    End Property

End Class

Public Class Ext
    Public Const DATA As String = ".db"
    Public Const ZIP As String = ".zip"
    Public Const XML As String = ".xml"
    Public Const AVATAR As String = ".ava"
    Public Const BRAIN As String = ".brn"
    Public Const IO As String = ".io"
    Public Const VB As String = ".vb"
    Public Const TEXT As String = ".txt"
    Public Const WAV As String = ".wav"
    Public Const PLUG As String = ".plg"
    Public Const DBG As String = ".dbg"
    Public Const WORD As String = ".wd"
    Public Const PNG As String = ".png"
    Public Const PRO As String = ".pro"
    Public Const CUB As String = ".cub"
    Public Const EXE As String = ".exe"
End Class

Public Class Column
    Public Shared ReadOnly Property Detect As String = "Detect"
    Public Shared ReadOnly Property Response As String = "Response"
    Public Shared ReadOnly Property Repeat As String = "Repeat"
    Public Shared ReadOnly Property Replace As String = "Replace"
    Public Shared ReadOnly Property Name As String = "Name"
    Public Shared ReadOnly Property Store As String = "Store"
    Public Shared ReadOnly Property Normal As String = "Normal"
    Public Shared ReadOnly Property Pool As String = "Pool"

    Public Shared ReadOnly Property Filter As String = "Filter" 'list
    Public Shared ReadOnly Property Block As String = "Block" 'list
    Public Shared ReadOnly Property Empty As String = "Empty" 'list
    Public Shared ReadOnly Property Leads As String = "Leads" 'list
    Public Shared ReadOnly Property Random As String = "Random" 'list
    Public Shared ReadOnly Property Dreams As String = "Dreams" 'list
    Public Shared ReadOnly Property Foods As String = "Foods" 'list
    Public Shared ReadOnly Property Insults As String = "Insults" 'list
    Public Shared ReadOnly Property Compliments As String = "Compliments" 'list
End Class

Public Class Table
    'Standard
    Public Shared ReadOnly Property SUBSTITUTE As String = "SUBSTITUTE"
    Public Shared ReadOnly Property TAGS As String = "TAGS"
    Public Shared ReadOnly Property STORE As String = "STORE"
    Public Shared ReadOnly Property LISTS As String = "LISTS"
    Public Shared ReadOnly Property BANK As String = "BANKS"
    Public Shared ReadOnly Property TOPICS As String = "TOPICS"
    Public Shared ReadOnly Property REPOSITORY As String = "REPOSITORY"
    Public Shared ReadOnly Property QUERY As String = "QUERY"
    Public Shared ReadOnly Property NORMALS As String = "NORMALS"

    Public Shared ReadOnly Property INSULTS As String = "INSULT_DETECT"
    Public Shared ReadOnly Property COMPLIMENTS As String = "COMPLIMENT_DETECT"

    Public Shared ReadOnly Property AND_DETECT As String = "AND_DETECT"
    Public Shared ReadOnly Property NAND_DETECT As String = "NAND_DETECT"
    Public Shared ReadOnly Property OR_DETECT As String = "OR_DETECT"
    Public Shared ReadOnly Property NOR_DETECT As String = "NOR_DETECT"
    Public Shared ReadOnly Property SEQ_DETECT As String = "SEQ_DETECT"

End Class

Public Class Story
    Public Property Chapters As New Chapter
    Public ReadOnly Property Name() As String
        Get
            Return Utility.GetFileName(Source(), True)
        End Get
    End Property

    Public Property Source() As String = ""
    Public Property Header() As String
        Get
            Return Chapters.Header()
        End Get
        Set(value As String)
            Chapters.Header() = value
        End Set
    End Property

    Public ReadOnly Property Metas() As Tags
        Get
            Dim newTags As New Tags
            newTags.Load(Header())
            Return newTags
        End Get
    End Property


    '<Story>
    '   <Chapter>
    '       <"page name">
    '           <note>
    '               "response"
    '           </note>
    '           <tags>
    '	            #save,database,table,column,row,value
    '               #replace,[sentence],replace, replacewith
    '           </tags>
    '           <navigate>
    '               <entry1>
    '                   <detect #type=search>"detect"</detect>
    '                   <goto>newpage</goto>
    '                   <tags>#cmd:arg,arg</tags>
    '                   <rate>0</rate>
    '               </entry1>
    '               <entry2>
    '                   <detect>default</detect>
    '                   <goto>newpage</goto>
    '                   <tags>#cmd:arg,arg</tags>
    '                   <rate>0</rate>
    '               </entry2>
    '           </navigate>
    '       </page name>
    '   </chapter>
    '</story>


    'What's going on here
    'within the story and chapter there are pages
    'the pages consist of a page name, page note, and page navigation
    'the navigation is there to direct you to a new page
    'it directs to the new goto page with any three other variables
    'these variables are Detect, Rate, and State
    'Detect can contain full sentence or a few topic words
    'Rate allows a singe goto page to be set above the rest
    'for instance only the higher rated goto page can be chosen
    'State variable can contain any unique content that might set an goto page apart from the rest
    '
    'I have been combining variable combonations to choose the best goto page
    'I've combined Detect and Rate by default where if you find by multiple pages by detect
    'it will then choose one goto page by highest rate

    'the goto page can also be found by choose one randomly

    'if no other goto page can be found then the default page can be found 
    'the default page is set by changing the Detect variable to "default"

    'there is no clean way of finding a page's parent, however, we can do this using a buffer
    'therefore we can then scroll through page history 


#Region "CONSTANTS"

    Private Const ADDRESS_L1 As String = "story"
    Private Const ADDRESS_L2 As String = "chapter"

    Public Const START_NAME As String = "start"
    Private Const NAVIGATE_NAME As String = "navigate"
    Private Const NOTE_NAME As String = "note" 'response
    Private Const JUMP_NAME As String = "jump"
    Private Const TAG_NAME As String = "tags"
    Private Const INHERIT_NAME As String = "inherit"
    Private Const DETECT_NAME As String = "detect"
    Private Const SEARCH_NAME As String = "search"
    Private Const GOTO_NAME As String = "goto" 'goto page
    Private Const RATE_NAME As String = "rate"
    Private Const ENTRY_NAME As String = "entry"
    Public ReadOnly Property DEFAULT_NAME As String() = {"[default]"}
    Public Const DEF_EXIT As String = "[EXIT]"
    Public Const DEF_TAGPARSE_START As String = "tags("
    Public Const DEF_TAGPARSE_END As String = ")"
    Private Const CELL_NAME As String = "C"
    Public Const DEF_DELIM As String = "|"
    Private Const PREVIOUS_PAGE_LIMIT = 1000
    Public Property CurrentPage() As New Chapter.Page
    Public Property Previous() As New Detection
    Public Property CurrentInherited() As New List(Of Detection)

#End Region


#Region "Private"

    Private Function IsEmpty(ByVal newDetect As Detection) As Boolean
        If Utility.IsEmpty(newDetect.Detect()) = True Then
            If Utility.IsEmpty(newDetect.GoTo()) = True Then
                Return True
            End If
        End If
        Return False
    End Function
    Public Function RandomPageName() As String
        Dim Results = ""
        'creating a unique cell name
        For i = 0 To 100
            Results = CELL_NAME & Utility.RandKey(10)
            If Chapters.DoesPageExist(Results) = False Then Return Results
        Next

        Return Results
    End Function
    Public Function RandDetection(ByVal DetectList As List(Of Detection)) As Detection
        If DetectList.Count() > 0 Then
            If DetectList.Count = 1 Then
                Return DetectList(0)
            Else
                Return DetectList(Utility.Rand(0, DetectList.Count() - 1))
            End If
        Else
            Return Nothing
        End If
    End Function
    Public Function SortNavigateByRate(ByVal NavList As List(Of Detection), Optional ByRef HasRates As Boolean = False) As List(Of Detection)
        'Highest Rate at 0 index 
        Dim newNavList As New List(Of Detection)
        Dim sortLt As New List(Of Integer)

        For Each Nav In NavList
            sortLt.Add(Nav.Rate())
        Next

        Dim low = Utility.GetLowInt(sortLt)
        Dim high = Utility.GetHighInt(sortLt)

        If low = high Then
            HasRates = False
        Else
            HasRates = True
        End If

        For i = high To low Step -1
            For Each Nav In NavList
                If Nav.Rate() = i Then
                    newNavList.Add(Nav)
                End If
            Next
        Next

        Return newNavList
    End Function
    Public Sub JoinInherited()
        CurrentInherited() = New List(Of Detection)
        CurrentInherited.AddRange(CurrentPage.Navigate)
        CurrentInherited.AddRange(GetInherited())
    End Sub

#End Region


#Region "Main"

    Public Function Load(ByVal start As String) As Boolean
        Chapters.Create(START_NAME, Nothing, Nothing, Nothing, Nothing, Nothing)

        If Chapters.DoesPageExist(start) = True Then
            CurrentPage = Chapters.Find(start)
            PreviousPage()
            Return True
        Else
            Return False
        End If
    End Function
    Public Sub Load()
        PreviousReset()
        Chapters.Create(START_NAME, Nothing, Nothing, Nothing, Nothing, Nothing)
        CurrentPage = Chapters.Find(START_NAME)
        PreviousPage()
    End Sub
    Public Sub Loadfile()
        Chapters.Clear()
        Chapters.LoadFromFile(Source())
        Chapters.LoadFromXml({ADDRESS_L1, ADDRESS_L2})
        Load()
    End Sub
    Public Sub Loadfile(ByVal StartPage As String)
        Chapters.Clear()
        Chapters.LoadFromFile(Source())
        Chapters.LoadFromXml({ADDRESS_L1, ADDRESS_L2})
        Load(StartPage)
    End Sub

    Public Sub Reset()
        Load()
    End Sub

    Public Sub Save()
        Chapters.SaveToFile(Source())
    End Sub
    Public Sub Save(ByVal location As String)
        Chapters.SaveToFile(location)
    End Sub

    Public Sub RefreshPage()
        CurrentPage = Chapters.Find(CurrentPage.Name())
        PreviousRefresh()
    End Sub


    Public Sub Clear()
        Source() = ""
        Header() = ""
        Chapters.Clear()
        PreviousReset()
    End Sub

    Public Sub RemovePage(Optional ByVal removeHistory As Boolean = False)
        If Utility.Search(CurrentPage.Name, START_NAME, Gate.eEQUALS) = True Then
            Chapters.Update(START_NAME, {""}, "", "", {""}, New List(Of Detection))
            RefreshPage()
        Else
            Chapters.Remove(CurrentPage.Name())
            If removeHistory = True Then
                Chapters.RemoveNavigate(ParentPage.Name, Previous, True)
                PreviousRemove()
                CurrentPage = ParentPage()
            End If
        End If
    End Sub

    Public Sub Commands(ByVal cmd As String)

        If Utility.Search(cmd, {"s/*", "s\*"}, Gate.eEQUALS) = True Then
            cmd = Utility.Remove(cmd, {"s/", "s\"})

            'this resets the pages to the start(origin) page
            If Utility.Search(cmd, "start", Gate.eSTART) = True Then
                Reset()
                PreviousPages.Clear()
            End If

            'this randomly sets the page to one the current page goto names without regards to the detect variable 
            If Utility.Search(cmd, "random", Gate.eSTART) = True Then
                CurrentPage() = Chapters.Find(Utility.RandArray(GetGoTos(), ""))
                PreviousPage()
            End If

            'this randomly sets the page to a sibling of the current page
            If Utility.Search(cmd, "sibling", Gate.eSTART) = True Then
                SiblingByRandom()
            End If

            'this forces the page to change to a set goto page by name
            If Utility.Search(cmd, "goto", Gate.eSTART) = True Then
                Dim arg1 = Utility.Trim(Utility.GetRightOfArg(cmd, "goto", Gate.eEQUALS))
                CurrentPage = Chapters.Find(arg1)
                PreviousPage()
            End If

            'this forces the page to change to the default goto page
            If Utility.Search(cmd, "default", Gate.eSTART) = True Then
                CurrentPage() = Chapters.Find(GetDefaultGoto().GoTo())
            End If


            'this sets the pages to the previous page in the PrevousPage List
            If Utility.Search(cmd, "back", Gate.eSTART) = True Then
                BackPage()
            End If
            'this sets the pages to the next page in the PrevousPage List
            If Utility.Search(cmd, "forward", Gate.eSTART) = True Then
                ForwardPage()
            End If


            'there is a next command but no previous.. would be good if cycle by rate in the opposite direction
            'this is cycling by rate but it could also cycle by array index of goto list or by random selection (just remove the previously selected goto)
            'we are using rate-cycling because we want the best choice (highest rate first)

            'resets the current cycle, in order to choose a current page by rank
            If Utility.Search(cmd, "resetcycle", Gate.eSTART) = True Then
                ResetCycle()
            End If
            If Utility.Search(cmd, "nextcycle", Gate.eSTART) = True Then
                CycleByRate()
            End If
            If Utility.Search(cmd, "cycleindex", Gate.eSTART) = True Then
                Dim arg1 = Utility.Trim(Utility.GetRightOfArg(cmd, "cycleindex", Gate.eEQUALS))
                If IsNumeric(arg1) = True Then
                    CycleByRate(Utility.ToInt(arg1))
                End If
            End If

        End If

    End Sub


#End Region


#Region "Explore"


    Public Function FindGoto(ByVal detect As String, ByVal rate As Integer?, ByVal index As Integer?, ByVal tags As Tags) As Detection
        'index of 0 will return the greatest rating
        JoinInherited()

        If CurrentInherited().Count() = 0 Then
            Return Nothing
        End If

        Dim NavList As New List(Of Detection)
        Dim defaults As New Detection

        For Each Nav In CurrentInherited()

            If Utility.Search(Nav.Detect(), DEFAULT_NAME, True, Gate.eEQUALS) = True Then
                defaults = Nav
                Continue For
            End If

            If IsNothing(detect) = True Then GoTo line1
            If Utility.IsEmpty(detect) = True Then GoTo line1

            If Utility.Search(detect, Nav.Detect(), Gate.eEQUALS) = True Then

line1:          If IsNothing(rate) = True Then GoTo line2
                If Nav.Rate = rate Then

line2:              If IsNothing(tags) = True Then GoTo line3
                    If tags.Count() = 0 Then GoTo line3
                    If Nav.Tag.DoTagsMatch(tags) = True Then
                        'all the input tags (if they exist in choices) must match

line3:                  NavList.Add(Nav)
                    End If
                End If
            End If
        Next


        If NavList.Count() > 0 Then
            If IsNothing(index) = False Then
                Dim sorted = SortNavigateByRate(NavList)
                Return GetIndex(sorted, index, sorted.First)
            Else

                Dim hasRates As Boolean = False
                Dim sorted = SortNavigateByRate(NavList, hasRates)

                If hasRates = False Then
                    Return RandDetection(NavList)
                Else
                    Return GetIndex(sorted, 0, sorted.First)
                End If

            End If
        Else
            Return defaults
        End If

    End Function

    Public Shared Function GetIndex(ByVal array_1 As List(Of Detection), ByVal index As Integer, ByVal Defaults As Detection) As Detection
        If index < array_1.Count And index > -1 Then
            Return array_1(index)
        Else
            Return Defaults
        End If
    End Function


    Public Property PreviewDetection() As New Detection
    Public Function Explorer(ByVal input As String, ByVal rate As Integer?, ByVal index As Integer?, ByVal tags As Tags) As Boolean
        'Process any commands before finding the Goto
        Commands(input)
        PreviewDetection() = FindGoto(input, rate, index, tags)

        If IsNothing(PreviewDetection()) = False Then
            If Chapters.DoesPageExist(PreviewDetection().GoTo()) = True Then
                Return True
            End If
        End If

        Return False
    End Function

    Public Function Commit() As Boolean
        If IsNothing(PreviewDetection()) = False Then
            If Chapters.DoesPageExist(PreviewDetection().GoTo()) = True Then
                CurrentPage = Chapters.Find(PreviewDetection().GoTo())
                Previous() = PreviewDetection()
                PreviousPage()
                Return True
            End If
        End If

        Return False
    End Function

    Public Function Jump() As Boolean
        If Utility.IsEmpty(GetJump()) = False Then
            If Chapters.DoesPageExist(GetJump()) = True Then
                CurrentPage = Chapters.Find(GetJump())
                PreviousRemove()
                PreviousPage()
                Return True
            End If
        End If

        Return False
    End Function


#End Region


#Region "Get Properties"

    Public Function GetPage() As String
        Return CurrentPage.Name()
    End Function

    Public Function GetNote() As String()
        'This returns the current pages note.
        Return CurrentPage.Note()
    End Function

    Public Function GetJump() As String
        'This returns the current pages Jump page.
        Return CurrentPage.Jump()
    End Function

    Public Function GetRandomNote(Optional ByVal chances As Integer() = Nothing) As String
        'This returns the current pages note.
        'It splits the Note() into an array and returns a random entry
        Return CurrentPage.RandomNote(chances)
    End Function

    Public Function GetTags() As String
        'This returns the current pages tag.
        Return CurrentPage.Tags()
    End Function

    Public Function GetTag() As Tags
        'This returns the current pages tag.
        Return CurrentPage.Tag()
    End Function

    Public Function GetDetection(ByVal index As Integer) As Detection
        Dim count = CurrentPage.Navigate.Count
        If index > 0 And index < count Then
            Return CurrentPage.Navigate(index)
        Else
            Return New Detection
        End If
    End Function

    Public Function GetNavigation() As List(Of Detection)
        Return CurrentPage.Navigate
    End Function

    Public Function GetDetections(ByVal detect As String, ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags) As List(Of Detection)
        Return CurrentPage.GetDetection(detect, gotos, rate, tags)
    End Function

    Public Function GetDetects() As List(Of String())
        'this returns a list of the current pages detections
        Dim newList As New List(Of String())

        For Each Dir1 In CurrentPage.Navigate
            newList.Add(Dir1.Detect)
        Next

        Return newList
    End Function

    Public Function GetGoTos() As List(Of String)
        'this returns a list of the current pages gotos
        Dim DirArray As New List(Of String)

        For Each Dir1 In CurrentPage.Navigate
            DirArray.Add(Dir1.GoTo())
        Next

        Return DirArray
    End Function

    Public Function GetDefaultGoto() As Detection
        'This returns a goto associated with with a specified detection of the current page
        For Each Nav In CurrentPage.Navigate
            If Utility.Search(Nav.Detect(), DEFAULT_NAME, True, Gate.eEQUALS) = True Then
                Return Nav
            End If
        Next
        Return New Detection
    End Function

    Public Function GetGoto(ByVal detect As String, ByVal eGate As Gate) As String
        'This returns a goto associated with with a specified detection of the current page
        For Each Nav In CurrentPage.Navigate
            If Utility.Search(Nav.Detect(), detect, eGate) = True Then
                Return Nav.GoTo()
            End If
        Next
        Return ""
    End Function

    Public Function GetGoToPages() As List(Of Chapter.Page)
        'This returns all the next level pages, these pages are located in the goto variable 
        Dim newPages As New List(Of Chapter.Page)

        For Each Nav In CurrentPage.Navigate
            Dim newPageName = Nav.GoTo()

            If Chapters.DoesPageExist(newPageName) = True Then
                newPages.Add(Chapters.Find(newPageName))
            End If

        Next

        Return newPages
    End Function

    Public Function GetInherited() As List(Of Detection)
        Dim newList As New List(Of Detection)

        For Each page1 In CurrentPage.Inheritance
            If Utility.IsEmpty(page1) = False Then
                Dim navList = Chapters.Find(page1).Navigate()

                If navList.Count() > 0 Then
                    newList.AddRange(navList)
                End If
            End If
        Next
        Return newList
    End Function

    Public Function GetInheritence() As String()
        Return CurrentPage.Inheritance()
    End Function

    Public Function SearchNotes(ByVal text As String, ByVal eGate As Gate) As List(Of Chapter.Page)
        Dim newPages As New List(Of Chapter.Page)

        For Each pg In Chapters.Pages
            If Utility.Search(pg.Note(), text, True, eGate) = True Then
                newPages.Add(pg)
            End If
        Next

        Return newPages
    End Function

#End Region


#Region "Page History"

    Public Property PreviousPages() As New List(Of String)
    Public Property PreviousDetections() As New List(Of Detection)
    Public Property CurrentDepth() As Integer = 0

    Public Sub PreviousClip()
        'if the number of pages are more than the pos then clip the end of the pages to match the pos
        If Utility.InRange(CurrentDepth, 0, PreviousPages.Count, True) = True Then
            PreviousPages() = PreviousPages.GetRange(0, CurrentDepth())
            PreviousDetections() = PreviousDetections.GetRange(0, CurrentDepth())
        End If
    End Sub

    Public Sub PreviousReset()
        PreviousDetections().Clear()
        PreviousPages.Clear()
        CurrentDepth() = 0
    End Sub

    Public Sub PreviousRemove()
        If Utility.InRange(CurrentDepth, 1, PreviousPages.Count, True) = True Then
            PreviousPages.RemoveAt(CurrentDepth() - 1)
            PreviousDetections.RemoveAt(CurrentDepth() - 1)
            CurrentDepth -= 1
        End If
    End Sub

    Public Sub PreviousRefresh()
        If CurrentDepth > 0 Then
            If Utility.InRange(CurrentDepth(), 0, PreviousPages.Count, True) = True Then
                PreviousPages.RemoveAt(CurrentDepth() - 1)
                PreviousPages.Insert(CurrentDepth() - 1, CurrentPage.Name())
            End If
        End If
    End Sub

    Public Sub PreviousPage()
        PreviousClip()

        PreviousPages.Add(CurrentPage.Name())
        PreviousDetections.Add(Previous)

        'adjusting for buffer limit
        If PreviousPages.Count() > PREVIOUS_PAGE_LIMIT Then
            PreviousPages.RemoveAt(0)
            PreviousDetections.RemoveAt(0)
        Else
            CurrentDepth() += 1
        End If
    End Sub

    Public Function ForwardPage() As Boolean
        'if last page is reached a true is returned

        If CurrentDepth() <= 1 Then
            CurrentDepth() = 1
        End If

        CurrentDepth() += 1

        If CurrentDepth() > PreviousPages.Count() Then
            CurrentDepth() = PreviousPages.Count()
            Return True
        Else
            CurrentPage() = Chapters.Find(PreviousPages(CurrentDepth - 1))
            Previous() = PreviousDetections(CurrentDepth - 1)
            Return False
        End If
    End Function

    Public Function BackPage() As Boolean
        'if last page is reached a true is returned

        CurrentDepth() -= 1

        If CurrentDepth() > PreviousPages.Count() Then
            CurrentDepth() = PreviousPages.Count()
        End If

        If CurrentDepth() <= 0 Then
            CurrentDepth() = 0
            Return True
        Else
            CurrentPage() = Chapters.Find(PreviousPages(CurrentDepth() - 1))
            Previous() = PreviousDetections(CurrentDepth() - 1)
            Return False
        End If
    End Function

    Public Function ParentPage() As Chapter.Page

        If PreviousPages.Count() > 1 And CurrentDepth() > 1 Then
            If Utility.InRange(CurrentDepth(), 1, PreviousPages.Count(), True) = True Then
                Return Chapters.Find(PreviousPages(CurrentDepth() - 2))
            End If
        End If

        Return New Chapter.Page
    End Function

#End Region


#Region "Goto Page Selection"

    Public Sub SiblingByRandom()
        Dim NaveList As New List(Of String)

        For Each Navigate1 In ParentPage().Navigate
            NaveList.Add(Navigate1.GoTo())
        Next

        CurrentPage = Chapters.Find(Utility.RandArray(NaveList, ""))
        PreviousPage()
    End Sub

    Private sibLevel As Integer = 0
    Public Sub ResetCycle()
        sibLevel = 0
    End Sub

    Public Sub CycleByRate()
        'this cycles through the current pages siblings by increasing the rate level 
        Dim allGotos = SortNavigateByRate(ParentPage().Navigate)

        If sibLevel < 0 Then sibLevel = 0
        If sibLevel > allGotos.Count - 1 Then
            sibLevel = 0
        End If

        Dim HighestGoto As New Detection
        If allGotos.Count() > 0 Then
            HighestGoto = allGotos(sibLevel)
        Else
            HighestGoto = GetDefaultGoto()
        End If

        If Chapters.DoesPageExist(HighestGoto.GoTo()) = True Then
            CurrentPage = Chapters.Find(HighestGoto.GoTo())
            PreviousPage()
            sibLevel += 1
        End If

    End Sub

    Public Sub CycleByRate(ByVal rateLevel As Integer)
        'this cycles through the current pages siblings by increasing the rate level 
        Dim allGotos = SortNavigateByRate(ParentPage().Navigate)

        If rateLevel < 0 Then rateLevel = 0
        If rateLevel > allGotos.Count - 1 Then
            rateLevel = allGotos.Count - 1
        End If

        Dim HighestGoto As New Detection
        If allGotos.Count() > 0 Then
            HighestGoto = allGotos(rateLevel)
        Else
            HighestGoto = GetDefaultGoto()
        End If

        If Chapters.DoesPageExist(HighestGoto.GoTo()) = True Then
            CurrentPage = Chapters.Find(HighestGoto.GoTo())
            PreviousPage()
        End If
    End Sub

    'This is used to create a markov-like chain of conversations within grid of rooms
    '
    'Within the textfile there should be pairs of first person vs second person's reaction.
    'i.e.
    'person1: hi
    'person2: hello there
    '
    'As it is parsed, it will place the first and second person conversation in a detect and response pattern
    'Now lets says that a second conversation occures and we try to add the new conversation to the same save file via this parser
    'If the conversation is identicle then the rating for it will be increased by 1, but if it is different then it will branch 
    'off on a different rout
    'It is important to note that conversations must be simular on the same node level within the chain
    'if not then a new conversation thread is created. 


    Public Sub Parse(ByVal textToParse As String, ByVal Deliminator As String)
        'This is used to create a markov-like chain of conversations within grid of rooms

        textToParse = textToParse + DEF_EXIT

        Dim Document = Utility.CleanArray(Utility.ToList(textToParse, Deliminator))

        Dim detect_page_name = START_NAME

        For i = 0 To Document.Count() - 1 Step 2
            'Parsing lines into Chapters

            If i + 1 = Document.Count() Then
                Exit For
            End If

            Dim textDetect = Utility.GetIndex(Document, i, "")
            Dim textNote = Utility.GetIndex(Document, (i + 1), "")

            '#state[or]:happy|smart
            Dim text_inTags = Utility.GetMidArg(textDetect, DEF_TAGPARSE_START, DEF_TAGPARSE_END, Gate.eEQUALS)
            Dim inTags As New Tags(text_inTags)
            textDetect = Utility.Remove(textDetect, DEF_TAGPARSE_START + text_inTags + DEF_TAGPARSE_END)

            Dim text_outTags = Utility.GetMidArg(textNote, DEF_TAGPARSE_START, DEF_TAGPARSE_END, Gate.eEQUALS)
            Dim outTags As New Tags(text_outTags)
            textNote = Utility.Remove(textNote, DEF_TAGPARSE_START + text_outTags + DEF_TAGPARSE_END)


            Dim newDetect = Utility.Trim(Utility.ToArray(textDetect, DEF_DELIM))
            Dim newNote = Utility.Trim(Utility.ToArray(textNote, DEF_DELIM))


            '***this section could be used to minimize or normalize the detect variable
            'newDetect = newDetect
            '***

            Dim foundPage As New Chapter.Page

            If Chapters.DoesDetectNoteExist(detect_page_name, newDetect, newNote, foundPage) = False Then
                Dim notePage As New Chapter.Page
                Dim note_page_name = RandomPageName()


                Chapters.AddNavigate(detect_page_name, New Detection(newDetect, note_page_name, 0, inTags))

                notePage.Name() = note_page_name
                notePage.Note() = newNote
                notePage.Tag() = outTags
                Chapters.Add(notePage)

                detect_page_name = note_page_name
            Else

                Chapters.UpdateRate(detect_page_name, newDetect, Nothing, -1, Nothing, 1)
                detect_page_name = foundPage.Name
            End If

        Next

    End Sub


#End Region


    Public Class Chapter
        Public Property XmlSource As New XmlSource
        Public Property Pages() As New List(Of Page)
        Public Property Header() As String = ""

        Public Sub SaveToFile(ByVal location As String)
            SaveToXml()
            XmlSource.Save(location)
        End Sub

        Public Sub SaveToXml()

            XmlSource.Clear()
            XmlSource.Header() = Header()

            For Each Page1 In Pages

                XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name()})
                If Utility.IsEmpty(Page1.Note()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), NOTE_NAME}, Utility.Join(Page1.Note(), DEF_DELIM))
                If Utility.IsEmpty(Page1.Jump()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), JUMP_NAME}, Page1.Jump())
                If Utility.IsEmpty(Page1.Tags()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), TAG_NAME}, Page1.Tags())
                If Utility.IsEmpty(Page1.Inheritance()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), INHERIT_NAME}, Utility.Join(Page1.Inheritance(), DEF_DELIM))

                Dim i = 0
                For Each Nav1 In Page1.Navigate
                    If Utility.IsEmpty(Nav1.Detect()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), NAVIGATE_NAME, ENTRY_NAME & i, DETECT_NAME}, Utility.Join(Nav1.Detect(), DEF_DELIM), "")
                    If Nav1.Rate() > 0 Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), NAVIGATE_NAME, ENTRY_NAME & i, RATE_NAME}, Nav1.Rate())
                    If Utility.IsEmpty(Nav1.Tags()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), NAVIGATE_NAME, ENTRY_NAME & i, TAG_NAME}, Nav1.Tags())
                    If Utility.IsEmpty(Nav1.GoTo()) = False Then XmlSource.Create({ADDRESS_L1, ADDRESS_L2, Page1.Name(), NAVIGATE_NAME, ENTRY_NAME & i, GOTO_NAME}, Nav1.GoTo())
                    i += 1
                Next

            Next

        End Sub

        Public Sub LoadFromFile(ByVal location As String)
            XmlSource.Loadfile(location)
        End Sub
        Public Sub LoadFromXml(ByVal address As String())
            Header() = XmlSource.Header()

            For Each Node_1 In XmlSource.Children(address)

                'newPage.Name() = ""
                'newPage.Note() = ""
                'newPage.Tags() = ""
                'newPage.Navigate() = New List(Of Detection)

                'this filters out the node addresses that contain the page names
                'ie. story/chapter/start
                If Node_1.Depth = 2 Then
                    Dim newPage As New Page

                    'ie. <start>
                    newPage.Name = Node_1.Name


                    'this gets the list of childern nodes that are contained within the named node address
                    For Each Node_2 In XmlSource.Children(Node_1.Address())

                        If Node_2.Depth = 3 Then

                            '<note>
                            If Utility.Search(Node_2.Name(), NOTE_NAME, Gate.eEQUALS) = True Then
                                newPage.Note() = Utility.Trim(Utility.ToArray(Node_2.InnerText(True), DEF_DELIM))
                            End If

                            '<jump>
                            If Utility.Search(Node_2.Name(), JUMP_NAME, Gate.eEQUALS) = True Then
                                newPage.Jump() = Node_2.InnerText(True)
                            End If

                            '<inherit>
                            If Utility.Search(Node_2.Name(), INHERIT_NAME, Gate.eEQUALS) = True Then
                                newPage.Inheritance() = Utility.Trim(Utility.ToArray(Node_2.InnerText(True), DEF_DELIM))
                            End If

                            '<tags>
                            If Utility.Search(Node_2.Name(), TAG_NAME, Gate.eEQUALS) = True Then
                                newPage.Tags() = Node_2.InnerText(True)
                            End If

                            '<navagate>
                            If Utility.Search(Node_2.Name(), NAVIGATE_NAME, Gate.eEQUALS) = True Then

                                For Each Node_3 In XmlSource.Children(Node_2.Address())

                                    '<entry0>
                                    If Node_3.Depth = 4 Then
                                        Dim Navigation As New Detection

                                        For Each Node_4 In XmlSource.Children(Node_3.Address())


                                            If Node_4.Depth = 5 Then


                                                '<detect>
                                                If Utility.Search(Node_4.Name(), DETECT_NAME, Gate.eEQUALS) = True Then
                                                    Navigation.Detect() = Utility.Trim(Utility.ToArray(Node_4.InnerText(True), DEF_DELIM))
                                                End If

                                                '<goto>
                                                If Utility.Search(Node_4.Name(), GOTO_NAME, Gate.eEQUALS) = True Then
                                                    Navigation.GoTo() = Node_4.InnerText(True)
                                                End If

                                                '<rate>
                                                If Utility.Search(Node_4.Name(), RATE_NAME, Gate.eEQUALS) = True Then
                                                    Navigation.Rate() = Utility.ToInt(Node_4.InnerText(True), 0)
                                                End If

                                                '<tags>
                                                If Utility.Search(Node_4.Name(), TAG_NAME, Gate.eEQUALS) = True Then
                                                    Navigation.Tags() = Node_4.InnerText(True)
                                                End If


                                            End If

                                        Next

                                        newPage.Navigate().Add(Navigation)

                                    End If

                                Next

                            End If
                        End If

                    Next

                    Pages.Add(newPage)

                End If

            Next

            XmlSource.Clear()

        End Sub


        Public Function Print() As String
            Dim results = ""
            results &= "[HEADER]" & vbCrLf & Header & vbCrLf

            For Each pg In Pages

                Dim navs = ""
                For Each nav In pg.Navigate
                    navs &= "[NAVIGATIONS]" & vbCrLf & CPrint(": detect:", Utility.Join(nav.Detect, DEF_DELIM)) & CPrint(":GOTO:", nav.GoTo) & CPrint(":RATE:", nav.Rate.ToString) & CPrint(":TAGS:", nav.Tags) & vbCrLf
                Next

                results &= "[DETECTION]" & CPrint(":NAME:", pg.Name) & CPrint(":NOTE:", Utility.Join(pg.Note, DEF_DELIM)) & CPrint(":INHERIT:", Utility.Join(pg.Inheritance, DEF_DELIM)) & CPrint(":TAGS:", pg.Tags) & vbCrLf & navs & vbCrLf
            Next

            Return "[PAGE]" & vbCrLf & results
        End Function
        Private Function CPrint(ByVal name As String, ByVal other As String) As String
            If other = "" Then
                Return ""
            Else
                Return name & other
            End If
        End Function

        Public Sub Refresh()
            XmlSource.Refresh()
        End Sub

        Public Sub Create(ByVal name As String, ByVal note As String(), ByVal jump As String, ByVal tags As String, ByVal Inherit As String(), ByVal Navigate As List(Of Detection))
            If DoesPageExist(name) = False Then
                Add(name, note, jump, tags, Inherit, Navigate)
            Else
                Update(name, note, jump, tags, Inherit, Navigate)
            End If
        End Sub

        Public Sub Rename(ByVal oldname As String, ByVal newname As String)
            For Each page In Pages
                If Utility.Search(page.Name(), oldname, True, Gate.eEQUALS) = True Then
                    page.Name() = newname
                End If
            Next
        End Sub

        Public Sub Add(ByVal Page1 As Page)
            Pages.Add(Page1)
        End Sub
        Public Sub Add(ByVal name As String, ByVal note As String(), ByVal jump As String, ByVal tags As String, ByVal Inherit As String(), ByVal Navigate As List(Of Detection))
            Dim Pag1 As New Page
            If IsNothing(name) = False Then Pag1.Name() = name
            If IsNothing(note) = False Then Pag1.Note() = note
            If IsNothing(jump) = False Then Pag1.Jump() = jump
            If IsNothing(Inherit) = False Then Pag1.Inheritance() = Inherit
            If IsNothing(tags) = False Then Pag1.Tags() = tags
            If IsNothing(Navigate) = False Then Pag1.Navigate() = Navigate
            Pages.Add(Pag1)
        End Sub

        Public Sub AddNavigate(ByVal name As String, ByVal Navigation As Detection)
            Dim AllPages As New List(Of Page)

            For Each Page1 In Pages
                If Utility.Search(Page1.Name(), name, Gate.eEQUALS) = True Then
                    Page1.Navigate.Add(Navigation)
                End If

                AllPages.Add(Page1)
            Next

            Pages = AllPages
        End Sub

        Public Sub UpdateNavigate(ByVal page As String, ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal newDetection As Detection)
            'Updates Navigation By Detection and Goto
            Dim AllPages As New List(Of Page)

            For Each Page1 In Pages
                If Utility.Search(Page1.Name, page, Gate.eEQUALS) = True Then
                    Page1.UpdateDetection(detect, gotos, rate, tags, newDetection)
                End If

                AllPages.Add(Page1)
            Next

            Pages = AllPages
        End Sub
        Public Sub UpdateNavigate(ByVal page As String, ByVal oldDetection As Detection, ByVal newDetection As Detection)
            'Updates Navigation By Detection and Goto
            Dim AllPages As New List(Of Page)

            For Each Page1 In Pages
                If Utility.Search(Page1.Name, page, Gate.eEQUALS) = True Then
                    Page1.UpdateDetection(oldDetection.Detect, oldDetection.GoTo, oldDetection.Rate, oldDetection.Tag, newDetection)
                End If

                AllPages.Add(Page1)
            Next

            Pages = AllPages
        End Sub

        Public Sub RemoveNavigate(ByVal page As String, ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal removeAll As Boolean)

            For Each Page1 In Pages
                If Utility.Search(Page1.Name, page, Gate.eEQUALS) = True Then
                    Page1.RemoveDetection(detect, gotos, rate, tags, removeAll)
                End If
            Next

        End Sub
        Public Sub RemoveNavigate(ByVal page As String, ByVal detection As Detection, ByVal removeAll As Boolean)

            For Each Page1 In Pages
                If Utility.Search(Page1.Name, page, Gate.eEQUALS) = True Then
                    Page1.RemoveDetection(detection.Detect, detection.GoTo, detection.Rate, detection.Tag, removeAll)
                End If
            Next

        End Sub

        Public Function GetNavigate(ByVal page As String, ByVal detect As String, ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags) As List(Of Detection)
            For Each Page1 In Pages
                If Utility.Search(Page1.Name, page, Gate.eEQUALS) = True Then
                    Return Page1.GetDetection(detect, gotos, rate, tags)
                End If
            Next
            Return New List(Of Detection)
        End Function

        Public Sub UpdateRate(ByVal page As String, ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal change As Integer)
            Dim AllPages As New List(Of Page)

            For Each Pag1 In Pages

                If Utility.Search(Pag1.Name(), page, Gate.eEQUALS) = True Then
                    Pag1.UpdateRate(detect, gotos, rate, tags, change)
                End If

                AllPages.Add(Pag1)
            Next

            Pages = AllPages
        End Sub

        Public Sub Update(ByVal page As String, ByVal Page1 As Page)
            Dim AllPages As New List(Of Page)

            For Each Pag1 In Pages

                If Utility.Search(Pag1.Name(), page, Gate.eEQUALS) = True Then
                    Pag1 = Page1
                End If

                AllPages.Add(Pag1)
            Next

            Pages = AllPages

        End Sub

        Public Sub Update(ByVal page As String, ByVal note As String(), ByVal jump As String, ByVal tags As String, ByVal Inherit As String(), ByVal Navigate As List(Of Detection))
            Dim AllPages As New List(Of Page)

            For Each Pag1 In Pages

                If Utility.Search(Pag1.Name(), page, Gate.eEQUALS) = True Then
                    If IsNothing(note) = False Then Pag1.Note() = note
                    If IsNothing(jump) = False Then Pag1.Jump() = jump
                    If IsNothing(Inherit) = False Then Pag1.Inheritance() = Inherit
                    If IsNothing(tags) = False Then Pag1.Tags() = tags
                    If IsNothing(Navigate) = False Then Pag1.Navigate() = Navigate
                End If

                AllPages.Add(Pag1)
            Next

            Pages = AllPages

        End Sub

        Public Sub Remove(ByVal name As String)

            Dim AllPages As New List(Of Page)

            For Each Pag1 In Pages
                If Utility.Search(Pag1.Name(), name, Gate.eEQUALS) = False Then
                    AllPages.Add(Pag1)
                End If
            Next

            Pages = AllPages
        End Sub

        Public Sub Clear()
            Header() = ""
            Pages.Clear()
        End Sub

        Public Function SearchNotes(ByVal find As String, ByVal eGate As Gate) As List(Of Page)
            'This searches all notes in all pages and returns all the pages
            Dim newPages As New List(Of Page)

            For Each Page1 In Pages
                If Utility.Search(Page1.Note(), find, eGate) = True Then
                    newPages.Add(Page1)
                End If
            Next

            Return newPages
        End Function

        Public Function Find(ByVal name As String) As Page
            For Each pg In Pages
                If Utility.Search(pg.Name, name, Gate.eEQUALS) = True Then
                    Return pg
                End If
            Next
            Return New Page
        End Function

        Public Function DoesPageExist(ByVal page As String) As Boolean

            For Each Pag1 In Pages
                If Utility.Search(Pag1.Name(), page, Gate.eEQUALS) = True And Pag1.Name() > "" Then
                    Return True
                End If
            Next

            Return False
        End Function

        Public Function DoesNavigateExist(ByVal page As String, ByVal navigate As Detection) As Boolean
            'Detects by Detect and Goto
            For Each Page1 In Pages

                If Utility.Search(page, Page1.Name(), Gate.eEQUALS) = True Then
                    For Each Nav1 In Page1.Navigate
                        If Utility.Search(Nav1.GoTo(), navigate.GoTo(), Gate.eEQUALS) = True Then
                            If Utility.Search(Nav1.Detect(), navigate.Detect(), True, Gate.eEQUALS) = True Then
                                Return True
                            End If
                        End If
                    Next
                End If

            Next

            Return False
        End Function

        Public Function DoesDetectNoteExist(ByVal page As String, ByVal detect As String(), ByVal note As String(), ByRef gotoPage As Chapter.Page) As Boolean
            'This checks to see if the detect-note pare exists 
            'and if it does exist then returns the page to which the note exists

            If DoesPageExist(page) = True Then
                Dim page_node = Find(page)

                For Each Nav1 In page_node.Navigate

                    If Utility.Search(Nav1.Detect, detect, True, Gate.eEQUALS) = True Then
                        Dim goto_node = Find(Nav1.GoTo)

                        If Utility.Search(goto_node.Note, note, True, Gate.eEQUALS) = True Then
                            gotoPage = goto_node
                            Return True
                        End If
                    End If
                Next

            End If

            Return False
        End Function

        Public Function DoesDetectExist(ByVal page As String, ByVal detect As String, ByRef navigate As List(Of Detection)) As Boolean
            'Detects by Detect and Goto
            Dim Results As Boolean = False
            For Each Page1 In Pages

                If Utility.Search(page, Page1.Name(), Gate.eEQUALS) = True Then
                    For Each Nav1 In Page1.Navigate
                        If Utility.Search(Nav1.Detect(), detect, Gate.eEQUALS) = True Then
                            navigate.Add(Nav1)
                            Results = True
                        End If
                    Next
                End If

            Next

            Return Results
        End Function

        Public Function GetGotoPages(ByVal page As String) As List(Of Page)

            Dim Allpages As New List(Of Page)

            For Each page1 In Pages
                If Utility.Search(page1.Name(), page, Gate.eEQUALS) = True Then

                    For Each Nav1 In page1.Navigate
                        Allpages.Add(Find(Nav1.GoTo))
                    Next
                    Return Allpages
                End If

            Next

            Return Allpages
        End Function


        Public Class Page
            'This is used to store the detection that was chosen which led to the Goto page
            'This is only used when storing Page history
            Public Sub New()

            End Sub
            Public Sub New(ByVal name As String, ByVal note As String(), ByVal jump As String, ByVal tags As String, ByVal navigate As List(Of Detection))
                Me.Name = name
                Me.Note = note
                Me.Jump() = jump
                Me.Tags = tags
                Me.Navigate = navigate
            End Sub

            Public Function GetDetection(ByVal detect As String, ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags) As List(Of Detection)
                Dim DetList As New List(Of Detection)
                For Each nav In Navigate

                    If IsNothing(detect) = True Then GoTo Line0
                    If Utility.Search(nav.Detect, detect, Gate.eEQUALS) = True Then

Line0:                  If IsNothing(gotos) = True Then GoTo Line1
                        If Utility.Search(nav.GoTo, gotos, Gate.eEQUALS) = True Then

Line1:                      If IsNothing(tags) = True Then GoTo Line2
                            If nav.Tag.CompareTags(tags) = True Then

Line2:                          If IsNothing(rate) = True Then GoTo Line3
                                If nav.Rate = rate Or IsNothing(rate) = True Then

Line3:                              DetList.Add(nav)
                                End If
                            End If
                        End If
                    End If
                Next

                Return DetList
            End Function

            Public Sub UpdateDetection(ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal newDetection As Detection)
                Dim DetList As New List(Of Detection)
                For Each nav In Navigate

                    If IsNothing(detect) = True Then GoTo Line0
                    If Utility.Search(nav.Detect, detect, True, Gate.eEQUALS) = True Then

Line0:                  If IsNothing(gotos) = True Then GoTo Line1
                        If Utility.Search(nav.GoTo, gotos, Gate.eEQUALS) = True Then

Line1:                      If IsNothing(tags) = True Then GoTo Line2
                            If nav.Tag.CompareTags(tags) = True Then

Line2:                          If IsNothing(rate) = True Then GoTo Line3
                                If nav.Rate = rate Or IsNothing(rate) = True Then

Line3:                              nav.Detect = Utility.Defaults(newDetection.Detect, {""})
                                    nav.GoTo = Utility.Defaults(newDetection.GoTo, "")
                                    nav.Rate = Utility.Defaults(newDetection.Rate, 0)
                                    nav.Tags = Utility.Defaults(newDetection.Tags, "")
                                End If
                            End If
                        End If
                    End If

                    DetList.Add(nav)
                Next

                Navigate = DetList
            End Sub

            Public Sub RemoveDetection(ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal removeAll As Boolean)
                Dim DetList As New List(Of Detection)

                Dim count = 0
                Dim threshold = 0

                For Each nav In Navigate
                    Dim IsFound As Boolean = False

                    If IsNothing(detect) = True Then GoTo Line0
                    If Utility.Search(nav.Detect, detect, True, Gate.eEQUALS) = True Then

Line0:                  If IsNothing(gotos) = True Then GoTo Line1
                        If Utility.Search(nav.GoTo, gotos, Gate.eEQUALS) = True Then

Line1:                      If IsNothing(tags) = True Then GoTo Line2
                            If nav.Tag.CompareTags(tags) = True Then

Line2:                          If IsNothing(rate) = True Then GoTo Line3
                                If nav.Rate = rate Then

Line3:                              If count <= threshold Or removeAll = True Then
                                        IsFound = True
                                    End If

                                    count += 1
                                End If
                            End If
                        End If
                    End If

                    If IsFound = False Then
                        DetList.Add(nav)
                    End If

                Next

                Navigate = DetList

            End Sub

            Public Sub UpdateRate(ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal tags As Tags, ByVal change As Integer)
                Dim DetList As New List(Of Detection)

                For Each nav In Navigate

                    If IsNothing(detect) = True Then GoTo Line0
                    If Utility.Search(nav.Detect, detect, True, Gate.eEQUALS) = True Then

Line0:                  If IsNothing(gotos) = True Then GoTo Line1
                        If Utility.Search(nav.GoTo, gotos, Gate.eEQUALS) = True Then

Line1:                      If IsNothing(tags) = True Then GoTo Line2
                            If nav.Tag.CompareTags(tags) = True Then

Line2:                          If rate = -1 Then GoTo Line3
                                If nav.Rate = rate Then

Line3:                              nav.Delta(change)
                                End If
                            End If
                        End If
                    End If

                    DetList.Add(nav)
                Next

                Navigate = DetList
            End Sub

            Public Property Tag() As New Tags
            Public Property Name() As String = ""
            Public Property Note() As String() = {""}
            Public Property Jump() As String = ""
            Public Property Inheritance() As String() = {""}

            Public ReadOnly Property RandomNote(Optional ByVal chances As Integer() = Nothing) As String
                Get
                    Utility.IsNull(chances)
                    If chances.Count() = 0 Then
                        Return Utility.Trim(Utility.RandArray(Note(), ""))
                    Else
                        Return Utility.Trim(Utility.RandArray(Note(), chances, ""))
                    End If
                End Get
            End Property
            Public Property Tags() As String
                Get
                    Return Tag.Print()
                End Get
                Set(value As String)
                    Tag.Load(value)
                End Set
            End Property
            Public Property Navigate() As New List(Of Detection)

            Public Function CopyNavigate() As List(Of Detection)
                Dim newList As New List(Of Detection)

                For Each Nav In Navigate
                    Dim newDetect As New Detection
                    newDetect.Copy(Nav)
                    newList.Add(newDetect)
                Next

                Return newList
            End Function


        End Class

    End Class


    Public Class Detection

        Public Sub New()

        End Sub
        Public Sub New(ByVal detect As String(), ByVal gotos As String, ByVal rate As Integer, ByVal newtags As Tags)
            Me.Detect() = detect
            Me.GoTo() = gotos
            Me.Rate() = rate
            Me.Tag() = newtags
        End Sub

        Public Sub Copy(ByVal toCopy As Detection)
            Detect() = toCopy.Detect()
            [GoTo]() = toCopy.GoTo()
            Rate() = toCopy.Rate()
            Tag() = toCopy.Tag()
        End Sub

        Public Sub Delta(ByVal change As Integer)
            Rate() += change
            If Rate < 0 Then Rate = 0
        End Sub

        Public Property Tag() As New Tags
        Public Property Detect() As String() = {""}
        Public Property [GoTo]() As String = ""
        Public Property Rate() As Integer = 0
        Public Property Tags() As String
            Get
                Return Tag.Print()
            End Get
            Set(value As String)
                Tag.Load(value)
            End Set
        End Property
    End Class


End Class

Public Class Tags
    Public Property TagList() As New List(Of Tag)
    Public Const DEF_TAGS As String = "#"
    Public Const DEF_PROPERTY As String = ":"
    Public Const DEF_VALUES As String = "|"
    Public Const DEF_OPEN As String = "["
    Public Const DEF_CLOSE As String = "]"
    Public Const DEF_END As String = vbCrLf

    '#state[or]:happy|smart

    Public Class Tag
        Public Sub New()

        End Sub
        Public Sub New(ByVal name As String, ByVal values As String())
            Me.Name() = name
            Me.Values() = values
        End Sub
        Public Sub New(ByVal name As String, ByVal value As String)
            Me.Name() = name
            Me.Values() = {value}
        End Sub
        Public Sub New(ByVal name As String, ByVal value As String, ByVal search As String)
            Me.Name() = name
            Me.Values() = {value}
            Me.Search() = search
        End Sub
        Public Sub New(ByVal name As String, ByVal values As String(), ByVal search As String)
            Me.Name() = name
            Me.Values() = values
            Me.Search() = search
        End Sub

        Public Property Name() As String = ""
        Public Property Values() As String()
        Public Function Index(ByVal indx As Integer, Optional defaults As String = "") As String
            Return Utility.GetIndex(Values(), indx, defaults)
        End Function
        Public Property Search() As String = ""
    End Class

    Public Function IsEmpty() As Boolean
        If TagList.Count = 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub Load(ByVal tags As String)
        TagList().Clear()
        Dim alltags = Utility.ToArray(tags, DEF_TAGS)

        For Each item In alltags
            item = Utility.Trim(item)
            Dim entries = Utility.ToArray(item, DEF_PROPERTY)

            Dim name = Utility.GetIndex(entries, 0, "")
            Dim search = Utility.GetMidArg(name, DEF_OPEN, DEF_CLOSE, Gate.eEQUALS)
            name = Utility.Trim(Utility.Remove(name, search))
            search = Utility.Trim(search)
            Dim values = Utility.Trim(Utility.ToArray(Utility.GetIndex(entries, 1, ""), DEF_VALUES))

            If Utility.IsEmpty(name) = False Then
                If Utility.IsEmpty(values) = False Then
                    TagList.Add(New Tag(name, values, search))
                End If
            End If

        Next

    End Sub

    Public Sub New()
    End Sub
    Public Sub New(ByVal tags As String)
        Load(tags)
    End Sub

    Public Function Count() As Integer
        Return TagList().Count()
    End Function

    Public Function GetValue(ByVal name As String, Optional ByVal remove As Boolean = False) As String()
        Dim results As String() = {}

        For Each item In TagList()
            If Utility.Search(item.Name(), name, Gate.eEQUALS) = True Then
                results = item.Values()
            End If
        Next

        If remove = True Then
            Me.Remove(name)
        End If

        Return results
    End Function
    Public Function GetValue(ByVal name As String, ByVal index As Integer, ByVal defaults As String, Optional ByVal remove As Boolean = False) As String
        Dim results = Utility.GetIndex(GetTag(name).Values(), index, defaults)

        If remove = True Then
            Me.Remove(name)
        End If

        Return results
    End Function
    Public Function GetSearch(ByVal name As String) As String
        For Each item In TagList()
            If Utility.Search(item.Name(), name, Gate.eEQUALS) = True Then
                Return item.Search()
            End If
        Next
        Return ""
    End Function
    Public Function GetTag(ByVal name As String) As Tag
        For Each item In TagList()
            If Utility.Search(item.Name(), name, Gate.eEQUALS) = True Then
                Return item
            End If
        Next
        Return New Tag
    End Function


    Public Sub Update(ByVal newTag As Tag)
        For Each item In TagList()
            If Utility.Search(item.Name(), newTag.Name(), Gate.eEQUALS) = True Then
                item.Values() = newTag.Values()
                item.Search() = newTag.Search()
            End If
        Next
    End Sub

    Public Sub Update(ByVal name As String, ByVal newValue As String())
        For Each item In TagList()
            If Utility.Search(item.Name(), name, Gate.eEQUALS) = True Then
                item.Values() = newValue
            End If
        Next
    End Sub
    Public Sub Update(ByVal name As String, ByVal newValue As String)
        Update(name, {newValue})
    End Sub
    Public Sub Update(ByVal name As String, ByVal newValue As String(), ByVal eGate As Gate)
        For Each item In TagList()
            If Utility.Search(item.Name(), name, eGate) = True Then
                item.Values() = newValue
            End If
        Next
    End Sub
    Public Sub Update(ByVal name As String, ByVal newValue As String, ByVal eGate As Gate)
        Update(name, {newValue}, eGate)
    End Sub
    Public Sub Update(ByVal name As String, ByVal newValue As String(), ByVal search As String, ByVal eGate As Gate)
        For Each item In TagList()
            If Utility.Search(item.Name(), name, eGate) = True Then
                item.Values() = newValue
                item.Search() = search
            End If
        Next
    End Sub
    Public Sub Update(ByVal name As String, ByVal newValue As String, ByVal search As String)
        Update(name, {newValue}, search)
    End Sub


    Public Function DoesValueExist(ByVal name As String, ByVal value As String()) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, name, Gate.eEQUALS) = True Then
                If Utility.Compare(item.Values, value, True, Gate.eEQUALS) = True Then Return True
            End If
        Next
        Return False
    End Function
    Public Function DoesValueExist(ByVal name As String, ByVal value As String) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, name, Gate.eEQUALS) = True Then
                If Utility.Search(item.Values, value, Gate.eEQUALS) = True Then Return True
            End If
        Next
        Return False
    End Function
    Public Function DoesValueExist(ByVal name As String, ByVal value As String, ByVal index As Integer) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, name, Gate.eEQUALS) = True Then
                If Utility.Search(Utility.GetIndex(item.Values, index, ""), value, Gate.eEQUALS) = True Then Return True
            End If
        Next
        Return False
    End Function
    Public Function DoesValueExist(ByVal tag As Tag) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, tag.Name, Gate.eEQUALS) = True Then
                If Utility.Compare(item.Values, tag.Values, True, Gate.eEQUALS) = True Then Return True
            End If
        Next
        Return False
    End Function
    Public Function DoesValueExist(ByVal tags As Tags) As Boolean
        For Each tag1 In tags.TagList()
            If DoesValueExist(tag1) = False Then
                Return False
            End If
        Next
        Return True
    End Function
    Public Function DoesValueExist(ByVal tags As String) As Boolean
        For Each tag1 In New Tags(tags).TagList()
            If DoesValueExist(tag1) = False Then
                Return False
            End If
        Next
        Return True
    End Function

    Public Const DEF_COMPARE As String = "compare"
    Public Const DEF_NOR As String = "nor"
    Public Const DEF_OR As String = "or"
    Public Const DEF_GREATER As String = "greater"
    Public Const DEF_LESSER As String = "lesser"
    Public Const DEF_EQUALS As String = "equals"
    Public Const DEF_RANGE As String = "range"
    Public Const DEF_TAGTYPE As String = "tagtype"
    Public Const DEF_NOT As String = "not"
    Public Const DEF_INCLUDE As String = "include"

    Public Function CompareTags(ByVal tags As Tags) As Boolean

        If tags.Count() = Count() Then
            For Each tag1 In tags.TagList
                If DoesValueExist(tag1) = False Then
                    Return False
                End If
            Next
        Else
            Return False
        End If

        Return True
    End Function

    Public Function DoTagsMatch(ByVal tags As Tags) As Boolean
        'this returns true only if the all local tags exist in this function's input tags  
        'if there are no local tags then all local tags were found and thus returns a value of true

        Dim tagtype As String() = GetValue(DEF_TAGTYPE)
        Dim typeNOT As Boolean = False
        Dim typeINCLUDE As Boolean = False

        If Utility.Search(DEF_NOT, tagtype, True, Gate.eOR) = True Then
            typeNOT = True
        End If
        If Utility.Search(DEF_INCLUDE, tagtype, True, Gate.eOR) = True Then
            typeINCLUDE = True
        End If

        If typeNOT = True Then
            If Count() = 0 Then Return False
        Else
            If Count() = 0 Then Return True
        End If

        Dim found As Boolean = True

        For Each tag1 In TagList()
            If tags.DoesVaribleExist(tag1.Name()) = True Then

                If Utility.Search(tag1.Search(), DEF_OR, True, Gate.eEQUALS) = True Then
                    found = Utility.Search(tags.GetValue(tag1.Name(), 0, ""), tag1.Values(), True, Gate.eEQUALS)

                ElseIf Utility.Search(tag1.Search(), DEF_NOR, True, Gate.eEQUALS) = True Then
                    found = Utility.Search(tags.GetValue(tag1.Name(), 0, ""), tag1.Values(), True, Gate.eEQUALS) = False

                ElseIf Utility.Search(tag1.Search(), DEF_GREATER, True, Gate.eEQUALS) = True Then
                    Dim int_input = Utility.ToDbl(Utility.GetIndex(tag1.Values(), 0, "0"))
                    Dim int_internal = Utility.ToDbl(GetValue(tag1.Name(), 0, "0"))
                    found = Utility.CompareDbl(int_internal, int_input, OCompare.GreaterEquals)

                ElseIf Utility.Search(tag1.Search(), DEF_LESSER, True, Gate.eEQUALS) = True Then
                    Dim int_input = Utility.ToDbl(Utility.GetIndex(tag1.Values(), 0, "0"))
                    Dim int_internal = Utility.ToDbl(GetValue(tag1.Name(), 0, "0"))
                    found = Utility.CompareDbl(int_internal, int_input, OCompare.LesserEquals)

                ElseIf Utility.Search(tag1.Search(), DEF_RANGE, True, Gate.eEQUALS) = True Then
                    Dim int_min = Utility.ToDbl(Utility.GetIndex(tag1.Values(), 0, "0"))
                    Dim int_max = Utility.ToDbl(Utility.GetIndex(tag1.Values(), 1, "0"))
                    Dim int_internal = Utility.ToDbl(GetValue(tag1.Name(), 0, "0"))
                    found = Utility.InRange(int_internal, int_min, int_max, True)

                ElseIf Utility.Search(tag1.Search(), DEF_EQUALS, True, Gate.eEQUALS) = True Then
                    Dim int_input = Utility.ToDbl(Utility.GetIndex(tag1.Values(), 0, "0"))
                    Dim int_internal = Utility.ToDbl(GetValue(tag1.Name(), 0, "0"))
                    found = Utility.CompareDbl(int_internal, int_input, OCompare.Equals)

                ElseIf Utility.Search(tag1.Search(), DEF_COMPARE, True, Gate.eEQUALS) = True Then
                    found = tags.DoesValueExist(tag1)
                Else
                    found = tags.DoesValueExist(tag1)
                End If

            Else
                If typeINCLUDE = True Then found = False
            End If

            If found = False Then Exit For
        Next

        If typeNOT = True Then
            If found = False Then
                Return True
            Else
                Return False
            End If
        Else
            If found = False Then
                Return False
            Else
                Return True
            End If
        End If

    End Function

    Public Function DoesVaribleExist(ByVal name As String, ByVal eGate As Gate) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, name, eGate) = True Then
                Return True
            End If
        Next
        Return False
    End Function
    Public Function DoesVaribleExist(ByVal name As String) As Boolean
        For Each item In TagList()
            If Utility.Search(item.Name, name, Gate.eEQUALS) = True Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Sub Add(ByVal newTag As Tag)
        TagList.Add(newTag)
    End Sub
    Public Sub Add(ByVal name As String, ByVal values As String())
        TagList.Add(New Tag(name, values))
    End Sub
    Public Sub Add(ByVal name As String, ByVal value As String)
        TagList.Add(New Tag(name, value))
    End Sub
    Public Sub Add(ByVal name As String, ByVal values As String(), ByVal search As String)
        TagList.Add(New Tag(name, values, search))
    End Sub
    Public Sub Add(ByVal name As String, ByVal value As String, ByVal search As String)
        TagList.Add(New Tag(name, value, search))
    End Sub


    Public Property Indexes(ByVal name As String, Optional ByVal remove As Boolean = False) As String()
        Get
            Return GetValue(name, remove)
        End Get
        Set(value As String())
            If DoesVaribleExist(name) = True Then
                Update(name, value)
            Else
                Add(name, value)
            End If
        End Set
    End Property
    Public ReadOnly Property Index(ByVal name As String, ByVal defaults As String, Optional ByVal remove As Boolean = False) As String
        Get
            Return GetValue(name, 0, defaults, remove)
        End Get
    End Property
    Public WriteOnly Property Index(ByVal name As String) As String
        Set(value As String)
            If DoesVaribleExist(name) = True Then
                Update(name, value)
            Else
                Add(name, value)
            End If
        End Set
    End Property

    Public Sub AddRange(ByVal newTags As List(Of Tag))
        TagList.AddRange(newTags)
    End Sub
    Public Sub AddRange(ByVal newTags As Tags)
        TagList.AddRange(newTags.TagList())
    End Sub

    Public Sub Merge(ByVal newTags As List(Of Tag))
        For Each tag1 In newTags
            If DoesVaribleExist(tag1.Name()) = True Then
                Update(tag1)
            Else
                Add(tag1)
            End If
        Next
    End Sub
    Public Sub Merge(ByVal newTags As Tags)
        For Each tag1 In newTags.TagList()
            If DoesVaribleExist(tag1.Name()) = True Then
                Update(tag1)
            Else
                Add(tag1)
            End If
        Next
    End Sub
    Public Sub Merge(ByVal newTags As String)
        Dim newTagList As New Tags
        newTagList.Load(newTags)

        For Each tag1 In newTagList.TagList()
            If DoesVaribleExist(tag1.Name()) = True Then
                Update(tag1)
            Else
                Add(tag1)
            End If
        Next
    End Sub


    Public Sub Remove(ByVal name As String)
        Dim newTags As New List(Of Tag)

        For Each item In TagList()
            If Utility.Search(item.Name, name, Gate.eEQUALS) = False Then
                newTags.Add(item)
            End If
        Next

        TagList() = newTags
    End Sub
    Public Sub Remove(ByVal name As String, ByVal eGate As Gate)
        Dim newTags As New List(Of Tag)

        For Each item In TagList()
            If Utility.Search(item.Name, name, eGate) = False Then
                newTags.Add(item)
            End If
        Next

        TagList() = newTags
    End Sub

    Public Sub Clear()
        TagList.Clear()
    End Sub
    Public Function Print() As String
        Dim results As String = ""

        For Each tag In TagList()
            If Utility.IsEmpty(tag.Search()) = True Then
                results = results & DEF_TAGS & tag.Name & DEF_PROPERTY & Utility.Join(tag.Values, DEF_VALUES) & DEF_END
            Else
                results = results & DEF_TAGS & tag.Name & DEF_OPEN & tag.Search() & DEF_CLOSE & DEF_PROPERTY & Utility.Join(tag.Values, DEF_VALUES) & DEF_END
            End If
        Next

        Return Utility.Trim(results)
    End Function

End Class


'***************************
'Natural Language Processing
'***************************

Public Class NLP


#Region "Local Variables"

    Public Property General() As New Relations
    Public Shared ReadOnly Property DEF_CAT As String() = {"categories"}
    Public Shared ReadOnly Property DEF_COMP As String() = {"compounds"}
    Public Shared ReadOnly Property DEF_POS As String() = {"generalpos"}
    Public Shared ReadOnly Property DEF_SYN As String() = {"synonyms"}

    Private Property IgnoreCase() As Boolean = True

    'noun::cow

    Public Shared ReadOnly Property DASH As String() = {"[DASH]"}
    Public Shared ReadOnly Property COMMA As String() = {"[COMMA]"}
    Public Shared ReadOnly Property DQUOT As String() = {"[DQUOT]"}
    Public Shared ReadOnly Property SQUOT As String() = {"[SQUOT]"}
    Public Shared ReadOnly Property OPAR As String() = {"[OPAR]"}
    Public Shared ReadOnly Property CPAR As String() = {"[CPAR]"}

    Private Shared ReadOnly Property DEF_DASH As String() = {"-", " [DASH] "}
    Private Shared ReadOnly Property DEF_COMMA As String() = {",", " [COMMA] "}
    Private Shared ReadOnly Property DEF_DQUOT As String() = {"""", " [DQUOT] "}
    Private Shared ReadOnly Property DEF_SQUOT As String() = {"'", " [SQUOT] "}
    Private Shared ReadOnly Property DEF_OPAR As String() = {"(", " [OPAR] "}
    Private Shared ReadOnly Property DEF_CPAR As String() = {")", " [CPAR] "}


    'This is an easy means of processing a string of text within a sentence
    Private m_original As String
    Public Property Original() As String
        Get
            Return removeTags(m_original)
        End Get
        Set(value As String)
            value = setTags(value)
            value = Utility.SingleSpace(value)
            m_original = value
        End Set
    End Property


    Public Property ToArray() As String()
    Public Property ToList() As New List(Of String)
    Public Function Index(ByVal int As Integer) As String
        Return Utility.GetIndex(ToList(), int, "")
    End Function



    Private Function ToCompounds(ByVal text As String) As List(Of String())
        'This goes through a sentence text and finds compound words 
        'then is stored those compound words in single units (as a string())
        'blue jay is judged as a single unit

        'Dim compounds = General.Find(DEF_COMP, DEF_COMP, Nothing, IgnoreCase(), Gate.eEQUALS)
        Dim compounds = General.GetEntries(DEF_COMP, IgnoreCase(), Gate.eEQUALS)
        Dim newList As New List(Of String())

        Dim lvl As Integer = 0
        Dim wFound As Boolean = False

        Do Until lvl >= ToArray.Count()
            wFound = False

            For Each cWord In compounds
                Dim wCount = cWord.Length()

                If wCount > 1 Then
                    Dim sWord = Utility.GetRange(ToArray(), lvl, lvl + wCount - 1)

                    If Utility.Compare(cWord, sWord, IgnoreCase(), Gate.eEQUALS) = True Then
                        newList.Add(sWord)
                        wFound = True
                        lvl += wCount
                        Exit For
                    End If
                End If
            Next

            If wFound = False Then
                Dim word = Utility.GetRange(ToArray(), lvl, lvl)
                lvl += 1
                newList.Add(word)
            End If
        Loop

        Return newList
    End Function

    Private Function ToRootSynonyms(ByVal input As List(Of String())) As List(Of String())
        Dim newList As New List(Of String())

        For Each i In input
            newList.Add(GetRandomParent(i, NLPC.SYN))
        Next

        Return newList
    End Function



    'This is the word/array count of the sentence
    Public Property Count() As Integer = 0
    Public Property WCount() As Integer = 0

    'This is the sentence array
    Public Property SEN() As New List(Of String())
    Public ReadOnly Property SEN(ByVal index As Integer) As String()
        Get
            Return Utility.GetIndex(SEN(), index, Nothing)
        End Get
    End Property

    'This is a refined list of PofS for every word found in the sentence array (SEN)
    'We fill this array by selecting PofS from POSL, choosing only one PofS per Sentence word
    Public Property POS() As New List(Of String())
    Public Property POS(ByVal index As Integer) As String()
        Get
            Return Utility.GetIndex(POS(), index, Nothing)
        End Get
        Set(value As String())
            If Count > index And index > -1 Then
                POS() = Utility.SetIndex(POS(), index, value)
            End If
        End Set
    End Property

    'This is the list of every known PofS for every word in the sentence array (SEN)
    Public Property POSL() As New List(Of List(Of String()))
    Public Property POSL(ByVal index As Integer) As List(Of String())
        Get
            Return Utility.GetIndex(POSL(), index, Nothing)
        End Get
        Set(value As List(Of String()))
            If Count > index And index > -1 Then
                POSL() = Utility.SetIndex(POSL(), index, value)
            End If
        End Set
    End Property

    'This contains a list of categories for every word in a sentence array
    Public Property CAT() As New List(Of List(Of String()))
    Public Property CAT(ByVal index As Integer) As List(Of String())
        Get
            Return Utility.GetIndex(CAT(), index, Nothing)
        End Get
        Set(value As List(Of String()))
            If Count > index And index > -1 Then
                CAT() = Utility.SetIndex(CAT(), index, value)
            End If
        End Set
    End Property


    Public Property Tags() As New Tags

#End Region


#Region "Main"

    Public Property Location() As String

    Public Sub Load()
        General.LoadAll(Location())
    End Sub

    Private Sub Clear()
        POS.Clear()
        POSL.Clear()
        CAT.Clear()
    End Sub

    Private Sub LOAD_ARRAYS()
        ToArray() = Utility.ToArray(m_original, " ")
        ToList() = Utility.ToList(m_original, " ")
        Dim compounds = ToCompounds(m_original)
        SEN() = ToRootSynonyms(compounds)
        Count() = SEN().Count()
        WCount() = ToArray().Length()
    End Sub

    Public Sub Save()
        General.SaveAll(Location())
    End Sub

    Public Function Print() As String
        Dim text As String = ""
        text &= ":SENTENCE:" & vbCrLf

        text &= Original() & vbCrLf

        text &= vbCrLf & ":FULL_POS_LIST:" & vbCrLf

        'Full List of possible POS
        For i = 0 To Count - 1
            text &= "*" & Utility.Join(SEN(i), " ") & vbCrLf
            For Each PS In POSL(i)
                text &= "   " & Utility.Join(PS, " ") & vbCrLf
            Next
        Next

        text &= vbCrLf & vbCrLf & ":REFINED_POS_LIST:" & vbCrLf

        'Refined List of probable POS
        For i = 0 To Count - 1
            text &= "*" & Utility.Join(SEN(i), " ") & vbCrLf & "   " & Utility.Join(POS(i), " ") & vbCrLf
        Next

        text &= vbCrLf & vbCrLf & ":CAT_LIST:" & vbCrLf

        'Refined List of probable CAT
        For i = 0 To Count - 1
            text &= "*" & Utility.Join(SEN(i), " ") & vbCrLf
            For Each PS In CAT(i)
                text &= "   " & Utility.Join(PS, " ") & vbCrLf
            Next
        Next

        Return text
    End Function

    Private m_posdef As Object() = {DEF_POS}
    Public Property POSDef() As Object()
        Set(value As Object())
            value = CleanObjectList(value)
            value = Utility.SetIndex(value, value.Count(), DEF_POS())
            m_posdef = value
        End Set
        Get
            Return m_posdef
        End Get
    End Property

    Private m_catdef As Object() = {DEF_CAT}
    Public Property CATDef() As Object()
        Set(value As Object())
            value = CleanObjectList(value)
            value = Utility.SetIndex(value, value.Count(), DEF_CAT())
            m_catdef = value
        End Set
        Get
            Return m_catdef
        End Get
    End Property

    Private m_syndef As Object() = {DEF_SYN}
    Public Property SYNDef() As Object()
        Set(value As Object())
            value = CleanObjectList(value)
            value = Utility.SetIndex(value, value.Count(), DEF_SYN())
            m_syndef = value
        End Set
        Get
            Return m_syndef
        End Get
    End Property


    Private Sub LOAD_POS()
        'This looks through the sentence array (SEN) to find the PofS for every word.
        'All categories associated with that word will be added to the POSL list
        'The resources used to identify parts of speech are: the General Relation and a hardcoded Class of Resources

        For Each WD In SEN()
            Dim G_POS As New List(Of String())

            For Each posd In POSDef
                G_POS.AddRange(General.Find(posd, Nothing, WD, IgnoreCase(), Gate.eEQUALS))
            Next

            POSL.Add(G_POS)
        Next

    End Sub

    Private Sub LOAD_CAT()
        'hypernyms = general words, ie. flower is general for daisy
        'hyponym = sub-type words, ie. daisy is a sub-type, more specific, of flower
        'holonym = something that has parts, ie, an apple tree is a whole part of a an apple
        'meronym = part of something, ie. an apple is a part of an apple tree

        'this could contain
        'hypernym
        'holonym 
        'synonym (general::alternative)


        For Each WD In SEN()
            Dim G_POS As New List(Of String())

            For Each catd In CATDef
                G_POS.AddRange(General.Find(catd, Nothing, WD, IgnoreCase(), Gate.eEQUALS))
            Next

            CAT.Add(G_POS)
        Next

    End Sub

    Public Sub Parse()
        Clear()
        LOAD_ARRAYS()
        LOAD_POS()
        LOAD_CAT()
    End Sub


#End Region


#Region "PUBLIC"

    Public Function Split(ByVal words As String) As String()
        Return Utility.WordArray(words)
    End Function

    Public Function Join(ByVal words As String()) As String
        words = Utility.Trim(words)
        Return Utility.Join(words, " ")
    End Function

    Public Function ToListOfArray(ByVal Objects As Object()) As List(Of String())
        Dim newList As New List(Of String())

        For Each entry In Objects
            Dim newText As String()

            If TypeOf entry Is String Then
                newText = Split(entry)
            ElseIf TypeOf entry Is Integer Then
                newText = Split(Utility.ToStr(entry))
            ElseIf TypeOf entry Is String() Then
                newText = TryCast(entry, String())
            Else
                newText = {}
            End If

            newList.Add(newText)
        Next

        Return newList
    End Function

    Private Function CleanObjectList(ByVal Objects As Object()) As Object()

        Dim newObjList(Objects.Length()) As Object

        Dim cnt As Integer = 0
        For Each entry In Objects

            If Utility.IsStr(entry) = True Then
                entry = Split(entry)
            End If
            If Utility.IsNumber(entry) = True Then
                entry = Split(Utility.ToStr(entry))
            End If

            newObjList(cnt) = entry
            cnt += 1
        Next

        Return newObjList
    End Function


    Public Function Search(ByVal find As Object(), ByVal skips As Integer(), ByVal eNLPC As NLPC) As Boolean
        'eComparer SEN and POS will only search whether the 'find variable' is located in the Sentence
        'eComparer CAT and POSL will do a parallel search
        'This is used to search categories like if you were comparing two arrays
        'each word can have multiple categories, match a search category for each sentence word
        'skip words you do not want to search categories for
        'Search({T({"cat"}), T({"bird"}), Nothing}, {0, 0, 1})
        'It also finds the StartIndex() when it discovers the 'find' location

        Dim newList = ToRootSynonyms(ToListOfArray(find))
        StartIndex() = 0

        If eNLPC = NLPC.SEN Then
            StartIndex() = Utility.InArray(SEN(), newList, skips, IgnoreCase(), Gate.eEQUALS)
            If StartIndex() > -1 Then
                Return True
            Else
                Return False
            End If
        ElseIf eNLPC = NLPC.POS Then
            StartIndex() = Utility.InArray(POS(), newList, skips, IgnoreCase(), Gate.eEQUALS)
            If StartIndex() > -1 Then
                Return True
            Else
                Return False
            End If
        ElseIf eNLPC = NLPC.CAT Then
            Return Search(CAT(), newList, skips, StartIndex())
        ElseIf eNLPC = NLPC.POSL Then
            Return Search(POSL(), newList, skips, StartIndex())
        Else
            Return False
        End If

    End Function
    Public Function Search(ByVal find As Object(), ByVal skips As Integer(), ByVal limit As Integer, ByVal eNLPC As NLPC) As Boolean
        'eComparer SEN and POS will only search whether the 'find variable' is located in the Sentence
        'eComparer CAT and POSL will do a parallel search
        'This is used to search categories like if you were comparing two arrays
        'each word can have multiple categories, match a search category for each sentence word
        'skip words you do not want to search categories for
        'Search({T({"cat"}), T({"bird"}), Nothing}, {0, 0, 1})
        'It also finds the StartIndex() when it discovers the 'find' location

        If Count() <> limit Then
            Return False
        End If

        Return Search(find, skips, eNLPC)
    End Function

    Private Property StartIndex() As Integer = 0

    Public Function SearchIndex(ByVal index As Integer) As String()
        'This returns the word/compound at the relative position in the sentence
        If StartIndex() = -1 Then StartIndex() = 0
        Return SEN(StartIndex() + index)
    End Function
    Public Function SearchIndexJoined(ByVal index As Integer) As String
        'This returns the word/compound at the relative position in the sentence
        If StartIndex() = -1 Then StartIndex() = 0
        Return Join(SEN(StartIndex() + index))
    End Function

    Public Function Compare(ByVal find As Object(), ByVal skips As Integer(), ByVal eNLPC As NLPC) As Boolean

        'This directly compares an input to (the compound sentence) SEN() or POS() 
        Dim newList = ToRootSynonyms(ToListOfArray(find))

        If eNLPC = NLPC.SEN Then
            Return Utility.Compare(SEN(), newList, skips, IgnoreCase(), Gate.eEQUALS)
        ElseIf eNLPC = NLPC.POS Then
            Return Utility.Compare(POS(), newList, skips, IgnoreCase(), Gate.eEQUALS)
        Else
            Return False
        End If
    End Function
    Public Function Compare(ByVal find As Object(), ByVal skips As Integer(), ByVal limit As Integer, ByVal eNLPC As NLPC) As Boolean

        'This directly compares an input to (the compound sentence) SEN() or POS() 
        If Count() <> limit Then
            Return False
        End If

        Return Compare(find, skips, eNLPC)
    End Function

    Public Function Find(ByVal findarg As Object(), ByVal skips As Integer(), ByVal eNLPC As NLPC) As Integer

        'This is used to search categories like if you were comparing two arrays
        'It is used to find the position of the 'findarg'
        'each word can have multiple categories, match a search category for each sentence word
        'skip words you do not want to search categories for
        'Find({T({"cat"}), T({"bird"}), Nothing}, {0, 0, 1})

        Dim newList = ToRootSynonyms(ToListOfArray(findarg))

        If eNLPC = NLPC.CAT Then
            Return Find(CAT(), newList, skips)
        ElseIf eNLPC = NLPC.POSL Then
            Return Find(POSL(), newList, skips)
        Else
            Return -1
        End If

    End Function

    Public Function SearchAll(ByVal category As String(), ByVal eNLPC As NLPC) As Boolean
        'searches entire list for any category, sentence word, or Part of Speech
        'ie. if we search a real sentence for the category of 'bird', and one of the compound words in that sentence is 'blue jay'
        'then this function will return true for finding the category of bird

        category = GetRandomParent(category, NLPC.SYN)

        If eNLPC = NLPC.SEN Then
            Return Search(SEN(), category)
        ElseIf eNLPC = NLPC.POS Then
            Return Search(POS(), category)
        ElseIf eNLPC = NLPC.CAT Then
            Return Search(CAT(), category)
        Else
            Return False
        End If
    End Function
    Public Function SearchAll(ByVal category As String, ByVal eNLPC As NLPC) As Boolean
        Return SearchAll(Split(category), eNLPC)
    End Function

    Public Sub Update(ByVal compared As Object(), ByVal skips As Integer(), ByVal eNLPC As NLPC)
        'Only POS is updated
        'The POS must be set externally, otherwise the POS variable remains empty
        'The Update Sub finds the match (skips are included) (this means that it searches for POS with the skip-wildcards)
        'if it finds it, then it will replace it fully with the full match
        Dim newList = ToRootSynonyms(ToListOfArray(compared))

        If eNLPC = NLPC.POS Then
            Update(POS(), newList, skips)
        End If
    End Sub
    Public Sub Update(ByVal compared As Object(), ByVal skips As Integer(), ByVal limit As Integer, ByVal eNLPC As NLPC)
        'Only POS is updated
        'The POS must be set externally, otherwise the POS variable remains empty
        'The Update Sub finds the match (skips are included) (this means that it searches for POS with the skip-wildcards)
        'if it finds it, then it will replace it fully with the full match
        If Count() <> limit Then
            Exit Sub
        End If

        Update(compared, skips, eNLPC)
    End Sub

    Public Function GetWordFromSentence(ByVal category As String(), ByVal eNLPC As NLPC) As String()
        'returns the SEN(i) (sentence compound word) associated with the (POS or CAT) category we are searching for
        'it returns the first category matched 
        'does not search SEN()

        If eNLPC = NLPC.POS Then
            Return GetWord(POS(), category)
        ElseIf eNLPC = NLPC.CAT Then
            'Search CAT for cat (ie. bird) and returns sentence word
            Return GetWord(CAT(), category)
        Else
            Return {}
        End If
    End Function
    Public Function GetWordFromSentence(ByVal category As String, ByVal eNLPC As NLPC) As String
        Return Join(GetWordFromSentence(Split(category), eNLPC))
    End Function

    Public Sub SetEntry(ByVal relationship As String(), ByVal category As String(), ByVal entry As String())
        General.Create(relationship, category, entry, True, Gate.eEQUALS)
    End Sub

    Public Sub SetEntry(ByVal category As String(), ByVal entry As String(), ByVal eNLPC As NLPC)
        Dim relationship() As String = {}

        If eNLPC = NLPC.SYN Then
            relationship = DEF_SYN
        ElseIf eNLPC = NLPC.POS Then
            relationship = DEF_POS
        ElseIf eNLPC = NLPC.CAT Then
            relationship = DEF_CAT
        ElseIf eNLPC = NLPC.COM Then
            relationship = DEF_COMP
        End If

        General.Create(relationship, category, entry, True, Gate.eEQUALS)
    End Sub
    Public Sub SetEntry(ByVal category As String, ByVal entry As String, ByVal eNLPC As NLPC)
        SetEntry(Split(category), Split(entry), eNLPC)
    End Sub

    Public Function GetRandomParent(ByVal word As String(), ByVal eNLPC As NLPC) As String()
        'gets the category
        'ask for 'bird', retuns 'noun'
        Dim Def_List() As Object = {}

        If eNLPC = NLPC.SYN Then
            Def_List = SYNDef()
        ElseIf eNLPC = NLPC.POS Then
            Def_List = POSDef()
        ElseIf eNLPC = NLPC.CAT Then
            Def_List = CATDef()
        End If

        For Each gen In Def_List
            Dim out = General.Find(gen, Nothing, word, IgnoreCase(), Gate.eEQUALS)
            If out.Count() > 0 Then
                Return Utility.RandArray(out, {})
            End If
        Next

        Return word
    End Function
    Public Function GetRandomParent(ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Join(GetRandomParent(Split(word), eNLPC))
    End Function

    Public Function GetRandomSibling(ByVal word As String(), ByVal eNLPC As NLPC) As String()

        'two words found in category
        'ask for 'ball', returns 'cheese pizza'
        'ask for 'run', returns 'dance'

        Dim newGEN = GetRandomParent(word, eNLPC)
        Dim Def_List() As Object = {}

        If eNLPC = NLPC.SYN Then
            Def_List = SYNDef()
        ElseIf eNLPC = NLPC.POS Then
            Def_List = POSDef()
        ElseIf eNLPC = NLPC.CAT Then
            Def_List = CATDef()
        End If

        For Each gen In Def_List
            Dim out = General.Find(gen, newGEN, Nothing, IgnoreCase(), Gate.eEQUALS)
            If out.Count() > 0 Then
                Return Utility.RandArray(out, word)
            End If
        Next

        Return word
    End Function
    Public Function GetRandomSibling(ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Join(GetRandomSibling(Split(word), eNLPC))
    End Function

    Public Function GetRandomSibling(ByVal category As String(), ByVal word As String(), ByVal eNLPC As NLPC) As String()

        'two words found in category
        'ask for 'ball', returns 'cheese pizza'
        'ask for 'run', returns 'dance'

        Dim Def_List() As Object = {}

        If eNLPC = NLPC.SYN Then
            Def_List = SYNDef()
        ElseIf eNLPC = NLPC.POS Then
            Def_List = POSDef()
        ElseIf eNLPC = NLPC.CAT Then
            Def_List = CATDef()
        End If

        For Each gen In Def_List
            Dim out = General.Find(gen, category, Nothing, IgnoreCase(), Gate.eEQUALS)
            If out.Count() > 0 Then
                Return Utility.RandArray(out, word)
            End If
        Next

        Return word
    End Function
    Public Function GetRandomSibling(ByVal category As String, ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Join(GetRandomSibling(Split(category), Split(word), eNLPC))
    End Function

    Public Function GetRandomChild(ByVal category As String(), ByVal eNLPC As NLPC) As String()
        'gets a random word from a category
        'ask for a 'noun', returns 'ball'

        Dim Def_List() As Object = {}

        If eNLPC = NLPC.SYN Then
            Def_List = SYNDef()
        ElseIf eNLPC = NLPC.POS Then
            Def_List = POSDef()
        ElseIf eNLPC = NLPC.CAT Then
            Def_List = CATDef()
        End If

        For Each gen In Def_List
            Dim out = General.Find(gen, category, Nothing, IgnoreCase(), Gate.eEQUALS)
            If out.Count() > 0 Then
                Return Utility.RandArray(out, {})
            End If
        Next

        Return category
    End Function
    Public Function GetRandomChild(ByVal category As String, ByVal eNLPC As NLPC) As String
        Return Join(GetRandomChild(Split(category), eNLPC))
    End Function

    Public Function DoesRelationExist(ByVal category As String(), ByVal entry As String(), ByVal eNLPC As NLPC) As Boolean
        'gets the category
        'ask for 'bird', retuns 'noun'
        Dim Def_List() As Object = {}

        If eNLPC = NLPC.SYN Then
            Def_List = SYNDef()
        ElseIf eNLPC = NLPC.POS Then
            Def_List = POSDef()
        ElseIf eNLPC = NLPC.CAT Then
            Def_List = CATDef()
        End If

        For Each gen In Def_List
            Dim out = General.DoesRelationExist(gen, category, entry, IgnoreCase(), Gate.eEQUALS)
            If out = True Then Return True
        Next

        Return False
    End Function
    Public Function DoesRelationExist(ByVal category As String, ByVal entry As String, ByVal eNLPC As NLPC) As Boolean
        Return DoesRelationExist(Split(category), Split(entry), eNLPC)
    End Function

    Public Function DoesRelationExist(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal eGate As Gate) As Boolean
        Return General.DoesRelationExist(relationship, category, entry, True, Gate.eEQUALS)
    End Function
    Public Function DoesRelationExist(ByVal relationship As String, ByVal category As String, ByVal entry As String, ByVal eGate As Gate) As Boolean
        Return General.DoesRelationExist(Split(relationship), Split(category), Split(entry), True, Gate.eEQUALS)
    End Function

#End Region


#Region "PRIVATE"

    Private Function setTags(ByVal sentence As String) As String
        sentence = Utility.Replace(sentence, DEF_DASH(0), DEF_DASH(1))
        sentence = Utility.Replace(sentence, DEF_COMMA(0), DEF_COMMA(1))
        sentence = Utility.Replace(sentence, DEF_DQUOT(0), DEF_DQUOT(1))
        sentence = Utility.Replace(sentence, DEF_SQUOT(0), DEF_SQUOT(1))
        sentence = Utility.Replace(sentence, DEF_OPAR(0), DEF_OPAR(1))
        sentence = Utility.Replace(sentence, DEF_CPAR(0), DEF_CPAR(1))
        Return sentence
    End Function
    Private Function removeTags(ByVal sentence As String) As String
        sentence = Utility.Replace(sentence, DEF_DASH(1), DEF_DASH(0))
        sentence = Utility.Replace(sentence, DEF_COMMA(1), DEF_COMMA(0))
        sentence = Utility.Replace(sentence, DEF_DQUOT(1), DEF_DQUOT(0))
        sentence = Utility.Replace(sentence, DEF_SQUOT(1), DEF_SQUOT(0))
        sentence = Utility.Replace(sentence, DEF_OPAR(1), DEF_OPAR(0))
        sentence = Utility.Replace(sentence, DEF_CPAR(1), DEF_CPAR(0))
        Return sentence
    End Function


    Private Function Search(ByVal list As List(Of List(Of String())), ByVal find As String()) As Boolean
        'Search All
        Utility.IsNull(find)
        Utility.IsNull(list)

        For Each line In list
            If Search(line, find) = True Then
                Return True
            End If
        Next

        Return False
    End Function
    Private Function Search(ByVal list As List(Of String()), ByVal find As String()) As Boolean
        'Search All

        Utility.IsNull(list)
        Utility.IsNull(find)

        For Each line In list
            If Utility.Compare(line, find, IgnoreCase(), Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Private Function Search(ByVal list As List(Of List(Of String())), ByVal match As List(Of String()), ByVal skips As Integer(), ByRef refstart As Integer) As Boolean
        refstart = Find(list, match, skips)
        If refstart = -1 Then
            Return False
        Else
            Return True
        End If
    End Function
    Private Function Search(ByVal list As List(Of List(Of String())), ByVal match As List(Of String()), ByVal skips As Integer()) As Boolean
        If Find(list, match, skips) = -1 Then
            Return False
        Else
            Return True
        End If
    End Function
    Private Function Search(ByVal text As String(), ByVal find As List(Of String())) As Boolean
        Utility.IsNull(text)
        Utility.IsNull(find)

        If text.Length() = 0 And find.Count() = 0 Then Return True

        For Each line In find
            If Utility.Compare(text, line, IgnoreCase(), Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Private Function Find(ByVal list As List(Of List(Of String())), ByVal match As List(Of String()), ByVal skips As Integer()) As Integer
        Utility.IsNull(list)
        Utility.IsNull(match)
        Utility.IsNull(skips)

        If list.Count() < match.Count() Then Return -1
        If list.Count() = 0 And match.Count() = 0 Then Return 0
        If match.Count() = 0 Then Return -1

        Dim iCount As Integer = 0
        Dim i As Integer = 0

        Do Until i >= list.Count()

            If Utility.GetIndex(skips, iCount, 0) = 1 Then
                iCount += 1
                i += 1
                Continue Do
            End If

            If Search(match(iCount), list(i)) = False Then
                i -= iCount
                iCount = 0
            Else
                iCount += 1

                If iCount >= match.Count Then
                    Return i - (iCount - 1)
                End If
            End If

            i += 1
        Loop

        Return -1
    End Function
    Private Function Find(ByVal list As List(Of String()), ByVal match As List(Of String()), ByVal skips As Integer()) As Integer
        Return Utility.InArray(list, match, skips, IgnoreCase(), Gate.eEQUALS)
    End Function

    Private Sub Update(ByRef list As List(Of String()), ByVal match As List(Of String()), ByVal skips As Integer())
        'This finds an array match in an array, skips over all entries mark with 1, 
        'and then replaces that skipped match with the whole match

        Dim start = Find(list, match, skips)
        If start = -1 Then start = 0

        For i = 0 To match.Count() - 1
            list = Utility.SetIndex(list, i + start, match(i))
        Next
    End Sub

    Private Function GetWord(ByVal list As List(Of String()), ByVal category As String()) As String()
        Utility.IsNull(list)
        Utility.IsNull(category)

        For i = 0 To list.Count - 1
            If Utility.Compare(list(i), category, IgnoreCase(), Gate.eEQUALS) = True Then
                Return SEN(i)
            End If
        Next

        Return {}
    End Function
    Private Function GetWord(ByVal list As List(Of List(Of String())), ByVal category As String()) As String()
        Utility.IsNull(list)
        Utility.IsNull(category)

        For i = 0 To list.Count - 1
            If Search(list(i), category) = True Then
                Return SEN(i)
            End If
        Next

        Return {}
    End Function

#End Region


End Class

Public Class Relations

    'RELATIONSHIP of NAME is ENTRY
    'The POS of ROCK is NOUN
    'A PROPERTY of THING is SIZE

    Public Property Items() As New List(Of Relation)
    Public ReadOnly Property ListOnly() As String() = {"|ListOnly|"}

    Private Const SPACE As String = "_"
    Private Const DELIM_1 As String = "|"
    Private Const DELIM_2 As String = vbCrLf

    Private Const ADDRESS_L1 As String = "relations"
    Private Const CATEGORY As String = "category"
    Private Const ENTRY As String = "entry"

    '<relations>
    ''''<"relationship">
    '''''''<SECG43>
    ''''''''''<category></category>
    ''''''''''<entry></entry>
    '''''''</SECG43>
    ''''</"relationship">
    '</relations>

    'ZIP file save
    'file name (is the) relationship_name.io
    'category_category::entry_entry

    'Note: When loading, if there is no category, only a relationship and entries, then the category name will be the name of the relationship

#Region "Private"

    Private Function Find_Entries_ByCategory(ByVal relationship As String(), ByVal category As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then

                For Each rel1 In rel.Relations
                    If Utility.Compare(rel1.Category(), category, ignoreCase, eGate) = True Then
                        newArray.Add(rel1.Entry())
                    End If
                Next

            End If
        Next

        Return newArray
    End Function

    Private Function Find_Entries_ByRelationship(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                For Each rel1 In rel.Relations
                    newArray.Add(rel1.Entry())
                Next
            End If
        Next

        Return newArray
    End Function

    Private Function Find_Categories_ByEntry(ByVal relationship() As String, ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then

                For Each rel1 In rel.Relations
                    If Utility.Compare(rel1.Entry(), entry, ignoreCase, eGate) = True Then
                        newArray.Add(rel1.Category())
                    End If
                Next

            End If
        Next

        Return newArray
    End Function

    Private Function Find_Categories_ByRelationship(ByVal relationship() As String, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then

                For Each rel1 In rel.Relations
                    newArray.Add(rel1.Category())
                Next
            End If
        Next

        Return newArray
    End Function


    Private Function Find_Relationships_ByRelate(ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If rel.DoesRelationExist(category, entry, ignoreCase, eGate) = True Then
                newArray.Add(rel.Relationship())
                Continue For
            End If
        Next

        Return newArray
    End Function

    'Not used
    Private Function Find_Relationships_ByRelate(ByVal relate As Relation.Relate, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Dim newArray As New List(Of String())

        For Each rel In Items
            If rel.DoesRelationExist(relate, ignoreCase, eGate) = True Then
                newArray.Add(rel.Relationship())
                Continue For
            End If
        Next

        Return newArray
    End Function


    Private Sub Add_Relate_toRelation(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        'This adds new relate to a relation by the relationship
        Dim newList As New List(Of Relation)

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                rel.Relations.Add(New Relation.Relate(category, entry))
            End If

            newList.Add(rel)
        Next

        Items = newList
    End Sub

    Private Sub Add_Relate_toRelation(ByVal relationship As String(), ByVal relations As List(Of Relation.Relate), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        'This adds new relate to a relation by the relationship
        Dim newList As New List(Of Relation)

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                rel.Relations() = relations
            End If

            newList.Add(rel)
        Next

        Items = newList
    End Sub

#End Region


#Region "Main"

    Public Function SaveToXML(ByVal file As String) As String
        Dim newXML As New XmlSource

        For Each Relate In Items
            For Each Relation_1 In Relate.Relations()
                Dim UNIQUEID = Utility.RandKey(8)
                newXML.Create({ADDRESS_L1, Utility.Join(Relate.Relationship(), SPACE), UNIQUEID, CATEGORY}, Utility.Join(Relation_1.Category(), SPACE))
                newXML.Create({ADDRESS_L1, Utility.Join(Relate.Relationship(), SPACE), UNIQUEID, ENTRY}, Utility.Join(Relation_1.Entry(), SPACE))
            Next
        Next

        newXML.Save(file)
        Return newXML.Document()
    End Function

    Public Sub LoadfromXML(ByVal file As String)
        Dim newXML As New XmlSource
        newXML.Loadfile(file)

        Dim allrelations = newXML.Children({ADDRESS_L1})

        Dim namesList As New List(Of String)
        For Each rel In allrelations
            If Utility.Search(namesList, rel.Index(1), False, Gate.eEQUALS) = False Then
                namesList.Add(rel.Index(1))
            End If
        Next

        For Each relationship In namesList
            Dim relations = newXML.Children({ADDRESS_L1, relationship})

            Dim idList As New List(Of String)
            For Each rel In relations
                If Utility.Search(namesList, rel.Index(2), False, Gate.eEQUALS) = False Then
                    idList.Add(rel.Index(2))
                End If
            Next

            Dim newList As New List(Of Relation.Relate)

            For Each id In idList
                Dim CAT = newXML.InnerText({ADDRESS_L1, relationship, id, CATEGORY})
                Dim ENT = newXML.InnerText({ADDRESS_L1, relationship, id, ENTRY})

                newList.Add(New Relation.Relate(Utility.ToArray(CAT, SPACE), Utility.ToArray(ENT, SPACE)))
            Next

            Items.Add(New Relation(Utility.ToArray(relationship, SPACE), newList))
        Next

    End Sub

    Public Sub Save(ByVal location As String, ByVal relationship As String())

        For Each Relate In Items

            If Utility.Compare(relationship, Relate.Relationship(), True, Gate.eEQUALS) = True Then
                Dim TextToWrite As String = ""

                For Each Relation_1 In Relate.Relations()
                    If Utility.Compare(Relation_1.Category(), Relate.Relationship(), True, Gate.eEQUALS) = False Then
                        TextToWrite &= Utility.Join(Relation_1.Category(), SPACE) + DELIM_1 + Utility.Join(Relation_1.Entry(), SPACE) + DELIM_2
                    Else
                        TextToWrite &= Utility.Join(Relation_1.Entry(), SPACE) + DELIM_2
                    End If
                Next

                Utility.ZipWriter(location, Utility.Join(Relate.Relationship(), SPACE) & Ext.IO, TextToWrite)
                Exit Sub
            End If
        Next
    End Sub

    Public Sub Load(ByVal location As String, ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        Remove(relationship, ignoreCase, eGate) 'clears memory from double entries

        Dim entry = Utility.ZipReader(location, Utility.Join(relationship, SPACE) & Ext.IO)

        Dim newList As New List(Of Relation.Relate)
        Dim pairArray = Utility.ToList(entry, DELIM_2)

        For Each pair1 In pairArray
            Dim twoArray = Utility.ToList(pair1, DELIM_1)
            If twoArray.Count() = 2 Then
                newList.Add(New Relation.Relate(Utility.ToArray(Utility.GetIndex(twoArray, 0, ""), SPACE), Utility.ToArray(Utility.GetIndex(twoArray, 1, ""), SPACE)))
            ElseIf twoArray.Count() = 1 Then
                newList.Add(New Relation.Relate(relationship, Utility.ToArray(Utility.GetIndex(twoArray, 0, ""), SPACE)))
            End If
        Next

        Items.Add(New Relation(relationship, newList))

    End Sub

    Public Sub SaveAll(ByVal location As String)

        For Each Relate In Items
            Dim TextToWrite As String = ""

            For Each Relation_1 In Relate.Relations()
                If Utility.Compare(Relation_1.Category(), Relate.Relationship(), True, Gate.eEQUALS) = False Then
                    TextToWrite &= Utility.Join(Relation_1.Category(), SPACE) + DELIM_1 + Utility.Join(Relation_1.Entry(), SPACE) + DELIM_2
                Else
                    TextToWrite &= Utility.Join(Relation_1.Entry(), SPACE) + DELIM_2
                End If
            Next

            Utility.ZipWriter(location, Utility.Join(Relate.Relationship(), SPACE) & Ext.IO, TextToWrite)
        Next

    End Sub

    Public Sub LoadAll(ByVal location As String)

        If Utility.FileExists(location) = False Then Exit Sub
        Clear() 'clears memory from double entries

        'Try
        Using zipStream As New IO.FileStream(location, IO.FileMode.Open)
            Using archive As New IO.Compression.ZipArchive(zipStream, IO.Compression.ZipArchiveMode.Read)

                For Each entries In archive.Entries
                    Dim name = entries.Name()
                    name = Utility.Remove(name, Ext.IO)

                    If name.Length() > 0 Then

                        Dim text = ""
                        Using Reader As New IO.StreamReader(entries.Open())
                            text = Reader.ReadToEnd()
                        End Using

                        Dim newList As New List(Of Relation.Relate)
                        Dim pairsArray = Utility.ToList(text, DELIM_2)

                        For Each pair1 In pairsArray
                            Dim twoArray = Utility.ToList(pair1, DELIM_1)
                            If twoArray.Count() = 2 Then
                                newList.Add(New Relation.Relate(Utility.ToArray(Utility.GetIndex(twoArray, 0, ""), SPACE), Utility.ToArray(Utility.GetIndex(twoArray, 1, ""), SPACE)))
                            ElseIf twoArray.Count() = 1 Then
                                newList.Add(New Relation.Relate(Utility.ToArray(name, SPACE), Utility.ToArray(Utility.GetIndex(twoArray, 0, ""), SPACE)))
                            End If
                        Next

                        Items.Add(New Relation(Utility.ToArray(name, SPACE), newList))

                    End If

                Next

            End Using
        End Using
        'Catch : End Try

    End Sub



    Public Function Print() As String
        Dim text As String = ""

        For Each Relate In Items
            Dim TextToWrite As String = ""

            For Each Relation_1 In Relate.Relations()
                TextToWrite &= Utility.Join(Relation_1.Category(), SPACE) & DELIM_1 & Utility.Join(Relation_1.Entry(), SPACE) & DELIM_2
            Next

            text &= Utility.Join(Relate.Relationship(), SPACE) & DELIM_2 & TextToWrite & DELIM_2
        Next

        Return text
    End Function

    Public Sub Clear()
        Items.Clear()
    End Sub

#End Region


#Region "Body"

    Public Function DoesRelationExist(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function DoesRelationExist(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                If rel.DoesRelationExist(category, entry, ignoreCase, eGate) = True Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesRelationExist(ByVal relationship As String(), ByVal relations As List(Of Relation.Relate), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                If rel.DoesRelationExist(relations, ignoreCase, eGate) = True Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function

    Public Function DoesRelationExist(ByVal relation As Relation, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

        For Each rels In Items
            If Utility.Compare(rels.Relationship(), relation.Relationship(), ignoreCase, eGate) = True Then

                For Each rel In relation.Relations
                    If rels.DoesRelationExist(rel, ignoreCase, eGate) = True Then
                        Return True
                    End If
                Next

            End If
        Next

        Return False
    End Function


    Public Sub Create(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        If DoesRelationExist(relationship, ignoreCase, eGate) = False Then
            Items.Add(New Relation(relationship, category, entry))
        Else
            If DoesRelationExist(relationship, category, entry, ignoreCase, eGate) = False Then
                Add_Relate_toRelation(relationship, category, entry, ignoreCase, eGate)
            End If
        End If
    End Sub

    Public Sub Create(ByVal relationship As String(), ByVal relations As List(Of Relation.Relate), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        If DoesRelationExist(relationship, ignoreCase, eGate) = False Then
            Items.Add(New Relation(relationship, relations))
        Else
            If DoesRelationExist(relationship, relations, ignoreCase, eGate) = False Then
                Add_Relate_toRelation(relationship, relations, ignoreCase, eGate)
            End If
        End If
    End Sub


    Public Sub Remove(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        Dim newList As New List(Of Relation)

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = False Then
                newList.Add(rel)
            End If
        Next

        Items = newList
    End Sub

    Public Sub Remove(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
        Dim newList As New List(Of Relation)

        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                rel.Remove(category, entry, ignoreCase, eGate)
                newList.Add(rel)
            End If
        Next

        Items = newList
    End Sub

    Public Function GetCategories(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Return Find_Categories_ByRelationship(relationship, ignoreCase, eGate)
    End Function
    Public Function GetEntries(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())
        Return Find_Entries_ByRelationship(relationship, ignoreCase, eGate)
    End Function

    Public Function Find(ByVal relationship As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of Relation)
        Dim Relations As New List(Of Relation)
        For Each rel In Items
            If Utility.Compare(rel.Relationship(), relationship, ignoreCase, eGate) = True Then
                Relations.Add(rel)
            End If
        Next

        Return Relations
    End Function

    Public Function Find(ByVal relationship As String(), ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As List(Of String())

        If IsNothing(relationship) = True And IsNothing(category) = False And IsNothing(entry) = False Then
            'This returns the relationship
            Return Find_Relationships_ByRelate(category, entry, ignoreCase, eGate)
        End If

        If IsNothing(relationship) = False And IsNothing(category) = False And IsNothing(entry) = True Then
            'This returns the entry
            Return Find_Entries_ByCategory(relationship, category, ignoreCase, eGate)
        End If

        If IsNothing(relationship) = False And IsNothing(category) = True And IsNothing(entry) = False Then
            'This returns the name
            Return Find_Categories_ByEntry(relationship, entry, ignoreCase, eGate)
        End If

        Return New List(Of String())
    End Function


#End Region


    Public Class Relation
        Public Sub New()
        End Sub
        Public Sub New(ByVal relationship As String(), ByVal relations As List(Of Relate))
            Me.Relationship() = relationship
            Me.Relations() = relations
        End Sub
        Public Sub New(ByVal relationship As String(), ByVal category As String(), ByVal entry As String())
            Me.Relationship() = relationship
            Add(category, entry)
        End Sub

        Public Function DoesRelationExist(ByVal relate As Relate, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

            For Each rel In Relations
                If Utility.Compare(rel.Category(), relate.Category(), ignoreCase, eGate) = True Then
                    If Utility.Compare(rel.Entry(), relate.Entry(), ignoreCase, eGate) = True Then
                        Return True
                    End If
                End If
            Next

            Return False
        End Function
        Public Function DoesRelationExist(ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

            For Each rel In Relations
                If Utility.Compare(rel.Category(), category, ignoreCase, eGate) = True Then
                    If Utility.Compare(rel.Entry(), entry, ignoreCase, eGate) = True Then
                        Return True
                    End If
                End If
            Next

            Return False
        End Function

        Public Function DoesRelationExist(ByVal relations As List(Of Relate), ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Boolean

            If Me.Relations.Count() = relations.Count() Then
                For Each rel In relations
                    If Count(Me.Relations, rel, ignoreCase, eGate) <> Count(relations, rel, ignoreCase, eGate) Then
                        Return False
                    End If
                Next
            Else
                Return False
            End If

            Return True
        End Function

        Public Function Count(ByVal relations As List(Of Relate), ByVal relate As Relate, ByVal ignoreCase As Boolean, ByVal eGate As Gate) As Integer
            Dim Count1 As Integer = 0

            For Each rel In relations
                If Utility.Compare(rel.Category(), relate.Category(), ignoreCase, eGate) = True Then
                    If Utility.Compare(rel.Entry, relate.Entry, ignoreCase, eGate) = True Then
                        Count1 = Count1 + 1
                    End If
                End If
            Next

            Return Count1
        End Function

        Public Sub Remove(ByVal relate As Relate, ByVal ignoreCase As Boolean, ByVal eGate As Gate)
            Dim newList As New List(Of Relate)

            For Each rel In Me.Relations()
                If Utility.Compare(rel.Category(), relate.Category(), ignoreCase, eGate) = True Then
                    If Utility.Compare(rel.Entry(), relate.Entry(), ignoreCase, eGate) = True Then
                        Continue For
                    End If
                End If

                newList.Add(rel)
            Next

            Me.Relations() = newList
        End Sub
        Public Sub Remove(ByVal category As String(), ByVal entry As String(), ByVal ignoreCase As Boolean, ByVal eGate As Gate)
            Dim newList As New List(Of Relate)

            For Each rel In Me.Relations()
                If Utility.Compare(rel.Category(), category, ignoreCase, eGate) = True Then
                    If Utility.Compare(rel.Entry(), entry, ignoreCase, eGate) = True Then
                        Continue For
                    End If
                End If

                newList.Add(rel)
            Next

            Me.Relations() = newList
        End Sub

        Public Sub Add(ByVal relate As Relate)
            Me.Relations().Add(relate)
        End Sub
        Public Sub Add(ByVal category As String(), ByVal entry As String())
            Me.Relations().Add(New Relate(category, entry))
        End Sub

        Private m_relationship As String() = {""}
        Public Property Relationship() As String()
            Get
                Return m_relationship
            End Get
            Set(value As String())
                Utility.IsNull(value)
                m_relationship = value
            End Set
        End Property

        Public Property Relations As List(Of Relate)


        Public Class Relate
            Public Sub New()
            End Sub

            Public Sub New(ByVal category As String(), ByVal entry As String())
                Utility.IsNull(category)
                Utility.IsNull(entry)
                Me.Category() = category
                Me.Entry() = entry
            End Sub

            Public Property Category() As String() = {""}
            Public Property Entry() As String() = {""}
        End Class

    End Class



End Class

Public Enum NLPC
    POS
    POSL
    SEN
    CAT
    SYN
    COM
End Enum

Public Enum OCompare
    Greater
    Lesser
    Equals
    GreaterEquals
    LesserEquals
    Range
End Enum

Public Class POS
    'PofS categories
    Public Shared ReadOnly Property PRO As String() = {"pronoun"}
    Public Shared ReadOnly Property NOUN As String() = {"noun"}
    Public Shared ReadOnly Property VERB As String() = {"verb"}
    Public Shared ReadOnly Property ADV As String() = {"adverb"}
    Public Shared ReadOnly Property ART As String() = {"article"}
    Public Shared ReadOnly Property AUX As String() = {"auxillery"}
    Public Shared ReadOnly Property CONJ As String() = {"conjunction"}
    Public Shared ReadOnly Property PREP As String() = {"preposition"}
    Public Shared ReadOnly Property ADJ As String() = {"adjective"}
    Public Shared ReadOnly Property INTR As String() = {"interjection"}
End Class

Public Class SST
    'Sentence Struction Categories
    Public Shared ReadOnly Property ACT As String() = {"action"}
    Public Shared ReadOnly Property PAT As String() = {"patient"}
    Public Shared ReadOnly Property REC As String() = {"recipient"}
    Public Shared ReadOnly Property AGT As String() = {"agent"}
    Public Shared ReadOnly Property INS As String() = {"instrument"}

    'location
    'direction
    'manor
    'exemption

    's(pat(bill) act(throw(time(past) manor(slow) dir(house(owner(agt))) loc(barn))) pat(ball(color(red))) rec(fred(height(tall))) ins(cannon(weight(heavy)))
    'While at the barn, Bill slowly threw a red ball at tall Fred using a heavy cannon in the direction of Bill's house.

End Class

Public Class Syntax

    'sen(agt(i) act(run(speed(quick(opt(very))), emotion(happy)))

#Region "CONSTANTS"

    Public Const BEG_CHAR As Char = "("
    Public Const END_CHAR As Char = ")"
    Public Const GROUND_CHAR As Char = "_"
    Public Const COMMA_CHAR As Char = ","
    Public Const BKSH As Char = "\"
    Public Const SENTENCE_START As String = "s("

    Public SD As New Sentence
    Public Property SentenceList() As List(Of Sentence)

#End Region

#Region "Main"
    'processing VLL syntax
    Public Sub Parse(ByVal strSentence As String)
        'Parses one sentence

        Dim Beg_Char_Found = False
        Dim End_Char_Found = False
        Dim Ground_Char_Found = False
        Dim BuildString = ""
        Dim SaveEntry = False
        Dim Address As New List(Of String)
        Dim Entry As String = ""
        Dim Skip = False

        Dim allSenteces = Utility.ToList(strSentence, SENTENCE_START)

        For Each Sentence In allSenteces
            Dim newSentence As New Sentence

            Dim SenArray = Sentence.ToCharArray()
            For Each chr1 In SenArray
                Skip = False

                If chr1 = BEG_CHAR Then
                    Beg_Char_Found = True
                    Skip = True
                End If

                If chr1 = END_CHAR Then
                    End_Char_Found = True
                    Skip = True
                End If

                If chr1 = GROUND_CHAR Then
                    Ground_Char_Found = True
                    Skip = True
                End If

                If Skip = False Then
                    BuildString = BuildString + chr1
                End If

                If BuildString > "" Then

                    If Beg_Char_Found = True Then
                        BuildString = Utility.Remove(BuildString, COMMA_CHAR)
                        BuildString = Utility.Trim(BuildString)
                        Address.Add(BuildString)
                        Beg_Char_Found = False
                        BuildString = ""
                    End If

                    If End_Char_Found = True Or Ground_Char_Found = True Then
                        Entry = Utility.Trim(BuildString)
                        BuildString = ""
                        SaveEntry = True
                    End If

                End If

                If SaveEntry = True Then
                    Dim newProp As New Sentence.Segment
                    newProp.Address() = Address
                    newProp.Entry() = Entry

                    newSentence.Add(newProp)
                    Entry = ""
                    SaveEntry = False
                End If

                If End_Char_Found = True Then
                    Address = Utility.RemoveLast(Address)
                    End_Char_Found = False
                End If

                If Ground_Char_Found = True Then
                    Address.Clear()
                    Ground_Char_Found = False
                End If

            Next

            SentenceList.Add(newSentence)
        Next

    End Sub


    Private Function toAddress(ByVal address As String) As List(Of String)
        Return Utility.ToList(address, BKSH)
    End Function


    Public Function Find(ByVal sentence As String, ByVal segment1 As String) As String
        'returns an entry from sentence
        Dim Syn As New Syntax

        Dim address = toAddress(segment1)
        Syn.Parse(sentence)

        For Each sen In Syn.SentenceList()
            For Each Prop In sen.SegmentList()

                If Utility.Compare(Prop.Address, address, True, Gate.eEQUALS) = True Then
                    Return Prop.Entry()
                End If
            Next
        Next

        Return ""
    End Function


    Public Function Search(ByVal sentence As Sentence, ByVal args As String()) As Boolean
        'returns True if it find all the args in the sentence

        For Each arg In args
            If sentence.DoesSegmentExist(toAddress(arg)) = False Then
                Return False
            End If
        Next

        Return True
    End Function

    Public Function Find(ByVal args As String(), ByVal segment As String) As List(Of String)
        'returns an entry from the sentence list
        Dim Results As New List(Of String)
        Dim address = toAddress(segment)

        For Each sen In SentenceList()
            If Search(sen, args) = True Then

                'Returns the entry by a specific segment
                Dim entry = sen.FindEntry(address)
                If Utility.IsEmpty(entry) = False Then Results.Add(entry)
            End If
        Next

        Return Results
    End Function

    'UNFINISHED

    Public Function Search(ByVal senList As List(Of Sentence), ByVal args As String(), ByVal eGate As Gate) As List(Of Sentence)
        'returns a list of sentences that contain the arguments
        'can be used to refine searches by placing the results back into the senList
        Dim newList As New List(Of Sentence)

        Select Case eGate
            Case Gate.eAND

            Case Gate.eOR

        End Select





        Return newList
    End Function

    Public Function Search(ByVal args As String(), ByVal eGate As Gate) As List(Of Sentence)
        'returns a list of sentences that contain the arguments
        'can be used to refine searches by placing the results back into the senList
        Return New List(Of Sentence)
    End Function

    Public Function DoesSentenceExist() As Boolean

        Return False
    End Function



#End Region

    Public Class Sentence

        Public Property SegmentList() As List(Of Segment)
        Public Property Rating() As Integer

        Public Sub Add(ByVal segment As Segment)
            SegmentList.Add(segment)
        End Sub

        Public Function DoesSegmentExist(ByVal address As List(Of String)) As Boolean
            For Each seg In SegmentList()
                If Utility.Compare(seg.Address(), address, True, Gate.eEQUALS) = True Then
                    Return True
                End If
            Next
            Return False
        End Function

        Public Function DoesSegmentExist(ByVal segment As Segment) As Boolean
            For Each seg In SegmentList()
                If Utility.Compare(seg.Address(), segment.Address(), True, Gate.eEQUALS) = True Then
                    If Utility.Search(seg.Entry(), segment.Entry(), Gate.eEQUALS) = True Then
                        Return True
                    End If
                End If
            Next
            Return False
        End Function

        Public Function FindEntry(ByVal address As List(Of String)) As String
            For Each seg In SegmentList()
                If Utility.Compare(seg.Address(), address, True, Gate.eEQUALS) = True Then
                    Return seg.Entry()
                End If
            Next

            Return ""
        End Function


        Public Sub Clear()
            SegmentList.Clear()
        End Sub

        Public Class Segment
            Public Property Address() As New List(Of String)
            Public Property Entry() As String = ""
        End Class

    End Class

End Class




'TESTING CLASSES
Public Class VerbInterpreter

    Public Shared Property ItemList() As New List(Of Items)
    Public Shared Property DefList() As New List(Of String())

    Public Shared Sub Load()
        Dim newItem As New Items

        newItem.Name() = {"i"}
        newItem.CanDo() = {"shot", "fly"}
        newItem.CanBeDoneTo() = {"kick", "shot"}
        ItemList.Add(newItem)

        newItem = New Items
        newItem.Name() = {"dog"}
        newItem.CanDo() = {"bark", "run"}
        newItem.CanBeDoneTo() = {"kick", "shot"}
        ItemList.Add(newItem)


        DefList.Add({"*", "shot", "*", "*"})


    End Sub



    Public Shared Function Process(ByVal text As String()) As String

        For Each def_line In DefList

            If Utility.Search(text, def_line, True, Gate.eEQUALS) = True Then


                If def_line.Length = 4 Then

                    If SearchCanDo(text(0), text(1)) = True Then

                    Else
                        Return text(0) & " cannot " & text(1)
                    End If

                    If SearchCanBeDoneTo(text(3), text(1)) = True Then

                    Else

                        Return text(3) & " cannot be " & text(1)
                    End If


                    Return "This statement is logical."

                End If
            End If
        Next

        Return "false"
    End Function



    Public Shared Function SearchCanDo(ByVal name As String, ByVal cando As String) As Boolean
        For Each itm In ItemList()
            If Utility.Search(name, itm.Name(), Gate.eEQUALS) = True Then
                If Utility.Search(cando, itm.CanDo(), Gate.eEQUALS) = True Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function

    Public Shared Function SearchCanBeDoneTo(ByVal name As String, ByVal canbedone As String) As Boolean
        For Each itm In ItemList()
            If Utility.Search(name, itm.Name(), Gate.eEQUALS) = True Then
                If Utility.Search(canbedone, itm.CanBeDoneTo(), Gate.eEQUALS) = True Then
                    Return True
                End If
            End If
        Next

        Return False
    End Function

    Public Shared Function SearchItem(ByVal name As String) As Boolean
        For Each itm In ItemList
            If Utility.Search(name, itm.Name(), Gate.eEQUALS) = True Then
                Return True
            End If
        Next

        Return False
    End Function

    Public Function IsArt(ByVal text As String) As Boolean
        Return Utility.Search(text, Resource.Articles(), Gate.eEQUALS)
    End Function


    Public Class Items
        Public Property Name() As String() = {""}
        Public Property CanDo() As String() = {}
        Public Property CanBeDoneTo() As String() = {}
    End Class

    Public Class Resource
        Public Shared Property Articles() As String() = {"a", "an", "the"}
    End Class


End Class





'Author : Leroy S Dyer
'Year : 2016
Namespace NeuralNetworking
    ''' <summary>
    ''' This produces a three layered neural network object
    ''' </summary>
    ''' <remarks></remarks>
    Public Class ClassNeuralNetwork


        Public ArtificialNeuralNetwork As NeuralNetwork

        Public Sub New(ByRef nNetworkParameters As NeuralNetworkParameters)
            ArtificialNeuralNetwork = CreateNeuralNetwork(nNetworkParameters)
        End Sub

        ''' <summary>
        ''' Creates a Layer of the Neural Network 
        ''' </summary>
        ''' <param name="nLayerType">Input / Hidden / Output</param>
        ''' <param name="NumberOfNodes">Amount of Nodes in the layer</param>
        ''' <param name="ActivationFunct">Activation FunctionType</param>
        ''' <returns>Created Layer</returns>
        ''' <remarks>there are only three layers in this network</remarks>
        Private Function CreateLayer(ByRef nLayerType As LayerType,
                                ByRef NumberOfNodes As Integer,
                       ActivationFunct As TransferFunctionType) As Layer

            Dim nLayer As New Layer
            Dim Nodes As New List(Of Neuron)
            Dim Node As New Neuron

            'Create nodes
            For i = 1 To NumberOfNodes
                Nodes.Add(Node)
            Next
            nLayer.ActivationFunction = ActivationFunct
            nLayer.Nodes = Nodes
            nLayer.NumberOfNodes = NumberOfNodes
            nLayer.LayerType = nLayerType
            Return nLayer
        End Function

        ''' <summary>
        ''' creates neural network
        ''' can create a deep belief network or simple three layered network
        ''' </summary>
        ''' <param name="NetworkParameters">Parameters required to create the network</param>
        ''' <returns>created network</returns>
        ''' <remarks>requires training
        ''' generated neural network, number of nodes in hidden layers is calculated by this generation algorithm</remarks>
        Private Function CreateNeuralNetwork(ByRef NetworkParameters As NeuralNetworkParameters) As NeuralNetwork
            Dim NN As New NeuralNetwork
            Dim HiddenLayer As New Layer
            'Number of hidden nodes is calculated by the mean of the input and output nodes
            Dim NumberOfHiddenNodes As Integer = CalculateNumberOfHiddenNodes(NetworkParameters.NumberOfInputs, NetworkParameters.NumberOfOutputs)


            'Create InputLayer nodes
            NN.InputLayer = CreateLayer(LayerType.Input, NetworkParameters.NumberOfInputs, TransferFunctionType.none)
            For Each node As Neuron In NN.InputLayer.Nodes
                node.weight = CreateRandWeight(NetworkParameters.NumberOfInputs, NumberOfHiddenNodes)
            Next


            'Create hidden layers
            For i = 1 To NetworkParameters.NumberOfHiddenLayers
                'Create HiddenLayer nodes
                HiddenLayer = CreateLayer(LayerType.Hidden, NumberOfHiddenNodes, NetworkParameters.HiddenLayerFunctionType)
                For Each node As Neuron In HiddenLayer.Nodes
                    node.weight = CreateRandWeight(NetworkParameters.NumberOfInputs, NumberOfHiddenNodes)
                Next
                NN.HiddenLayers.Add(HiddenLayer)
            Next


            'Create OutputLayer nodes
            NN.OutputLayer = CreateLayer(LayerType.Output, NetworkParameters.NumberOfOutputs, NetworkParameters.OutputLayerFunctionType)



            'Return DeepBelief Network(neuralNetwork)
            Return NN
        End Function

        ''' <summary>
        ''' Network Transfer Functions
        ''' </summary>
        ''' <remarks></remarks>
        Public Class ClassTransferFunctions
            'Evaluate
            Public Shared Function EvaluateTransferFunct(ByRef TransferFunct As TransferFunctionType, ByRef Input As Double) As Integer
                EvaluateTransferFunct = 0
                Select Case TransferFunct
                    Case TransferFunctionType.none
                        Return Input
                    Case TransferFunctionType.sigmoid
                        Return Sigmoid(Input)
                    Case TransferFunctionType.HyperbolTangent
                        Return HyperbolicTangent(Input)
                    Case TransferFunctionType.BinaryThreshold
                        Return BinaryThreshold(Input)
                    Case TransferFunctionType.RectifiedLinear
                        Return RectifiedLinear(Input)
                    Case TransferFunctionType.Logistic
                        Return Logistic(Input)
                    Case TransferFunctionType.Gaussian
                        Return Gaussian(Input)
                    Case TransferFunctionType.Signum
                        Return Signum(Input)
                End Select
            End Function
            Public Shared Function EvaluateTransferFunctionDerivative(ByRef TransferFunct As TransferFunctionType, ByRef Input As Double) As Integer
                EvaluateTransferFunctionDerivative = 0
                Select Case TransferFunct
                    Case TransferFunctionType.none
                        Return Input
                    Case TransferFunctionType.sigmoid
                        Return SigmoidDerivitive(Input)
                    Case TransferFunctionType.HyperbolTangent
                        Return HyperbolicTangentDerivative(Input)
                    Case TransferFunctionType.Logistic
                        Return LogisticDerivative(Input)
                    Case TransferFunctionType.Gaussian
                        Return GaussianDerivative(Input)
                End Select
            End Function
            'Linear Neurons
            ''' <summary>
            ''' in a liner neuron the weight(s) represent unknown values to be determined
            ''' the outputs could represent the known values of a meal and the
            ''' inputs the items in the meal and the
            ''' weights the prices of the individual items
            ''' There are no hidden layers
            ''' </summary>
            ''' <remarks>answers are determined by determining the weights of the linear neurons
            ''' the delta rule is used as the learning rule: Weight = Learning rate * Input * LocalError of neuron</remarks>
            Public Shared Function Linear(ByRef value As Double) As Double
                ' Output = Bias + (Input*Weight)
                Return value
            End Function
            ''' <summary>
            ''' the step function rarely performs well except in some rare cases with (0,1)-encoded binary data.
            ''' </summary>
            ''' <param name="Value"></param>
            ''' <returns></returns>
            ''' <remarks></remarks>
            Public Shared Function BinaryThreshold(ByRef Value As Double) As Double

                ' Z = Bias+ (Input*Weight)
                'TransferFunction
                'If Z > 0 then Y = 1
                'If Z < 0 then y = 0

                If Value < 0 = True Then

                    Return 0
                Else
                    Return 1
                End If
            End Function
            Public Shared Function RectifiedLinear(ByRef Value As Double) As Double
                'z = B + (input*Weight)
                'If Z > 0 then output = z
                'If Z < 0 then output = 0
                If Value < 0 = True Then

                    Return 0
                Else
                    Return Value
                End If
            End Function
            Public Shared Function StochasticBinary(ByRef value As Double) As Double
                'Uncreated
                Return value
            End Function
            'Non Linear neurons
            Public Shared Function Logistic(ByRef Value As Double) As Double
                'z = bias + (sum of all inputs ) * (input*weight)
                'output = Sigmoid(z)
                'derivative input = z/weight
                'derivative Weight = z/input
                'Derivative output = output*(1-Output)
                'learning rule = Sum of total training error* derivative input * derivative output * rootmeansquare of errors

                Return 1 / 1 + Math.Exp(-Value)
            End Function
            Public Shared Function LogisticDerivative(ByRef Value As Double) As Double
                'z = bias + (sum of all inputs ) * (input*weight)
                'output = Sigmoid(z)
                'derivative input = z/weight
                'derivative Weight = z/input
                'Derivative output = output*(1-Output)
                'learning rule = Sum of total training error* derivative input * derivative output * rootmeansquare of errors

                Return Logistic(Value) * (1 - Logistic(Value))
            End Function
            Private Shared Function Gaussian(ByRef x As Double) As Double
                Gaussian = Math.Exp((-x * -x) / 2)
            End Function
            Private Shared Function GaussianDerivative(ByRef x As Double) As Double
                GaussianDerivative = Gaussian(x) * (-x / (-x * -x))
            End Function
            Public Shared Function HyperbolicTangent(ByRef Value As Double) As Double
                '    TanH(x) = (Math.Exp(x) - Math.Exp(-x)) / (Math.Exp(x) + Math.Exp(-x))

                Return Math.Tanh(Value)
            End Function
            Public Shared Function HyperbolicTangentDerivative(ByRef Value As Double) As Double
                HyperbolicTangentDerivative = 1 - (HyperbolicTangent(Value) * HyperbolicTangent(Value)) * Value
            End Function
            ''' <summary>
            ''' the log-sigmoid function constrains results to the range (0,1),
            ''' the function is sometimes said to be a squashing function in neural network literature.
            ''' It is the non-linear characteristics of the log-sigmoid function (and other similar activation functions)
            ''' that allow neural networks to model complex data.
            ''' </summary>
            ''' <param name="Value"></param>
            ''' <returns></returns>
            ''' <remarks>1 / (1 + Math.Exp(-Value))</remarks>
            Public Shared Function Sigmoid(ByRef Value As Integer) As Double
                'z = Bias + (Input*Weight)
                'Output = 1/1+e**z
                Return 1 / (1 + Math.Exp(-Value))
            End Function
            Public Shared Function SigmoidDerivitive(ByRef Value As Integer) As Double
                Return Sigmoid(Value) * (1 - Sigmoid(Value))
            End Function
            Public Shared Function Signum(ByRef Value As Integer) As Double
                'z = Bias + (Input*Weight)
                'Output = 1/1+e**z
                Return Math.Sign(Value)
            End Function
        End Class
        Public Class ClassTrainNeuralNetwork
            'Training the network : 
            'The network Is forward propagated which sets creates an output which may not be accurate 
            'Backward propagation of the network is required to adjust weights then the network is forward propagated again with the new weights
            'The network can be run until the correct weights are defined.
            'A Maximum epochs can be set to prevent and endless loop, 
            'Learning can be set to a small integer to define the steps of which the gradient will be defined.


            ''' <summary>
            ''' forward propagation of network
            ''' </summary>
            ''' <param name="nInput">Input to neural network</param>
            ''' <param name="nn">Neural network</param>
            ''' <returns>Output generated by network</returns>
            ''' <remarks>Output of network may not be correct until trained</remarks>
            Private Function ForwardProp(ByRef nInput As List(Of Integer), ByRef nn As NeuralNetwork) As List(Of Integer)
                'Forward Propagation: 
                'the initial execution of the network produces a result which may not be correct 
                'This means that training the network still needs to be accomplished

                '1. Get input(array) <Input>
                '2. node total(output)
                '3. sum layer pass to input hidden
                '4. node total
                '5. activate hidden
                '6. Pass Layer
                '7. repeat 2. (if required) deep learning
                '8. sum layer pass to input output
                '9. activate output
                '10. Get Output(array)<Output>

                Dim nOutput As New List(Of Integer)
                Dim count As Integer = 0

                'L1. Input Layer
                Dim LayerOutputs As New Integer
                '1. Get Inputs

                For Each node As Neuron In nn.InputLayer.Nodes
                    node.input = nInput(count)
                    count += 1
                Next
                '2. GetLayerOutputs
                LayerOutputs = SumLayerOutputs(nn.InputLayer)

                'L2. Hidden Layer(s)
                '3. PassLayerOutputs 
                For Each hlayer As Layer In nn.HiddenLayers
                    For Each node As Neuron In hlayer.Nodes
                        node.input = LayerOutputs
                    Next
                    '4. activate layer
                    hlayer = ActivateLayer(hlayer)
                    '5. GetLayerOutputs
                    LayerOutputs = SumLayerOutputs(hlayer)
                Next

                'L3. Output Layer
                'PassLayerOutputs
                For Each Node As Neuron In nn.OutputLayer.Nodes
                    Node.input = LayerOutputs
                Next
                'activate layer
                nn.OutputLayer = ActivateLayer(nn.OutputLayer)
                count = 0

                'Get Output 
                For Each node As Neuron In nn.OutputLayer.Nodes
                    nOutput(count) = node.output
                Next

                Return nOutput
            End Function
            ''' <summary>
            ''' Back Propagation Changes the weights 
            ''' </summary>
            ''' <param name="NN">Current neural network</param>
            ''' <param name="learningrate">rate of change to weights</param>
            ''' <param name="NeuronError">error generated by forward propagation</param>
            ''' <remarks></remarks>
            Private Function BackProp(ByRef NN As NeuralNetwork, ByRef learningrate As Integer,
                                  ByRef NeuronError As Integer) As NeuralNetwork
                Dim DeltaOutput As Double = 0.0
                Dim DeltaInput As Double = 0.0
                Dim DeltaHidden As Double = 0.0
                Dim SumOfDeltaOutput As Integer = 0
                Dim SumOfDeltaHidden As Integer = 0
                Dim SumOfHiddenLayerWeights As Integer = 0

                'Starts with the Output layer going backwards
                For Each Node As Neuron In NN.OutputLayer.Nodes
                    DeltaOutput = NeuronError * ClassTransferFunctions.EvaluateTransferFunctionDerivative(Node.input, NN.OutputLayer.ActivationFunction)
                    'NewWeight=weight+learning-Rate*node.output*DeltaOutput
                    Node.weight = Node.weight + learningrate * Node.output * DeltaOutput
                    SumOfDeltaOutput += DeltaOutput
                Next


                'then the hidden layer
                For Each hlayer As Layer In NN.HiddenLayers
                    For Each Node As Neuron In hlayer.Nodes
                        'deltaHidden = SigmoidDerivitive(Node.input) * SumOfDeltaOutput*weights
                        DeltaHidden = ClassTransferFunctions.EvaluateTransferFunctionDerivative(Node.input, hlayer.ActivationFunction) * SumWeights(NN.OutputLayer) * SumOfDeltaOutput
                        Node.weight = Node.weight + learningrate * Node.output * DeltaHidden
                        SumOfDeltaHidden = SumOfDeltaHidden + DeltaHidden
                        SumOfHiddenLayerWeights += Node.weight
                    Next
                Next

                'Then the input layer
                For Each node As Neuron In NN.InputLayer.Nodes
                    DeltaInput = SumOfHiddenLayerWeights * SumOfDeltaHidden
                    node.weight = node.weight + learningrate * node.output * DeltaInput
                Next


                Return NN
            End Function




            'CMDS

            ''' <summary>
            ''' Once network is created training data can be sent to the network, 
            ''' adjusting the weights as necessary, according to training parameters a max epochs is specified 
            ''' this prevents a case of an endless loop
            ''' </summary>
            ''' <param name="NN">Network to be trained</param>
            ''' <param name="NewCase">training case</param>
            ''' <param name="CaseTrainingParameters">training parameters</param>
            ''' <param name="Trained">set to true if trained</param>
            ''' <param name="NetworkError">returns error</param> 
            ''' <returns>neural network executed to max epochs</returns>
            ''' <remarks>if trained is false then the network weights 
            ''' will have been adjusted this may be closer to a true value, 
            ''' maybe more epochs may need to be added or 
            ''' after more cases have been trained the network will become trained</remarks>
            Public Function TrainCase(ByRef NN As NeuralNetwork, ByRef NewCase As TrainingCase,
                                  ByRef CaseTrainingParameters As TrainingParameters,
                                  ByRef Trained As Boolean, ByRef NetworkError As Integer) As NeuralNetwork


                Dim NetworkOutput As List(Of Integer) = ForwardProp(NewCase.Input, NN)
                NetworkError = CheckError(NetworkOutput, NewCase.Output)
                Trained = False
                'Loop until MaxEpochs or weights detected
                For Epoch = 1 To CaseTrainingParameters.MaxEpochs - 1
                    If NetworkError <= CaseTrainingParameters.ErrorThreshold = False Then
                        BackProp(NN, CaseTrainingParameters.LearningRate, NetworkError)
                        NetworkOutput = ForwardProp(NewCase.Input, NN)
                        NetworkError = CheckError(NetworkOutput, NewCase.Output)
                        Trained = False
                    Else
                        'Found <True weights detected>
                        Epoch = CaseTrainingParameters.MaxEpochs
                        NetworkError = CheckError(NetworkOutput, NewCase.Output)
                        Trained = True
                    End If
                Next

                Return NN
            End Function
            ''' <summary>
            ''' this function executes until a trained network is returned, 
            ''' the execution time of this network is unknown
            ''' </summary>
            ''' <param name="NN">network to be trained</param>
            ''' <param name="NewCase">training case</param>
            ''' <param name="CaseTrainingParameters">max epochs is not used</param>
            ''' <returns>trained network for the case</returns>
            ''' <remarks>to prevent over-fitting many training cases should be used, 
            ''' network error not used as the error will be 0</remarks>
            Public Function TrainCaseUntil(ByRef NN As NeuralNetwork, ByRef NewCase As TrainingCase,
                              ByRef CaseTrainingParameters As TrainingParameters) As NeuralNetwork


                Dim NetworkOutput As List(Of Integer) = ForwardProp(NewCase.Input, NN)
                Dim NetworkError As Integer = CheckError(NetworkOutput, NewCase.Output)

                'Loop until the network has adjusted the weights to an acceptable level
                Do Until (NetworkError <= CaseTrainingParameters.ErrorThreshold)
                    BackProp(NN, CaseTrainingParameters.LearningRate, NetworkError)
                    NetworkOutput = ForwardProp(NewCase.Input, NN)
                    NetworkError = CheckError(NetworkOutput, NewCase.Output)
                Loop

                Return NN
            End Function
            ''' <summary>
            ''' trains case until MaxEpochs which is useful for training whole lists
            ''' the intuition is that at the end of training the set will be trained. 
            ''' if not parameters can be adjusted and set retrained
            ''' </summary>
            ''' <param name="NN"></param>
            ''' <param name="NewCase"></param>
            ''' <param name="CaseTrainingParameters"></param>
            ''' <returns></returns>
            ''' <remarks>use CheckTrained() to see if network has been trained</remarks>
            Public Function TrainCase(ByRef NN As NeuralNetwork, ByRef NewCase As TrainingCase,
                          ByRef CaseTrainingParameters As TrainingParameters) As NeuralNetwork


                Dim NetworkOutput As List(Of Integer) = ForwardProp(NewCase.Input, NN)
                Dim NetworkError As Integer = CheckError(NetworkOutput, NewCase.Output)

                'Loop until MaxEpochs or weights detected
                For Epoch = 1 To CaseTrainingParameters.MaxEpochs - 1
                    If NetworkError <= CaseTrainingParameters.ErrorThreshold = False Then
                        BackProp(NN, CaseTrainingParameters.LearningRate, NetworkError)
                        NetworkOutput = ForwardProp(NewCase.Input, NN)
                        NetworkError = CheckError(NetworkOutput, NewCase.Output)
                    Else
                        'Found <True weights detected>
                        Epoch = CaseTrainingParameters.MaxEpochs
                        NetworkError = CheckError(NetworkOutput, NewCase.Output)
                    End If
                Next

                Return NN
            End Function
            Public Sub TrainList(ByRef NN As NeuralNetwork, ByRef trainingCases As List(Of TrainingCase), ByRef CaseTrainingParameters As TrainingParameters)
                For Each newcase As TrainingCase In trainingCases
                    NN = TrainCase(NN, newcase, CaseTrainingParameters)
                Next
            End Sub
            ''' <summary>
            ''' Checks neural network if trained for test case
            ''' </summary>
            ''' <param name="NN"></param>
            ''' <param name="Newcase"></param>
            ''' <returns>true/false</returns>
            ''' <remarks></remarks>
            Public Function CheckTrained(ByRef NN As NeuralNetwork, ByRef Newcase As TrainingCase, ByRef NewTrainingParameters As TrainingParameters) As Boolean
                Dim trained As Boolean = False

                Dim NetworkOutput As List(Of Integer) = ForwardProp(Newcase.Input, NN)
                Dim NetworkError As Integer = GetNetworkError(NN, Newcase)

                If NetworkError <= NewTrainingParameters.ErrorThreshold = False Then
                    trained = False
                Else
                    trained = True
                End If

                Return trained
            End Function
            ''' <summary>
            ''' returns the training error
            ''' </summary>
            ''' <param name="NN"></param>
            ''' <param name="Newcase"></param>
            ''' <returns>sum of squared errors of output</returns>
            ''' <remarks></remarks>
            Public Function GetNetworkError(ByRef NN As NeuralNetwork,
                                        ByRef Newcase As TrainingCase) As Integer
                Dim NetworkOutput As List(Of Integer) = ForwardProp(Newcase.Input, NN)
                Dim NetworkError As Integer = CheckError(NetworkOutput, Newcase.Output)
                Return NetworkError
            End Function
            ''' <summary>
            ''' returns network prediction
            ''' </summary>
            ''' <param name="NN">Neural network(trained or untrained)</param>
            ''' <param name="Newcase">Case input</param>
            ''' <returns>Network prediction</returns>
            ''' <remarks></remarks>
            Public Function GetNetworkOutput(ByRef NN As NeuralNetwork,
                                        ByRef Newcase As TrainingCase) As List(Of Integer)
                Dim NetworkOutput As List(Of Integer) = ForwardProp(Newcase.Input, NN)
                Return NetworkOutput
            End Function

        End Class
    End Class

    ''' <summary>
    ''' Required Components
    ''' </summary>
    ''' <remarks></remarks>
    Public Module ModuleNeuralNetworkComponents
        ''' <summary>
        ''' Neural network Layer types
        ''' </summary>
        ''' <remarks></remarks>
        Enum LayerType
            Input
            Hidden
            Output
        End Enum
        ''' <summary>
        ''' maximum time the network should be executed until a trained network is found (used in training)
        ''' </summary>
        ''' <remarks></remarks>
        Enum TransferFunctionType
            none
            sigmoid
            HyperbolTangent
            BinaryThreshold
            RectifiedLinear
            Logistic
            StochasticBinary
            Gaussian
            Signum
        End Enum
        ''' <summary>
        ''' Each layer consists of nodes (neurons) these are each individual. all layers contain nodes,
        ''' training cases will also use nodes as inputs to the neural network
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure Neuron
            ''' <summary>
            ''' The input of the node is the collective sum of the inputs and their respective weights
            ''' </summary>
            ''' <remarks></remarks>
            Public input As Double
            ''' <summary>
            ''' the output of the node is also relational to the transfer function used
            ''' </summary>
            ''' <remarks></remarks>
            Public output As Double
            ''' <summary>
            ''' There is a value attached with dendrite called weight.
            ''' The weight associated with a dendrites basically determines the importance of incoming value.
            ''' A weight with larger value determines that the value from that particular neuron is of higher significance.
            ''' To achieve this what we do is multiply the incoming value with weight.
            ''' So no matter how high the value is, if the weight is low the multiplication yields the final low value.
            ''' </summary>
            ''' <remarks></remarks>
            Public weight As Double
            ''' <summary>
            ''' the error that is produced respective to the output node, is required for calculations of the new weights
            ''' </summary>
            ''' <remarks></remarks>
            Public NeuronError As Double
        End Structure
        ''' <summary>
        ''' Each layer consists of neurons(nodes) the training cases also use an input layer and an output layer
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure Layer
            ''' <summary>
            ''' Collection of nodes
            ''' </summary>
            ''' <remarks></remarks>
            Public Nodes As List(Of Neuron)
            ''' <summary>
            ''' Activation function used by the nodes in the layer
            ''' </summary>
            ''' <remarks></remarks>
            Public ActivationFunction As TransferFunctionType
            ''' <summary>
            ''' Type of layer (Input, Hidden, Output)
            ''' </summary>
            ''' <remarks></remarks>
            Public LayerType As LayerType
            ''' <summary>
            ''' The number of nodes is stored to make iteration easier
            ''' </summary>
            ''' <remarks></remarks>
            Public NumberOfNodes As Integer
        End Structure
        Public Structure NeuralNetwork
            ''' <summary>
            ''' layer takes the inputs(the values you pass) and forwards it to hidden layer.
            ''' You can just imagine input layer as a group of neurons whose sole task is to pass the numeric inputs to the next level.
            '''  Input layer never processes data, it just hands over it.
            ''' </summary>
            ''' <remarks>there is only one layer for the input</remarks>
            Public InputLayer As Layer
            ''' <summary>
            ''' Middle layer: This layer is the real thing behind the network. Without this layer, network would not be capable of solving complex problems.
            ''' There can be any number or middle or hidden layers. But, for most of the tasks, one is sufficient. The number of neurons in this layer is crucial.
            ''' There is no formula for calculating the number, just hit and trial works.
            ''' This layer takes the input from input layer, does some calculations and forwards to the next layer, in most cases it is the output layer.
            ''' </summary>
            ''' <remarks>in a deep belief network there can be many hidden layers</remarks>
            Public HiddenLayers As List(Of Layer)
            ''' <summary>
            ''' Output layer: This layer consists of neurons which output the result to you. This layer takes the value from the previous layer,
            ''' does calculations and gives the final result. Basically,
            ''' this layer is just like hidden layer but instead of passing values to the next layer, the values are treated as output.
            ''' </summary>
            ''' <remarks>there is only one layer for the output</remarks>
            Public OutputLayer As Layer
        End Structure

        ''' <summary>
        ''' These are the parameters of the network to be created
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure NeuralNetworkParameters
            'Network Parameters
            Public NumberOfInputs As Integer
            Public NumberOfOutputs As Integer
            Public NumberOfHiddenLayers As Integer
            Public OutputLayerFunctionType As TransferFunctionType
            Public HiddenLayerFunctionType As TransferFunctionType
        End Structure

    End Module
    ''' <summary>
    ''' Training Functions
    ''' </summary>
    ''' <remarks></remarks>
    Public Module ModuleTrainingComponents
        ''' <summary>
        ''' Each training case has only inputs and expected outputs
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure TrainingCase
            ''' <summary>
            ''' input of Case to be calculated
            ''' </summary>
            ''' <remarks></remarks>
            Public Input As List(Of Integer)
            ''' <summary>
            ''' Expected output of training case
            ''' </summary>
            ''' <remarks></remarks>
            Public Output As List(Of Integer)
        End Structure
        ''' <summary>
        ''' Results of the network case
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure NetworkResult
            ''' <summary>
            ''' Output provided by the network / Or prediction
            ''' </summary>
            ''' <remarks></remarks>
            Public NetworkOutput As List(Of Integer)
            ''' <summary>
            ''' Sum of the squared errors
            ''' </summary>
            ''' <remarks></remarks>
            Public NetworkError As Integer
        End Structure
        ''' <summary>
        ''' training parameters used to train each case through the network 
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure TrainingParameters
            ''' <summary>
            ''' Acceptable error threshold
            ''' </summary>
            ''' <remarks></remarks>
            Public ErrorThreshold As Integer
            ''' <summary>
            ''' rate of learning small steps
            ''' </summary>
            ''' <remarks></remarks>
            Public LearningRate As Integer
            ''' <summary>
            ''' Maximum times to execute each case in training 
            ''' this may not be used if training mode is Set to endless
            ''' </summary>
            ''' <remarks></remarks>
            Public MaxEpochs As Integer
        End Structure

    End Module
    ''' <summary>
    ''' the calculations required for training and creating the network
    ''' </summary>
    ''' <remarks></remarks>
    Public Module ModuleNeuralNetworkCalculations


        'NeuralNetwork Generation :
        'The main calculations are for the calculation 
        'of initial weights required by the layers of the neural network
        'And the hidden nodes required by the deep belief network.
        ''' <summary>
        ''' Initial Weights can be determined by the number of hidden nodes and 
        ''' the number of input nodes 
        ''' this is a rule of thumb
        ''' </summary>
        ''' <param name="InputL">Number of Input Nodes</param>
        ''' <param name="InputH">Number of Hidden Nodes</param>
        ''' <returns>a random weight amount which can be used as initial weights</returns>
        ''' <remarks></remarks>
        Public Function CreateRandWeight(ByRef InputL As Integer, ByRef InputH As Integer) As Integer
            Randomize()
            Dim value As Integer = CInt(Int((InputH * Rnd()) + InputL))
            Return value
        End Function
        ''' <summary>
        ''' The number of hidden nodes to become effective is actually unknown yet a simple calculation 
        ''' can be used to determine an initial value which should be effective; 
        ''' </summary>
        ''' <param name="NumbeOfInputNodes">the number of input node used in the network</param>
        ''' <param name="NumberOfOutputNodes">the number of out put nodes in the network</param>
        ''' <returns>a reasonable calculation for hidden nodes</returns>
        ''' <remarks>Deep layer networks have multiple hidden layers with varied number of nodes</remarks>
        Public Function CalculateNumberOfHiddenNodes(ByRef NumbeOfInputNodes As Integer, ByRef NumberOfOutputNodes As Integer) As Integer
            CalculateNumberOfHiddenNodes = NumbeOfInputNodes + NumberOfOutputNodes / 2
            If CalculateNumberOfHiddenNodes < NumberOfOutputNodes Then CalculateNumberOfHiddenNodes = NumberOfOutputNodes
        End Function

        '3.
        ''' <summary>
        ''' the output for the layer can be provided as an input 
        ''' to each node in the next layer
        ''' </summary>
        ''' <param name="nlayer">Layer to be evaluated</param>
        ''' <returns>total output for the layer</returns>
        ''' <remarks>
        ''' LayerOutput = SumOfNodeOutputs</remarks>
        Public Function SumLayerOutputs(ByRef nlayer As Layer) As Double
            Dim LayerOutput As Double = 0

            For Each node As Neuron In nlayer.Nodes
                node.output = NodeTotal(node)
                LayerOutput += node.output
            Next
            'to be passed to next layer
            Return LayerOutput
        End Function
        '4.
        ''' <summary>
        ''' Produces a node total which can be fed to the activation function
        ''' </summary>
        ''' <param name="Node">Node to be calculated</param>
        ''' <returns>Node input * Node Weight</returns>
        ''' <remarks></remarks>
        Private Function NodeTotal(ByRef Node As Neuron) As Double
            Dim Sum As Double = 0
            Sum = Sum + (Node.input * Node.weight)
            Return Sum
        End Function
        '5./9.
        ''' <summary>
        ''' Activates each node in the layer
        ''' </summary>
        ''' <param name="Hlayer">Layer to be activated (Hidden or Output)</param>
        ''' <returns>activated layer</returns>
        ''' <remarks>layer to be summed to be passed to the inputs of the next layer</remarks>
        Public Function ActivateLayer(ByRef Hlayer As Layer) As Layer
            For Each node As Neuron In Hlayer.Nodes
                node = ActivateNode(node, Hlayer.ActivationFunction)
            Next
            Return Hlayer
        End Function
        ''' <summary>
        ''' Activates Node and sets the output for the node
        ''' </summary>
        ''' <param name="Node">Node to be activated</param>
        ''' <param name="Activation">Activation Function</param>
        ''' <returns>Activated Node</returns>
        ''' <remarks>ActivationFunction(Node.input * Node.weight)</remarks>
        Public Function ActivateNode(ByRef Node As Neuron, ByRef Activation As TransferFunctionType) As Neuron
            Dim Sum As Double = 0
            Sum = NodeTotal(Node)
            Node.output = ClassTransferFunctions.EvaluateTransferFunct(Activation, Sum)
            Return Node
        End Function
        '6.
        ''' <summary>
        ''' Layer outputs are passed to the next layer
        ''' </summary>
        ''' <param name="layeroutput">layer output produced from "SumlayerOutputs"</param>
        ''' <param name="nLayer">Next Layer</param>
        ''' <remarks>Passed to output or another hidden layer</remarks>
        Public Sub PassLayerOutputs(ByRef layeroutput As Double, ByRef nLayer As Layer)
            For Each node As Neuron In nLayer.Nodes
                node.input = layeroutput
            Next
        End Sub



        ''' <summary>
        ''' the output from the training set is measured against the output from the neural network
        ''' this cost function produces a sum of the squared errors 
        ''' which can be used to find new weights for the neural network
        ''' </summary>
        ''' <param name="nOutput">Output from Neural Network</param>
        ''' <param name="ExpectedOutput">Expected Output from training set</param>
        ''' <returns>The Sum of the squared errors * 0.5</returns>
        ''' <remarks>Cost function for gradient descent</remarks>
        Public Function CheckError(ByRef nOutput As List(Of Integer),
                          ByRef ExpectedOutput As List(Of Integer)) As Integer
            Dim count As Integer = 0
            Dim cost As Integer = 0
            Dim SquErr As New Integer
            Dim SumSquaredErr As New Integer

            For Each nOut As Integer In nOutput
                cost = CheckNodeErr(nOut, ExpectedOutput(count))

                SquErr = cost * cost
                SumSquaredErr += SquErr
                count += 1
            Next
            Return SumSquaredErr
        End Function

        ''' <summary>
        ''' Returns the error
        ''' </summary>
        ''' <param name="Recieved"></param>
        ''' <param name="Expected"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Function CheckNodeErr(ByRef Recieved As Double, ByRef Expected As Double) As Double
            Return Expected - Recieved
        End Function

        ''' <summary>
        ''' Produces a sum of the weights of the layer
        ''' </summary>
        ''' <param name="nlayer"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function SumWeights(ByRef nlayer As Layer) As Double
            Dim Sum As Double = 0
            For Each node As Neuron In nlayer.Nodes
                Sum = Sum + node.weight
            Next
            Return Sum
        End Function
    End Module

End Namespace

