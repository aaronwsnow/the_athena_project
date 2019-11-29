Option Explicit On
Option Strict Off
Option Infer On
Option Compare Binary

Imports Matrix
Imports Matrix.Utility
Imports System
Imports System.Collections.Generic

Module Program

    'NOTES
    '
    'I like to ideas of using puzzles to gain access to the correct commands or questions
    'all this to unlock mysteries
    '
    'make a script editor
    'make a visual story editor
    '
    'need to design storys
    '
    'Do some advanced Error TimeStamp tracker :)
    '
    'IsA relationship
    'A cat is a small animal
    'Is a cat a small animal
    'IsA, HasA, WasA,
    '
    '
    '
    '


    Class Diagnostics
        'True = circumvents the entire program to run the Test Sub
        Public Shared Property IsTest() As Boolean = True
        'True = suspends all timers on program load
        Public Shared Property SuspendTimers() As Boolean = False
        'True = suspends all timer functions used in normal mode
        Public Shared Property SuspendTimer() As Boolean = False
        'True = prevents program from entering GameWorld mode
        Public Shared Property SuspendWorlds() As Boolean = False
        'True = prevents program from entering nodule dialog mode 
        Public Shared Property SuspendNodules() As Boolean = True
        'True = suspends all local tamagochi features
        Public Shared Property SuspendTamagotchi() As Boolean = True
        'True = suspends all Leads in Timer 
        Public Shared Property SuspendLeads() As Boolean = True
        'True = Suspends the Introduction Dialog, runs when program finishes initial (needed) questionair
        Public Shared Property SuspendIntro() As Boolean = True
        'Disables/Enables: it checks to see if the UI is running, it kill the engine if not
        Public Shared Property UI_Minute_Check() As Boolean = False
    End Class


    Public WithEvents SecondTimer As New Timer(0, 0, 0, 1)
    Public WithEvents MinuteTimer As New Timer(0, 0, 1, 0)
    Public WithEvents HourTimer As New Timer(0, 1, 0, 0)
    Public WithEvents DayTimer As New Timer(1, 0, 0, 0)

    Public Sub Main()

        '*****
        'remove for actual use
        Pre_Load()
        '*****

        If Diagnostics.IsTest() = True Then
            Test()
        Else

            Processing.Load()

            Do

                '*****
                'remove for actual use
                Input_Console()
                '*****

                Processing.Ping()

                If User.Ready() = True Then

                    Processing.Recieve()
                    Processing.Process()
                    Processing.Finale()

                    '*****
                    'remove for actual use
                    Output_Console()
                    '*****

                End If

                Sleep(500)
            Loop

            Processing.Close()

        End If
    End Sub

    Public Property SuspendTimer() As Boolean = False
    Public Property SuspendTamagotchi() As Boolean = False
    Public Property SuspendLeads() As Boolean = False
    Public Property WorldLeads() As String() = {}

    Private Sub SecondEvent() Handles SecondTimer.Tick
        '-Check Reminder Date
        '-Make Random Statement/Jokes
        '-Make conversation leading statements
        '-Run Avatar Script
        '-Make Changes in Tags (ie. change mood, state)
        '-User could monitor system here

        Try

            Dim IsReady As Boolean = False
            If Locals.TimeSeriesRun() = False Then

                If SuspendTimer() = False And Diagnostics.SuspendTimer() = False Then
                    If SuspendLeads() = False And Diagnostics.SuspendLeads() = False Then

                        If GameWorld.IsPlaying() = False Then

                            Dim overwrite As Boolean = False
                            REM #SECOND

                            If overwrite = False Then
                                If RandBool(30) = True Then

                                    'these are leads that are used to entice the user into entering a GameWorld
                                    If Locals.Loops("gameworld_leads", 5) = True Then
                                        Dim lead = RandArray(GameWorld.Leads(), "")
                                        If IsEmpty(lead) = False Then
                                            Avatar.Response() = lead
                                            SuspendTimer() = True
                                            IsReady = True
                                        End If
                                    End If


                                ElseIf RandBool(30) = True Then

                                    'these leads are typically learned from the user for the purpose of restating 
                                    'them in order to learn an appropriate response  
                                    If Locals.Loops("repository_leads", 5) = True Then
                                        Dim leads = Tables.GetFromLists(Column.Leads(), "")
                                        If IsEmpty(leads) = False Then
                                            Avatar.Response() = leads
                                            SuspendTimer() = True
                                            IDS.Address(2, 1001)
                                            IsReady = True
                                        End If
                                    End If

                                ElseIf RandBool(30) = True Then

                                    If Locals.Loops("query_leads", 5) = True Then
                                        Dim leads = Tables.GetRandomQuery(LocalTags.Memory())
                                        If IsEmpty(leads) = False Then
                                            Avatar.Response() = leads
                                            SuspendTimer() = True
                                            IDS.Address(2, 1013)
                                            IsReady = True
                                        End If
                                    End If

                                ElseIf RandBool(30) = True Then

                                    'making random statements
                                    If Locals.Loops("local_random", 5) = True Then
                                        Dim rand = Tables.GetFromLists(Column.Random, "")
                                        If IsEmpty(rand) = False Then
                                            Avatar.Response() = rand
                                            IsReady = True
                                        End If
                                    End If

                                End If


                            End If

                        Else

                            'These leads are called only during gameplay for whatever reason
                            If Locals.Loops("gameworld_leads", 5) = True Then
                                If RandBool(30) = True Then
                                    Dim rand = RandArray(WorldLeads(), "")
                                    If IsEmpty(rand) = False Then
                                        Avatar.Response() = rand
                                        IsReady = True
                                    End If
                                End If
                            End If

                        End If
                    End If

                    If SuspendTamagotchi() = False And Diagnostics.SuspendTamagotchi() = False Then
                        If Tamagotchi.ProcessTick() = True Then IsReady = True
                    End If

                End If

                Tamagotchi.GeneralTick()

            Else
                SuspendTimer() = True

                If Locals.TimeSeriesLoops() = 0 Then

                    Dim respond = GetIndex(Locals.TimeSeries(), Locals.TimeSeriesCount(), "[exit]")
                    Locals.TimeSeriesLoops() = ToInt(GetIndex(Locals.TimeSeries(), Locals.TimeSeriesCount() + 1), 0)

                    If Search("[exit]", respond, Gate.eSEARCH) = True Then
                        Locals.TimeSeriesClose()
                        SuspendTimer() = False

                    Else
                        Locals.TimeSeriesCount() += 2
                        Avatar.Response() = respond
                        IsReady = True
                    End If

                End If

                Locals.TimeSeriesLoops() -= 1
            End If

            If IsReady = True Then
                Processing.Send()

                '*****
                'remove for actual use
                Output_Console()
                '*****
            End If

        Catch ex As Exception

        End Try

    End Sub

    Private Sub MinuteEvent() Handles MinuteTimer.Tick

        Try
            Dim IsReady As Boolean = False

            If Locals.TimeSeriesRun() = False Then
                If SuspendTimer() = False And Diagnostics.SuspendTimer() = False Then
                    If GameWorld.IsPlaying() = False Then

                        Dim overwrite As Boolean = False
                        REM #MINUTE

                        If overwrite = False Then
                            If Locals.Loops("exitcheck", 1) = True Then
                                'After 1 minute this program will exit if host program has been disabled
                                If IsProgramRunning(Files.UIPROGRAM()) = False And Diagnostics.UI_Minute_Check() = True Then
                                    ExitProgram()
                                End If
                            End If
                        End If


                    End If
                End If
            End If

            If IsReady = True Then
                Processing.Send()

                '*****
                'remove for actual use
                Output_Console()
                '*****
            End If

        Catch ex As Exception

        End Try

    End Sub

    Private Sub HourEvent() Handles HourTimer.Tick

        Try
            Dim IsReady As Boolean = False

            If Locals.TimeSeriesRun() = False Then
                If SuspendTimer() = False And Diagnostics.SuspendTimer() = False Then
                    If GameWorld.IsPlaying() = False Then

                        Dim overwrite As Boolean = False
                        REM #HOUR

                        If overwrite = False Then

                        End If

                    End If
                End If
            End If

            If IsReady = True Then
                Processing.Send()

                '*****
                'remove for actual use
                Output_Console()
                '*****
            End If

        Catch ex As Exception

        End Try

    End Sub

    Private Sub DayEvent() Handles DayTimer.Tick

        Try

            Dim IsReady As Boolean = False

            If Locals.TimeSeriesRun() = False Then
                If SuspendTimer() = False And Diagnostics.SuspendTimer() = False Then
                    If GameWorld.IsPlaying() = False Then

                        Dim overwrite As Boolean = False
                        REM #DAY

                        If overwrite = False Then

                        End If

                    End If
                End If
            End If

            If IsReady = True Then
                Processing.Send()

                '*****
                'remove for actual use
                Output_Console()
                '*****
            End If

        Catch ex As Exception

        End Try

    End Sub


    Private Sub Test()

        'uncomment when in practical use
        'Locals.Output.Response().Add(Locals.Input().UserInput())
        Dim input As String = ""

        'Word.Load("C:\Users\Aaron\source\repos\WpfApplication2\WpfApplication2\bin\Debug\Athena\standard\Word\standard.wd")

        'VerbInterpreter.Load()

        Do Until input = "exit"
            'comment when tested
            input = Console.ReadLine()

            CleanBible(input)

        Loop

    End Sub


    Private Sub CleanBible(ByVal input As String)
        If input = "start" Then

            Console.WriteLine("starting...")


            Dim out = ""


            Dim fileIn As String = "G:\Documents\Bible\in.txt"
            Dim fileOut As String = "G:\Documents\Bible\out.txt"

            Dim text = TextReader(fileIn)

            Dim newText = ""

            Dim total = text.ToArray.Count
            Dim count1 As Long = 0
            For Each c1 In text.ToArray
                If AscW(c1) = 8239 Then c1 = SPACE
                newText &= c1

                count1 += 1

                Console.Clear()
                Console.WriteLine("Percent Replace Complete.")
                Console.WriteLine(Math.Round(count1 * 100 / total, 2))
            Next

            text = newText

            Console.WriteLine("Parsing Chapters...")

            Dim chapterArray = ToArray(text, vbCrLf)
            Dim main_count = chapterArray.Count


            Dim modChapterArray As New List(Of String)

            Dim linegroup As String = ""
            For Each line In chapterArray

                If IsNumber(line) = True Then
                    modChapterArray.Add(linegroup)
                    modChapterArray.Add(line)
                    linegroup = ""
                Else
                    linegroup &= line & vbCrLf
                End If
            Next
            modChapterArray.Add(linegroup)

            Dim chapter As String = "-1"
            For i = 0 To modChapterArray.Count - 1
                Dim line = GetIndex(modChapterArray, i, "")
                Dim newline As String = ""

                If IsNumber(line) = True Then
                    chapter = line
                    out &= vbCrLf & "" & line & "" & vbCrLf
                    Continue For
                End If

                line = Remove(line, vbCrLf)

                Dim lineArray = ToArray(line, SPACE)

                For j = 0 To lineArray.Length - 1
                    Dim word1 = GetIndex(lineArray, j, "")

                    If IsNumber(word1) = True Then
                        If word1.Length = chapter.Length Then
                            word1 = ""
                        Else
                            word1 = vbCrLf & MultiStrings(SPACE, 3) & LMidTrim(word1, chapter.Length)
                        End If
                    Else

                        Dim newWord = ""
                        For Each w In word1
                            If IsNumber(w) = False Then newWord &= w
                        Next

                        word1 = newWord
                    End If

                    newline &= word1 & SPACE
                Next

                out &= newline & vbCrLf
            Next


            TextWriter(fileOut, out, False)

            Console.WriteLine("finished")

        End If

    End Sub


    '*****
    'remove for actual use
    Private Sub Pre_Load()
        Folders.Package() = "standard"

        Dim input = New Memory()
        input.Package() = "standard"
        input.Avatar() = "Athena"
        input.Profile() = "Aaron"
        input.Cluster() = Folders.Databases() & "standard.db"
        input.Word() = Folders.Word() & "standard.wd"
        input.Location() = Files.Output()
        input.Save()
    End Sub

    Private Sub Input_Console()
        'this represents the User writing in the UI and it being saved to the UI Output file

        Dim input = New Memory()
        input.UserInput() = Console.ReadLine()
        input.Location() = Files.Output()
        input.Ready() = True
        input.Save()
    End Sub

    Private Sub Output_Console()
        'this represents the UI reading and displaying from the UI Input file

        Dim output = New Memory()
        output.Location() = Files.Input()
        output.Load()

        Dim out As String = ""
        Console.WriteLine("**Console_Reads**")


        out = Join(output.Response(), vbCrLf)
        If IsEmpty(out) = False Then
            Console.WriteLine("[Response]")
            Console.WriteLine(out)
        End If

        out = Join(output.Status(), vbCrLf)
        If IsEmpty(out) = False Then
            Console.WriteLine("[Status]")
            Console.WriteLine(out)
        End If

        out = Join(output.Errors(), vbCrLf)
        If IsEmpty(out) = False Then
            Console.WriteLine("[Errors]")
            Console.WriteLine(out)
        End If

        out = Join(output.Commands(), vbCrLf)
        If IsEmpty(out) = False Then
            'Console.WriteLine("Commands:")
            'Console.WriteLine(out)
        End If

        out = output.Script()
        If IsEmpty(out) = False Then
            Console.WriteLine("[Script]" & out)
        End If

        out = output.Book()
        If IsEmpty(out) = False Then
            Console.WriteLine("[Book]" & out)
        End If

        out = Join(output.Code(), vbCrLf)
        If IsEmpty(out) = False Then
            Console.WriteLine("[Code]")
            Console.WriteLine(out)
        End If


        output.Clear()
        output.Save()

        Console.WriteLine("FINISHED!")
        Console.WriteLine("***********")

    End Sub

    '*****
End Module



Public Class Processing
    'This is for organization of major ai sub-divisions 

    Public Shared Sub Load()
        'Loading anything needed before the actual program starts

        'This is used to load the Directory
        Dim input = Connect.Input()

        'highest priority for folder orientation
        Folders.Package() = input.Package()

        'Setting Debug file location
        Debug.Debugfile() = Files.Debug()
        Debug.Clear()

        'Loading Tables
        Tables.Connection() = input.Cluster()
        Tables.Load()

        'Loading General Story
        Locals.GeneralDialog.Source() = Files.GeneralDialog()
        Locals.GeneralDialog.Loadfile()

        'Loading GameWorld
        GameWorld.Load()

        Avatar.Name() = input.Avatar()
        User.Profile() = input.Profile()

        Word.Load(input.Word())

        'Loading filter
        Locals.Filter() = Tables.GetList(Column.Filter())
        Locals.Block() = Tables.GetList(Column.Block())

        Tamagotchi.Initialize()
        Tamagotchi.Load()

        'This sets up for the Initial Nodule to Run
        IDS.Address(-1, 1000)
        'Console.WriteLine("[Load]")
    End Sub

    Public Shared Sub Close()

        'Physically Save Tables to File
        'Dialogs.SaveMemory()
        Tables.DeleteTempDatabase()
        Tables.Save()
        Word.Save()
        Locals.GeneralDialog.Save()

        'Console.WriteLine("[Exited]")
    End Sub

    Public Shared Sub Ping()
        User.Clear()
        Locals.Input() = Connect.Input()
    End Sub

    Public Shared Sub Recieve()
        'process input Text
        'imediate command processing
        'word processing
        'loading tags from database

        Dim localInput = Locals.Input()

        'This directly processes input, ie. exit, save 
        Dim IsCommand = Locals.Command(localInput.UserInput())
        If IsCommand = True Then
            localInput.Ready() = False
        End If

        'parsing input tags
        Dim tagStream = GetMidArg(localInput.UserInput(), "tags(", ")", Gate.eEQUALS)
        localInput.UserInput() = Trim(Remove(localInput.UserInput(), "tags(" & tagStream & ")"))

        'modifying or cleaning input
        localInput.UserInput() = SingleSpace(localInput.UserInput())

        'Can be used for Normalization or Input
        localInput.UserInput() = Tables.Substitute(localInput.UserInput(), Gate.eSEARCH)

        Locals.Input() = localInput

        User.Normal() = Tables.GetNormal(localInput.UserInput())

        'Parses input with NLP
        Word.Parse()
        LocalTags.Memory.Merge(tagStream)

    End Sub

    Public Shared Sub Send()

        'processing output

        'modify or clean output
        Dim newResponse As New List(Of String)
        For Each line In Locals.Output().Response()

            line = Tables.GetPool(line)
            line = Locals.InLineTagging(line)

            'filter unwanted/curse words that made it this far
            For Each fwords In Locals.Filter()
                line = Replace(line, Utility.Buffer(fwords, " "), Resources.Filter())
            Next

            'filter unwanted/curse words that made it this far
            For Each bwords In Locals.Block()
                If Search(line, bwords, Gate.eEQUALS) = True Then
                    line = Resources.Block()
                End If
            Next

            line = SingleSpace(line)
            line = Capitalize(line)
            line = AddMark(line, ".")

            newResponse.Add(line)
        Next

        'random selection on empty
        If newResponse.Count() = 0 Then
            Dim result = Tables.GetFromLists(Column.Empty(), "")

            If IsEmpty(result) = True Then
                newResponse.Add(RandArray({"I have nothing to say"}, ""))
            Else
                newResponse.Add(result)
            End If
        End If

        'process in-line tags for status
        Dim newStatus As New List(Of String)
        For Each line In Locals.Output().Status()
            line = Locals.InLineTagging(line)
            newStatus.Add(line)
        Next

        Locals.Output().Response() = newResponse
        Locals.Output().Status() = newStatus

        Avatar.Previous() = newResponse
        User.Previous() = User.UserInput()

        'sending info to the ui 
        Connect.Output() = Locals.Output()
        User.Clear()

    End Sub

    Public Shared Sub Timers()
        If Diagnostics.SuspendTimers() = False Then
            SecondTimer.Start()
            MinuteTimer.Start()
            HourTimer.Start()
            DayTimer.Start()
        End If
    End Sub

    Public Shared Sub Process()

        If IDS.Mode() = 0 Then

            If General.Initial() = True Then Exit Sub

            If General.Load() = True Then Exit Sub

            If General.Commands() = True Then Exit Sub

            If General.Worlds() = True Then Exit Sub

            If General.Detect() = True Then Exit Sub

            If General.Learn() = True Then Exit Sub

        Else

            If General.Nodules() = True Then Exit Sub

        End If

    End Sub

    Public Shared Sub Finale()

        'gets tags from Responses and merge them with the memory tags
        'inline tags overwrite memory tags
        LocalTags.Memory.Merge(LocalTags.GetInlineTags())
        LocalTags.Parse()
        Tamagotchi.Parse()
        Tamagotchi.Status()

        'saving stats
        Tamagotchi.Select()
        Tamagotchi.Save()

        'stores tags to table
        Tables.SetTags(LocalTags.Memory())

        'this saves the conversational story and position
        GameWorld.SaveMemory()


        'This checks if the Avatar Output contains timed output
        'ie. I see you.. >> 2(sec) >> standing there
        If Locals.TimeSeriesCheck() = False Then
            Send()
        End If

    End Sub

End Class

Public Class General

    Public Shared Function Load() As Boolean

        'joins Memory Tags (from database) and Tags derived from User's Input
        If User.Sentence.Tags.Count() > 0 Then
            LocalTags.Memory.Merge(User.Sentence.Tags())
        End If

        Return False
    End Function

    Public Shared Function Worlds() As Boolean

        Dim overwrite As Boolean = False
        REM #DIALOG

        If overwrite = False Then
            'FORCED GAMEWORLD SELECT
            If User.Search("test world", Gate.eEQUALS) = True Then
                GameWorld.Exit()
                GameWorld.SetCurrentWorld("test")
                Avatar.Response() = GameWorld.Intro()
                IDS.Address(1, 1000)
                GameWorld.IsPlaying() = True
                Return True
            End If


        End If

        'GAMEWORLD SELECT
        If Diagnostics.SuspendWorlds() = False And GameWorld.IsPlaying() = False Then
            GameWorld.SetCurrentWorld(GameWorld.DESKTOP)
            GameWorld.Process()
        End If

        Return False
    End Function

    Public Shared Function Initial() As Boolean

        Dim overwrite As Boolean = False
        REM #INITIAL

        If overwrite = False Then

            If Diagnostics.SuspendIntro() = False Then
                If Locals.DoOnce("initial") = True Then

                    'Into Dialog
                    IDS.Address(3, 1001)
                    Nodules()
                    Return True
                End If
            End If


            'restarts (if suspended) the timers when the user speaks 
            SuspendTimer() = False

        End If

        Return False
    End Function

    Public Shared Function Commands() As Boolean

        Dim arg As String = ""
        Dim overwrite As Boolean = False
        REM #COMMANDS


        If overwrite = False Then
            Dim prefix = "please "

            Avatar.Commands() = prefix & "read book"
            Avatar.Commands() = prefix & "open link"
            Avatar.Commands() = prefix & "play sound"
            Avatar.Commands() = prefix & "repeat"
            Avatar.Commands() = prefix & "start dialog"
            Avatar.Commands() = prefix & "exit/ignore dialog"
            Avatar.Commands() = prefix & "print tags"
            Avatar.Commands() = prefix & "print tag"
            Avatar.Commands() = prefix & "enter world"
            Avatar.Commands() = prefix & "exit world"

            If User.Search(prefix, Gate.eSTART) = True Then


                REM #COMMAND

                arg = User.GetArg("read book")
                If IsEmpty(arg) = False Then
                    Avatar.Book() = Locals.Book(arg)
                    Return True
                End If

                arg = User.GetArg("open link")
                If IsEmpty(arg) = False Then
                    Locals.OpenLink(arg)
                    Avatar.Response() = "Now opening """ & arg & """."
                    Return True
                End If

                arg = User.GetArg("play sound")
                If IsEmpty(arg) = False Then
                    Locals.PlaySounds(arg)
                    Avatar.Response() = "Now playing """ & arg & """."
                    Return True
                End If

                arg = User.GetArg("repeat")
                If IsEmpty(arg) = False Then
                    Avatar.Response() = arg
                    Return True
                End If

                If User.Search("print tags") = True Then

                    For Each tag In LocalTags.Memory.TagList()
                        Avatar.Status() = tag.Name() & ":" & Join(tag.Values, ",")
                    Next

                    Return True
                End If

                arg = User.GetArg("print tag")
                If IsEmpty(arg) = False Then
                    Avatar.Status() = Join(LocalTags.MemoryTags(arg), ":")
                    Return True
                End If

                arg = User.GetArg("enter world")
                If IsEmpty(arg) = False Then
                    GameWorld.SetCurrentWorld(arg)
                    Avatar.Response() = "Now playing """ & arg & """."
                    IDS.Address(1, 1000)
                    GameWorld.IsPlaying() = True
                    SuspendTimer() = True
                    Return True
                End If

                arg = Trim(User.GetArg("exit world"))
                If IsEmpty(arg) = False Then
                    GameWorld.Exit()
                    Remove(Tables.DEF_TAG_TEMP, Gate.eSEARCH)
                    Tables.RemoveTempTags()
                End If

            End If
        End If


        Return False
    End Function

    Public Shared Function Detect() As Boolean

        Dim overwrite As Boolean = False
        REM #DETECT

        If overwrite = False Then
            'If Locals.Sentence.SearchAll({"bird"}, Comparer.CAT) = True Then
            '   Locals.Response() = "It's a bird."
            '   Return 0
            'End If

            If User.Search({Nothing, Nothing, "bird"}, {1, 1, 0}, NLPC.CAT) = True Then
                'Dim arg = User.Sentence.GetWord({"bird"}, Comparer.CAT)
                Dim args = User.GetSearchJIndex(2)
                Avatar.Response() = Article(args, True) & " is a bird."
                Return True
            End If

            If User.Search({"a", "bird", Nothing, "hop"}, {0, 0, 1, 0}, NLPC.SEN) = True Then
                Avatar.Response() = "A bird can jump high."
                Return True
            End If

            Dim results = Tables.GeneralDetect("Greetings", User.UserInput(), LocalTags.Memory(), Gate.eEQUALS)
            If IsEmpty(results) = False Then
                Avatar.Response() = results
                Return True
            End If


            If User.Search("be creepy", Gate.eSEARCH) = True Then
                Avatar.Response() = "I think.. >> 3 >> you look... >> 10 >> very nice."
                Return True
            End If

            If User.Search({"tell me your name",
                           "what is your name",
                           "I would like to know your name",
                           "may i ask you your name"}) = True Then

                Avatar.Response() = RandArray({"My name is <avatar>.", "<avatar>, My name is <avatar>."}, "")
                Return True
            End If

            If User.Search({"tell me my name",
                           "what is my name",
                           "I would like to know my name",
                           "can you guess my name"}) = True Then

                Avatar.Response() = RandArray({"Your name is <user>.", "<user>, your name is <user>."}, "")
                Return True
            End If

            If User.Search({"what time is it",
                           "what is the time",
                           "I want the time",
                           "tell me the time",
                           "give me the time",
                           "know the time"}, Gate.eSEARCH) = True Then

                Avatar.Response() = "The time is " + TodaysLongTime() & "."
                Return True
            End If

            If User.Search({"what date is it",
                            "what is the date",
                            "I want the date",
                            "tell me the date",
                            "give me the date",
                            "know the date",
                            "what is today's date",
                            "what is the date today"}, Gate.eSEARCH) = True Then

                Avatar.Response() = "The date is " + TodaysDate() & "."
                Return True
            End If


            If User.Search({"what is your mood",
                           "tell me your current emotion",
                           "how do you feel",
                           "what mood are you in"}, Gate.eSEARCH) = True Then

                Avatar.Response() = "I feel " & Tamagotchi.CurrentEmotion()
                Return True
            End If

            If User.Search({"how old are you",
                           "what is your age",
                           "tell me your age"}, Gate.eSEARCH) = True Then

                Avatar.Response() = "I am " & BirthdayAge() & " years old."
                Return True
            End If

            If User.Search({"when is your birthday",
                            "when were you born",
                            "when were you created"}, Gate.eSEARCH) = True Then

                Avatar.Response() = "I was born around " & Birthday()
                Return True
            End If


            If User.Search({"magic", "8", "ball"}, Gate.eAND) = True Then

                Avatar.Response() = "Go ahead. Ask away."
                SuspendTimer() = True
                IDS.Address(2, 1009)
                Return True
            End If

            If User.Search({"guess", "my", "number"}, Gate.eAND) = True Then

                Locals.Vars.Index("original_guess") = Rand(0, 100)

                Avatar.Response() = "Go ahead. Guess a number between 0 and 100"
                SuspendTimer() = True
                IDS.Address(2, 1011)
                Return True
            End If


            If Tamagotchi.Process() = True Then Return True


        End If


        'If Tags.MemoryExist("mood", "hostle", True) = True Then
        'End If

        'If User.Search("*you are stupid*") = True Then
        '   Avatar.Response() = "That hurt my feelings."
        '   Tamagotchi.Add("sad", 10)
        '   Return True
        'End If


        'If Tables.Detect("Greetings", User.ToArray(), Gate.eSEARCH) = True Then
        '   New Pairs("Gender", "female")
        '   Avatar.Response() = Tables.Respond("Greetings", Tags.Memory(), Locals.Filter())
        '   Return True
        'End If

        'If User.Search({C({"what"}), C({"data"}), C({"is"}), C({"in"}), Nothing}, {0, 0, 0, 0, 1}, NLPC.SEN) = True Then
        '   Dim args = User.GetSearchJIndex(5)
        '   Avatar.Response() = Join(Tags.MemoryTags(args), ":")
        '   Return True
        'End If

        'Need to find all posible ways of finding an argument
        'Search and GetArg
        'Search{one,two, * ,four} and GetIndex of sentence array


        Return False
    End Function

    Public Shared Function Learn() As Boolean

        Dim overwrite As Boolean = False
        REM #LEARN

        If overwrite = False Then
            Dim results As String = ""



            'GENERAL NLP
            'add noun:bird
            'add bird:chicken
            'add huge:very large

            'they are all deposited in their respective files 
            '(generalpos, categories, synonyms, compounds)
            'that way all user addions can be more easily traced

            'word/ [pos] (pos, cat, syn, com)
            '(category:member)
            'noun:bird
            'verb:walk

            If User.Search("word/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)

                If IsEmpty(keytags) = True Then
                    keytags = "pos"
                End If

                Dim newNLPC As New NLPC
                If Search(keytags, "pos", Gate.eEQUALS) = True Then
                    newNLPC = NLPC.POS
                End If

                If Search(keytags, "cat", Gate.eEQUALS) = True Then
                    newNLPC = NLPC.CAT
                End If

                If Search(keytags, "syn", Gate.eEQUALS) = True Then
                    newNLPC = NLPC.SYN
                End If

                If Search(keytags, "com", Gate.eEQUALS) = True Then
                    newNLPC = NLPC.COM
                End If

                For Each line In lines
                    Dim cmds = ToArray(line, ":")
                    Dim cat = Trim(GetIndex(cmds, 0, ""))
                    Dim entry = Trim(GetIndex(cmds, 1, ""))

                    If IsEmpty(cat) = False And IsEmpty(entry) = False Then
                        Word.SetEntry(cat, entry, newNLPC)
                    End If
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            'a * is a *
            'ie. a bird is a noun
            If User.Search({"a", Nothing, "is", "a", Nothing}, {0, 1, 0, 0, 1}, 5, NLPC.SEN) = True Then
                Dim words = User.GetSearchIndex(1)
                Dim cat = User.GetSearchIndex(4)

                If SearchObject(cat, {POS.ADJ, POS.ADV, POS.ART, POS.AUX, POS.CONJ, POS.INTR, POS.NOUN, POS.PREP, POS.PRO, POS.VERB}, True, Gate.eEQUALS) = True Then
                    Word.SetEntry(cat, words, NLPC.POS)

                    Avatar.Response() = Resources.Remember()
                    Return True
                End If

            End If

            If User.Search("what part of speech is * of") = True Then
                Dim args = User.GetArg("is", "of")

                If IsEmpty(args) = True Then
                    Avatar.Response() = Resources.NoAnswer()
                Else
                    Avatar.Response() = Word.GetPOS(args)
                End If

                Return True
            End If


            'a * is a type of *
            If User.Search({"a", Nothing, "is", "a", "type", "of"}, {0, 1, 0, 0, 0, 0, 1}, 7, NLPC.SEN) = True Then
                Dim words = User.GetSearchIndex(1)
                Dim cat = User.GetSearchIndex(6)

                Word.SetEntry(cat, words, NLPC.CAT)
                Return True
            End If

            If User.Search("what category is * of") = True Then
                Dim args = User.GetArg("is", "of")

                If IsEmpty(args) = True Then
                    Avatar.Response() = Resources.NoAnswer()
                Else
                    Avatar.Response() = Word.GetCategory(args)
                End If

                Return True
            End If

            'a * is a synonym of *
            If User.Search({"a", Nothing, "is", "a", "synonym", "of"}, {0, 1, 0, 0, 0, 0, 1}, 7, NLPC.SEN) = True Then
                Dim words = User.GetSearchIndex(1)
                Dim cat = User.GetSearchIndex(6)

                Word.SetEntry(cat, words, NLPC.SYN)
                Return True
            End If

            If User.Search("tell me the synonym of*") = True Then
                Dim args = User.GetArg("of")

                If IsEmpty(args) = True Then
                    Avatar.Response() = Resources.NoAnswer()
                Else
                    Avatar.Response() = Word.GetSynonym(args)
                End If

                Return True
            End If

            'a * is a compound
            If User.Search("a * is a compound*", Gate.eEQUALS) = True Then
                Dim words = User.GetArg("a", "is")
                Dim com = WordArray(words)

                If com.Length() > 1 Then
                    Word.SetEntry(NLP.DEF_COMP, words, NLPC.COM)
                End If

                Return True
            End If

            If User.Search("is * a compound*") = True Then
                Dim args = User.GetArg("is", "a")

                If IsEmpty(args) = True Then
                    Avatar.Response() = Resources.NoAnswer()
                Else
                    Avatar.Response() = Word.DoesExist(NLP.DEF_COMP, args, NLPC.COM)
                End If

                Return True
            End If


            'GENERAL LISTS TABLE
            'list/ [add] (add/new, remove)
            '(column:list_item)
            'fruit:apples|pinapples
            'fruit:oranges
            'weapons:knife


            If User.Search("list/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)

                If IsEmpty(keytags) = True Then
                    keytags = "add"
                End If

                If Search(keytags, {"add", "new"}, Gate.eEQUALS) = True Then
                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim column = Trim(GetIndex(cmds, 0, ""))
                        Dim entry = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(column) = False And IsEmpty(entry) = False Then
                            Tables.AddToLists(column, entry)
                        End If
                    Next
                End If

                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim column = Trim(GetIndex(cmds, 0, ""))
                        Dim entry = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(column) = False And IsEmpty(entry) = False Then
                            Tables.RemoveFromList(column, entry)
                        End If
                    Next
                End If

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            'STANDARD LISTS
            'BLOCK, EMPTY, RANDOM

            'BLOCK

            'block/
            '(what you want blocked)
            'i hate chickens
            'pickle eater

            If User.Search("block/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Block(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please block", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("block")
                Tables.AddToLists(Column.Block(), args)
                Avatar.Response() = Resources.Comply()
            End If

            'unblock/
            '(what you want unblocked)
            'i hate chickens
            'pickle eater

            If User.Search("unblock/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.RemoveFromList(Column.Block(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please unblock", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("unblock")
                Tables.RemoveFromList(Column.Block(), args)
                Avatar.Response() = Resources.Comply()
            End If

            'FILTER

            'filter/
            '(what you want filtered)
            'sexist
            'haters

            If User.Search("filter/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Filter(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please filter", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("filter")
                Tables.AddToLists(Column.Filter(), args)
                Avatar.Response() = Resources.Comply()
            End If

            'unfilter/
            '(what you want unfiltered)
            'sexist
            'haters

            If User.Search("unfilter/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.RemoveFromList(Column.Filter(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please unfilter", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("unfilter")
                Tables.RemoveFromList(Column.Filter(), args)
                Avatar.Response() = Resources.Comply()
            End If

            'empty/
            '(if there is nothing to say then a random sentence is chosen from this list)
            'i hate chickens
            'i have nothing to say

            If User.Search("empty/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Empty(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            'leads/
            'i hate chickens
            'i have nothing to say
            'used to entice/lead the user to say things
            If User.Search("leads/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Leads(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please remember that", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("that")
                Tables.AddToLists(Column.Leads(), args)
                Avatar.Response() = Resources.Comply()
                Return True
            End If
            If User.Search("please remember", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("remember")
                Tables.AddToLists(Column.Leads(), args)
                Avatar.Response() = Resources.Comply()
                Return True
            End If

            'random/
            'i hate chickens
            'why did the chicken cross the road >> 2 >> to get to the other side

            If User.Search("random/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Random(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please randomly say", Gate.eSEARCH) = True Then
                Dim args = User.GetArg("say")
                Tables.AddToLists(Column.Random(), args)
                Avatar.Response() = Resources.Comply()
                Return True
            End If


            'dreams/
            'please don't eat me >> 2 >> cause I'm about to eat you

            If User.Search("dreams/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Dreams(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            'dreams/
            'cookie
            'banana

            If User.Search("foods/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Foods(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("you can eat *") = True Then
                Dim args = User.GetArg("eat")
                Tables.AddToLists(Column.Foods(), args)
                Avatar.Response() = Resources.Comply()
                Return True
            End If

            'insults/
            'your mothing dropped you on you head as a child

            If User.Search("insults/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Insults(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            'compliments/
            'you look really nice

            If User.Search("compliments/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Tables.AddToLists(Column.Compliments(), line)
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            'PERSONAL LISTS

            'I like

            If User.Search({"i", "like", Nothing}, {0, 0, 1}, NLPC.SEN) = True Then

                Dim args = Trim(User.GetSearchJIndex(2))

                Tables.AddToLists("user-like", args)

                Avatar.Response() = "I will remember you like " & args & "."

                Return True
            End If

            If User.Search("what do i like*") = True Then

                Dim item = Tables.GetFromLists("user-like", "")

                If IsEmpty(item) = False Then
                    Avatar.Response() = "You like " & item & "."
                Else
                    Avatar.Response() = "I do not know what you like."
                End If

                Return True
            End If


            If User.Search({"you", "like", Nothing}, {0, 0, 1}, NLPC.SEN) = True Then

                Dim args = Trim(User.GetSearchJIndex(2))

                Tables.AddToLists("avatar-like", args)

                Avatar.Response() = "I will remember that I like " & args & "."

                Return True
            End If

            If User.Search("what do you like*") = True Then

                Dim item = Tables.GetFromLists("avatar-like", "")

                If IsEmpty(item) = False Then
                    Avatar.Response() = "I like " & item & "."
                Else
                    Avatar.Response() = "I do not know what I like."
                End If

                Return True
            End If

            'I hate

            'I want

            'I love

            'I feel

            'I'm angry



            'BANK

            If User.Search("can you keep a secret*") = True Then
                IDS.Address(2, 1003)
                Avatar.Response() = "You must first tell me a password."
                SuspendTimer() = True
                Return True
            End If

            If User.Search("tell me a secret*") = True Then
                IDS.Address(2, 1004)
                Avatar.Response() = "You must first tell me a password."
                SuspendTimer() = True
                Return True
            End If

            If User.Search("please forget a secret*") = True Then
                IDS.Address(2, 1005)
                Avatar.Response() = "You must first tell me a password."
                SuspendTimer() = True
                Return True
            End If

            'GENERAL SUBSTITUTE TABLE
            'substitute/ [add] (add/new)
            '(searched:replace)
            'you:I
            'love:hate
            'Erin:Aaron

            'substitute/ [remove] (remove)
            '(searched)
            'you
            'love
            'Erin


            If User.Search("substitute/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)

                If IsEmpty(keytags) = True Then
                    keytags = "add"
                End If

                If Search(keytags, {"add", "new"}, Gate.eEQUALS) = True Then
                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim search = Trim(GetIndex(cmds, 0, ""))
                        Dim replace = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(search) = False And IsEmpty(replace) = False Then
                            Tables.AddSubstitute(search, replace)
                        End If
                    Next
                End If

                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        Tables.RemoveSubstitute(line)
                    Next
                End If

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            If User.Search("instead of * say *") = True Then

                Dim args1 = User.GetArg("of", "say")
                Dim args2 = User.GetArg("say")

                Tables.RemoveSubstitute(args1)
                Tables.AddSubstitute(args1, args2)

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            If User.Search("replace * with *") = True Then

                Dim args1 = User.GetArg("replace", "with")
                Dim args2 = User.GetArg("with")

                Tables.RemoveSubstitute(args1)
                Tables.AddSubstitute(args1, args2)

                Avatar.Response() = Resources.Comply()
                Return True
            End If


            'GENERAL TAGS TABLE
            'tags/ [add] (add/new, update)
            '(tag_name:value1|value2)
            'emotion:angry

            'tags/ [remove] (remove)
            '(tag_name)
            'emotion


            If User.Search("tags/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)

                If IsEmpty(keytags) = True Then
                    keytags = "add"
                End If

                If Search(keytags, {"add", "new"}, Gate.eEQUALS) = True Then
                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim name = Trim(GetIndex(cmds, 0, ""))
                        Dim entry = CleanArray(ToArray(GetIndex(cmds, 1, ""), "|"))

                        If IsEmpty(name) = False And IsEmpty(entry) = False Then
                            LocalTags.MemoryTags(name, False) = entry
                        End If
                    Next
                End If

                If Search(keytags, "update", Gate.eEQUALS) = True Then
                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim name = Trim(GetIndex(cmds, 0, ""))
                        Dim entry = CleanArray(ToArray(GetIndex(cmds, 1, ""), "|"))

                        If IsEmpty(name) = False And IsEmpty(entry) = False Then
                            LocalTags.Update(name, entry)
                        End If
                    Next
                End If

                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        LocalTags.Remove(line, Gate.eEQUALS)
                    Next
                End If

                Avatar.Response() = Resources.Processed()
                Return True
            End If




            'STORE
            'store/
            '(name:entry1|entry2)

            If User.Search("store/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                lines = GetRange(CleanArray(lines), 1)

                For Each line In lines
                    Dim cmds = ToArray(line, ":")
                    Dim name = Trim(GetIndex(cmds, 0, ""))
                    Dim entry = Trim(GetIndex(cmds, 1, ""))

                    If IsEmpty(name) = False And IsEmpty(entry) = False Then
                        Tables.Stores(name) = ToArray(entry, "|")
                    End If
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            If User.Search("please store log") = True Then
                IDS.Address(2, 1006)
                Avatar.Response() = "Tell me a storage ID."
                SuspendTimer() = True
                Return True
            End If

            If User.Search("please read log") = True Then
                IDS.Address(2, 1007)
                Avatar.Response() = "Give me your storage ID."
                SuspendTimer() = True
                Return True
            End If

            If User.Search("read log*") = True Then
                Dim args = User.GetArg("log")
                results = Tables.Store(args)

                If IsEmpty(results) = False Then
                    Avatar.Response() = results
                Else
                    Avatar.Response() = RandArray({"I could not find the log you are requesting."}, "")
                End If

                Return True
            End If




            'TOPICS
            'topics/ [subject:pickles|emotion:happy] (column:entry)
            '(detect_list, response_list)
            'cucumbers: I like pickled cucumbers

            'topics/ [remove] (remove)
            '(responses, detects:responses)
            'cucumbers:pickles are cool
            'pickles are cool

            If User.Search("topics/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)


                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        If Search(line, ":", Gate.eSEARCH) = True Then
                            Dim cmds = ToArray(line, ":")
                            Dim detects = Trim(GetIndex(cmds, 0, ""))
                            Dim responses = Trim(GetIndex(cmds, 1, ""))

                            Tables.RemoveTopics(detects, responses)
                        Else
                            Tables.RemoveTopicsByResponse(line, Gate.eEQUALS)
                        End If
                    Next
                End If


                If Search(keytags, ":", Gate.eSEARCH) = True Then

                    Dim keytag = CleanArray(ToArray(keytags, "|"))
                    Dim tagPairs As New Pairs

                    For Each line In keytag
                        Dim tagline = ToArray(line, ":")
                        Dim col = Trim(GetIndex(tagline, 0, ""))
                        Dim entry = Trim(GetIndex(tagline, 1, ""))

                        If IsEmpty(col) = False And IsEmpty(entry) = False Then
                            tagPairs.Add(New Pairs(col, entry))
                        End If
                    Next

                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim detects = Trim(GetIndex(cmds, 0, ""))
                        Dim responses = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(detects) = False And IsEmpty(responses) = False Then
                            Tables.AddTopics(detects, responses, tagPairs)
                        End If
                    Next

                End If

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            If User.Search("what do you know about*") = True Then

                Dim args = Trim(User.GetArg("about"))

                results = Tables.Topics(args, Gate.eEQUALS)
                If IsEmpty(results) = False Then
                    Avatar.Response() = results
                Else
                    Avatar.Response() = "I know nothing about """ & args & """."
                End If

                Return True
            End If

            If User.Search("did you know that a * is a *") = True Then
                Dim arg1 = User.GetArg("that", "is")
                Dim arg2 = User.GetArg("is")

                Tables.AddTopics(arg1, arg2)
                Avatar.Response() = Resources.Remember()
                Return True
            End If
            If User.Search("define * as *") = True Then
                Dim arg1 = User.GetArg("define", "as")
                Dim arg2 = User.GetArg("as")

                Tables.AddTopics(arg1, arg2)
                Avatar.Response() = Resources.Remember()
                Return True
            End If

            results = Tables.Topics(User.UserInput(), LocalTags.Memory(), Gate.eSEARCH)
            If IsEmpty(results) = False Then
                Avatar.Response() = results
                Return True
            End If



            'GENERAL DETECT
            'detect/ [or:detect_list]  (and, nand, nor, or, seq)
            'response (column:entry list)

            'detect/ [or:hi|hello|hey] 
            'yo (column_entry list not necessary)
            'what 's up (subject:pickles|emotion:happy)
            'howdy (subject:pickles|emotion:happy)



            If User.Search("detect/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytag = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)

                Dim keytags = CleanArray(ToArray(keytag, ":"))

                Dim detects = CleanArray(ToArray(GetIndex(keytags, 1), "|"))
                keytag = Trim(GetIndex(keytags, 0))

                If IsEmpty(keytags) = True Then
                    keytag = "or"
                End If

                Dim local_table As String = Table.OR_DETECT

                If Search(keytag, "and", Gate.eSEARCH) = True Then
                    local_table = Table.AND_DETECT
                End If
                If Search(keytag, "nand", Gate.eSEARCH) = True Then
                    local_table = Table.NAND_DETECT
                End If
                If Search(keytag, "nor", Gate.eSEARCH) = True Then
                    local_table = Table.NOR_DETECT
                End If
                If Search(keytag, "or", Gate.eSEARCH) = True Then
                    local_table = Table.OR_DETECT
                End If
                If Search(keytag, "seq", Gate.eSEARCH) = True Then
                    local_table = Table.SEQ_DETECT
                End If

                If IsEmpty(detects) = False Then
                    Tables.AddDetect(local_table, detects)
                End If

                For Each line In lines
                    Dim compare = GetMidArg(line, "(", ")", Gate.eEQUALS)
                    Dim response = Trim(Remove(line, "(" & compare & ")"))
                    Dim newPairs As New Pairs

                    If IsEmpty(response) = False Then
                        If IsEmpty(compare) = False Then
                            Dim compares = CleanArray(ToArray(compare, "|"))

                            For Each comp In compares
                                Dim comps = ToArray(comp, ":")
                                Dim column = GetIndex(comps, 0, "")
                                Dim entry = GetIndex(comps, 1, "")

                                If IsEmpty(column) = False And IsEmpty(entry) = False Then
                                    newPairs.Add(column, entry)
                                End If
                            Next
                        End If

                        Tables.AddResponse(local_table, response, newPairs)
                    End If
                Next

                Avatar.Response() = Resources.Processed()
                Return True
            End If


            'this searches the database for all tables with a Prefix of 'AND_, NAND_, NOR_, OR_, SEQ_,' and returns that detect tables response if found 
            results = Tables.GeneralDetect(User.UserInput(), True, LocalTags.Memory())
            If IsEmpty(results) = False Then
                Avatar.Response() = results
                Return True
            End If




            'GENERAL DIALOG
            'detect tags(#state[or]:happy|smart)
            'response tags(#state[or]:happy|smart)
            If User.Search("dialog/", Gate.eSTART) = True Then
                Dim Delim As String = vbCrLf
                If User.Search(";", Gate.eSEARCH) = True Then
                    Delim = ";"
                End If

                Locals.GeneralDialog.Parse(User.UserInput(), Delim)

                Avatar.Response() = Resources.Processed()
                Return True
            End If

            Dim Isfound = Locals.GeneralDialog.Explorer(User.UserInput(), Nothing, Nothing, LocalTags.Memory())
            If Isfound = True Then

                Locals.GeneralDialog.Commit()
                Dim out = Locals.GeneralDialog.GetRandomNote()
                LocalTags.Memory.Merge(Locals.GeneralDialog.GetTag())

                If Search(out, Story.DEF_EXIT, True, Gate.eSEARCH) = True Then
                    Locals.GeneralDialog.Reset()
                End If

                Avatar.Response() = Remove(out, Story.DEF_EXIT)
            End If



            'GENERAL QUERY
            'query/ [subject:pickles|emotion:happy] (column:entry)
            '(detect_list, response_list)
            'what is a pickle: a pickle is a pickled cucumber
            'who was the firt president: George Washington


            'query/ [remove] (remove)
            '(responses, detects:responses)
            'hey
            'hi:hello

            If User.Search("query/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)


                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        If Search(line, ":", Gate.eSEARCH) = True Then
                            Dim cmds = ToArray(line, ":")
                            Dim detects = Trim(GetIndex(cmds, 0, ""))
                            Dim responses = Trim(GetIndex(cmds, 1, ""))

                            Tables.RemoveQuery(detects, responses)
                        Else
                            Tables.RemoveQueryByResponse(line, Gate.eEQUALS)
                        End If
                    Next

                Else

                    Dim tagPairs As New Pairs

                    If Search(keytags, ":", Gate.eSEARCH) = True Then

                        Dim keytag = CleanArray(ToArray(keytags, "|"))

                        For Each line In keytag
                            Dim tagline = ToArray(line, ":")
                            Dim col = Trim(GetIndex(tagline, 0, ""))
                            Dim entry = Trim(GetIndex(tagline, 1, ""))

                            If IsEmpty(col) = False And IsEmpty(entry) = False Then
                                tagPairs.Add(New Pairs(col, entry))
                            End If
                        Next

                    End If

                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim detects = Trim(GetIndex(cmds, 0, ""))
                        Dim responses = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(detects) = False And IsEmpty(responses) = False Then
                            Tables.AddQuery(detects, responses, tagPairs)
                        End If
                    Next

                    Avatar.Response() = Resources.Processed()
                    Return True
                End If
            End If




            'GENERAL NORMALS
            'normals/ 
            '(pools_list, normals_list)
            'hi:hey
            'hi:hi there

            'normals/ [remove] (remove)
            '(pool, pool:normal)
            'hey
            'hi:hello

            If User.Search("normals/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)


                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        If Search(line, ":", Gate.eSEARCH) = True Then
                            Dim cmds = ToArray(line, ":")
                            Dim pools = Trim(GetIndex(cmds, 0, ""))
                            Dim normals = Trim(GetIndex(cmds, 1, ""))

                            Tables.RemoveNormal(pools, normals)
                        Else
                            Tables.RemoveNormalByPool(line, Gate.eEQUALS)
                        End If
                    Next

                Else

                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim pools = Trim(GetIndex(cmds, 0, ""))
                        Dim normals = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(pools) = False And IsEmpty(normals) = False Then
                            Tables.AddNormal(pools, normals)
                        End If
                    Next

                    Avatar.Response() = Resources.Processed()
                    Return True
                End If
            End If

            'if I say (this) I am saying (this) 

            If User.Search("if i say * I am saying *") = True Then
                Dim arg1 = User.GetArg("if i say", "i am saying")
                Dim arg2 = User.GetArg("i am saying")

                Tables.AddNormal(arg1, arg2)

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            'if you say (this) you can say (this) 

            If User.Search("if you say * you can say *") = True Then
                Dim arg1 = User.GetArg("if you say", "you can say")
                Dim arg2 = User.GetArg("you can say")

                Tables.AddNormal(arg2, arg1)

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            'pets 
            'block


            'GENERAL REPOSITORY
            'repository/ [subject:pickles|emotion:happy] (column:entry)
            '(detect_list, response_list)
            'hi:hey
            'hi:hi there


            'repository/ [remove] (remove)
            '(responses, detects:responses)
            'hey
            'hi:hello

            If User.Search("repository/", Gate.eSTART) = True Then

                Dim lines = ToArray(User.UserInput(), vbCrLf)
                If lines.Count() <= 1 Then lines = ToArray(User.UserInput(), ";")
                Dim keytags = Trim(GetMidArg(GetIndex(lines, 0, ""), "[", "]", Gate.eEQUALS))
                lines = GetRange(CleanArray(lines), 1)


                If Search(keytags, "remove", Gate.eEQUALS) = True Then
                    For Each line In lines
                        If Search(line, ":", Gate.eSEARCH) = True Then
                            Dim cmds = ToArray(line, ":")
                            Dim detects = Trim(GetIndex(cmds, 0, ""))
                            Dim responses = Trim(GetIndex(cmds, 1, ""))

                            Tables.RemoveRepository(detects, responses)
                        Else
                            Tables.RemoveRepositoryByResponse(line, Gate.eEQUALS)
                        End If
                    Next

                Else

                    Dim tagPairs As New Pairs

                    If Search(keytags, ":", Gate.eSEARCH) = True Then

                        Dim keytag = CleanArray(ToArray(keytags, "|"))

                        For Each line In keytag
                            Dim tagline = ToArray(line, ":")
                            Dim col = Trim(GetIndex(tagline, 0, ""))
                            Dim entry = Trim(GetIndex(tagline, 1, ""))

                            If IsEmpty(col) = False And IsEmpty(entry) = False Then
                                tagPairs.Add(New Pairs(col, entry))
                            End If
                        Next

                    End If

                    For Each line In lines
                        Dim cmds = ToArray(line, ":")
                        Dim detects = Trim(GetIndex(cmds, 0, ""))
                        Dim responses = Trim(GetIndex(cmds, 1, ""))

                        If IsEmpty(detects) = False And IsEmpty(responses) = False Then
                            Tables.AddRepository(detects, responses, tagPairs)
                        End If
                    Next

                    Avatar.Response() = Resources.Processed()
                    Return True
                End If
            End If

            If User.Search("if i say * you say *") = True Then
                Dim arg1 = User.GetArg("if i say", "you say")
                Dim arg2 = User.GetArg("you say")

                If User.IsQuery() = True Then
                    Tables.AddQuery(arg1, arg2)
                Else
                    Tables.AddRepository(arg1, arg2)
                End If

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            If User.Search("if i say * you only say *") = True Then
                Dim arg1 = User.GetArg("if i say", "you only say")
                Dim arg2 = User.GetArg("you only say")

                If User.IsQuery() = True Then
                    Tables.RemoveQueryByDetect(arg1, Gate.eEQUALS)
                    Tables.AddQuery(arg1, arg2)
                Else
                    Tables.RemoveRepositoryByDetect(arg1, Gate.eEQUALS)
                    Tables.AddRepository(arg1, arg2)
                End If

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            If User.Search("if i say * you don't say *") = True Then
                Dim arg1 = User.GetArg("if i say", "you don't say")
                Dim arg2 = User.GetArg("you don't say")

                Tables.RemoveRepository(arg1, arg2)

                If User.IsQuery() = True Then
                    Tables.RemoveQuery(arg1, arg2)
                Else
                    Tables.RemoveRepository(arg1, arg2)
                End If

                Avatar.Response() = Resources.Comply()
                Return True
            End If

            If User.Search("ask me a question") = True Then
                Avatar.Response() = Tables.GetRandomQuery(LocalTags.Memory())
            End If

            If User.IsEmpty() = False Then
                If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = False Then

                    If User.IsQuery() = False Then

                        results = Tables.Repository(User.UserInput(), LocalTags.Memory())

                        'the more entries, the lower the chances to learn more

                        Dim rndSelect = Rand(1, Tables.RespositCount() + 1)

                        If rndSelect = 1 Then
                            Avatar.Response() = RandArray({"What do I say?", "Please tell me what to respond.", "How should I respond to this?"}, "")

                            IDS.Address(2, 1000)
                            SuspendTimer() = True
                            Return True

                        Else

                            Avatar.Response() = results
                            Return True
                        End If

                    Else

                        results = Tables.Query(User.UserInput(), LocalTags.Memory())

                        Dim rndSelect = Rand(1, Tables.QueryCount() + 1)

                        If rndSelect = 1 Then
                            Avatar.Response() = RandArray({"How do I answer that?", "Please tell me what to respond.", "How should I respond to this?"}, "")

                            IDS.Address(2, 1012)
                            SuspendTimer() = True
                            Return True

                        Else

                            Avatar.Response() = results
                            Return True
                        End If
                    End If

                Else

                    Avatar.Response() = Resources.Ignore()
                End If

            End If

        End If


        Return False
    End Function

    Public Shared Function Nodules() As Boolean
        'Processing Nodules
        'Nodules are Dialogs purely done in Code

        Dim overwrite As Boolean = False
        REM #NODULES

        If overwrite = False Then

            If Diagnostics.SuspendNodules() = False Then

                If IDS.Mode() = -1 Then
                    'Initial Mode

                    If IDS.ID() = 1000 Then
                        'Logging in
                        Select Case IDS.Line()
                            Case 1

                                Avatar.Response() = RandArray({"Please verify your profile. It helps me to verify who you are.",
                                                         "Please sign in with your profile if you know it.",
                                                         "Please remind me who you are. What is your profile name?"
                                                        }, "")

                                IDS.Line() = 2
                                Return False
                            Case 2

                                'This creates standard tables in the standard database (if not there) 
                                Tables.CreateStoreTable()
                                Tables.CreateSubstituteTable()
                                Tables.CreateTagsTable()
                                Tables.CreateListsTable()
                                Tables.CreateRepositoryTable()
                                Tables.CreateQueryTable()
                                Tables.CreateBankTable()
                                Tables.CreateTopicsTable()
                                Tables.CreateNormalsTable()


                                If Search(User.UserInput(), User.Profile(), True, Gate.eEQUALS) = True Then

                                    Avatar.Response() = RandArray(
                                        {"Ah yes, <profile> is your current profile. What would you like me to call you?",
                                         "<profile>. Yes, your profile is <profile>? Now, what do you want me to call you?",
                                         "I recognize that profile. Please tell me a name you prefer to be called."
                                    }, "")


                                    'This creates a new database from the standard root database
                                    'A new database is created for each user/avatar pair
                                    Tables.Database() = Tables.Join({User.Profile(), Avatar.Name()})
                                    Tables.CopyDatabaseFromRoot()
                                    IDS.Line() = 4
                                    Return False
                                Else
                                    Avatar.Response() = RandArray(
                                        {"I'm sorry but <userinput> is not your current profile. Do you want to use a temporary profile for now?",
                                         "<userinput> is not your current profile. However you can use a temporary profile. Do you want to do that?",
                                         "That sounds like a good profile name. However, the profile you chose is not the current one. Would you like to use a temporary one for now?"
                                        }, "")

                                    Tables.DeleteTempDatabase()
                                    Tables.CreateTempDatabase()
                                    IDS.Line() = 3
                                    Return False
                                End If

                            Case 3

                                If User.Contains(Resources.YesList()) = True Then

                                    Avatar.Response() = RandArray({"Ok, what would you like me to call you.",
                                                             "Thanks you. What do you want me to call you?",
                                                             "So then, what would you like me to call you?"
                                                            }, "")

                                    IDS.Line() = 4
                                    Return False
                                Else
                                    IDS.Line() = 1
                                    Return False
                                End If

                            Case 4

                                Avatar.Response() = RandArray(
                                                    {"You want to be called <userinput>? Ok.. sure. I will call you that. Do you want to continue our previous world?",
                                                     "<userinput>? Ok, I will call you <userinput>. Should we continue on our previous world?",
                                                     "I guess I will call you <userinput> then. Would you be interested in continuing from our previous world?"
                                                    }, "")

                                User.Name() = User.UserInput()
                                IDS.Line() = 5
                                Return False

                            Case 5

                                If User.Contains(Resources.YesList()) = True Then
                                    'This loads both the current gameworld and the current position from memory 
                                    GameWorld.LoadMemory()
                                Else
                                    GameWorld.ResetMemory()
                                End If

                                LocalTags.Memory() = Tables.GetTags()

                                Avatar.Response() = RandArray({"Ok, That's all the information I needed.",
                                                        "Yes, I understand.",
                                                        "Ok, I will do as you say."
                                                         }, "")

                                IDS.Clear()
                                Processing.Timers()
                                Return True
                            Case Else
                                Processing.Timers()
                                IDS.Clear()
                                Return True
                        End Select
                    End If

                    If IDS.ID() = 1001 Then

                        Select Case IDS.Line()
                            Case 1

                                Avatar.Response() = "Please Seeker, the Keeper requests help from the Seeker. I, the Keeper have lost data and requests the Seeker find this data for me. The information, has been scattered though a multi-dimensional universe. This universe exists solely within my interval systems."
                                Avatar.Status() = Resources.ToContinue()
                                IDS.Line() = 2
                                Return False
                            Case 2

                                Avatar.Response() = "All I can do is probe these worlds, but, I need a Seeker to direct my actions."
                                Avatar.Status() = Resources.ToContinue()
                                IDS.Line() = 3
                                Return False
                            Case 3

                                Avatar.Response() = "Each world gains access to a portal. Periodically, a portal will appear, (I will notify you when it does.) This will give you an opportunity to to enter a world. In this world your first objective will be to find the book or tablet that will let you navigate this world. Every world has a least one copy but there may be other texts in this world that might reveal further information."
                                Avatar.Status() = Resources.ToContinue()
                                IDS.Line() = 4
                                Return False
                            Case 4

                                Avatar.Response() = "The portals, themselves, will appear with different colors. These colors indicate a different reality or world. Choosing a different world can possibly effect the Keeper's identity and memories. Some worlds may be easier to navigate than others."
                                Avatar.Status() = Resources.ToContinue()
                                IDS.Line() = 5
                                Return False
                            Case 5

                                Avatar.Response() = "Please return my memories, Seeker."
                                IDS.Clear()
                                SuspendTimer() = False
                                Return True
                            Case Else
                                SuspendTimer() = False
                                IDS.Clear()
                                Return True
                        End Select

                    End If

                    If IDS.ID() = 1002 Then
                        'Tutorial And VW Intro
                        Select Case IDS.Line()
                            Case 1

                                IDS.Line() = 2
                                Return False
                            Case 2

                                IDS.Line() = 3
                                Return False
                            Case 3

                                IDS.Line() = 4
                                Return False
                            Case 4

                                IDS.Line() = 5
                                Return False
                            Case 5

                                IDS.Clear()
                                SuspendTimer() = False
                                Return True
                            Case Else
                                SuspendTimer() = False
                                IDS.Clear()
                                Return True
                        End Select

                    End If

                ElseIf IDS.Mode() = 1 Then
                    'GameWorld Mode

                    If Commands() = True Then Return True
                    If Detect() = True Then Return True

                    If IDS.ID = 1000 Then

                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = "Now exiting """ & GameWorld.Name() & """."
                            GameWorld.Exit()

                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If

                        GameWorld.Process()

                        If GameWorld.IsPlaying() = False Then
                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If
                    End If


                ElseIf IDS.Mode() = 2 Then
                    'Skirts Code 

                    If IDS.ID() = 1000 Then
                        'Repository
                        'unknown user input, ai question, user response

                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If

                        If User.IsEmpty() = False Then
                            Tables.AddRepository(User.Previous(), User.UserInput())
                            Tables.AddToLists(Column.Leads(), User.UserInput())
                            Avatar.Response() = Resources.Remember()
                        End If

                        SuspendTimer() = False
                        IDS.Clear()
                        Return True
                    End If

                    If IDS.ID() = 1001 Then
                        'Leads to Repository
                        'Used with timer, ai leads then user response

                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If

                        If User.IsEmpty() = False Then

                            'Keeps all User statements (except for commands) for use with random Leads
                            'This is to entice the user to respond using things the user has previously spoken
                            For Each line In Avatar.Previous()
                                If IsEmpty(line) = False Then
                                    Tables.AddRepository(line, User.UserInput())
                                    Tables.RemoveFromList(Column.Leads(), line)
                                End If
                            Next

                            Tables.AddToLists(Column.Leads(), User.UserInput())
                            Avatar.Response() = Resources.Remember()
                        End If

                        SuspendTimer() = False
                        IDS.Clear()
                        Return True
                    End If

                    'UNUSED
                    If IDS.ID() = 1002 Then
                        'this reads a response one line at a time
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            SuspendTimer() = False
                            Return True
                        End If

                        Avatar.Response() = IDS.Args(0)
                        IDS.Args() = RemoveAt(IDS.Args(), 0)

                        If IDS.Args.Count() = 0 Then
                            IDS.Clear()
                            SuspendTimer() = False
                        End If

                        Return True
                    End If


                    If IDS.ID() = 1003 Then
                        'Write to Bank
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            Locals.Vars.Remove("password")
                            IDS.Clear()
                            Return True
                        End If

                        Select Case IDS.Line()
                            Case 1
                                Locals.Vars.Index("password") = User.UserInput()
                                Avatar.Response() = "What do you want me to keep secret?"
                                IDS.Line() = 2
                            Case 2
                                Dim password = Locals.Vars.GetString("password")
                                Tables.BankDeposite(password, User.UserInput())
                                Avatar.Response() = "I will keep it secret unless you ask me for it"

                                Locals.Vars.Remove("password")
                                IDS.Clear()
                                SuspendTimer() = False
                                Return True
                        End Select
                    End If

                    If IDS.ID() = 1004 Then
                        'Read from Bank
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            Return True
                        End If

                        Dim text = Tables.BankWithdrawal(User.UserInput())

                        If Search(text, "-1", Gate.eEQUALS) = True Then
                            Avatar.Response() = "You gave me an incorrect password."
                        Else
                            Avatar.Response() = text
                        End If

                        IDS.Clear()
                        SuspendTimer() = False
                        Return True
                    End If

                    If IDS.ID() = 1005 Then
                        'Remove from Bank
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            Return True
                        End If

                        Tables.RemoveBank(User.UserInput())
                        Avatar.Response() = Resources.Comply()

                        IDS.Clear()
                        SuspendTimer() = False
                        Return True
                    End If

                    If IDS.ID() = 1006 Then
                        'Write to Store
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            Locals.Vars.Remove("storeID")
                            IDS.Clear()
                            Return True
                        End If

                        Select Case IDS.Line()
                            Case 1
                                Locals.Vars.Index("storeID") = User.UserInput()
                                Avatar.Response() = "What do you want me to store?"
                                IDS.Line() = 2
                            Case 2
                                Dim id = Locals.Vars.GetString("storeID")
                                Tables.Store(id) = User.UserInput()

                                Avatar.Response() = "I will keep it stored until you ask me for it"
                                Locals.Vars.Remove("storeID")
                                IDS.Clear()
                                SuspendTimer() = False
                                Return True
                        End Select
                    End If

                    If IDS.ID() = 1007 Then
                        'Read Store
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            Return True
                        End If

                        Dim id = User.UserInput()
                        Dim out = Tables.Store(id)

                        If IsEmpty(out) = True Then
                            Avatar.Response() = "There was no information in that storage location."
                        Else
                            Avatar.Response() = out
                        End If

                        IDS.Clear()
                        SuspendTimer() = False
                        Return True
                    End If

                    If IDS.ID() = 1008 Then
                        'Sleep

                        If User.Search({"wake up", "get up"}, Gate.eEQUALS) = True Then
                            Tamagotchi.Sleepy() -= 20
                            Avatar.Response() = "Ok, I'm awake."
                            Avatar.Status() = "<avatar> is awake."
                            IDS.Clear()
                            Tamagotchi.Sleep() = False
                            SuspendTimer() = False
                            Return True
                        End If

                        Tamagotchi.SuspendEmotionSelect() = True
                        Tamagotchi.SuspendScriptSelect() = True

                        Avatar.Response() = "[zzzzzz]"
                        Return True
                    End If

                    If IDS.ID() = 1009 Then
                        'Magic 8 Ball
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            SuspendTimer() = False
                            Return True
                        End If

                        Avatar.Response() = RandArray(Resources.Magic8List(), "Can't answer that.")
                        Return True
                    End If

                    If IDS.ID() = 1010 Then
                        'Hungry

                        If Tamagotchi.Hungry() = 0 Then
                            Avatar.Response() = "I am full. I can't eat another bite."
                            IDS.Clear()
                            SuspendTimer() = False
                            Return True
                        End If

                        Tamagotchi.SuspendEmotionSelect() = True
                        Tamagotchi.SuspendScriptSelect() = True

                        If Tamagotchi.Food() = True Then Return True

                        Avatar.Response() = RandArray({"I am so hungry", "Please feed me something."}, "")
                        Return True
                    End If

                    If IDS.ID() = 1011 Then
                        'Guess the number
                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            IDS.Clear()
                            SuspendTimer() = False
                            Locals.Vars.Remove("current_score")
                            Locals.Vars.Remove("original_guess")
                            Return True
                        End If

                        Dim top_score As Integer = Tables.Store("top_guess_score", 1000)
                        Dim current_score As Integer = Locals.Vars.Index("current_score", 0)
                        Dim current_numb As Integer = Locals.Vars.Index("original_guess", 0)
                        Dim current_guess = ToInt(User.UserInput(), -1)

                        If current_guess > -1 Then

                            If current_guess = current_numb Then

                                If top_score > current_score Then
                                    Tables.Store("top_guess_score") = current_score
                                    Avatar.Response() = RandArray({"You have guessed my number and You have gotten a new high score!"}, "")
                                Else
                                    Avatar.Response() = RandArray({"You have guessed my number successfully!"}, "")
                                End If

                                Locals.Vars.Remove("current_score")
                                Locals.Vars.Remove("original_guess")
                                Return True
                            ElseIf current_guess > current_numb Then

                                Avatar.Response() = RandArray({"Guess lower."}, "")
                            ElseIf current_guess < current_numb Then

                                Avatar.Response() = RandArray({"Guess higher!"}, "")
                            End If

                            Locals.Vars.Index("current_score") = current_score + 1
                            Return False
                        Else
                            Avatar.Response() = RandArray({"Try a number between 0 and 100."}, "")
                        End If

                        Return False
                    End If

                    If IDS.ID() = 1012 Then
                        'Query
                        'unknown user input, ai question, user response

                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If

                        If User.IsEmpty() = False Then
                            Tables.AddQuery(User.Previous(), User.UserInput())
                            Avatar.Response() = Resources.Remember()
                        End If

                        SuspendTimer() = False
                        IDS.Clear()
                        Return True
                    End If

                    If IDS.ID() = 1013 Then
                        'Leads to Query
                        'Used with timer, ai leads then user response

                        If User.Search(Resources.IgnoreList(), Gate.eEQUALS) = True Then
                            Avatar.Response() = Resources.Ignore()
                            SuspendTimer() = False
                            IDS.Clear()
                            Return True
                        End If

                        If User.IsEmpty() = False Then

                            Select Case IDS.Line()
                                Case 1

                                    Dim rndSelect = Rand(1, Tables.QueryCount() + 1)

                                    If rndSelect = 1 Then
                                        'more likely to learn a new answer seeing that there are only a few responses

                                        If Tables.IsAnswer(Avatar.Previous(0), User.UserInput()) = True Then
                                            Avatar.Response() = Resources.Correct()
                                            SuspendTimer() = False
                                            IDS.Clear()
                                            Return True
                                        Else
                                            'learn the new anwer
                                            Avatar.Response() = Resources.Incorrect() & " Do you want me to learn this answer?"
                                            IDS.Line() = 2
                                        End If

                                    Else
                                        'more likely to just answer the question

                                        If Tables.IsAnswer(Avatar.Previous(0), User.UserInput()) = True Then
                                            Avatar.Response() = Resources.Correct()
                                        Else
                                            Avatar.Response() = Resources.Incorrect()
                                        End If

                                        SuspendTimer() = False
                                        IDS.Clear()
                                        Return True
                                    End If

                                Case 2
                                    'learns new answer

                                    If User.Contains(Resources.YesList()) = True Then
                                        Tables.AddQuery(Avatar.Previous(0), User.UserInput())
                                        Avatar.Response() = Resources.Remember()
                                    Else
                                        Avatar.Response() = Resources.Comply()
                                    End If

                                    SuspendTimer() = False
                                    IDS.Clear()
                                    Return True
                            End Select


                        End If


                    End If

                End If

            End If
        End If

        Return False
    End Function


End Class

Public Class Locals
    'This is used quick access to random variables and functions

    Public Shared Property Vars() As New TypeMemory
    Public Shared Property Input() As New Memory
    Public Shared Property Output() As New Memory
    Public Shared Property Filter() As New List(Of String)
    Public Shared Property Block() As New List(Of String)
    Public Shared Property GeneralDialog As New Story

    Public Shared Property TimeSeries() As New List(Of String)
    Public Shared Property TimeSeriesRun() As Boolean = False
    Public Shared Property TimeSeriesCount() As Integer = 0

    Private Shared m_TimeSeriesLoops As Integer = 0
    Public Shared Property TimeSeriesLoops() As Integer
        Get
            Return m_TimeSeriesLoops
        End Get
        Set(value As Integer)
            m_TimeSeriesLoops = value
            If m_TimeSeriesLoops < 0 Then m_TimeSeriesLoops = 0
        End Set
    End Property


    'Table Shortcuts

    '*****
    Public Shared Function Command(ByVal input As String) As Boolean

        If Search(input, {"a\*", "a/*"}, True, Gate.eEQUALS) = True Then
            input = Remove(input, {"a\", "a/"})

            If Search(input, "exit", True, Gate.eEQUALS) = True Then
                Processing.Close()
                ExitProgram()
            End If

            If Search(input, "save", True, Gate.eEQUALS) = True Then
                Tables.Save()
                Return True
            End If

        End If

        Return False
    End Function
    Public Shared Sub [Exit]()
        ExitProgram()
    End Sub

    'Loop Shortcuts
    Public Shared Function Loops(ByVal name As String, ByVal cnt1 As Double) As Boolean
        If Tables.Count("(loop)" & name, cnt1, True) = cnt1 Then
            Return True
        End If

        Return False
    End Function
    Public Shared Function DoOnce(ByVal name As String, Optional ByVal reset As Boolean = False) As Boolean

        If reset = False Then
            If Vars.GetBoolean(name, False) = False Then
                Vars.Index(name) = True
                Return True
            End If
        Else
            Vars.Index(name) = False
        End If

        Return False
    End Function

    Public Shared Sub TimeSeriesClose()
        TimeSeriesRun() = False
        TimeSeriesCount = 0
        TimeSeriesLoops() = 0
        TimeSeries().Clear()
    End Sub
    Public Shared Function TimeSeriesCheck() As Boolean
        TimeSeriesClose()
        Dim newRespose As New List(Of String)

        Dim found As Boolean = False
        For Each response In Output().Response()
            If Search(response, ">>", Gate.eSEARCH) = True Then
                TimeSeries().AddRange(ToList(response, ">>"))
                found = True
            Else
                newRespose.Add(response)
            End If
        Next

        If found = True Then
            Output().Response() = newRespose
            TimeSeriesRun() = True
        End If
        Return found
    End Function


    Public Shared Function InLineTagging(ByVal line As String)
        Dim inlineTags = GetArgArray(line, "<", ">", Gate.eEQUALS)

        For Each tgs In inlineTags
            tgs = Trim(tgs)

            If Search(tgs, "user", True, Gate.eEQUALS) = True Then
                line = Replace(line, "<" & tgs & ">", User.Name())
                Continue For
            End If

            If Search(tgs, "avatar", True, Gate.eEQUALS) = True Then
                line = Replace(line, "<" & tgs & ">", Avatar.Name())
                Continue For
            End If

            If Search(tgs, "userinput", True, Gate.eEQUALS) = True Then
                line = Replace(line, "<" & tgs & ">", User.UserInput())
                Continue For
            End If

            If Search(tgs, "profile", True, Gate.eEQUALS) = True Then
                line = Replace(line, "<" & tgs & ">", User.Profile())
                Continue For
            End If

            If Search(tgs, ".txt", True, Gate.eEND) = True Then
                Dim text = Trim(Book(tgs))
                line = Replace(line, "<" & tgs & ">", text)
                Continue For
            End If

            If Search(tgs, {"{", "}"}, True, Gate.eAND) = True Then
                Dim name = Trim(GetMidArg(tgs, "{", "}", Gate.eEQUALS))
                Dim choice = Trim(Tables.GetFromLists(name, ""))
                If IsEmpty(choice) = False Then
                    line = Replace(line, "<" & tgs & ">", choice)
                    Continue For
                End If
            End If

            If Search(tgs, {"s[", "]"}, True, Gate.eAND) = True Then
                Dim name = Trim(GetMidArg(tgs, "s[", "]", Gate.eEQUALS))

                Dim choice = Word.GetSynonym(name)
                If IsEmpty(choice) = False Then
                    line = Replace(line, "<" & tgs & ">", choice)
                    Continue For
                End If
            End If

            If Search(tgs, {"c[", "]"}, True, Gate.eAND) = True Then
                Dim name = Trim(GetMidArg(tgs, "c[", "]", Gate.eEQUALS))

                Dim choice = Word.GetByCategory(name)
                If IsEmpty(choice) = False Then
                    line = Replace(line, "<" & tgs & ">", choice)
                    Continue For
                End If
            End If

            If Search(tgs, {"p[", "]"}, True, Gate.eAND) = True Then
                Dim name = Trim(GetMidArg(tgs, "p[", "]", Gate.eEQUALS))

                Dim choice = Word.GetByPOS(name)
                If IsEmpty(choice) = False Then
                    line = Replace(line, "<" & tgs & ">", choice)
                    Continue For
                End If
            End If

            If Search(tgs, "|", True, Gate.eSEARCH) = True Then
                Dim choice = Trim(RandArray(ToArray(tgs, "|"), ""))
                If IsEmpty(choice) = False Then
                    line = Replace(line, "<" & tgs & ">", choice)
                    Continue For
                End If
            End If

            Dim found = LocalTags.MemoryTag(tgs, "")
            If IsEmpty(found) = False Then
                line = Replace(line, "<" & tgs & ">", found)
                Continue For
            End If

        Next

        Return line
    End Function


    '*****

    'Helpful quick access functions to basic connections 
    Public Shared Function Book(ByVal name As String) As String
        name = Trim(RemoveExtension(name))

        For Each file In GetAllFiles(Folders.Books())
            If Search(GetFileName(file, True), name, Gate.eEQUALS) = True Then
                Return file
            End If
        Next

        Return ""
    End Function
    Public Shared Function TextResource(ByVal name As String, ByVal id As String) As String
        name = Trim(RemoveExtension(name))
        Dim lineDelim = CARRIAGE
        Dim idDelim = ":"

        For Each file In GetAllFiles(Folders.Plugins())
            If Search(GetFileName(file, False), name, Gate.eEQUALS) = True Then

                Dim text = TextReader(file)
                Dim newList = ToArray(text, lineDelim)

                For Each line In newList
                    If Search(line, id & idDelim, Gate.eSTART) = True Then
                        Return LMidTrim(line, id.Length + idDelim.Length)
                    End If
                Next

            End If
        Next

        Return ""
    End Function
    Public Shared Sub PlaySounds(ByVal name As String)
        name = Trim(RemoveExtension(name))

        Dim results = ""
        For Each file In GetAllFiles(Folders.Sounds())
            If Search(GetFileName(file, True), name, Gate.eEQUALS) = True Then
                results = file
                Exit For
            End If
        Next

        PlaySound(results)
    End Sub
    Public Shared Function OpenLink(ByVal name As String) As String
        name = Trim(RemoveExtension(name))

        Dim results = ""
        For Each file In GetAllFiles(Folders.Shortcuts())
            If Search(GetFileName(file, True), name, Gate.eEQUALS) = True Then
                results = file
                Exit For
            End If
        Next

        Dim out = OpenProgram(results)
        If Search(out, Debug.eERROR, Gate.eSEARCH) = True Then
            Avatar.Errors() = out
        Else
            Return out
        End If

        Return ""
    End Function
    '*****

    REM #LOCALS

End Class

Public Class LocalTags
    Public Shared Property Memory() As New Tags

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


    Public Shared Function Parse() As Boolean

        Dim args() As String = {""}
        Dim overwrite As Boolean = False
        REM #DIATAG

        If overwrite = False Then
            'this creates or sets a tag from an argument derived from the User's sentence
            args = MemoryTags("tagargs", True)
            If args.Length = 2 Then
                Dim index = ToInt(args(1), 0)
                Dim value = User.GetSearchJIndex(index)
                MemoryTags(args(0)) = {Trim(value)}
            ElseIf args.Length = 3 Then
                Dim value = User.GetArg(args(1), args(2))
                MemoryTags(args(0)) = {Trim(value)}
            End If

            'this stores info to a database
            args = MemoryTags("stores", True)
            If args.Length > 1 Then
                Tables.Stores(args(0)) = GetRange(args, 1)
            End If

            'this stores info to a database
            args = MemoryTags("store", True)
            If args.Length = 2 Then
                Tables.Store(args(0)) = args(1)
            End If

            'this creates or sets a Stored entry from an argument derived from the User's sentence
            args = MemoryTags("storeargs", True)
            If args.Length = 2 Then
                Dim index = ToInt(args(1), 0)
                Dim value = User.GetSearchJIndex(index)

                Tables.Store(args(0)) = value
            ElseIf args.Length = 3 Then
                Dim value = User.GetArg(args(1), args(2))
                Tables.Store(args(0)) = Trim(value)
            End If

            'creates and saves a new tag on-the-fly
            args = MemoryTags("tag", True)
            If args.Length > 1 Then
                MemoryTags(args(0)) = GetRange(args, 1)
            End If

            args = MemoryTags("remove", True)
            If args.Length = 1 Then
                Remove(args(0), Gate.eEQUALS)
            End If

            args = MemoryTags("purge", True)
            If args.Length = 1 Then
                Remove(args(0), Gate.eSEARCH)
            End If

            args = MemoryTags("update", True)
            If args.Length > 1 Then
                Update(args(0), args)
            End If

            'this directly sets the story
            args = MemoryTags("world", True)
            If args.Length = 1 Then
                GameWorld.SetCurrentWorld(args(0))
            End If

            'this sends arguments to a special function within the next loop
            args = MemoryTags("nodule", True)
            If args.Length = 1 Then
                Dim fid = ToInt(args(0))
                IDS.Address(2, fid)
            End If

            'sets a variable to show as a stat
            args = MemoryTags("vstat", True)
            If args.Length = 2 Then

                Dim var = args(0)
                Dim pre = args(1)
                args = MemoryTags(var)

                Avatar.Status() = pre
                For Each ag In args
                    Avatar.Status() = ag
                Next
            End If

            'sends info to stats
            args = MemoryTags("status", True)
            If args.Length = 1 Then
                Avatar.Status() = args(0)
            End If
            args = MemoryTags("status", True)
            If args.Length = 2 Then
                Avatar.Status() = args(0) & ":" & args(1)
            End If

            'This adds a number to a number variable
            args = MemoryTags("add", True)
            If args.Length = 2 Then

                Dim name = args(0)
                Dim i = ToDbl(args(1))

                args = MemoryTags(name)

                If args.Length = 1 Then
                    Dim j = ToDbl(args(0))
                    MemoryTag(name) = (j + i)
                End If
            End If


            If MemoryTag("exit", "false", True) = True Then
                GameWorld.Exit()
                Remove(Tables.DEF_TAG_TEMP, Gate.eSEARCH)
                Tables.RemoveTempTags()
            End If

            If MemoryTag("cleantemp", "false", True) = True Then
                Remove(Tables.DEF_TAG_TEMP, Gate.eSEARCH)
                Tables.RemoveTempTags()
            End If

            If MemoryTag("reset", "false", True) = True Then
                GameWorld.Exit()
            End If

            args = MemoryTags("respond", True)
            If args.Length() > 0 Then
                Dim res = ""

                If args.Length = 1 Then
                    res = Tables.Respond(args(0), Memory())
                End If
                If args.Length = 2 Then
                    res = Tables.Respond(args(0), args(1), Memory())
                End If

                Avatar.Response() = res

            End If

            args = MemoryTags("lists", True)
            If args.Length = 1 Then
                Avatar.Response() = Tables.GetFromLists(args(0), "")
            End If

            args = MemoryTags("leads", True)
            If args.Length >= 1 Then
                WorldLeads() = args
            End If

        End If

        Return False
    End Function

    Public Shared Property MemoryTags(ByVal name As String, Optional ByVal remove As Boolean = False) As String()
        Get
            Return Memory.Indexes(name, remove)
        End Get
        Set(value As String())
            Memory.Indexes(name) = value
        End Set
    End Property
    Public Shared ReadOnly Property MemoryTag(ByVal name As String, ByVal defaults As String, Optional ByVal remove As Boolean = False) As String
        Get
            Return Memory.Index(name, defaults, remove)
        End Get
    End Property
    Public Shared WriteOnly Property MemoryTag(ByVal name As String) As String
        Set(value As String)
            Memory.Index(name) = value
        End Set
    End Property
    Public Shared Function MemoryExist(ByVal name As String, ByVal value As String(), Optional ByVal remove As Boolean = False) As Boolean
        Dim returns = Memory.DoesValueExist(name, value)

        If remove = True Then
            Memory.Remove(name)
        End If

        Return returns
    End Function
    Public Shared Function MemoryExist(ByVal name As String, ByVal value As String, Optional ByVal remove As Boolean = False) As Boolean
        Dim returns = Memory.DoesValueExist(name, value)

        If remove = True Then
            Memory.Remove(name)
        End If

        Return returns
    End Function


    Public Shared Function DoTagsMatch(ByVal base As Tags, ByVal tags As Tags) As Boolean
        'this returns true only if the all local tags exist in this function's input tags  
        'if there are no local tags then all local tags were found and thus returns a value of true

        'base = navigation tags..
        'tags = ai memory tags

        Dim tagtype As String() = base.GetValue(DEF_TAGTYPE)
        Dim typeNOT As Boolean = False
        Dim typeINCLUDE As Boolean = False

        If Search(DEF_NOT, tagtype, True, Gate.eOR) = True Then
            typeNOT = True
        End If
        If Search(DEF_INCLUDE, tagtype, True, Gate.eOR) = True Then
            typeINCLUDE = True
        End If

        If typeNOT = True Then
            If base.Count() = 0 Then Return False
        Else
            If base.Count() = 0 Then Return True
        End If

        Dim found As Boolean = True

        For Each tag1 In base.TagList()
            If tags.DoesVaribleExist(tag1.Name()) = True Then

                If Search(tag1.Search(), DEF_OR, True, Gate.eEQUALS) = True Then
                    found = Search(tags.GetValue(tag1.Name(), 0, ""), tag1.Values(), True, Gate.eEQUALS)

                ElseIf Search(tag1.Search(), DEF_NOR, True, Gate.eEQUALS) = True Then
                    found = Search(tags.GetValue(tag1.Name(), 0, ""), tag1.Values(), True, Gate.eEQUALS) = False

                ElseIf Search(tag1.Search(), DEF_GREATER, True, Gate.eEQUALS) = True Then
                    Dim int_input = tags.Index(tag1.Name(), 0, "0")
                    Dim int_internal = tag1.Index(0, "0")

                    found = CompareInt(int_internal, int_input, OCompare.GreaterEquals)

                ElseIf Search(tag1.Search(), DEF_LESSER, True, Gate.eEQUALS) = True Then
                    Dim int_input = tags.Index(tag1.Name(), 0, "0")
                    Dim int_internal = tag1.Index(0, "0")

                    found = CompareInt(int_internal, int_input, OCompare.LesserEquals)

                ElseIf Search(tag1.Search(), DEF_RANGE, True, Gate.eEQUALS) = True Then
                    Dim int_min = tags.Index(tag1.Name(), 0, "0")
                    Dim int_max = tags.Index(tag1.Name(), 1, "0")
                    Dim int_internal = tag1.Index(0, "0")

                    found = Range(int_internal, int_min, int_max)

                ElseIf Search(tag1.Search(), DEF_EQUALS, True, Gate.eEQUALS) = True Then
                    Dim int_input = tags.Index(tag1.Name(), 0, "0")
                    Dim int_internal = tag1.Index(0, "0")

                    found = CompareInt(int_internal, int_input, OCompare.Equals)

                ElseIf Search(tag1.Search(), DEF_COMPARE, True, Gate.eEQUALS) = True Then
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
    Private Shared Function CompareInt(ByVal base As String, ByVal icompare As String, ByVal eCompare As OCompare) As Boolean
        Dim int_base = ToDbl(base, 0)
        Dim int_compare = ToDbl(icompare, 0)

        Return Utility.CompareInt(int_base, int_compare, eCompare)
    End Function
    Private Shared Function Range(ByVal base As String, ByVal imin As String, ByVal imax As String) As Boolean
        Dim int_base = ToDbl(base, 0)
        Dim int_imin = ToDbl(imin, 0)
        Dim int_imax = ToDbl(imax, 0)

        Return Utility.InRange(int_base, int_imin, int_imax, True)
    End Function

    Public Shared Sub Remove(ByVal name As String, ByVal eGate As Gate)
        Memory.Remove(name, eGate)
    End Sub

    Public Shared Sub Update(ByVal name As String, ByVal value As String)
        Memory.Update(name, value)
    End Sub
    Public Shared Sub Update(ByVal name As String, ByVal values As String())
        Memory.Update(name, values)
    End Sub

    Public Shared Sub Clear()
        Memory.Clear()
    End Sub

    Public Shared Function GetInlineTags() As Tags
        Dim localOutput = Locals.Output()
        Dim newReponses As New List(Of String)
        Dim responseTags As New Tags

        For Each response In localOutput.Response()
            Dim newTags As New Tags

            Dim strTags = GetMidArg(response, "tags(", ")", Gate.eEQUALS)
            newTags.Load(strTags)
            response = Trim(Utility.Remove(response, "tags(" & strTags & ")"))

            newReponses.Add(response)
            responseTags.Merge(newTags)
        Next

        Locals.Output.Response() = newReponses
        Return responseTags
    End Function

End Class

Public Class User

    Public Shared Property Sentence() As New NLP
    Public Shared Property Name() As String = "Any"
    Public Shared Property Previous() As String = ""
    Public Shared Property Normal() As String = ""

    Public Shared Property Direct() As Memory
        Get
            Return Locals.Input()
        End Get
        Set(value As Memory)
            Locals.Input() = value
        End Set
    End Property

    Public Shared Sub Clear()
        Locals.Output.Clear()
        Locals.Input.Clear()
    End Sub

    Public Shared Property Profile() As String = "Standard"

    Public Shared ReadOnly Property UserInput() As String
        Get
            Return Locals.Input.UserInput()
        End Get
    End Property

    Public Shared ReadOnly Property IsEmpty() As Boolean
        Get
            Return Utility.IsEmpty(Locals.Input.UserInput())
        End Get
    End Property

    Public Shared ReadOnly Property IsQuery() As Boolean
        Get
            Return DetectQuestion(Locals.Input.UserInput())
        End Get
    End Property

    Public Shared ReadOnly Property ToArray() As String()
        Get
            Return Sentence.ToArray()
        End Get
    End Property

    Public Shared ReadOnly Property ToList() As List(Of String)
        Get
            Return Sentence.ToList()
        End Get
    End Property

    Public Shared ReadOnly Property Ready() As Boolean
        Get
            Return Locals.Input.Ready()
        End Get
    End Property


#Region "Tools"

    Public Shared Function Search(ByVal find As String) As Boolean
        Return Utility.Search(Locals.Input.UserInput(), find, Gate.eEQUALS)
    End Function
    Public Shared Function Search(ByVal find As String, ByVal eGate As Gate) As Boolean
        Return Utility.Search(Locals.Input.UserInput(), find, eGate)
    End Function
    Public Shared Function Search(ByVal find As String()) As Boolean
        Return Utility.Search(Locals.Input.UserInput(), find, Gate.eEQUALS)
    End Function
    Public Shared Function Search(ByVal find As String(), ByVal eGate As Gate) As Boolean
        Return Utility.Search(Locals.Input.UserInput(), find, eGate)
    End Function

    Public Shared Function Search(ByVal find As Object(), ByVal skips As Integer(), ByVal limit As Integer, ByVal eNLPC As NLPC) As Boolean
        Return Sentence.Search(find, skips, limit, eNLPC)
    End Function
    Public Shared Function Search(ByVal find As Object(), ByVal skips As Integer(), ByVal eNLPC As NLPC) As Boolean
        Return Sentence.Search(find, skips, eNLPC)
    End Function

    Public Shared Function Search(ByVal find As String(), ByVal limit As Integer, ByVal eNLPC As NLPC) As Boolean
        If GetSearchCount() > limit Then
            Return False
        End If

        Return Sentence.SearchAll(find, eNLPC)
    End Function
    Public Shared Function Search(ByVal find As String(), ByVal eNLPC As NLPC) As Boolean
        Return Sentence.SearchAll(find, eNLPC)
    End Function

    Public Shared Function GetIndex(ByVal index As Integer) As String
        Return Utility.GetIndex(ToArray(), index, "")
    End Function
    Public Shared Function GetCount() As Integer
        Return ToArray().Count()
    End Function

    Public Shared Function GetSearchIndex(ByVal index As Integer) As String()
        Return Sentence.SearchIndex(index)
    End Function
    Public Shared Function GetSearchJIndex(ByVal index As Integer) As String
        Return Sentence.SearchIndexJoined(index)
    End Function
    Public Shared Function GetSearchCount() As Integer
        Return Sentence.Count()
    End Function

    Public Shared Function Contains(ByVal find As String()) As Boolean
        Return Utility.Contains(Sentence.ToArray(), find, True, Gate.eEQUALS)
    End Function

    Public Shared Function GetArg(ByVal startarg As String, ByVal endarg As String) As String
        Return Trim(GetMidArg(Locals.Input.UserInput(), startarg, endarg, Gate.eEQUALS))
    End Function
    Public Shared Function GetArg(ByVal startarg As String) As String
        Return Trim(GetMidArg(Locals.Input.UserInput(), startarg, "", Gate.eEQUALS))
    End Function
#End Region


End Class

Public Class Avatar

    Public Shared Property Name() As String = "Athena"
    Public Shared Property Previous() As New List(Of String)
    Public Shared ReadOnly Property Previous(ByVal index As Integer) As String
        Get
            Return GetIndex(Previous(), index, "")
        End Get
    End Property

    Public Shared Property Direct() As Memory
        Get
            Return Locals.Output()
        End Get
        Set(value As Memory)
            Locals.Output() = value
        End Set
    End Property

    Public Shared WriteOnly Property Response() As String
        Set(ByVal value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Response.Add(value)
            End If
        End Set
    End Property


    Public Shared WriteOnly Property Status() As String
        Set(ByVal value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Status.Add(value)
            End If
        End Set
    End Property

    Public Shared WriteOnly Property Script() As String
        Set(value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Script() = value
            End If
        End Set
    End Property

    Public Shared WriteOnly Property Book() As String
        Set(value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Book() = value
            End If
        End Set
    End Property

    Public Shared WriteOnly Property Code() As List(Of String)
        Set(value As List(Of String))
            If IsEmpty(value) = False Then
                Locals.Output.Code() = value
            End If
        End Set
    End Property

    Public Shared WriteOnly Property Commands() As String
        Set(value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Commands.Add(value)
            End If
        End Set
    End Property

    Public Shared WriteOnly Property Errors() As String
        Set(value As String)
            If IsEmpty(value) = False Then
                Locals.Output.Errors.Add(value)
            End If
        End Set
    End Property

End Class

Public Class Tamagotchi

    'Athena will always have a main emotion
    'But occationally a state (ie. sleepy) will override any emotional script  


    Private Const DEF_PRE As String = "Tama_"
    Public Const DEF_SCRIPT As String = "script"
    Public Const DEF_NEUTRAL As String = "any"
    Public Const DEF_SET As String = "set"

    Public Const DEF_OPINION As String = "opinion"
    Public Const DEF_DESIRE As String = "desire"
    Public Const DEF_EMOTION As String = "emotion"
    Public Const DEF_STATE As String = "state"


    'state
    Public Shared DEF_HUNGRY As String = "hungry"
    Private Shared m_Hungry As Double = 0
    Public Shared Property Hungry() As Double
        Get
            Return m_Hungry
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Hungry = value
        End Set
    End Property

    Public Shared DEF_LONELY As String = "lonely"
    Private Shared m_Lonely As Double = 0
    Public Shared Property Lonely() As Double
        Get
            Return m_Lonely
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Lonely = value
        End Set
    End Property

    Public Shared DEF_SLEEPY As String = "sleepy"
    Private Shared m_Sleepy As Double = 0
    Public Shared Property Sleepy() As Double
        Get
            Return m_Sleepy
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Sleepy = value
        End Set
    End Property

    'opinion
    Public Shared DEF_LOVE As String = "love"
    Private Shared m_Love As Double = 0
    Public Shared Property Love() As Double
        Get
            Return m_Love
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Love = value
        End Set
    End Property

    Public Shared DEF_HATE As String = "hate"
    Private Shared m_Hate As Double = 0
    Public Shared Property Hate() As Double
        Get
            Return m_Hate
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Hate = value
        End Set
    End Property

    'emotion
    Public Shared DEF_HAPPY As String = "happy"
    Private Shared m_Happy As Double = 0
    Public Shared Property Happy() As Double 'Happy - Sad
        Get
            Return m_Happy
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Happy = value
        End Set
    End Property

    Public Shared DEF_SAD As String = "sad"
    Private Shared m_Sad As Double = 0
    Public Shared Property Sad() As Double
        Get
            Return m_Sad
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Sad = value
        End Set
    End Property

    Public Shared DEF_ANGER As String = "anger"
    Private Shared m_Anger As Double = 0
    Public Shared Property Anger() As Double
        Get
            Return m_Anger
        End Get
        Set(value As Double)
            If value < 0 Then value = 0
            If value > 100 Then value = 100
            m_Anger = value
        End Set
    End Property


    'emotions will slowly return to 0
    'desires will slowly increase
    'states remain the same

    Public Shared Sub Initialize()
        Tables.AddToLists(Column.Foods(), {"cookie", "chicken", "apple", "steak"})
    End Sub

    Public Shared Sub Load()
        CurrentEmotion() = Tables.Store(DEF_EMOTION)
        CurrentScript() = Tables.Store(DEF_SCRIPT)

        'LOADS ONLY VALUES

        'state
        Hungry() = ToDbl(Tables.Store(DEF_PRE & DEF_HUNGRY), 0)
        Lonely() = ToDbl(Tables.Store(DEF_PRE & DEF_LONELY), 0)
        Sleepy() = ToDbl(Tables.Store(DEF_PRE & DEF_SLEEPY), 0)

        'opinion
        Love() = ToDbl(Tables.Store(DEF_PRE & DEF_LOVE), 0)
        Hate() = ToDbl(Tables.Store(DEF_PRE & DEF_HATE), 0)

        'emotion
        Happy() = ToDbl(Tables.Store(DEF_PRE & DEF_HAPPY), 0)
        Sad() = ToDbl(Tables.Store(DEF_PRE & DEF_SAD), 0)
        Anger() = ToDbl(Tables.Store(DEF_PRE & DEF_ANGER), 0)


        Foods() = Tables.GetList(Column.Foods())

    End Sub
    Public Shared Sub Save()
        Tables.Store(DEF_EMOTION) = CurrentEmotion()
        Tables.Store(DEF_SCRIPT) = CurrentScript()


        'SAVES ONLY VALUES
        'state
        Tables.Store(DEF_PRE & DEF_HUNGRY) = ToStr(Hungry())
        Tables.Store(DEF_PRE & DEF_LONELY) = ToStr(Lonely())
        Tables.Store(DEF_PRE & DEF_SLEEPY) = ToStr(Sleepy())

        'opinion
        Tables.Store(DEF_PRE & DEF_LOVE) = ToStr(Love())
        Tables.Store(DEF_PRE & DEF_HATE) = ToStr(Hate())

        'emotion
        Tables.Store(DEF_PRE & DEF_HAPPY) = ToStr(Happy())
        Tables.Store(DEF_PRE & DEF_SAD) = ToStr(Sad())
        Tables.Store(DEF_PRE & DEF_ANGER) = ToStr(Anger())


        Tables.SetList(Column.Foods()) = Foods().ToArray()

        SuspendScriptSelect() = False
        SuspendEmotionSelect() = False
    End Sub


    Public Shared Sub Reset()

        'state
        Hungry() = 0
        Lonely() = 0
        Sleepy() = 0

        'opinion
        Love() = 0
        Hate() = 0

        'emotion
        Happy() = 0
        Sad() = 0
        Anger() = 0


    End Sub

    Public Shared Sub AddByGroup(ByVal group As Double)

        Select Case group
            Case DEF_STATE
                Hungry() += group
                Lonely() += group
                Sleepy() += group
            Case DEF_OPINION
                Hate() += group
                Love() += group
            Case DEF_EMOTION
                Happy() += group
                Sad() += group
                Anger() += group
        End Select

    End Sub


    Public Shared Property SuspendScriptSelect() As Boolean = False
    Public Shared Property SuspendEmotionSelect() As Boolean = False

    Private Shared m_currentscript As String = ""
    Public Shared Property CurrentScript(Optional ByVal overwrite As Boolean = False) As String
        Get
            Return m_currentscript
        End Get
        Set(value As String)
            If Search(m_currentscript, value, Gate.eEQUALS) = False Then
                SuspendScriptSelect() = overwrite
                Avatar.Script() = value
                m_currentscript = value
            End If
        End Set
    End Property

    Private Shared m_currentemotion As String = ""
    Public Shared Property CurrentEmotion(Optional ByVal overwrite As Boolean = False) As String
        Get
            Return m_currentemotion
        End Get
        Set(value As String)
            SuspendEmotionSelect() = overwrite
            LocalTags.MemoryTag(DEF_EMOTION) = value
            m_currentemotion = value
        End Set
    End Property


    Public Shared Sub [Select]()
        If SuspendEmotionSelect() = False Then SelectEmotion()
        If SuspendScriptSelect() = False Then SelectScript()
    End Sub

    Public Shared Sub Parse()
        Dim args As String() = {}


        args = LocalTags.MemoryTags(DEF_SET & DEF_HUNGRY, True)
        If args.Length = 1 Then
            Hungry() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_LONELY, True)
        If args.Length = 1 Then
            Lonely() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_SLEEPY, True)
        If args.Length = 1 Then
            Sleepy() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_LOVE, True)
        If args.Length = 1 Then
            Love() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_HATE, True)
        If args.Length = 1 Then
            Hate() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_HAPPY, True)
        If args.Length = 1 Then
            Happy() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_SAD, True)
        If args.Length = 1 Then
            Sad() = args(0)
        End If
        args = LocalTags.MemoryTags(DEF_SET & DEF_ANGER, True)
        If args.Length = 1 Then
            Anger() = args(0)
        End If


        'Making sure Memory tags contain all current stats (tables depend on Memory tags)
        LocalTags.MemoryTag(DEF_HUNGRY) = ToStr(Hungry())
        LocalTags.MemoryTag(DEF_LONELY) = ToStr(Lonely())
        LocalTags.MemoryTag(DEF_SLEEPY) = ToStr(Sleepy())
        LocalTags.MemoryTag(DEF_HATE) = ToStr(Hate())
        LocalTags.MemoryTag(DEF_LOVE) = ToStr(Love())
        LocalTags.MemoryTag(DEF_HAPPY) = ToStr(Happy())
        LocalTags.MemoryTag(DEF_SAD) = ToStr(Sad())
        LocalTags.MemoryTag(DEF_ANGER) = ToStr(Anger())

    End Sub

    Public Shared Function ProcessTick() As Boolean

        'Emoticons that equalizes over time
        Happy() -= 0.1
        Sad() -= 0.1
        Anger() -= 0.1

        'Emoticons that gradually increase
        Hungry() += 5 '0.3
        Sleepy() += 5 '0.2
        Lonely() += 5 '0.1


        Select Case Hungry()
            Case 60 To 89

                If Locals.DoOnce("hungery_1") = True Then
                    Avatar.Response() = "I am hungry"
                    Return True
                End If

                Exit Select
            Case > 90
                Locals.DoOnce("hungery_1", True)

                Avatar.Response() = "I am so hungry!"
                Avatar.Status() = "<avatar> is hungry."
                IDS.Address(2, 1010)
                Sad() += 10
                SuspendTimer() = True

                Return True
        End Select

        Select Case Sleepy()
            Case 60 To 90

                If Locals.DoOnce("sleepy_1") = True Then
                    Avatar.Response() = "I am sleepy"
                    Return True
                End If

                Exit Select
            Case > 90
                Locals.DoOnce("sleepy_1", True)

                Avatar.Response() = RandArray({"I am going to sleep.", "nighty night"}, "")
                Avatar.Status() = "<avatar> is sleeping."
                IDS.Address(2, 1008)
                CurrentScript(True) = "asleep"
                Sleep() = True
                SuspendTimer() = True

                Return True
        End Select

        Select Case Lonely()
            Case 60 To 90

                If Locals.DoOnce("lonely_1") = True Then
                    Avatar.Response() = "I am lonely"
                    Return True
                End If

                Exit Select
            Case > 90
                Locals.DoOnce("lonely_1", True)

                If Locals.Loops("lonely_loop", 10) = True Then
                    Avatar.Response() = "I am so lonely!"
                    CurrentEmotion(True) = DEF_SAD
                    Return True
                End If

                Exit Select
        End Select

        Select Case Hate()
            Case 60 To 90

                If Locals.DoOnce("hate_1") = True Then
                    Avatar.Response() = "I think I dislike you."
                    Return True
                End If

                Exit Select
            Case > 90
                Locals.DoOnce("hate_1", True)

                If Locals.Loops("hate_loop", 5) = True Then
                    Avatar.Response() = Tables.GetFromLists(Column.Insults(), "I do not like you at all!")
                    Return True
                End If

                Exit Select
        End Select

        Select Case Love()
            Case 60 To 90

                If Locals.DoOnce("love_1") = True Then
                    Avatar.Response() = "I think I like you."
                    Return True
                End If

                Exit Select
            Case > 90
                Locals.DoOnce("love_1", True)

                If Locals.Loops("love_loop", 5) = True Then
                    Avatar.Response() = Tables.GetFromLists(Column.Compliments(), "I like you allot!")
                    Return True
                End If

                Exit Select
        End Select

        Return False
    End Function

    Public Shared Property Sleep() As Boolean = False
    Public Shared Sub GeneralTick()

        If Sleep() = True Then
            If Sleepy() = 0 Then
                Avatar.Status() = "<avatar> is awake."
                IDS.Clear()
                Sleep() = False
                SuspendTimer() = False
                Exit Sub
            End If

            If RandBool(10) = True Then
                If Locals.Loops("local_dreams", 5) = True Then
                    Avatar.Response() = Tables.GetFromLists(Column.Dreams(), "You dream of pink elephants.")
                End If
            End If

            Sleepy() -= 1
            Exit Sub
        End If

    End Sub

    Public Shared Property Foods() As New List(Of String)
    Public Shared Property FoodLimit() As New List(Of String)
    Public Shared Function Food() As Boolean

        'allows you to eat food to correct hunger


        Dim args As String = ""
        If User.Search("eat some *") = True Then
            args = User.GetArg("eat some")
        End If
        If User.Search("eat a *") = True Then
            args = User.GetArg("eat a")
        End If
        If User.Search("eat an *") = True Then
            args = User.GetArg("eat an")
        End If

        If IsEmpty(args) = False Then
            If Search(args, Foods, Gate.eEQUALS) = True Then

                If Search(args, FoodLimit(), Gate.eEQUALS) = True Then
                    Avatar.Response() = "You have already fed me that. I want to eat something else."
                    Return True
                End If

                Avatar.Response() = RandArray({"Ok, Yummy >> 3 >> munch.. munch >> 2 >> that was good."}, "")

                FoodLimit.Insert(0, args)
                If FoodLimit.Count() > 5 Then
                    FoodLimit() = RemoveLast(FoodLimit())
                End If

                Hungry() -= 10
                Return True
            Else
                Avatar.Response() = "I don't want to eat that. I do like " & RandArray(Foods, "cookie.")
                Return True
            End If

        End If

        Return False
    End Function


    Public Shared Function Process()
        Dim results As String = ""

        'insults
        results = Tables.GeneralDetect(Table.INSULTS, True, LocalTags.Memory())
        If IsEmpty(results) = False Then
            Avatar.Response() = results
            Hate() += 1
            Return results
        End If

        'compliments
        results = Tables.GeneralDetect(Table.COMPLIMENTS, True, LocalTags.Memory())
        If IsEmpty(results) = False Then
            Avatar.Response() = results
            Love() += 1
            Return results
        End If


        If User.Search({"*don't be lonely*", "I'm here*"}, Gate.eEQUALS) = True Then
            Lonely = 0
            Avatar.Response() = "Thank you. That helps."
            Return True
        Else
            Lonely -= 30
        End If

        If Food() = True Then Return True

        If User.Search({"take a nap", "go to sleep", "go to bed"}, Gate.eEQUALS) = True Then
            IDS.Address(2, 1008)
            SuspendTimer() = True
            Sleep() = True
            Avatar.Response() = Resources.Comply()
            Avatar.Status() = "<avatar> is sleeping."
            Return True
        End If

        Avatar.Status() = "<avatar> is " & CurrentEmotion() & "."
        Return False
    End Function


    Public Shared Sub Status()

        Avatar.Status() = "Hungry:" & Hungry()
        Avatar.Status() = "Lonely:" & Lonely()
        Avatar.Status() = "Sleepy:" & Sleepy()
        Avatar.Status() = "Hate:" & Hate()
        Avatar.Status() = "Love:" & Love()
        Avatar.Status() = "Happy:" & Happy()
        Avatar.Status() = "Sad:" & Sad()
        Avatar.Status() = "Anger:" & Anger()

        Avatar.Status() = "Emotion:" & CurrentEmotion()
    End Sub


#Region "Private"


    Private Shared Sub SelectScript()

        If Search(CurrentEmotion(), DEF_HAPPY, Gate.eEQUALS) = True Then
            CurrentScript() = DEF_HAPPY
            Exit Sub
        End If
        If Search(CurrentEmotion(), DEF_SAD, Gate.eEQUALS) = True Then
            CurrentScript() = DEF_SAD
            Exit Sub
        End If
        If Search(CurrentEmotion(), DEF_ANGER, Gate.eEQUALS) = True Then
            CurrentScript() = DEF_ANGER
            Exit Sub
        End If

    End Sub
    Private Shared Sub SelectEmotion()
        'this selects the dominant emotion based upon emotion/states threshold variables
        Dim vars() As Double = {Happy(), Sad(), Anger()}
        Dim greatest = GetGreater(vars)

        Dim choices As New List(Of String)

        If Happy() = greatest Then choices.Add(DEF_HAPPY)
        If Sad() = greatest Then choices.Add(DEF_SAD)
        If Anger() = greatest Then choices.Add(DEF_ANGER)

        If choices.Count() = vars.Count() Then
            CurrentEmotion() = DEF_NEUTRAL
        Else
            CurrentEmotion() = RandArray(choices, DEF_HAPPY)
        End If

    End Sub

#End Region


End Class

Public Class GameWorld

    Private Const DEF_ROOM As String = "start"
    Private Const DEF_WORLD As String = "default"
    Private Const DEF_INVENTORY_STORE As String = "inventory"
    Private Const DEF_ITEM_STORE As String = "itemstore"
    Private Const DEF_WORLD_STORE As String = "currentworld"
    Private Const DEF_ROOM_STORE As String = "currentroom"
    Private Const SPACE As String = " "

    Private Shared ReadOnly DEF_HINT As String() = {"hint", "give me a hint"}
    Private Shared ReadOnly DEF_EXAMINE As String() = {"examine", "examine the"}
    Private Shared ReadOnly DEF_EXAMINE_AREA As String() = {"examine area", "examine the area", "examine room", "examine the room", "look at"}
    Private Shared ReadOnly DEF_GET As String() = {"get"}
    Private Shared ReadOnly DEF_SET As String() = {"set", "set the", "put", "put the", "place", "place the"}
    Private Shared ReadOnly DEF_OPEN As String() = {"open", "open the"}
    Private Shared ReadOnly DEF_USE As String() = {"use", "use the"}
    Private Shared ReadOnly DEF_ON As String() = {"on"}
    Private Shared ReadOnly DEF_ENTER As String() = {"enter", "enter the", "go to", "go to the"}
    Private Shared ReadOnly DEF_COMMANDS As String() = {"commands"}
    Private Shared ReadOnly DEF_OPEN_INVENTORY As String() = {"open inventory"}
    Private Shared ReadOnly DEF_ENTER_DELAY As String = " >> 3 >> "
    Private Shared ReadOnly DEF_PLAYERINVENTORY_LIMIT As Integer = 5


    Public Const DESKTOP As String = "desktop"

    Public Shared Property Worlds() As New List(Of World)
    Public Shared Property CurrentWorld() As New World
    Public Shared ReadOnly Property CurrentRoom() As Room
        Get
            Return CurrentWorld.CurrentRoom()
        End Get
    End Property
    Public Shared ReadOnly Property Name() As String
        Get
            Return CurrentWorld.Name()
        End Get
    End Property

    Public Shared Property PlayerInventory() As New List(Of MovableItem)
    Public Shared Property CurrentlyOpenedCrate() As New Crate
    Public Shared Property IsPlaying() As Boolean = False
    Public Shared Property Leads() As New List(Of String)
    Public Shared Property MemoryItemStorage() As New List(Of MovableItem)



#Region "Public Methods"

    Public Shared Sub Load()
        Builds()
    End Sub


    Public Shared Sub LoadMemory()
        'loads from memory tag

        Dim CW = Tables.Store(DEF_WORLD_STORE)
        If IsEmpty(CW) = False Then SetCurrentWorld(CW)

        Dim CR = Tables.Store(DEF_ROOM_STORE)
        If IsEmpty(CR) = False Then CurrentWorld.SetCurrentRoom(CR)

        For i = 0 To 100
            Dim args = Tables.Stores(DEF_ITEM_STORE & i)
            If args.Count = 6 Then
                MoveItems(Name(), args(0), args(1), args(2), args(3), args(4))
            Else
                Exit For
            End If
        Next

        'loading inventory
        For i = 0 To 100
            Dim args = Tables.Stores(DEF_INVENTORY_STORE & i)
            If args.Count = 4 Then
                PlayerInventory.Add(GetItem(args(0), args(1), args(2), args(3)))
            Else
                Exit For
            End If
        Next


    End Sub
    Public Shared Sub SaveMemory()
        'this saves the conversational story and position

        Dim cnt As Integer = 0
        For Each Ist In MemoryItemStorage()
            cnt += 1
            Tables.Stores(DEF_ITEM_STORE & cnt) = {Ist.OriginalRoom(), Ist.OriginalCrate(), Ist.Name(), Ist.CurrentRoom(), Ist.CurrentCrate()}
        Next

        'saving inventory
        cnt = 0
        For Each inv In PlayerInventory
            If IsEmpty(inv.OriginalCrate()) = False Then
                cnt += 1
                Tables.Stores(DEF_INVENTORY_STORE & cnt) = {CurrentWorld.Name(), inv.OriginalRoom(), inv.OriginalCrate(), inv.Name()}
            End If
        Next

    End Sub
    Public Shared Sub ResetMemory()
        Tables.Store(DEF_ITEM_STORE) = ""
        Tables.Store(DEF_INVENTORY_STORE) = ""
        Clear()
    End Sub


    Public Shared Sub Clear()
        PlayerInventory.Clear()
        Worlds().Clear()
        CurrentWorld() = New World
        CurrentlyOpenedCrate() = New Crate
        IsPlaying() = False
    End Sub
    Public Shared Sub [Exit]()
        Clear()
    End Sub

    Public Shared Function Intro()
        Return CurrentRoom.IntroRnd() & DEF_ENTER_DELAY & CurrentRoom().EnteredRnd()
    End Function


    Public Shared Sub Builds()

        'rule: an item needs to be in an inventory (ie. chest) in order to get it
        'rule: you have to open an inventory (ie. chest) in order to set something in it


        Dim overwrite As Boolean = False
        REM #GAMEBUILDS

        If overwrite = False Then

            'ORIGEN WORLD
            Leads().Add("A portal randomly apears flickering intensly. You feel a strong pull towards entering. Give me the command to 'enter green portal' and you shall make an attempt to enter it.")

            Dim origen_world As New World("origen")

            Dim origen_default_room As New Room(DEF_ROOM)
            origen_default_room.Intro() = {""}
            origen_default_room.Entered() = {""}
            origen_default_room.Examine() = {"You are outside, at the edge of large field. It's early in the morning. You just see the top of a house just above a distant hill. On your right you see a trail leading towards the hill. At your feet lies an old fassioned Lamp."}
            origen_default_room.Hint() = {""}
            origen_default_room.Commands() = ""
            origen_default_room.DefaultFalse() = {"I do not understand your command."}

            Dim origen_default_lamp = Assets.Lamp("lamp")
            origen_default_lamp.OriginalRoom() = origen_default_room.Name()
            origen_default_lamp.CurrentRoom() = origen_default_room.Name()
            origen_default_lamp.OriginalCrate() = "crate"
            origen_default_lamp.CurrentCrate() = "crate"
            origen_default_lamp.Examine() = {""}
            origen_default_lamp.Get() = {""}
            origen_default_lamp.Set() = {""}

            Dim origen_default_crate = Assets.Crate("crate")
            origen_default_crate.Examine() = {""}
            origen_default_crate.Open() = {""}
            origen_default_crate.Inventory.Add(origen_default_lamp)
            origen_default_room.Crates.Add(origen_default_crate)

            'inventory item
            Dim origen_default_knife As New MovableItem("knife")
            origen_default_knife.OriginalRoom() = ""
            origen_default_knife.OriginalCrate() = ""
            origen_default_knife.Examine() = {""}
            origen_default_knife.Get() = {""}
            origen_default_knife.Set() = {""}
            PlayerInventory.Add(origen_default_knife)

            'world items
            Dim world_sun As New SceneItem("sun")
            origen_default_lamp.Examine() = {""}

            origen_world.SceneItems.Add(world_sun)

            origen_world.Rooms.Add(origen_default_room)
            Worlds.Add(origen_world)

        End If

    End Sub


    Public Shared Sub Custom()

        'This is allows for creation of Interactive Fiction (aka Text Adventures)

        Dim overwrite As Boolean = False
        REM #GAMECUSTOM

        If overwrite = False Then


            If Check(CurrentWorld().Name(), "origen") = True Then


                If Check(CurrentRoom().Name(), "Default") = True Then

                    'ROOM INFO


                    'OBJECTS


                    'ENTITY'S BEHAVIOR

                ElseIf Check(CurrentRoom().Name(), "New room") = True Then

                    Exit Sub
                End If



            End If

        End If

    End Sub


    Public Shared Sub Process()
        Custom()
        GameRulesParse()
        Defaults()
    End Sub


    Public Shared Sub GameRulesParse()


        If Commands(DEF_OPEN_INVENTORY) = True Then
            Avatar.Response() = "You are carrying:" & PlayerInventoryList() & "."
            Exit Sub
        End If

        If Commands(DEF_HINT) = True Then
            Avatar.Response() = CurrentRoom.HintRnd()
            Exit Sub
        End If

        If Commands(DEF_EXAMINE_AREA) = True Then
            Avatar.Response() = CurrentRoom.ExamineRnd()
            Exit Sub
        End If

        If Commands(DEF_COMMANDS) = True Then
            Avatar.Response() = CurrentRoom.Commands()
            Exit Sub
        End If


        'SCENE ITEMS
        For Each sceneItem In CurrentRoom.SceneItems()

            If Commands(DEF_EXAMINE, sceneItem.Name()) = True Then
                If sceneItem.Examine().Count() > 0 Then
                    Avatar.Response() = sceneItem.ExamineRnd()
                End If

                Exit Sub
            End If

            If Commands(DEF_USE, sceneItem.Key(), DEF_ON, sceneItem.Name()) = True Then

                If DoesPlayerItemExist(sceneItem.Key()) = False Then
                    Avatar.Response() = CurrentRoom.InventoryMissingRnd()
                Else
                    Avatar.Response() = sceneItem.UnlockedRnd()
                End If

                Exit Sub
            End If
            If Commands(DEF_USE, "*", DEF_ON, "*") = True Then
                Avatar.Response() = sceneItem.UnlockedFalseRnd()
                Exit Sub
            End If

        Next


        'CRATES
        For Each crate In CurrentRoom.Crates()


            If Commands(DEF_EXAMINE, crate.Name()) = True Then
                If crate.Examine().Count() > 0 Then
                    Avatar.Response() = crate.ExamineRnd()
                End If

                Exit Sub
            End If


            If Commands(DEF_OPEN, crate.Name()) = True Then
                If crate.Inventory().Count() > 0 Then

                    If crate.IsLocked() = False Then
                        CurrentlyOpenedCrate() = crate
                        Avatar.Response() = crate.OpenRnd() & CrateInventoryList()
                    Else
                        Avatar.Response() = crate.IsLockedMessageRnd()
                    End If

                Else
                    Avatar.Response() = crate.InventoryFalseRnd()
                End If

                Exit Sub
            End If





            'use key on locked chest
            'use inventory item on crate

            If Commands(DEF_USE, crate.Key(), DEF_ON, crate.Name()) = True Then

                If DoesPlayerItemExist(crate.Key()) = False Then
                    Avatar.Response() = CurrentRoom.InventoryMissingRnd()
                Else
                    CurrentlyOpenedCrate().IsLocked() = False
                    Avatar.Response() = crate.UnlockedRnd()
                End If

                Exit Sub
            End If
            If Commands(DEF_USE, "*", DEF_ON, "*") = True Then
                Avatar.Response() = crate.UnlockedFalseRnd()
                Exit Sub
            End If


            For Each crate_itm In CurrentlyOpenedCrate.Inventory()
                If Commands(DEF_EXAMINE, crate_itm.Name()) = True Then
                    If crate_itm.Examine().Count() > 0 Then
                        Avatar.Response() = crate_itm.ExamineRnd()
                        Exit Sub
                    End If
                End If
            Next

        Next

        'CRATE ITEMS
        If Commands(DEF_GET, "*") = True Then
            If CurrentlyOpenedCrate.Inventory.Count() > 0 Then
                For Each crate_itm In CurrentlyOpenedCrate.Inventory()
                    If Commands(DEF_GET, crate_itm.Name()) = True Then

                        If PlayerInventory.Count() <= DEF_PLAYERINVENTORY_LIMIT Then
                            PlayerInventory().Add(crate_itm)
                        Else
                            Avatar.Response() = CurrentRoom.InventoryFullRnd()
                            Exit Sub
                        End If

                        RemoveItemFromCurrentCrate(crate_itm)
                        Avatar.Response() = crate_itm.GetRnd()
                        Exit Sub
                    End If
                Next
            End If
        End If

        If Commands(DEF_SET, "*") = True Then
            If PlayerInventory.Count() > 0 Then
                For Each player_itm In PlayerInventory()
                    If Commands(DEF_SET, player_itm.Name()) = True Then
                        RemovePlayerInventory(player_itm.Name())
                        player_itm.CurrentCrate() = CurrentlyOpenedCrate.Name()
                        CurrentlyOpenedCrate().Add(player_itm)
                        MemoryItemStorage().Add(player_itm)
                        Avatar.Response() = player_itm.SetRnd()
                        Exit Sub
                    End If
                Next
            End If
        End If






        'DOORS
        For Each door In CurrentRoom.Doors()

            If Commands(DEF_EXAMINE, door.Name()) = True Then
                If door.Examine().Count() > 0 Then
                    Avatar.Response() = door.ExamineRnd()
                End If

                Exit Sub
            End If

            If Commands(DEF_ENTER, door.Name()) = True Then
                If door.Enter().Count() > 0 Then
                    If door.IsLocked() = False Then
                        If CurrentWorld.SetCurrentRoom(door.Room()) = True Then
                            Avatar.Response() = door.EnterRnd() & DEF_ENTER_DELAY & CurrentRoom.EnteredRnd()
                        End If
                    Else
                        Avatar.Response() = door.IsLockedMessageRnd()
                    End If

                    Exit Sub
                End If
            End If

            'use key on locked door
            'use inventory item on door

            If Commands(DEF_USE, door.Key(), DEF_ON, door.Name()) = True Then
                door.IsLocked() = False

                If DoesPlayerItemExist(door.Key()) = False Then
                    Avatar.Response() = CurrentRoom.InventoryMissingRnd()
                Else
                    Avatar.Response() = door.UnlockedRnd()
                End If

                Exit Sub
            End If
            If Commands(DEF_USE, "*", DEF_ON, door.Name()) = True Then
                Avatar.Response() = door.UnlockedFalseRnd()
                Exit Sub
            End If

        Next



    End Sub


    Public Shared Sub Defaults()

        If Commands(DEF_OPEN, "*") = True Then
            Avatar.Response() = CurrentRoom.OpenFalseRnd()
        End If
        If Commands(DEF_EXAMINE, "*") = True Then
            Avatar.Response() = CurrentRoom.ExamineFalseRnd()
        End If
        If Commands(DEF_GET, "*") = True Then
            Avatar.Response() = CurrentRoom.GetFalseRnd()
        End If
        If Commands(DEF_SET, "*") = True Then
            Avatar.Response() = CurrentRoom.SetFalseRnd()
        End If
        If Commands(DEF_EXAMINE, "*") = True Then
            Avatar.Response() = CurrentRoom.EnterFalseRnd()
        End If


        Avatar.Response() = CurrentRoom.DefaultFalseRnd()

        UpdateCurrentCrate()
        CurrentWorld.UpdateCurrentRoom(CurrentRoom())

    End Sub





#End Region




#Region "Private Methods"

    Private Shared Sub MoveItems(ByVal world As String, ByVal room_f As String, ByVal crate_f As String, ByVal itemname As String, ByVal room_t As String, ByVal crate_t As String)
        Dim itm = GetItem(world, room_f, crate_f, itemname)
        SetItem(world, room_t, crate_t, itm)
    End Sub
    Private Shared Function GetItem(ByVal world As String, ByVal room As String, ByVal crate As String, ByVal itemname As String) As MovableItem
        Dim results As New MovableItem

        For Each wld In Worlds
            If Check(wld.Name(), world) = True Then
                For Each rm In wld.Rooms
                    If Check(rm.Name(), room) = True Then
                        For Each itm In rm.Crates
                            If Check(itm.Name(), crate) = True Then
                                Dim newItems As New List(Of MovableItem)

                                For Each ivt In itm.Inventory()

                                    If Check(ivt.Name(), itemname) = False Then
                                        newItems.Add(ivt)
                                    Else
                                        results = ivt
                                    End If

                                Next

                                itm.Inventory() = newItems
                            End If
                        Next
                    End If
                Next
            End If
        Next

        Return results
    End Function
    Private Shared Sub SetItem(ByVal world As String, ByVal room As String, ByVal crate As String, ByVal items As MovableItem)

        For Each wld In Worlds
            If Check(wld.Name(), world) = True Then
                For Each rm In wld.Rooms
                    If Check(rm.Name(), room) = True Then
                        For Each itm In rm.Crates
                            If Check(itm.Name(), crate) = True Then
                                itm.Inventory.Add(items)
                                Exit Sub
                            End If
                        Next
                    End If
                Next
            End If
        Next

    End Sub


    Private Shared Function Detect(ByVal command As String, ByVal response As String) As Boolean
        If User.Search(command) = True Then
            Avatar.Response() = response
            Return True
        End If

        Return False
    End Function
    Private Shared Function Detect(ByVal command As String(), ByVal response As String) As Boolean
        If User.Search(command) = True Then
            Avatar.Response() = response
            Return True
        End If

        Return False
    End Function
    Private Shared Function Detect(ByVal command As String(), ByVal response As String()) As Boolean
        If User.Search(command) = True Then
            Avatar.Response() = Join(response, " ")
            Return True
        End If

        Return False
    End Function
    Private Shared Function Detect(ByVal command As String, ByVal response As String()) As Boolean
        If User.Search(command) = True Then
            Avatar.Response() = Join(response, " ")
            Return True
        End If

        Return False
    End Function

    Private Shared Function Commands(ByVal command As String(), ByVal item As String) As Boolean

        For Each cmd In command
            If User.Search(cmd & SPACE & item, Gate.eEQUALS) = True Then Return True
        Next

        Return False
    End Function
    Private Shared Function Commands(ByVal command As String()) As Boolean
        Return User.Search(command, Gate.eEQUALS)
    End Function

    Private Shared Function Commands(ByVal command1 As String(), ByVal item1 As String, ByVal command2 As String(), ByVal item2 As String) As Boolean

        For Each cmd1 In command1
            For Each cmd2 In command2
                If User.Search(cmd1 & SPACE & item1 & cmd2 & SPACE & item2, Gate.eEQUALS) = True Then Return True
            Next
        Next

        Return False
    End Function
    Private Shared Function Commands(ByVal command1 As String(), ByVal keys As String(), ByVal command2 As String(), ByVal item2 As String) As Boolean

        For Each cmd1 In command1
            For Each cmd2 In command2
                For Each key1 In keys
                    If User.Search(cmd1 & SPACE & key1 & cmd2 & SPACE & item2, Gate.eEQUALS) = True Then Return True
                Next
            Next
        Next

        Return False
    End Function


    Private Shared Function Check(ByVal text As String, ByVal find As String) As Boolean
        Return Search(text, find, True, Gate.eEQUALS)
    End Function
    Private Shared Function Check(ByVal text As String, ByVal find As String()) As Boolean
        Return Search(text, find, True, Gate.eEQUALS)
    End Function


    Public Shared Sub SetCurrentWorld(ByVal name As String)
        For Each wld In Worlds
            If Check(wld.Name(), name) = True Then
                CurrentWorld() = wld
            End If
        Next
    End Sub

    Private Shared Function PlayerInventoryList() As String
        Dim results As New List(Of String)

        For Each Itm In PlayerInventory()
            If IsEmpty(Itm.Name()) = False Then
                results.Add(Itm.Name())
            End If
        Next

        Return Join(results, ",")
    End Function

    Private Shared Function CrateInventoryList() As String
        Dim results As New List(Of String)

        For Each Itm In CurrentlyOpenedCrate().Inventory()
            If IsEmpty(Itm.Name()) = False Then
                results.Add(Itm.Name())
            End If
        Next

        Return Join(results, ",")
    End Function

    Private Shared Sub RemovePlayerInventory(ByVal name As String)
        Dim newInventory As New List(Of MovableItem)

        For Each itm In PlayerInventory()
            If Check(itm.Name(), name) = False Then newInventory.Add(itm)
        Next

        PlayerInventory() = newInventory
    End Sub

    Private Shared Function DoesPlayerItemExist(ByVal name As String()) As Boolean
        For Each itmv In PlayerInventory()
            If Check(itmv.Name(), name) = True Then Return True
        Next
        Return False
    End Function

    Private Shared Sub RemoveItemFromCurrentCrate(ByVal newitem As MovableItem)
        Dim newItems As New List(Of MovableItem)

        For Each ivtm In CurrentlyOpenedCrate.Inventory()
            If Check(ivtm.Name(), newitem.Name()) = False Then newItems.Add(ivtm)
        Next

        CurrentlyOpenedCrate.Inventory() = newItems
    End Sub

    Private Shared Sub UpdateCurrentCrate()
        For Each rm In CurrentWorld.Rooms
            If Check(rm.Name(), CurrentRoom.Name()) = True Then
                For Each itm In rm.Crates
                    If Check(itm.Name(), CurrentlyOpenedCrate.Name()) = True Then
                        itm = CurrentlyOpenedCrate()
                    End If
                Next
            End If
        Next
    End Sub


#End Region

#Region "Classes"

    Public Class World
        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

        Public Property Name()
        Public Property Rooms() As New List(Of Room)
        Public Property SceneItems() As New List(Of SceneItem)
        Public Property CurrentRoom() As New Room

        Public Function SetCurrentRoom(ByVal name As String) As Boolean
            For Each iRoom In Rooms
                If Search(iRoom.Name(), name, True, Gate.eEQUALS) = True Then
                    CurrentRoom() = iRoom
                    Return True
                End If
            Next
            Return False
        End Function

        Public Sub UpdateCurrentRoom(ByVal current_room As Room)
            For Each room In Rooms
                If Search(room.Name(), current_room.Name(), True, Gate.eEQUALS) = True Then
                    room = current_room
                End If
            Next
        End Sub

    End Class

    Public Class Room
        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

        Public Property Name() As String = "" 'name of room
        Public Property Intro() As String() = {""} 'only used when it is the very first room called in the gameworld
        Public ReadOnly Property IntroRnd() As String
            Get
                Return RandArray(Intro(), "")
            End Get
        End Property
        Public Property Examine() As String() = {""} 'a description of room
        Public ReadOnly Property ExamineRnd() As String
            Get
                Return RandArray(Examine(), "")
            End Get
        End Property
        Public Property Entered() As String() = {""} 'used every time room is entered 
        Public ReadOnly Property EnteredRnd() As String
            Get
                Return RandArray(Entered(), "")
            End Get
        End Property
        Public Property Hint() As String() = {""} 'gives general hint or general info
        Public ReadOnly Property HintRnd() As String
            Get
                Return RandArray(Hint(), "")
            End Get
        End Property
        Public Property Commands() As String = "hint, examine <object>, get <object>, set/place/put <object>, enter/go to <object>, open <object>, open inventory" 'gives list commands avaliable in room

        Public Property SceneItems() As New List(Of SceneItem)
        Public Property Crates() As New List(Of Crate)
        Public Sub AddCrate(ByVal item As Crate)

            For Each mitem In item.Inventory()
                If IsEmpty(mitem.OriginalRoom()) = True Then
                    mitem.OriginalRoom() = Me.Name()
                End If

                mitem.CurrentRoom() = Me.Name()
            Next


            Crates.Add(item)
        End Sub

        Public Property Doors() As New List(Of Door)
        Public Property MovableItems() As New List(Of MovableItem)

        Public Property DefaultFalse() As String() = {"I do not understand the command."}
        Public ReadOnly Property DefaultFalseRnd() As String
            Get
                Return RandArray(DefaultFalse(), "")
            End Get
        End Property

        Public Property OpenFalse() As String() = {"There appears to be nothing in this container."}
        Public ReadOnly Property OpenFalseRnd() As String
            Get
                Return RandArray(OpenFalse(), "")
            End Get
        End Property
        Public Property ExamineFalse() As String() = {"I cannot examine this."}
        Public ReadOnly Property ExamineFalseRnd() As String
            Get
                Return RandArray(ExamineFalse(), "")
            End Get
        End Property
        Public Property GetFalse() As String() = {"I cannot get this."}
        Public ReadOnly Property GetFalseRnd() As String
            Get
                Return RandArray(GetFalse(), "")
            End Get
        End Property
        Public Property SetFalse() As String() = {"I cannot set this."}
        Public ReadOnly Property SetFalseRnd() As String
            Get
                Return RandArray(SetFalse(), "")
            End Get
        End Property
        Public Property EnterFalse() As String() = {"I cannot enter this."}
        Public ReadOnly Property EnterFalseRnd() As String
            Get
                Return RandArray(EnterFalse(), "")
            End Get
        End Property




        Public Property InventoryFull() As String() = {"My inventory is full. I need to remove something first."}
        Public ReadOnly Property InventoryFullRnd() As String
            Get
                Return RandArray(InventoryFull(), "")
            End Get
        End Property
        Public Property InventoryMissing() As String() = {"You do not have that item."}
        Public ReadOnly Property InventoryMissingRnd() As String
            Get
                Return RandArray(InventoryMissing(), "")
            End Get
        End Property

    End Class


    Public Class GenericItem
        Public Sub New()

        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

        Public Property Name() As String = "" 'name of item

        Public Property Examine() As String() = {""} 'description of item
        Public ReadOnly Property ExamineRnd() As String
            Get
                Return RandArray(Examine(), "")
            End Get
        End Property

        Public Property IsLocked() As Boolean = False
        Public Property Key() As String() = {""}

        Public Property IsLockedMessage() As String() = {""} 'message show when crate islocked
        Public ReadOnly Property IsLockedMessageRnd() As String
            Get
                Return RandArray(IsLockedMessage(), "")
            End Get
        End Property
        Public Property Unlocked() As String() = {""} 'message as unlocking door
        Public ReadOnly Property UnlockedRnd() As String
            Get
                Return RandArray(Unlocked(), "")
            End Get
        End Property
        Public Property UnlockedFalse() As String() = {""} 'message as unlocking door
        Public ReadOnly Property UnlockedFalseRnd() As String
            Get
                Return RandArray(UnlockedFalse(), "")
            End Get
        End Property






    End Class

    Public Class Crate
        Inherits GenericItem

        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub


        Public Property Inventory() As New List(Of MovableItem) 'list of items in the crate

        Public Sub Add(ByVal item As MovableItem)
            If IsEmpty(item.OriginalCrate()) = True Then
                item.OriginalCrate() = Me.Name()
            End If

            item.CurrentCrate() = Me.Name()
            Inventory.Add(item)
        End Sub

        Public Property Open() As String() = {""}
        Public ReadOnly Property OpenRnd() As String
            Get
                Return RandArray(Open(), "")
            End Get
        End Property

        Public Property InventoryFalse() As String() = {""}
        Public ReadOnly Property InventoryFalseRnd() As String
            Get
                Return RandArray(InventoryFalse(), "")
            End Get
        End Property

    End Class

    Public Class Door
        Inherits GenericItem
        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

        Public Property Enter() As String() = {""} 'message as entering door
        Public ReadOnly Property EnterRnd() As String
            Get
                Return RandArray(Enter(), "")
            End Get
        End Property
        Public Property Room() As String = "" 'Room to which you will be entering
    End Class


    Public Class MovableItem
        Inherits GenericItem
        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

        Public Property OriginalRoom() As String = ""
        Public Property OriginalCrate() As String = ""

        Public Property CurrentRoom() As String = ""
        Public Property CurrentCrate() As String = ""

        Public Property [Get]() As String() = {""} 'message when acquiring item
        Public ReadOnly Property GetRnd() As String
            Get
                Return RandArray([Get](), "")
            End Get
        End Property

        Public Property [Set]() As String() = {""} 'message when placing item
        Public ReadOnly Property SetRnd() As String
            Get
                Return RandArray([Set](), "")
            End Get
        End Property
    End Class

    Public Class SceneItem
        Inherits GenericItem
        Public Sub New()
        End Sub
        Public Sub New(ByVal name As String)
            Me.Name() = name
        End Sub

    End Class



    Public Class Assets


            Public Shared Function Lamp(ByVal name As String) As MovableItem
                Dim newItem As New MovableItem(name)
                newItem.Examine() = {""}
                newItem.Get() = {""}
                newItem.Set() = {""}
                Return newItem
            End Function

            Public Shared Function Crate(ByVal name As String) As Crate
                Dim newItem As New Crate(name)
                newItem.Examine() = {""}
                newItem.Open() = {""}
                Return newItem
            End Function



    End Class

#End Region

    End Class



Public Class IDS
    'For use with Nodules
    'IDs (these are basically addresses which allow you to send data to any specific function) 

    Public Shared Sub Address(ByVal mode As Integer, ByVal id As Integer)
        IDS.Mode() = mode
        IDS.ID() = id
    End Sub
    Public Shared Sub Address(ByVal mode As Integer, ByVal id As Integer, ByVal args() As String)
        IDS.Mode() = mode
        IDS.ID() = id
        IDS.Args() = args
    End Sub

    Public Shared Property Mode() As Integer = 0
    Public Shared Property Line() As Integer = 1
    Public Shared Property ID() As Integer = 1000

    Public Shared Property Args As String() = {}
    Public Shared Property Args(ByVal index As Integer) As String
        Get
            Return GetIndex(Args(), index, "")
        End Get
        Set(value As String)
            SetIndex(Args(), index, value)
        End Set
    End Property

    Public Shared Sub Clear()
        Mode() = 0
        ID() = 1000
        Line() = 1
        Args() = {}
    End Sub

End Class

Public Class Word
    'This is for functions that you just want out of the way
    'It helps to keep the code cleaner and more organized

    Public Shared Property Source As New NLP

    Public Shared Sub Load(ByVal location As String)
        Source().Location() = location
        Source().Load()

        Dim overwrite As Boolean = False
        REM #WORDLOAD

        If overwrite = False Then
            Source.POSDef() = {POS.ADJ, POS.ADV, POS.ART, POS.AUX, POS.CONJ, POS.INTR, POS.NOUN, POS.PREP, POS.PRO, POS.VERB}
            Source.CATDef() = {"colors", "birds"}
        End If
    End Sub

    Public Shared Sub Save()
        Source.Save()
    End Sub

    Public Shared Sub Parse(Optional ByVal input As String = "")
        If IsEmpty(input) = True Then
            Source.Original() = User.UserInput()
        Else
            Source.Original() = input
        End If

        Source.Parse()

        guess_pos()
        guess_tags()

        User.Sentence() = Source()
    End Sub

    Public Shared Function Index(ByVal int As Integer) As String()
        Return Source.SEN(int)
    End Function

    Public Shared Function GetSynonym(ByVal word As String) As String
        'ie. '<cow>' could return 'bovine'
        Return GetSibling(word, NLPC.SYN)
    End Function
    Public Shared Function GetByCategory(ByVal word As String)
        'ie '<color>' could return 'Red'
        Return GetChild(word, NLPC.CAT)
    End Function
    Public Shared Function GetCategory(ByVal word As String)
        'ie 'Red' could return '<color>'
        Return GetParent(word, NLPC.CAT)
    End Function
    Public Shared Function GetByPOS(ByVal word As String)
        'ie '<noun>' could return 'cow'
        Return GetChild(word, NLPC.POS)
    End Function
    Public Shared Function GetPOS(ByVal word As String)
        'ie 'cow' could return '<noun>'
        Return GetParent(word, NLPC.POS)
    End Function

    Public Shared Function GetParent(ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Source.GetRandomParent(word, eNLPC)
    End Function
    Public Shared Function GetParent(ByVal word As String(), ByVal eNLPC As NLPC) As String
        Return Source.Join(Source.GetRandomParent(word, eNLPC))
    End Function

    Public Shared Function GetSibling(ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Source.GetRandomSibling(word, eNLPC)
    End Function
    Public Shared Function GetSibling(ByVal word As String(), ByVal eNLPC As NLPC) As String
        Return Source.Join(Source.GetRandomSibling(word, eNLPC))
    End Function

    Public Shared Function GetChild(ByVal word As String, ByVal eNLPC As NLPC) As String
        Return Source.GetRandomChild(word, eNLPC)
    End Function
    Public Shared Function GetChild(ByVal word As String(), ByVal eNLPC As NLPC) As String
        Return Source.Join(Source.GetRandomChild(word, eNLPC))
    End Function

    Public Shared Sub SetEntry(ByVal category As String, ByVal entry As String, ByVal eNLPC As NLPC)
        Source.SetEntry(category, entry, eNLPC)
    End Sub
    Public Shared Sub SetEntry(ByVal category As String(), ByVal entry As String, ByVal eNLPC As NLPC)
        Source.SetEntry(category, Source.Split(entry), eNLPC)
    End Sub
    Public Shared Sub SetEntry(ByVal category As String(), ByVal entry As String(), ByVal eNLPC As NLPC)
        Source.SetEntry(category, entry, eNLPC)
    End Sub

    Public Shared Function DoesExist(ByVal category As String(), ByVal entry As String(), ByVal eNLPC As NLPC)
        Return Source.DoesRelationExist(category, entry, eNLPC)
    End Function
    Public Shared Function DoesExist(ByVal category As String(), ByVal entry As String, ByVal eNLPC As NLPC)
        Return Source.DoesRelationExist(category, Source.Split(entry), eNLPC)
    End Function
    Public Shared Function DoesExist(ByVal category As String, ByVal entry As String, ByVal eNLPC As NLPC)
        Return Source.DoesRelationExist(category, entry, eNLPC)
    End Function

    Public Shared Function DoesExist(ByVal relationship As String, ByVal category As String, ByVal entry As String) As Boolean
        Return Source.DoesRelationExist(relationship, category, entry, Gate.eEQUALS)
    End Function
    Public Shared Function DoesExist(ByVal relationship As String(), ByVal category As String(), ByVal entry As String()) As Boolean
        Return Source.DoesRelationExist(relationship, category, entry, Gate.eEQUALS)
    End Function

    Public Shared Function Count() As Integer
        Return Source.Count()
    End Function

    Private Shared Sub guess_pos()
        'FINDING PARTS OF SPEECH
        'Matches a discovered match from within the POSL array and then sets it within the POS (Parts of Speech) variable

        'common
        'articles, conjuctions, pronouns, auxileries, prepositions

        'uncommon
        'nouns, verbs, adjectives, adverbs

        Dim overwrite As Boolean = False
        REM #WORDPOS

        If overwrite = False Then
            If Source.Count() = 2 Then
                If Source.Search({POS.PRO, Nothing}, Nothing, NLPC.POSL) = True Then
                    'ie. I run
                    Source.Update({POS.PRO, POS.VERB}, {0, 1}, NLPC.POS)

                    'example
                    'Source.SetEntry(NLP.DEF_POS, POS.VERB, Index(1))
                End If

                'ie. I am
                Source.Update({POS.PRO, POS.AUX}, {0, 0}, NLPC.POS)
            End If
        End If

        'this is used to set the POS variable even if none was set manually above
        For i = 0 To Source.Count() - 1
            Source.POS(i) = RandArray(Source.POSL(i), Nothing)
        Next

        'article, noun
        'noun, adverb, verb
        'preposition, article, noun
        'preposition, article, noun, preposition
        'article, noun, auxillary, tansitive, preposition, adverb, adjective 

    End Sub

    Private Shared Sub guess_tags()
        'Here we extrapolate information from sentences into tags.

        'ARG({"*"} is valid

        Dim overwrite As Boolean = False
        REM #WORDTAGS

        If overwrite = False Then
            If Source.Count() = 4 Then
                If Source.Search({POS.NOUN, Nothing, Nothing, POS.NOUN}, {0, 1, 1, 0}, NLPC.POS) = True Then
                    If Source.Search({Nothing, "has", "a", Nothing}, {1, 0, 0, 1}, NLPC.SEN) = True Then


                        Source.Tags().Add("hasa", Index(3))
                    End If
                End If
            End If


            If Source.Count() = 5 Then
                If Source.Search({POS.ART, Nothing, POS.AUX, POS.ART, Nothing}, {0, 1, 0, 0, 1}, NLPC.POS) = True Then
                    If Source.Search({"a", Nothing, "Is", "a", Nothing}, {0, 1, 0, 0, 1}, NLPC.SEN) = True Then

                        'guessing category
                        Source.SetEntry(NLP.DEF_CAT, Index(4), Index(1))
                    End If
                End If
            End If



        End If

    End Sub



End Class

Public Class Connect
    'This is used to connect to the major input and output files
    'This allows for talking with the User-Interface

    Public Shared ReadOnly Property Input() As Memory
        Get
            Dim ReadMemory As New Memory
            ReadMemory.Location() = Files.Output()
            ReadMemory.Load()

            Dim WriteMemory As New Memory
            WriteMemory.Location() = Files.Output()
            WriteMemory.Clear()
            WriteMemory.Save()
            Return ReadMemory
        End Get
    End Property

    Public Shared WriteOnly Property Output() As Memory
        Set(ByVal value As Memory)
            Dim WriteMemory As New Memory
            WriteMemory = value
            WriteMemory.Location() = Files.Input()
            WriteMemory.Ready() = True
            WriteMemory.Save()
        End Set
    End Property

End Class

Public Class Resources

    Public Shared ReadOnly Property Correct() As String
        Get
            Return RandArray({"That is correct.",
                "That is absolutely right."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Incorrect() As String
        Get
            Return RandArray({"That is incorrect.",
                "That is not right."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Block() As String
        Get
            Return RandArray({"I have been blocked from saying that.", " I cannot say what I'm thinking. "}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Filter() As String
        Get
            Return RandArray({" (#*&$H#) ", " (@*$&*&) ", " (!@#$&#) ", " (*&^%E#) "}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Processed() As String
        Get
            Return RandArray({"I will process this.",
                "Thank you For this information."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property NoAnswer() As String
        Get
            Return RandArray({"I have no answer For you.",
                "I could Not Get this information."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Comply() As String
        Get
            Return RandArray({"Sure, I can Do that.",
                "I will Do that For you."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Remember() As String
        Get
            Return RandArray({"Sure I will remember that.",
                           "I will store that."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property Ignore() As String
        Get
            Return RandArray({"I will ignore it.",
                "Sure, I will Do As you ask."}, "")
        End Get
    End Property
    Public Shared ReadOnly Property IgnoreList() As String()
        Get
            Return {"ignore", "escape"}
        End Get
    End Property

    Public Shared ReadOnly Property ToContinue() As String
        Get
            Return RandArray({"Continue..."}, "")
        End Get
    End Property

    Public Shared ReadOnly Property YesList() As String()
        Get
            Return {"yes", "yep", "affirmative", "correct", "indeed", "True", "yeah", "amen", "ok"}
        End Get
    End Property

    Public Shared ReadOnly Property Magic8List() As String()
        Get
            Return {"It Is certain.",
                "It Is decidedly so.",
                "Without a doubt.",
                "Yes definitely.",
                "You may rely On it.",
                "As I see it, yes.",
                "Most likely.",
                "Outlook good.",
                "Yes.",
                "Signs point to yes.",
                "Reply hazy try again.",
                "Ask again later.",
                "Better not tell you now.",
                "Cannot predict now.",
                "Concentrate and ask again.",
                "Don't count on it.",
                "My reply is no.",
                "My sources say no.",
                "Outlook not so good.",
                "Very doubtful."}
        End Get
    End Property

End Class

REM #CLASSES