Public Class Form1
    Dim zamaneShabihSazi As Double
    Dim shorueShabihsazi As Integer = 0
    Dim pishamadeFeli As Integer
    Dim zamaneAvalinVorudeAdi As Double
    Dim list As New List(Of Double)
    Dim FEL As New List(Of List(Of Double))
    Dim NoEeMosafer As Integer
    Dim NoE1 As Integer = 1
    Dim NoE2 As Integer = 2
    Dim Noe2BaOlaviat As Integer = 3
    Dim saat As Double
    Dim t As Double = 0
    Dim t0 As Double 'zamane vorude Mosafer
    Dim t1(5) As Double  'zamane etmame khedmat dehi
    Dim t2(5) As Double 'zamane shorue Khedmat dehi
    Dim t3(5) As Double
    Dim r As New Random
    Dim trace As New DataTable
    Dim shomareyeKhedmatdahande As Integer 'khedmat dahande 1,2,3,4
    Dim Null As Integer = 0
    Dim vaziateKhedmatdehande(7) As Integer 'azad,mashghul,amadeEsterahattulani,amadeEsterahatkutah,darHaleEsterahattulai,darHaleEsterahatkutah
    Dim azad As Integer = 1
    Dim mashgul As Integer = 2
    Dim amadeEsterahatTulani As Integer = 3
    Dim amadeEsterahatKutah As Integer = 4
    Dim darHaleEsterahatTulani As Integer = 5
    Dim darHaleEsterahatKutah As Integer = 6
    Dim safeBarNoeAval As Integer = 900
    Dim safeBarNoeDovomeAdi As Integer = 800
    Dim safeBarNoeDovomeBaOlaviat As Integer = 700
    'Pishamad ha:
    Dim vorudeBaMinibus As Integer = 10
    Dim vorudeAdi As Integer = 20
    Dim etmameResidan As Integer = 30
    Dim etmemeKhedmatDehi As Integer = 40
    Dim etmamekhoruj As Integer = 50
    Dim shorueEsterahatTulani As Integer = 60
    Dim shorueEsterahatKutah As Integer = 70
    Dim etmameEsterahsatTulani As Integer = 80
    Dim etmameEsterahatKutah As Integer = 90
    'mavarede Khaste Shode
    Dim tedadTakhireKutah(5) As Integer
    Dim miangineTakhirKutah(5) As Double
    Dim a(5) As Double
    Dim tedadTakhireTulani(5) As Integer
    Dim miangineTakhireTulani(5) As Double
    Dim b(5) As Double
    Dim zaribEshteghal(5) As Double
    Dim c1(5) As Double
    Dim miangineMandanDarSystem(4) As Double
    Dim d(5) As Integer
    Dim d1(5) As Double
    Dim f(7) As Integer
    Dim l As Integer = 0
    Dim s As Boolean
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TextBox1.Text = "" Then
            MsgBox("زمان شبیه سازی را وارد کنید ")
        Else
            zamaneShabihSazi = TextBox1.Text
            starter()
            main()
            Form2.TextBox1.Text = tedadTakhireKutah(1)
            Form2.TextBox2.Text = tedadTakhireKutah(2)
            Form2.TextBox3.Text = tedadTakhireKutah(3)
            Form2.TextBox4.Text = tedadTakhireKutah(4)
            Form2.TextBox5.Text = tedadTakhireTulani(1)
            Form2.TextBox6.Text = tedadTakhireTulani(2)
            Form2.TextBox7.Text = tedadTakhireTulani(3)
            Form2.TextBox8.Text = tedadTakhireTulani(4)

            Form2.TextBox9.Text = miangineTakhirKutah(1)
            Form2.TextBox10.Text = miangineTakhirKutah(2)
            Form2.TextBox11.Text = miangineTakhirKutah(3)
            Form2.TextBox12.Text = miangineTakhirKutah(4)
            Form2.TextBox13.Text = miangineTakhireTulani(1)
            Form2.TextBox14.Text = miangineTakhireTulani(2)
            Form2.TextBox15.Text = miangineTakhireTulani(3)
            Form2.TextBox16.Text = miangineTakhireTulani(4)
            Form2.TextBox17.Text = zaribEshteghal(1)
            Form2.TextBox18.Text = zaribEshteghal(2)
            Form2.TextBox19.Text = zaribEshteghal(3)
            Form2.TextBox20.Text = zaribEshteghal(4)

            Form2.TextBox21.Text = miangineMandanDarSystem(1)
            Form2.TextBox22.Text = miangineMandanDarSystem(2)
            Form2.TextBox24.Text = miangineMandanDarSystem(3)
            Form2.Show()
        End If
    End Sub

    Private Sub starter()
        vaziateKhedmatdehande(1) = azad
        vaziateKhedmatdehande(2) = azad
        vaziateKhedmatdehande(3) = azad
        vaziateKhedmatdehande(4) = azad
        safeBarNoeAval = 0
        safeBarNoeDovomeAdi = 0
        safeBarNoeDovomeBaOlaviat = 0
        For i As Integer = 1 To 5
            t1(i) = 0
            t2(i) = 0
            t3(i) = 0
            tedadTakhireKutah(i) = 0
            tedadTakhireTulani(i) = 0
            miangineTakhirKutah(i) = 0
            a(i) = 0
            miangineTakhireTulani(i) = 0
            b(i) = 0
            zaribEshteghal(5) = 0
            c1(i) = 0
            f(i) = 0
            If i <= 4 Then
                miangineMandanDarSystem(i) = 0
                d(i) = 0
                d1(i) = 0
            End If
            s = False
        Next
    End Sub

    Private Sub main()
        While (t <= zamaneShabihSazi)
            If t = 0 Then
                jadvaleTrace()
            End If
            saat = ((t / 16) - Int(t / 16)) * 16 + 8
            l = Int(t / 16)
            If saat = 8 Then
                zamaneAvalinVorudeAdi = Exp(10 / 60)
                t += zamaneAvalinVorudeAdi
                FELbuilder(t, t, 1, vorudeAdi, shomareyeKhedmatdahande)
            End If
            Dim newFEL As List(Of List(Of Double)) = (From k In FEL Order By k(1) Ascending Select k).ToList
            Dim pishamadeGHaribolVoghu As List(Of Double) = (From k In FEL Order By k(1) Ascending Select k).FirstOrDefault
            If newFEL.Count > 0 Then
                t = pishamadeGHaribolVoghu(1)
            Else
                s = True
                jadvaleTrace()
                s = False
                For i As Integer = 1 To 5
                    f(i) = 0
                Next
                l = l + 1
                t = (l) * 16
                zamaneAvalinVorudeAdi = Exp(10 / 60)
                t += zamaneAvalinVorudeAdi
                FELbuilder(t, t, 1, vorudeAdi, shomareyeKhedmatdahande)
                newFEL = (From k In FEL Order By k(1) Ascending Select k).ToList
                pishamadeGHaribolVoghu = (From k In FEL Order By k(1) Ascending Select k).FirstOrDefault
                t = pishamadeGHaribolVoghu(1)
            End If
            saat = ((t / 16) - Int(t / 16)) * 16 + 8
            l = Int(t / 16)
            If saat > 11 And f(0) = 0 Then
                f(0) = 1
                t = l * 16 + 3
                t0 = t + U(0, 60 / 60)
                FELbuilder(t0, t0, 3, vorudeBaMinibus, Null)
            End If

            If (saat > 9.5 And f(1) = 0) Then
                f(1) = 1
                t = l * 16 + 1.5
                PishamadeShorueEsterahatKutah(1)
                PishamadeShorueEsterahatKutah(3)
            End If

            If (saat > 10 And f(2) = 0) Then
                f(2) = 1
                t = l * 16 + 2
                PishamadeShorueEsterahatKutah(2)
                PishamadeShorueEsterahatKutah(4)
            End If

            If (saat > 17.5 And f(3) = 0) Then
                f(3) = 1
                t = l * 16 + 9.5
                PishamadeShorueEsterahatKutah(1)
                PishamadeShorueEsterahatKutah(3)
            End If

            If (saat > 18 And f(3) = 0) Then
                f(3) = 1
                PishamadeShorueEsterahatKutah(1)
                PishamadeShorueEsterahatKutah(4)
            End If

            If saat > 19.5 And f(3) = 0 Then
                f(3) = 1
                t = l * 16 + 11.5
                PishamadeShorueEsterahatTulani(1)
                PishamadeShorueEsterahatTulani(3)
            End If

            If saat > 11.5 And f(4) = 0 Then
                f(4) = 1
                t = l * 16 + 3.5
                PishamadeShorueEsterahatTulani(1)
                PishamadeShorueEsterahatTulani(3)
            End If
            pishamadeGHaribolVoghu = (From k In FEL Order By k(1) Ascending Select k).FirstOrDefault
            t = pishamadeGHaribolVoghu(1)
            NoEeMosafer = pishamadeGHaribolVoghu(2)
            pishamadeFeli = pishamadeGHaribolVoghu(3)
            shomareyeKhedmatdahande = pishamadeGHaribolVoghu(4)
            Select Case pishamadeFeli
                Case vorudeAdi
                    PishamadeVorudeADi()
                Case vorudeBaMinibus
                    PishamadeVorudeBaMnibus(NoEeMosafer)
                Case etmameResidan
                    PishamadeEtmameResidan(NoEeMosafer)
                Case etmemeKhedmatDehi
                    PishamadeEtmameKhedmatdehi(shomareyeKhedmatdahande, NoEeMosafer)
                Case etmameEsterahatKutah
                    PishamadeEtmameEsterahsatKutah(shomareyeKhedmatdahande)
                Case etmameEsterahsatTulani
                    PishamadeEtmameEsterahsatTulani(shomareyeKhedmatdahande)
                Case etmamekhoruj
                    PishamadeEtmamekhoruj(NoEeMosafer)
            End Select
            jadvaleTrace()
            FEL.Remove(pishamadeGHaribolVoghu)
        End While
    End Sub


    Private Sub FELbuilder(ByRef t0 As Double, ByRef t As Double, ByVal NoEeMosafer As Integer, ByVal pishamad As Integer, shomareyeKhedmatdahande As Integer)
        Dim list As New List(Of Double)
        Dim c As Double
        list.Add(t0)
        Select Case pishamad
            Case vorudeAdi
                c = t + Exp(1 / 60)
                list.Add(c)
                c = saat = ((c / 16) - Int(c / 16)) * 16 + 8
            Case vorudeBaMinibus
                list.Add(t0)
            Case etmameResidan
                If NoEeMosafer = NoE1 Then
                    list.Add(t + Exp(2.4 / 60))
                ElseIf NoEeMosafer = NoE2 Or NoEeMosafer = Noe2BaOlaviat Then
                    list.Add(t + Exp(4.4 / 60))
                End If
            Case etmemeKhedmatDehi
                If shomareyeKhedmatdahande = 1 Or shomareyeKhedmatdahande = 2 Then
                    list.Add(t + Normal(7 / 60, 1.2 / 60))
                Else
                    list.Add(t + Exp(7 / 60))
                End If
            Case shorueEsterahatKutah
                list.Add(t)
            Case shorueEsterahatTulani
                list.Add(t)
            Case etmameEsterahatKutah
                list.Add(t + 0.25)
            Case etmameEsterahsatTulani
                list.Add(t + 0.5)
            Case etmamekhoruj
                list.Add(t + U(1 / 60, 2 / 60))
        End Select
        list.Add(NoEeMosafer)
        list.Add(pishamad)
        list.Add(shomareyeKhedmatdahande)
        If pishamad = vorudeAdi Then
            If c <= 23.15 Then
                FEL.Add(list)
            End If
        Else
            FEL.Add(list)
        End If
    End Sub


    Function Normal(ByVal m, ByVal v)
        Return v * Math.Sqrt(-2 * Math.Log(r.NextDouble)) * Math.Sin(r.NextDouble * 2 *
       Math.PI) + m
    End Function

    Function U(ByVal a, ByVal b)
        Return a + (b - a) * r.NextDouble
    End Function

    Function Exp(ByVal beta)
        Return -beta * Math.Log(r.NextDouble)
    End Function
    Function poisson(ByRef landa)
        Dim j As Integer = 1
        Dim s As Integer = 1
        While -(1 / landa) * s <= 1 And -(1 / landa) * s * r.NextDouble >= 1
            s = s * Math.Log(r.NextDouble)
            j += 1
        End While
        Return j
    End Function


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        With trace.Columns
            .Add("زمان")
            .Add("نوع پیشامد")
            .Add("پیشامد1")
            .Add("پیشامد2")
            .Add("پیشامد3")
            .Add("پیشامد4")
            .Add("پیشامد5")
            .Add("پیشامد6")
            .Add("پیشامد7")
            .Add("پیشامد8")
            .Add("پیشامد9")
            .Add("پیشامد10")
            .Add("تعداد در صف از نوع1")
            .Add("تعداد در صف از نوع2 عادی")
            .Add("تعداد در صف از نوع2 بااولویت")
            .Add("وضعیت خدمت دهنده1")
            .Add("وضعیت خدمت دهنده2")
            .Add("وضعیت خدمت دهنده3")
            .Add("وضعیت خدمت دهنده4")
            .Add("تعداد مواردی که خدمت دهنده 1 برای استراحت کوتاه تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 2 برای استراحت کوتاه تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 3 برای استراحت کوتاه تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 4 برای استراحت کوتاه تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 1 برای استراحت طولانی تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 2 برای استراحت طولانی تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 3 برای استراحت طولانی تاخیر داشته")
            .Add("تعداد مواردی که خدمت دهنده 4 برای استراحت طولانی تاخیر داشته")

            .Add("میانگین زمانی که خدمت دهنده 1 برای استراحت کوتاه تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 2 برای استراحت کوتاه تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 3 برای استراحت کوتاه تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 4 برای استراحت کوتاه تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 1 برای استراحت طولانی تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 2 برای استراحت طولانی تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 3 برای استراحت طولانی تاخیر داشته")
            .Add("میانگین زمانی که خدمت دهنده 4 برای استراحت طولانی تاخیر داشته")

            .Add("ضریب اشتغال خدمت دهنده1")
            .Add("ضریب اشتغال خدمت دهنده2")
            .Add("ضریب اشتغال خدمت دهنده3")
            .Add("ضریب اشتغال خدمت دهنده4")

            .Add("میانگین مدت ماندن در سیستم برای 1")
            .Add("میانگین مدت ماندن در سیستم برای 2")
            .Add("میانگین مدت ماندن در سیستم برای 3")

        End With
        Form2.DataGridView1.DataSource = trace
        Form2.DataGridView1.Columns(0).Width = 130
        Form2.DataGridView1.Columns(1).Width = 150
        For index = 2 To 4
            Form2.DataGridView1.Columns(index).Width = 240
        Next
        For index = 5 To 11
            Form2.DataGridView1.Columns(index).Width = 1
        Next
        For index = 12 To 14
            Form2.DataGridView1.Columns(index).Width = 30
        Next
        For index = 15 To 18
            Form2.DataGridView1.Columns(index).Width = 80
        Next
    End Sub

    Private Sub jadvaleTrace()
        Dim row = trace.NewRow
        If t = 0 Then
            row("زمان") = t
            row("نوع پیشامد") = "شروع شبیه سازی"
        Else
            If s = True Then
                row("زمان") = "اتمام روز کاری"
            Else
                row("زمان") = t
                If t < zamaneShabihSazi Then
                    Dim newFEL As List(Of List(Of Double)) = (From k In FEL Order By k(1) Ascending Select k).ToList
                    list = newFEL(0)
                    Dim z As String = list(2)
                    If list(3) = vorudeAdi Or list(3) = vorudeBaMinibus Or
                        list(3) = shorueEsterahatKutah Or list(3) = shorueEsterahatTulani Or list(3) = etmameEsterahatKutah Or list(3) = etmameEsterahsatTulani Then
                        row("نوع پیشامد") = nam(pishamadeFeli)
                    Else
                        row("نوع پیشامد") = nam(pishamadeFeli) + "مسافر نوع  " + z
                    End If
                    For i = 1 To newFEL.Count - 1
                        Dim first = newFEL(1)
                        Dim index As String = i
                        Dim zaman As String = first(1)
                        Dim shomare As String = first(2)
                        If i <= 10 Then
                            If first(3) = vorudeAdi Or first(3) = vorudeBaMinibus Or
                                list(3) = shorueEsterahatKutah Or list(3) = shorueEsterahatTulani Or list(3) = etmameEsterahatKutah Or list(3) = etmameEsterahsatTulani Then
                                row("پیشامد" + index) = nam(first(3)) + "در زمان " + zaman
                            Else
                                row("پیشامد" + index) = nam(first(3)) + "مسافر نوع " + shomare + "در زمان " + zaman
                            End If
                        End If
                        newFEL.Remove(first)
                    Next
                End If
                row("تعداد در صف از نوع1") = safeBarNoeAval
                row("تعداد در صف از نوع2 عادی") = safeBarNoeDovomeAdi
                row("تعداد در صف از نوع2 بااولویت") = safeBarNoeDovomeBaOlaviat
                row("وضعیت خدمت دهنده1") = nam(vaziateKhedmatdehande(1))
                row("وضعیت خدمت دهنده2") = nam(vaziateKhedmatdehande(2))
                row("وضعیت خدمت دهنده3") = nam(vaziateKhedmatdehande(3))
                row("وضعیت خدمت دهنده4") = nam(vaziateKhedmatdehande(4))

                row("تعداد مواردی که خدمت دهنده 1 برای استراحت کوتاه تاخیر داشته") = tedadTakhireKutah(1)
                row("تعداد مواردی که خدمت دهنده 2 برای استراحت کوتاه تاخیر داشته") = tedadTakhireKutah(2)
                row("تعداد مواردی که خدمت دهنده 3 برای استراحت کوتاه تاخیر داشته") = tedadTakhireKutah(3)
                row("تعداد مواردی که خدمت دهنده 4 برای استراحت کوتاه تاخیر داشته") = tedadTakhireKutah(4)

                row("تعداد مواردی که خدمت دهنده 1 برای استراحت طولانی تاخیر داشته") = tedadTakhireTulani(1)
                row("تعداد مواردی که خدمت دهنده 2 برای استراحت طولانی تاخیر داشته") = tedadTakhireTulani(2)
                row("تعداد مواردی که خدمت دهنده 3 برای استراحت طولانی تاخیر داشته") = tedadTakhireTulani(3)
                row("تعداد مواردی که خدمت دهنده 4 برای استراحت طولانی تاخیر داشته") = tedadTakhireTulani(4)

                row("میانگین زمانی که خدمت دهنده 1 برای استراحت کوتاه تاخیر داشته") = miangineTakhirKutah(1)
                row("میانگین زمانی که خدمت دهنده 2 برای استراحت کوتاه تاخیر داشته") = miangineTakhirKutah(2)
                row("میانگین زمانی که خدمت دهنده 3 برای استراحت کوتاه تاخیر داشته") = miangineTakhirKutah(3)
                row("میانگین زمانی که خدمت دهنده 4 برای استراحت کوتاه تاخیر داشته") = miangineTakhirKutah(4)
                row("میانگین زمانی که خدمت دهنده 1 برای استراحت طولانی تاخیر داشته") = miangineTakhirKutah(1)
                row("میانگین زمانی که خدمت دهنده 2 برای استراحت طولانی تاخیر داشته") = miangineTakhirKutah(2)
                row("میانگین زمانی که خدمت دهنده 3 برای استراحت طولانی تاخیر داشته") = miangineTakhirKutah(3)
                row("میانگین زمانی که خدمت دهنده 4 برای استراحت طولانی تاخیر داشته") = miangineTakhirKutah(4)

                row("ضریب اشتغال خدمت دهنده1") = zaribEshteghal(1)
                row("ضریب اشتغال خدمت دهنده2") = zaribEshteghal(2)
                row("ضریب اشتغال خدمت دهنده3") = zaribEshteghal(3)
                row("ضریب اشتغال خدمت دهنده4") = zaribEshteghal(4)

                row("میانگین مدت ماندن در سیستم برای 1") = miangineMandanDarSystem(1)
                row("میانگین مدت ماندن در سیستم برای 2") = miangineMandanDarSystem(2)
                row("میانگین مدت ماندن در سیستم برای 3") = miangineMandanDarSystem(3)
            End If
        End If
        trace.Rows.Add(row)
    End Sub
    Function nam(ByVal x)
        Select Case x
            Case etmemeKhedmatDehi
                Return "اتمام خدمت دهی "
            Case vorudeAdi
                Return "  ورود  "
            Case vorudeBaMinibus
                Return " ورود مینی بوس"
            Case etmameResidan
                Return " اتمام رسیدن "
            Case etmamekhoruj
                Return "اتمام خروج "
            Case shorueEsterahatKutah
                Return "شروع استراحت کوتاه "
            Case shorueEsterahatTulani
                Return " شروع استراحت طولانی"
            Case etmameEsterahsatTulani
                Return " اتمام استراحت طولانی"
            Case etmameEsterahatKutah
                Return "اتمام استراحت کوتاه "
            Case mashgul
                Return "مشغول"
            Case azad
                Return "آزاد"
            Case darHaleEsterahatKutah
                Return "در حال استراحت"
            Case darHaleEsterahatTulani
                Return "در حال استراحت"
            Case amadeEsterahatKutah
                Return "آماده استراحت"
            Case amadeEsterahatTulani
                Return "آماده استراحت"
        End Select
        Return ""
    End Function

    Private Sub PishamadeVorudeADi()
        If saat < 23.15 Then
            Dim r1 As Double
            r1 = r.NextDouble
            t0 = t
            If r1 <= 0.3 Then
                FELbuilder(t0, t, NoE1, etmameResidan, shomareyeKhedmatdahande)
            Else
                FELbuilder(t0, t, NoE2, etmameResidan, shomareyeKhedmatdahande)
            End If
            FELbuilder(t0, t, NoEeMosafer, vorudeAdi, shomareyeKhedmatdahande)
        End If
    End Sub

    Private Sub PishamadeVorudeBaMnibus(ByVal NoEeMosafer As Integer)
        t0 = t
        Dim n As Integer
        n = poisson(2)
        For i As Integer = 1 To n
            FELbuilder(t0, t, Noe2BaOlaviat, etmameResidan, shomareyeKhedmatdahande)
        Next
    End Sub


    Private Sub PishamadeEtmameResidan(ByVal NoEeMosafer As Integer)
        Select Case NoEeMosafer
            Case NoE1
                If vaziateKhedmatdehande(1) = azad Then
                    vaziateKhedmatdehande(1) = mashgul
                    t2(1) = t
                    shomareyeKhedmatdahande = 1
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)

                ElseIf vaziateKhedmatdehande(2) = azad Then
                    vaziateKhedmatdehande(2) = mashgul
                    t2(2) = t
                    shomareyeKhedmatdahande = 2
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                Else
                    safeBarNoeAval = +1
                End If
            Case NoE2
                If vaziateKhedmatdehande(3) = azad Then
                    vaziateKhedmatdehande(3) = mashgul
                    shomareyeKhedmatdahande = 3
                    t2(3) = t
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                ElseIf vaziateKhedmatdehande(4) = azad Then
                    vaziateKhedmatdehande(4) = mashgul
                    shomareyeKhedmatdahande = 4
                    t2(4) = t
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                Else
                    safeBarNoeDovomeAdi = +1
                End If
            Case Noe2BaOlaviat
                If vaziateKhedmatdehande(3) = azad Then
                    vaziateKhedmatdehande(3) = mashgul
                    shomareyeKhedmatdahande = 3
                    t2(3) = t
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                ElseIf vaziateKhedmatdehande(4) = azad Then
                    vaziateKhedmatdehande(4) = mashgul
                    shomareyeKhedmatdahande = 4
                    t2(4) = t
                    FELbuilder(t0, t, NoEeMosafer, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                Else
                    safeBarNoeDovomeBaOlaviat = +1
                End If
        End Select
    End Sub


    Private Sub PishamadeEtmameKhedmatdehi(ByVal shomareyeKhedmatdahande As Integer, NoEeMosafer As Integer)
        c1(shomareyeKhedmatdahande) += t - t2(shomareyeKhedmatdahande)
        zaribEshteghal(shomareyeKhedmatdahande) = c1(shomareyeKhedmatdahande) / t
        FELbuilder(t0, t, NoEeMosafer, etmamekhoruj, shomareyeKhedmatdahande)
        Select Case vaziateKhedmatdehande(shomareyeKhedmatdahande)
            Case amadeEsterahatKutah
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
                PishamadeShorueEsterahatKutah(shomareyeKhedmatdahande)
                a(shomareyeKhedmatdahande) += t - t1(shomareyeKhedmatdahande)
                miangineTakhirKutah(shomareyeKhedmatdahande) = a(shomareyeKhedmatdahande) / tedadTakhireKutah(shomareyeKhedmatdahande)
            Case amadeEsterahatTulani
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
                PishamadeShorueEsterahatTulani(shomareyeKhedmatdahande)
                b(shomareyeKhedmatdahande) += t - t3(shomareyeKhedmatdahande)
                miangineTakhireTulani(shomareyeKhedmatdahande) = b(shomareyeKhedmatdahande) / tedadTakhireTulani(shomareyeKhedmatdahande)
            Case mashgul
                If (shomareyeKhedmatdahande = 1 Or shomareyeKhedmatdahande = 2) Then
                    If safeBarNoeAval > 0 Then
                        safeBarNoeAval -= 1
                        t2(shomareyeKhedmatdahande) = t
                        FELbuilder(t0, t, NoE1, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                    Else
                        vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
                    End If
                End If
                If (shomareyeKhedmatdahande = 3 Or shomareyeKhedmatdahande = 4) Then
                    If safeBarNoeDovomeBaOlaviat > 0 Then
                        safeBarNoeDovomeAdi -= 1
                        t2(shomareyeKhedmatdahande) = t
                        FELbuilder(t0, t, Noe2BaOlaviat, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                    ElseIf safeBarNoeDovomeAdi > 0 Then
                        safeBarNoeDovomeAdi -= 1
                        t2(shomareyeKhedmatdahande) = t
                        FELbuilder(t0, t, NoE2, etmemeKhedmatDehi, shomareyeKhedmatdahande)
                    Else
                        vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
                    End If
                End If
        End Select

    End Sub

    Private Sub PishamadeEtmamekhoruj(NoEeMosafer As Integer)
        d(NoEeMosafer) += 1
        d1(NoEeMosafer) += (t - t0)
        miangineMandanDarSystem(NoEeMosafer) = d1(NoEeMosafer) / d(NoEeMosafer)
    End Sub

    Private Sub PishamadeShorueEsterahatKutah(shomareyeKhedmatdahande As Integer)
        If vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad Then
            vaziateKhedmatdehande(shomareyeKhedmatdahande) = darHaleEsterahatKutah
            FELbuilder(t0, t, Null, shorueEsterahatKutah, shomareyeKhedmatdahande)
            FELbuilder(t0, t, Null, etmameEsterahatKutah, shomareyeKhedmatdahande)
        Else
            vaziateKhedmatdehande(shomareyeKhedmatdahande) = amadeEsterahatKutah
            t1(shomareyeKhedmatdahande) = t
            tedadTakhireKutah(shomareyeKhedmatdahande) += 1
        End If
    End Sub

    Private Sub PishamadeShorueEsterahatTulani(shomareyeKhedmatdahande As Integer)
        If vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad Then
            vaziateKhedmatdehande(shomareyeKhedmatdahande) = darHaleEsterahatTulani
            FELbuilder(t0, t, Null, shorueEsterahatTulani, shomareyeKhedmatdahande)
            FELbuilder(t0, t, Null, etmameEsterahsatTulani, shomareyeKhedmatdahande)
        Else
            vaziateKhedmatdehande(shomareyeKhedmatdahande) = amadeEsterahatTulani
            tedadTakhireTulani(shomareyeKhedmatdahande) += 1
            t3(shomareyeKhedmatdahande) = t
        End If
    End Sub

    Private Sub PishamadeEtmameEsterahsatKutah(shomareyeKhedmatdahande As Integer)
        If (shomareyeKhedmatdahande = 1 Or shomareyeKhedmatdahande = 2) Then
            If safeBarNoeAval > 0 Then
                safeBarNoeAval -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, NoE1, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            Else
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
            End If
        End If
        If (shomareyeKhedmatdahande = 3 Or shomareyeKhedmatdahande = 4) Then
            If safeBarNoeDovomeBaOlaviat > 0 Then
                safeBarNoeDovomeAdi -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, Noe2BaOlaviat, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            ElseIf safeBarNoeDovomeAdi > 0 Then
                safeBarNoeDovomeAdi -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, NoE2, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            Else
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
            End If
        End If
    End Sub

    Private Sub PishamadeEtmameEsterahsatTulani(shomareyeKhedmatdahande As Integer)
        If (shomareyeKhedmatdahande = 1 Or shomareyeKhedmatdahande = 2) Then
            If safeBarNoeAval > 0 Then
                safeBarNoeAval -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, NoE1, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            Else
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
                If shomareyeKhedmatdahande = 1 Then
                    PishamadeShorueEsterahatTulani(2)
                End If
            End If
        End If
        If (shomareyeKhedmatdahande = 3 Or shomareyeKhedmatdahande = 4) Then
            If safeBarNoeDovomeBaOlaviat > 0 Then
                safeBarNoeDovomeBaOlaviat -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, Noe2BaOlaviat, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            ElseIf safeBarNoeDovomeAdi > 0 Then
                safeBarNoeDovomeAdi -= 1
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = mashgul
                FELbuilder(t0, t, NoE2, etmemeKhedmatDehi, shomareyeKhedmatdahande)
            Else
                vaziateKhedmatdehande(shomareyeKhedmatdahande) = azad
            End If
            If shomareyeKhedmatdahande = 3 Then
                PishamadeShorueEsterahatTulani(4)
            End If
        End If
    End Sub

End Class
