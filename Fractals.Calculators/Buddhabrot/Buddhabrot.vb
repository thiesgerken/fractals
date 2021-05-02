' Copyright (C) 2010-2012 Thies Gerken

' This file is part of Fractals.

' Fractals is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.

' Fractals is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with Fractals. If not, see <http://www.gnu.org/licenses/>.

Imports System.Threading
Imports System.Drawing
Imports System.Globalization
Imports Cloo
Imports Fluent
Imports Fractals.Mathematics
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging
Imports System.Threading.Tasks
Imports Fractals.Painters
Imports Fractals.Utilities
Imports System.Linq
Imports CommandLineParser

Public Class Buddhabrot
    Inherits Fractal

    Public Enum ComputingModes
        Normal
        Render
        Elaborate
    End Enum

#Region "User params"

    Private _minIter As Integer

#End Region

#Region "Constants"

    'viewport size
    Private Const _defaultViewPortWidth = 2.4
    Private Const _defaultViewPortHeight = 2.4
    Private Const _defaultOffsetX = -0.4
    Private Const _defaultOffsetY = 0

#End Region

#Region "OpenCL fields"

    Private Shared _clInited As Boolean
    Private Shared _clBuddhabrotKernel As ComputeKernel
    Private Shared _clShiftingKernel As ComputeKernel
    Private Shared _clProgram As ComputeProgram
    Private Shared _clCommands As ComputeCommandQueue
    Private Shared _clEvents As ComputeEventList
    Private _randomBuffer As ComputeBuffer(Of Vectors.UInt2)
    Private _outputBuffer As ComputeBuffer(Of UInteger)
    Private _output() As UInteger
    Private _outputGC As GCHandle
    Private _rand As Random
    Private _loopCount As Integer

    'speed measurement
    Private _overallSamples As Long
    Private _overallTime As Long
    Private _samplesDone As Long

    'time to wait before spawning the next wave of workers
    Private Shared _waitTime As Integer = 10

#End Region

#Region "Properties"

    Dim suffixConverter As Func(Of String, Long) = Function(value As String) As Long
                                                       Dim factor = 1

                                                       If value.Contains("k") Then
                                                           value = value.Replace("k", "")

                                                           factor = 1000
                                                       ElseIf value.Contains("M") Then
                                                           value = value.Replace("M", "")

                                                           factor = 1000000
                                                       Else
                                                           Return Long.Parse(value, CultureInfo.InvariantCulture)
                                                       End If

                                                       Return CLng(Double.Parse(value, CultureInfo.InvariantCulture) * factor)
                                                   End Function

    Private _minIterArgument As New ValueArgument(Of Integer)("miniter", "", 0, "Minimum Iteration Count")
    Private _brightnessArgument As New ValueArgument(Of Double)("brightness", "", 0, "Percentage of pixels that should appear white, has to be in ]0;1[")
    Private _saveEveryArgument As New ValueArgument(Of Integer)("saveevery", "", 0, "Save data every n-th pass")
    Private _paletteArgument As New ValueArgument(Of Utilities.ColorPalette)("palette", "", Nothing, "Colorpalette ('id:[name]' (see below for ids), the location of a bitmap file, or '([location1]=[color1];[location2]=[color2];...)')")
    Private _sampleCountArgument As New ValueArgument(Of Long)("samplecount", "", 0, "Amount of samples to use", suffixConverter)
    Private _modeArgument As New EnumeratedValueArgument("cmode", "", "", "Calculation mode, one of [render|elaborate|normal]", {"render", "elaborate", "normal"}.ToList)
    Private _dataFilesArgument As New ValueListArgument(Of String)("storage", "", New List(Of String), "Location of datafile(s) to load/store results from/in")

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property CommandLineArguments As Argument()
        Get
            Return New Argument() {_minIterArgument, _saveEveryArgument, _sampleCountArgument, _modeArgument, _dataFilesArgument, _brightnessArgument, _paletteArgument}
        End Get
    End Property

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overrides Sub EvaluateCommandLineArguments()
        If _modeArgument.IsParsed Then
            If _modeArgument.Value = "render" Then
                Mode = ComputingModes.Render
            ElseIf _modeArgument.Value = "elaborate" Then
                Mode = ComputingModes.Elaborate
            Else
                Mode = ComputingModes.Normal
            End If
        End If

        If _minIterArgument.IsParsed Then
            If Mode = ComputingModes.Render Then Throw New Exception("--miniter is only valid when calculating")

            _minIter = _minIterArgument.Value
        End If

        If _saveEveryArgument.IsParsed Then
            If Mode <> ComputingModes.Elaborate Then Throw New Exception("--saveevery is only valid when elaborating on data")

            SaveEvery = _saveEveryArgument.Value
        End If

        If _sampleCountArgument.IsParsed Then
            If Mode <> ComputingModes.Normal Then Throw New Exception("--samplecount is only valid when calculation normally")

            SampleCount = _sampleCountArgument.Value
        End If

        If _brightnessArgument.IsParsed Then
            If Mode = ComputingModes.Elaborate Then Throw New Exception("--brightness is only valid when rendering data")

            If _brightnessArgument.Value <= 0 Or _brightnessArgument.Value >= 1 Then
                Throw New Exception("--brightness takes only values from ]0;1[")
            End If

            _brightness = _brightnessArgument.Value
        End If

        If _paletteArgument.IsParsed Then
            If Mode = ComputingModes.Elaborate Then Throw New Exception("--palette is only valid when rendering data")

            Palette = _paletteArgument.Value
        End If

        If _dataFilesArgument.IsParsed Then
            If Mode = ComputingModes.Normal Then Throw New Exception("--storage is only valid when calculation normally")

            DataFiles = _dataFilesArgument.Value.ToArray
        Else
            If Mode = ComputingModes.Elaborate Then Throw New Exception("--storage has to be specified when elaborating")
        End If
    End Sub

    Private _brightness As Double = 0.1

    ''' <summary>
    ''' Gets or sets the brightness.
    ''' </summary>
    ''' <value>The brightness.</value>
    Public Property Brightness As Double
        Get
            Return _brightness
        End Get
        Set(ByVal value As Double)
            _brightness = value

            OnPropertyChanged("Brightness")
            OnFractalSettingChanged("Brightness")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the MinIter.
    ''' </summary>
    ''' <value>The MinIter.</value>
    Public Property MinIter As Integer
        Get
            Return _minIter
        End Get
        Set(ByVal value As Integer)
            _minIter = value

            OnPropertyChanged("MinIter")
            OnFractalSettingChanged("MinIter")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the sample count.
    ''' </summary>
    ''' <value>The sample count.</value>
    Public Property SampleCount As Long = CLng(10 ^ 8)

    ''' <summary>
    ''' Gets or sets the palette.
    ''' </summary>
    ''' <value>The palette.</value>
    Public Property Palette As Utilities.ColorPalette = New Utilities.ColorPalette("grey")

    ''' <summary>
    ''' Gets or sets the save every.
    ''' </summary>
    ''' <value>The save every.</value>
    Public Property SaveEvery As Integer = 20

    ''' <summary>
    ''' Gets or sets the name of the data file.
    ''' </summary>
    ''' <value>The name of the data file.</value>
    Public Property DataFiles As String() = {}

    ''' <summary>
    ''' Gets or sets the mode.
    ''' </summary>
    ''' <value>The mode.</value>
    Public Property Mode As ComputingModes = ComputingModes.Normal

    ''' <summary>
    ''' Gets or sets the results.
    ''' </summary>
    ''' <value>The results.</value>
    Public Property Results As UInteger()
        Get
            Return _output
        End Get
        Set(ByVal value As UInteger())
            _output = value
        End Set
    End Property

    ''' <summary>
    ''' Gets the progress.
    ''' </summary>
    ''' <value>The progress.</value>
    Public Overrides ReadOnly Property Progress As Double
        Get
            If _totalPixels = 0 Or _progress = 0 Then Return 0
            Return Math.Min(2, _progress / (_totalPixels))
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Buddhabrot"
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this fractal is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As RibbonTabItem
        Get
            Dim tab As New RibbonTabItem

            Dim limitGroupBox As New RibbonGroupBox
            limitGroupBox.Header = "Colors"

            Dim GenerateTextBox = Function(parameter As String)
                                      parameter = parameter(0).ToString.ToUpper & parameter.Substring(1)

                                      Dim box As New TextBox

                                      box.Header = parameter & ":"

                                      Dim title = parameter
                                      title = title.Replace("Min", "Minimum Iteration Count for ")
                                      title = title.Replace("Max", "Maximum Iteration Count for ")

                                      title = title.Replace("R", "Red")
                                      title = title.Replace("G", "Green")
                                      title = title.Replace("B", "Blue")

                                      Dim words = title.Split(" "c)

                                      Dim type = words(0)
                                      Dim color = words(words.Length - 1)

                                      box.ToolTip = New ScreenTip With {.Title = title, .Text = type & " Iteration Count to paint a orbit " & color & "."}
                                      box.SetBinding(TextBox.TextProperty, parameter)
                                      box.InputWidth = 70

                                      Return box
                                  End Function

            limitGroupBox.Items.Add(GenerateTextBox("minR"))
            limitGroupBox.Items.Add(GenerateTextBox("minG"))
            limitGroupBox.Items.Add(GenerateTextBox("minB"))
            limitGroupBox.Items.Add(GenerateTextBox("maxR"))
            limitGroupBox.Items.Add(GenerateTextBox("maxG"))
            limitGroupBox.Items.Add(GenerateTextBox("maxB"))

            tab.Groups.Add(limitGroupBox)

            'Dim otherGroupBox As New RibbonGroupBox
            'otherGroupBox.Header = "Other"

            'Dim resolutionTextBox As New TextBox
            'resolutionTextBox.Header = "Resolution:"
            'resolutionTextBox.SetBinding(TextBox.TextProperty, "Resolution")
            'resolutionTextBox.ToolTip = New ScreenTip With {.Title = "Resolution", .Text = "Factor determining how many subpixels are calculated per axis." & Environment.NewLine & "Calculating More Samples means having more orbits to paint."}
            'resolutionTextBox.InputWidth = 40

            'otherGroupBox.Items.Add(resolutionTextBox)
            'tab.Groups.Add(otherGroupBox)

            Return tab
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [requires specific painter].
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if [requires specific painter]; otherwise, <c>false</c>.
    ''' </value>
    Public Overrides ReadOnly Property PaintersChangeable As Boolean
        Get
            Return False
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [supports anti aliasing].
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if [supports anti aliasing]; otherwise, <c>false</c>.
    ''' </value>
    Public Overrides ReadOnly Property SupportsAntiAliasing As Boolean
        Get
            Return False
        End Get
    End Property

    ''' <summary>
    ''' Gets the description.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public Overrides ReadOnly Property PreviewImage As Bitmap
        Get
            Return My.Resources.Buddhabrot
        End Get
    End Property

    ''' <summary>
    ''' Gets the speed (samples / s).
    ''' </summary>
    ''' <value>The speed.</value>
    Public Overrides ReadOnly Property Speed As Double
        Get
            If _overallTime = 0 Then Return 0

            Return _samplesDone / (_overallTime / 1000)
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [supports smoothing].
    ''' </summary>
    ''' <value><c>true</c> if [supports smoothing]; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property SupportsSmoothing As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets the initial rotation.
    ''' </summary>
    ''' <value>The initial rotation.</value>
    Protected Overrides ReadOnly Property DefaultRotation As Double
        Get
            Return Math.PI / 2
        End Get
    End Property

    ''' <summary>
    ''' Gets the default offset on the viewport.
    ''' </summary>
    ''' <value>The default offset.</value>
    Protected Overrides ReadOnly Property DefaultOffset As ComplexNumber
        Get
            Return New ComplexNumber(_defaultOffsetX, _defaultOffsetY)
        End Get
    End Property

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected Overrides ReadOnly Property DefaultViewportSize As System.Drawing.SizeF
        Get
            Return New SizeF(_defaultViewPortWidth, _defaultViewPortHeight)
        End Get
    End Property

    ''' <summary>
    ''' Gets the default escape value.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected Overrides ReadOnly Property DefaultBailoutValue As Double
        Get
            Return 4
        End Get
    End Property

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected Overrides ReadOnly Property DefaultMaxIterationCount As Integer
        Get
            Return 800
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected Overrides ReadOnly Property Source As String
        Get
            Return ""
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Multibrot" /> class.
    ''' </summary>
    Public Sub New()
        MyBase.New()

        Me.OutsidePainter = New DefaultPainter
        Me.InsidePainter = New DefaultPainter
    End Sub

#End Region

#Region "Methods"

#Region "GPU"

    Private _watch As New Stopwatch

    ''' <summary>
    ''' Calculates the GPU.
    ''' </summary>
    Protected Overrides Sub RenderOpenCL()
        _loopCount = CInt(Math.Ceiling(SampleCount / WorkSize))
        _overallTime = 0
        _samplesDone = 0
        _overallSamples = SampleCount

        _totalPixels = _loopCount + 1
        _progress = 0

        If Mode <> ComputingModes.Normal Then
            If Mode = ComputingModes.Render Then
                If DataFiles.Length = 0 Then
                    Throw New Exception("There is no data to render")
                End If

                Dim lastSize As Size = New Size(-1, 0)
                For Each f In DataFiles
                    If IO.File.Exists(f) Then
                        Dim currentSize = ReadSize(f)

                        If Not (lastSize.Width = -1 Or currentSize = lastSize) Then
                            Throw New Exception("The sizes of the data files are not consistent")
                        End If

                        lastSize = currentSize
                    Else
                        Throw New Exception("Data file '" & f & "' could not be found")
                    End If
                Next

                If lastSize.Width = -1 Then
                    Throw New Exception("There is no data to render")
                End If

                If lastSize <> Me.Size Then
                    WriteLogMessage("The Datafile" & If(DataFiles.Length <> 1, "s", "") & " contain" & If(DataFiles.Length = 1, "s", "") & " data of " & lastSize.Width & "x" & lastSize.Height & ", adjusting fractal size to match it.")

                    Me.Size = lastSize
                End If
            End If

            ReDim _output(Size.Width * Size.Height - 1)

            For Each f In DataFiles
                If Not IO.File.Exists(f) Then
                    WriteLogMessage("Could not find data file '" & f & "'")
                Else
                    If ReadSize(f) <> Me.Size Then
                        WriteLogMessage("Data in data file '" & f & "' could not be recognized")
                    Else
                        WriteLogMessage("Including data from '" & f & "'")

                        LoadData(f)
                    End If
                End If
            Next
        Else
            'create new output array
            _output = New UInteger(Size.Width * Size.Height - 1) {}
        End If

        If Mode = ComputingModes.Render Then
            RenderResults()
        Else
            If Not _clInited Then
                WriteLogMessage("Compiling OpenCL kernels")

                'setup opencl objects
                _clCommands = New ComputeCommandQueue(_clContext, _clContext.Devices(0), ComputeCommandQueueFlags.None)
                _clEvents = New ComputeEventList()
                _clProgram = New ComputeProgram(_clContext, New String() {My.Resources.BuddhabrotKernel})

                'build kernels
                _clProgram.Build(Nothing, "-cl-no-signed-zeros -cl-finite-math-only", Nothing, IntPtr.Zero)
                _clBuddhabrotKernel = _clProgram.CreateKernel("buddhabrot")
                _clShiftingKernel = _clProgram.CreateKernel("xorshift")

                _clInited = True
            Else
                WriteLogMessage("OpenCL kernels are already compiled")
            End If

            _outputGC = GCHandle.Alloc(_output, GCHandleType.Pinned)

            'create buffers
            _randomBuffer = New ComputeBuffer(Of Vectors.UInt2)(_clContext, ComputeMemoryFlags.ReadWrite, WorkSize)
            _outputBuffer = New ComputeBuffer(Of UInteger)(_clContext, ComputeMemoryFlags.ReadWrite, Size.Width * Size.Height)

            'set arguments
            _clShiftingKernel.SetValueArgument(4, WorkSize)
            _clShiftingKernel.SetMemoryArgument(5, _randomBuffer)

            _clBuddhabrotKernel.SetValueArgument(0, _minIter)
            _clBuddhabrotKernel.SetValueArgument(1, _maxIterations)
            _clBuddhabrotKernel.SetValueArgument(2, New Vectors.Int2(Size.Width, Size.Height))
            _clBuddhabrotKernel.SetValueArgument(3, New Vectors.Float2 With {.X = _defaultViewPortWidth, .Y = _defaultViewPortHeight})
            _clBuddhabrotKernel.SetValueArgument(4, CSng(BailoutValue))
            _clBuddhabrotKernel.SetValueArgument(5, New Vectors.Float2 With {.X = CSng(_defaultOffsetX), .Y = CSng(_defaultOffsetY)})
            _clBuddhabrotKernel.SetValueArgument(6, New Vectors.Float2 With {.X = CSng(Math.Sin(Rotation)), .Y = CSng(Math.Cos(Rotation))})
            _clBuddhabrotKernel.SetValueArgument(7, Convert.ToInt32(SmoothIterationCounts))
            _clBuddhabrotKernel.SetMemoryArgument(8, _randomBuffer)
            _clBuddhabrotKernel.SetMemoryArgument(9, _outputBuffer)

            If Mode = ComputingModes.Normal Then
                WriteLogMessage("Starting normal Buddhabrot rendering with " & _overallSamples & " samples divided in " & _loopCount & " passes")
            Else
                WriteLogMessage("Starting elaboration on Buddhabrot data")
            End If

            _clCommands.Write(_outputBuffer, True, 0, Size.Width * Size.Height, _outputGC.AddrOfPinnedObject(), _clEvents)

            If Mode = ComputingModes.Normal Then
                For i As Integer = 1 To _loopCount
                    DoPass(i)
                Next

                'read result into _output
                _clCommands.Read(_outputBuffer, True, 0, Size.Width * Size.Height, _outputGC.AddrOfPinnedObject(), _clEvents)
                _clCommands.Finish()

                RenderResults()
            ElseIf Mode = ComputingModes.Elaborate Then
                Dim i As Long = 1

                While True
                    DoPass(i)

                    If i Mod SaveEvery = 0 Then
                        _watch.Start()

                        'read result into _output
                        _clCommands.Read(_outputBuffer, True, 0, Size.Width * Size.Height, _outputGC.AddrOfPinnedObject(), _clEvents)
                        _clCommands.Finish()

                        WriteLogMessage("Copied Data: " & _watch.ElapsedMilliseconds & "ms")

                        _watch.Restart()

                        For Each f In DataFiles
                            SaveData(f)
                        Next

                        WriteLogMessage("Saved Data: " & _watch.ElapsedMilliseconds & "ms")

                        _watch.Reset()
                    End If

                    If i Mod 50 = 0 Then
                        GC.Collect()
                    End If

                    i += 1
                End While
            End If

            'free not needed resources
            _randomBuffer.Dispose()
            _outputBuffer.Dispose()
            _outputGC.Free()

            WriteLogMessage("Finished Calculating with average of " & Math.Round(_overallTime / _overallSamples * 1000000, 2) & " ns/Sample")
        End If

        'finished
        _progress = CInt(_totalPixels)
    End Sub

    ''' <summary>
    ''' Does the pass.
    ''' </summary>
    ''' <param name="i">The i.</param>
    Private Sub DoPass(ByVal i As Long)
        While Me.State = WorkingState.Paused
            Thread.Sleep(100)
        End While

        _watch.Reset()
        _watch.Start()

        'shift rand numbers
        _rand = New Random(Date.Now.Millisecond + Date.Now.Second + Date.Now.Minute + Date.Now.Hour)
        _clShiftingKernel.SetValueArgument(0, CUInt(_rand.Next()))
        _clShiftingKernel.SetValueArgument(1, CUInt(_rand.Next()))
        _clShiftingKernel.SetValueArgument(2, CUInt(_rand.Next()))
        _clShiftingKernel.SetValueArgument(3, CUInt(_rand.Next()))
        _clCommands.Execute(_clShiftingKernel, Nothing, {1}, Nothing, _clEvents)

        'execute buddhabrot kernel
        Dim count = If(i = _loopCount And Mode <> ComputingModes.Elaborate, _overallSamples - WorkSize * (i - 1), WorkSize)
        _clCommands.Execute(_clBuddhabrotKernel, Nothing, {count}, Nothing, _clEvents)

        _clCommands.Finish()

        _watch.Stop()

        _overallTime += _watch.ElapsedMilliseconds
        _samplesDone += count

        Dim countStr As String
        If count >= 1000000 Then
            countStr = Math.Round(count / 1000000, 2) & "M Sample" & If(count = 1000000, "", "s")
        Else
            countStr = Math.Round(count / 1000, 2) & "k Sample" & If(count = 1000, "", "s")
        End If

        WriteLogMessage("Pass " & i & If(Mode = ComputingModes.Normal, " of " & _loopCount, "") & ": " & _watch.ElapsedMilliseconds & " ms @ " & countStr)

        _watch.Stop()

        If Mode = ComputingModes.Normal Then _progress += 1

        'prevent long display freezes
        If i <> _loopCount Then Thread.Sleep(_waitTime)
    End Sub

    ''' <summary>
    ''' Renders the results.
    ''' </summary>
    Private Sub RenderResults()
        WriteLogMessage("Rendering Results")

        Dim maxfound As UInteger

        'find out highest value
        Dim threadCount As Integer = 10
        Dim finished As Integer = 0
        Dim localMax(threadCount - 1) As UInteger
        Dim regionWidth = CInt(Math.Floor(_output.Length / localMax.Length))

        For i As Integer = 0 To 9
            Dim nr = i
            Dim localSearch As New Thread(Sub()
                                              For ii As Integer = nr * regionWidth To If(nr = localMax.Length - 1, _output.Length - 1, (nr + 1) * regionWidth - 1)
                                                  If (_output(ii) > localMax(nr)) Then localMax(nr) = _output(ii)
                                              Next

                                              Interlocked.Increment(finished)
                                          End Sub)

            localSearch.Start()
        Next

        While Not finished = threadCount
            Thread.Sleep(5)
        End While

        For i As Integer = 0 To localMax.Length - 1
            If localMax(i) > maxfound Then maxfound = localMax(i)
        Next

        'histogram
        Dim histogram(10000) As Integer
        finished = 0

        For i As Integer = 0 To 9
            Dim nr = i
            Dim histogramBuilder As New Thread(Sub()
                                                   For ii As Integer = nr * regionWidth To If(nr = localMax.Length - 1, _output.Length - 1, (nr + 1) * regionWidth - 1)
                                                       If _output(ii) > 0 Then Interlocked.Increment(histogram(CInt(Math.Round(_output(ii) / maxfound * (histogram.Length - 1)))))
                                                   Next

                                                   Interlocked.Increment(finished)
                                               End Sub)

            histogramBuilder.Start()
        Next

        While Not finished = threadCount
            Thread.Sleep(5)
        End While

        'find out how many pixels have a value > 0 
        Dim realCount As Long = 0
        For i As Integer = 0 To histogram.Length - 1
            realCount += histogram(i)
        Next

        'set overExp so that brightness*realcount pixels are painted white
        Dim overExp As Double = 1
        Dim currentVal As Double = 0

        For i As Integer = histogram.Length - 1 To 0 Step -1
            currentVal += histogram(i) / realCount

            If currentVal >= Brightness Then
                overExp = (histogram.Length - 1) / i
                Exit For
            End If
        Next

        If Double.IsInfinity(overExp) Then overExp = histogram.Length - 1

        WriteLogMessage("Applying overexposure of " & Math.Round(overExp, 2))

        'set pixel colors
        If UseFastBitmap Then
            'write a bitmap line by line

            FastBitmapOutput.Position = 0

            Dim bmp As New FastBitmap(FastBitmapOutput)
            bmp.Width = Me.Size.Width
            bmp.Height = Me.Size.Height
            bmp.SetPalette(Palette)
            bmp.ColorDepth = 8
            bmp.IsIndexed = True

            Dim lineBuffer(Me.Size.Width - 1) As Byte

            For y As Integer = 0 To Me.Size.Height - 1
                Dim yy = y

                Parallel.For(0, Me.Size.Width, Sub(i) lineBuffer(i) = CByte(Math.Min(_output(yy * Me.Size.Width + i) / maxfound * 255 * overExp, 255)))
                bmp.WriteLine(lineBuffer)
            Next

            ReDim lineBuffer(0)
            bmp.Close()
        Else
            'conventional creation of bitmap object

            Dim buffer(Size.Width * Size.Height - 1) As Byte
            Parallel.For(0, _output.Length, Sub(i) buffer(i) = CByte(Math.Min(_output(i) / maxfound * 255 * overExp, 255)))

            'build image
            _image = New Bitmap(Me.Size.Width, Me.Size.Height, PixelFormat.Format8bppIndexed)

            Dim data = _image.LockBits(New Rectangle(0, 0, Size.Width, Size.Height), ImageLockMode.ReadWrite, PixelFormat.Format8bppIndexed)
            Dim pal = _image.Palette

            For i As Integer = 0 To 255
                pal.Entries(i) = Palette(i / 255)
            Next

            _image.Palette = pal

            'copy the data back to the image
            Marshal.Copy(buffer, 0, data.Scan0, buffer.Length)

            _image.UnlockBits(data)
            ReDim buffer(0)
        End If
    End Sub


#End Region

#Region "Loading / Saving"
    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("SampleCount")
        x.WriteValue(SampleCount)
        x.WriteEndElement()

        x.WriteStartElement("MinimumIterationCount")
        x.WriteValue(MinIter)
        x.WriteEndElement()

        x.WriteStartElement("Brightness")
        x.WriteValue(Brightness)
        x.WriteEndElement()

        x.WriteStartElement("Palette")
        Palette.Save(x)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        SampleCount = CLng(x.Element("SampleCount").Value)
        MinIter = CInt(x.Element("MinimumIterationCount").Value)
        Brightness = CInt(x.Element("Brightness").Value)
        Palette = Utilities.ColorPalette.Load(x.Element("Palette"))
    End Sub

    ''' <summary>
    ''' Reads the size.
    ''' </summary>
    ''' <param name="file">The file.</param>
    ''' <returns></returns>
    Private Function ReadSize(ByVal file As String) As Size
        Try
            Dim inputstream As New FileStream(file, IO.FileMode.Open)
            Dim br As New BinaryReader(inputstream)

            Dim width = br.ReadInt32()
            Dim height = br.ReadInt32()

            inputstream.Dispose()

            Return New Size(width, height)
        Catch ex As Exception
            Return New Size(-1, -1)
        End Try
    End Function

    ''' <summary>
    ''' Loads the data.
    ''' </summary>
    Private Sub LoadData(ByVal file As String)
        Try
            Dim inputstream As New FileStream(file, IO.FileMode.Open)
            Dim br As New BinaryReader(inputstream)

            br.ReadInt32()
            br.ReadInt32()

            For i As Integer = 0 To _output.Length - 1
                _output(i) += br.ReadUInt32
            Next

            inputstream.Dispose()
        Catch ex As Exception
        End Try
    End Sub

    ''' <summary>
    ''' Saves the data.
    ''' </summary>
    Private Sub SaveData(ByVal file As String)
        Try
            Dim outputStream As New FileStream(file & ".tmp", IO.FileMode.Create)
            Dim writer As New BinaryWriter(outputStream)

            writer.Write(Size.Width)
            writer.Write(Size.Height)

            For i As Integer = 0 To _output.Length - 1
                writer.Write(_output(i))
            Next

            writer.Flush()
            outputStream.Dispose()

            IO.File.Delete(file)
            IO.File.Move(file & ".tmp", file)
        Catch ex As Exception
        End Try
    End Sub

#End Region
#End Region
End Class
