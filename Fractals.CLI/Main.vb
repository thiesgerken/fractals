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

Imports System.IO
Imports System.Drawing
Imports System.Reflection
Imports Cloo
Imports Fractals.Calculators
Imports System.Drawing.Imaging
Imports System.Text
Imports System.Threading
Imports Fractals.Utilities
Imports System.Globalization
Imports Fractals.Painters
Imports CommandLineParser
Imports Fractals.Mathematics

Module Main

    Public BuildRev As String = ""
    Public BuildDate As String = ""
    Public BuildTime As String = ""

    Private _devices As List(Of ComputeDevice)

    Private _target As Renderer = Nothing
    Private _targetFractal As Fractal = Nothing
    Private _targetVideo As FractalVideo = Nothing
    Private _imageFormat As ImageFormat
    Private _guessedExt As String

    Private _aborted As Boolean
    Private _lastProgressUpdate As Date

    Private _sizeConverter As Func(Of String, Size) = Function(value As String) As Size
                                                          If value.Contains("x"c) Then
                                                              Dim splits() = value.Split({"x"c}, StringSplitOptions.RemoveEmptyEntries)

                                                              If splits.Length <> 2 Then Throw New Exception("'" & value & "' is not recognized as a size setting.")

                                                              Return New Size(_suffixConverter(splits(0)), _suffixConverter(splits(1)))
                                                          Else
                                                              Throw New Exception("'" & value & "' is not recognized as a size setting.")
                                                          End If
                                                      End Function

    Private _superSamplingConverter As Func(Of String, Integer) = Function(value As String) As Integer
                                                                      If value.Contains("x"c) Then
                                                                          Dim splits() = value.Split({"x"c}, StringSplitOptions.RemoveEmptyEntries)

                                                                          If splits.Length <> 2 Then Throw New Exception("'" & value & "' is not recognized as a supersampling setting.")

                                                                          If splits(0) <> splits(1) Then Throw New Exception("'" & value & "' is not recognized as a supersampling setting.")

                                                                          Return CInt(splits(0)) * CInt(splits(0))
                                                                      Else
                                                                          If CInt(CInt(value) ^ 0.5) ^ 2 <> CInt(value) Then Throw New Exception("when using supersampling=[m], m has to be a n^2.")

                                                                          Return CInt(value)
                                                                      End If
                                                                  End Function

    Private _suffixConverter As Func(Of String, Integer) = Function(value As String) As Integer
                                                               Dim factor = 1

                                                               If value.Contains("k") Then
                                                                   value = value.Replace("k", "")

                                                                   factor = 1000
                                                               ElseIf value.Contains("M") Then
                                                                   value = value.Replace("M", "")

                                                                   factor = 1000000
                                                               Else
                                                                   Return Integer.Parse(value, CultureInfo.InvariantCulture)
                                                               End If

                                                               Return CInt(Double.Parse(value, CultureInfo.InvariantCulture) * factor)
                                                           End Function

    Private _colorConverter As Func(Of String, Color) = Function(value As String) As Color
                                                            Dim c = Color.FromName(value)

                                                            If c.ToArgb = 0 Then Throw New Exception("Unknown Color")
                                                            Return c
                                                        End Function

    Private _imageFormats As String() = {"png", "jpg", "bmp", "html", "txt", "gif"}
    Private _videoFormats As String() = {"avi", "mp4", "wmv", "flv"}

    Private _cmd As New CommandLineParser.CommandLineParser

    Private _outputArgument As New ValueArgument(Of String)("output", "o", "", "Output filename (Required)")
    Private _inputArgument As New ValueArgument(Of String)("input", "i", "", "The filename of the settingsfile to use")
    Private _saveArgument As New ValueArgument(Of String)("save", "", "", "save the current settings to a settingsfile")
    Private _helpArgument As New SwitchArgument("help", "h", False, "Prints usage information")
    Private _quietArgument As New SwitchArgument("quiet", "q", False, "Suppresses output of the progress bar and warning messages")
    Private _verboseArgument As New SwitchArgument("verbose", "v", False, "Prints detailed status information")
    Private _maxIterationsArgument As New ValueArgument(Of Integer)("maxiter", "", 0, "The fractal's maximum iteration threshold")
    Private _workSizeArgument As New ValueArgument(Of Integer)("worksize", "w", 100000, "Amount of openCL threads that are spawned simultaneously", _suffixConverter)
    Private _deviceArgument As New ValueArgument(Of Integer)("device", "d", 0, "OpenCL Device number (use --help for a list of openCL devices and their id numbers)")
    Private _bailoutArgument As New ValueArgument(Of Single)("bailout", "", 0, "The fractal's bailout threshold")
    Private _typeArgument As New EnumeratedValueArgument("type", "t", "Multibrot", "The fractal type to use (Required; use --help for a list of available types)", AvailableCalculators)
    Private _insidePainterArgument As New EnumeratedValueArgument("insidepainter", "", "Fill", "The painter type to use on samples that belong to the fractal (use --help for a list of available types)", AvailablePainters)
    Private _outsidePainterArgument As New EnumeratedValueArgument("outsidepainter", "", "Palette", "The painter type to use on samples that do not belong to the fractal (use --help for a list of available types)", AvailablePainters)
    Private _formatArgument As New EnumeratedValueArgument("format", "", "", "The image/video format to save the output in. Must be one of " & FormatList, (_imageFormats.Concat(_videoFormats)).ToList)
    Private _modeArgument As New EnumeratedValueArgument("mode", "m", "image", "Takes 'image' or 'video' as value and determines whether to render a single image or a video.", {"image", "video"}.ToList)
    Private _supersampleArgument As New ValueArgument(Of Integer)("supersample", "a", 1, "Antialiasing Quality. Has to be 'nxn' or a single value which is an n^2.", _superSamplingConverter)
    Private _sizeArgument As New ValueArgument(Of Size)("size", "s", New Size(100, 100), "The fractal's size", _sizeConverter)
    Private _continueVideoArgument As New SwitchArgument("continue", "c", False, "Reuse already rendered video frames")
    Private _fpsArgument As New ValueArgument(Of Integer)("fps", "", 0, "Frames per second (video only)")
    Private _bitrateArgument As New ValueArgument(Of Integer)("bitrate", "", 0, "Bitrate (video only)", _suffixConverter)
    Private _frameCountArgument As New ValueArgument(Of Integer)("frames", "", 0, "Amount of frames to render (video only)")
    Private _zoomStateArgument As New ValueArgument(Of ZoomState)("zoomstate", "z", New ZoomState, "The zoom state to render.")
    Private _overwriteArgument As New SwitchArgument("overwrite", "f", False, "Overwrite the output file if it already exists")
    Private _norenderArgument As New SwitchArgument("norender", "", False, "Only save settingsfile (use --save) and then exit")
    Private _axesArgument As New SwitchArgument("axes", "", False, "Print Axes on Image")
    Private _axesCenterArgument As New ValueArgument(Of ComplexNumber)("axescenter", "", 0, "Center of the axes")
    Private _axesStepArgument As New ValueArgument(Of Double)("axesstep", "", 0.1, "Step on the scales")
    Private _fontColorArgument As New ValueArgument(Of Color)("fontcolor", "", Color.Black, "Color of anything that is printed on the image", _colorConverter)
    Private _commentArgument As New ValueArgument(Of String)("comment", "", "", "Comment that should be printed on the image. Use \n for newline.")

    ''' <summary>
    ''' Gets the available calculators.
    ''' </summary>
    ''' <value>The available calculators.</value>
    Private ReadOnly Property AvailableCalculators As List(Of String)
        Get
            Dim calcs As New List(Of String)
            For Each calc In GetTypes(Of Fractal)()
                calcs.Add(CType(Activator.CreateInstance(calc), Fractal).Name)
            Next

            Return calcs
        End Get
    End Property

    ''' <summary>
    ''' Gets the available painters.
    ''' </summary>
    ''' <value>The available painters.</value>
    Private ReadOnly Property AvailablePainters As List(Of String)
        Get
            Return (From painter In GetTypes(Of Painter)() Select inst = CType(Activator.CreateInstance(painter), Painter) Where Not inst.Name = "None" Select inst.Name).ToList()
        End Get
    End Property

    ''' <summary>
    ''' Gets the available data sources.
    ''' </summary>
    ''' <value>The available data sources.</value>
    Private ReadOnly Property AvailableDataSources As IEnumerable(Of String)
        Get
            Return (From source In GetTypes(Of DataSource)() Select inst = CType(Activator.CreateInstance(source), DataSource) Where Not inst.Name = "None" Select inst.Name).ToList()
        End Get
    End Property

    ''' <summary>
    ''' Gets the format list.
    ''' </summary>
    ''' <value>The format list.</value>
    Private ReadOnly Property FormatList As String
        Get
            Dim s = (_imageFormats.Concat(_videoFormats)).ToList.Aggregate("[", Function(current, f) current & (f & "|"))

            Return s.Substring(0, s.Length - 1) & "]"
        End Get
    End Property

    ''' <summary>
    ''' Gets the devices.
    ''' </summary>
    Private ReadOnly Property Devices As List(Of ComputeDevice)
        Get
            If _devices Is Nothing Then
                _devices = New List(Of ComputeDevice)

                For Each clDevice In From clPlatform In ComputePlatform.Platforms From clDevice1 In clPlatform.Devices Select clDevice1
                    _devices.Add(clDevice)
                Next
            End If

            Return _devices
        End Get
    End Property

    ''' <summary>
    ''' The Main Entry point for the command line interface of fractals.
    ''' </summary>
    Sub Main()
        Try
            _cmd.Arguments.Add(_outsidePainterArgument)
            _cmd.Arguments.Add(_insidePainterArgument)
            _cmd.Arguments.Add(_outputArgument)
            _cmd.Arguments.Add(_inputArgument)
            _cmd.Arguments.Add(_quietArgument)
            _cmd.Arguments.Add(_verboseArgument)
            _cmd.Arguments.Add(_maxIterationsArgument)
            _cmd.Arguments.Add(_bailoutArgument)
            _cmd.Arguments.Add(_typeArgument)
            _cmd.Arguments.Add(_workSizeArgument)
            _cmd.Arguments.Add(_deviceArgument)
            _cmd.Arguments.Add(_formatArgument)
            _cmd.Arguments.Add(_modeArgument)
            _cmd.Arguments.Add(_helpArgument)
            _cmd.Arguments.Add(_supersampleArgument)
            _cmd.Arguments.Add(_sizeArgument)
            _cmd.Arguments.Add(_continueVideoArgument)
            _cmd.Arguments.Add(_fpsArgument)
            _cmd.Arguments.Add(_bitrateArgument)
            _cmd.Arguments.Add(_frameCountArgument)
            _cmd.Arguments.Add(_zoomStateArgument)
            _cmd.Arguments.Add(_overwriteArgument)
            _cmd.Arguments.Add(_saveArgument)
            _cmd.Arguments.Add(_norenderArgument)
            _cmd.Arguments.Add(_axesArgument)
            _cmd.Arguments.Add(_axesCenterArgument)
            _cmd.Arguments.Add(_axesStepArgument)
            _cmd.Arguments.Add(_fontColorArgument)
            _cmd.Arguments.Add(_commentArgument)

            Try
                _cmd.ComplainUnknown = False
                _cmd.Parse()
                EvaluateCommandLineArguments()

                For Each arg In _targetFractal.CommandLineArguments
                    arg.ShortName = ""
                Next

                For Each arg In _targetFractal.InsidePainter.CommandLineArguments
                    arg.LongName = "insidepainter:" & arg.LongName
                    arg.ShortName = ""
                Next

                For Each arg In _targetFractal.OutsidePainter.CommandLineArguments
                    arg.LongName = "outsidepainter:" & arg.LongName
                    arg.ShortName = ""
                Next

                _cmd.Arguments.AddRange(_targetFractal.CommandLineArguments)
                _cmd.Arguments.AddRange(_targetFractal.InsidePainter.CommandLineArguments)
                _cmd.Arguments.AddRange(_targetFractal.OutsidePainter.CommandLineArguments)

                _cmd.ComplainUnknown = True
                _cmd.Parse()

                _targetFractal.EvaluateCommandLineArguments()
                _targetFractal.InsidePainter.EvaluateCommandLineArguments()
                _targetFractal.OutsidePainter.EvaluateCommandLineArguments()

                If _saveArgument.IsParsed Then
                    If File.Exists(_saveArgument.Value) Then
                        If _overwriteArgument.IsParsed Then
                            If _verboseArgument.IsParsed Then Console.WriteLine("Destination for settingsfile already exists; deleting it")

                            File.Delete(_saveArgument.Value)
                        Else
                            Console.WriteLine("Error: Destination for settingsfile already exists")
                            Environment.Exit(0)
                        End If
                    End If

                    If _verboseArgument.IsParsed Then Console.WriteLine("Saving settings to '" & _saveArgument.Value & "'")

                    If _modeArgument.Value = "image" Then
                        Fractal.Save(_targetFractal, _saveArgument.Value)
                    Else
                        FractalVideo.Save(_targetVideo, _saveArgument.Value)
                    End If
                End If

                If _norenderArgument.IsParsed Then
                    If _saveArgument.IsParsed Then
                        Console.WriteLine("Exiting because --norender was specified")
                    Else
                        Console.WriteLine("I don't see the point in specifying --norender without --save, but i am exiting anyway.")
                    End If

                    Environment.Exit(0)
                End If
            Catch ex As Exception
                Console.WriteLine("Error: " & ex.Message)
                Console.WriteLine("Use '" & Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly.Location) & " --help' for usage instructions.")
                Environment.Exit(0)
            End Try

            Render()
        Catch ex As Exception
            Console.WriteLine("Error: " & ex.GetType.Name & " thrown: " & ex.Message & Environment.NewLine & "at " & ex.StackTrace)
            Environment.Exit(0)
        End Try
    End Sub

    ''' <summary>
    ''' Parses the command line.
    ''' </summary>
    Private Sub EvaluateCommandLineArguments()
        If _helpArgument.IsParsed Then
            PrintHelp()
            Environment.Exit(0)
        End If

        If Not _outputArgument.IsParsed And Not _norenderArgument.IsParsed Then
            Console.WriteLine("Error: --output has to be specified.")
            Console.WriteLine("Use '" & Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly.Location) & " --help' for usage instructions.")
            Environment.Exit(0)
        End If

        If Not _typeArgument.IsParsed And Not _inputArgument.IsParsed Then
            Console.WriteLine("Error: Either --type or --input have to be used.")
            Console.WriteLine("Use '" & Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly.Location) & " --help' for usage instructions.")
            Environment.Exit(0)
        End If

        If (_axesCenterArgument.IsParsed Or _axesStepArgument.IsParsed) And Not _axesArgument.IsParsed Then
            Console.WriteLine("Error: --axesStep and --axesCenter do not work without --axes")
            Environment.Exit(0)
        End If

        If _inputArgument.IsParsed And _typeArgument.IsParsed Then
            Console.WriteLine("Error: --type and --input cannot be specified simultaneously")
            Environment.Exit(0)
        End If

        If _verboseArgument.Value And _quietArgument.Value Then
            Console.WriteLine("Error: --verbose and --quiet can not be used simultaneously.")
            Environment.Exit(0)
        End If

        If _inputArgument.IsParsed Then
            If Not File.Exists(_inputArgument.Value) Then
                Console.WriteLine("Error: Specified fractal settings file does not seem to exist.")
                Environment.Exit(0)
            End If

            If _modeArgument.IsParsed And Not _quietArgument.Value Then
                Console.WriteLine("Warning: Ignoring --mode argument because --input was specified")
            End If

            'find out what is in the file
            Try
                _targetFractal = Fractal.Load(_inputArgument.Value)
                _target = _targetFractal
                _targetVideo = Nothing

                _modeArgument.Value = "image"
                If _verboseArgument.Value Then Console.WriteLine("The settingsfile was recognized as fractal image")
            Catch ex1 As Exception
                Try
                    _targetVideo = FractalVideo.Load(_inputArgument.Value)
                    _target = _targetVideo
                    _targetFractal = _targetVideo.Fractal

                    _modeArgument.Value = "video"
                    If _verboseArgument.Value Then Console.WriteLine("The settingsfile was recognized as fractal video")
                Catch ex2 As Exception
                    Console.WriteLine("Error: The contents of the settings file could not be recognized.")
                    Environment.Exit(0)
                End Try
            End Try
        Else
            _targetFractal = CreateInstance(Of Fractal)(_typeArgument.Value)

            If _modeArgument.Value = "image" Then
                _target = _targetFractal
            Else
                _target = New FractalVideo With {.Fractal = _targetFractal}
            End If
        End If

        If _workSizeArgument.IsParsed Then
            If _workSizeArgument.Value < 0 Then
                Console.WriteLine("Error: WorkSize must not be negative")
                Environment.Exit(0)
            End If

            Fractal.WorkSize = _workSizeArgument.Value
        End If

        If _sizeArgument.IsParsed Then _targetFractal.Size = _sizeArgument.Value
        If _supersampleArgument.IsParsed Then _targetFractal.AntiAliasingSamples = _supersampleArgument.Value

        If _deviceArgument.IsParsed Then
            If _deviceArgument.Value < 0 Or _deviceArgument.Value > Devices.Count - 1 Then
                Console.WriteLine("Error: There is no device with id " & _deviceArgument.Value)
                Environment.Exit(0)
            End If

            Fractal.Device = Devices(_deviceArgument.Value)
        End If

        If _zoomStateArgument.IsParsed Then _targetFractal.ZoomState = _zoomStateArgument.Value

        If _maxIterationsArgument.IsParsed Then
            If _maxIterationsArgument.Value = 0 Then
                _targetFractal.AutoAdjustIterations = True
            ElseIf _maxIterationsArgument.Value > 0 Then
                _targetFractal.AutoAdjustIterations = False
                _targetFractal.MaxIterations = _maxIterationsArgument.Value
            Else
                Console.WriteLine("Error: maxIterations has to be positive")
                Environment.Exit(0)
            End If
        End If

        If _bailoutArgument.IsParsed Then
            If _bailoutArgument.Value <= 0 Then
                Console.WriteLine("Error: bailout has to be positive")
                Environment.Exit(0)
            End If

            _targetFractal.BailoutValue = _bailoutArgument.Value
        End If

        If _insidePainterArgument.IsParsed Then
            _targetFractal.InsidePainter = CreateInstance(Of Painter)(_insidePainterArgument.Value)
        End If

        If _outsidePainterArgument.IsParsed Then
            _targetFractal.OutsidePainter = CreateInstance(Of Painter)(_outsidePainterArgument.Value)
        End If

        If _fpsArgument.IsParsed Then
            If _modeArgument.Value <> "video" Then
                Console.WriteLine("Error: --fps is video-only")
                Environment.Exit(0)
            End If

            _targetVideo.FPS = _fpsArgument.Value
        End If

        If _bitrateArgument.IsParsed Then
            If _modeArgument.Value <> "video" Then
                Console.WriteLine("Error: --bitrate is video-only")
                Environment.Exit(0)
            End If

            _targetVideo.Bitrate = _bitrateArgument.Value
        End If

        If _continueVideoArgument.IsParsed Then
            If _modeArgument.Value <> "video" Then
                Console.WriteLine("Error: --continue is video-only")
                Environment.Exit(0)
            End If

            _targetVideo.ReUse = _continueVideoArgument.Value
        End If

        If _frameCountArgument.IsParsed Then
            If _modeArgument.Value <> "video" Then
                Console.WriteLine("Error: --frames is video-only")
                Environment.Exit(0)
            End If

            _targetVideo.FrameCount = _frameCountArgument.Value
        End If

        If _modeArgument.Value = "video" Then
            _targetVideo.OutputFileName = _outputArgument.Value
        End If

        If _norenderArgument.IsParsed And Not _formatArgument.IsParsed Then
            _formatArgument.Value = "png"
            _formatArgument.IsParsed = True
        End If

        If Not _formatArgument.IsParsed Then
            If Not _outputArgument.Value.Contains(".") Then
                Console.WriteLine("Error: file format could not be determined from output filename")
                Environment.Exit(0)
            End If

            Dim splits() = _outputArgument.Value.Split({"."c}, StringSplitOptions.RemoveEmptyEntries)
            Dim ext = splits(splits.Length - 1).ToLowerInvariant

            If Not _formatArgument.PossibleValues.Contains(ext) Then
                Console.WriteLine("Error: file format could not be determined from output filename")
                Environment.Exit(0)
            End If

            _guessedExt = ext
            _formatArgument.Value = ext
            _formatArgument.IsParsed = True
        End If

        _imageFormat = ConvertExtension(_formatArgument.Value)

        If (_modeArgument.Value = "image" And Not _imageFormats.Contains(_formatArgument.Value.ToLowerInvariant)) Or (_modeArgument.Value = "video" And Not _videoFormats.Contains(_formatArgument.Value.ToLowerInvariant)) Then
            Console.WriteLine("Error: '" & _formatArgument.Value & "' is not a " & _modeArgument.Value & " format.")
            Environment.Exit(0)
        End If

        If File.Exists(_outputArgument.Value) Then
            If Not _overwriteArgument.IsParsed Then
                Console.WriteLine("Error: output file already exists")
                Environment.Exit(0)
            Else
                If _verboseArgument.IsParsed Then Console.WriteLine("Output file '" & _outputArgument.Value & "' already exists, deleting it")
                File.Delete(_outputArgument.Value)
            End If
        End If

        If _modeArgument.Value = "image" And _imageFormat IsNot Nothing And Not _axesArgument.IsParsed Then
            If _imageFormat.Guid = Imaging.ImageFormat.Bmp.Guid Then
                _targetFractal.UseFastBitmap = True
                _targetFractal.FastBitmapOutput = New FileStream(_outputArgument.Value, FileMode.Create)
            End If
        End If

        If _verboseArgument.Value Then AddHandler _target.MessageLogged, Sub(s, e) WriteLine(_target.LastLogMessage, True)
    End Sub

    ''' <summary>
    ''' Renders the fractal.
    ''' </summary>
    Private Sub Render()
        _target.StartRendering()

        'capture keys to control rendering
        Dim inputThread As New Thread(AddressOf CheckForInput)
        inputThread.Start()

        'wait until rendering is completed, periodically update progress in cmd
        Dim exitNext As Boolean = False

        While True
            If Not _quietArgument.Value And (Now - _lastProgressUpdate).Milliseconds >= 100 Then
                If _target.State = Utilities.WorkingState.Paused Then
                    Console.ForegroundColor = ConsoleColor.Yellow
                End If

                Console.Write(PrintProgress(_target.Progress, _target.RemainingTime))
                Console.ResetColor()

                _lastProgressUpdate = Now
            End If

            Thread.Sleep(100)

            If exitNext Then Exit While
            If _target.State = Utilities.WorkingState.None Then exitNext = True
        End While

        If Not _aborted Then
            If Not _target.Succeeded Then
                Console.WriteLine("Error: " & _target.Failure.Message)

                If _modeArgument.Value = "image" And _imageFormat.Guid = Imaging.ImageFormat.Bmp.Guid Then
                    Try
                        _targetFractal.FastBitmapOutput.Close()
                        IO.File.Delete(_outputArgument.Value)
                    Catch ex As Exception
                    End Try
                End If

                Environment.Exit(0)
            End If

            inputThread.Abort()

            If Not _quietArgument.Value Then ClearLine()

            If _modeArgument.Value = "image" Then
                If _imageFormat IsNot Nothing Then
                    If _imageFormat.Guid <> ImageFormat.Bmp.Guid Then
                        If _verboseArgument.Value Then WriteLine("Saving Image", True)

                        Dim img = _targetFractal.Image

                        If _axesArgument.IsParsed Then
                            DrawAxes(img)
                        ElseIf _commentArgument.Value <> "" Then
                            Using g = Graphics.FromImage(img)
                                g.DrawString(_commentArgument.Value.Replace("\n", Environment.NewLine), New Font("Consolas", 12), New SolidBrush(_fontColorArgument.Value), New Point(0, 0))
                            End Using
                        End If

                        img.Save(_outputArgument.Value, _imageFormat)
                    End If
                Else
                    If _verboseArgument.Value Then WriteLine("Saving Image", True)

                    Dim ascii = ""

                    If _guessedExt = "txt" Then
                        ascii = AsciiArtConverter.ConvertImageToAsciiText(_targetFractal.Image)
                    ElseIf _guessedExt = "html" Then
                        ascii = AsciiArtConverter.ConvertImageToAsciiHtml(_targetFractal.Image)
                    End If

                    File.WriteAllText(_outputArgument.Value, ascii)
                End If

                If _verboseArgument.Value Then WriteLine("Saved Image", True)
            End If

            If _verboseArgument.Value Then WriteLine("Exiting", False)
            Environment.Exit(0)
        End If
    End Sub
    ''' <summary>
    ''' Clears the line.
    ''' </summary>
    Private Sub ClearLine()
        Dim emptyText = vbCr

        For i = emptyText.Length To Console.WindowWidth - 1
            emptyText &= " "
        Next

        Console.Write(emptyText & vbCr)
    End Sub

    ''' <summary>
    ''' Draws the axes.
    ''' </summary>
    ''' <param name="img">The img.</param>
    Private Sub DrawAxes(ByVal img As Bitmap)
        Using g = Graphics.FromImage(img)
            g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

            Dim scalePen = New Pen(New SolidBrush(_fontColorArgument.Value))

            Dim center = _targetFractal.ViewportToScreen(_axesCenterArgument.Value)
            Dim xPoint = _targetFractal.ViewportToScreen(_axesCenterArgument.Value + 1)
            Dim yPoint = _targetFractal.ViewportToScreen(_axesCenterArgument.Value + New ComplexNumber(0, 1))

            Dim x1 = GetAxisPoint(center, New Point(0, 0), xPoint)
            Dim x2 = GetAxisPoint(center, New Point(img.Width, img.Height), xPoint)

            Dim y1 = GetAxisPoint(center, New Point(0, 0), yPoint)
            Dim y2 = GetAxisPoint(center, New Point(img.Width, img.Height), yPoint)

            g.DrawLine(scalePen, x1, x2)
            g.DrawLine(scalePen, y1, y2)

            Dim pSub = Sub(direction As Integer, pp1 As PointF, pp2 As PointF, axis As ComplexNumber)
                           Dim diffAbs = (Math.Sqrt((pp1.X - pp2.X) ^ 2 + (pp1.Y - pp2.Y) ^ 2)) / 4
                           Dim diffUnit = New PointF(CSng(Math.Ceiling((pp1.X - pp2.X) / diffAbs)), CSng(Math.Ceiling((pp1.Y - pp2.Y) / diffAbs)))

                           Dim i As Integer = 1
                           Dim c As ComplexNumber = 0

                           While (New RectangleF(0, 0, img.Width, img.Height).Contains(_targetFractal.ViewportToScreen(_axesCenterArgument.Value + axis * New ComplexNumber(0, i * direction * _axesStepArgument.Value))))
                               c = _axesCenterArgument.Value + axis * New ComplexNumber(0, i * direction * _axesStepArgument.Value)

                               Dim p = _targetFractal.ViewportToScreen(c)
                               Dim p1 = New PointF(CSng(Math.Round(p.X + diffUnit.X)), CSng(Math.Round(p.Y + diffUnit.Y)))
                               Dim p2 = New PointF(CSng(Math.Round(p.X - diffUnit.X)), CSng(Math.Round(p.Y - diffUnit.Y)))

                               g.DrawLine(scalePen, p1, p2)

                               i += 1
                           End While

                           If direction = -1 And axis <> 1 Or direction = 1 And axis = 1 Then
                               Dim pp = _targetFractal.ViewportToScreen(c)
                               Dim ppLbl = New PointF(CSng(Math.Round(pp.X - diffUnit.X)), CSng(Math.Round(pp.Y - diffUnit.Y)))
                               g.DrawString(If(axis = 1, "Im", "Re").ToString, New Font("Consolas", 12), New SolidBrush(_fontColorArgument.Value), pp)
                           End If
                       End Sub

            pSub(1, x1, x2, 1)
            pSub(-1, x1, x2, 1)

            pSub(1, y1, y2, New ComplexNumber(0, 1))
            pSub(-1, y1, y2, New ComplexNumber(0, 1))

            g.DrawString(If(_commentArgument.Value <> "", _commentArgument.Value.Replace("\n", Environment.NewLine) & Environment.NewLine & Environment.NewLine, "") & "center: " & _axesCenterArgument.Value.ToString & Environment.NewLine & "step: " & _axesStepArgument.Value, New Font("Consolas", 12), New SolidBrush(_fontColorArgument.Value), New Point(0, 0))
        End Using
    End Sub
    
    ''' <summary>
    ''' Gets the axis point.
    ''' </summary>
    ''' <param name="center">The center.</param>
    ''' <param name="offset">The offset.</param>
    ''' <param name="p">The p.</param><returns></returns>
    Private Function GetAxisPoint(ByVal center As PointF, ByVal offset As PointF, ByVal p As PointF) As PointF
        Dim t1 = (offset.X - center.X) / (p.X - center.X)
        Dim t2 = (offset.Y - center.Y) / (p.Y - center.Y)

        Dim t As Double

        If Double.IsInfinity(t1) Then
            t = t2
        ElseIf Double.IsInfinity(t2) Then
            t = t1
        Else
            t = If(Math.Abs(t1) > Math.Abs(t2), t1, t2)
        End If

        Return New Point(CInt(center.X + (p.X - center.X) * t), CInt(center.Y + (p.Y - center.Y) * t))
    End Function

    ''' <summary>
    ''' Creates the instance.
    ''' </summary>
    ''' <param name="name">The name.</param>
    ''' <returns></returns>
    Private Function CreateInstance(Of T)(ByVal name As String) As T
        For Each t In GetTypes(Of T)()
            Dim inst As T = CType(Activator.CreateInstance(t), T)

            Dim nameProp = t.GetProperty("Name")

            If nameProp IsNot Nothing Then
                If nameProp.GetValue(inst, Nothing).ToString = name Then Return inst
            End If
        Next
    End Function

    ''' <summary>
    ''' Writes the line.
    ''' </summary>
    ''' <param name="s">The s.</param>
    ''' <param name="writeprogress">if set to <c>true</c> [writeprogress].</param>
    Private Sub WriteLine(ByVal s As String, ByVal writeprogress As Boolean)
        s = vbCr & s

        For i = s.Length To Console.WindowWidth - 1
            s &= " "
        Next

        If writeprogress Then
            Console.WriteLine(s)
            Console.Write(PrintProgress(_target.Progress, _target.RemainingTime))
        Else
            Console.Write(s)
        End If
    End Sub

    ''' <summary>
    ''' Checks for input.
    ''' </summary>
    Private Sub CheckForInput()
        Try
            While True
                Dim k = Console.ReadKey()

                If k.KeyChar = " " Then
                    If _target.State = Utilities.WorkingState.Working Then
                        _target.SuspendRendering()
                    ElseIf _target.State = Utilities.WorkingState.Paused Then
                        _target.ResumeRendering()
                    End If
                ElseIf Char.ToUpperInvariant(k.KeyChar) = "Q" Then
                    _target.AbortRendering()
                    _aborted = True

                    ClearLine()

                    If _verboseArgument.Value Then WriteLine("Exiting", False)
                    Return
                End If
            End While
        Catch ex As Exception
        End Try
    End Sub

    ''' <summary>
    ''' Prints the progress.
    ''' </summary>
    ''' <param name="progress">The progress.</param>
    ''' <param name="eta">The eta.</param>
    ''' <returns></returns>
    Private Function PrintProgress(ByVal progress As Double, ByVal eta As TimeSpan) As String
        If progress = 0 Then Return ""

        Dim timeStr As String = ""
        timeStr &= Math.Round(progress * 100, 2).ToString("##0.00") & "%  ETA: "
        timeStr &= CInt(eta.TotalSeconds) & "s"

        'show progress bar
        Dim sb As New StringBuilder

        sb.Append(vbCr)

        For p As Integer = 0 To CInt(progress * (Console.WindowWidth - timeStr.Length - 4))
            sb.Append("=")
        Next
        sb.Append("> ")

        For s As Integer = sb.Length To Console.WindowWidth - timeStr.Length - 1
            sb.Append(" ")
        Next

        sb.Append(timeStr)

        Return sb.ToString & Chr(13)
    End Function

    ''' <summary>
    ''' Prints the help.
    ''' </summary>
    Private Sub PrintHelp()
        Dim version As String

        If BuildRev <> "" Then
            version = "revision " & BuildRev & ", built " & BuildDate & BuildTime
        Else
            version = Assembly.GetExecutingAssembly.GetName.Version.ToString
        End If

        Console.WriteLine("Fractals " & version)
        Console.WriteLine("Copyright (c) 2010-2011 Thies Gerken")
        Console.WriteLine("This project uses FFmpeg for video encoding.")
        Console.WriteLine()
        Console.WriteLine("Usage: " & Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly.Location) & " [options]")
        Console.WriteLine("[options] may consist of the following parameters:")

        _cmd.Arguments.Add(New ValueArgument(Of String)("insidepainter:[param]", "", "", "Extra insidepainter parameter, see 'Additional options' for parameters"))
        _cmd.Arguments.Add(New ValueArgument(Of String)("outsidepainter:[param]", "", "", "Extra outsidepainter parameter, see 'Additional options' for parameters"))

        _cmd.PrintHelp()

        Dim someoneHasOptions As Boolean = False

        Console.WriteLine()
        Console.WriteLine("Additional options per fractal type:")

        For Each t In GetTypes(Of Fractal)()
            Dim inst = CType(Activator.CreateInstance(t), Fractal)

            _cmd.Arguments.Clear()
            _cmd.Arguments.AddRange(inst.CommandLineArguments)

            If _cmd.Arguments.Count > 0 Then
                Console.WriteLine("  " & inst.Name)
                _cmd.PrintHelp()
                someoneHasOptions = True
            End If
        Next

        If Not someoneHasOptions Then
            Console.WriteLine("  (No Fractals with extra options available)")
        End If

        someoneHasOptions = False

        Console.WriteLine()
        Console.WriteLine("Additional options per painter type:")

        For Each t In GetTypes(Of Painter)()
            Dim inst = CType(Activator.CreateInstance(t), Painter)

            _cmd.Arguments.Clear()
            _cmd.Arguments.AddRange(inst.CommandLineArguments)

            If _cmd.Arguments.Count > 0 Then
                Console.WriteLine("  " & inst.Name)
                _cmd.PrintHelp(True, "  ")
                someoneHasOptions = True
            End If
        Next

        If Not someoneHasOptions Then
            Console.WriteLine("  (No Painters with extra options available)")
        End If

        Console.WriteLine()
        Console.WriteLine("Available OpenCL Devices:")

        For i As Integer = 0 To Devices.Count - 1
            Console.WriteLine("  " & i & ": " & Devices(i).Name & " (using " & Devices(i).Platform.Name & ")")
        Next

        Console.WriteLine()
        Console.WriteLine("Available Fractal Types:")

        For Each calc In AvailableCalculators
            Console.WriteLine("  " & calc)
        Next

        Console.WriteLine()
        Console.WriteLine("Available Painters:")

        For Each painter In AvailablePainters
            Console.WriteLine("  " & painter)
        Next

        Console.WriteLine()
        Console.WriteLine("Available DataSources:")

        For Each source In AvailableDataSources
            Console.WriteLine("  " & source)
        Next

        Console.WriteLine()
        Console.WriteLine("Available Palettes:")

        For Each pal In Utilities.ColorPalette.AvailablePalettes
            Console.WriteLine("  " & pal)
        Next

        Console.WriteLine()
        Console.WriteLine("Controls: ")
        Console.WriteLine("  space : suspend / resume rendering")
        Console.WriteLine("  q     : abort rendering")
    End Sub

    ''' <summary>
    ''' Converts the extension.
    ''' </summary>
    ''' <param name="ext">The ext.</param>
    ''' <returns></returns>
    Private Function ConvertExtension(ByVal ext As String) As ImageFormat
        Select Case ext.ToLowerInvariant
            Case "png"
                Return ImageFormat.Png
            Case "jpg"
                Return ImageFormat.Jpeg
            Case "bmp"
                Return ImageFormat.Bmp
            Case "gif"
                Return ImageFormat.Gif
        End Select

        Return Nothing
    End Function

End Module
