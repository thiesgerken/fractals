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
Imports System.Security.Cryptography
Imports System.ComponentModel
Imports Cloo
Imports Fractals.Utilities
Imports Fractals.Mathematics
Imports System.IO
Imports Fractals.Painters
Imports Fluent
Imports System.Xml
Imports System.Xml.Linq
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging
Imports CommandLineParser

Public MustInherit Class Fractal
    Inherits Renderer
    Implements ICloneable, IDisposable, ICommandLineExtender

#Region "Private Fields"

    'openCL interop
    Private _insidePainterPositions As New Dictionary(Of String, Integer)
    Private _outsidePainterPositions As New Dictionary(Of String, Integer)
    Private _insidePainterDataSourcePositions As New Dictionary(Of String, Integer)
    Private _outsidePainterDataSourcePositions As New Dictionary(Of String, Integer)
    Private Shared _clProperties As ComputeContextPropertyList = New ComputeContextPropertyList(ComputePlatform.Platforms(0))
    Protected Shared _clPlatform As ComputePlatform = ComputePlatform.Platforms(0)
    Protected Shared _clDevice As ComputeDevice = ComputePlatform.Platforms(0).Devices(0)
    Protected Shared _clContext As ComputeContext = New ComputeContext(ComputePlatform.Platforms(0).Devices, _clProperties, Nothing, IntPtr.Zero)
    Private _clInited As Boolean
    Private _clCalculationKernel As ComputeKernel
    Private _clRenderingKernel As ComputeKernel
    Private _clProgram As ComputeProgram
    Private _clCommands As ComputeCommandQueue
    Private _clEvents As ComputeEventList
    Private _bitmapBuffer As ComputeBuffer(Of Byte)
    Private _outputBuffer As ComputeBuffer(Of Vectors.Double5)
    Private _output() As Vectors.Double5
    Private _outputGC As GCHandle
    Private _loopOutputBuffer As ComputeBuffer(Of Vectors.Double5)
    Private _loopOutput() As Vectors.Double5
    Private _loopOutputGC As GCHandle
    Private _watch As New Stopwatch

    'speed and progress measurement
    Protected _totalPixels As Integer
    Protected _progress As Long
    Private _overallSamples As Integer
    Private _overallTime As Long
    Private _samplesDone As Long
    Private _loops As Integer
    Private _timePerPixel As Double = 0
    Private _lastSpeed As Double

    'time to wait before spawning the next wave of workers
    Private Shared _waitTime As Integer = 20

    'private fields
    Private WithEvents _outsidePainter As Painter
    Private WithEvents _insidePainter As Painter
    Protected _maxIterations As Integer
    Protected _image As Bitmap
    Protected _bailoutValue As Double
    Protected _antiAliasingSamples As Integer = 1
    Private _disposedValue As Boolean 'for IDisposable support to detect redundant calls
    Private _zoomStates As New Stack(Of ZoomState)
    Private _zoomState As ZoomState
    Private _size As Size
    Private _smoothIterationCounts As Boolean = True
    Private _autoAdjustIterations As Boolean = True
    Private _lastMessage As String
    Private _needsRecalculation As Boolean = True

    'precalculated viewport <-> screen conversion fields
    Private _transformMatrix As New RotationMatrix(0)
    Private _antiTransformMatrix As New RotationMatrix(0)
    Private _scaleX As Double
    Private _scaleY As Double
    Private _scale As Double

    'precalculated antialiasing fields
    Private _pixelWidth As Double
    Private _pixelHeight As Double
    Private _subPixelWidth As Double
    Private _subPixelHeight As Double
    Private _samplesX As Integer  'samplesX * samplesY = AntiAliasingSamples
    Private _samplesY As Integer

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the wait time.
    ''' </summary>
    ''' <value>The wait time.</value>
    Public Shared Property WaitTime As Integer
        Get
            Return _waitTime
        End Get
        Set(ByVal value As Integer)
            _waitTime = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether [needs recalculation].
    ''' </summary>
    ''' <value><c>true</c> if [needs recalculation]; otherwise, <c>false</c>.</value>
    Public Property NeedsRecalculation As Boolean
        Get
            Return _needsRecalculation
        End Get
        Set(ByVal value As Boolean)
            _needsRecalculation = value

            OnPropertyChanged("NeedsRecalculation")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the anti aliasing samples. Has to be a x^2
    ''' </summary>
    ''' <value>The anti aliasing samples.</value>
    Public Property AntiAliasingSamples As Integer
        Get
            Return _antiAliasingSamples
        End Get
        Set(ByVal value As Integer)
            If value <> _antiAliasingSamples Then
                _antiAliasingSamples = value

                OnPropertyChanged("AntiAliasingSamples")
                OnFractalSettingChanged("AntiAliasingSamples")
            End If
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the samples per loop.
    ''' </summary>
    ''' <value>The samples per loop.</value>
    Public Shared Property WorkSize As Integer = 1000000

    ''' <summary>
    ''' Gets or sets the outside painter.
    ''' </summary>
    ''' <value>The painter.</value>
    Public Property OutsidePainter As Painter
        Get
            Return _outsidePainter
        End Get
        Set(ByVal value As Painter)
            _outsidePainter = value
            _outsidePainter.Type = PainterType.Exterior
            _clInited = False

            If _outsidePainter.NeedsData Then
                _outsidePainter.DataSource = New IterationCountLogDataSource
            Else
                _outsidePainter.DataSource = New DefaultDataSource
            End If

            OnPropertyChanged("OutsidePainter")
            OnFractalSettingChanged("OutsidePainter")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the inside painter.
    ''' </summary>
    ''' <value>The inside painter.</value>
    Public Property InsidePainter As Painter
        Get
            Return _insidePainter
        End Get
        Set(ByVal value As Painter)
            _insidePainter = value
            _insidePainter.Type = PainterType.Interior
            _clInited = False

            If _insidePainter.NeedsData Then
                _insidePainter.DataSource = New ArgumentDataSource
            Else
                _insidePainter.DataSource = New DefaultDataSource
            End If

            OnPropertyChanged("InsidePainter")
            OnFractalSettingChanged("InsidePainter")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the escape value.
    ''' </summary>
    ''' <value>The escape value.</value>
    Public Property BailoutValue As Double
        Get
            Return _bailoutValue
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                _bailoutValue = value
            Else
                _bailoutValue = Me.DefaultBailoutValue
            End If

            OnPropertyChanged("BailoutValue")
            OnFractalSettingChanged("BailoutValue")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether this <see cref="Fractal" /> is smooth.
    ''' </summary>
    ''' <value><c>true</c> if smooth; otherwise, <c>false</c>.</value>
    Public Property SmoothIterationCounts As Boolean
        Get
            Return _smoothIterationCounts And SupportsSmoothing
        End Get
        Set(ByVal value As Boolean)
            _smoothIterationCounts = value

            OnPropertyChanged("SmoothIterationCounts")
            OnFractalSettingChanged("SmoothIterationCounts")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the zoom states.
    ''' </summary>
    ''' <value>The zoom states.</value>
    Public Property ZoomStates As Stack(Of ZoomState)
        Get
            Return _zoomStates
        End Get
        Set(ByVal value As Stack(Of ZoomState))
            _zoomStates = value

            OnPropertyChanged("ZoomStates")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the size.
    ''' </summary>
    ''' <value>The size.</value>
    Public Property Size As Size
        Get
            Return _size
        End Get
        Set(ByVal value As Size)
            _size = value

            OnPropertyChanged("Size")
            NeedsRecalculation = True
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether [auto adjust iterations].
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if [auto adjust iterations]; otherwise, <c>false</c>.
    ''' </value>
    Public Property AutoAdjustIterations As Boolean
        Get
            Return _autoAdjustIterations
        End Get
        Set(ByVal value As Boolean)
            _autoAdjustIterations = value

            OnPropertyChanged("AutoAdjustIterations")

            Dim oldMax = _maxIterations

            RefreshIterations()

            If oldMax <> _maxIterations Then
                OnPropertyChanged("MaxIterations")
                OnFractalSettingChanged("MaxIterations")
            End If
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the max iterations.
    ''' </summary>
    ''' <value>The max iterations.</value>
    Public Property MaxIterations As Integer
        Get
            If Not State = WorkingState.Working Then RefreshIterations()

            Return _maxIterations
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then
                _maxIterations = value
            Else
                _maxIterations = Me.DefaultMaxIterationCount
            End If

            OnPropertyChanged("MaxIterations")
            OnFractalSettingChanged("MaxIterations")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the zoom.
    ''' </summary>
    ''' <value>The zoom.</value>
    Public Property ZoomState As ZoomState
        Get
            Return _zoomState
        End Get
        Set(ByVal value As ZoomState)
            If Not value = New ZoomState Then
                ZoomStates.Push(_zoomState)
            End If

            _zoomState = value

            OnPropertyChanged("ZoomState")
            OnPropertyChanged("CanReset")
            OnZoomStateChanged()
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the device.
    ''' </summary>
    ''' <value>The device.</value>
    Public Shared Property Device As ComputeDevice
        Get
            Return _clDevice
        End Get
        Set(ByVal value As ComputeDevice)
            _clDevice = value

            _clPlatform = value.Platform
            _clProperties = New ComputeContextPropertyList(_clPlatform)
            _clContext = New ComputeContext(_clPlatform.Devices, _clProperties, Nothing, IntPtr.Zero)
        End Set
    End Property

    Public Property UseFastBitmap As Boolean
    Public Property FastBitmapOutput As Stream

    ''' <summary>
    ''' Gets the default escape value for Diverge and epsilon for Converge.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected MustOverride ReadOnly Property DefaultBailoutValue As Double

    ''' <summary>
    ''' Gets the default offset on the viewport (Location of the center point).
    ''' </summary>
    ''' <value>The default offset.</value>
    Protected MustOverride ReadOnly Property DefaultOffset As ComplexNumber

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected MustOverride ReadOnly Property DefaultMaxIterationCount As Integer

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected MustOverride ReadOnly Property DefaultViewportSize As SizeF

    ''' <summary>
    ''' Gets the initial rotation.
    ''' </summary>
    ''' <value>The initial rotation.</value>
    Protected Overridable ReadOnly Property DefaultRotation As Double
        Get
            Return 0
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected MustOverride ReadOnly Property Source As String

    ''' <summary>
    ''' Gets the progress of the rendering.
    ''' </summary>
    ''' <value>The progress.</value>
    Public Overrides ReadOnly Property Progress As Double
        Get
            Dim val = _progress / _totalPixels

            If Double.IsInfinity(val) Or Double.IsNaN(val) Then Return 0
            Return Math.Min(1, val)
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this fractal is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public MustOverride ReadOnly Property RibbonTab As RibbonTabItem

    ''' <summary>
    ''' Gets a list of additional outside painters that can be selected for this fractal even if the painter is not selectable.
    ''' </summary>
    ''' <value>The additional outside painters.</value>
    Public Overridable ReadOnly Property AdditionalOutsidePainters As ICollection(Of Type)
        Get
            Return Type.EmptyTypes
        End Get
    End Property

    ''' <summary>
    ''' Gets a list of additional inside painters that can be selected for this fractal even if the painter is not selectable.
    ''' </summary>
    ''' <value>The additional inside painters.</value>
    Public Overridable ReadOnly Property AdditionalInsidePainters As ICollection(Of Type)
        Get
            Return Type.EmptyTypes
        End Get
    End Property

    Public Shared ReadOnly Property Context As ComputeContext
        Get
            Return _clContext
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [requires specific painter].
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if [requires specific painter]; otherwise, <c>false</c>.
    ''' </value>
    Public Overridable ReadOnly Property PaintersChangeable As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [supports anti aliasing].
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if [supports anti aliasing]; otherwise, <c>false</c>.
    ''' </value>
    Public Overridable ReadOnly Property SupportsAntiAliasing As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether [supports smoothing].
    ''' </summary>
    ''' <value><c>true</c> if [supports smoothing]; otherwise, <c>false</c>.</value>
    Public Overridable ReadOnly Property SupportsSmoothing As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets the rotation.
    ''' </summary>
    ''' <value>The rotation.</value>
    Public ReadOnly Property Rotation As Double
        Get
            Return Me.ZoomState.Rotation + Me.DefaultRotation
        End Get
    End Property

    ''' <summary>
    ''' Gets or sets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public ReadOnly Property Image As Bitmap
        Get
            Return _image
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public MustOverride ReadOnly Property Name As String

    ''' <summary>
    ''' Gets the description.
    ''' </summary>
    ''' <value>The description.</value>
    Public MustOverride ReadOnly Property Description As String

    ''' <summary>
    ''' Gets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public MustOverride ReadOnly Property PreviewImage As Bitmap

    ''' <summary>
    ''' Gets the speed (samples / s).
    ''' </summary>
    ''' <value>The speed.</value>
    Public Overridable ReadOnly Property Speed As Double
        Get
            If Not State = WorkingState.Working Then Return _lastSpeed

            Return Size.Width * Size.Height * AntiAliasingSamples * Progress / ElapsedTime.TotalSeconds
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this instance can reset.
    ''' </summary>
    ''' <value><c>true</c> if this instance can reset; otherwise, <c>false</c>.</value>
    Public ReadOnly Property CanReset As Boolean
        Get
            Return Not (ZoomState = New ZoomState Or _zoomStates.Count = 0)
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fractal" /> class.
    ''' </summary>
    Protected Sub New()
        Me.Size = New Size(400, 400)
        _maxIterations = Me.DefaultMaxIterationCount
        _bailoutValue = Me.DefaultBailoutValue

        Me.OutsidePainter = New PalettePainter
        Me.InsidePainter = New FillPainter
    End Sub

#End Region
#Region "Methods"

#Region "Rendering"

    ''' <summary>
    ''' Calculates this instance.
    ''' </summary>
    Protected Overrides Sub Render()
        'raise propertychanged event every 50ms for all progress related properties until rendering is completed
        Dim notifier As New Thread(Sub()
                                       While Not State = WorkingState.None
                                           OnPropertyChanged("Progress")
                                           OnPropertyChanged("Speed")
                                           OnPropertyChanged("ElapsedTime")
                                           OnPropertyChanged("RemainingTime")

                                           Thread.Sleep(50)
                                       End While

                                       OnPropertyChanged("Progress")
                                       OnPropertyChanged("Speed")
                                       OnPropertyChanged("ElapsedTime")
                                       OnPropertyChanged("RemainingTime")
                                   End Sub)
        notifier.Start()

        RefreshIterations()

        _progress = 0
        _totalPixels = 0

        _scaleX = Me.DefaultViewportSize.Width / Me.Size.Width / Me.ZoomState.Factor
        _scaleY = Me.DefaultViewportSize.Height / Me.Size.Height / Me.ZoomState.Factor
        _scale = Math.Max(_scaleX, _scaleY)
        _transformMatrix = New RotationMatrix(Rotation)
        _antiTransformMatrix = New RotationMatrix(-Rotation)
        _samplesX = CInt(Math.Sqrt(AntiAliasingSamples))
        _samplesY = _samplesX
        _pixelWidth = ComplexNumber.Abs(ScreenToViewport(New Point(1, 1)) - ScreenToViewport(New Point(2, 1)))
        _pixelHeight = ComplexNumber.Abs(ScreenToViewport(New Point(1, 1)) - ScreenToViewport(New Point(1, 2)))
        _subPixelWidth = _pixelWidth / _samplesX
        _subPixelHeight = _pixelHeight / _samplesY

        Dim info As New FractalInfo(Size, MaxIterations, BailoutValue, SmoothIterationCounts, ZoomState)

        GetReady()

        'initialize painters
        Me.OutsidePainter.ParentInfo = info
        Me.OutsidePainter.Initialize()

        Me.InsidePainter.ParentInfo = info
        Me.InsidePainter.Initialize()

        WriteLogMessage("Computing Fractal on " & Device.Name & " using " & _clPlatform.Name)

        RenderOpenCL()

        _lastSpeed = Speed
        _timePerPixel = ElapsedTime.TotalMilliseconds / (Me.Size.Width * Me.Size.Height)

        OnPropertyChanged("Image")

        NeedsRecalculation = False
    End Sub

    ''' <summary>
    ''' Calculates the GPU.
    ''' </summary>
    Protected Overridable Sub RenderOpenCL()
        _overallSamples = Size.Width * Size.Height * AntiAliasingSamples
        _overallTime = 0
        _samplesDone = 0
        _progress = 0
        _loops = Math.Max((CInt(Math.Ceiling(_overallSamples / WorkSize))), 1)
        _totalPixels = _loops + 1

        If Not _clInited Then
            WriteLogMessage("Compiling OpenCL kernels")

            'setup opencl objects
            _clCommands = New ComputeCommandQueue(_clContext, Device, ComputeCommandQueueFlags.None)
            _clEvents = New ComputeEventList()

            'assemble source, start with general functions and declarations
            Dim source As String = My.Resources.OpenCLFunctions

            'calculator's source can simply be appended
            source &= Environment.NewLine & Me.Source

            'search for a suitable fp64 extension (nvidia and ati have different names for it, idk about intel)
            Dim fp64Support As String = ""

            For Each ext In _clDevice.Extensions
                If ext = "cl_amd_fp64" Or ext = "cl_khr_fp64" Then
                    fp64Support = ext
                    Exit For
                End If
            Next

            If fp64Support = "" Then
                Throw New Exception("The selected computing environment does not have 64-bit floating point support")
            End If

            'append extension inclusion directive
            source = "#pragma OPENCL EXTENSION " & fp64Support & ": enable" & Environment.NewLine & source

            'append painter sources
            source &= Environment.NewLine & InsidePainter.Source.Replace("char4 Paint(", "char4 Paint_Inside(")
            source &= Environment.NewLine & OutsidePainter.Source.Replace("char4 Paint(", "char4 Paint_Outside(")

            'append datasource sources
            source &= Environment.NewLine & OutsidePainter.DataSource.Source.Replace("double GetData(", "double GetData_Outside(")
            source &= Environment.NewLine & InsidePainter.DataSource.Source.Replace("double GetData(", "double GetData_Inside(")

            'now the paint kernel
            source &= Environment.NewLine & My.Resources.PaintKernel

            'data needed?
            If Not InsidePainter.NeedsData Then
                source = source.Replace("GetData_Inside(maxIterations, resultBuffer[i] /*{params_inside_data}*/)", "1")
            End If

            If Not OutsidePainter.NeedsData Then
                source = source.Replace("GetData_Outside(maxIterations, resultBuffer[i] /*{params_outside_data}*/)", "1")
            End If

            'arguments
            Dim declarations As String = ""
            Dim position = 4

            Dim insidePainterCall As String = ""
            Dim outsidePainterCall As String = ""
            Dim insidePainterDataSourceCall As String = ""
            Dim outsidePainterDataSourceCall As String = ""

            Dim s = Sub(args As Dictionary(Of String, String), ByRef callString As String, ByRef positions As Dictionary(Of String, Integer))
                        positions = New Dictionary(Of String, Integer)

                        For Each arg In args
                            declarations &= ", " & arg.Value & " arg" & position
                            callString &= ", arg" & position

                            positions.Add(arg.Key, position)

                            position += 1
                        Next
                    End Sub

            s(InsidePainter.Arguments, insidePainterCall, _insidePainterPositions)
            s(OutsidePainter.Arguments, outsidePainterCall, _outsidePainterPositions)
            s(InsidePainter.DataSource.Arguments, insidePainterDataSourceCall, _insidePainterDataSourcePositions)
            s(OutsidePainter.DataSource.Arguments, outsidePainterDataSourceCall, _outsidePainterDataSourcePositions)

            source = source.Replace("/*{declarations}*/", declarations)
            source = source.Replace("/*{params_inside_painter}*/", insidePainterCall)
            source = source.Replace("/*{params_outside_painter}*/", outsidePainterCall)
            source = source.Replace("/*{params_inside_data}*/", insidePainterDataSourceCall)
            source = source.Replace("/*{params_outside_data}*/", outsidePainterDataSourceCall)

            'build kernel
            _clProgram = New ComputeProgram(_clContext, {source})
            _clProgram.Build(Nothing, "-cl-no-signed-zeros -cl-finite-math-only", Nothing, IntPtr.Zero)
            System.Console.WriteLine(_clProgram.GetBuildLog(_clDevice))

            _clCalculationKernel = _clProgram.CreateKernel("CalculateSample")
            _clRenderingKernel = _clProgram.CreateKernel("PaintPixel")
            _clInited = True
        Else
            WriteLogMessage("OpenCL kernels are already compiled")
        End If

        'create output array
        ReDim _output(CInt(_overallSamples) - 1)
        ReDim _loopOutput(WorkSize - 1)

        If _loopOutputGC.IsAllocated Then _loopOutputGC.Free()

        _loopOutputGC = GCHandle.Alloc(_loopOutput, GCHandleType.Pinned)

        'create buffers
        _loopOutputBuffer = New ComputeBuffer(Of Vectors.Double5)(_clContext, ComputeMemoryFlags.WriteOnly, WorkSize)

        'set constant arguments
        _clCalculationKernel.SetValueArgument(0, New Vectors.Int2(Me.Size.Width, Me.Size.Height))
        _clCalculationKernel.SetValueArgument(1, New Vectors.Float2(Me.DefaultViewportSize.Width, Me.DefaultViewportSize.Height))
        _clCalculationKernel.SetValueArgument(2, New Vectors.Double2(Me.DefaultOffset.RealPart, Me.DefaultOffset.ImaginaryPart))
        _clCalculationKernel.SetValueArgument(3, New Vectors.Int2(If(InsidePainter.DataSource.NeedsLastZ, 1, 0), If(SmoothIterationCounts, 1, 0)))
        _clCalculationKernel.SetValueArgument(4, MaxIterations)
        _clCalculationKernel.SetValueArgument(5, CSng(BailoutValue * BailoutValue))
        _clCalculationKernel.SetValueArgument(6, AntiAliasingSamples)
        _clCalculationKernel.SetValueArgument(7, ZoomState.Factor)
        _clCalculationKernel.SetValueArgument(8, New Vectors.Double2(ZoomState.Offset.RealPart, ZoomState.Offset.ImaginaryPart))
        _clCalculationKernel.SetValueArgument(9, New Vectors.Double2(Math.Sin(ZoomState.Rotation), Math.Cos(ZoomState.Rotation)))

        'output buffer
        _clCalculationKernel.SetMemoryArgument(11, _loopOutputBuffer)

        'custom arguments
        SetArguments(_clCalculationKernel)

        WriteLogMessage("Starting calculation of " & GenerateCountString(_overallSamples) & " samples divided in " & _loops & " pass" & If(_loops <> 1, "es", ""))

        For i As Integer = 1 To _loops
            While _paused
                'do not do anything if paused
                Thread.Sleep(100)
            End While

            _watch.Restart()

            Dim count = If(i = _loops, _overallSamples - WorkSize * (i - 1), WorkSize)

            If i = _loops Then
                'last loop is usually smaller than the rest
                _loopOutputGC.Free()
                ReDim _loopOutput(CInt(count - 1))
                _loopOutputGC = GCHandle.Alloc(_loopOutput, GCHandleType.Pinned)
            End If

            'tell the kernel its offset
            _clCalculationKernel.SetValueArgument(10, CInt((i - 1) * WorkSize))

            'execute!
            _clCommands.Execute(_clCalculationKernel, {(i - 1) * WorkSize}, {count}, Nothing, _clEvents)

            'read result into _loopOutput
            _clCommands.Read(_loopOutputBuffer, True, 0, count, _loopOutputGC.AddrOfPinnedObject(), _clEvents)
            _clCommands.Finish()

            'copy output of this loop into global output
            _loopOutput.CopyTo(_output, WorkSize * (i - 1))

            'time and progress measurement
            _watch.Stop()
            _overallTime += _watch.ElapsedMilliseconds
            _samplesDone += count
            _progress += 1

            'write log entry
            WriteLogMessage("Pass " & i & " of " & _loops & ": " & _watch.ElapsedMilliseconds & " ms @ " & GenerateCountString(count))

            'prevent long display freezes
            If i <> _loops Then Thread.Sleep(_waitTime)
        Next

        WriteLogMessage("Finished Calculating with average of " & Math.Round(_overallTime / _overallSamples * 1000000, 2) & " ns/Sample")

        _watch.Restart()

        'create buffers
        _outputBuffer = New ComputeBuffer(Of Vectors.Double5)(_clContext, ComputeMemoryFlags.ReadOnly, _output.Length)
        _bitmapBuffer = New ComputeBuffer(Of Byte)(_clContext, ComputeMemoryFlags.WriteOnly, Size.Width * Size.Height * 4)
        _outputGC = GCHandle.Alloc(_output, GCHandleType.Pinned)

        'copy results
        _clCommands.Write(_outputBuffer, True, 0, _output.Length, _outputGC.AddrOfPinnedObject, _clEvents)

        'custom actions of datasources
        If InsidePainter.NeedsData Then InsidePainter.DataSource.Init(_clCommands, _clProgram, _outputBuffer, InsidePainter.ParentInfo)
        If OutsidePainter.NeedsData Then OutsidePainter.DataSource.Init(_clCommands, _clProgram, _outputBuffer, InsidePainter.ParentInfo)

        'set constant arguments
        _clRenderingKernel.SetValueArgument(0, MaxIterations)
        _clRenderingKernel.SetValueArgument(1, AntiAliasingSamples)
        _clRenderingKernel.SetMemoryArgument(2, _outputBuffer)
        _clRenderingKernel.SetMemoryArgument(3, _bitmapBuffer)
        InsidePainter.SetArguments(_clRenderingKernel, _clCommands, _insidePainterPositions)
        OutsidePainter.SetArguments(_clRenderingKernel, _clCommands, _outsidePainterPositions)
        InsidePainter.DataSource.SetArguments(_clRenderingKernel, _clCommands, _insidePainterDataSourcePositions)
        OutsidePainter.DataSource.SetArguments(_clRenderingKernel, _clCommands, _outsidePainterDataSourcePositions)

        'paint
        _clCommands.Execute(_clRenderingKernel, New Long() {0}, New Long() {Size.Width * Size.Height}, Nothing, _clEvents)

        'read bitmap
        If UseFastBitmap Then
            Dim bmp As New FastBitmap(FastBitmapOutput)
            bmp.Width = Me.Size.Width
            bmp.Height = Me.Size.Height
            bmp.ColorDepth = 32

            Dim lineBuffer(Me.Size.Width * 4 - 1) As Byte
            Dim lineBufferGC = GCHandle.Alloc(lineBuffer, GCHandleType.Pinned)

            For y As Integer = 0 To Me.Size.Height - 1
                _clCommands.Read(_bitmapBuffer, True, y * Size.Width * 4, Size.Width * 4, lineBufferGC.AddrOfPinnedObject, _clEvents) '_bitmapBytesGC.AddrOfPinnedObject()

                bmp.WriteLine(lineBuffer)
            Next

            ReDim lineBuffer(0)
            lineBufferGC.Free()

            bmp.Close()
        Else
            'generate a new image
            _image = New Bitmap(Me.Size.Width, Me.Size.Height, PixelFormat.Format32bppRgb)
            Dim imageData = _image.LockBits(New Rectangle(0, 0, Size.Width, Size.Height), ImageLockMode.WriteOnly, PixelFormat.Format32bppRgb)

            _clCommands.Read(_bitmapBuffer, True, 0, Size.Width * Size.Height * 4, imageData.Scan0, _clEvents) '_bitmapBytesGC.AddrOfPinnedObject()

            _image.UnlockBits(imageData)
        End If

        _clCommands.Finish()

        _watch.Stop()
        WriteLogMessage("Painting: " & _watch.ElapsedMilliseconds & " ms (" & Math.Round(_watch.ElapsedMilliseconds / Size.Width / Size.Height * 1000000, 2) & " ns/Pixel)")

        'free not needed resources
        ReDim _loopOutput(0)
        ReDim _output(0)

        _loopOutputBuffer.Dispose()
        _loopOutputGC.Free()
        _bitmapBuffer.Dispose()
        _outputBuffer.Dispose()
        _outputGC.Free()

        'finished
        _progress = CInt(_totalPixels)
    End Sub

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    Protected Overridable Sub SetArguments(ByVal kernel As ComputeKernel)
    End Sub

    ''' <summary>
    ''' Called before calculation starts, place to (re)initialize private fields used during IterationStep
    ''' </summary>
    Protected Overridable Sub GetReady()
        'No must to override this although it is only for the iterator
    End Sub

#End Region

#Region "Screen <-> Viewport coordinate conversion"

    ''' <summary>
    ''' Points to screen.
    ''' </summary>
    ''' <param name="point">The point.</param>
    ''' <returns></returns>
    Public Function ViewportToScreen(ByVal point As ComplexNumber) As PointF
        If Not State = WorkingState.Working Or _scale = 0 Then
            _transformMatrix = New RotationMatrix(Rotation)
            _antiTransformMatrix = New RotationMatrix(-Rotation)

            _scaleX = Me.DefaultViewportSize.Width / Me.Size.Width / _zoomState.Factor
            _scaleY = Me.DefaultViewportSize.Height / Me.Size.Height / _zoomState.Factor
            _scale = Math.Max(_scaleX, _scaleY)
        End If

        Dim screenPoint As ComplexNumber = CType(_antiTransformMatrix * (point - _zoomState.Offset - DefaultOffset), ComplexNumber)
        Return New PointF(CSng(screenPoint.RealPart / _scale + Me.Size.Width / 2), CSng((screenPoint.ImaginaryPart / _scale - Me.Size.Height / 2) * -1))
    End Function

    ''' <summary>
    ''' Screens to point.
    ''' </summary>
    ''' <param name="point">The point.</param>
    ''' <returns></returns>
    Public Function ScreenToViewport(ByVal point As PointF) As ComplexNumber
        Return ScreenToViewport(point.X, point.Y)
    End Function

    ''' <summary>
    ''' Screens to point.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns></returns>
    Public Function ScreenToViewport(ByVal x As Double, ByVal y As Double) As ComplexNumber
        If Not State = WorkingState.Working Or _scale = 0 Then
            _transformMatrix = New RotationMatrix(Rotation)
            _antiTransformMatrix = New RotationMatrix(-Rotation)

            _scaleX = Me.DefaultViewportSize.Width / Me.Size.Width / _zoomState.Factor
            _scaleY = Me.DefaultViewportSize.Height / Me.Size.Height / _zoomState.Factor
            _scale = Math.Max(_scaleX, _scaleY)
        End If

        Return CType(_transformMatrix * (New ComplexNumber((x - Me.Size.Width / 2) * _scale, ((-y + Me.Size.Height / 2) * _scale))), ComplexNumber) + Me.DefaultOffset + _zoomState.Offset
    End Function

    ''' <summary>
    ''' Screens to viewport.
    ''' </summary>
    ''' <param name="point">The point.</param>
    ''' <returns></returns>
    Function ScreenToViewport(ByVal point As Windows.Point) As ComplexNumber
        Return ScreenToViewport(point.X, point.Y)
    End Function

#End Region

#Region "Zooming"

    ''' <summary>
    ''' Undoes the zoom.
    ''' </summary>
    Public Sub UndoZoom()
        'manual because otherwise the 'new' zoomstate would be pushed on the stack
        If ZoomStates.Count > 0 Then
            _zoomState = ZoomStates.Pop
        Else
            _zoomState = New ZoomState()
        End If

        OnPropertyChanged("ZoomState")
        OnPropertyChanged("CanReset")
        OnZoomStateChanged()
    End Sub

    ''' <summary>
    ''' Resets the zoom.
    ''' </summary>
    Public Sub ResetZoom()
        ZoomState = New ZoomState
        ZoomStates.Clear()
    End Sub

    ''' <summary>
    ''' Zooms on the specified rectangle.
    ''' </summary>
    ''' <param name="rect">The rectangle to zoom on (screen coordinates)</param>
    Public Sub Zoom(ByVal rect As RectangleF, ByVal angle As Double)
        Dim oldCenter = New PointF(CSng(Me.ZoomState.Offset.RealPart + Me.Size.Width / 2), CSng(Me.ZoomState.Offset.ImaginaryPart + Me.Size.Height / 2))
        Dim newCenter = New PointF(CSng(Me.ZoomState.Offset.RealPart + rect.X + rect.Width / 2), CSng(Me.ZoomState.Offset.ImaginaryPart + rect.Y + rect.Height / 2))

        Dim oldCenterViewPort = Me.ScreenToViewport(oldCenter)
        Dim newCenterViewPort = Me.ScreenToViewport(newCenter)

        Me.ZoomState = New ZoomState(Me.ZoomState.Offset + newCenterViewPort - oldCenterViewPort, Me.ZoomState.Factor * Math.Min(Me.Size.Width / rect.Width, Me.Size.Height / rect.Height), Me.ZoomState.Rotation + angle)
    End Sub

    ''' <summary>
    ''' Zooms on the specified rectangle.
    ''' </summary>
    ''' <param name="rect">The rectangle to zoom on (screen coordinates)</param>
    Sub Zoom(ByVal rect As System.Windows.Rect, ByVal angle As Double)
        Zoom(New RectangleF(CSng(rect.Left), CSng(rect.Top), CSng(rect.Width), CSng(rect.Height)), angle)
    End Sub

    ''' <summary>
    ''' Zooms the specified rect.
    ''' </summary>
    ''' <param name="rect">The rect.</param>
    Sub Zoom(ByVal rect As System.Windows.Rect)
        Zoom(rect, 0)
    End Sub

    ''' <summary>
    ''' Zooms in/out without changing the region.
    ''' </summary>
    ''' <param name="factor">The factor that should be applied to the zoom factor (2 would double the zoom, 1/2 halves it).</param>
    Sub Zoom(ByVal factor As Double)
        ZoomState = New ZoomState(ZoomState.Offset, ZoomState.Factor * factor, ZoomState.Rotation)
    End Sub

    ''' <summary>
    ''' Zooms the specified p.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <param name="factor">The factor.</param>
    Sub Zoom(ByVal p As Windows.Point, ByVal factor As Double)
        Dim r As New Windows.Rect

        r.Width = Size.Width / factor
        r.Height = Size.Height / factor
        r.X = p.X - r.Width / 2
        r.Y = p.Y - r.Height / 2

        Zoom(r)
    End Sub

#End Region


#Region "Loading & Saving"

    ''' <summary>
    ''' Saves the specified obj.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <param name="x">The x.</param>
    Public Shared Sub Save(ByVal obj As Fractal, ByVal x As XmlWriter)
        x.WriteStartElement("Type")
        x.WriteType(obj.GetType())
        x.WriteEndElement()

        x.WriteStartElement("Properties")

        x.WriteStartElement("Common")

        x.WriteStartElement("AntiAliasingSamples")
        x.WriteValue(obj.AntiAliasingSamples)
        x.WriteEndElement()

        x.WriteStartElement("BailoutValue")
        x.WriteValue(obj.BailoutValue)
        x.WriteEndElement()

        x.WriteStartElement("AutoAdjustIterations")
        x.WriteValue(obj.AutoAdjustIterations)
        x.WriteEndElement()

        x.WriteStartElement("MaxIterations")
        x.WriteValue(obj.MaxIterations)
        x.WriteEndElement()

        x.WriteStartElement("SmoothIterationCounts")
        x.WriteValue(obj.SmoothIterationCounts)
        x.WriteEndElement()

        x.WriteStartElement("Size")

        x.WriteStartElement("Width")
        x.WriteValue(obj.Size.Width)
        x.WriteEndElement()

        x.WriteStartElement("Height")
        x.WriteValue(obj.Size.Height)
        x.WriteEndElement()

        x.WriteEndElement()

        x.WriteStartElement("OutsidePainter")
        Painter.Save(obj.OutsidePainter, x)
        x.WriteEndElement()

        x.WriteStartElement("InsidePainter")
        Painter.Save(obj.InsidePainter, x)
        x.WriteEndElement()


        x.WriteStartElement("ZoomState")
        ZoomState.Save(obj.ZoomState, x)
        x.WriteEndElement()

        x.WriteStartElement("PreviousZoomStates")

        For Each z In obj.ZoomStates
            x.WriteStartElement("ZoomState")
            ZoomState.Save(z, x)
            x.WriteEndElement()
        Next

        x.WriteEndElement()

        x.WriteEndElement()

        x.WriteStartElement("Extra")
        obj.SaveParameters(x)
        x.WriteEndElement()

        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Saves the settings of this fractal to the specified destination.
    ''' </summary>
    ''' <param name="destination">The destination.</param>
    ''' <param name="obj">The fractal to save.</param>
    Public Shared Sub Save(ByVal obj As Fractal, ByVal destination As Stream)
        Using x = XmlWriter.Create(destination, New XmlWriterSettings With {.Indent = True})
            x.WriteStartDocument()
            x.WriteStartElement("Fractal")

            Save(obj, x)

            x.WriteEndElement()
            x.WriteEndDocument()
        End Using
    End Sub

    ''' <summary>
    ''' Saves the settings of this fractal to the specified destination.
    ''' </summary>
    ''' <param name="filename">The destination.</param>
    ''' <param name="obj">The fractal to save.</param>
    Public Shared Sub Save(ByVal obj As Fractal, ByVal filename As String)
        Using fs As New FileStream(filename, FileMode.Create)
            Save(obj, fs)
        End Using
    End Sub

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub SaveParameters(ByVal x As XmlWriter)

    ''' <summary>
    ''' Loads a fractal from the specified file.
    ''' </summary>
    ''' <param name="filename">The file to load from.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal filename As String) As Fractal
        If Not File.Exists(filename) Then Throw New Exception("The file does not exist")

        Using fs As New FileStream(filename, FileMode.Open)
            Return Load(fs)
        End Using
    End Function

    ''' <summary>
    ''' Loads a fractal from the specified source.
    ''' </summary>
    ''' <param name="source">The source.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal source As Stream) As Fractal
        Dim fx = XDocument.Load(source).Element("Fractal")

        Return Load(fx)
    End Function

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As Fractal
        Dim cp = x.Element("Properties").Element("Common")
        Dim ep = x.Element("Properties").Element("Extra")

        Dim inst As Fractal = CType(Activator.CreateInstance(SaveUtils.LoadType(x.Element("Type"))), Fractal)

        inst.AntiAliasingSamples = CInt(cp.Element("AntiAliasingSamples").Value)
        inst.BailoutValue = CDbl(cp.Element("BailoutValue").Value)
        inst.AutoAdjustIterations = CBool(cp.Element("AutoAdjustIterations").Value)
        inst.MaxIterations = CInt(cp.Element("MaxIterations").Value)
        inst.SmoothIterationCounts = CBool(cp.Element("SmoothIterationCounts").Value)
        inst.ZoomState = ZoomState.Load(cp.Element("ZoomState"))
        inst.OutsidePainter = Painter.Load(cp.Element("OutsidePainter"))
        inst.InsidePainter = Painter.Load(cp.Element("InsidePainter"))

        Dim w = CInt(cp.Element("Size").Element("Width").Value)
        Dim h = CInt(cp.Element("Size").Element("Height").Value)

        inst.Size = New Size(w, h)

        inst._zoomStates.Clear()

        For Each zx In cp.Element("PreviousZoomStates").Elements
            inst._zoomStates.Push(ZoomState.Load(zx))
        Next

        inst.LoadParameters(ep)

        Return inst
    End Function

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub LoadParameters(ByVal x As XElement)

#End Region


#Region "Other"

    ''' <summary>
    ''' Refreshes the iterations.
    ''' </summary>
    Private Sub RefreshIterations()
        If Me.AutoAdjustIterations Then
            Dim old = _maxIterations

            Try
                _maxIterations = CInt(Me.DefaultMaxIterationCount + (Math.Log(Me.ZoomState.Factor) * Me.DefaultMaxIterationCount))
            Catch ex As Exception
                _maxIterations = -1
            End Try

            If _maxIterations <= DefaultMaxIterationCount Then _maxIterations = Me.DefaultMaxIterationCount
            If _outsidePainter.DataSource.MaximumIterationCountIndependent Then _maxIterations = CInt(_maxIterations * 1.6)

            If old <> _maxIterations Then OnPropertyChanged("MaxIterations")
        End If
    End Sub

    ''' <summary>
    ''' Creates the hash.
    ''' </summary>
    ''' <returns></returns>
    Private Function CreateHash() As Byte()
        'Collect all information on this instance
        Using infoStream As New MemoryStream
            Fractal.Save(Me, infoStream)
            infoStream.Position = 0

            Using md5 As New MD5CryptoServiceProvider
                Return md5.ComputeHash(infoStream)
            End Using
        End Using
    End Function

    ''' <summary>
    ''' Hashes to string.
    ''' </summary>
    ''' <param name="hash">The hash.</param>
    ''' <returns></returns>
    Private Shared Function HashToString(ByVal hash() As Byte) As String
        Dim hashstr = ""

        For Each b In hash
            hashstr &= BitConverter.ToString(New Byte() {b}) '16 Bytes
        Next

        Return hashstr
    End Function

    ''' <summary>
    ''' Gets the offset.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <param name="width">The width.</param>
    ''' <param name="stride">The stride.</param>
    ''' <returns></returns>
    Protected Shared Function GetOffset(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal stride As Integer) As Integer
        Return stride * (y * width + x)
    End Function

    ''' <summary>
    ''' Gets the offset.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <param name="width">The width.</param>
    ''' <returns></returns>
    Protected Shared Function GetOffset(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer) As Integer
        Return 4 * (y * width + x)
    End Function

    ''' <summary>
    ''' Generates the count string.
    ''' </summary>
    ''' <param name="count">The count.</param>
    ''' <returns></returns>
    Private Function GenerateCountString(ByVal count As Integer) As String
        Dim countStr As String

        If count >= 1000000 Then
            countStr = Math.Round(count / 1000000, 2) & "M Sample" & If(count = 1000000, "", "s")
        Else
            countStr = Math.Round(count / 1000, 2) & "k Sample" & If(count = 1000, "", "s")
        End If

        Return countStr
    End Function

    ''' <summary>
    ''' Handles the PropertyChanged event of the _insidePainter control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.PropertyChangedEventArgs" /> instance containing the event data.</param>
    Private Sub InsidePainter_PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Handles _insidePainter.PropertyChanged
        OnFractalSettingChanged("InsidePainterProperty")

        If e.PropertyName = "DataSource" Then _clInited = False
    End Sub

    ''' <summary>
    ''' Handles the PropertyChanged event of the _outsidePainter control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.PropertyChangedEventArgs" /> instance containing the event data.</param>
    Private Sub OutsidePainter_PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Handles _outsidePainter.PropertyChanged
        OnFractalSettingChanged("OutsidePainterProperty")

        If e.PropertyName = "DataSource" Then _clInited = False
    End Sub

#End Region

#Region "Events"

    ''' <summary>
    ''' Called when [zoom state changed].
    ''' </summary>
    Private Sub OnZoomStateChanged()
        NeedsRecalculation = True

        RaiseEvent ZoomStateChanged(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [fractal setting changed].
    ''' </summary>
    Protected Sub OnFractalSettingChanged(ByVal propertyName As String)
        NeedsRecalculation = True

        RaiseEvent FractalSettingChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

#End Region

#Region "ICloneable Support"

    ''' <summary>
    ''' Creates a new object that is a copy of the current instance.
    ''' </summary>
    ''' <returns>
    ''' A new object that is a copy of this instance.
    ''' </returns>
    Public Overridable Function Clone() As Object Implements System.ICloneable.Clone
        Using ms As New MemoryStream
            Fractal.Save(Me, ms)

            ms.Position = 0

            Return Fractal.Load(ms)
        End Using
    End Function

#End Region

#Region "IDisposable Support"

    ''' <summary>
    ''' Releases unmanaged and - optionally - managed resources
    ''' </summary>
    ''' <param name="disposing"><c>true</c> to release both managed and unmanaged resources; <c>false</c> to release only unmanaged resources.</param>
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me._disposedValue Then
            If disposing Then
                'dispose managed state (managed objects).
                If _clInited Then
                    'setup opencl objects
                    _clContext.Dispose()
                    _clCommands.Dispose()
                    _clProgram.Dispose()
                    _clCalculationKernel.Dispose()

                    _clInited = False
                End If

                If Not _image Is Nothing Then _image.Dispose()
                _zoomStates.Clear()
            End If

            'free unmanaged resources (unmanaged objects) and override Finalize() below.
            'set large fields to null.
        End If
        Me._disposedValue = True
    End Sub

    ''' <summary>
    ''' Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
    ''' </summary>
    Public Sub Dispose() Implements IDisposable.Dispose
        ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub
#End Region
#End Region

#Region "Events"

    ''' <summary>
    ''' Occurs when [zoom state changed].
    ''' </summary>
    Public Event ZoomStateChanged(ByVal sender As Object, ByVal e As EventArgs)

    ''' <summary>
    ''' Occurs when [setting changed].
    ''' </summary>
    Public Event FractalSettingChanged(ByVal sender As Object, ByVal e As PropertyChangedEventArgs)

#End Region

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overridable Sub EvaluateCommandLineArguments() Implements ICommandLineExtender.EvaluateCommandLineArguments
    End Sub

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overridable ReadOnly Property CommandLineArguments As Argument() Implements ICommandLineExtender.CommandLineArguments
        Get
            Return New Argument() {}
        End Get
    End Property
End Class
