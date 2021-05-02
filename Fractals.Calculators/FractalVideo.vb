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
Imports System.Windows.Forms
Imports System.Drawing
Imports Fractals.Utilities
Imports System.Drawing.Imaging
Imports System.Threading
Imports System.Xml.Linq
Imports System.Xml
Imports System.Text

Public Class FractalVideo
    Inherits Renderer

    Public Shared ReadOnly Property FFMpegPath As String
        Get
            Dim base = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "ffmpeg")

            If File.Exists(base & ".exe") Then Return base & ".exe"
            If Environment.Is64BitOperatingSystem And File.Exists(base & "64.exe") Then Return base & "64.exe"
            If File.Exists(base & "32.exe") Then Return base & "32.exe"

            Return ""
        End Get
    End Property

    Public Shared Property TempPath As String = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Application.ProductName & "\FramesTemp\")
    Public Shared Property OptimizeMemoryUsage As Boolean = True
    Public Shared Property UseNeedsRecalculation As Boolean = True

    Public Property Fractal As Fractal
    Public Property FrameCount As Integer
    Public Property RenderFrameCount As Integer
    Public Property FPS As Integer
    Public Property Bitrate As Integer
    Public Property Storyboard As New List(Of Animation)
    Public Property Codec As String
    Public Property OutputFileName As String
    Public Property ReUse As Boolean

    Private _currentFrame As Integer
    Private _currentSpeed As Long
    Private _encodingThread As Thread
    Private _lastSpeed As Double

    ''' <summary>
    ''' Gets the count.
    ''' </summary>
    Private ReadOnly Property Count As Integer
        Get
            If RenderFrameCount <> 0 Then
                Return RenderFrameCount
            Else
                Return FrameCount
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the current frame.
    ''' </summary>
    ''' <value>The current frame.</value>
    Public ReadOnly Property CurrentFrame As Integer
        Get
            Return _currentFrame
        End Get
    End Property

    ''' <summary>
    ''' Gets the current speed.
    ''' </summary>
    ''' <value>The current speed.</value>
    Public ReadOnly Property CurrentSpeed As Double
        Get
            Return 1 / _currentSpeed * 1000
        End Get
    End Property

    ''' <summary>
    ''' Gets the average speed.
    ''' </summary>
    ''' <value>The average speed.</value>
    Public ReadOnly Property AverageSpeed As Double
        Get
            If State = WorkingState.None Then
                Return _lastSpeed
            Else
                Return (_currentFrame - 1) / ElapsedTime.TotalMilliseconds * 1000
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the progress of the rendering.
    ''' </summary>
    ''' <value>The progress.</value>
    Public Overrides ReadOnly Property Progress As Double
        Get
            If State <> WorkingState.None Then
                Return ((_currentFrame + 1) / Count + If(Fractal.State <> WorkingState.None, Fractal.Progress / Count, 0)) * 0.9
            Else
                Return 1
            End If
        End Get
    End Property

    ''' <summary>
    ''' Initializes a new instance of the <see cref="FractalVideo" /> class.
    ''' </summary>
    ''' <param name="baseFractal">The base fractal.</param>
    Public Sub New(ByVal baseFractal As Fractal, ByVal frameCount As Integer, ByVal fps As Integer, ByVal output As String)
        Me.FrameCount = frameCount
        Me.OutputFileName = output
        Me.FPS = fps

        Fractal = baseFractal
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="FractalVideo" /> class.
    ''' </summary>
    Public Sub New()
        Me.Fractal = New Multibrot
        Me.OutputFileName = "out.avi"
        Me.FPS = 20
        Me.FrameCount = 100
    End Sub

    ''' <summary>
    ''' Renders whatever this class is supposed to render.
    ''' </summary>
    Protected Overrides Sub Render()
        Try
            If Not File.Exists(FFMpegPath) Then Throw New Exception("ffmpeg not found")
            If Not IO.Directory.Exists(TempPath) Then IO.Directory.CreateDirectory(TempPath)
            If File.Exists(OutputFileName) Then File.Delete(OutputFileName)

            If FrameCount <= 0 Then Throw New ArgumentException("Framecount must not be <= 0")
            If Bitrate < 0 Then Throw New ArgumentException("Bitrate must not be < 0")
            If OutputFileName = "" Then Throw New ArgumentException("OutputFileName may not be empty")
            If FPS <= 0 Then Throw New ArgumentException("FPS has to be a nonnegative number")

            Dim digits = CInt(Math.Floor(Math.Log10(FrameCount) + 1))
            Dim formatString As String = ""

            For i As Integer = 1 To digits
                formatString &= "0"
            Next

            For Each f In Directory.GetFiles(TempPath, "*.png")
                If Not ReUse Or CDbl(Path.GetFileNameWithoutExtension(f)) >= FrameCount Then
                    File.Delete(f)
                End If
            Next

            Dim storyCopy As New List(Of Animation)

            For Each ani In Storyboard
                Dim mem As New MemoryStream
                Dim x As New XmlTextWriter(mem, Encoding.UTF8)

                x.WriteStartDocument()
                x.WriteStartElement("Ani")
                Animation.Save(ani, x)
                x.WriteEndElement()
                x.WriteEndDocument()
                x.Flush()

                mem.Position = 0
                Dim copy = Animation.Load(XDocument.Load(mem).Root)

                copy.StartFrame = CInt(ani.StartFrame * (Count / FrameCount))
                copy.EndFrame = CInt(ani.EndFrame * (Count / FrameCount))

                storyCopy.Add(copy)

                mem.Dispose()
            Next

            For Me._currentFrame = 0 To Count - 1
                While _paused
                    Thread.Sleep(50)
                End While

                If _currentFrame Mod 300 = 0 And OptimizeMemoryUsage Then
                    Dim saveStream As New MemoryStream
                    Fractal.Save(Fractal, saveStream)
                    saveStream.Position = 0

                    Fractal.Dispose()
                    GC.Collect()

                    Fractal = Fractal.Load(saveStream)
                End If

                Dim startTime As Long = CLng(ElapsedTime.TotalMilliseconds)

                For Each ani In storyCopy
                    If ani.StartFrame <= _currentFrame And _currentFrame <= ani.EndFrame Then
                        ani.Modify(Fractal, CInt(_currentFrame - ani.StartFrame))
                    End If
                Next

                Dim fileName = Path.Combine(TempPath, _currentFrame.ToString(formatString) & ".png")

                Dim reusable As Boolean = False

                If File.Exists(fileName) And ReUse Then
                    Try
                        Dim b As New Bitmap(fileName)
                        If b.Size = Fractal.Size Then reusable = True
                    Catch ex As Exception
                    End Try
                End If

                If Not (reusable) Then
                    If File.Exists(fileName) Then File.Delete(fileName)

                    If Fractal.NeedsRecalculation Or Not UseNeedsRecalculation Then
                        WriteLogMessage(String.Format("Calculating Frame {0}", _currentFrame))

                        Fractal.StartRendering()
                        Fractal.Join()
                    Else
                        WriteLogMessage(String.Format("Skipping Frame {0}", _currentFrame))
                    End If

                    Fractal.Image.Save(fileName, ImageFormat.Png)
                Else
                    WriteLogMessage(String.Format("Re-using Frame {0}", _currentFrame))
                End If

                _currentSpeed = CLng(ElapsedTime.TotalMilliseconds) - startTime

                'force garbage collection
                GC.Collect()
            Next

            'current frame is incremented after each pass in the for loop, even after the last one, so it ends with TotalFrames, which obviously is incorrect.
            _currentFrame = Count - 1

            WriteLogMessage("Calculation complete")
            WriteLogMessage("Encoding video")

            Dim arguments = ""

            If Bitrate = 0 Then
                arguments &= "-sameq "
            Else
                arguments &= String.Format("-b {0} ", Bitrate)
            End If

            If Codec <> "" Then
                arguments &= String.Format("-vcodec {0} ", Codec)
            End If

            arguments &= String.Format("-r {0} ", FPS)
            arguments &= String.Format("-i %0{0}d.png ", digits)
            arguments &= String.Format("""{0}"" ", Path.GetFullPath(OutputFileName))

            Dim p = Process.Start(New ProcessStartInfo(FFMpegPath, arguments) With {.WorkingDirectory = TempPath, .WindowStyle = ProcessWindowStyle.Hidden})

            p.WaitForExit()

            WriteLogMessage("Encoding complete")
            WriteLogMessage("Deleting temporary files")

            For Each f In Directory.GetFiles(TempPath, "*.png")
                File.Delete(f)
            Next

            _lastSpeed = (_currentFrame - 1) / ElapsedTime.TotalMilliseconds * 1000
        Catch ex As ThreadAbortException
        End Try
    End Sub

#Region "Loading & Saving"

    ''' <summary>
    ''' Saves the settings of this fractal to the specified destination.
    ''' </summary>
    ''' <param name="destination">The destination.</param>
    ''' <param name="obj">The fractal to save.</param>
    Public Shared Sub Save(ByVal obj As FractalVideo, ByVal destination As Stream)
        Using x = XmlWriter.Create(destination, New XmlWriterSettings With {.Indent = True})
            x.WriteStartDocument()
            x.WriteStartElement("FractalVideo")

            Save(obj, x)

            x.WriteEndElement()
        End Using
    End Sub

    ''' <summary>
    ''' Saves the specified obj.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <param name="x">The x.</param>
    Public Shared Sub Save(ByVal obj As FractalVideo, ByVal x As XmlWriter)
        x.WriteStartElement("Type")
        x.WriteType(obj.GetType())
        x.WriteEndElement()

        x.WriteStartElement("Fractal")
        Fractal.Save(obj.Fractal, x)
        x.WriteEndElement()

        x.WriteStartElement("FrameCount")
        x.WriteValue(obj.FrameCount)
        x.WriteEndElement()

        x.WriteStartElement("Storyboard")

        For Each ani In obj.Storyboard
            x.WriteStartElement("Animation")

            Animation.Save(ani, x)

            x.WriteEndElement()
        Next

        x.WriteEndElement()
        x.WriteEndDocument()
    End Sub

    ''' <summary>
    ''' Saves the settings of this fractal to the specified destination.
    ''' </summary>
    ''' <param name="filename">The destination.</param>
    ''' <param name="obj">The fractal to save.</param>
    Public Shared Sub Save(ByVal obj As FractalVideo, ByVal filename As String)
        Using fs As New FileStream(filename, FileMode.Create)
            Save(obj, fs)
        End Using
    End Sub

    ''' <summary>
    ''' Loads a fractal from the specified file.
    ''' </summary>
    ''' <param name="filename">The file to load from.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal filename As String) As FractalVideo
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
    Public Shared Function Load(ByVal source As Stream) As FractalVideo
        Dim fx = XDocument.Load(source).Element("FractalVideo")

        Dim inst As FractalVideo = CType(Activator.CreateInstance(SaveUtils.LoadType(fx.Element("Type"))), FractalVideo)

        inst.FrameCount = CInt(fx.Element("FrameCount").Value)
        inst.Fractal = Fractal.Load(fx.Element("Fractal"))

        For Each zx In fx.Element("Storyboard").Elements
            inst.Storyboard.Add(Animation.Load(zx))
        Next

        Return inst
    End Function

#End Region

End Class
