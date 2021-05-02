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
Imports System.ComponentModel
Imports Fractals.Utilities

''' <summary>
''' Base class for classes having to do with rendering. 
''' </summary>
Public MustInherit Class Renderer
    Implements INotifyPropertyChanged

    Private _renderingThread As New Thread(AddressOf Render)
    Protected _paused As Boolean
    Private _watch As New Stopwatch
    Private _lastLogMessage As String
    Private _completed As Boolean = True

    ''' <summary>
    ''' Gets the progress of the rendering.
    ''' </summary>
    ''' <value>The progress.</value>
    Public MustOverride ReadOnly Property Progress As Double

    ''' <summary>
    ''' Gets the (estimated) remaining time to complete the rendering.
    ''' </summary>
    ''' <value>The remaining time.</value>
    Public ReadOnly Property RemainingTime As TimeSpan
        Get
            If Progress = 0 Then Return TimeSpan.FromTicks(0)

            Return TimeSpan.FromTicks(CLng(_watch.Elapsed.Ticks / Progress * (1 - Progress)))
        End Get
    End Property

    ''' <summary>
    ''' Gets the elapsed time since the rendering was started.
    ''' </summary>
    ''' <value>The elapsed time.</value>
    Public ReadOnly Property ElapsedTime As TimeSpan
        Get
            Return _watch.Elapsed
        End Get
    End Property

    ''' <summary>
    ''' Gets the current working state.
    ''' </summary>
    ''' <value>The state.</value>
    Public ReadOnly Property State As WorkingState
        Get
            If _completed Then
                Return WorkingState.None
            Else
                If _paused Then
                    Return WorkingState.Paused
                Else
                    Return WorkingState.Working
                End If
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the last log message.
    ''' </summary>
    ''' <value>The last log message.</value>
    Public ReadOnly Property LastLogMessage As String
        Get
            Return _lastLogMessage
        End Get
    End Property

    ''' <summary>
    ''' Renders whatever this class is supposed to render.
    ''' </summary>
    Protected MustOverride Sub Render()

    ''' <summary>
    ''' Starts the rendering.
    ''' </summary>
    Public Sub StartRendering()
        'make sure there is no on-going rendering process
        AbortRendering()

        _renderingThread = New Thread(Sub()
                                          OnRenderingStarted()

                                          Try
                                              Render()

                                              Succeeded = True
                                          Catch ex As Exception
                                              Succeeded = False
                                              Failure = ex
                                          End Try

                                          OnRenderingCompleted()
                                      End Sub)

        _renderingThread.Start()
    End Sub

    ''' <summary>
    ''' Resumes the rendering.
    ''' </summary>
    Public Sub ResumeRendering()
        If _paused Then OnRenderingResumed()
    End Sub

    ''' <summary>
    ''' Suspends the rendering.
    ''' </summary>
    Public Sub SuspendRendering()
        If _renderingThread.IsAlive Then OnRenderingSuspended()
    End Sub

    ''' <summary>
    ''' Aborts the rendering.
    ''' </summary>
    Public Sub AbortRendering()
        If _renderingThread.IsAlive Then OnRenderingAborted()
    End Sub

    ''' <summary>
    ''' Writes a log message.
    ''' </summary>
    ''' <param name="message">The message.</param>
    Protected Sub WriteLogMessage(ByVal message As String)
        OnMessageLogged(message)
    End Sub

    ''' <summary>
    ''' Blocks the calling thread until rendering is completed
    ''' </summary>
    Public Sub Join()
        _renderingThread.Join()
    End Sub

    ''' <summary>
    ''' Gets or sets a value indicating whether this <see cref="Renderer" /> is success.
    ''' </summary>
    ''' <value><c>true</c> if success; otherwise, <c>false</c>.</value>
    Public Property Succeeded As Boolean

    ''' <summary>
    ''' Gets or sets the failure cause.
    ''' </summary>
    ''' <value>The failure cause.</value>
    Public Property Failure As Exception

    ''' <summary>
    ''' Called when [rendering started].
    ''' </summary>
    Private Sub OnRenderingStarted()
        _watch.Restart()
        _completed = False
        _paused = False
        OnPropertyChanged("State")
        WriteLogMessage("Rendering started")

        RaiseEvent RenderingStarted(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [rendering completed].
    ''' </summary>
    Private Sub OnRenderingCompleted()
        _watch.Stop()
        _completed = True
        OnPropertyChanged("State")

        If Succeeded Then
            WriteLogMessage("Rendering completed")
        End If

        RaiseEvent RenderingCompleted(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [rendering suspended].
    ''' </summary>
    Private Sub OnRenderingSuspended()
        _watch.Stop()
        _paused = True
        OnPropertyChanged("State")
        WriteLogMessage("Rendering suspended")

        RaiseEvent RenderingSuspended(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [rendering resumed].
    ''' </summary>
    Private Sub OnRenderingResumed()
        _watch.Start()
        _paused = False
        OnPropertyChanged("State")
        WriteLogMessage("Rendering resumed")

        RaiseEvent RenderingResumed(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [rendering aborted].
    ''' </summary>
    Private Sub OnRenderingAborted()
        _watch.Stop()
        _renderingThread.Abort()
        _completed = True
        OnPropertyChanged("State")
        WriteLogMessage("Rendering aborted")

        RaiseEvent RenderingAborted(Me, New EventArgs)
    End Sub

    ''' <summary>
    ''' Called when [message logged].
    ''' </summary>
    ''' <param name="message">The message.</param>
    Private Sub OnMessageLogged(ByVal message As String)
        _lastLogMessage = message
        OnPropertyChanged("LastLogMessage")
        Trace.WriteLine(message)

        RaiseEvent MessageLogged(Me, New MessageLoggedEventArgs(message))
    End Sub

    ''' <summary>
    ''' Called when [property changed].
    ''' </summary>
    ''' <param name="propertyName">Name of the property.</param>
    Protected Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    ''' <summary>
    ''' Occurs when the rendering was started.
    ''' </summary>
    Public Event RenderingStarted(ByVal sender As Object, ByVal e As EventArgs)
    ''' <summary>
    ''' Occurs when the rendering was suspended.
    ''' </summary>
    Public Event RenderingSuspended(ByVal sender As Object, ByVal e As EventArgs)
    ''' <summary>
    ''' Occurs when the rendering was resumed.
    ''' </summary>
    Public Event RenderingResumed(ByVal sender As Object, ByVal e As EventArgs)

    ''' <summary>
    ''' Occurs when the rendering was aborted.
    ''' </summary>
    Public Event RenderingAborted(ByVal sender As Object, ByVal e As EventArgs)

    ''' <summary>
    ''' Occurs when the rendering was completed.
    ''' </summary>
    Public Event RenderingCompleted(ByVal sender As Object, ByVal e As EventArgs)

    ''' <summary>
    ''' Occurs when a message was logged.
    ''' </summary>
    Public Event MessageLogged(ByVal sender As Object, ByVal e As MessageLoggedEventArgs)

    ''' <summary>
    ''' Occurs when a property value changes.
    ''' </summary>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As PropertyChangedEventArgs) Implements INotifyPropertyChanged.PropertyChanged
End Class
