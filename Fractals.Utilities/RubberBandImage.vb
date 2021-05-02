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

Imports System.Windows.Controls
Imports System.Windows
Imports System.Windows.Media
Imports System.Windows.Input

''' <summary>
''' Rubberband Image Control for WPF
''' </summary>
Public Class RubberBandImage
    Inherits Image

#Region "Private Fields"

    Private _mousePosition As New Point(-1, -1)
    Private _crossHairPen As New Pen(Brushes.LightCyan, 1)
    Private _bandStart As New Point(-1, -1)
    Private _bandEnd As New Point(-1, -1)
    Private _creating As Boolean

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets a value indicating whether this <see cref="RubberbandImage" /> is currently creating a rubberband.
    ''' </summary>
    ''' <value><c>true</c> if currently creating; otherwise, <c>false</c>.</value>
    Public ReadOnly Property Creating As Boolean
        Get
            Return _creating
        End Get
    End Property

    ''' <summary>
    ''' Gets the rubber rectangle.
    ''' </summary>
    ''' <value>The rubber rectangle.</value>
    Private ReadOnly Property RubberRectangle As Rect
        Get
            Return New Rect(_bandStart, _bandEnd)
        End Get
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether this <see cref="RubberbandImage" /> is enabled.
    ''' </summary>
    ''' <value><c>true</c> if enabled; otherwise, <c>false</c>.</value>
    Public Property BandEnabled As Boolean = True

    ''' <summary>
    ''' Gets or sets a value indicating whether to show a crosshair.
    ''' </summary>
    ''' <value><c>true</c> if a crosshair is shown; otherwise, <c>false</c>.</value>
    Public Property ShowCrosshair As Boolean = True

    ''' <summary>
    ''' Gets or sets a value indicating whether to show a rectangle with the same ratios.
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if a rectangle with the same ratios should be shown; otherwise, <c>false</c>.
    ''' </value>
    Public Property ShowUniformRatioRectangle As Boolean

#End Region

#Region "Methods"

    ''' <summary>
    ''' Resets this instance.
    ''' </summary>
    Private Sub Reset()
        'reset 
        _bandStart = New Point(-1, -1)
        _bandEnd = New Point(-1, -1)
        _creating = False

        Me.InvalidateVisual()
    End Sub

    ''' <summary>
    ''' Handles the MouseLeftButtonUp event of the RubberbandImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseButtonEventArgs" /> instance containing the event data.</param>
    Private Sub RubberbandImage_MouseLeftButtonUp(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles Me.MouseLeftButtonUp
        If BandEnabled And _creating Then
            If New Rect(_bandStart, _bandEnd).Width > 0 And New Rect(_bandStart, _bandEnd).Height > 0 Then
                OnRubberBandCreated()
            Else
                Reset()
            End If
        End If
    End Sub

    ''' <summary>
    ''' Handles the MouseLeftButtonDown event of the RubberbandImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseButtonEventArgs" /> instance containing the event data.</param>
    Private Sub RubberbandImage_MouseLeftButtonDown(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles Me.MouseLeftButtonDown
        If BandEnabled Then
            _bandStart = e.GetPosition(Me)
            _bandEnd = e.GetPosition(Me)
            _creating = True

            Me.InvalidateVisual()
        End If
    End Sub

    ''' <summary>
    ''' Handles the MouseEnter event of the RubberbandImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub RubberbandImage_MouseEnter(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.MouseEnter
        If ShowCrosshair Then Mouse.OverrideCursor = Cursors.None
    End Sub

    ''' <summary>
    ''' Handles the MouseLeave event of the RubberbandImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub RubberbandImage_MouseLeave(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.MouseLeave
        _mousePosition = New Point(-1, -1)
        Mouse.OverrideCursor = Cursors.Arrow

        'reset 
        _bandStart = New Point(-1, -1)
        _bandEnd = New Point(-1, -1)
        _creating = False

        Me.InvalidateVisual()
    End Sub

    ''' <summary>
    ''' Handles the MouseMove event of the RubberbandImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub RubberbandImage_MouseMove(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.MouseMove
        _mousePosition = e.GetPosition(Me)

        If BandEnabled And _creating Then
            _bandEnd = e.GetPosition(Me)
        End If

        If ShowCrosshair Or _creating Then Me.InvalidateVisual()
    End Sub

    ''' <summary>
    ''' Renders the contents of an <see cref="T:System.Windows.Controls.Image" />.
    ''' </summary>
    ''' <param name="dc">An instance of <see cref="T:System.Windows.Media.DrawingContext" /> used to render the control.</param>
    Protected Overrides Sub OnRender(ByVal dc As DrawingContext)
        MyBase.OnRender(dc)

        If ShowCrosshair And Not _mousePosition = New Point(-1, -1) Then
            'draw crosshair, split one line so that the intersection has the same alpha value as the other pixels on the line

            dc.DrawLine(_crossHairPen, New Point(_mousePosition.X, 0), New Point(_mousePosition.X, _mousePosition.Y - _crossHairPen.Thickness / 2))
            dc.DrawLine(_crossHairPen, New Point(_mousePosition.X, _mousePosition.Y + _crossHairPen.Thickness / 2), New Point(_mousePosition.X, Me.ActualHeight))

            dc.DrawLine(_crossHairPen, New Point(0, _mousePosition.Y), New Point(Me.ActualWidth, _mousePosition.Y))
        End If

        If BandEnabled Then
            Dim rect = RubberRectangle

            dc.DrawRectangle(New SolidColorBrush(Color.FromArgb(120, Colors.LightCyan.R, Colors.LightCyan.G, Colors.LightCyan.B)), New Pen(Brushes.LightCyan, 1), rect)

            If ShowUniformRatioRectangle Then
                Dim ratio = Math.Min(Me.ActualWidth / rect.Width, Me.ActualHeight / rect.Height)

                Dim uniSize As New Size(Me.ActualWidth / ratio, Me.ActualHeight / ratio)
                Dim uniCenter As New Point(rect.Left + rect.Width / 2 - uniSize.Width / 2, rect.Top + rect.Height / 2 - uniSize.Height / 2)

                Dim uniRect As New Rect(uniCenter, uniSize)

                dc.DrawRectangle(New SolidColorBrush(Color.FromArgb(120, Colors.LightGreen.R, Colors.LightGreen.G, Colors.LightGreen.B)), New Pen(Brushes.LightCyan, 1), uniRect)
            End If
        End If
    End Sub

#End Region

#Region "Events"

    ''' <summary>
    ''' Called when [rubber band created].
    ''' </summary>
    Private Sub OnRubberBandCreated()
        RaiseEvent RubberBandCreated(Me, New RubberBandCreatedEventArgs(New Rect(_bandStart, _bandEnd), 0))

        Reset()
    End Sub

    Public Event RubberBandCreated(ByVal sender As Object, ByVal e As RubberBandCreatedEventArgs)

#End Region
End Class
