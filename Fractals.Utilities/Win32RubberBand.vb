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

Imports System.Windows.Forms
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports Fractals.Mathematics

''' <summary>
''' Implements a rubberband that can be bound to a control (e.g. a picture box, see Fractals.GUI.MainForm)
''' </summary>
Public Class Win32RubberBand
#Region "Fields"

    Private WithEvents _parent As Control
    Private _rubberStart As New Point(-1, -1)
    Private _rubberEnd As New Point(-1, -1)
    Private _movementStart As New Point
    Private _movementEnd As New Point
    Private _resizeStart As New Point
    Private _resizeEnd As New Point
    Private _rotationStart As New Point
    Private _rotationEnd As New Point
    Private _rotationAngle As Double
    Private _resizeXDirection As Direction
    Private _resizeYDirection As Direction
    Private _hovered As Boolean
    Private _action As BandActions = BandActions.None
    Private _bitmap As New Bitmap(1, 1)
    Private _enabled As Boolean = True
    Private _showAxes As Boolean = True
    Private _showFactor As Boolean = True

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the inner size margin.
    ''' </summary>
    ''' <value>The size margin inner.</value>
    Public Property SizeMarginInner As Integer = 11

    ''' <summary>
    ''' Gets or sets the outer size margin.
    ''' </summary>
    ''' <value>The size margin outer.</value>
    Public Property SizeMarginOuter As Integer = 3

    ''' <summary>
    ''' Gets or sets the rotation box distance.
    ''' </summary>
    ''' <value>The rotation box distance.</value>
    Public Property RotationBoxDistance As Integer = -35

    ''' <summary>
    ''' Gets or sets the size of the rotation box.
    ''' </summary>
    ''' <value>The size of the rotation box.</value>
    Public Property RotationBoxSize As Integer = 60

    ''' <summary>
    ''' Gets or sets the fill alpha.
    ''' </summary>
    ''' <value>The fill alpha.</value>
    Public Property FillAlpha As Byte = 120

    ''' <summary>
    ''' Gets or sets the line alpha.
    ''' </summary>
    ''' <value>The line alpha.</value>
    Public Property LineAlpha As Byte = 200

    ''' <summary>
    ''' Gets or sets a value indicating whether this <see cref="Win32RubberBand" /> is enabled.
    ''' </summary>
    ''' <value><c>true</c> if enabled; otherwise, <c>false</c>.</value>
    Public Property Enabled As Boolean
        Get
            Return _enabled
        End Get
        Set(ByVal value As Boolean)
            _enabled = value

            If Not value Then
                _rubberEnd = New Point(-1, -1)
                _rubberStart = New Point(-1, -1)
                _action = BandActions.None
            End If

            _parent.Refresh()
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether to draw the rubber band directly on the graphics or to store it. Handy if you want to control in which order the graphics operations are applied on the control.
    ''' </summary>
    ''' <value><c>true</c> if [draw to graphics]; otherwise, <c>false</c>.</value>
    Public Property DrawToGraphics As Boolean = False

    ''' <summary>
    ''' Gets or sets the color of the band.
    ''' </summary>
    ''' <value>The color of the band.</value>
    Public Property BandColor As Color = Color.LightCyan

    ''' <summary>
    ''' Gets or sets a value indicating whether to paint the axes or not.
    ''' </summary>
    ''' <value><c>true</c> if the axes should be shown; otherwise, <c>false</c>.</value>
    Public Property ShowAxes As Boolean
        Get
            Return _showAxes
        End Get
        Set(ByVal value As Boolean)
            _showAxes = value
            Refresh()
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether to show the resulting magnification factors in the band
    ''' </summary>
    ''' <value><c>true</c> if the factors should be shown; otherwise, <c>false</c>.</value>
    Public Property ShowFactor As Boolean
        Get
            Return _showFactor
        End Get
        Set(ByVal value As Boolean)
            _showFactor = value
            Refresh()
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether instant zoom is enabled.
    ''' </summary>
    ''' <value><c>true</c> if instant zoom is enabled; otherwise, <c>false</c>.</value>
    Public Property InstantZoom As Boolean = False

    ''' <summary>
    ''' Gets the rubber bitmap. Only used if DrawToGraphics = False
    ''' </summary>
    ''' <value>The rubber bitmap.</value>
    Public ReadOnly Property RubberBitmap As Bitmap
        Get
            Return _bitmap
        End Get
    End Property

    ''' <summary>
    ''' Gets the rectangle.
    ''' </summary>
    ''' <value>The rectangle.</value>
    Public ReadOnly Property RubberRectangle As Rectangle
        Get
            Dim rect As New Rectangle

            rect.X = Math.Min(_rubberStart.X, _rubberEnd.X) + _movementEnd.X - _movementStart.X
            rect.Y = Math.Min(_rubberStart.Y, _rubberEnd.Y) + _movementEnd.Y - _movementStart.Y

            rect.Width = Math.Abs(Math.Max(_rubberStart.X, _rubberEnd.X) - rect.X + _movementEnd.X - _movementStart.X)
            rect.Height = Math.Abs(Math.Max(_rubberStart.Y, _rubberEnd.Y) - rect.Y + _movementEnd.Y - _movementStart.Y)

            If _resizeXDirection = Direction.Left Then
                If rect.Width - _resizeEnd.X + _resizeStart.X < 0 Then
                    rect.X += rect.Width
                    rect.Width = Math.Abs(rect.Width - _resizeEnd.X + _resizeStart.X) - (rect.X - rect.Width) + _resizeStart.X
                Else
                    rect.X += _resizeEnd.X - _resizeStart.X
                    rect.Width -= _resizeEnd.X - _resizeStart.X
                End If
            End If

            If _resizeXDirection = Direction.Right Then
                If rect.Width + _resizeEnd.X - _resizeStart.X < 0 Then
                    Dim oldRight = rect.Right
                    rect.X += _resizeEnd.X - _resizeStart.X + rect.Width - rect.Right + _resizeStart.X
                    rect.Width = Math.Abs(rect.Width + _resizeEnd.X - _resizeStart.X) + oldRight - _resizeStart.X
                Else
                    rect.Width += _resizeEnd.X - _resizeStart.X
                End If
            End If

            If _resizeYDirection = Direction.Top Then
                If rect.Height - _resizeEnd.Y + _resizeStart.Y < 0 Then
                    rect.Y += rect.Height
                    rect.Height = Math.Abs(rect.Height - _resizeEnd.Y + _resizeStart.Y) - (rect.Y - rect.Height) + _resizeStart.Y
                Else
                    rect.Y += _resizeEnd.Y - _resizeStart.Y
                    rect.Height -= _resizeEnd.Y - _resizeStart.Y
                End If
            End If

            If _resizeYDirection = Direction.Bottom Then
                If rect.Height + _resizeEnd.Y - _resizeStart.Y < 0 Then
                    Dim oldBottom = rect.Bottom
                    rect.Y += _resizeEnd.Y - _resizeStart.Y + rect.Height - rect.Bottom + _resizeStart.Y
                    rect.Height = Math.Abs(rect.Height + _resizeEnd.Y - _resizeStart.Y) + oldBottom - _resizeStart.Y
                Else
                    rect.Height += _resizeEnd.Y - _resizeStart.Y
                End If
            End If

            Return rect
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this <see cref="Win32RubberBand" /> is hovered.
    ''' </summary>
    ''' <value><c>true</c> if hovered; otherwise, <c>false</c>.</value>
    Public ReadOnly Property Hovered As Boolean
        Get
            Return _hovered
        End Get
    End Property

    ''' <summary>
    ''' Gets the hover rectangle.
    ''' </summary>
    ''' <value>The hover rectangle.</value>
    Private ReadOnly Property HoverRectangle As Rectangle
        Get
            If RubberRectangle.Width = 0 Or RubberRectangle.Height = 0 Then Return New Rectangle(-100, -100, 0, 0)

            Return New Rectangle(SizeRectangle.Left - RotationBoxDistance - RotationBoxSize - 10, SizeRectangle.Top - RotationBoxDistance - RotationBoxSize - 10, SizeRectangle.Width + 2 * (RotationBoxDistance + RotationBoxSize + 10), SizeRectangle.Height + 2 * (RotationBoxDistance + RotationBoxSize + 10))
        End Get
    End Property

    ''' <summary>
    ''' Gets the move rectangle.
    ''' </summary>
    ''' <value>The move rectangle.</value>
    Private ReadOnly Property MoveRectangle As Rectangle
        Get
            Return New Rectangle(RubberRectangle.X + SizeMarginInner, RubberRectangle.Y + SizeMarginInner, RubberRectangle.Width - SizeMarginInner * 2, RubberRectangle.Height - SizeMarginInner * 2)
        End Get
    End Property

    ''' <summary>
    ''' Gets the size rectangle.
    ''' </summary>
    ''' <value>The size rectangle.</value>
    Private ReadOnly Property SizeRectangle As Rectangle
        Get
            Return New Rectangle(RubberRectangle.X - SizeMarginOuter, RubberRectangle.Y - SizeMarginOuter, RubberRectangle.Width + SizeMarginOuter * 2, RubberRectangle.Height + SizeMarginOuter * 2)
        End Get
    End Property

    ''' <summary>
    ''' Gets the rotation rectangles.
    ''' </summary>
    ''' <value>The rotation rectangles.</value>
    Private ReadOnly Property RotationRectangles As Rectangle()
        Get
            Dim rects(3) As Rectangle

            rects(1) = New Rectangle(SizeRectangle.Left - RotationBoxDistance - RotationBoxSize, SizeRectangle.Top - RotationBoxDistance - RotationBoxSize, RotationBoxSize, RotationBoxSize)
            rects(2) = New Rectangle(SizeRectangle.Right + RotationBoxDistance, SizeRectangle.Top - RotationBoxDistance - RotationBoxSize, RotationBoxSize, RotationBoxSize)
            rects(3) = New Rectangle(SizeRectangle.Right + RotationBoxDistance, SizeRectangle.Bottom + RotationBoxDistance, RotationBoxSize, RotationBoxSize)
            rects(0) = New Rectangle(SizeRectangle.Left - RotationBoxDistance - RotationBoxSize, SizeRectangle.Bottom + RotationBoxDistance, RotationBoxSize, RotationBoxSize)

            Return rects
        End Get
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Win32RubberBand" /> class.
    ''' </summary>
    ''' <param name="parent">The parent.</param>
    Public Sub New(ByVal parent As Control)
        _parent = parent
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Win32RubberBand" /> class.
    ''' </summary>
    ''' <param name="parent">The parent.</param>
    ''' <param name="instantZoom">if set to <c>true</c> [instant zoom].</param>
    Public Sub New(ByVal parent As Control, ByVal instantZoom As Boolean)
        _parent = parent
        Me.InstantZoom = instantZoom
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Resets this instance.
    ''' </summary>
    Public Sub Reset()
        Reset(True)
    End Sub

    ''' <summary>
    ''' Resets this instance.
    ''' </summary>
    Public Sub Reset(ByVal refresh As Boolean)
        _rubberStart = New Point(-1, -1)
        _rubberEnd = New Point(-1, -1)
        _movementEnd = New Point
        _movementStart = New Point
        _resizeStart = New Point
        _resizeEnd = New Point
        _action = BandActions.None
        _rotationAngle = 0
        If refresh Then Me.Refresh()
    End Sub

    ''' <summary>
    ''' Processes the key.
    ''' </summary>
    ''' <param name="key">The key.</param>
    ''' <returns></returns>
    Public Function ProcessKey(ByVal key As Integer) As Boolean
        Dim moveX As Integer = 0
        Dim moveY As Integer = 0

        If key = Keys.Left Then
            moveX = -1
        ElseIf key = Keys.Right Then
            moveX = 1
        ElseIf key = Keys.Up Then
            moveY = -1
        ElseIf key = Keys.Down Then
            moveY = 1
        End If

        _rubberStart.X += moveX
        _rubberStart.Y += moveY

        _rubberEnd.X += moveX
        _rubberEnd.Y += moveY

        Refresh()

        Return moveX <> 0 Or moveY <> 0
    End Function

    ''' <summary>
    ''' Refreshes this instance.
    ''' </summary>
    Private Sub Refresh()
        If Not DrawToGraphics Then
            _bitmap = New Bitmap(_parent.Size.Width, _parent.Size.Height)
            Paint(Graphics.FromImage(_bitmap))
        End If

        _parent.Refresh()
    End Sub

    ''' <summary>
    ''' Rotates a given point.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <returns></returns>
    Private Function TranslatePoint(ByVal p As Point) As Point
        Dim rect = RubberRectangle
        Dim centerPoint As New Vector2(rect.Left + rect.Width / 2, rect.Top + rect.Height / 2)

        Return CType(New RotationMatrix(-_rotationAngle - GetRotationAngle(), centerPoint) * p, Point)
    End Function

    ''' <summary>
    ''' Paints the specified g.
    ''' </summary>
    ''' <param name="g">The g.</param>
    Private Sub Paint(ByRef g As Graphics)
        If g Is Nothing Or Not _enabled Then Return

        g.SmoothingMode = SmoothingMode.AntiAlias
        g.CompositingQuality = CompositingQuality.HighQuality

        If _parent.GetType = GetType(PictureBox) Then
            Dim box As PictureBox = CType(_parent, PictureBox)

            If box.Image Is Nothing Then
                g.Clear(box.BackColor)
            Else
                g.DrawImage(box.Image, New Point)
            End If
        End If

        If Not (_rubberStart.X = -1 Or _rubberEnd.X = -1) Then
            Dim rect = Me.RubberRectangle

            g.TranslateTransform(CSng(rect.Left + rect.Width / 2), CSng(rect.Top + rect.Height / 2))
            g.RotateTransform(CSng((_rotationAngle + GetRotationAngle()) / Math.PI * 180))
            g.TranslateTransform(-CSng(rect.Left + rect.Width / 2), -CSng(rect.Top + rect.Height / 2))

            Dim linePen As New Pen(New SolidBrush(Color.FromArgb(LineAlpha, BandColor)), 1)
            Dim fillBrush As New SolidBrush(Color.FromArgb(FillAlpha, BandColor))

            g.FillRectangle(fillBrush, rect)
            g.DrawRectangle(linePen, rect)

            Dim rects As New List(Of Rectangle)
            rects.Add(New Rectangle(rect.Left, rect.Top, rect.Width, CInt((rect.Height - MoveRectangle.Height) / 2)))
            rects.Add(New Rectangle(rect.Left, rect.Bottom - rects(0).Height, rect.Width, rects(0).Height))
            rects.Add(New Rectangle(rect.Left, rect.Top, CInt((rect.Width - MoveRectangle.Width) / 2), rect.Height))
            rects.Add(New Rectangle(rect.Right - rects(2).Width, rect.Top, rects(2).Width, rect.Height))

            For Each r In rects
                g.FillRectangle(New SolidBrush(Color.FromArgb(CInt(FillAlpha / 4), BandColor)), r)
            Next

            'axes
            Dim middleX As Integer = CInt(rect.Left + rect.Width / 2)
            Dim middleY As Integer = CInt(rect.Top + rect.Height / 2)


            'enough space for arrows and rotation boxes?
            If rect.Width > 20 And rect.Height > 20 Then
                If ShowAxes Then
                    g.DrawLine(linePen, middleX, rect.Top, middleX, rect.Bottom)
                    g.DrawLine(linePen, rect.Left, middleY, rect.Right, middleY)

                    g.DrawLine(linePen, middleX - 1, rect.Top + 1, middleX - 7, rect.Top + 9)
                    g.DrawLine(linePen, middleX + 1, rect.Top + 1, middleX + 7, rect.Top + 9)

                    g.DrawLine(linePen, rect.Right - 1, middleY - 1, rect.Right - 9, middleY - 7)
                    g.DrawLine(linePen, rect.Right - 1, middleY + 1, rect.Right - 9, middleY + 7)
                End If

                'Rotation is not possible when instant zoom is enabled
                If Not InstantZoom Then
                    For i As Integer = 0 To 3
                        Dim r = RotationRectangles(i)

                        r.Width -= 4
                        r.Height -= 4
                        If i = 2 Or i = 1 Then r.Y += 4
                        If i = 0 Or i = 1 Then r.X += 4

                        g.DrawArc(New Pen(New SolidBrush(Color.FromArgb(FillAlpha, BandColor)), 8), r, (i + 1) * 90, 90)
                    Next
                End If
            End If

            If rect.Width > 60 And rect.Height > 60 And ShowFactor Then
                g.DrawLine(linePen, rect.Right + 8, rect.Top + 1, rect.Right + 8, rect.Bottom - 1)
                g.DrawLine(linePen, rect.Right + 4, rect.Top, rect.Right + 12, rect.Top)
                g.DrawLine(linePen, rect.Right + 4, rect.Bottom, rect.Right + 12, rect.Bottom)
                g.DrawString("x " & Math.Round(_parent.Height / rect.Height, 2).ToString, New Font("Segoe UI", 12), New SolidBrush(Color.FromArgb(LineAlpha, Me.BandColor)), New Rectangle(New Point(rect.Right + 12, rect.Top), New Size(20, rect.Height)), New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center, .FormatFlags = StringFormatFlags.DirectionVertical})

                g.DrawLine(linePen, rect.Left + 1, rect.Top - 8, rect.Right - 1, rect.Top - 8)
                g.DrawLine(linePen, rect.Left, rect.Top - 12, rect.Left, rect.Top - 4)
                g.DrawLine(linePen, rect.Right, rect.Top - 12, rect.Right, rect.Top - 4)
                g.DrawString("x " & Math.Round(_parent.Width / rect.Width, 2).ToString, New Font("Segoe UI", 12), New SolidBrush(Color.FromArgb(LineAlpha, Me.BandColor)), New Rectangle(New Point(rect.Left, rect.Top - 28), New Size(rect.Width, 20)), New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center})
            End If
        End If
    End Sub

    ''' <summary>
    ''' Gets the rotation angle in radians.
    ''' </summary>
    Private Function GetRotationAngle() As Double
        If _rotationStart = New Point And _rotationEnd = New Point Then Return 0

        Dim rect = RubberRectangle
        Dim centerPoint As New Point(CInt(rect.Left + rect.Width / 2), CInt(rect.Top + rect.Height / 2))

        Dim vecOE As Vector2 = _rotationEnd
        Dim vecOS As Vector2 = _rotationStart
        Dim vecOC As Vector2 = centerPoint

        Dim vecCE As Vector2 = vecOE - vecOC
        Dim vecCS As Vector2 = vecOS - vecOC

        'cos alpha = (a*b)/(a.abs*b.abs) for 2 vectors a and b with their angle alpha (definition of dot product)
        Dim angle = Math.Acos((vecCE * vecCS) / (vecCE.Abs * vecCS.Abs))

        'valid?
        If Double.IsInfinity(angle) Or Double.IsNaN(angle) Then Return 0

        'find out whether this is meant clock- or counterclockwise
        'rotating the unit vector of CE by angle should give the unit vector of CE
        Dim uvecCE = vecCE.GetUnitVector
        Dim uvecCS = vecCS.GetUnitVector

        If Not (New RotationMatrix(angle) * uvecCS - uvecCE).Abs < 0.001 Then
            angle *= -1
        End If

        Return angle
    End Function

    ''' <summary>
    ''' Rotations the rectangle contains.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <returns></returns>
    Private Function RotationRectangleContains(ByVal p As Point) As Boolean
        For Each r In RotationRectangles
            If r.Contains(p) Then Return True
        Next

        Return False
    End Function

#End Region

#Region "Event Handlers"

    ''' <summary>
    ''' Handles the MouseDoubleClick event of the parent control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Forms.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub parent_MouseDoubleClick(ByVal sender As Object, ByVal e As MouseEventArgs) Handles _parent.MouseDoubleClick
        If e.Button = MouseButtons.Left And _enabled And RubberRectangle.Contains(e.Location) Then
            OnBandCreated()
        End If
    End Sub

    ''' <summary>
    ''' Handles the MouseDown event of the parent control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Forms.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub parent_MouseDown(ByVal sender As Object, ByVal e As MouseEventArgs) Handles _parent.MouseDown
        If Not _enabled Then Return

        Dim translated = TranslatePoint(e.Location)

        If e.Button = MouseButtons.Left Then
            If MoveRectangle.Contains(translated) Then
                'Move
                _movementStart = e.Location
                _movementEnd = e.Location

                _action = BandActions.Move
            ElseIf SizeRectangle.Contains(translated) Then
                'Size 
                Dim maxVariance = Math.Max(SizeMarginInner, SizeMarginOuter)

                Dim left = Math.Abs(RubberRectangle.Left - translated.X) <= maxVariance
                Dim right = Math.Abs(RubberRectangle.Right - translated.X) <= maxVariance
                Dim top = Math.Abs(RubberRectangle.Top - translated.Y) <= maxVariance
                Dim bottom = Math.Abs(RubberRectangle.Bottom - translated.Y) <= maxVariance

                If (left And (top Or bottom)) Or (right And (top Or bottom)) Then
                    _action = BandActions.ReSizeXY
                ElseIf left Or right Then
                    _action = BandActions.ReSizeX
                ElseIf top Or bottom Then
                    _action = BandActions.ReSizeY
                End If

                If left Then
                    _resizeXDirection = Direction.Left
                Else
                    _resizeXDirection = Direction.Right
                End If

                If top Then
                    _resizeYDirection = Direction.Top
                Else
                    _resizeYDirection = Direction.Bottom
                End If

                _resizeStart = translated
                _resizeEnd = translated
            ElseIf RotationRectangleContains(translated) Then
                _rotationStart = e.Location
                _rotationEnd = e.Location

                _action = BandActions.Rotate
            Else
                'New Band
                Reset(False)

                _rubberStart = e.Location
                _rubberEnd = e.Location

                _action = BandActions.Size
            End If

            Refresh()
        End If
    End Sub

    ''' <summary>
    ''' Handles the MouseMove event of the parent control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Forms.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub parent_MouseMove(ByVal sender As Object, ByVal e As MouseEventArgs) Handles _parent.MouseMove
        If Not _enabled Then Return

        Dim translated = TranslatePoint(e.Location)

        If HoverRectangle.Contains(translated) And Not _hovered Then
            OnMouseEnter(e.Location)
        ElseIf Not HoverRectangle.Contains(translated) And _hovered Then
            OnMouseLeave(e.Location)
        End If

        If e.Button = MouseButtons.Left Then
            If _action = BandActions.Size Then
                _rubberEnd = e.Location
            ElseIf _action = BandActions.Move Then
                _movementEnd = e.Location
            ElseIf _action = BandActions.ReSizeX Or _action = BandActions.ReSizeXY Or _action = BandActions.ReSizeY Then
                If _action = BandActions.ReSizeX Or _action = BandActions.ReSizeXY Then
                    _resizeEnd.X = translated.X
                End If

                If _action = BandActions.ReSizeY Or _action = BandActions.ReSizeXY Then
                    _resizeEnd.Y = translated.Y
                End If
            ElseIf _action = BandActions.Rotate Then
                _rotationEnd = e.Location
            End If

            Refresh()
        End If
    End Sub

    ''' <summary>
    ''' Handles the MouseUp event of the parent control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Forms.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub parent_MouseUp(ByVal sender As Object, ByVal e As MouseEventArgs) Handles _parent.MouseUp
        If Not _enabled Then Return

        If e.Button = MouseButtons.Left Then
            If InstantZoom Then
                'Band Finished
                OnBandCreated()
            ElseIf _action = BandActions.Move Then
                'Apply Movement to Rectangle
                Dim rect = Me.RubberRectangle
                _rubberStart = rect.Location
                _rubberEnd = rect.Location + rect.Size

                _movementEnd = New Point
                _movementStart = New Point
            ElseIf _action = BandActions.ReSizeX Or _action = BandActions.ReSizeXY Or _action = BandActions.ReSizeY Then
                'Apply Resize to Rectangle
                Dim rect = Me.RubberRectangle
                _rubberStart = rect.Location
                _rubberEnd = rect.Location + rect.Size

                _resizeEnd = New Point
                _resizeStart = New Point
            ElseIf _action = BandActions.Rotate Then
                'Apply Rotation
                _rotationAngle += GetRotationAngle()

                _rotationEnd = New Point
                _rotationStart = New Point
            End If
        End If

        _action = BandActions.None
        Refresh()
    End Sub

    ''' <summary>
    ''' Handles the Paint event of the parent control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Forms.PaintEventArgs" /> instance containing the event data.</param>
    Private Sub parent_Paint(ByVal sender As Object, ByVal e As PaintEventArgs) Handles _parent.Paint
        If Not _enabled Then Return

        If DrawToGraphics Then
            Paint(e.Graphics)
        End If
    End Sub

#End Region

#Region "Events"

    ''' <summary>
    ''' Called when [band created].
    ''' </summary>
    Private Sub OnBandCreated()
        RaiseEvent BandCreated(Me, New Win32BandCreatedEventArgs(RubberRectangle, _rotationAngle))

        Reset()
    End Sub

    ''' <summary>
    ''' Called when [mouse enter].
    ''' </summary>
    ''' <param name="p">The p.</param>
    Private Sub OnMouseEnter(ByVal p As Point)
        _hovered = True
        RaiseEvent MouseEnter(Me, New MouseEventArgs(MouseButtons.None, 0, p.X, p.Y, 0))
    End Sub

    ''' <summary>
    ''' Called when [mouse leave].
    ''' </summary>
    ''' <param name="p">The p.</param>
    Private Sub OnMouseLeave(ByVal p As Point)
        _hovered = False
        RaiseEvent MouseEnter(Me, New MouseEventArgs(MouseButtons.None, 0, p.X, p.Y, 0))
    End Sub

    ''' <summary>
    ''' Occurs when [band created].
    ''' </summary>
    Public Event BandCreated(ByVal sender As Object, ByVal e As Win32BandCreatedEventArgs)

    ''' <summary>
    ''' Occurs when [mouse enter].
    ''' </summary>
    Public Event MouseEnter(ByVal sender As Object, ByVal e As MouseEventArgs)

    ''' <summary>
    ''' Occurs when [mouse leave].
    ''' </summary>
    Public Event MouseLeave(ByVal sender As Object, ByVal e As MouseEventArgs)

#End Region
End Class
