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

Imports System.Drawing
Imports Fractals.Utilities
Imports Fractals.Mathematics

Public Class ZoomAnimation
    Inherits Animation

    ''' <summary>
    ''' Gets or sets the target zoom.
    ''' </summary>
    ''' <value>The target zoom.</value>
    Public Property TargetZoom As ZoomState

    ''' <summary>
    ''' Gets or sets the initial zoom.
    ''' </summary>
    ''' <value>The initial zoom.</value>
    Public Property InitialZoom As ZoomState

    Private _firstState As ZoomState

    ''' <summary>
    ''' Performs an animation on a fractal.
    ''' </summary>
    ''' <param name="target">The target fractal.</param>
    ''' <param name="frame">The current frame relative to StartFrame.</param>
    Public Overrides Sub Modify(ByVal target As Fractal, ByVal frame As Integer)
        'use last zoom state if InititalZoom is undefined
        If frame = 0 Then
            If ZoomState.IsNaN(InitialZoom) Then
                _firstState = target.ZoomState
            Else
                _firstState = InitialZoom
            End If
        End If
        
        'retrieve the default viewport size
        Dim zOld = target.ZoomState
        target.ResetZoom()
        Dim defSize = target.ScreenToViewport(New PointF(target.Size.Width, target.Size.Height)) - target.ScreenToViewport(New PointF(0, 0))
        target.ZoomState = zOld

        Dim zoomBase As Double = 1
        Dim z = target.ZoomState

        If Not Double.IsNaN(TargetZoom.Factor) Then
            'zooming: every frame the factor gets multiplied by a constant
            'endFactor = startFactor * zoomBase ^ length
            '--> zoomBase = (endFactor / startfactor) ^ (1/length)
            zoomBase = (TargetZoom.Factor / _firstState.Factor) ^ (1 / Length)


            'apply the zoomFactor frame times
            z.Factor = _firstState.Factor * zoomBase ^ frame
        End If

        If Not ComplexNumber.IsNaN(TargetZoom.Offset) Then
            'movement while zooming: after every frame, the image is moved by a constant percentage
            'endOffset = startOffset + sum_{i=0}^{framecount}{size(i)*x}
            'size(i) = sizeDefault / zoomFactor(i) = sizeDefault / (startFactor * zoomBase ^ i)
            '--> x = (endOffset - startOffset) / sum_{i=0}^{framecount}{size(i)*x}
            Dim sum As Double = 0

            'general sum without sizeDefault
            For i As Integer = 0 To Length - 1
                sum += 1 / (_firstState.Factor * zoomBase ^ i)
            Next

            'use this sum to calculate sums for width and height
            Dim wSum = defSize.RealPart * sum
            Dim hSum = defSize.ImaginaryPart * sum

            'now use this factor to calculate the x
            Dim wFactor = (TargetZoom.Offset - _firstState.Offset).RealPart / wSum
            Dim hFactor = (TargetZoom.Offset - _firstState.Offset).ImaginaryPart / hSum

            'apply the offset frame times
            Dim off As ComplexNumber = _firstState.Offset
            If frame > 0 Then
                For i As Integer = 0 To frame - 1
                    off += New ComplexNumber(wFactor * defSize.RealPart / (_firstState.Factor * zoomBase ^ i), hFactor * defSize.ImaginaryPart / (_firstState.Factor * zoomBase ^ i))
                Next
            End If

            z.Offset = off
        End If

        If Not Double.IsNaN(TargetZoom.Rotation) Then
            'apply rotation
            z.Rotation = _firstState.Rotation + (TargetZoom.Rotation - _firstState.Rotation) * frame / Length
        End If

        target.ZoomState = z
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ZoomAnimation" /> class.
    ''' </summary>
    ''' <param name="startFrame">The start frame.</param>
    ''' <param name="endFrame">The end frame.</param>
    ''' <param name="initialZoom">The initial zoom or ZoomState.NaN if the zoomstate provided by the fractal should be used.</param>
    ''' <param name="targetZoom">The target zoom. Some of its properties may be NaN when they should not be altered by this animation.</param>
    Sub New(ByVal startFrame As Integer, ByVal endFrame As Integer, ByVal initialZoom As ZoomState, ByVal targetZoom As ZoomState)
        MyBase.New(startFrame, endFrame)

        Me.InitialZoom = initialZoom
        Me.TargetZoom = targetZoom
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ZoomAnimation" /> class.
    ''' </summary>
    Sub New()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        InitialZoom = ZoomState.Load(x.Element("InitialZoom"))
        TargetZoom = ZoomState.Load(x.Element("TargetZoom"))
    End Sub

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("InitialZoom")
        ZoomState.Save(InitialZoom, x)
        x.WriteEndElement()

        x.WriteStartElement("TargetZoom")
        ZoomState.Save(TargetZoom, x)
        x.WriteEndElement()
    End Sub
End Class
