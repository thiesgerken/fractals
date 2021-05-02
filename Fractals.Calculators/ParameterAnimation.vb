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

Public Class ParameterAnimation
    Inherits Animation
    
    ''' <summary>
    ''' Performs an animation on a fractal.
    ''' </summary>
    ''' <param name="target">The target fractal.</param>
    ''' <param name="frame">The current frame relative to StartFrame.</param>
    Public Overrides Sub Modify(ByVal target As Fractal, ByVal frame As Integer)
        Dim c = target.GetType.GetProperty(ParameterName)
        c.SetValue(target, StartValue + frame / (Length - 1) * (EndValue - StartValue), Nothing)
    End Sub

    ''' <summary>
    ''' Gets or sets the name of the parameter.
    ''' </summary>
    ''' <value>The name of the parameter.</value>
    Public Property ParameterName As String

    ''' <summary>
    ''' Gets or sets the start value.
    ''' </summary>
    ''' <value>The start value.</value>
    Public Property StartValue As Double

    ''' <summary>
    ''' Gets or sets the end value.
    ''' </summary>
    ''' <value>The end value.</value>
    Public Property EndValue As Double

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ParameterAnimation" /> class.
    ''' </summary>
    ''' <param name="startFrame">The start frame.</param>
    ''' <param name="endFrame">The end frame.</param>
    ''' <param name="parameterName">Name of the parameter.</param>
    ''' <param name="startValue">The start value.</param>
    ''' <param name="endValue">The end value.</param>
    Public Sub New(ByVal startFrame As Integer, ByVal endFrame As Integer, ByRef parameterName As String, ByVal startValue As Double, ByVal endValue As Double)
        MyBase.New(startFrame, endFrame)

        Me.StartValue = startValue
        Me.EndValue = endValue
        Me.ParameterName = parameterName
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ParameterAnimation" /> class.
    ''' </summary>
    Sub New()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        StartValue = CDbl(x.Element("StartValue").Value)
        EndValue = CDbl(x.Element("EndValue").Value)
        ParameterName = CStr(x.Element("ParameterName").Value)
    End Sub

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("StartValue")
        x.WriteValue(StartValue)
        x.WriteEndElement()

        x.WriteStartElement("EndValue")
        x.WriteValue(EndValue)
        x.WriteEndElement()

        x.WriteStartElement("ParameterName")
        x.WriteValue(ParameterName)
        x.WriteEndElement()
    End Sub
End Class
