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

Imports System.Xml
Imports System.Xml.Linq
Imports Fractals.Utilities

Public MustInherit Class Animation
    ''' <summary>
    ''' Gets or sets the first frame on which the animation should be applied.
    ''' </summary>
    ''' <value>The start frame.</value>
    Public Property StartFrame As Integer

    ''' <summary>
    ''' Gets or sets the last frame on which the animation should be applied.
    ''' </summary>
    ''' <value>The end frame.</value>
    Public Property EndFrame As Integer

    ''' <summary>
    ''' Gets the amount of frames for which this animation is responsible.
    ''' </summary>
    ''' <value>The length.</value>
    Public ReadOnly Property Length As Integer
        Get
            'both bounds are inclusive (4 to 4 has a length of 1, not 0), hence the + 1 was put there
            Return EndFrame - StartFrame + 1
        End Get
    End Property

    ''' <summary>
    ''' Performs an animation on a fractal.
    ''' </summary>
    ''' <param name="target">The target fractal.</param>
    ''' <param name="frame">The current frame relative to StartFrame.</param>
    Public MustOverride Sub Modify(ByVal target As Fractal, ByVal frame As Integer)

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Animation" /> class.
    ''' </summary>
    ''' <param name="startFrame">The start frame.</param>
    ''' <param name="endFrame">The end frame.</param>
    Public Sub New(ByVal startFrame As Integer, ByVal endFrame As Integer)
        Me.StartFrame = startFrame
        Me.EndFrame = endFrame
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Animation" /> class.
    ''' </summary>
    Public Sub New()
    End Sub

    ''' <summary>
    ''' Saves the specified obj.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <param name="x">The x.</param>
    Public Shared Sub Save(ByVal obj As Animation, ByVal x As XmlWriter)
        x.WriteStartElement("Type")
        x.WriteType(obj.GetType())
        x.WriteEndElement()

        x.WriteStartElement("StartFrame")
        x.WriteValue(obj.StartFrame)
        x.WriteEndElement()

        x.WriteStartElement("EndFrame")
        x.WriteValue(obj.EndFrame)
        x.WriteEndElement()

        obj.SaveParameters(x)
    End Sub

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As Animation
        Dim inst As Animation = CType(Activator.CreateInstance(SaveUtils.LoadType(x.Element("Type"))), Animation)

        inst.StartFrame = CInt(x.Element("StartFrame"))
        inst.EndFrame = CInt(x.Element("EndFrame"))

        inst.LoadParameters(x)

        Return inst
    End Function

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub LoadParameters(ByVal x As XElement)

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub SaveParameters(ByVal x As XmlWriter)

End Class
