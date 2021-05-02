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
Imports Fractals.Mathematics
Imports System.IO
Imports System.Xml
Imports System.Globalization

Public Structure ZoomState
    Implements ICloneable

#Region "Fields"

    Private _factor As Double
  
#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the offset.
    ''' </summary>
    ''' <value>The offset.</value>
    Public Property Offset As ComplexNumber
    
    ''' <summary>
    ''' Gets or sets the zoom factor.
    ''' </summary>
    ''' <value>The zoom factor.</value>
    Public Property Factor As Double
        Get
            If _factor <= 0 Then
                _factor = 1
            End If

            Return _factor
        End Get
        Set(ByVal value As Double)
            If value <= 0 Then
                value = 1
            End If

            _factor = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the rotation.
    ''' </summary>
    ''' <value>The rotation in radians.</value>
    Public Property Rotation As Double

    ''' <summary>
    ''' Gets a undefined zoomstate.
    ''' </summary>
    ''' <value></value>
    Public Shared ReadOnly Property NaN As ZoomState
        Get
            Return New ZoomState(New ComplexNumber(Double.NaN, Double.NaN), Double.NaN, Double.NaN)
        End Get
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ZoomState" /> class.
    ''' </summary>
    ''' <param name="offset">The offset.</param>
    ''' <param name="zoomFactor">The zoom factor.</param>
    ''' <param name="rotation">The rotation in radians.</param>
    Public Sub New(ByVal offset As ComplexNumber, ByVal zoomFactor As Double, ByVal rotation As Double)
        Me.Offset = offset
        Me.Factor = zoomFactor
        Me.Rotation = rotation
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Determines whether a specified instance is undefined.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <returns>
    ''' <c>true</c> if is NaN; otherwise, <c>false</c>.
    ''' </returns>
    Public Shared Function IsNaN(ByVal obj As ZoomState) As Boolean
        Return Double.IsNaN(obj.Offset.RealPart) And Double.IsNaN(obj.Offset.ImaginaryPart) And Double.IsNaN(obj.Rotation) And Double.IsNaN(obj.Factor)
   End Function

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overrides Function ToString() As String
        Return Me.Offset.ToString & ";" & Me.Factor & ";" & Me.Rotation
    End Function

    ''' <summary>
    ''' Parses the specified value.
    ''' </summary>
    ''' <param name="value">The value.</param>
    ''' <returns></returns>
    Public Shared Function Parse(ByVal value As String, ByVal format As CultureInfo) As ZoomState
        Dim properties() = value.Split({";"c}, StringSplitOptions.RemoveEmptyEntries)

        Dim z As New ZoomState

        z.Offset = ComplexNumber.Parse(properties(0), format)
        z.Factor = Double.Parse(properties(1), format)
        z.Rotation = Double.Parse(properties(2), format)

        Return z
    End Function

    ''' <summary>
    ''' Saves the specified obj.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <param name="x">The x.</param>
    Public Shared Sub Save(ByVal obj As ZoomState, ByVal x As XmlWriter)
        x.WriteStartElement("Offset")
        ComplexNumber.Save(obj.Offset, x)
        x.WriteEndElement()

        x.WriteStartElement("Factor")
        x.WriteValue(obj.Factor)
        x.WriteEndElement()

        x.WriteStartElement("Rotation")
        x.WriteValue(obj.Rotation)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As ZoomState
        Dim z As New ZoomState

        z.Factor = CDbl(x.Element("Factor"))
        z.Rotation = CDbl(x.Element("Rotation"))
        z.Offset = ComplexNumber.Load(x.Element("Offset"))

        Return z
    End Function

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator &lt;&gt;.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator <>(ByVal x As ZoomState, ByVal y As ZoomState) As Boolean
        Return Not x = y
    End Operator

    ''' <summary>
    ''' Implements the operator =.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator =(ByVal x As ZoomState, ByVal y As ZoomState) As Boolean
        If x.Factor <> y.Factor Then Return False
        If x.Offset <> y.Offset Then Return False
        If x.Rotation <> y.Rotation Then Return False

        Return True
    End Operator

#End Region

#Region "ICloneable Member"

    ''' <summary>
    ''' Creates a new object that is a copy of the current instance.
    ''' </summary>
    ''' <returns>
    ''' A new object that is a copy of this instance.
    ''' </returns>
    Public Function Clone() As Object Implements ICloneable.Clone
        Return New ZoomState(Me.Offset, Me.Factor, Me.Rotation)
    End Function

#End Region

End Structure
